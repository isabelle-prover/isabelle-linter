/* Author: Yecine Megdiche, TU Muenchen

Implementation of lints for HOL.
 */

package isabelle.linter


import Linter.*
import isabelle.*
import isabelle.linter.Severity.Level

import scala.annotation.tailrec


object Lints {
  val apply_isar_switch = Lint_Wrapper("apply_isar_switch", (name, severity) =>
    new Proper_Commands_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Switching from an apply script to a structured Isar proof is error-prone and hard to read.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Switching from an apply script to a structured Isar proof results in an overall ")
        .add("proof that is hard to read without relying on Isabelle. The Isar proof is also ")
        .add("sensitive to the output of the apply script, and might therefore break easily.")
        .references("http://proofcraft.org/blog/isabelle-style.html")

    @tailrec
    def lint_proper(commands: List[Parsed_Command], report: Report): Report =
      commands match {
        case Parsed_Command("apply") :: (proof@Parsed_Command("proof")) :: next =>
          val new_report = add_result(
            "Do not switch between apply-style and ISAR proofs.",
            proof.range,
            None,
            proof,
            report)
          lint_proper(next, new_report)
        case _ :: next => lint_proper(next, report)
        case Nil => report
      }
  })

  val use_apply = Lint_Wrapper("use_apply", (name, severity) => new AST_Lint(name, severity) {
    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Use the expanded form corresponding to the ").inline_code("by")
        .add(" command.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("This lint is the inverse direction of the ").inline_code("use_by")
        .add(" lint: it identifies usages of the ").inline_code("by").add(" command and suggests to")
        .addln("expand the methods. As an example, it helps transform")
        .code_block(
          "lemma …",
          "  by (induction xs) auto")
        .add("into")
        .code_block(
          "lemma …",
          "  apply (induction xs)",
          "  apply auto",
          "done")

    override def lint_proof(proof: Text.Info[Proof], report: Reporter): Option[Result] =
      proof.info match {
        case By(method1, method2) =>
          val apply1 = s"apply ${report.source(method1.range)}\n"
          val apply2 = method2.map(some_method => s"apply ${report.source(some_method.range)}\n").getOrElse("")
          val replacement = apply1 + apply2 + "done"
          report("Use \"apply\" instead of \"by\".", proof.range, Some(Edit(proof.range, replacement)))
        case _ => None
      }
  })

  val use_by = Lint_Wrapper("use_by", (name, severity) =>
    new Proper_Commands_Lint(name, severity) with Token_Parsers with Complex_Methods {

    val modifier_threshold = 1
    val combinator_threshold = 3

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Using the ").inline_code("by")
        .add(" command is more concise than short apply-scripts.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("The ").inline_code("by")
        .add(" command allows to express method applications using ").inline_code("apply")
        .addln(" more concisely. For example, instead of")
        .code_block(
          "lemma …",
          "  apply (induction xs)",
          "  apply auto",
          "done")
        .breakline
        .inline_code("by").addln("can be used:")
        .code_block(
          "lemma …",
          "  by (induction xs) auto")

    private def removeApply: Parser[String] = (p_command("apply") ~ p_space.?) ~> p_any.* ^^ mk_string

    private def removeBy: Parser[String] = (p_command("by") ~ p_space.?) ~> p_any.* ^^ mk_string

    private def gen_replacement(
      apply_script: List[Parsed_Command],
      has_by: Boolean = false
    ): Option[String] = {
      apply_script match {
        case apply1 :: apply2 :: _ :: Nil =>
          for {
            method1 <- try_transform(removeApply, apply1, true)
            method2 <- try_transform(removeApply, apply2, true)
          } yield s"by $method1 $method2"
        case apply :: _ :: Nil if !has_by =>
          for {
            method <- try_transform(removeApply, apply, true)
          } yield s"by $method"
        case apply :: by :: Nil if has_by =>
          for {
            method1 <- try_transform(removeApply, apply, true)
            method2 <- try_transform(removeBy, by, true)
          } yield s"by $method1 $method2"
        case _ => None
      }
    }

    private def report_lint(
      apply_script: List[Parsed_Command],
      report: Report,
      has_by: Boolean = false
    ): Report = {
      val new_report = for {
        replacement <- gen_replacement(apply_script, has_by)
      } yield add_result(
        """Use "by" instead of a short apply-script.""",
        Parsers.list_range(apply_script.map(_.range)),
        Some(Edit(Parsers.list_range(apply_script map (_.range)), replacement)),
        apply_script,
        report)
      new_report.getOrElse(report)
    }

    private def check_first_command(command: Parsed_Command): Boolean =
      List("lemma", "theorem", "corollary").contains(command.kind)

    private def single_by(command: Parsed_Command): Boolean =
      command.ast_node.info match {
        case By(_, None) => true
        case _ => false
      }

    @tailrec
    def lint_proper(commands: List[Parsed_Command], report: Report): Report =
      commands match {
        case first
          :: (apply1@Parsed_Command("apply"))
          :: (apply2@Parsed_Command("apply"))
          :: (done@Parsed_Command("done"))
          :: next
          if (check_first_command(first) && !is_complex(apply1) && !is_complex(apply2)) =>
          lint_proper(next, report_lint(apply1 :: apply2 :: done :: Nil, report))
        case first
          :: (apply@Parsed_Command("apply"))
          :: (done@Parsed_Command("done"))
          :: next if check_first_command(first) && !is_complex(apply) =>
          lint_proper(next, report_lint(apply :: done :: Nil, report))
        case first
          :: (apply@Parsed_Command("apply"))
          :: (by@Parsed_Command("by"))
          :: next
          if check_first_command(first) && !is_complex(apply) && single_by(by) =>
          lint_proper(next, report_lint(apply :: by :: Nil, report, has_by = true))
        case _ :: next => lint_proper(next, report)
        case Nil => report
      }
  })

  val unrestricted_auto = Lint_Wrapper("unrestricted_auto",
    (name, severity) => new Proper_Commands_Lint(name, severity) {

    private val PROOF_COMMANDS = List("apply", "by", "proof", "using", "unfolding", "supply", "subgoal")

    val short_description: Lint_Description =
      Lint_Description.empty
        .inline_code("auto").add(" should be used as a terminal proof method or be restricted.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Using auto in the middle of a proof on all goals (i.e. unrestricted) might ")
        .add("produce an unpredictable proof state. It should rather be used as a terminal ")
        .add("proof method, or be restricted to a set of goals that it fully solves. ")
        .references("http://proofcraft.org/blog/isabelle-style.html")

    private def is_proof_command(command: Parsed_Command): Boolean =
      PROOF_COMMANDS.contains(command.kind)

    private def are_unrestricted(modifiers: List[Method.Modifier]): Boolean =
      !modifiers.exists(_.isInstanceOf[Method.Modifier.Restrict])

    private def is_unrestricted_auto__method(method: Method): Boolean = method match {
      case Simple_Method(name, modifiers, _) =>
        name.info.content == "auto" && are_unrestricted(modifiers.map(_.info))
      case _ => false
    }

    private def is_unrestricted_auto(element: AST_Node): Boolean = element match {
      case Apply(method) => is_unrestricted_auto__method(method.info)
      case _ => false
    }

    private def report_lint(apply: Parsed_Command, report: Report): Report =
      add_result("Do not use unrestricted auto as a non-terminal proof method.", apply.range,
        None, apply, report)

    @tailrec
    def lint_proper(commands: List[Parsed_Command], report: Report): Report =
      commands match {
        case (apply@Parsed_Command("apply")) :: next_command :: next
          if is_proof_command(next_command) && is_unrestricted_auto(apply.ast_node.info) =>
          lint_proper(next_command :: next, report_lint(apply, report))
        case _ :: next => lint_proper(next, report)
        case Nil => report
      }
    })

  val low_level_apply_chain = Lint_Wrapper("low_level_apply_chain",
    (name, severity) => new Proper_Commands_Lint(name, severity) {

    val short_description:Lint_Description =
      Lint_Description.empty.add("Long apply-scripts should be avoided.")

    val MIN_LENGTH = 100

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Using long apply-scripts with low-level methods can quickly make proofs unreadable and ")
        .add("unnecessarily long. This lints flags such scripts that are longer than " + MIN_LENGTH + " commands.")

    val LOW_LEVEL_RULES =
      List("erule", "rule", "simp", "clarsimp", "rule_tac", "frule", "erule", "drule", "subst",
        "rewrite", "unfold")

    private def is_low_level_method(method: Method): Boolean = method match {
      case Simple_Method(name, _, _) =>
        LOW_LEVEL_RULES.contains(name.info.content)
      case _ => false
    }

    private def is_low_level_apply(command: Parsed_Command): Boolean = command.ast_node.info match {
      case Apply(method) => is_low_level_method(method.info)
      case _ => false
    }

    @tailrec
    def lint_proper(commands: List[Parsed_Command], report: Report): Report = {
      val (low_level_commands, rest) =
        commands.dropWhile(!is_low_level_apply(_)).span(is_low_level_apply)

      val new_report =
        if (low_level_commands.length > MIN_LENGTH) {
          add_result(
            "Compress low-level proof methods into automated search.",
            Parsers.list_range(low_level_commands.map(_.range)),
            None,
            low_level_commands,
            report)
        }
        else report

      if (rest.isEmpty) new_report
      else lint_proper(rest, new_report)
    }
  })

  val global_attribute_changes = Lint_Wrapper("global_attribute_changes",
    (name, severity) => new Proper_Commands_Lint(name, severity) with Token_Parsers {

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Global lemma attributes should not be changed temporarily, use ")
        .inline_code("declare").add(" instead.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Changing lemma attributes (e.g. ").inline_code("simp").add(") to to accomodate to a local proof")
        .add(" discouraged, as it is error-prone and might result in hard-to debug problems.")
        .empty_line
        .addln("Concretely, the lints warns the users of using this pattern:")
        .code_block(
          "declare word_neq_0_conv [simp]",
          "",
          "  lemma …",
          "  lemma …",
          "",
          "",
          "declare word_neq_0_conv [simp del]")
        .add("Instead, users should use the context, notes or bundle commands.")
        .references("http://proofcraft.org/blog/isabelle-style-part2.html")

    type Declaration = (String, List[String]) // Identifier, attribute list without whitespaces

    private def declaration: Parser[Declaration] = p_ident ~ p_sq_bracketed(p_attributes) ^^ {
      case identifier ~ attributes => (identifier.info.content, attributes map mk_string)
    }

    private def declare_command: Parser[List[Declaration]] =
      p_command("declare") ~> chainl1[List[Declaration]](
        declaration ^^ { List(_) },
        p_keyword("and") ^^^ { _ ::: _ }
      )

    private def has_simp(attrs: List[String]): Boolean =
      attrs.exists(attr => attr == "simp" || attr == "simpadd")

    private def has_simp_del(attrs: List[String]): Boolean =
      attrs.contains("simpdel") // Whitespaces are ignored

    private def proces_declaration(command: Parsed_Command)(
      report_simpset: (Report, Set[String]),
      declaration: Declaration): (Report, Set[String]
      ) = {
      val (ident, attrs) = declaration
      val (report, simpset) = report_simpset
      val new_report =
        if (simpset.contains(ident) && has_simp_del(attrs)) {
          add_result(
            "Use context or bundles instead of global simp attribute changes.",
            command.range,
            None,
            command,
            report)
        }
        else report
      val new_simpset =
        if (has_simp(attrs)) simpset + ident
        else if (has_simp_del(attrs)) simpset - ident
        else simpset
      (new_report, new_simpset)
    }

    @tailrec
    private def go(
      commands: List[Parsed_Command],
      report: Report,
      simpset: Set[String]
    ): Report = {
      commands match {
        case (head@Parsed_Command("declare")) :: next =>
          try_transform(declare_command, head) match {
            case Some(decls) =>
              val (new_report, new_simpset) =
                decls.foldLeft((report, simpset))(proces_declaration(head))
              go(next, new_report, new_simpset)
            case None => go(next, report, simpset)
          }
        case _ :: next => go(next, report, simpset)
        case Nil => report
      }
    }

    def lint_proper(commands: List[Parsed_Command], report: Report): Report =
      go(commands, report, Set.empty)
  })

  val use_isar = Lint_Wrapper("use_isar",
    (name, severity) => new Single_Command_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .inline_code("apply").add("-scripts should be avoided.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("This lint triggers on every use of the ").inline_code("apply")
        .add(" command and suggests to use an Isar proof instead.")

    def lint(command: Parsed_Command, report: Reporter): Option[Result] = command match {
      case c@Parsed_Command("apply") =>
        report("Use Isar instead of apply-scripts.", c.range, None)
      case _ => None
    }
  })

  val axiomatization_with_where = Lint_Wrapper("axiomatization_with_where",
    (name, severity) => new Single_Command_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .inline_code("axiomatization").add(" commands should not have a ").inline_code("where")
        .add(" clause, unless when creating or extending a logic.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Unless when creating a new logic or extending an existing one with new axioms,")
        .add("the ").inline_code("axiomatization").add(" command, when used, should not include a ")
        .inline_code("where").add(" clause.")
        .empty_line
        .add("The problem with the ").inline_code("where").add(" clause is that it can introduce inconsistencies")
        .addln("into the logic, for example:")
        .code_block(
          "axiomatization",
          """  P :: "'a ⇒ bool"""",
          "where",
          """  all_true: "∀x. P x" and """,
          """all_false: "∀x. ¬P x"""")
        .references("http://proofcraft.org/blog/isabelle-style.html")

    def lint(command: Parsed_Command, report: Reporter): Option[Result] = command.tokens match {
      case RToken(Token.Kind.COMMAND, "axiomatization", _) :: next =>
        next.dropWhile(_.info.source != "where") match {
          case xs@_ :: _ =>
            report(
              """Do not use axiomatization with a where clause.""",
              Text.Range(xs.head.range.start, xs.last.range.stop),
              Some(Edit(Parsers.list_range(xs.map(_.range)), "", Some("Remove where"))))
          case Nil => None
        }
      case _ => None
    }
  })

  class Illegal_Command_Lint(
    name: String,
    severity: Severity.Level,
    message: String,
    illegal_commands: List[String],
    description: String
  ) extends Single_Command_Lint(name, severity) {
    val short_description: Lint_Description =
      Lint_Description.empty.add(description)

    val long_description: Lint_Description = {
      val desc = Lint_Description.empty.addln(description + ": ").inline_code(
        illegal_commands.head)
      illegal_commands.tail.foldLeft(desc) {
        case (descr, command) => descr.add(", ").inline_code(command)
      }
    }

    def lint(command: Parsed_Command, report: Reporter): Option[Result] =
      if (illegal_commands.contains(command.kind))
        report(message, command.range, Some(Edit(command.range, "", Some("Remove invocation"))))
      else None
  }

  val unfinished_proof = Lint_Wrapper("unfinished_proof", (name, severity) =>
    new Illegal_Command_Lint(name, severity,
      "Consider finishing the proof.",
      List("sorry", "\\<proof>"),
      "This lint detects unfinished proofs, characterized by the following commands"))

  val smt_oracle = Lint_Wrapper("smt_oracle",
    (name, severity) => new Parser_Lint(name, severity) {
    val short_description = Lint_Description.empty
      .add("SMT oracle proofs should not be trusted, as they might not be suitable Isabelle proofs.")
    val long_description = Lint_Description.empty
      .add("Using ").inline_code("declare [[smt_oracle]]").add(" will make all smt act as an oracle.")
      .add(" This might prove to be problematic, as oracle proofs are usually not to be trusted.")

    private def is_smt_oracle(attr: List[Elem]): Boolean =
      attr.headOption.exists(_.info.content == "smt_oracle")

    override def parser(report: Reporter): Parser[Some[Result]] =
      p_command("declare") ~> p_sq_bracketed(p_sq_bracketed(
        p_attributes >> {
          _.find(is_smt_oracle) match {
            case None => failure("no match")
            case Some(tokens) =>
              success(report("Do not use smt_oracle.", tokens.head.range, None))
          }
        }))
  })
  
  val proof_finder = Lint_Wrapper("proof_finder", (name, severity) =>
    new Illegal_Command_Lint(name, severity,
      "Remove proof finder command.",
      List("sledgehammer", "solve_direct", "try", "try0"),
      "This lint detects proof-finder commands"))

  val bad_style_command = Lint_Wrapper("bad_style_command", (name, severity) =>
    new Illegal_Command_Lint(name, severity,
      "Bad style command.",
      List("back", "apply_end"),
      "This lint detects bad-style commands"))

  val diagnostic_command = Lint_Wrapper("diagnostic_command", (name, severity) =>
    new Illegal_Command_Lint(name, severity,
      "Remove interactive diagnostic command",
      List("ML_val", "class_deps", "code_deps", "code_thms", "find_consts", "find_theorems",
        "find_unused_assms", "full_prf", "help", "locale_deps", "prf", "print_ML_antiquotations",
        "print_abbrevs", "print_antiquotations", "print_attributes", "print_bnfs", "print_bundles",
        "print_case_translations", "print_cases", "print_claset", "print_classes", "print_codeproc",
        "print_codesetup", "print_coercions", "print_commands", "print_context", 
        "print_definitions", "print_defn_rules", "print_facts", "print_induct_rules",
        "print_inductives", "print_interps", "print_locale", "print_locales", "print_methods",
        "print_options", "print_orders", "print_quot_maps", "print_quotconsts", "print_quotients",
        "print_quotientsQ3", "print_quotmapsQ3", "print_record", "print_rules", "print_simpset",
        "print_state", "print_statement", "print_syntax", "print_term_bindings", "print_theorems",
        "print_theory", "print_trans_rules", "smt_status", "thm_deps", "thm_oracles", "thy_deps",
        "unused_thms", "welcome", "term", "prop", "thm", "typ"),
      "This lint finds diagnostic commands"))

  val counter_example_finder = Lint_Wrapper("counter_example_finder",
    (name, severity) => new AST_Lint(name, severity) {

    val description_start: Lint_Description =
      Lint_Description.empty.add("This lint detects counter-example finders with no specific purpose")

    val short_description: Lint_Description =
      description_start.add(".")

    override val long_description =
      description_start.add(": ").inline_code("nitpick").add(", ").inline_code("nunchaku")
        .add(", and ").inline_code("quickcheck").add(" (without ").inline_code("expect").add(" or ")
        .inline_code("satisfy").add(".")

    val ok_attribs = List("expect", "satisfy")

    def no_expect(attributes: List[List[Text.Info[Token]]]): Boolean =
      !attributes.exists(_.headOption.exists(text => ok_attribs.contains(text.info.content)))

    override def lint_ast_node(elem: Text.Info[AST_Node], report: Reporter): Option[Result] =
      elem.info match {
        case Counter_Example_Finder(_, attributes) if attributes.isEmpty || no_expect(attributes)
        => report(
          "Remove counter-example finder command.",
          elem.range,
          Some(Edit(elem.range, "", Some("Remove invocation"))))
        case _ => None
      }
  })

  val short_name = Lint_Wrapper("short_name",
    (name, severity) => new Parser_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty.add("Finds functions or definitions with short names (one character).")

    val long_description: Lint_Description = short_description

    override def parser(report: Reporter): Parser[Some[Result]] =
      p_command("fun", "primrec", "abbreviation", "definition", "inductive", "inductive_set") ~>
        elem("ident", t => t.info.is_ident && t.info.content.length < 2) ^^ {
          case Text.Info(range, token) =>
            report(s"""Name "${token.content}" is too short.""", range, None)
        }
  })

  val global_attribute_on_unnamed_lemma = Lint_Wrapper("global_attribute_on_unnamed_lemma",
    (name, severity) => new Parser_Lint(name, severity) {

    val GLOBAL_ATTRIBUTES = List("simp", "cong", "intro", "elim", "dest")

    val short_description: Lint_Description =
      Lint_Description.empty.add("Setting global attributes on unnamed lemmas should be avoided.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Setting a global attribute (like ").inline_code("simp").add(" or ").inline_code("elim")
        .add(") on an unnamed lemma should be avoided, since it can make debugging proofs")
        .add(" and removing the effect of that attribute harder.")
        .references("http://proofcraft.org/blog/isabelle-style.html")

    private def simp_or_cong(attr: List[Elem]): Boolean = attr match {
      case head :: _ => GLOBAL_ATTRIBUTES.contains(head.info.content)
      case _ => false
    }

    private def p_attr: Parser[(Option[Elem], Option[Elem])] =
      (p_ident.? ~ p_sq_bracketed(p_attributes).?) ^^ {
        case name ~ Some(attrs) => name -> attrs.find(simp_or_cong).map(_.head)
        case name ~ None => name -> None
      }

    private def p_show: Parser[(Option[Elem], Option[Elem])] =
      p_atom(t => !(t.is_keyword("shows") || t.is_keyword("and"))).* ~>
        (p_keyword("shows", "and") ~> p_attr | p_keyword("shows", "and")^^(_ => (None, None)))

    override def parser(reporter: Reporter): Parser[Some[Result]] = {
      def report(attr: Elem): Parser[Some[Result]] =
        success(reporter("Do not use global attributes on unnamed lemmas.", attr.range, None))

      ((p_command("lemma", "theorem", "corollary") ~> p_attr) ~
        (p_atom(t => !t.is_keyword("shows")).* ~> p_show.*)) >> {
        case (Some(_), _) ~ _ => failure("no match")
        case (None, Some(attr)) ~ Nil => report(attr)
        case (None, Some(attr)) ~ shows =>
          shows.collectFirst { case (None, _) => attr } match {
            case Some(attr) => report(attr)
            case None => failure("no match")
          }
        case (None, None) ~ shows =>
          shows.collectFirst { case (None, Some(attr)) => attr } match {
            case Some(attr) => report(attr)
            case None => failure("no match")
          }
      }
    }
  })

  val tactic_proofs = Lint_Wrapper("tactic_proofs",
    (name, severity) => new AST_Lint(name, severity) {

    private val TACTICS = List("induct_tac", "rule_tac", "case_tac")

    val short_description: Lint_Description =
      Lint_Description.empty.add("When using tactics, avoid outdated ")
        .inline_code("induct_tac")
        .add(", and do not refer to system-generated names.")

    val long_description: Lint_Description =
      short_description
        .add(" The lints warns about using the following methods: ")
        .inline_code(TACTICS.mkString(", "))

    override def lint_method(method: Text.Info[Method], report: Reporter): Option[Result] =
      method.info match {
        case Simple_Method(RToken(_, name, _), _, _) if TACTICS.contains(name) =>
          report("Do not use tactic proofs.", method.range, None)
        case Combined_Method(left, _, right, _) =>
          lint_method(left, report).orElse(lint_method(right, report))
        case _ => None
      }
  })

  val lemma_transforming_attribute = Lint_Wrapper("lemma_transforming_attribute",
    (name, severity) => new Parser_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Setting lemma-transforming attributes on lemmas should be avoided,")
        .add(" instead the transformed form should be used.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("This lint warns of using transforming attributes (")
        .inline_code("simplified").add(", ").inline_code("rule_format").inline_code(", and")
        .inline_code("unfolded").add(") on lemmas. Instead, the user should write the transformed form.")
        .references("http://proofcraft.org/blog/isabelle-style-part2.html")

    private def simp_or_cong(attr: List[Elem]): Boolean = attr match {
      case head :: _ => List("simplified", "rule_format", "unfolded").contains(head.info.content)
      case _ => false
    }

    override def parser(report: Reporter): Parser[Some[Result]] =
      (p_command("lemma", "theorem", "corollary") ~ p_ident.?) ~> p_sq_bracketed(p_attributes) >> {
        _.find(simp_or_cong) match {
          case None => failure("no match")
          case Some(tokens) =>
            success(report("Do not use transforming attributes on lemmas.", tokens.head.range, None))
        }
      }
  })

  val implicit_rule = Lint_Wrapper("implicit_rule",
    (name, severity) => new AST_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Explicitly stating the used rule is preferred to using just ")
        .inline_code("rule").add(".")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Using ").inline_code("apply rule").add(" results in Isabelle finding the suitable ")
        .add("rule for the given context. However, if the process for finding the rule ")
        .add("changes in the future, the proof might break. Instead, users should ")
        .add("explicitly state which rule is needed.")
        .references("http://proofcraft.org/blog/isabelle-style.html")

    override def lint_method(method: Text.Info[Method], report: Reporter): Option[Result] =
      method.info match {
        case Simple_Method(RToken(_, "rule", _), _, Nil) =>
          report("Do not use implicit rule.", method.range, None)
        case Combined_Method(left, _, right, _) =>
          lint_method(left, report) orElse lint_method(right, report)
        case _ => None
      }
  })

  trait Complex_Methods {
    val modifier_threshold: Int
    val combinator_threshold: Int

    def has_complex_modifiers(method: Method): Boolean =
      method match {
        case Simple_Method(_, modifiers, _) => modifiers.length > modifier_threshold
        case Combined_Method(left, _, right, modifiers) =>
          modifiers.length > modifier_threshold || has_complex_modifiers(left.info) ||
            has_complex_modifiers(right.info)
      }

    def mkList(method: Method): List[Simple_Method] =
      method match {
        case s@Simple_Method(_, _, _) => s :: Nil
        case Combined_Method(left, _, right, _) => mkList(left.info) ::: mkList(right.info)
      }

    def is_complex_method(method: Method): Boolean =
      has_complex_modifiers(method) || mkList(method).length > combinator_threshold

    def is_complex(element: AST_Node): Boolean = element match {
      case Apply(method) => is_complex_method(method.info)
      case Isar_Proof(Some(method)) => is_complex_method(method.info)
      case _ => false
    }

    def is_complex(command: Parsed_Command): Boolean = is_complex(command.ast_node.info)
  }

  abstract class Complex_Methods_Lint(
    name: String, severity: Severity.Level,
    val modifier_threshold: Int,
    val combinator_threshold: Int
  ) extends AST_Lint(name, severity) with Complex_Methods

  val complex_isar_initial_method = Lint_Wrapper("complex_isar_initial_method",
    (name, severity) => new Complex_Methods_Lint(name, severity, 2, 4) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Using complex methods in the ").inline_code("proof").add(" command makes the proof")
        .add(" brittle and hard to read.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Initial ").inline_code("proof").add(" methods should be kept simple, in order ")
        .add("to keep the goals of the proof clear. For instance, not too many methods should ")
        .add("be combined. This lint finds complex methods in proof commands.")
        .references("http://proofcraft.org/blog/isabelle-style.html")

    override def lint_isar_proof(
      method: Option[Text.Info[Method]],
      report: Reporter
    ): Option[Result] =
      for {
        Text.Info(range, s_method) <- method
        if is_complex_method(s_method)
      } yield report("Keep initial proof methods simple.", range, None).get
  })

  val simplifier_isar_initial_method = Lint_Wrapper("simplifier_isar_initial_method",
    (name, severity) => new AST_Lint(name, severity) {
      val SIMPLIFIER_METHOD =
        List("simp", "simp_all", "fastforce", "slowsimp", "bestsimp", "force", "auto", "clarsimp")

      def calls_simplifier(method: Method): Boolean = method match {
        case Simple_Method(RToken(_, name, _), _, _) => SIMPLIFIER_METHOD.contains(name)
        case Combined_Method(left, _, right, _) =>
          calls_simplifier(left.info) || calls_simplifier(right.info)
      }

      val short_description: Lint_Description =
        Lint_Description.empty
          .add("Using the simplifier in the ").inline_code("proof").add(" command makes the proof")
          .add(" brittle and hard to read.")

      val long_description: Lint_Description =
        Lint_Description.empty
          .add("Initial ").inline_code("proof").add(" methods should not use the simplifier,")
          .add(" in order to keep the goals of the proof clear and unchanged.")
          .references("http://proofcraft.org/blog/isabelle-style.html")

      override def lint_isar_proof(
        method: Option[Text.Info[Method]],
        report: Reporter
      ): Option[Result] =
        for {
          Text.Info(range, s_method) <- method
          if calls_simplifier(s_method)
        } yield report("Do not use simplifier as initial proof methods.", range, None).get
    })

  val force_failure = Lint_Wrapper("force_failure",
    (name, severity) => new AST_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Forcing failure, for example ").inline_code("apply (simp; fail)")
        .add(", might be helpful.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Since some methods do not guarantee to solve all their goals, it might be ")
        .add("helpful to consider forcing their failure (e.g. using ")
        .inline_code("apply (simp; fail)").add(" instead of just ").inline_code("apply simp")
        .add(") in order to make debugging proofs easier.")
        .references("http://proofcraft.org/blog/isabelle-style-part2.html")

    override def lint_method(method: Text.Info[Method], report: Reporter): Option[Result] =
      method.info match {
        case Simple_Method(RToken(_, "simp", _), _, _) =>
          report("Consider forcing failure.", method.range, None)
        case _ => None
      }
  })

  val auto_structural_composition = Lint_Wrapper("auto_structural_composition",
    (name, severity) => new AST_Lint(name, severity) {

    val short_description: Lint_Description =
      Lint_Description.empty
        .add("Using ").inline_code("apply (auto;…)").add(" is discouraged.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .add("Using ").inline_code("apply (auto;…)").add(" results in a behavior that is hard to predict, ")
        .add("so it is discouraged.")
        .references("http://proofcraft.org/blog/isabelle-style-part2.html")

    private def has_auto(method: Method): Boolean = method match {
      case Simple_Method(name, _, _) => name.info.source == "auto"
      case Combined_Method(left, _, right, _) =>
        has_auto(left.info) || has_auto(right.info)
    }

    override def lint_method(method: Text.Info[Method], report: Reporter): Option[Result] =
      method.info match {
        case Simple_Method(_, _, _) => None
        case Combined_Method(left, Method.Combinator.Struct, _, _) =>
          if (has_auto(left.info)) report("Do not use apply (auto;...)", method.range, None) else None
        case Combined_Method(left, _, right, _) =>
          lint_method(left, report).orElse(lint_method(right, report))
      }
  })


  val complex_method = Lint_Wrapper("complex_method",
    (name, severity) => new Complex_Methods_Lint(name, severity, 2, 4) {

    val short_description: Lint_Description =
      Lint_Description.empty.add("Using complex methods (e.g. too much nesting) should be avoided.")

    val long_description: Lint_Description =
      Lint_Description.empty
        .addln("Warns users from using overly complex methods, i.e. if one of the following holds:")
        .breakline
        .add("- has more than one modifier (").inline_code("?, +, or []").add("), for example ").inline_code("auto?[4]")
        .breakline
        .add("- has three or more combinators (").inline_code("|, ;, ,").add("), for example ")
        .inline_code("auto ; rule , (force | blast)").breakline

    val message: String = "Avoid complex methods."

    override def lint_method(method: Text.Info[Method], report: Reporter): Option[Result] =
      if (is_complex_method(method.info)) report(message, method.range, None)
      else None
  })

  val print_ast = Lint_Wrapper("print_ast", (name, severity) => new AST_Lint(name, severity) {
    val short_description: Lint_Description =
      Lint_Description.empty.add("Debugging lint. Prints the AST of the active command.")

    val long_description: Lint_Description = short_description

    override def lint_ast_node(elem: Text.Info[AST_Node], report: Reporter): Option[Result] =
      report(s"Parsed: ${elem.info}", elem.range, None)
  })
}

class Isabelle_Lints(val lints: Lint_Wrapper*) extends Isabelle_System.Service

class Lints extends Isabelle_Lints(
  Lints.apply_isar_switch,
  Lints.auto_structural_composition,
  Lints.axiomatization_with_where,
  Lints.bad_style_command,
  Lints.complex_isar_initial_method,
  Lints.simplifier_isar_initial_method,
  Lints.complex_method,
  Lints.counter_example_finder,
  Lints.diagnostic_command,
  Lints.force_failure,
  Lints.global_attribute_changes,
  Lints.global_attribute_on_unnamed_lemma,
  Lints.implicit_rule,
  Lints.lemma_transforming_attribute,
  Lints.low_level_apply_chain,
  Lints.proof_finder,
  Lints.short_name,
  Lints.smt_oracle,
  Lints.tactic_proofs,
  Lints.unfinished_proof,
  Lints.unrestricted_auto,
  Lints.use_apply,
  Lints.use_by,
  Lints.use_isar)
