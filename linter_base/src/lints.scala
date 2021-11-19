package isabelle.linter

import Linter._

import isabelle._
import scala.annotation.tailrec

object Apply_Isar_Switch extends Proper_Commands_Lint {

  val name = "apply_isar_switch"
  val severity: Severity.Level = Severity.Warn

  @tailrec
  def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
    commands match {
      case Parsed_Command("apply") :: (proof @ Parsed_Command("proof")) :: next => {
        val new_report = add_result(
          "Do not switch between apply-style and ISAR proofs.",
          proof.range,
          None,
          proof,
          report
        )
        lint_proper(next, new_report)
      }
      case _ :: next => lint_proper(next, report)
      case Nil       => report
    }
}

object Use_By extends Proper_Commands_Lint with TokenParsers {

  val name: String = "use_by"
  val severity: Severity.Level = Severity.Info

  private def removeApply: Parser[String] = (pCommand("apply") ~ pSpace.?) ~> pAny.* ^^ mkString

  private def removeBy: Parser[String] = (pCommand("by") ~ pSpace.?) ~> pAny.* ^^ mkString

  private def gen_replacement(
      apply_script: List[Parsed_Command],
      has_by: Boolean = false
  ): Option[String] =
    apply_script match {
      case apply1 :: apply2 :: done :: Nil =>
        for {
          method1 <- tryTransform(removeApply, apply1, true)
          method2 <- tryTransform(removeApply, apply2, true)
        } yield s"by $method1 $method2"
      case apply :: done :: Nil if (!has_by) =>
        for {
          method <- tryTransform(removeApply, apply, true)
        } yield s"by $method"
      case apply :: by :: Nil if (has_by) =>
        for {
          method1 <- tryTransform(removeApply, apply, true)
          method2 <- tryTransform(removeBy, by, true)
        } yield s"by $method1 $method2"
      case _ => None
    }

  private def report_lint(
      apply_script: List[Parsed_Command],
      report: Lint_Report,
      has_by: Boolean = false
  ): Lint_Report = {
    val new_report = for {
      replacement <- gen_replacement(apply_script, has_by)
    } yield add_result(
      """Use "by" instead of a short apply-script.""",
      list_range(apply_script.map(_.range)),
      Some(Edit(list_range(apply_script map (_.range)), replacement)),
      apply_script,
      report
    )
    new_report.getOrElse(report)
  }

  private def check_first_command(command: Parsed_Command): Boolean =
    List("lemma", "theorem", "corollary").contains(command.kind)

  private def single_by(command: Parsed_Command): Boolean =
    command.ast_node.info match {
      case By(_, None) => true
      case _           => false
    }

  @tailrec
  def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
    commands match {
      case first
          :: (apply1 @ Parsed_Command("apply"))
          :: (apply2 @ Parsed_Command("apply"))
          :: (done @ Parsed_Command("done"))
          :: next
          if (check_first_command(first)
            && !(Complex_Method.is_complex(apply1) || Complex_Method.is_complex(apply2))) =>
        lint_proper(next, report_lint(apply1 :: apply2 :: done :: Nil, report))
      case first
          :: (apply @ Parsed_Command("apply"))
          :: (done @ Parsed_Command("done"))
          :: next if (check_first_command(first) && !Complex_Method.is_complex(apply)) =>
        lint_proper(next, report_lint(apply :: done :: Nil, report))
      case first
          :: (apply @ Parsed_Command("apply"))
          :: (by @ Parsed_Command("by"))
          :: next
          if (check_first_command(first) && !Complex_Method.is_complex(apply) && single_by(by)) =>
        lint_proper(next, report_lint(apply :: by :: Nil, report, has_by = true))
      case _ :: next => lint_proper(next, report)
      case Nil       => report
    }

}

object Unrestricted_Auto extends Proper_Commands_Lint {
  val name: String = "unrestricted_auto"
  val severity: Severity.Level = Severity.Error

  private def is_terminal(command: Parsed_Command): Boolean =
    List("sorry", "oops", "done", "\\<proof>").contains(command.kind)

  private def are_unrestricted(modifiers: List[Method.Modifier]): Boolean =
    !modifiers.exists(_.isInstanceOf[Method.Modifier.Restrict])

  private def is_unrestricted_auto__method(method: Method): Boolean = method match {

    case Simple_Method(name, modifiers, args) =>
      name.info.content == "auto" && are_unrestricted(modifiers.map(_.info))

    case _ => false
  }

  private def is_unrestricted_auto(element: ASTNode): Boolean = element match {
    case Apply(method) => is_unrestricted_auto__method(method.info)
    case _             => false
  }

  private def report_lint(apply: Parsed_Command, report: Lint_Report): Lint_Report =
    add_result(
      "Do not use unrestricted auto as a non-terminal proof method.",
      apply.range,
      None,
      apply,
      report
    )

  @tailrec
  def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
    commands match {
      case (apply @ Parsed_Command("apply")) :: next_command :: next
          if !is_terminal(next_command) && is_unrestricted_auto(apply.ast_node.info) =>
        lint_proper(next_command :: next, report_lint(apply, report))
      case _ :: next => lint_proper(next, report)
      case Nil       => report
    }
}

object Low_Level_Apply_Chain extends Proper_Commands_Lint {

  val name: String = "low_level_apply_chain"
  val severity: Severity.Level = Severity.Info

  val LOW_LEVEL_RULES = List(
    "erule",
    "rule",
    "simp",
    "clarsimp",
    "rule_tac",
    "frule",
    "erule",
    "drule",
    "subst",
    "rewrite",
    "unfold"
  )

  private def is_low_level_method(method: Method): Boolean = method match {
    case Simple_Method(name, modifiers, args) =>
      LOW_LEVEL_RULES.contains(name.info.content)
    case _ => false
  }

  private def is_low_level_apply(command: Parsed_Command): Boolean = command.ast_node.info match {
    case Apply(method) => is_low_level_method(method.info)
    case _             => false
  }

  def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report = {
    val (low_level_commands, rest) =
      commands.dropWhile(!is_low_level_apply(_)).span(is_low_level_apply(_))

    val new_report =
      if (low_level_commands.length >= 5)
        add_result(
          "Compress low-level proof methods into automated search.",
          list_range(low_level_commands.map(_.range)),
          None,
          low_level_commands,
          report
        )
      else
        report

    if (rest.isEmpty)
      new_report
    else lint_proper(rest, new_report)
  }
}

object Global_Attribute_Changes extends Proper_Commands_Lint with TokenParsers {

  val name: String = "global_attribute_changes"
  val severity: Severity.Level = Severity.Info

  type Declaration = (String, List[String]) // Identifier, attribute list without whitespaces

  private def declaration: Parser[Declaration] = pIdent ~ pSqBracketed(pAttributes) ^^ {
    case identifier ~ attributes => (identifier.info.content, attributes map mkString)
  }

  private def declare_command: Parser[List[Declaration]] =
    pCommand("declare") ~> chainl1[List[Declaration]](
      declaration ^^ { List(_) },
      pKeyword("and") ^^^ { _ ::: _ }
    )

  private def has_simp(attrs: List[String]): Boolean =
    attrs.exists(attr => attr == "simp" || attr == "simpadd")

  private def has_simp_del(attrs: List[String]): Boolean =
    attrs.exists(_ == "simpdel") // Whitespaces are ignored

  private def proces_declaration(command: Parsed_Command)(
      report_simpset: (Lint_Report, Set[String]),
      declaration: Declaration
  ): (Lint_Report, Set[String]) = {
    val (ident, attrs) = declaration
    val (report, simpset) = report_simpset
    val new_report =
      if (simpset.contains(ident) && has_simp_del(attrs))
        add_result(
          "Use context or bundles instead of global simp attribute changes.",
          command.range,
          None,
          command,
          report
        )
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
      report: Lint_Report,
      simpset: Set[String]
  ): Lint_Report = commands match {
    case (head @ Parsed_Command("declare")) :: next =>
      tryTransform(declare_command, head) match {
        case Some(decls) =>
          val (new_report, new_simpset) =
            decls.foldLeft((report, simpset))(proces_declaration(head))
          go(next, new_report, new_simpset)
        case None => go(next, report, simpset)
      }

    case _ :: next => go(next, report, simpset)
    case Nil       => report
  }

  def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
    go(commands, report, Set.empty)
}

object Use_Isar extends Single_Command_Lint {

  val name: String = "use_isar"
  val severity: Severity.Level = Severity.Info

  def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] = command match {
    case (c @ Parsed_Command("apply")) =>
      report("Use Isar instead of apply-scripts.", c.range, None)
    case _ => None
  }
}

object Axiomatization_With_Where extends Single_Command_Lint {

  val name: String = "axiomatization_with_where"
  val severity: Severity.Level = Severity.Error

  def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] = command.tokens match {
    case RToken(Token.Kind.COMMAND, "axiomatization", range) :: next =>
      next.dropWhile(_.info.source != "where") match {
        case xs @ (_ :: _) =>
          report(
            """Do not use axiomatization with a where clause.""",
            Text.Range(xs.head.range.start, xs.last.range.stop),
            Some(Edit(list_range(xs.map(_.range)), "", Some("Remove where")))
          )
        case Nil => None
      }
    case _ => None
  }
}

abstract class Illegal_Command_Lint(
    message: String,
    lint_name: String,
    illegal_commands: List[String],
    lint_severity: Severity.Level
) extends Single_Command_Lint {

  val name: String = lint_name
  val severity: Severity.Level = lint_severity

  def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] =
    if (illegal_commands.contains(command.kind))
      report(message, command.range, Some(Edit(command.range, "", Some("Remove invocation"))))
    else None
}

object Unfinished_Proof
    extends Illegal_Command_Lint(
      "Consider finishing the proof.",
      "unfinished_proof",
      List("sorry", "oops", "\\<proof>"),
      Severity.Error
    )

object Proof_Finder
    extends Illegal_Command_Lint(
      "Remove proof finder command.",
      "proof_finder",
      List(
        "sledgehammer",
        "solve_direct",
        "try",
        "try0"
      ),
      Severity.Error
    )

object Counter_Example_Finder
    extends Illegal_Command_Lint(
      "Remove counter-example finder command.",
      "counter_example_finder",
      List(
        "nitpick",
        "nunchaku",
        "quickcheck"
      ),
      Severity.Error
    )

object Bad_Style_Command
    extends Illegal_Command_Lint(
      "Bad style command.",
      "bad_style_command",
      List("back", "apply_end"),
      Severity.Warn
    )

object Diagnostic_Command
    extends Illegal_Command_Lint(
      "Remove interactive diagnostic command",
      "diagnostic_command",
      List(
        "ML_val",
        "class_deps",
        "code_deps",
        "code_thms",
        "find_consts",
        "find_theorems",
        "find_unused_assms",
        "full_prf",
        "help",
        "locale_deps",
        "prf",
        "print_ML_antiquotations",
        "print_abbrevs",
        "print_antiquotations",
        "print_attributes",
        "print_bnfs",
        "print_bundles",
        "print_case_translations",
        "print_cases",
        "print_claset",
        "print_classes",
        "print_codeproc",
        "print_codesetup",
        "print_coercions",
        "print_commands",
        "print_context",
        "print_definitions",
        "print_defn_rules",
        "print_facts",
        "print_induct_rules",
        "print_inductives",
        "print_interps",
        "print_locale",
        "print_locales",
        "print_methods",
        "print_options",
        "print_orders",
        "print_quot_maps",
        "print_quotconsts",
        "print_quotients",
        "print_quotientsQ3",
        "print_quotmapsQ3",
        "print_record",
        "print_rules",
        "print_simpset",
        "print_state",
        "print_statement",
        "print_syntax",
        "print_term_bindings",
        "print_theorems",
        "print_theory",
        "print_trans_rules",
        "smt_status",
        "thm_deps",
        "thm_oracles",
        "thy_deps",
        "unused_thms",
        "value",
        "values",
        "welcome",
        "term",
        "prop",
        "thm",
        "typ"
      ),
      Severity.Info
    )

object Short_Name extends Parser_Lint {

  val name: String = "short_name"
  val severity: Severity.Level = Severity.Info

  override def parser(report: Reporter): Parser[Some[Lint_Result]] =
    pCommand("fun", "definition") ~> elem("ident", _.info.content.size < 2) ^^ {
      case Text.Info(range, token) =>
        report(s"""Name "${token.content}" is too short.""", range, None)
    }
}

object Global_Attribute_On_Unnamed_Lemma extends Parser_Lint {

  val name: String = "global_attribute_on_unnamed_lemma"
  val severity: Severity.Level = Severity.Error

  private def simp_or_cong(attr: List[Elem]): Boolean = attr match {
    case head :: _ => List("simp", "cong").contains(head.info.content)
    case _         => false
  }

  override def parser(report: Reporter): Parser[Some[Lint_Result]] =
    pCommand("lemma") ~> pSqBracketed(pAttributes) >> {
      _.find(simp_or_cong(_)) match {
        case None => failure("no match")
        case Some(tokens) =>
          success(
            report("Do not use simplifier attributes on unnamed lemmas.", tokens.head.range, None)
          )
      }
    }
}

object Lemma_Transforming_Attribute extends Parser_Lint {

  val name: String = "lemma_transforming_attribute"
  val severity: Severity.Level = Severity.Warn

  private def simp_or_cong(attr: List[Elem]): Boolean = attr match {
    case head :: _ => List("simplified", "rule_format", "unfolded").contains(head.info.content)
    case _         => false
  }

  override def parser(report: Reporter): Parser[Some[Lint_Result]] =
    (pCommand("lemma") ~ pIdent.?) ~> pSqBracketed(pAttributes) >> {
      _.find(simp_or_cong(_)) match {
        case None => failure("no match")
        case Some(tokens) =>
          success(report("Do not use transforming attributes on lemmas.", tokens.head.range, None))
      }
    }
}

object Implicit_Rule extends AST_Lint {

  val name: String = "implicit_rule"
  val severity: Severity.Level = Severity.Warn

  override def lint_method(method: Text.Info[Method], report: Reporter): Option[Lint_Result] =
    method.info match {
      case Simple_Method(RToken(_, "rule", _), _, Nil) =>
        report("Do not use implicit rule.", method.range, None)
      case Combined_Method(left, _, right, _) =>
        lint_method(left, report) orElse lint_method(right, report)
      case _ => None
    }
}

object Complex_Isar_Initial_Method extends AST_Lint {

  val name: String = "complex_isar_initial_method"
  val severity: Severity.Level = Severity.Warn

  val SIMPLIFIER_METHOD = List("auto", "simp", "clarsimp", "bestsimp", "slowsimp")

  def calls_simplifier(method: Method): Boolean = method match {
    case Simple_Method(RToken(_, name, _), _, _) => SIMPLIFIER_METHOD.contains(name)
    case Combined_Method(left, _, right, _) =>
      calls_simplifier(left.info) || calls_simplifier(right.info)
  }

  override def lint_isar_proof(
      method: Option[Text.Info[Method]],
      report: Reporter
  ): Option[Lint_Result] =
    for {
      Text.Info(range, s_method) <- method
      if calls_simplifier(s_method) || Complex_Method.is_complex_method(
        s_method,
        allow_modifiers = false
      )
    } yield report("Keep initial proof methods simple.", range, None).get
}

object Force_Failure extends AST_Lint {
  val name: String = "force_failure"
  val severity: Severity.Level = Severity.Info

  override def lint_method(method: Text.Info[Method], report: Reporter): Option[Lint_Result] =
    method.info match {
      case Simple_Method(RToken(_, "simp", _), modifiers, args) =>
        report("Consider forcing failure.", method.range, None)
      case _ => None
    }
}

object Auto_Structural_Composition extends AST_Lint {
  val name: String = "auto_structural_composition"
  val severity: Severity.Level = Severity.Info

  private def has_auto(method: Method): Boolean = method match {
    case Simple_Method(name, modifiers, args) => name.info.source == "auto"
    case Combined_Method(left, combinator, right, modifiers) =>
      has_auto(left.info) || has_auto(right.info)

  }

  override def lint_method(method: Text.Info[Method], report: Reporter): Option[Lint_Result] =
    method.info match {
      case Simple_Method(name, modifiers, args) => None
      case Combined_Method(left, Method.Combinator.Struct, right, modifiers) =>
        if (has_auto(left.info)) report("Do not use apply (auto;...)", method.range, None) else None
      case Combined_Method(left, _, right, modifiers) =>
        lint_method(left, report).orElse(lint_method(right, report))
    }
}

object Complex_Method extends AST_Lint {

  val name: String = "complex_method"
  val severity: Severity.Level = Severity.Warn

  val modifier_length: Int = 1
  val combinator_threshold: Int = 4
  val message: String = "Avoid complex methods."

  private def has_modifiers(method: Method): Boolean = method match {
    case Simple_Method(_, modifiers, _) => !modifiers.isEmpty
    case Combined_Method(left, _, right, modifiers) =>
      !modifiers.isEmpty || has_modifiers(left.info) || has_modifiers(right.info)
  }

  private def has_complex_modifiers(method: Method): Boolean = method match {
    case Simple_Method(_, modifiers, _) => modifiers.length > modifier_length
    case Combined_Method(left, _, right, modifiers) =>
      modifiers.length > modifier_length || has_modifiers(left.info) || has_modifiers(right.info)
  }

  private def mkList(method: Method): List[Simple_Method] = method match {
    case s @ Simple_Method(_, _, _)         => s :: Nil
    case Combined_Method(left, _, right, _) => mkList(left.info) ::: mkList(right.info)
  }

  private def has_many_combinators(method: Method): Boolean =
    mkList(method).length >= combinator_threshold

  def is_complex_method(method: Method, allow_modifiers: Boolean = true): Boolean =
    (if (allow_modifiers) has_complex_modifiers(method) else has_modifiers(method)) ||
      has_many_combinators(method)

  def is_complex(element: ASTNode): Boolean = element match {
    case Apply(method)            => is_complex_method(method.info)
    case Isar_Proof(Some(method)) => is_complex_method(method.info)
    case _                        => false
  }

  def is_complex(command: Parsed_Command): Boolean = is_complex(command.ast_node.info)

  override def lint_method(method: Text.Info[Method], report: Reporter): Option[Lint_Result] =
    if (is_complex_method(method.info)) report(message, method.range, None) else None

}

object Print_AST extends AST_Lint {

  val name: String = "print_structure"
  val severity: Severity.Level = Severity.Info

  override def lint_ast_node(
      elem: Text.Info[ASTNode],
      report: Reporter
  ): Option[Lint_Result] =
    report(s"Parsed: ${elem.info}", elem.range, None)
}
