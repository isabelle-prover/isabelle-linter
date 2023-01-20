/* Author: Yecine Megdiche and Fabian Huch, TU Muenchen

Registration and configuration for lints and lint bundles.
 */

package isabelle.linter


import Linter.*
import isabelle.*


object Lint_Store {
  lazy val known_lints: List[Lint_Wrapper] =
    Isabelle_System.make_services(classOf[Isabelle_Lints]).flatMap(_.lints)

  def get_lint(lint_name: String, severity: Severity.Level): Option[Lint] =
    known_lints.find(_.name == lint_name).map(_.get(severity))

  def print_lints(progress: Progress = new Progress): Unit = {
    val header = Utils.HTML.table_header(
      List("Name", "Description", "Bundles").map(HTML.text))
    val rows = known_lints.map(_.get(Lint_Wrapper.default_config)).map { lint =>
      val row =
        List(HTML.text(lint.name),
          Lint_Description.Markdown_Presentation.render(lint.long_description),
          HTML.text(commas(Bundle.get_bundles_for_lint(lint.name))))
      Utils.HTML.table_row(row)
    }
    progress.echo(XML.string_of_tree(Utils.HTML.mk_table(header, rows)))
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lint_descriptions", "print the list of lints with their descriptions",
    Scala_Project.here, 
  { args =>
    val getopts = Getopts("""
Usage: isabelle lint_descriptions

Print lint descriptions.
""")

    getopts(args)

    val progress = new Console_Progress()

    progress.interrupt_handler { print_lints(progress = progress) }
  })


  /* Bundles */

  case class Bundle(name: String,
    warnings: Set[String] = Set.empty,
    errors: Set[String] = Set.empty
  ) {
    def contains(lint_name : String): Boolean =
      warnings.contains(lint_name) || errors.contains(lint_name)
  }

  object Bundle {
    val afp_mandatory =
      Bundle("afp_mandatory", errors = Set(
        Lints.unfinished_proof.name,
        Lints.bad_style_command.name,
        Lints.counter_example_finder.name,
        Lints.global_attribute_on_unnamed_lemma.name,
        Lints.smt_oracle.name))

    val foundational =
      Bundle("foundational", warnings = Set(
        Lints.apply_isar_switch.name,
        Lints.auto_structural_composition.name,
        Lints.bad_style_command.name,
        Lints.complex_isar_initial_method.name,
        Lints.complex_method.name,
        Lints.global_attribute_changes.name,
        Lints.global_attribute_on_unnamed_lemma.name,
        Lints.implicit_rule.name,
        Lints.lemma_transforming_attribute.name,
        Lints.low_level_apply_chain.name,
        Lints.implicit_rule.name,
        Lints.tactic_proofs.name,
        Lints.unrestricted_auto.name))

    val default =
      afp_mandatory.copy(name = "default",
        warnings = foundational.warnings ++ Set(Lints.short_name.name, Lints.smt_oracle.name),
        errors = foundational.errors ++ Set(Lints.axiomatization_with_where.name))


    /* Add-on bundles */

    val non_interactive_addon =
      Bundle("non_interactive_addon",
        warnings =
          Set(Lints.counter_example_finder.name, Lints.diagnostic_command.name, Lints.proof_finder.name),
        errors = Set(Lints.unfinished_proof.name))

    val pedantic_addon = Bundle("pedantic_addon", warnings = Set(Lints.force_failure.name, Lints.use_isar.name))

    private var store: Map[String, Bundle] = Map.empty

    def register_bundle(bundle: Bundle): Unit = { store += ((bundle.name, bundle)) }

    for (bundle <- List(
      Bundle.default,
      Bundle.afp_mandatory,
      Bundle.non_interactive_addon,
      Bundle.foundational,
      Bundle.pedantic_addon))
    {  register_bundle(bundle) }

    def get_bundle(name: String): Option[Bundle] = store.get(name)

    def bundles: List[Bundle] = store.values.toList

    def get_bundles_for_lint(lint_name: String): List[String] = {
      for {
        bundle <- bundles
        if bundle.contains(lint_name)
      } yield bundle.name
    }

    def print_bundles(progress: Progress = new Progress): Unit = {
      val header = Utils.HTML.table_header(
        List(HTML.text("Bundle Name"), HTML.text("Warnings"), HTML.text("Errors")))
      val rows = bundles.map { bundle =>
        Utils.HTML.table_row(List(
          HTML.text(bundle.name),
          HTML.text(commas(bundle.warnings.toList.sorted)),
          HTML.text(commas(bundle.errors.toList.sorted))))
      }

      progress.echo(XML.string_of_tree(Utils.HTML.mk_table(header, rows)))
    }


    /* Isabelle tool wrapper */

    val isabelle_tool = Isabelle_Tool("lint_bundles", "print the lints belonging to each bundle.",
      Scala_Project.here,
    { args =>
      val getopts = Getopts("""
Usage: isabelle lint_bundles

print the lints belonging to each bundle.
""")

      getopts(args)

      val progress = new Console_Progress()

      progress.interrupt_handler { print_bundles(progress = progress) }
    })
  }


  /* Configurations */

  object Selection {
    def apply(options: Options): Selection = {
      val bundles = space_explode(',', options.string("lint_bundles"))
      val warnings = space_explode(',', options.string("lints_warning"))
      val errors = space_explode(',', options.string("lints_error"))
      val disabled = space_explode(',', options.string("lints_disabled"))

      Selection.empty
        .add_bundles(bundles)
        .enable_lints(warnings, Severity.Warn)
        .enable_lints(errors, Severity.Error)
        .disable_lints(disabled)
    }

    def empty: Selection = new Selection(Set.empty, Set.empty)
  }

  case class Selection(warnings: Set[String], errors: Set[String]) {
    def enable_lint(lint_name: String, severity: Severity.Level = Severity.Warn): Selection =
      severity match {
        case Severity.Warn => copy(warnings = warnings + lint_name)
        case Severity.Error => copy(errors = errors + lint_name)
      }

    def enable_lints(lint_names: List[String], severity: Severity.Level): Selection =
      lint_names.foldLeft(this)((config, lint) => config.enable_lint(lint, severity))

    def disable_lint(lint_name: String): Selection =
      copy(warnings = warnings - lint_name, errors - lint_name)

    def disable_lints(lint_names: List[String]): Selection =
      lint_names.foldLeft(this)((config, lint) => config.disable_lint(lint))

    def add_bundle(bundle_name: String): Selection =
      Bundle.get_bundle(bundle_name).map(bundle =>
        enable_lints(bundle.warnings.toList, Severity.Warn).enable_lints(bundle.errors.toList, Severity.Error)).getOrElse(this)

    def add_bundles(bundle_names: List[String]): Selection =
      bundle_names.foldLeft(this)((config, bundle) => config.add_bundle(bundle))

    def get_lints: List[Linter.Lint] =
      warnings.flatMap(get_lint(_, Severity.Warn)).toList ++
        errors.flatMap(get_lint(_, Severity.Error))
  }
}
