/* Author: Yecine Megdiche, TU Muenchen

Registration and configuration for lints and lint bundles.
 */

package isabelle.linter


import Linter._
import isabelle._


object Lint_Store
{
  private var store: Map[String, Lint] = Map.empty

  def register_lint(lint: Lint): Unit =
    store += ((lint.name, lint))

  def get_lint(lint_name: String): Option[Lint] = store.get(lint_name)

  private val lints: List[Lint] = List(
    Apply_Isar_Switch,
    Auto_Structural_Composition,
    Axiomatization_With_Where,
    Bad_Style_Command,
    Complex_Isar_Initial_Method,
    Complex_Method,
    Counter_Example_Finder_Lint,
    Diagnostic_Command,
    Force_Failure,
    Global_Attribute_Changes,
    Global_Attribute_On_Unnamed_Lemma,
    Implicit_Rule,
    Lemma_Transforming_Attribute,
    Low_Level_Apply_Chain,
    Proof_Finder,
    Short_Name,
    Tactic_Proofs,
    Unfinished_Proof,
    Unrestricted_Auto,
    Use_Apply,
    Use_By,
    Use_Isar)

  private val all_lints: List[Lint] = Print_AST :: lints

  for (lint <- all_lints) register_lint(lint)


  /* Bundles */

  case class Bundle(name: String, lint_names: Set[String]) {

    def contains(lint_name : String): Boolean =
      lint_names.contains(lint_name)
  }

  object Bundle
  {
    val pedantic = Bundle("pedantic", Set(
      Low_Level_Apply_Chain.name,
      Use_Isar.name,
      Short_Name.name,
      Force_Failure.name,
      Auto_Structural_Composition.name))

    val non_interactive = Bundle("non_interactive", Set(
        Unfinished_Proof.name,
        Proof_Finder.name,
        Counter_Example_Finder_Lint.name,
        Diagnostic_Command.name))

    val foundational = Bundle("foundational", Set(
        Apply_Isar_Switch.name,
        Bad_Style_Command.name,
        Complex_Isar_Initial_Method.name,
        Complex_Method.name,
        Global_Attribute_Changes.name,
        Global_Attribute_On_Unnamed_Lemma.name,
        Lemma_Transforming_Attribute.name,
        Implicit_Rule.name,
        Unrestricted_Auto.name,
        Use_By.name))

    val afp = Bundle("afp", Set(
        Apply_Isar_Switch.name,
        Bad_Style_Command.name,
        Complex_Isar_Initial_Method.name,
        Counter_Example_Finder_Lint.name,
        Complex_Method.name,
        Global_Attribute_Changes.name,
        Global_Attribute_On_Unnamed_Lemma.name,
        Lemma_Transforming_Attribute.name,
        Implicit_Rule.name,
        Unrestricted_Auto.name,
        Use_By.name))

    val default = Bundle("default", Set(
        Apply_Isar_Switch.name,
        Unrestricted_Auto.name,
        Low_Level_Apply_Chain.name,
        Axiomatization_With_Where.name,
        Bad_Style_Command.name,
        Global_Attribute_On_Unnamed_Lemma.name,
        Lemma_Transforming_Attribute.name,
        Implicit_Rule.name,
        Complex_Isar_Initial_Method.name,
        Global_Attribute_Changes.name,
        Complex_Method.name,
        Tactic_Proofs.name,
        Counter_Example_Finder_Lint.name))

    val all = Bundle("all", all_lints.map(_.name).toSet)

    val bundles = List(
      Bundle.all,
      Bundle.default,
      Bundle.afp,
      Bundle.non_interactive,
      Bundle.foundational,
      Bundle.pedantic)

    def print_bundles(progress: Progress = new Progress): Unit =
    {
      val header = Utils.HTML.table_header(
        List(HTML.text("Bundle Name"), HTML.text("Lints")))
      val rows = bundles.map { bundle =>
        Utils.HTML.table_row(List(
          HTML.text(bundle.name),
          HTML.text(commas(bundle.lint_names.toList.sorted))))
      }

      progress.echo(XML.string_of_tree(Utils.HTML.mk_table(header, rows)))
    }

    /* Isabelle tool wrapper */

    val isabelle_tool = Isabelle_Tool("lint_bundles", "print the lints belonging to each bundle.",
      Scala_Project.here, args =>
    {
      val getopts = Getopts("""
Usage: isabelle lint_bundles

print the lints belonging to each bundle.
""")

      getopts(args)

      val progress = new Console_Progress()

      progress.interrupt_handler { print_bundles(progress = progress) }
    })
  }

  private var bundle_store: Map[String, Bundle] = Map.empty

  def register_bundle(bundle: Bundle): Unit =
    bundle_store += ((bundle.name, bundle))

  def get_bundle(name: String): Option[Bundle] =
    bundle_store.get(name)


  for (bundle <- Bundle.bundles) register_bundle(bundle)

  def get_bundles_for_lint(lint_name: String): List[String] =
  {
    for {
      bundle <- Bundle.bundles
      if bundle != Bundle.all && bundle.contains(lint_name)
    } yield bundle.name
  }

  def print_lints(progress: Progress = new Progress): Unit =
  {
    val header = Utils.HTML.table_header(
      List("Name", "Severity", "Short description", "Description", "Bundles").map(HTML.text))
    val rows = Lint_Store.lints.map { lint =>
      val row = List(
        lint.name, lint.severity.toString, Markdown_Renderer.render(lint.short_description),
        Markdown_Renderer.render(lint.long_description), commas(get_bundles_for_lint(lint.name)))
      Utils.HTML.table_row(row.map(HTML.text))
    }
    progress.echo(XML.string_of_tree(Utils.HTML.mk_table(header, rows)))
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lint_descriptions", "print the list of lints with their descriptions",
    Scala_Project.here, args =>
    {
      val getopts = Getopts("""
Usage: isabelle lint_descriptions

Print lint descriptions.
""")

      getopts(args)

      val progress = new Console_Progress()

      progress.interrupt_handler { print_lints(progress = progress) }
    })


  /* Configurations */

  object Configuration
  {
    val ENABLED_BUNDLES_OPTION = "enabled_bundles"
    val ENABLED_LINTS_OPTION = "enabled_lints"
    val DISABLED_LINTS_OPTION = "disabled_lints"

    def apply(options: Options): Configuration =
    {
      val bundles = space_explode(',', options.string(ENABLED_BUNDLES_OPTION))
      val enabled_lints = space_explode(',', options.string(ENABLED_LINTS_OPTION))
      val disabled_lints = space_explode(',', options.string(DISABLED_LINTS_OPTION))

      Configuration.empty
        .add_bundles(bundles)
        .enable_lints(enabled_lints)
        .disable_lints(disabled_lints)
    }

    def apply(lints: Set[String]): Configuration = new Configuration(lints)

    def empty: Configuration = new Configuration(Set.empty)
  }

  class Configuration(private val lints: Set[String])
  {
    def enable_lint(lint_name: String): Configuration =
      Configuration(lints + lint_name)

    def enable_lints(lint_names: List[String]): Configuration =
      lint_names.foldLeft(this)((config, lint) => config.enable_lint(lint))

    def disable_lint(lint_name: String): Configuration =
      Configuration(lints - lint_name)

    def disable_lints(lint_names: List[String]): Configuration =
      lint_names.foldLeft(this)((config, lint) => config.disable_lint(lint))

    def add_bundle(bundle_name: String): Configuration =
      (for {bundle <- Lint_Store.get_bundle(bundle_name)} yield {
        Configuration(lints ++ bundle.lint_names)
      }).getOrElse(this)

    def add_bundles(bundle_names: List[String]): Configuration =
      bundle_names.foldLeft(this)((config, bundle) => config.add_bundle(bundle))

    def get_lints: List[Linter.Lint] =
      lints.toList.flatMap(Lint_Store.get_lint).sortBy(_.severity.id)
  }
}

class Lint_Bundles extends Isabelle_Scala_Tools(Lint_Store.Bundle.isabelle_tool)

class Lint_Descriptions extends Isabelle_Scala_Tools(Lint_Store.isabelle_tool)
