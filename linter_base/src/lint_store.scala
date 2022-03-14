/* Author: Yecine Megdiche, TU Muenchen

Registration and configuration for lints and lint bundles.
 */

package isabelle.linter


import Linter._
import isabelle._


object Lint_Store
{
  private var store: Map[String, Lint] = Map.empty

  def register_lint(lint: Lint): Unit = { store += ((lint.name, lint)) }

  for (lint <- List(
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
    SMT_Oracle,
    Tactic_Proofs,
    Unfinished_Proof,
    Unrestricted_Auto,
    Use_Apply,
    Use_By,
    Use_Isar))
  { register_lint(lint) }

  def get_lint(lint_name: String): Option[Lint] = store.get(lint_name)

  def lints: List[Lint] = store.values.toList

  def print_lints(progress: Progress = new Progress): Unit =
  {
    val header = Utils.HTML.table_header(
      List("Name", "Severity", "Short description", "Description", "Bundles").map(HTML.text))
    val rows = Lint_Store.lints.map { lint =>
      val row = List(HTML.text(lint.name), HTML.text(lint.severity.toString),
        Lint_Description.Markdown_Presentation.render(lint.short_description),
        Lint_Description.Markdown_Presentation.render(lint.long_description),
        HTML.text(commas(Bundle.get_bundles_for_lint(lint.name))))
      Utils.HTML.table_row(row)
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


  /* Bundles */

  case class Bundle(name: String, lint_names: Set[String])
  {
    def contains(lint_name : String): Boolean =
      lint_names.contains(lint_name)
  }

  object Bundle
  {
    val afp_mandatory = Bundle("afp_mandatory", Set(
      Unfinished_Proof.name,
      Bad_Style_Command.name,
      Counter_Example_Finder_Lint.name,
      Global_Attribute_On_Unnamed_Lemma.name,
      SMT_Oracle.name))

    val foundational = Bundle("foundational", Set(
      Apply_Isar_Switch.name,
      Auto_Structural_Composition.name,
      Bad_Style_Command.name,
      Complex_Isar_Initial_Method.name,
      Complex_Method.name,
      Global_Attribute_Changes.name,
      Global_Attribute_On_Unnamed_Lemma.name,
      Implicit_Rule.name,
      Lemma_Transforming_Attribute.name,
      Low_Level_Apply_Chain.name,
      Implicit_Rule.name,
      Tactic_Proofs.name,
      Unrestricted_Auto.name))

    val default = Bundle("default", foundational.lint_names ++ Set(
      Axiomatization_With_Where.name,
      Short_Name.name,
      SMT_Oracle.name))


    /* Add-on bundles */

    val non_interactive_addon = Bundle("non_interactive_addon", Set(
      Counter_Example_Finder_Lint.name,
      Diagnostic_Command.name,
      Proof_Finder.name,
      Unfinished_Proof.name))

    val pedantic_addon = Bundle("pedantic_addon", Set(
      Force_Failure.name,
      Use_Isar.name))

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

    def get_bundles_for_lint(lint_name: String): List[String] =
    {
      for {
        bundle <- bundles
        if bundle.contains(lint_name)
      } yield bundle.name
    }

    def print_bundles(progress: Progress = new Progress): Unit =
    {
      val header = Utils.HTML.table_header(
        List(HTML.text("Bundle Name"), HTML.text("Lints")))
      val rows = bundles.toList.map { bundle =>
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


  /* Configurations */

  object Selection
  {
    def apply(options: Options): Selection =
    {
      val bundles = space_explode(',', options.string("lint_bundles"))
      val enabled_lints = space_explode(',', options.string("lints_enabled"))
      val disabled_lints = space_explode(',', options.string("lints_disabled"))

      Selection.empty
        .add_bundles(bundles)
        .enable_lints(enabled_lints)
        .disable_lints(disabled_lints)
    }

    def apply(lints: Set[String]): Selection = new Selection(lints)

    def empty: Selection = new Selection(Set.empty)
  }

  class Selection(private val lints: Set[String])
  {
    def enable_lint(lint_name: String): Selection = Selection(lints + lint_name)

    def enable_lints(lint_names: List[String]): Selection =
      lint_names.foldLeft(this)((config, lint) => config.enable_lint(lint))

    def disable_lint(lint_name: String): Selection = Selection(lints - lint_name)

    def disable_lints(lint_names: List[String]): Selection =
      lint_names.foldLeft(this)((config, lint) => config.disable_lint(lint))

    def add_bundle(bundle_name: String): Selection =
      (for {bundle <- Bundle.get_bundle(bundle_name)} yield {
        Selection(lints ++ bundle.lint_names)
      }).getOrElse(this)

    def add_bundles(bundle_names: List[String]): Selection =
      bundle_names.foldLeft(this)((config, bundle) => config.add_bundle(bundle))

    def get_lints: List[Linter.Lint] =
      lints.toList.flatMap(get_lint).sortBy(_.severity.id)
  }
}

class Lint_Bundles extends Isabelle_Scala_Tools(Lint_Store.Bundle.isabelle_tool)

class Lint_Descriptions extends Isabelle_Scala_Tools(Lint_Store.isabelle_tool)
