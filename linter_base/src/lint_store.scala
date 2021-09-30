package isabelle.linter

import Linter._
import scala.collection.mutable.Map

object Lint_Store {

  private val store: Map[String, Lint] = Map.empty

  def register_lint(lint: Lint): Unit =
    store += ((lint.name, lint))

  def get_lint(lint_name: String): Option[Lint] = store.get(lint_name)

  private val all_lints: List[Lint] = List(
    Apply_Isar_Switch,
    Auto_Structural_Composition,
    Axiomatization_With_Where,
    Bad_Style_Command,
    Complex_Isar_Initial_Method,
    Complex_Method,
    Counter_Example_Finder,
    Diagnostic_Command,
    Force_Failure,
    Global_Attribute_Changes,
    Global_Attribute_On_Unnamed_Lemma,
    Implicit_Rule,
    Lemma_Transforming_Attribute,
    Low_Level_Apply_Chain,
    Proof_Finder,
    Short_Name,
    Unfinished_Proof,
    Unrestricted_Auto,
    Use_By,
    Use_Isar,
    // Debugging lints
    Print_AST,
  )

  for (lint <- all_lints) register_lint(lint)

  // bundles
  case class Bundle(val name: String, val lint_names: Set[String])

  object Bundle {

    val pedantic = Bundle(
      "pedantic",
      Set(
        Low_Level_Apply_Chain.name,
        Use_Isar.name,
        Short_Name.name,
        Force_Failure.name,
        Auto_Structural_Composition.name
      )
    )

    val non_interactive = Bundle(
      "non_interactive",
      Set(
        Unfinished_Proof.name,
        Proof_Finder.name,
        Counter_Example_Finder.name,
        Diagnostic_Command.name
      )
    )

    val foundational = Bundle(
      "foundational",
      Set(
        Apply_Isar_Switch.name,
        Bad_Style_Command.name,
        Complex_Isar_Initial_Method.name,
        Complex_Method.name,
        Global_Attribute_Changes.name,
        Global_Attribute_On_Unnamed_Lemma.name,
        Lemma_Transforming_Attribute.name,
        Implicit_Rule.name,
        Unrestricted_Auto.name,
        Use_By.name
      )
    )

    val afp = Bundle(
      "afp",
      Set(
        Apply_Isar_Switch.name,
        Bad_Style_Command.name,
        Complex_Isar_Initial_Method.name,
        Counter_Example_Finder.name,
        Complex_Method.name,
        Global_Attribute_Changes.name,
        Global_Attribute_On_Unnamed_Lemma.name,
        Lemma_Transforming_Attribute.name,
        Implicit_Rule.name,
        Unrestricted_Auto.name,
        Use_By.name
      )
    )

    val default = Bundle(
      "default",
      Set(
        Apply_Isar_Switch.name,
        Use_By.name,
        Unrestricted_Auto.name,
        Low_Level_Apply_Chain.name,
        Axiomatization_With_Where.name,
        Bad_Style_Command.name,
        Global_Attribute_On_Unnamed_Lemma.name,
        Lemma_Transforming_Attribute.name,
        Implicit_Rule.name,
        Complex_Isar_Initial_Method.name,
        Global_Attribute_Changes.name,
        Complex_Method.name
      )
    )

    val all = Bundle(
      "all",
      all_lints.map(_.name).toSet
    )
  }

  val bundle_store: Map[String, Bundle] = Map.empty

  def register_bundle(bundle: Bundle): Unit =
    bundle_store += ((bundle.name, bundle))

  def get_bundle(name: String): Option[Bundle] =
    bundle_store.get(name)

  private val all_bundles =
    List(
      Bundle.non_interactive,
      Bundle.foundational,
      Bundle.pedantic,
      Bundle.default,
      Bundle.all,
      Bundle.afp
    )

  for (bundle <- all_bundles) register_bundle(bundle)
}
