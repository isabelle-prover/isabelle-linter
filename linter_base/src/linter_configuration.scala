package isabelle.linter

object Linter_Configuration {
  def apply(lints: Set[String]): Linter_Configuration = new Linter_Configuration(lints)

  def empty: Linter_Configuration = Linter_Configuration(Set.empty)
}

class Linter_Configuration(private val lints: Set[String]) {

  def enable_lint(lint_name: String): Linter_Configuration =
    Linter_Configuration(lints + lint_name)

  def enable_lints(lint_names: List[String]): Linter_Configuration =
    lint_names.foldLeft(this)((config, lint) => config.enable_lint(lint))

  def disable_lint(lint_name: String): Linter_Configuration =
    Linter_Configuration(lints - lint_name)

  def disable_lints(lint_names: List[String]): Linter_Configuration =
    lint_names.foldLeft(this)((config, lint) => config.disable_lint(lint))

  def add_bundle(bundle_name: String): Linter_Configuration =
    (for { bundle <- Lint_Store.get_bundle(bundle_name) } yield {
      Linter_Configuration(lints ++ bundle.lint_names)
    }).getOrElse(this)

  def add_bundles(bundle_names: List[String]): Linter_Configuration =
    bundle_names.foldLeft(this)((config, bundle) => config.add_bundle(bundle))

  def get_lints: List[Linter.Lint] =
    lints.toList.map(Lint_Store.get_lint _).flatten.sortBy(_.severity.id)
}
