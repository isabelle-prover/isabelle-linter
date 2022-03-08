/* Author: Yecine Megdiche, TU Munich

Configuration of enabled lints.
 */

package isabelle.linter


import isabelle._


object Linter_Configuration
{
  val ENABLED_BUNDLES_OPTION = "enabled_bundles"
  val ENABLED_LINTS_OPTION = "enabled_lints"
  val DISABLED_LINTS_OPTION = "disabled_lints"

  def apply(options: Options): Linter_Configuration =
  {
    val bundles = space_explode(',', options.string(ENABLED_BUNDLES_OPTION))
    val enabled_lints = space_explode(',', options.string(ENABLED_LINTS_OPTION))
    val disabled_lints = space_explode(',', options.string(DISABLED_LINTS_OPTION))

    Linter_Configuration.empty
      .add_bundles(bundles)
      .enable_lints(enabled_lints)
      .disable_lints(disabled_lints)
  }

  def apply(lints: Set[String]): Linter_Configuration = new Linter_Configuration(lints)

  def empty: Linter_Configuration = new Linter_Configuration(Set.empty)
}

class Linter_Configuration(private val lints: Set[String])
{
  def enable_lint(lint_name: String): Linter_Configuration =
    Linter_Configuration(lints + lint_name)

  def enable_lints(lint_names: List[String]): Linter_Configuration =
    lint_names.foldLeft(this)((config, lint) => config.enable_lint(lint))

  def disable_lint(lint_name: String): Linter_Configuration =
    Linter_Configuration(lints - lint_name)

  def disable_lints(lint_names: List[String]): Linter_Configuration =
    lint_names.foldLeft(this)((config, lint) => config.disable_lint(lint))

  def add_bundle(bundle_name: String): Linter_Configuration =
    (for {bundle <- Lint_Store.get_bundle(bundle_name)} yield {
      Linter_Configuration(lints ++ bundle.lint_names)
    }).getOrElse(this)

  def add_bundles(bundle_names: List[String]): Linter_Configuration =
    bundle_names.foldLeft(this)((config, bundle) => config.add_bundle(bundle))

  def get_lints: List[Linter.Lint] =
    lints.toList.flatMap(Lint_Store.get_lint).sortBy(_.severity.id)
}
