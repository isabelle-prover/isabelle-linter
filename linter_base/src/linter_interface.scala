package isabelle.linter


import isabelle._


class Linter_Interface(cache: Boolean) {
  var lint_cache: Map[Document.Node.Name, (Document.Version, Linter.Lint_Report)] = Map.empty
  var configuration: Linter_Configuration = Linter_Configuration.empty

  private def update_cache(snapshot: Document.Snapshot): Unit = {
    lazy val new_cache =
      lint_cache + (snapshot.node_name -> (snapshot.version, Linter.lint(
        snapshot,
        configuration
      )))
    lint_cache get snapshot.node_name match {
      case None               => lint_cache = new_cache
      case Some((version, _)) => if (snapshot.version.id < version.id) lint_cache = new_cache
    }
  }

  def do_lint(snapshot: Document.Snapshot): Unit =
    update_cache(snapshot)

  def lint_report(snapshot: Document.Snapshot): Linter.Lint_Report = {
    if (cache) {
      (for { (_, report) <- lint_cache.get(snapshot.node_name) } yield report)
        .getOrElse(Linter.Lint_Report.empty)
    } else Linter.lint(snapshot, configuration)
  }
}

class Linter_Variable(cache: Boolean = true) {
  private val no_linter: Option[Linter_Interface] = None
  private var current_linter: Option[Linter_Interface] = no_linter

  def get: Option[Linter_Interface] = synchronized { current_linter }

  def update(options: Options): Unit = synchronized {
    if (options.bool("linter")) {
      if (current_linter.isEmpty) {
        current_linter = Some(new Linter_Interface(cache))
      }
    } else current_linter = no_linter
    for (linter <- current_linter) {
      val bundles = space_explode(',', options.string("enabled_bundles"))
      val enabled_lints = space_explode(',', options.string("enabled_lints"))
      val disabled_lints = space_explode(',', options.string("disabled_lints"))
      linter.configuration = Linter_Configuration.empty
        .add_bundles(bundles)
        .enable_lints(enabled_lints)
        .disable_lints(disabled_lints)
    }
  }
}
