package isabelle.jedit_linter


import isabelle._
import isabelle.jedit.PIDE
import isabelle.linter._


class Linter_Variable
{
  val LINTER_ENABLED_OPTION = "linter"

  private var lint_cache: Map[Document.Node.Name, (Document.Version, Linter.Lint_Report)] = Map.empty
  private var configuration: Linter_Configuration = Linter_Configuration.empty
  private var _enabled: Boolean = false

  private def update_cache(snapshot: Document.Snapshot): Unit =
  {
    lazy val new_cache =
      lint_cache + (snapshot.node_name -> (snapshot.version, Linter.lint(snapshot, configuration)))
    lint_cache get snapshot.node_name match {
      case None => lint_cache = new_cache
      case Some((version, _)) => if (snapshot.version.id < version.id) lint_cache = new_cache
    }
  }

  def enabled: Boolean = _enabled

  def update(options: Options): Unit = synchronized {
    this._enabled = options.bool(LINTER_ENABLED_OPTION)
    if (_enabled) this.configuration = Linter_Configuration(options)
  }

  def do_lint(snapshot: Document.Snapshot): Unit =
    if (_enabled) update_cache(snapshot)

  def lint_report(snapshot: Document.Snapshot): Linter.Lint_Report =
  {
    if (_enabled) {
      lint_cache.get(snapshot.node_name) match {
        case Some((_, report)) => report
        case None => Linter.Lint_Report.empty
      }
    }
    else Linter.Lint_Report.empty
  }

  private def refresh_lint(): Unit = synchronized {
    for {
      snapshot <- PIDE.maybe_snapshot()
      if enabled && !snapshot.is_outdated
    } {
      do_lint(snapshot)
      val report = lint_report(snapshot)
      val overlays = Overlay_Presenter.present_for_snapshot(report)
      Linter_Plugin.instance.foreach(_.overlays.update(overlays))
    }
  }

  private val main =
    Session.Consumer[Any](getClass.getName) { _ =>
      GUI_Thread.later {
        refresh_lint()
        // FIXME maybe a separate event for the linter?
        PIDE.session.caret_focus.post(Session.Caret_Focus)
      }
    }

  def install_handlers(): Unit =
  {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
  }

  def uninstall_handlers(): Unit =
  {
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
  }
}
