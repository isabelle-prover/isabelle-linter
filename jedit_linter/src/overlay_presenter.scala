package isabelle.jedit_linter


import isabelle.Document.Node
import isabelle._
import isabelle.jedit_linter.Linter_Overlay.State
import isabelle.linter._


object Overlay_Presenter extends Presenter[Linter_Overlay.State]
{
  override def to_string(report: State): String = report.toString()

  override def mk_string(reports: List[State]): String = reports.mkString(", ")

  override def present_for_command(lint_report: Linter.Lint_Report,
    id: Document_ID.Command): Linter_Overlay.State =
    present_for_snapshot(lint_report).find(_._1.id == id).map(Map(_)).getOrElse(Map.empty)

  override def present_for_snapshot(lint_report: Linter.Lint_Report, show_desriptions: Boolean = false): Linter_Overlay.State =
  {
    val commands = lint_report.results
      .flatMap(result => result.commands.map(_ -> result))
      .groupBy(_._1)

    val overlays = for ((command, results) <- commands.toSeq) yield
    {
      val res = results.map(_._2).flatMap { result =>
        val level = result.severity.toString
        val edit = result.edit match {
          case Some(edit) =>
            XML_Presenter.text("\n  Consider: ") ::: XML_Presenter.edit_markup(edit)
          case None =>
            Nil
        }
        val text = XML_Presenter.text(s"${result.message} (${result.lint_name})")
        val body = XML_Presenter.add_meta(text ::: edit, result)
        level :: body.map(XML.string_of_tree)
      }

      val args = "info" ::
        XML.string_of_tree(XML.elem(Markup.KEYWORD1, XML_Presenter.text("lints:"))) ::
        res

      command.command -> args
    }
    overlays.toMap
  }

  override def with_info(report: State, node: Node.Name, elapsed: Time): State =
    report.view.mapValues(args => "info" :: ("Linted " + node + " in " + elapsed) :: args).toMap
}
