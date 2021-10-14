package isabelle.jedit_linter


import isabelle._
import isabelle.linter._


object Overlay_Reporter extends Reporter[Linter_Overlay.State]
{
  override def report_for_command(lint_report: Linter.Lint_Report,
    id: Document_ID.Command): Linter_Overlay.State = {
    report_for_snapshot(lint_report).find(_._1.id == id).map(Map(_)).getOrElse(Map.empty)
  }

  override def report_for_snapshot(lint_report: Linter.Lint_Report): Linter_Overlay.State =
  {
    val commands = lint_report.results
      .flatMap(result => result.commands.map(_ -> result))
      .groupBy(_._1)

    val overlays = for {
      (command, results) <- commands.toSeq
    } yield {
      val res = results.map(_._2).flatMap { result =>
        val level = result.severity.toString
        val edit = result.edit match {
          case Some(edit) =>
            XML_Reporter.text("\n  Consider: ") ::: XML_Reporter.edit_markup(edit)
          case None =>
            Nil
        }
        val text = XML_Reporter.text(s"${result.message} (${result.lint_name})")
        val body = XML_Reporter.add_meta(text ::: edit, result)
        level :: body.map(XML.string_of_tree)
      }

      val args = "info" ::
        XML.string_of_tree(XML.elem(Markup.KEYWORD1, XML_Reporter.text("lints:"))) ::
        res

      command.command -> args
    }
    overlays.toMap
  }
}
