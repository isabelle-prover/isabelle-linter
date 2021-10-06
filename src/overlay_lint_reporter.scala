package isabelle.jedit_linter

import isabelle._
import isabelle.linter._
import isabelle.linter.XML_Lint_Reporter.{add_meta, edit_markup, text}


case class Overlay(summary: XML.Body, overlays: Linter_Overlay.State)

object Overlay_Lint_Reporter extends Reporter[Overlay]
{
  override def report_for_command(lint_report: Linter.Lint_Report,
    id: Document_ID.Command): Overlay =
    Overlay(XML_Lint_Reporter.report_for_command(lint_report, id), Map.empty)

  override def report_for_snapshot(lint_report: Linter.Lint_Report): Overlay =
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
          case Some(edit) => text("\n  Consider: ") ::: edit_markup(edit)
          case None       => Nil
        }
        val body = add_meta(text(s"${result.message} (${result.lint_name})") ::: edit, result)
        level :: body.map(XML.string_of_tree)
      }

      val args = "info" :: XML.string_of_tree(XML.elem(Markup.KEYWORD1, text("lints:"))) :: res

      command.command -> args
    }

    Overlay(XML_Lint_Reporter.report_for_snapshot(lint_report), overlays.toMap)
  }

}
