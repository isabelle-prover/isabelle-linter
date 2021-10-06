package isabelle.jedit_linter

import isabelle._
import isabelle.jedit._

import isabelle.linter._
import isabelle.linter.Linter.Severity
import isabelle.linter.XML_Lint_Reporter.{add_meta, edit_markup, text}


object Overlay_Lint_Reporter extends Reporter[XML.Body] {

  override def report_for_command(lint_report: Linter.Lint_Report,
    id: Document_ID.Command): XML.Body = XML_Lint_Reporter.report_for_command(lint_report, id)

  override def report_for_snapshot(lint_report: Linter.Lint_Report): XML.Body =
  {
    val commands = lint_report.results
      .flatMap(result => result.commands.map(_ -> result))
      .groupBy(_._1)

    for {
      (command, results) <- commands.toSeq
    } {
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

      PIDE.editor.insert_overlay(command.command, "print_xml", args)
    }

    XML_Lint_Reporter.report_for_snapshot(lint_report)
  }

}
