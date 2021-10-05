package isabelle.linter

import isabelle._

abstract class Reporter[A] {
  def report_for_command(lint_report: Linter.Lint_Report, id: Document_ID.Command): A

  def report_for_snapshot(lint_report: Linter.Lint_Report): A
}

object JSON_Reporter extends Reporter[JSON.T] {
  def report_for_command(lint_report: Linter.Lint_Report, id: Document_ID.Command): JSON.T =
    JSON.Object("results" -> lint_report.command_lints(id))

  def report_for_snapshot(lint_report: Linter.Lint_Report): JSON.T =
    JSON.Object("results" -> lint_report.results.map(report_result))

  private def report_result(lint_result: Linter.Lint_Result): JSON.T = JSON.Object(
    "name" -> lint_result.lint_name,
    "severity" -> lint_result.severity.toString,
    "startOffset" -> lint_result.range.start,
    "stopOffset" -> lint_result.range.stop,
    "startPosition" -> lint_result.line_range.start.print,
    "stopPosition" -> lint_result.line_range.stop.print,
    "commands" -> lint_result.commands.map(_.command.id),
    "edit" -> lint_result.edit
      .map({ edit =>
        JSON.Object(
          "startOffset" -> edit.range.start,
          "stopOffset" -> edit.range.stop,
          "replacement" -> edit.replacement,
          "msg" -> edit.msg.getOrElse(null)
        )
      })
      .getOrElse(null)
  )

}

object Text_Reporter extends Reporter[String] {
  def report_for_command(lint_report: Linter.Lint_Report, id: Document_ID.Command): String =
    report_results(lint_report.command_lints(id))

  def report_for_snapshot(lint_report: Linter.Lint_Report): String =
    report_results(lint_report.results)

  private def report_results(lint_results: List[Linter.Lint_Result]): String =
    lint_results.map(report_result _).mkString("\n" + "=" * 30 + "\n")

  private def report_result(lint_result: Linter.Lint_Result): String = {
    val commands_range = Linter.list_range(lint_result.commands.map(_.range))
    val position = lint_result.line_range.start
    val commands_source =
      lint_result.node
        .command_iterator(commands_range)
        .map({ case (cmd, _) => cmd.source })
        .mkString

    val snippet =
      if (lint_result.range contains commands_range)
        commands_source
      else underline(commands_source, lint_result.range - commands_range.start)

    val edit = lint_result.edit match {
      case None       => ""
      case Some(edit) => s"Suggestion: ${edit.message}"
    }

    s"""|At ${position.print}:                [${lint_result.lint_name}]
        |  ${lint_result.message}
        |  Severity: ${lint_result.severity}
        |
        |$snippet
        |
        |  $edit
        |""".stripMargin
  }

  private def underline(source: String, range: Text.Range): String = {

    def underline(line: String, range: Text.Range): (String, Text.Range) = {
      val line_range = Text.Range(0, line.length())
      if (range.is_singularity)
        (line, range)
      else if (line_range.apart(range)) {
        val new_range = range - line_range.stop - 1
        (line, new_range)
      } else {
        val underlined_range = range.restrict(line_range)
        val new_range = Text.Range(0, 0 max (range.stop - line_range.length - 1))
        val underlined =
          line + "\n" + (" " * underlined_range.start) + ("^" * underlined_range.length)
        (underlined, new_range)
      }

    }

    Utils.mapAccumL(source.split("\n").toList, range, underline).mkString("\n")
  }
}

object XML_Lint_Reporter extends Reporter[XML.Body] {

  def report_for_command(lint_report: Linter.Lint_Report, id: Document_ID.Command): XML.Body = {
    val xml = report_lints(lint_report.command_lints(id))
    if (xml.isEmpty) Nil
    else XML.elem(Markup.KEYWORD1, text("lints:")) :: xml
  }

  def report_for_snapshot(lint_report: Linter.Lint_Report): XML.Body =
    report_lints(
      lint_report.results,
      compact = false
    )

  private def report_lints(
      lint_results: List[Linter.Lint_Result],
      compact: Boolean = true
  ): XML.Body =
    lint_results.zipWithIndex
      .map(ri =>
        report_lint(
          ri._1,
          ri._2,
          compact = compact
        )
      )
      .flatten

  private def report_lint(
      lint_result: Linter.Lint_Result,
      lint_number: Int = 0,
      compact: Boolean = true
  ): XML.Body = {

    val edit = lint_result.edit match {
      case Some(edit) => text("\n    Consider: ") ::: edit_markup(edit)
      case None       => Nil
    }

    val inner =
      if (compact)
        text(s" ${lint_number + 1}. ${lint_result.message}") ::: edit
      else
        (
          position_markup(lint_result)
            ::: text(s" ${lint_result.message}")
            ::: edit
            ::: text(s"\n    Name: ${lint_result.lint_name}")
            ::: text(s"\n    Severity: ${lint_result.severity}")
        )
    add_meta(inner, lint_result)
  }

  /* xml helpers */

  def edit_markup(edit: Linter.Edit): XML.Body = XML.Elem(
    Markup(
      Markup.SENDBACK,
      Position.Range(edit.range) ::: Markup.Content(edit.replacement)
    ),
    text(edit.message)
  ) :: Nil

  def position_markup(lint_result: Linter.Lint_Result): XML.Body =
    text("At ") ::: XML.Elem(
      Markup(Markup.GOTO_OFFSET, Position.Range(lint_result.range)),
      text(lint_result.line_range.start.print)
    ) :: text(":\n")

  def add_meta(body: XML.Body, lint_result: Linter.Lint_Result): XML.Body = {
    XML.Elem(
      Markup(
        Linter_Markup.LINT_RESULT,
        Linter_Markup.Lint_Name(lint_result.lint_name)
          ::: Linter_Markup.Lint_Message(lint_result.message)
          ::: Linter_Markup.Lint_Severity(lint_result.severity)
          ::: Linter_Markup.Lint_Commands(lint_result.commands.map(_.command.id))
      ),
      body
    ) :: Nil

  }

  def text(content: String): XML.Body = XML.Text(content) :: Nil

  def block(inner: XML.Body): XML.Body =
    XML.elem(Markup.Block.name, inner) :: Nil

}
