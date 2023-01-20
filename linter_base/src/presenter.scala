/* Author: Yecine Megdiche, TU Muenchen

Lint report presentation in different formats.
 */

package isabelle.linter


import isabelle._


trait Presenter[A] {
  def to_string(report: A): String
  def mk_string(reports: List[A]): String

  def present_for_command(lint_report: Linter.Lint_Report, id: Document_ID.Command): A
  def present_for_snapshot(lint_report: Linter.Lint_Report, show_descriptions: Boolean = false): A

  def with_info(report: A, node: Document.Node.Name, elapsed: Time): A
}

object JSON_Presenter extends Presenter[JSON.T] {
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
          "msg" -> edit.msg.orNull)
      }).orNull)

  override def to_string(report: JSON.T): String =
    JSON.Format.apply(report)

  override def mk_string(reports: List[JSON.T]): String =
    JSON.Format.apply(JSON.Object("reports" -> reports))

  override def present_for_command(lint_report: Linter.Lint_Report, id: Document_ID.Command): JSON.T =
    JSON.Object("results" -> lint_report.command_lints(id))

  override def present_for_snapshot(lint_report: Linter.Lint_Report,
    show_descriptions: Boolean = false): JSON.T =
    JSON.Object("results" -> lint_report.results.map(report_result))

  override def with_info(report: JSON.T, node: Document.Node.Name, elapsed: Time): JSON.T =
    JSON.Object(
      "theory" -> node.toString,
      "report" -> report,
      "timing" -> elapsed.ms)
}

object Text_Presenter extends Presenter[String] {
  private def report_results(lint_results: List[Linter.Lint_Result]): String =
    lint_results.map(report_result).mkString("\n" + "=" * 30 + "\n")

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
      case None => ""
      case Some(edit) => s"Suggestion: ${edit.message}"
    }

    "At " + position.print + ":                [" + lint_result.lint_name + "}]\n  " +
      lint_result.message + "\n" +
      "  Severity: " + lint_result.severity + "\n\n" +
      snippet + "\n\n" +
      "  " + edit
  }

  private def underline(source: String, range: Text.Range): String = {
    def underline(line: String, range: Text.Range): (String, Text.Range) = {
      val line_range = Text.Range(0, line.length())
      if (range.is_singularity) (line, range)
      else if (line_range.apart(range)) {
        val new_range = range - line_range.stop - 1
        (line, new_range)
      }
      else {
        val underlined_range = range.restrict(line_range)
        val new_range = Text.Range(0, 0 max (range.stop - line_range.length - 1))
        val underlined =
          line + "\n" + (" " * underlined_range.start) + ("^" * underlined_range.length)
        (underlined, new_range)
      }
    }

    Utils.map_accum_l(source.split("\n").toList, range, underline).mkString("\n")
  }

  override def to_string(report: String): String =
    if (report.isEmpty) "No lints found." else report

  override def mk_string(reports: List[String]): String = reports.mkString("\n")

  override def present_for_command(lint_report: Linter.Lint_Report,
    id: Document_ID.Command): String =
    report_results(lint_report.command_lints(id))

  override def present_for_snapshot(lint_report: Linter.Lint_Report,
    show_descriptions: Boolean = false): String =
    report_results(lint_report.results)

  override def with_info(report: String, node: Document.Node.Name, elapsed: Time): String =
    "Linted " + node + " in " + elapsed + ":\n" + report
}

object XML_Presenter extends Presenter[XML.Body] {
  private def report_lints(
    lint_results: List[Linter.Lint_Result],
    compact: Boolean = true,
    show_descriptions: Boolean = false
  ): XML.Body = {
    lint_results.zipWithIndex
      .flatMap {
        ri =>
          report_lint(
            ri._1,
            ri._2,
            compact = compact,
            show_descriptions = show_descriptions)
      }
  }

  private def report_lint(
    lint_result: Linter.Lint_Result,
    lint_number: Int = 0,
    compact: Boolean = true,
    show_descriptions: Boolean = false
  ): XML.Body = {
    val edit = lint_result.edit match {
      case Some(edit) => text("\n    Consider: ") ::: edit_markup(edit)
      case None => Nil
    }

    val inner =
      if (compact) text(s" ${lint_number + 1}. ${lint_result.message}") ::: edit
      else {
        (position_markup(lint_result)
          ::: text(s" ${lint_result.message}")
          ::: edit
          ::: text(s"\n    Name: ${lint_result.lint_name}")
          ::: text(s"\n    Severity: ${lint_result.severity}")
          :::
          (if (show_descriptions)
            text(s"\n    Description: ") :::
              Lint_Description.XML_Presentation.render(lint_result.short_description)
           else Nil))
      }
    add_meta(inner, lint_result)
  }

  /* xml helpers */

  def edit_markup(edit: Linter.Edit): XML.Body =
    XML.Elem(
      Markup(Linter_Markup.LINTER_SENDBACK,
        Position.Range(edit.range) ::: Markup.Content(edit.replacement)),
      text(edit.message)) :: Nil

  def position_markup(lint_result: Linter.Lint_Result): XML.Body = {
    val pos = Position.Offset(lint_result.range.start + 1) :::
      Position.End_Offset(lint_result.range.stop) :::
      Position.File(lint_result.commands.head.node_name.node)
    text("At ") ::: XML.Elem(
      Markup(Linter_Markup.GOTO_POSITION, pos),
      text(lint_result.line_range.start.print)) :: text(":\n")
  }

  def add_meta(body: XML.Body, lint_result: Linter.Lint_Result): XML.Body = {
    XML.Elem(Markup(Linter_Markup.LINT_RESULT,
      Linter_Markup.Lint_Name(lint_result.lint_name)
        ::: Linter_Markup.Lint_Message(lint_result.message)
        ::: Linter_Markup.Lint_Severity(lint_result.severity)
        ::: Linter_Markup.Lint_Commands(lint_result.commands.map(_.command.id))),
      body) :: Nil
  }

  def text(content: String): XML.Body =
    XML.Text(content) :: Nil

  def block(inner: XML.Body): XML.Body =
    XML.elem(Markup.Block.name, inner) :: Nil

  override def to_string(report: XML.Body): String = XML.string_of_body(report)

  override def mk_string(reports: List[XML.Body]): String =
    XML.string_of_tree(XML.Elem(Markup("reports", Nil), reports.flatten))

  override def present_for_command(lint_report: Linter.Lint_Report,
    id: Document_ID.Command): XML.Body = {
    val xml = report_lints(lint_report.command_lints(id))
    if (xml.isEmpty) Nil
    else XML.elem(Markup.KEYWORD1, text("lints:")) :: xml
  }

  override def present_for_snapshot(lint_report: Linter.Lint_Report,
    show_descriptions: Boolean = false): XML.Body =
    report_lints(
      lint_report.results,
      compact = false,
      show_descriptions = show_descriptions)

  override def with_info(report: XML.Body, node: Document.Node.Name, elapsed: Time): XML.Body =
    List(XML.Elem(Markup("report",
      Linter_Markup.Theory(node.toString) ::: Linter_Markup.Timing(elapsed.ms)), report))
}
