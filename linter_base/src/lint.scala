package isabelle.linter


import isabelle.*
import Linter.*


sealed trait Lint {
  val name: String
  val severity: Severity.Level
  val short_description: Lint_Description
  val long_description: Lint_Description

  def lint(commands: List[Parsed_Command], report: Report): Report
}

object Severity extends Enumeration {
  type Level = Value
  val Warn = Value("warn")
  val Error = Value("error")

  def the_level(s: String): Level = unapply(s) getOrElse
    error("No such severity level: " + quote(s))

  def unapply(s: String): Option[Level] =
    values.find(_.toString == s)
}

abstract class Proper_Commands_Lint(val name: String, val severity: Severity.Level) extends Lint {
  def lint(commands: List[Parsed_Command], report: Report): Report =
    lint_proper(commands.filter(_.command.is_proper), report)

  def lint_proper(commands: List[Parsed_Command], report: Report): Report

  def add_result(
    message: String,
    range: Text.Range,
    command: Parsed_Command,
    report: Report
  ): Report =
    report + Result(name, message, range, severity, command, short_description)

  def add_result(
    message: String,
    range: Text.Range,
    commands: List[Parsed_Command],
    report: Report
  ): Report =
    report + Result(name, message, range, severity, commands, short_description)
}

abstract class Single_Command_Lint(val name: String, val severity: Severity.Level) extends Lint {
  def lint(commands: List[Parsed_Command], report: Report): Report = commands
    .flatMap(command => lint(command, Reporter(command, name, severity, short_description)))
    .foldLeft(report)(_ + _)

  def lint(command: Parsed_Command, report: Reporter): Option[Result]
}

abstract class Parser_Lint(override val name: String, override val severity: Severity.Level)
  extends Single_Command_Lint(name, severity) with Token_Parsers {

  def parser(report: Reporter): Parser[Some[Result]]

  def lint(command: Parsed_Command, report: Reporter): Option[Result] =
    parse(parser(report), command.tokens) match {
      case Success(result, _) => result
      case _ => None
    }
}

abstract class AST_Lint(override val name: String, override val severity: Severity.Level)
  extends Single_Command_Lint(name, severity) {

  def lint_method(method: Text.Info[Method], report: Reporter): Option[Result] = None

  def lint_by(
    method1: Text.Info[Method],
    method2: Option[Text.Info[Method]],
    report: Reporter
  ): Option[Result] =
    lint_method(method1, report) orElse method2.flatMap(lint_method(_, report))

  def lint_apply(method: Text.Info[Method], report: Reporter): Option[Result] =
    lint_method(method, report)

  def lint_qed(method: Option[Text.Info[Method]], report: Reporter): Option[Result] =
    method.flatMap(lint_method(_, report))

  def lint_isar_proof(method: Option[Text.Info[Method]], report: Reporter): Option[Result] =
    method.flatMap(lint_method(_, report))

  def lint_proof(proof: Text.Info[Proof], report: Reporter): Option[Result] =
    proof.info match {
      case Apply(method) => lint_apply(method, report)
      case Isar_Proof(method) => lint_isar_proof(method, report)
      case By(method1, method2) => lint_by(method1, method2, report)
      case Qed(method) => lint_qed(method, report)
    }

  def lint_ast_node(
    elem: Text.Info[AST_Node],
    report: Reporter
  ): Option[Result] = elem.info match {
    case p: Proof => lint_proof(Text.Info(elem.range, p), report)
    case _ => None
  }

  def lint(command: Parsed_Command, report: Reporter): Option[Result] =
    lint_ast_node(command.ast_node, report)
}