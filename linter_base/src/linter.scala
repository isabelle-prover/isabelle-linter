/* Author: Yecine Megdiche, TU Muenchen

Isabelle linter.
 */

package isabelle.linter


import isabelle.*
import isabelle.Browser_Info.Config


object Linter {
  def lint(snapshot: Document.Snapshot, lint_selection: Lint_Store.Selection): Report = {
    val parsed_commands = snapshot.node
      .command_iterator()
      .map { case (command, offset) => Parsed_Command(command, offset, snapshot) }
      .toList
    lint_selection.get_lints.foldLeft(Report.init(snapshot.node_name)) {
      case (report, lint) => lint.lint(parsed_commands, report)
    }
  }

  object RToken {
    def unapply(r: Text.Info[Token]): Option[(Token.Kind.Value, String, Text.Range)] =
      Some(r.info.kind, r.info.source, r.range)

    def apply(token: Token, offset: Text.Offset): Text.Info[Token] =
      Text.Info(Text.Range(0, token.source.length()) + offset, token)
  }

  def list_range(ranges: List[Text.Range]): Text.Range = ranges match {
    case _ :: _ => Text.Range(ranges.head.start, ranges.last.stop)
    case Nil => Text.Range.offside
  }

  object Parsed_Command {
    def unapply(command: Parsed_Command): Option[String] = Some(command.kind)
  }

  case class Parsed_Command(
    command: Command,
    offset: Text.Offset,
    snapshot: Document.Snapshot
  ) {
    val node_name: Document.Node.Name = snapshot.node_name

    val kind: String = command.span.kind.toString()

    val range: Text.Range = command.range + offset

    val source: String = command.source

    def source(range: Text.Range): String = command.source(range - this.range.start)

    def generate_positions(
      tokens: List[Token],
      start_offset: Text.Offset
    ): List[Text.Info[Token]] = {
      Utils.map_accum_l[Token, Text.Offset, Text.Info[Token]](tokens, start_offset, {
        case (token, offset) =>
          val rtoken = RToken(token, offset)
          (rtoken, rtoken.range.stop)
      })
    }

    val tokens: List[Text.Info[Token]] = generate_positions(command.span.content, offset)

    lazy val ast_node: Text.Info[AST_Node] =
      Token_Parsers.parse(Token_Parsers.token_parser, tokens) match {
        case Token_Parsers.Success(result, Token_Parsers.Token_Reader(Nil)) => result
        case Token_Parsers.Success(_, next) =>
          Text.Info(range, Failed(s"Failed parsing. $next left"))
        case failure: Token_Parsers.NoSuccess => Text.Info(range, Failed(failure.msg))
      }

  }

  case class Result(
    lint_name: String,
    message: String,
    range: Text.Range,
    edit: Option[Edit],
    severity: Severity.Value,
    commands: List[Parsed_Command],
    short_description: Lint_Description
  ) {
    if (commands.isEmpty) error("Expected at least one command.")
    val node = commands.head.snapshot.node

    lazy val line_range = Line.Document(node.source).range(range)
  }

  object Result {
    def apply(
      lint_name: String,
      message: String,
      range: Text.Range,
      edit: Option[Edit],
      severity: Severity.Value,
      command: Parsed_Command,
      short_description: Lint_Description): Result =
      Result(lint_name, message, range, edit, severity, command :: Nil, short_description)
  }

  object Report {
    def init(name: Document.Node.Name): Report = new Report(name, Nil)
  }

  class Report private(val name: Document.Node.Name, _results: List[Result]) {
    def +(result: Result): Report = new Report(name, result :: _results)

    def results: List[Result] = _results.sortBy(_.range.start)

    def command_lints(id: Document_ID.Command): Report =
      new Linter.Report(name, _results
        .filter(_.commands.exists(_.command.id == id))
        .sortBy(-_.severity.id)) // Highest severity first

    def ranges(line_range: Text.Range = Text.Range.full): List[Text.Info[Severity.Level]] =
      _results
        .filter(lint_result => !line_range.apart(lint_result.range))
        .map(lint_result => Text.Info(lint_result.range.restrict(line_range), lint_result.severity))
        .sortBy(_.info.id) // Lowest severity first
  }

  case class Edit(range: Text.Range, replacement: String, msg: Option[String] = None) {
    val message: String = msg.getOrElse(replacement)
  }

  case class Reporter(
    command: Parsed_Command,
    name: String,
    severity: Severity.Level,
    short_description: Lint_Description
  ) {
    def apply(message: String, range: Text.Range, edit: Option[Edit]): Some[Result] =
      Some(Result(name, message, range, edit, severity, command, short_description))

    def source(range: Text.Range): String = command.source(range)
  }

  case class Lint_Wrapper(name: String, apply: (String, Lint_Wrapper.Config) => Lint) {
    def get(config: Lint_Wrapper.Config): Lint = apply(name, config)
  }

  object Lint_Wrapper {
    type Config = Severity.Level
    val default_config: Config = Severity.Warn
  }
}
