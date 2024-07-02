/* Author: Yecine Megdiche, TU Muenchen

Isabelle linter.
 */

package isabelle.linter


import isabelle.*
import isabelle.Document.*
import isabelle.Command.Blobs_Info
import isabelle.Browser_Info.Config

import scala.collection.mutable


object Linter {
  /* result handling */

  case class Result(
    lint_name: String,
    message: String,
    range: Text.Range,
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
      severity: Severity.Value,
      command: Parsed_Command,
      short_description: Lint_Description): Result =
      Result(lint_name, message, range, severity, command :: Nil, short_description)
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

  case class Reporter(
    command: Parsed_Command,
    name: String,
    severity: Severity.Level,
    short_description: Lint_Description
  ) {
    def apply(message: String, range: Text.Range): Some[Result] =
      Some(Result(name, message, range, severity, command, short_description))

    def source(range: Text.Range): String = command.source(range)
  }

  case class Lint_Wrapper(name: String, apply: (String, Lint_Wrapper.Config) => Lint) {
    def get(config: Lint_Wrapper.Config): Lint = apply(name, config)
  }

  object Lint_Wrapper {
    type Config = Severity.Level
    val default_config: Config = Severity.Warn
  }


  /* linting */

  def read_theory(theory_context: Export.Theory_Context): Option[Snapshot] = {
    def decode_bytes(bytes: Bytes): String =
      Symbol.output(false, bytes.text)

    def read(name: String): Export.Entry =
      theory_context(name, permissive = true)

    def read_xml(name: String): XML.Body =
      YXML.parse_body(decode_bytes(read(name).bytes), cache = theory_context.cache)

    for ((thy_file, _) <- theory_context.files(permissive = true)) yield {
      val node_name =
        Document.Node.Name(thy_file, theory = theory_context.theory)

      val thy_xml = read_xml(Export.MARKUP)
      val command_spans = Token_Markup.from_xml(thy_xml)

      val counter = Counter.make()

      val (state0, node_edits, commands, _) = command_spans.foldLeft(State.init,
        List.empty[Text.Edit], List.empty[Command], 0) {
          case ((st, ed, cmd, offset), command_span) =>
            val command = Command(counter(), node_name, Blobs_Info.empty, command_span)
            (st.define_command(command), ed :+ Text.Edit.insert(offset, command.source),
              cmd :+ command, offset + command.length)
        }

      val version = Version.init
      val nodes0 = version.nodes
      val nodes1 = nodes0 + (node_name -> nodes0(node_name).update_commands(Linear_Set.from(commands)))
      val version1 = Version.make(nodes1)

      val edits: List[Edit_Text] =
        List(node_name -> Node.Edits(node_edits))

      val state1 =
        state0.continue_history(Future.value(version), edits, Future.value(version1))
          .define_version(version1, state0.the_assignment(version))
          .assign(version1.id, Nil, commands.map(c => c.id -> List(Document_ID.make())))._2

      state1.snapshot(node_name = node_name)
    }
  }

  def lint_snapshot(snapshot: Document.Snapshot, lint_selection: Lint_Store.Selection): Report = {
    val parsed_commands = snapshot.node
      .command_iterator()
      .map { case (command, offset) => Parsed_Command(command, offset, snapshot) }
      .toList
    lint_selection.get_lints.foldLeft(Report.init(snapshot.node_name)) {
      case (report, lint) => lint.lint(parsed_commands, report)
    }
  }

  def lint_session(
    session_name: String,
    selection: Lint_Store.Selection,
    presenter: Presenter[_],
    store: Store,
    deps: Sessions.Deps,
    verbose: Boolean,
    console: Boolean,
    progress: Progress
  ): List[Report] = {
    progress.echo("Linting " + session_name)

    val base = deps.get(session_name).getOrElse(error("Deps not found for " + session_name))
    val thys = base.proper_session_theories.map(_.theory)

    using(Export.open_session_context0(store, session_name)) { session_context =>
      session_context.theory_names().filter(thys.contains).flatMap { thy =>
        val thy_heading = "\nTheory " + quote(thy) + ":"
        progress.echo_if(verbose, "Processing " + thy + "...")
        read_theory(session_context.theory(thy)) match {
          case None =>
            progress.echo_warning(thy_heading + " missing")
            None
          case Some(snapshot) =>
            val report = Linter.lint_snapshot(snapshot, selection)

            if (console) {
              val msg = presenter.to_string(presenter.present(report, header = true))
              progress.echo_if(msg.nonEmpty, msg)
            }

            Some(report)
        }
      }
    }
  }

  def lint[A](
    lint_selection: Lint_Store.Selection,
    presenter: Presenter[A],
    out_file: Option[Path],
    options: Options,
    build_hosts: List[Build_Cluster.Host] = Nil,
    selection: Sessions.Selection = Sessions.Selection.empty,
    progress: Progress = new Progress,
    clean_build: Boolean = false,
    afp_root: Option[Path] = None,
    fail_on: Option[Severity.Level] = None,
    dirs: List[Path] = Nil,
    select_dirs: List[Path] = Nil,
    numa_shuffling: Boolean = false,
    max_jobs: Option[Int] = None,
    console: Boolean = true,
    verbose: Boolean = false
  ): List[A] = {
    val res =
      Build.build(options,
        build_hosts = build_hosts,
        selection = selection,
        progress = progress,
        check_unknown_files = Mercurial.is_repository(Path.ISABELLE_HOME),
        clean_build = clean_build,
        afp_root = afp_root,
        dirs = dirs,
        numa_shuffling = numa_shuffling,
        max_jobs = max_jobs,
        select_dirs = select_dirs)
    if (!res.ok) System.exit(res.rc)

    val store = Store(options)

    val full_sessions =
      Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

    val sessions_structure = full_sessions.selection(selection)
    val deps = Sessions.deps(sessions_structure)

    val lint_res: List[Report] =
      sessions_structure.build_selection(selection).map(session_name =>
        Future.fork {
          lint_session(session_name, selection = lint_selection, presenter = presenter,
            store = store, deps = deps, console = console, verbose = verbose, progress = progress)
          }).flatMap(_.join)

    val presented = lint_res.map(presenter.present(_, header = true))

    out_file.foreach(File.write(_, presenter.mk_string(presented)))

    fail_on match {
      case Some(severity) =>
        if (lint_res.flatMap(_.results).exists(_.severity >= severity))
          System.exit(1)
      case None =>
    }

    presented
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lint", "lint theory sources based on PIDE markup",
    Scala_Project.here,
  { args =>
    var afp_root: Option[Path] = None
    val base_sessions = new mutable.ListBuffer[String]
    val select_dirs = new mutable.ListBuffer[Path]
    val build_hosts = new mutable.ListBuffer[Build_Cluster.Host]
    var numa_shuffling = false
    var output_file: Option[Path] = None
    var requirements = false
    var verbose_build = false
    val exclude_session_groups = new mutable.ListBuffer[String]
    var all_sessions = false
    var clean_build = false
    val dirs = new mutable.ListBuffer[Path]
    var fail_on: Option[Severity.Level] = None
    val session_groups = new mutable.ListBuffer[String]
    var max_jobs: Option[Int] = None
    var list_lints = false
    var options = Options.init(specs = Options.Spec.ISABELLE_BUILD_OPTIONS)
    var mode = "text"
    var verbose = false
    val exclude_sessions = new mutable.ListBuffer[String]

    val getopts = Getopts("""
Usage: isabelle lint [OPTIONS] [SESSIONS ...]

  Options are:
  -A ROOT        include AFP with given root directory (":" for """ + AFP.BASE.implode + """)
  -B NAME        include session NAME and all descendants
  -D DIR         include session directory and select its sessions
  -H HOSTS       additional cluster host specifications of the form
                 NAMES:PARAMETERS (separated by commas)
  -N             cyclic shuffling of NUMA CPU nodes (performance tuning)
  -O FILE        write output to file instead of stdout
  -R             refer to requirements of selected sessions
  -V             verbose build
  -X NAME        exclude sessions from group NAME and all descendants
  -a             select all sessions
  -c             clean build
  -d DIR         include session directory
  -f SEVERITY    fail on lints with the specified severity
  -g NAME        select session group NAME
  -j INT         maximum number of parallel jobs
                 (default: 1 for local build, 0 for build cluster)
  -l             list the enabled lints (does not run the linter)
  -o OPTION      override Isabelle system OPTION (via NAME=VAL or NAME)
  -r MODE        how to report results (either "text", "json" or "xml", default "text")
  -v             verbose
  -x NAME        exclude session NAME and all descendants

Lint isabelle theories.
""",
      "A:" -> (arg => afp_root = Some(if (arg == ":") AFP.BASE else Path.explode(arg))),
      "B:" -> (arg => base_sessions += arg),
      "D:" -> (arg => select_dirs += Path.explode(arg)),
      "H:" -> (arg => build_hosts ++= Build_Cluster.Host.parse(Registry.global, arg)),
      "N" -> (_ => numa_shuffling = true),
      "O:" -> (arg => output_file = Some(Path.explode(arg))),
      "R" -> (_ => requirements = true),
      "V" -> (_ => verbose_build = true),
      "X:" -> (arg => exclude_session_groups += arg),
      "a" -> (_ => all_sessions = true),
      "c" -> (_ => clean_build = true),
      "d:" -> (arg => dirs += Path.explode(arg)),
      "f:" -> (arg => fail_on = Some(Severity.the_level(arg))),
      "g:" -> (arg => session_groups += arg),
      "j:" -> (arg => max_jobs = Some(Value.Nat.parse(arg))),
      "l" -> (_ => list_lints = true),
      "o:" -> (arg => options = options + arg),
      "r:" -> (arg => mode = arg),
      "v" -> (_ => verbose = true),
      "x:" -> (arg => exclude_sessions += arg))

    val sessions = getopts(args)

    val progress = new Console_Progress(verbose = verbose_build)

    val configuration = Lint_Store.Selection(options)

    if (list_lints) progress.echo(commas(configuration.get_lints.map(_.name).sorted))
    else {
      val presenter = mode match {
        case "text" => Text_Presenter(do_underline = true)
        case "afp" => Text_Presenter(do_underline = false)
        case "json" => JSON_Presenter
        case "xml" => XML_Presenter
        case "csv" => CSV_Presenter(configuration)
        case "count" => Count
        case _ => error("Unrecognized reporting mode " + mode)
      }

      progress.interrupt_handler {
        lint(
          configuration,
          presenter,
          output_file,
          options,
          selection = Sessions.Selection(
            requirements = requirements,
            all_sessions = all_sessions,
            base_sessions = base_sessions.toList,
            exclude_session_groups = exclude_session_groups.toList,
            exclude_sessions = exclude_sessions.toList,
            session_groups = session_groups.toList,
            sessions = sessions.toList),
          progress = progress,
          clean_build = clean_build,
          afp_root = afp_root,
          dirs = dirs.toList,
          select_dirs = select_dirs.toList,
          numa_shuffling = Host.numa_check(progress, numa_shuffling),
          max_jobs = max_jobs,
          build_hosts = build_hosts.toList,
          fail_on = fail_on,
          console = output_file.isEmpty,
          verbose = verbose)
      }
    }
  })
}

class Linter_Tools extends Isabelle_Scala_Tools(
  Linter.isabelle_tool, Lint_Store.isabelle_tool, Lint_Store.Bundle.isabelle_tool)