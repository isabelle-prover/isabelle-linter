/* Author: Yecine Megdiche and Fabian Huch, TU Muenchen

Linter command-line tool.
 */

package isabelle.linter


import isabelle.Command.Blobs_Info
import isabelle.Document.*
import isabelle.*
import isabelle.linter.Linter.{Lint_Report, Severity}

import scala.collection.mutable.ListBuffer


object Linter_Tool {
  def read_theory(theory_context: Export.Theory_Context): Option[Snapshot] = {
    def read(name: String): Export.Entry =
      theory_context(name, permissive = true)

    def read_xml(name: String): XML.Body =
      YXML.parse_body(
        Symbol.output(unicode_symbols = false, UTF8.decode_permissive(read(name).bytes)),
        cache = theory_context.cache)

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

  case class Report[A](raw: Lint_Report, reports: List[A]) {
    def +(other: Report[A]): Report[A] =
      Report(new Lint_Report(raw.results ++ other.raw.results), reports ++ other.reports)
  }

  def lint_session[A](
    session_name: String,
    selection: Lint_Store.Selection,
    presenter: Presenter[A],
    store: Sessions.Store,
    deps: Sessions.Deps,
    verbose: Boolean,
    console: Boolean,
    progress: Progress
  ): Report[A] = {
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
            val start = Date.now()
            val raw = Linter.lint(snapshot, selection)
            val end = Date.now()

            val output = presenter.present_for_snapshot(raw)
            if (raw.results.nonEmpty) progress.echo_if(console, "Lints in " + thy + ":")
            progress.echo_if(console, { presenter.to_string(output) })

            val report = presenter.with_info(output, snapshot.node_name, end.time - start.time)
            Some(Report(raw, List(report)))
        }
      }.fold(Report(new Lint_Report(Nil), Nil))(_ + _)
    }
  }

  def lint[A](
    lint_selection: Lint_Store.Selection,
    presenter: Presenter[A],
    out_file: Option[Path],
    options: Options,
    selection: Sessions.Selection = Sessions.Selection.empty,
    progress: Progress = new Progress,
    clean_build: Boolean = false,
    fail_on: Option[Severity.Level] = None,
    dirs: List[Path] = Nil,
    select_dirs: List[Path] = Nil,
    numa_shuffling: Boolean = false,
    max_jobs: Int = 1,
    console: Boolean = true,
    verbose_build: Boolean = false,
    verbose: Boolean = false
  ): Unit = {
    val res =
      Build.build(options,
        selection,
        progress = progress,
        check_unknown_files = Mercurial.is_repository(Path.ISABELLE_HOME),
        clean_build = clean_build,
        dirs = dirs,
        select_dirs = select_dirs,
        numa_shuffling = NUMA.enabled_warning(progress, numa_shuffling),
        max_jobs = max_jobs,
        verbose = verbose_build)
    if (!res.ok) System.exit(res.rc)

    val store = Sessions.store(options)

    val full_sessions =
      Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

    val sessions_structure = full_sessions.selection(selection)
    val deps = Sessions.deps(sessions_structure)

    val lint_res: Report[A] = sessions_structure.build_selection(selection).map(session_name =>
      Future.fork {
        lint_session(session_name, selection = lint_selection, presenter = presenter, store = store,
          deps = deps, console = console, verbose = verbose, progress = progress)
        }).map(_.join).fold(Report(new Lint_Report(Nil), Nil))(_ + _)

    out_file.foreach { file => File.write(file, presenter.mk_string(lint_res.reports))}
    
    fail_on match {
      case Some(severity) =>
        if (lint_res.raw.results.exists(_.severity >= severity))
          System.exit(1)
      case None =>
    }
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lint", "lint theory sources based on PIDE markup",
    Scala_Project.here,
  { args =>
    val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

    var fail_on: Option[Severity.Level] = None
    var base_sessions: List[String] = Nil
    var select_dirs: List[Path] = Nil
    var numa_shuffling = false
    var output_file: Option[Path] = None
    var requirements = false
    var verbose_build = false
    var exclude_session_groups: List[String] = Nil
    var all_sessions = false
    var clean_build = false
    var dirs: List[Path] = Nil
    var session_groups: List[String] = Nil
    var max_jobs = 1
    var list = false
    var options = Options.init(opts = build_options)
    var mode = "text"
    var verbose = false
    var exclude_sessions: List[String] = Nil

    val getopts = Getopts("""
Usage: isabelle lint [OPTIONS] [SESSIONS ...]

  Options are:
  -B NAME        include session NAME and all descendants
  -D DIR         include session directory and select its sessions
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
  -j INT         maximum number of parallel jobs (default 1)
  -l             list the enabled lints (does not run the linter)
  -o OPTION      override Isabelle system OPTION (via NAME=VAL or NAME)
  -r MODE        how to report results (either "text", "json" or "xml", default "text")
  -v             verbose
  -x NAME        exclude session NAME and all descendants

Lint isabelle theories.
""",
      "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
      "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
      "N" -> (_ => numa_shuffling = true),
      "O:" -> (arg => output_file = Some(Path.explode(arg))),
      "R" -> (_ => requirements = true),
      "V" -> (_ => verbose_build = true),
      "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
      "a" -> (_ => all_sessions = true),
      "c" -> (_ => clean_build = true),
      "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
      "f:" -> (arg => fail_on = Some(Severity.the_level(arg))),
      "g:" -> (arg => session_groups = session_groups ::: List(arg)),
      "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
      "l" -> (_ => list = true),
      "o:" -> (arg => options = options + arg),
      "r:" -> (arg => mode = arg),
      "v" -> (_ => verbose = true),
      "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val sessions = getopts(args)

    val progress = new Console_Progress(verbose = verbose_build)

    val configuration = Lint_Store.Selection(options)

    if (list) progress.echo(commas(configuration.get_lints.map(_.name).sorted))
    else {
      val presenter = mode match {
        case "text" => Text_Presenter
        case "json" => JSON_Presenter
        case "xml" => XML_Presenter
        case _ => error(s"Unrecognized reporting mode $mode")
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
            base_sessions = base_sessions,
            exclude_session_groups = exclude_session_groups,
            exclude_sessions = exclude_sessions,
            session_groups = session_groups,
            sessions = sessions),
          progress = progress,
          clean_build = clean_build,
          dirs = dirs,
          fail_on = fail_on,
          select_dirs = select_dirs,
          numa_shuffling = numa_shuffling,
          max_jobs = max_jobs,
          console = output_file.isEmpty,
          verbose_build = verbose_build,
          verbose = verbose)
      }
    }
  })
}

class Linter_Tools extends Isabelle_Scala_Tools(
  Linter_Tool.isabelle_tool, Lint_Store.isabelle_tool, Lint_Store.Bundle.isabelle_tool)
