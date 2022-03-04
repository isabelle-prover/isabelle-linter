package isabelle.linter


import isabelle.Command.Blobs_Info
import isabelle.Document._
import isabelle._

import scala.collection.mutable.ListBuffer


object Linter_Tool
{
  def read_theory(
    db_context: Sessions.Database_Context,
    session_hierarchy: List[String],
    theory: String): Option[Snapshot] =
  {
    def read(name: String): Export.Entry =
      db_context.get_export(session_hierarchy, theory, name)

    def read_xml(name: String): XML.Body =
      YXML.parse_body(
        Symbol.output(unicode_symbols = false, UTF8.decode_permissive(read(name).uncompressed)),
        cache = db_context.cache)

    split_lines(read(Export.FILES).text).headOption map
    { thy_file =>
      val node_name = Resources.file_node(Path.explode(thy_file), theory = theory)

      val thy_xml = read_xml(Export.MARKUP)
      val command_spans = Token_Markup.from_xml(thy_xml)

      val counter = Counter.make()

      val (state0, node_edits, commands, _) = command_spans.foldLeft(State.init,
        List.empty[Text.Edit], List.empty[Command], 0) {
          case ((st, ed, cmd, offset), command_span) =>
            val command = Command(counter(), node_name, Blobs_Info.none, command_span)
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


  abstract class Lint_CLI
  {
    protected type A
    val reporter: Reporter[A]

    def get_linter_variable: Linter_Variable

    def process_snapshot(
      linter: Linter_Interface,
      snapshot: Snapshot,
      progress: Progress
    ): Unit = ()

    def process_end(
      progress: Progress
    ): Unit = ()

    def apply(
      options: Options,
      selection: Sessions.Selection = Sessions.Selection.empty,
      progress: Progress = new Progress,
      clean_build: Boolean = false,
      dirs: List[Path] = Nil,
      select_dirs: List[Path] = Nil,
      numa_shuffling: Boolean = false,
      max_jobs: Int = 1,
      verbose_build: Boolean = false,
      verbose: Boolean = false,
      log: Logger = No_Logger): Unit =
    {
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

      val linter_variable = get_linter_variable
      linter_variable.update(options + "linter=true")

      val linter = linter_variable.get.get

      val full_sessions =
        Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

      val deps = Sessions.deps(full_sessions.selection(selection))

      full_sessions.imports_selection(selection).foreach { session_name =>
        progress.echo("Linting " + session_name)

        val theories = deps.get(session_name).get.session_theories.map(_.theory)

        val store = Sessions.store(options)

        using(store.open_database_context())(db_context => {
          val result =
            db_context.input_database(session_name)((db, _) => {
              val errors = store.read_errors(db, session_name)
              store.read_build(db, session_name).map(info => (theories, errors, info.return_code))
            })
          result match {
            case None => error("Missing build database for session " + quote(session_name))
            case Some((used_theories, errors, _)) =>
              if (errors.nonEmpty) error(errors.mkString("\n\n"))
              for {
                thy <- used_theories
              } {
                val thy_heading = "\nTheory " + quote(thy) + ":"
                read_theory(db_context, List(session_name), thy) match {
                  case None => progress.echo(thy_heading + " MISSING")
                  case Some(snapshot) =>
                    progress.echo_if(verbose, "Processing theory " + snapshot.node_name.toString + " ...")
                    process_snapshot(linter, snapshot, progress)
                }
              }
          }
        })
      }

      process_end(progress)
    }
  }

  class Lint_JSON extends Lint_CLI
  {
    override protected type A = JSON.T

    override val reporter: Reporter[JSON.T] = JSON_Reporter

    val reports = new ListBuffer[JSON.T]()

    def get_linter_variable: Linter_Variable =
      new Linter_Variable(cache = false)

    override def process_snapshot(
      linter: Linter_Interface,
      snapshot: Snapshot,
      progress: Progress
    ): Unit =
    {
      val start_date = Date.now()
      val result = linter.lint_report(snapshot)
      val report = reporter.report_for_snapshot(result)
      val end_date = Date.now()
      val timing = end_date.time - start_date.time
      reports += JSON.Object(
        "theory" -> snapshot.node_name.toString,
        "report" -> report,
        "timing" -> timing.ms
      )
    }

    override def process_end(
      progress: Progress
    ): Unit =
    {
      progress.echo(
        JSON.Format(
          JSON.Object(
            "reports" -> reports.toList
          )
        )
      )
    }
  }

  object Lint_Text extends Lint_CLI
  {
    override protected type A = String

    override val reporter: Reporter[String] = Text_Reporter

    override def get_linter_variable: Linter_Variable =
      new Linter_Variable(cache = false)

    override def process_snapshot(
      linter: Linter_Interface,
      snapshot: Snapshot,
      progress: Progress
    ): Unit =
    {
      progress.echo(snapshot.node_name.toString + ":")
      val result = linter.lint_report(snapshot)
      val report = reporter.report_for_snapshot(result)
      if (report.isEmpty)
        progress.echo("No lints found.")
      else
        progress.echo(report)
    }

  }

  class Lint_XML extends Lint_CLI
  {

    override protected type A = XML.Body

    override val reporter: Reporter[XML.Body] = XML_Reporter

    val reports = new ListBuffer[XML.Tree]()

    override def get_linter_variable: Linter_Variable =
      new Linter_Variable(cache = false)

    override def process_snapshot(
      linter: Linter_Interface,
      snapshot: Snapshot,
      progress: Progress
    ): Unit =
    {
      val start_date = Date.now()
      val result = linter.lint_report(snapshot)
      val report = reporter.report_for_snapshot(result)
      val end_date = Date.now()
      val timing = end_date.time - start_date.time
      reports += XML.Elem(
        Markup("report", Linter_Markup.Theory(snapshot.node_name.toString) ::: Linter_Markup.Timing(timing.ms)),
        report
      )
    }

    override def process_end(
      progress: Progress
    ): Unit =
    {
      val xml_reports = XML.Elem(Markup("reports", Nil), reports.toList)
      progress.echo(XML.string_of_tree(xml_reports))
    }
  }

  def list_lints(options: Options, progress: Progress): Unit =
  {
    val linter_variable = new Linter_Variable()
    linter_variable.update(options + "linter=true")

    val configuration = linter_variable.get.get.configuration

    progress.echo(commas(configuration.get_lints.map(_.name).sorted))
  }

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lint", "lint theory sources based on PIDE markup",
    Scala_Project.here, args =>
  {
    val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

    var base_sessions: List[String] = Nil
    var select_dirs: List[Path] = Nil
    var numa_shuffling = false
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

    val getopts = Getopts(
      """
Usage: isabelle lint [OPTIONS] SESSION

  Options are:
  -B NAME      include session NAME and all descendants
  -D DIR       include session directory and select its sessions
  -N           cyclic shuffling of NUMA CPU nodes (performance tuning)
  -R           refer to requirements of selected sessions
  -V           verbose build
  -X NAME      exclude sessions from group NAME and all descendants
  -a           select all sessions
  -c           clean build
  -d DIR       include session directory
  -g NAME      select session group NAME
  -j INT       maximum number of parallel jobs (default 1)
  -l           list the enabled lints (does not run the linter)
  -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
  -r MODE      how to report results (either "text", "json" or "xml", default "text")
  -v           verbose
  -x NAME      exclude session NAME and all descendants

Lint isabelle theories.
""",
      "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
      "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
      "N" -> (_ => numa_shuffling = true),
      "R" -> (_ => requirements = true),
      "V" -> (_ => verbose_build = true),
      "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
      "a" -> (_ => all_sessions = true),
      "c" -> (_ => clean_build = true),
      "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
      "g:" -> (arg => session_groups = session_groups ::: List(arg)),
      "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
      "l" -> (_ => list = true),
      "o:" -> (arg => options = options + arg),
      "r:" -> (arg => mode = arg),
      "v" -> (_ => verbose = true),
      "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val sessions = getopts(args)

    val progress = new Console_Progress(verbose = verbose_build)

    if (list) list_lints(options, progress)
    else {

      val lint = mode match {
        case "text" => Lint_Text
        case "json" => new Lint_JSON()
        case "xml" => new Lint_XML()
        case _ => error(s"Unrecognized reporting mode $mode")
      }

      progress.interrupt_handler {
        lint(
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
          select_dirs = select_dirs,
          numa_shuffling = numa_shuffling,
          max_jobs = max_jobs,
          verbose_build = verbose_build,
          verbose = verbose)
      }
    }
  })
}

class Linter_Tool extends Isabelle_Scala_Tools(Linter_Tool.isabelle_tool)
