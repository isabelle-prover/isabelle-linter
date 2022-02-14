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
      logic: String,
      session_name: String,
      verbose: Boolean = false,
      verbose_all: Boolean = false,
      progress: Progress = new Progress,
      log: Logger = No_Logger,
      dirs: List[Path] = Nil,
      selection: Sessions.Selection = Sessions.Selection.empty
    ): Unit =
    {

      val res =
        Build.build(
          options = options,
          progress = if (verbose) new Console_Progress(verbose = verbose_all) else new Progress,
          dirs = dirs,
          selection = selection
        )
      if (!res.ok) error("Failed to build: " + res.toString)

      val linter_variable = get_linter_variable
      linter_variable.update(options + "linter=true")

      val linter = linter_variable.get.get

      val store = Sessions.store(options)

      using(store.open_database_context())(db_context => {
        val result =
          db_context.input_database(session_name)((db, _) => {
            val theories = store.read_theories(db, session_name)
            val errors = store.read_errors(db, session_name)
            store.read_build(db, session_name).map(info => (theories, errors, info.return_code))
          })
        result match {
          case None => error("Missing build database for session " + quote(session_name))
          case Some((used_theories, errors, _)) =>
            if (errors.nonEmpty) error(errors.mkString("\n\n"))
            for {
              thy <- used_theories
              if thy.startsWith("HOL")
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

    sys.exit(0)
  }

  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool(
      "lint",
      "lint theory sources based on PIDE markup",
      Scala_Project.here,
      args => {
        val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

        var dirs: List[Path] = Nil
        var logic = Dump.default_logic
        var options = Options.init(opts = build_options)
        var verbose = false
        var verbose_all = false
        var mode = "text"
        var list = false

        val getopts = Getopts(
          """
Usage: isabelle lint [OPTIONS] SESSION

  Options are:
    -b NAME      base logic image (default """ + isabelle.quote(Dump.default_logic) +
            """)
    -d DIR       include session directory
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -v           verbose
    -V           verbose (General)
    -r MODE      how to report results (either "text", "json" or "xml", default "text")
    -l           list the enabled lints (does not run the linter)

  Lint isabelle theories.
""",
          "b:" -> (arg => logic = arg),
          "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
          "o:" -> (arg => options = options + arg),
          "v" -> (_ => verbose = true),
          "V" -> (_ => verbose_all = true),
          "r:" -> (arg => mode = arg),
          "l" -> (_ => list = true)
        )

        val more_args = getopts(args)

        val progress = new Console_Progress(verbose = verbose)

        if (list)
          list_lints(options, progress)

        val session_name =
          more_args match {
            case List(session_name) => session_name
            case _ => getopts.usage()
          }

        val lint = mode match {
          case "text" => Lint_Text
          case "json" => new Lint_JSON()
          case "xml" => new Lint_XML()
          case _ => error(s"Unrecognized reporting mode $mode")
        }

        val res = Build.build(
          options = options,
          selection = Sessions.Selection(sessions = List(session_name)),
          progress = progress,
        )

        progress.interrupt_handler {
          lint(
            options,
            logic,
            session_name,
            verbose = verbose,
            verbose_all = verbose_all,
            progress = progress,
            dirs = dirs,
            selection = Sessions.Selection(
              sessions = List(session_name)
            )
          )
        }
      }
    )
}

class Linter_Tool extends Isabelle_Scala_Tools(Linter_Tool.isabelle_tool)
