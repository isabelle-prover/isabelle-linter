package isabelle.linter


import isabelle._

import scala.collection.mutable.ListBuffer


object Linter_Tool {

  abstract class Lint_CLI {
    protected type A
    val reporter: Reporter[A]

    def get_linter_variable: Linter_Variable

    def process_args(
        linter: Linter_Interface,
        args: Dump.Args,
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
    ): Unit = {

      val context =
        Dump.Context(
          options,
          progress = if (verbose) new Console_Progress(verbose = verbose_all) else new Progress,
          dirs = dirs,
          selection = selection,
          skip_base = true
        )

      val linter_variable = get_linter_variable
      linter_variable.update(options + "linter=true")

      val linter = linter_variable.get.get

      context.build_logic(logic)
      context
        .sessions(logic, log = log)
        .foreach(_.process((args: Dump.Args) => {
          progress.echo_if(verbose, "Processing theory " + args.print_node + " ...")
          val base_session_name = Long_Name.explode(args.print_node).head
          if (base_session_name == session_name) {
            process_args(linter, args, progress)
          } else
            progress.echo_if(verbose, "Skipping " + args.print_node + " (session mismatch) ...")

        }))
      context.check_errors
      process_end(progress)
    }
  }

  class Lint_JSON extends Lint_CLI {
    override protected type A = JSON.T

    override val reporter: Reporter[JSON.T] = JSON_Reporter

    val reports = new ListBuffer[JSON.T]()

    def get_linter_variable: Linter_Variable =
      new Linter_Variable(cache = false)

    override def process_args(
        linter: Linter_Interface,
        args: Dump.Args,
        progress: Progress
    ): Unit = {
      val start_date = Date.now()
      val result = linter.lint_report(args.snapshot)
      val report = reporter.report_for_snapshot(result)
      val end_date = Date.now()
      val timing = end_date.time - start_date.time
      reports += JSON.Object(
        "theory" -> args.print_node,
        "report" -> report,
        "timing" -> timing.ms
      )
    }

    override def process_end(
        progress: Progress
    ): Unit = {
      progress.echo(
        JSON.Format(
          JSON.Object(
            "reports" -> reports.toList
          )
        )
      )
    }
  }

  object Lint_Text extends Lint_CLI {
    override protected type A = String

    override val reporter: Reporter[String] = Text_Reporter

    override def get_linter_variable: Linter_Variable =
      new Linter_Variable(cache = false)

    override def process_args(
        linter: Linter_Interface,
        args: Dump.Args,
        progress: Progress
    ): Unit = {
      progress.echo(args.print_node + ":")
      val result = linter.lint_report(args.snapshot)
      val report = reporter.report_for_snapshot(result)
      if (report.isEmpty)
        progress.echo("No lints found.")
      else
        progress.echo(report)
    }

  }

  class Lint_XML extends Lint_CLI {

    override protected type A = XML.Body

    override val reporter: Reporter[XML.Body] = XML_Reporter

    val reports = new ListBuffer[XML.Tree]()

    override def get_linter_variable: Linter_Variable =
      new Linter_Variable(cache = false)

    override def process_args(
        linter: Linter_Interface,
        args: Dump.Args,
        progress: Progress
    ): Unit = {
      val start_date = Date.now()
      val result = linter.lint_report(args.snapshot)
      val report = reporter.report_for_snapshot(result)
      val end_date = Date.now()
      val timing = end_date.time - start_date.time
      reports += XML.Elem(
        Markup("report", Linter_Markup.Theory(args.print_node) ::: Linter_Markup.Timing(timing.ms)),
        report
      )
    }

    override def process_end(
        progress: Progress
    ): Unit = {
      val xml_reports = XML.Elem(Markup("reports", Nil), reports.toList)
      progress.echo(XML.string_of_tree(xml_reports))
    }
  }

  def list_lints(options: Options, progress: Progress): Unit = {
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
        var dirs: List[Path] = Nil
        var logic = Dump.default_logic
        var options = Options.init()
        var verbose = false
        var verbose_all = false
        var mode = "text"
        var list = false

        val getopts = Getopts(
          """
Usage: isabelle lint [OPTIONS] SESSION

  Options are:
    -b NAME      base logic image (default """ + isabelle.quote(Dump.default_logic) + """)
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
            case _                  => getopts.usage()
          }

        val lint = mode match {
          case "text" => Lint_Text
          case "json" => new Lint_JSON()
          case "xml"  => new Lint_XML()
          case _      => error(s"Unrecognized reporting mode $mode")
        }

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
