/* Author: Yecine Megdiche, TU Munich

Auxiliary tool to print lints.
 */

package isabelle.linter


import isabelle._
import linter.Linter._


object Lint_Descriptions
{
  def surround(str: String, kind: String): String
    = s"<$kind>$str</$kind>"

  def row(kind: String, cs: String*): String =
    surround(cs.map(surround(_, kind)).mkString, "tr")

  def print_lint_line(lint: Lint): String =
    row("td",
      lint.name,
      lint.severity.toString,
      Markdown_Renderer.render(lint.short_description),
      Markdown_Renderer.render(lint.long_description),
      Library.commas(Lint_Store.get_bundles_for_lint(lint.name)))

  def print_descriptions(progress: Progress = new Progress): Unit =
  {
    progress.echo("<table>")
    progress.echo(row("th", "Name", "Severity", "Short description", "Description", "Bundles"))
    progress.echo(Library.cat_lines(Lint_Store.lints.map(print_lint_line)))
    progress.echo("</table>")
  }

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lint_descriptions", "print the list of lints with their descriptions",
    Scala_Project.here, args =>
  {
    val getopts = Getopts("""
Usage: isabelle lint_descriptions

Print lint descriptions.
""")

    getopts(args)

    val progress = new Console_Progress()

    progress.interrupt_handler {
      print_descriptions(progress = progress)
    }
  })
}

class Lint_Descriptions extends Isabelle_Scala_Tools(Lint_Descriptions.isabelle_tool)
