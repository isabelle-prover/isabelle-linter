/* Author: Yecine Megdiche, TU Munich

Auxiliary tool to print lint bundles.
 */

package isabelle.linter


import isabelle._

object Lint_Bundles
{
  def print_bundle_line(bundle: Lint_Store.Bundle): String =
    Lint_Descriptions.row(
      "td",
      bundle.name,
      Library.commas(bundle.lint_names.toList.sorted))

  def print_bundles(progress: Progress = new Progress): Unit =
  {
    progress.echo("<table>")
    progress.echo(Lint_Descriptions.row("th", "Bundle Name", "Lints"))
    progress.echo(Library.cat_lines(Lint_Store.all_bundles.map(print_bundle_line)))
    progress.echo("</table>")
  }

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lint_bundles", "print the lints belonging to each bundle.",
      Scala_Project.here, args =>
  {
      val getopts = Getopts("""
Usage: isabelle lint_bundles

print the lints belonging to each bundle.
""")

    getopts(args)

    val progress = new Console_Progress()

    progress.interrupt_handler {
      print_bundles(progress = progress)
    }
  })
}

class Lint_Bundles extends Isabelle_Scala_Tools(Lint_Bundles.isabelle_tool)
