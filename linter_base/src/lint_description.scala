/* Author: Yecine Megdiche, TU Muenchen

Styled lint description elements.
 */

package isabelle.linter


import isabelle._


abstract class Description_Element

case class Inline_Code(str: String) extends Description_Element
case class Plain(str: String) extends Description_Element
case class Line_Break() extends Description_Element
case class Empty_Line() extends Description_Element
case class Code_Block(strs: Seq[String]) extends Description_Element
case class Reference(url: String) extends Description_Element

case class Lint_Description(rev_els: List[Description_Element])
{
  def code_block(strs: String*): Lint_Description = add(Code_Block(strs))

  def inline_code(str: String): Lint_Description = add(Inline_Code(str))

  def add(e: Description_Element): Lint_Description = Lint_Description(e :: rev_els)

  def add(str: String): Lint_Description = add(Plain(str))

  def addln(str: String): Lint_Description = add(Plain(str)).breakline

  def breakline: Lint_Description = add(Line_Break())

  def references(url: String): Lint_Description = add(Reference(url))

  def empty_line: Lint_Description = add(Empty_Line())
}

object Lint_Description
{
  val empty: Lint_Description = Lint_Description(Nil)


  /* textual rendering */

  trait Presentation
  {
    def render(e: Description_Element): XML.Body

    def render(description: Lint_Description): XML.Body =
      description.rev_els.reverse.flatMap(render)
  }

  object Markdown_Presentation extends Presentation
  {
    override def render(e: Description_Element): XML.Body = e match {
      case Inline_Code(str) => List(HTML.code(HTML.text(str)))
      case Plain(str) => HTML.text(str)
      case Line_Break() => HTML.text("\n")
      case Empty_Line() => HTML.break ++ HTML.break
      case Code_Block(strs) => HTML.text("\n```isabelle\n" + strs.mkString("\n") + "\n```\n")
      case Reference(url) => HTML.break ++ HTML.break :+ HTML.link(url, HTML.text("\nReference"))
    }
  }

  object XML_Presentation extends Presentation
  {
    def render_string(e: Description_Element): String =
      e match {
        case Inline_Code(str) => Library.quote(str)
        case Plain(str) => str
        case Line_Break() => "\n"
        case Empty_Line() => "\n\n"
        case Code_Block(strs) => strs.mkString("\n")
        case Reference(url) => "\nReferences:" + url
      }

    override def render(e: Description_Element): XML.Body = List(XML.Text(render_string(e)))
  }
}
