package isabelle.linter

import isabelle._

object Lint_Description
{
  val empty: Lint_Description = Lint_Description(Nil)
}

abstract class Description_Element

case class Inline_Code(str: String) extends Description_Element
case class Plain(str: String) extends Description_Element
case class Line_Break() extends Description_Element
case class Empty_Line() extends Description_Element
case class Code_Block(strs: Seq[String]) extends Description_Element
case class Reference(url: String) extends Description_Element

case class Lint_Description(rev_els: List[Description_Element])
{
  def code_block(strs: String*): Lint_Description =
    add(Code_Block(strs))

  def inline_code(str: String): Lint_Description =
    add(Inline_Code(str))

  def add(e: Description_Element): Lint_Description =
    Lint_Description(e :: rev_els)

  def add(str: String): Lint_Description =
    add(Plain(str))

  def addln(str: String): Lint_Description =
    add(Plain(str)).breakline

  def breakline: Lint_Description =
    add(Line_Break())

  def references(url: String): Lint_Description =
    add(Reference(url))

  def empty_line: Lint_Description =
    add(Empty_Line())
}


abstract class Lint_Description_Renderer
{
  def render(e: Description_Element): String


  def render(description: Lint_Description): String =
    description.rev_els.reverse.map(render).mkString
}

object Markdown_Renderer extends Lint_Description_Renderer
{
  def wrap(left: String, right: String)(content: String): String =
    s"$left$content$right"

  def wrapTags(tag: String): String => String =
    wrap("<" + tag + ">", "</" + tag + ">")

  private val empty_line: String = "<br /><br />\n"

  private def sanitize(s: String): String =
    s.replace("<", "&lt").replace(">", "&gt")

  override def render(e: Description_Element): String =
    e match {
      case Inline_Code(str) => wrapTags("code")(sanitize(str))
      case Plain(str) => sanitize(str)
      case Line_Break() => "\n"
      case Empty_Line() => empty_line
      case Code_Block(strs) =>
        wrap("\n```isabelle\n", "\n```\n")(strs.map(sanitize).mkString("\n"))
      case Reference(url) => empty_line + "\nReferences: " + sanitize(url)
    }
}

object XML_Renderer extends Lint_Description_Renderer
{
  override def render(e: Description_Element): String =
    e match {
      case Inline_Code(str) => Library.quote(str)
      case Plain(str) => str
      case Line_Break() => "\n"
      case Empty_Line() => "\n\n"
      case Code_Block(strs) => strs.mkString("\n")
      case Reference(url) => "\nReferences:" + url
    }
}
