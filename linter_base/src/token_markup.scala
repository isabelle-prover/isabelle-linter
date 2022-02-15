/*  Title:      linter_base/src/token_markup.scala
    Author:     Fabian Huch

Maps markup trees into outer syntax tokens.
*/

package isabelle.linter


import isabelle.Markup.{Command_Span => _, Kind => _, _}
import isabelle.Token._
import isabelle._

import scala.annotation.tailrec


object Token_Markup
{
  def map_tokens(tree: XML.Tree): List[Token] = {

    def mk_token(kind: Token.Kind.Value, body: XML.Body): List[Token] =
      List(Token(kind, XML.content(body)))
    def mk_ident_token(ident: String): List[Token] =
      Token.explode(Keyword.Keywords.empty, ident)
    def mk_ident(body: XML.Body): List[Token] =
      mk_ident_token(XML.content(body))

    tree match {
      case XML.Elem(Markup(COMMENT, _), body) =>
        val content = XML.content(body)
        if (content.startsWith(Symbol.comment) || content.startsWith(Symbol.cancel)
        || content.startsWith(Symbol.latex) || content.startsWith(Symbol.marker))
          mk_token(Kind.FORMAL_COMMENT, body)
        else mk_token(Kind.INFORMAL_COMMENT, body)
      case XML.Text(ws) if ws.isBlank => List(Token(Kind.SPACE, ws))
      case XML.Elem(Markup(KEYWORD1, _), body) => mk_token(Kind.COMMAND, body)
      case XML.Elem(Markup(CARTOUCHE, _), body) => body match {
        case List(XML.Elem(Markup(LANGUAGE, props), body)) => props match {
            case Name(Language.DOCUMENT) | Name(Language.ML) | Name(Language.SML) |
                 Name(Language.PATH) | Name(Language.UNKNOWN) => mk_token(Kind.CARTOUCHE, body)
            case _ => mk_ident(body)
          }
        case _ => mk_ident(body)
      }
      case XML.Elem(m@Markup(Markup.ENTITY, _), body) => m match {
        case Entity.Def(_) => mk_ident(body)
        case Entity.Ref(_) => mk_ident(body)
        case _ => body.flatMap(map_tokens)
      }
      case XML.Elem(Markup(KEYWORD2, _), body) => body match {
        case List(XML.Elem(Markup(ENTITY, Markup.Kind(COMMAND)), body1)) => mk_token(Kind.COMMAND, body1)
        case _ => mk_token(Kind.KEYWORD, body)
      }
      case XML.Elem(Markup(ALT_STRING, _), body) => mk_token(Kind.ALT_STRING, body)
      case XML.Elem(Markup(STRING, _), body) => mk_token(Kind.STRING, body)
      case XML.Elem(Markup(DELIMITER, _), body) =>
        if (List("-", "_").contains(XML.content(body))) mk_token(Kind.SYM_IDENT, body)
        else if (XML.content(body).contains(Symbol.sub)) mk_token(Kind.IDENT, body)
        else mk_token(Kind.KEYWORD, body)
      case XML.Text(t) => Token.explode(Keyword.Keywords.empty, t)
      case XML.Elem(Markup(LANGUAGE, props), body) => props match {
        case Name(METHOD) | Name(ATTRIBUTE) | Name("mixfix_annotation") => body.flatMap(map_tokens)
        case _ => mk_ident(body)
      }
      case XML.Elem(Markup(FREE, _), body) => mk_ident(body)
      case XML.Elem(Markup(TFREE, _), body) => mk_token(Kind.TYPE_IDENT, body)
      case XML.Elem(Markup(QUASI_KEYWORD, _), body) => mk_ident(body)
      case XML.Elem(Markup(KEYWORD3, _), body) => mk_token(Kind.COMMAND, body)
      case XML.Elem(Markup(VAR, _), body) => mk_token(Kind.VAR, body)
      case XML.Elem(Markup(TVAR, _), body) => mk_token(Kind.TYPE_VAR, body)
      case XML.Elem(Markup(SKOLEM, _), body) => mk_ident(body)
      case XML.Elem(Markup(WARNING, _), body) => body.flatMap(map_tokens)
      case XML.Elem(Markup(CLASS_PARAMETER, _), body) => mk_ident(body)
      case XML.Elem(Markup(EXPORT_PATH, _), body) => body.flatMap(map_tokens)
      case t => mk_token(Kind.UNPARSED, List(t))
    }
  }

  def from_xml(thy_xml: XML.Body): List[Command_Span.Span] = {

    def find_span(t: XML.Tree): Option[String] = t match {
      case XML.Elem(Markup.Command_Span(name), _) => Some(name)
      case XML.Elem(_, body) => body.collectFirst((find_span _).unlift)
    }

    @tailrec
    def reduce(start: Int, body: XML.Body, res: List[(Command_Span.Span, Int)]): List[Command_Span.Span] =
    {
      def is_ignored(t: XML.Tree): Boolean = t match {
        case XML.Text(ws) if ws.isBlank => true
        case XML.Elem(Markup(Markup.COMMENT, Nil), _) => true
        case _ => false
      }

      body match {
        case tree :: body1 =>
          tree match {
            case XML.Elem(Markup.Command_Span(name), trees) =>
              val stop = start + XML.text_length(trees)
              val kind = Command_Span.Command_Span(name, Position.Range(Text.Range(start, stop)))
              val span = Command_Span.Span(kind, trees.flatMap(map_tokens))
              reduce(stop, body1, res :+ (span, start))
            case XML.Elem(Markup(Markup.COMMENT, Nil), _) =>
              val trees = tree :: body1.takeWhile(is_ignored)
              val span = Command_Span.Span(Command_Span.Ignored_Span, trees.flatMap(map_tokens))
              reduce(
                start + XML.text_length(trees), body1.drop(trees.length - 1), res :+ (span, start))
            case XML.Text(t) if t.isBlank =>
              val trees = tree :: body1.takeWhile(is_ignored)
              val span = Command_Span.Span(Command_Span.Ignored_Span, trees.flatMap(map_tokens))
              reduce(
                start + XML.text_length(trees), body1.drop(trees.length - 1), res :+ (span, start))
            case t =>
              find_span(t) match {
              case Some(kind) if XML.content(t) == kind =>
                val stop = start + kind.length
                val kind1 = Command_Span.Command_Span(kind, Position.Range(Text.Range(start, stop)))
                val span = Command_Span.Span(kind1, List(Token(Kind.COMMAND, kind)))
                reduce(stop, body1, res :+ (span, start))
              case _ => error("Not a command span: " + t)
            }
          }
        case Nil => res.map(_._1)
      }
    }

    def unwrap(t: XML.Tree): XML.Tree = t match {
      case XML.Wrapped_Elem(markup, _, body) => XML.Elem(markup, body.map(unwrap))
      case XML.Elem(markup, body) => XML.Elem(markup, body.map(unwrap))
      case t: XML.Text => t
    }

    reduce(start = 0, thy_xml.map(unwrap), res = Nil)
  }
}
