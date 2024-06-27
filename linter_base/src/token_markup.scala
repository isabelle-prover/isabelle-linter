/*  Title:      linter_base/src/token_markup.scala
    Author:     Fabian Huch

Maps markup trees into outer syntax tokens.
*/

package isabelle.linter


import isabelle._

import scala.annotation.tailrec


object Token_Markup {
  def map_tokens(tree: XML.Tree): List[Token] = {
    def mk_token(kind: Token.Kind.Value, body: XML.Body): List[Token] =
      List(Token(kind, XML.content(body)))
    def mk_ident_token(ident: String): List[Token] =
      Token.explode(Keyword.Keywords.empty, ident)
    def mk_ident(body: XML.Body): List[Token] =
      mk_ident_token(XML.content(body))

    tree match {
      case XML.Elem(Markup(Markup.COMMENT, _), body) =>
        val content = XML.content(body)
        if (content.startsWith(Symbol.comment) || content.startsWith(Symbol.cancel) ||
          content.startsWith(Symbol.latex) || content.startsWith(Symbol.marker)) {
          if (content.startsWith(Symbol.comment)) mk_token(Token.Kind.FORMAL_COMMENT, body)
          else mk_token(Token.Kind.UNPARSED, body) // TODO fix Pure/General/Comment.content instead
        }
        else mk_token(Token.Kind.INFORMAL_COMMENT, body)
      case XML.Text(ws) if ws.isBlank => List(Token(Token.Kind.SPACE, ws))
      case XML.Elem(Markup(Markup.KEYWORD1, _), body) => mk_token(Token.Kind.COMMAND, body)
      case XML.Elem(Markup(Markup.CARTOUCHE, _), body) => body match {
        case List(XML.Elem(Markup(Markup.LANGUAGE, props), body)) => props match {
            case Markup.Name(Markup.Language.DOCUMENT) | Markup.Name(Markup.Language.ML) |
                 Markup.Name(Markup.Language.SML) | Markup.Name(Markup.Language.PATH) |
                 Markup.Name(Markup.Language.UNKNOWN) =>
              mk_token(Token.Kind.CARTOUCHE, body)
            case _ => mk_ident(body)
          }
        case _ => mk_ident(body)
      }
      case XML.Elem(m@Markup(Markup.ENTITY, _), body) => m match {
        case Markup.Entity.Def(_) => mk_ident(body)
        case Markup.Entity.Ref(_) => mk_ident(body)
        case _ => body.flatMap(map_tokens)
      }
      case XML.Elem(Markup(Markup.KEYWORD2, _), body) => body match {
        case List(XML.Elem(Markup(Markup.ENTITY, Markup.Kind(Markup.COMMAND)), body1)) =>
          mk_token(Token.Kind.COMMAND, body1)
        case _ => mk_token(Token.Kind.KEYWORD, body)
      }
      case XML.Elem(Markup(Markup.ALT_STRING, _), body) => mk_token(Token.Kind.ALT_STRING, body)
      case XML.Elem(Markup(Markup.STRING, _), body) => mk_token(Token.Kind.STRING, body)
      case XML.Elem(Markup(Markup.DELIMITER, _), body) =>
        if (List("-", "_").contains(XML.content(body))) mk_token(Token.Kind.SYM_IDENT, body)
        else if (XML.content(body).contains(Symbol.sub)) mk_token(Token.Kind.IDENT, body)
        else mk_token(Token.Kind.KEYWORD, body)
      case XML.Text(t) => Token.explode(Keyword.Keywords.empty, t)
      case XML.Elem(Markup(Markup.LANGUAGE, props), body) => props match {
        case Markup.Name(Markup.METHOD) | Markup.Name(Markup.ATTRIBUTE) |
             Markup.Name("mixfix_annotation") =>
          body.flatMap(map_tokens)
        case _ => mk_ident(body)
      }
      case XML.Elem(Markup(Markup.FREE, _), body) => mk_ident(body)
      case XML.Elem(Markup(Markup.TFREE, _), body) => mk_token(Token.Kind.TYPE_IDENT, body)
      case XML.Elem(Markup(Markup.QUASI_KEYWORD, _), body) => mk_ident(body)
      case XML.Elem(Markup(Markup.KEYWORD3, _), body) => mk_token(Token.Kind.COMMAND, body)
      case XML.Elem(Markup(Markup.VAR, _), body) => mk_token(Token.Kind.VAR, body)
      case XML.Elem(Markup(Markup.TVAR, _), body) => mk_token(Token.Kind.TYPE_VAR, body)
      case XML.Elem(Markup(Markup.SKOLEM, _), body) => mk_ident(body)
      case XML.Elem(Markup(Markup.WARNING, _), body) => body.flatMap(map_tokens)
      case XML.Elem(Markup(Markup.CLASS_PARAMETER, _), body) => mk_ident(body)
      case XML.Elem(Markup(Markup.EXPORT_PATH, _), body) => body.flatMap(map_tokens)
      case t => mk_token(Token.Kind.UNPARSED, List(t))
    }
  }

  def from_xml(thy_xml: XML.Body): List[Command_Span.Span] = {

    def find_span(t: XML.Tree): Option[Markup.Command_Span.Arg] = t match {
      case XML.Elem(Markup.Command_Span(arg), _) => Some(arg)
      case XML.Elem(_, body) => body.collectFirst((find_span _).unlift)
      case _ => None
    }

    @tailrec
    def reduce(start: Int, body: XML.Body, res: List[(Command_Span.Span, Int)]): List[Command_Span.Span] = {
      def is_ignored(t: XML.Tree): Boolean = t match {
        case XML.Text(ws) if ws.isBlank => true
        case XML.Elem(Markup(Markup.COMMENT, Nil), _) => true
        case _ => false
      }

      body match {
        case tree :: body1 =>
          tree match {
            case XML.Elem(Markup.Command_Span(arg), trees) =>
              val stop = start + XML.text_length(trees)
              val kind = Command_Span.Command_Span(
                proper_string(arg.kind), arg.name, Position.Range(Text.Range(start, stop)))
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
              case Some(arg) if XML.content(t) == arg.name =>
                val stop = start + arg.name.length
                val range = Position.Range(Text.Range(start, stop))
                val kind1 = Command_Span.Command_Span(proper_string(arg.kind), arg.name, range)
                val span = Command_Span.Span(kind1, List(Token(Token.Kind.COMMAND, arg.name)))
                reduce(stop, body1, res :+ (span, start))
              case _ =>
                val span = Command_Span.Span(Command_Span.Ignored_Span, map_tokens(t))
                reduce(start + XML.text_length(List(t)), body1, res :+ (span, start))
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
