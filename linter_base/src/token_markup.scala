/*  Title:      linter_base/src/token_markup.scala
    Author:     Fabian Huch

Maps markup trees into outer syntax tokens.
*/

package isabelle.linter


import isabelle._

import scala.annotation.tailrec


object Token_Markup {
  val elements =
    Markup.Elements(
      Markup.COMMENT, Markup.KEYWORD1, Markup.CARTOUCHE, Markup.LANGUAGE, Markup.ENTITY,
      Markup.KEYWORD2, Markup.ALT_STRING, Markup.STRING, Markup.DELIMITER, Markup.FREE,
      Markup.TFREE, Markup.QUASI_KEYWORD, Markup.KEYWORD3, Markup.VAR, Markup.TVAR, Markup.SKOLEM,
      Markup.WARNING, Markup.CLASS_PARAMETER, Markup.EXPORT_PATH)

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
}
