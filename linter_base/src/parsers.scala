/* Author: Yecine Megdiche, TU Muenchen

Lints as parser combinators.
 */

package isabelle.linter

import scala.util.parsing.combinator._
import scala.util.parsing.input
import isabelle._

import Linter._


object Token_Parsers extends Token_Parsers
{
  case class Token_Reader(in: List[Text.Info[Token]]) extends input.Reader[Text.Info[Token]]
  {
    def first: Text.Info[Token] = in.head

    def rest: Token_Reader = Token_Reader(in.tail)

    def pos: input.Position = input.NoPosition

    def atEnd: Boolean = in.isEmpty
  }
}

trait Token_Parsers extends Parsers
{
  type Elem = Text.Info[Token]

  def anyOf[T](ps: => Seq[Parser[T]]): Parser[T] = ps.reduce(_ | _)

  def is_atom(token: Token): Boolean = token.is_name ||
    token.kind == Token.Kind.TYPE_IDENT ||
    token.kind == Token.Kind.TYPE_VAR ||
    token.kind == Token.Kind.VAR || // term_var
    token.is_nat ||
    token.is_float ||
    token.is_keyword ||
    token.kind == Token.Kind.CARTOUCHE

  def is_atom(rtoken: Text.Info[Token]): Boolean = is_atom(rtoken.info)

  /* Token kinds */
  def p_command(name: String): Parser[Elem] = elem(name, _.info.is_command(name))

  def p_command(names: String*): Parser[Elem] = anyOf(names.map(p_command))

  def p_space: Parser[Elem] = elem("space", _.info.is_space)

  def p_keyword(name: String): Parser[Elem] = elem(name, _.info.is_keyword(name))

  def p_ident: Parser[Elem] = elem("ident", _.info.is_ident)

  def p_nat: Parser[Elem] = elem("nat", _.info.is_nat)

  /* Surrounded parsers */
  def p_surrounded[T, U](left: Parser[T], right: Parser[T])(center: Parser[U]): Parser[U] =
    left ~> center <~ right

  def p_surrounded_extend[T, U](
    left: Parser[Text.Info[T]],
    right: Parser[Text.Info[T]]
  )(center: Parser[Text.Info[U]]): Parser[Text.Info[U]] =
    left ~ center ~ right ^^ { case Text.Info(lr, _) ~ Text.Info(_, c) ~ Text.Info(rr, _) =>
      Text.Info(Text.Range(lr.start, rr.stop), c)
    }

  def p_open_sq_bracket: Parser[Elem] = p_keyword("[")

  def p_closed_sq_bracket: Parser[Elem] = p_keyword("]")

  def p_sq_bracketed[U]: Parser[U] => Parser[U] = p_surrounded(p_open_sq_bracket, p_closed_sq_bracket)

  def p_sq_bracketed_extend[U]: Parser[Text.Info[U]] => Parser[Text.Info[U]] =
    p_surrounded_extend(p_open_sq_bracket, p_closed_sq_bracket)

  def p_open_paren: Parser[Elem] = p_keyword("(")

  def p_closed_paren: Parser[Elem] = p_keyword(")")

  def p_parened[U]: Parser[U] => Parser[U] = p_surrounded(p_open_paren, p_closed_paren)

  def p_parened_extend[U](center: Parser[Text.Info[U]]): Parser[Text.Info[U]] =
    p_surrounded_extend(p_open_paren, p_closed_paren)(center)

  /* Simple elements */

  // Atoms can be too general, so propagate a predicate
  def p_atom(pred: Token => Boolean): Parser[Elem] =
    elem("atom", t => is_atom(t) && pred(t.info))

  def p_name: Parser[Elem] = elem("name", _.info.is_name)

  /* Args */
  def p_single_arg(pred: Token => Boolean): Parser[List[Elem]] = p_atom(pred) ^^ { List(_) }

  def p_args(pred: Token => Boolean): Parser[List[Elem]] =
    (p_sq_bracketed(p_arg(pred).*) | p_parened(p_arg(pred).*)) ^^ { _.flatten }

  def p_arg: Parser[List[Elem]] = p_arg(_ => true)

  def p_arg(pred: Token => Boolean): Parser[List[Elem]] = p_single_arg(pred) | p_args(pred)

  object Method_Parsers
  {
    def p_method(isOuter: Boolean = true): Parser[Text.Info[Method]] = {
      val nameParser = if (isOuter) p_name_only else p_name_args
      (nameParser | p_parened_extend(p_methods)) ~ p_modifier.? ^^ {
        case body ~ modifier => add_modifier(body, modifier)
      }
    }

    def p_methods: Parser[Text.Info[Method]] = p_seq

    def p_name_args: Parser[Text.Info[Method]] = p_name ~ p_method_arg.* ^^ {
      case name ~ args => Text.Info(
        list_range(name.range :: args.flatten.map(_.range)),
        Simple_Method(name, args = args.flatten))
    }

    def p_name_only: Parser[Text.Info[Method]] =
      p_name ^^ (name => Text.Info(name.range, Simple_Method(name)))

    def p_modifier: Parser[Text.Info[Method.Modifier]] = p_try | p_rep1 | p_restrict

    def p_alt: Parser[Text.Info[Method]] =
      p_combinator("|", Method.Combinator.Alt, p_method(false))

    def p_struct: Parser[Text.Info[Method]] = p_combinator(";", Method.Combinator.Struct, p_alt)

    def p_seq: Parser[Text.Info[Method]] = p_combinator(",", Method.Combinator.Seq, p_struct)

    def p_combinator(
      sep: String,
      combinator: Method.Combinator,
      nextPrecedence: Parser[Text.Info[Method]]
    ): Parser[Text.Info[Method]] =
      chainl1[Text.Info[Method]](
        nextPrecedence,
        p_keyword(sep)
          ^^^ { (left, right) =>
          Text.Info(
            Text.Range(left.range.start, right.range.stop),
            Combined_Method(left, combinator, right))
        })

    def p_try: Parser[Text.Info[Method.Modifier]] = p_keyword("?") ^^ {
      case Text.Info(range, _) => Text.Info(range, Method.Modifier.Try)
    }

    def p_rep1: Parser[Text.Info[Method.Modifier]] = p_keyword("+") ^^ {
      case Text.Info(range, _) => Text.Info(range, Method.Modifier.Rep1)
    }

    def p_restrict: Parser[Text.Info[Method.Modifier]] = p_sq_bracketed_extend(
      p_nat ^^ {
        case Text.Info(range, n) => Text.Info(range, Method.Modifier.Restrict(n.content.toInt))
      }
    )

    def p_method_arg: Parser[List[Elem]] = p_arg { token =>
      !(token.is_open_bracket ||
        token.is_close_bracket ||
        (token.is_keyword && "|;,+".exists(token.is_keyword)))
    }

    def add_modifier(method: Text.Info[Method], modifier: Option[Text.Info[Method.Modifier]]):
    Text.Info[Method] =
    {
      modifier match {
        case None => method
        case Some(mod@Text.Info(mod_range, _)) =>
          method.info match {
            case Combined_Method(left, combinator, right, modifiers) =>
              Text.Info(
                Text.Range(method.range.start, mod_range.stop),
                Combined_Method(
                  left,
                  combinator,
                  right,
                  modifiers :+ mod))
            case Simple_Method(name, modifiers, args) =>
              Text.Info(
                Text.Range(method.range.start, mod_range.stop),
                Simple_Method(name, modifiers :+ mod, args))
          }
      }
    }
  }

  def p_method: Parser[Text.Info[Method]] = Method_Parsers.p_method()

  /* Apply */
  def p_apply: Parser[Text.Info[AST_Node]] = p_command("apply") ~ p_method ^^ {
    case applyToken ~ method =>
      Text.Info(Text.Range(applyToken.range.start, method.range.stop), Apply(method))
  }

  /* Isar-Proof */
  def p_isar_proof: Parser[Text.Info[AST_Node]] =
    p_command("proof") ~ p_method.? ^^ {
      case proofToken ~ Some(method) =>
        Text.Info(Text.Range(proofToken.range.start, method.range.stop), Isar_Proof(Some(method)))
      case proofToken ~ None => Text.Info(proofToken.range, Isar_Proof(None))
    }

  def p_qed: Parser[Text.Info[AST_Node]] = p_command("qed") ~ p_method.? ^^ {
    case qedToken ~ Some(method) =>
      Text.Info(Text.Range(qedToken.range.start, method.range.stop), Qed(Some(method)))
    case qedToken ~ None =>
      Text.Info(Text.Range(qedToken.range.start, qedToken.range.stop), Qed(None))
  }

  def p_by: Parser[Text.Info[AST_Node]] =
    p_command("by") ~ p_method ~ p_method.? ^^ {
      case by ~ method1 ~ method2 =>
        val methodStop = method2.map(_.range).getOrElse(method1.range).stop
        Text.Info(
          Text.Range(by.range.start, methodStop),
          By(method1, method2))
    }

  def p_counter_example_command: Parser[Text.Info[AST_Node]] =
    p_command("nunchaku", "nitpick", "quickcheck") ~ (p_sq_bracketed(p_attributes) | success(Nil))  ^^ {
      case commandName ~ attributes =>
        val attr_stop = attributes.lastOption.flatMap(_.lastOption).map(_.range.stop).getOrElse(commandName.range.stop)
        Text.Info(Text.Range(commandName.range.start, attr_stop), Counter_Example_Finder(commandName, attributes))
    }

  /* Attributes */
  def p_attribute: Parser[List[Elem]] = (p_ident | p_keyword("=") | p_keyword("!") | p_keyword("?")).*

  def p_attributes: Parser[List[List[Elem]]] =
    chainl1[List[List[Elem]]](p_attribute ^^ { List(_)}, p_keyword(",") ^^^ { _ ::: _ })

  def p_any: Parser[Elem] = elem("any", _ => true)

  def token_parser: Parser[Text.Info[AST_Node]] = p_apply | p_isar_proof | p_by | p_counter_example_command | p_qed

  def try_transform[T](
    p: Parser[T],
    command: Parsed_Command,
    keepSpaces: Boolean = false): Option[T] =
  {
    parse(p, command.tokens, keepSpaces) match {
      case Success(result, _) => Some(result)
      case _: NoSuccess => None
    }
  }

  def mk_string(tokens: List[Elem]): String = tokens.map(_.info.source).mkString

  def parse[T](p: Parser[T], in: List[Elem], keepSpaces: Boolean = false): ParseResult[T] =
  {
    val processed = if (keepSpaces) in else in.filterNot(_.info.is_space)
    p(Token_Parsers.Token_Reader(processed))
  }
}
