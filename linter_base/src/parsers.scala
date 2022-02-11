package isabelle.linter

import scala.util.parsing.combinator._
import scala.util.parsing.input
import isabelle._

import Linter._

object TokenParsers extends TokenParsers
{

  case class IndexPosition(ts: List[Text.Info[Token]], i: Int) extends input.Position
  {
    def column: Int = ts.slice(0, i + 1).map(_.info.content.length).sum

    def line: Int = 0

    protected def lineContents: String = (ts map {
      _.info.content
    }).mkString
  }

  case class TokenReader(in: List[Text.Info[Token]], from: Int = 0)
    extends input.Reader[Text.Info[Token]]
  {
    def first: Text.Info[Token] = in.head

    def rest: TokenReader = TokenReader(in.tail, from + 1)

    def pos: input.Position = IndexPosition(in, from)

    def atEnd: Boolean = in.isEmpty
  }

}

trait TokenParsers extends Parsers
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
  def pCommand(name: String): Parser[Elem] = elem(name, _.info.is_command(name))

  def pCommand(names: String*): Parser[Elem] = anyOf(names.map(pCommand))

  def pSpace: Parser[Elem] = elem("space", _.info.is_space)

  def pKeyword(name: String): Parser[Elem] = elem(name, _.info.is_keyword(name))

  def pIdent: Parser[Elem] = elem("ident", _.info.is_ident)

  def pNat: Parser[Elem] = elem("nat", _.info.is_nat)

  /* Surrounded parsers */
  def pSurrounded[T, U](left: Parser[T], right: Parser[T])(center: Parser[U]): Parser[U] =
    left ~> center <~ right

  def pSurroundedExtend[T, U](
    left: Parser[Text.Info[T]],
    right: Parser[Text.Info[T]]
  )(center: Parser[Text.Info[U]]): Parser[Text.Info[U]] =
    left ~ center ~ right ^^ { case Text.Info(lr, _) ~ Text.Info(_, c) ~ Text.Info(rr, _) =>
      Text.Info(Text.Range(lr.start, rr.stop), c)
    }

  def pOpenSqBracket: Parser[Elem] = pKeyword("[")

  def pClosedSqBracket: Parser[Elem] = pKeyword("]")

  def pSqBracketed[U]: Parser[U] => Parser[U] = pSurrounded(pOpenSqBracket, pClosedSqBracket)

  def pSqBracketedExtend[U]: Parser[Text.Info[U]] => Parser[Text.Info[U]] =
    pSurroundedExtend(pOpenSqBracket, pClosedSqBracket)

  def pOpenParen: Parser[Elem] = pKeyword("(")

  def pClosedParen: Parser[Elem] = pKeyword(")")

  def pParened[U]: Parser[U] => Parser[U] = pSurrounded(pOpenParen, pClosedParen)

  def pParenedExtend[U](center: Parser[Text.Info[U]]): Parser[Text.Info[U]] =
    pSurroundedExtend(pOpenParen, pClosedParen)(center)

  /* Simple elements */

  // Atoms can be too general, so propagate a predicate
  def pAtom(pred: Token => Boolean): Parser[Elem] =
    elem("atom", t => is_atom(t) && pred(t.info))

  def pName: Parser[Elem] = elem("name", _.info.is_name)

  /* Args */
  def pSingle_Arg(pred: Token => Boolean): Parser[List[Elem]] = pAtom(pred) ^^ {
    List(_)
  }

  def pArgs(pred: Token => Boolean): Parser[List[Elem]] =
    (pSqBracketed(pArg(pred).*) | pParened(pArg(pred).*)) ^^ {
      _.flatten
    }

  def pArg: Parser[List[Elem]] = pArg(_ => true)

  def pArg(pred: Token => Boolean): Parser[List[Elem]] = pSingle_Arg(pred) | pArgs(pred)

  object MethodParsers
  {

    /* Modifiers */
    def pTry: Parser[Text.Info[Method.Modifier]] = pKeyword("?") ^^ { case Text.Info(range, _) =>
      Text.Info(range, Method.Modifier.Try)
    }

    def pRep1: Parser[Text.Info[Method.Modifier]] = pKeyword("+") ^^ { case Text.Info(range, _) =>
      Text.Info(range, Method.Modifier.Rep1)
    }

    def pRestrict: Parser[Text.Info[Method.Modifier]] = pSqBracketedExtend(
      pNat ^^ { case Text.Info(range, n) =>
        Text.Info(range, Method.Modifier.Restrict(n.content.toInt))
      }
    )

    def pModifier: Parser[Text.Info[Method.Modifier]] = pTry | pRep1 | pRestrict

    def pCombinator(
      sep: String,
      combinator: Method.Combinator,
      nextPrecedence: Parser[Text.Info[Method]]
    ): Parser[Text.Info[Method]] =
      chainl1[Text.Info[Method]](
        nextPrecedence,
        pKeyword(sep)
          ^^^ { (left, right) =>
          Text.Info(
            Text.Range(left.range.start, right.range.stop),
            Combined_Method(left, combinator, right)
          )
        }
      )

    def pAlt: Parser[Text.Info[Method]] =
      pCombinator("|", Method.Combinator.Alt, pNameArgs | pParened(pMethods))

    def pStruct: Parser[Text.Info[Method]] = pCombinator(";", Method.Combinator.Struct, pAlt)

    def pSeq: Parser[Text.Info[Method]] = pCombinator(",", Method.Combinator.Seq, pStruct)

    /* Simple Methods */
    def pMethodArg: Parser[List[Elem]] = pArg { token =>
      !(token.is_open_bracket ||
        token.is_close_bracket ||
        (token.is_keyword && "|;,+".exists(token.is_keyword)))
    }

    def pNameOnly: Parser[Text.Info[Method]] =
      pName ^^ (name => Text.Info(name.range, Simple_Method(name)))

    def pNameArgs: Parser[Text.Info[Method]] = pName ~ pMethodArg.* ^^ { case name ~ args =>
      Text.Info(
        list_range(name.range :: args.flatten.map(_.range)),
        Simple_Method(name, args = args.flatten)
      )
    }

    def addModifier(
      method: Text.Info[Method],
      modifier: Option[Text.Info[Method.Modifier]]
    ): Text.Info[Method] =
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
                  modifiers :+ mod
                )
              )
            case Simple_Method(name, modifiers, args) =>
              Text.Info(
                Text.Range(method.range.start, mod_range.stop),
                Simple_Method(name, modifiers :+ mod, args)
              )
          }
      }

    /* Method */
    def pMethod: Parser[Text.Info[Method]] =
      (pNameOnly | pParenedExtend(pMethods)) ~ pModifier.? ^^ { case body ~ modifier =>
        addModifier(body, modifier)
      }

    def pMethods: Parser[Text.Info[Method]] = pSeq
  }

  def pMethod: Parser[Text.Info[Method]] = MethodParsers.pMethod

  /* Apply */
  def pApply: Parser[Text.Info[ASTNode]] = pCommand("apply") ~ pMethod ^^ {
    case applyToken ~ method =>
      Text.Info(Text.Range(applyToken.range.start, method.range.stop), Apply(method))
  }

  /* Isar-Proof */
  def pIsarProof: Parser[Text.Info[ASTNode]] =
    pCommand("proof") ~ pMethod.? ^^ {
      case proofToken ~ Some(method) =>
        Text.Info(Text.Range(proofToken.range.start, method.range.stop), Isar_Proof(Some(method)))
      case proofToken ~ None => Text.Info(proofToken.range, Isar_Proof(None))
    }

  def pQed: Parser[Text.Info[ASTNode]] = pCommand("qed") ~ pMethod.? ^^ {
    case qedToken ~ Some(method) =>
      Text.Info(Text.Range(qedToken.range.start, method.range.stop), Qed(Some(method)))
    case qedToken ~ None =>
      Text.Info(Text.Range(qedToken.range.start, qedToken.range.stop), Qed(None))
  }

  def pBy: Parser[Text.Info[ASTNode]] =
    pCommand("by") ~ pMethod ~ pMethod.? ^^ {
      case by ~ method1 ~ method2 =>
        val methodStop = method2.map(_.range).getOrElse(method1.range).stop
        Text.Info(
          Text.Range(by.range.start, methodStop),
          By(method1, method2)
        )
    }

  def pCounterExampleCommand: Parser[Text.Info[ASTNode]] =
    pCommand("nunchaku", "nitpick", "quickcheck") ~ (pSqBracketed(pAttributes) | success(Nil))  ^^ {
      case commandName ~ attributes =>
        val attr_stop = attributes.lastOption.flatMap(_.lastOption).map(_.range.stop).getOrElse(commandName.range.stop)
        Text.Info(Text.Range(commandName.range.start, attr_stop), Counter_Example_Finder(commandName, attributes))
    }

  /* Attributes */
  def pAttribute: Parser[List[Elem]] = (pIdent | pKeyword("=") | pKeyword("!") | pKeyword("?")).*

  def pAttributes: Parser[List[List[Elem]]] =
    chainl1[List[List[Elem]]](pAttribute ^^ {
      List(_)
    }, pKeyword(",") ^^^ {
      _ ::: _
    })

  def pAny: Parser[Elem] = elem("any", _ => true)

  def tokenParser: Parser[Text.Info[ASTNode]] = pApply | pIsarProof | pBy | pCounterExampleCommand | pQed

  def tryTransform[T](
    p: Parser[T],
    command: Parsed_Command,
    keepSpaces: Boolean = false
  ): Option[T] =
    parse(p, command.tokens, keepSpaces) match {
      case Success(result, _) => Some(result)
      case _: NoSuccess => None
    }

  def mkString(tokens: List[Elem]): String = tokens.map(_.info.source).mkString

  def parse[T](p: Parser[T], in: List[Elem], keepSpaces: Boolean = false): ParseResult[T] =
  {
    val processed = if (keepSpaces) in else in.filterNot(_.info.is_space)
    p(TokenParsers.TokenReader(processed))
  }
}
