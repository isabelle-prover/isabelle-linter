package isabelle.linter

import isabelle._

abstract class ASTNode
abstract class Proof extends ASTNode

object Method {
  trait Modifier
  object Modifier {
    object Try extends Modifier // ?
    object Rep1 extends Modifier // +
    case class Restrict(val n: Int) extends Modifier // [n]
  }

  trait Combinator
  object Combinator {
    object Seq extends Combinator // ,
    object Struct extends Combinator // ;
    object Alt extends Combinator // |
  }

}

abstract class Method extends ASTNode

case class Simple_Method(
    val name: Text.Info[Token],
    val modifiers: List[Text.Info[Method.Modifier]] = Nil,
    val args: List[Text.Info[Token]] = Nil
) extends Method

case class Combined_Method(
    val left: Text.Info[Method],
    val combinator: Method.Combinator,
    val right: Text.Info[Method],
    val modifiers: List[Text.Info[Method.Modifier]] = Nil
) extends Method

case class Apply(val method: Text.Info[Method]) extends Proof
case class Isar_Proof(val method: Option[Text.Info[Method]]) extends Proof
case class By(val method1: Text.Info[Method], val method2: Option[Text.Info[Method]]) extends Proof

case class Failed(val string: String) extends ASTNode
