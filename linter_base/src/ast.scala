/* Author: Yecine Megdiche, TU Muenchen

Common AST elements for lints.
 */

package isabelle.linter


import isabelle._


abstract class AST_Node

abstract class Proof extends AST_Node

object Method {
  trait Modifier

  object Modifier {
    object Try extends Modifier // ?

    object Rep1 extends Modifier // +

    case class Restrict(n: Int) extends Modifier // [n]
  }

  trait Combinator

  object Combinator {
    object Seq extends Combinator // ,

    object Struct extends Combinator // ;

    object Alt extends Combinator // |
  }
}

abstract class Method extends AST_Node

case class Simple_Method(
  name: Text.Info[Token],
  modifiers: List[Text.Info[Method.Modifier]] = Nil,
  args: List[Text.Info[Token]] = Nil
) extends Method

case class Combined_Method(
  left: Text.Info[Method],
  combinator: Method.Combinator,
  right: Text.Info[Method],
  modifiers: List[Text.Info[Method.Modifier]] = Nil
) extends Method

case class Apply(method: Text.Info[Method]) extends Proof

case class Isar_Proof(method: Option[Text.Info[Method]]) extends Proof

case class By(method1: Text.Info[Method], method2: Option[Text.Info[Method]])
  extends Proof

case class Qed(method: Option[Text.Info[Method]]) extends  Proof

case class Counter_Example_Finder(name: Text.Info[Token], attributes: List[List[Text.Info[Token]]])
  extends AST_Node

case class Failed(string: String) extends AST_Node
