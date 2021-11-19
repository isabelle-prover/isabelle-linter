package isabelle.linter

object Utils
{

  def mapAccumL[A, B, C](xs: List[A], acc: B, result: (A, B) => (C, B)): List[C] = xs match {
    case Nil => Nil
    case head :: next =>
      result(head, acc) match {
        case (y, new_acc) => y :: mapAccumL(next, new_acc, result)
      }
  }

}
