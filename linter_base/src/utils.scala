/* Author: Yecine Megdiche, TU Muenchen

Utilities.
 */

package isabelle.linter


object Utils
{
  def map_accum_l[A, B, C](xs: List[A], acc: B, result: (A, B) => (C, B)): List[C] = xs match {
    case Nil => Nil
    case head :: next =>
      result(head, acc) match {
        case (y, new_acc) => y :: map_accum_l(next, new_acc, result)
      }
  }
}
