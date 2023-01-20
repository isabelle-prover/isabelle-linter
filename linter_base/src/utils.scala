/* Author: Yecine Megdiche and Fabian Huch, TU Muenchen

Utilities.
 */

package isabelle.linter


import isabelle._


object Utils {
  def map_accum_l[A, B, C](xs: List[A], acc: B, result: (A, B) => (C, B)): List[C] = xs match {
    case Nil => Nil
    case head :: next =>
      result(head, acc) match {
        case (y, new_acc) => y :: map_accum_l(next, new_acc, result)
      }
  }

  object HTML {
    import isabelle.HTML._

    val th = new Operator("th")
    val td = new Operator("td")
    val tr = new Operator("tr")
    val table = new Operator("table")

    def table_header(cols: List[XML.Body]): XML.Elem = tr(cols.map(th.apply))
    def table_row(cols: List[XML.Body]): XML.Elem = tr(cols.map(td.apply))
    def mk_table(header: XML.Elem, rows: List[XML.Elem]): XML.Elem = table.apply(header :: rows)
  }
}
