/* Author: Fabian Huch, TU Muenchen

Various jEdit extensions.
 */

package isabelle.jedit_linter

import java.lang.invoke.{MethodHandles, VarHandle}
import java.lang.reflect.{Field, Modifier}

import isabelle._
import isabelle.jedit._
import isabelle.linter.{Token_Markup => _, _}

import org.gjt.sp.jedit.View
import org.gjt.sp.jedit.textarea.TextArea


// FIXME this should be code in upstream / unnecessary
object JEdit_Extension {
  private val handle: VarHandle = {
    val lookup = MethodHandles.privateLookupIn(classOf[Field], MethodHandles.lookup)
    lookup.findVarHandle(classOf[Field], "modifiers", classOf[Int])
  }

  private def remove_final(field: Field) = {
    field.setAccessible(true)
    val mods = field.getModifiers & ~Modifier.FINAL
    Handle.set_modifiers(handle, field, mods)
    field
  }

  def init: Unit = {
    val ext_elements = Markup.Elements(Linter_Markup.LINTER_SENDBACK, Linter_Markup.GOTO_POSITION)

    val active_elements = Rendering.getClass.getDeclaredField("active_elements")
    remove_final(active_elements).set(Rendering, Rendering.active_elements ++ ext_elements)
    val background_elements = Rendering.getClass.getDeclaredField("background_elements")
    remove_final(background_elements).set(Rendering, Rendering.background_elements ++ ext_elements)
  }

  def load_linter_thy(): Unit = {
    val path = Path.explode("$JEDIT_LINTER_HOME/Linter.thy").canonical

    val node = path.implode
    val theory = PIDE.resources.theory_name(Sessions.DRAFT, Thy_Header.theory_name(node))
    val node_name = Document.Node.Name(node, path.dir.implode, theory)

    GUI_Thread.later {
      Document_Model.provide_files(PIDE.session, List(node_name -> File.read(path)))
      Document_Model.node_required(node_name, set = true)
    }
  }

  def replace_range(
    snapshot: Document.Snapshot,
    text_area: TextArea,
    range: Symbol.Range, text: String
  ): Unit = {
    val buffer = text_area.getBuffer
    if (!snapshot.is_outdated) {
      JEdit_Lib.buffer_edit(buffer) {
        text_area.moveCaretPosition(range.start + range.length)
        val start_line = text_area.getCaretLine + 1
        text_area.setSelectedText(text)
        val end_line = text_area.getCaretLine
        for (line <- start_line to end_line) {
          Token_Markup.Line_Context.refresh(buffer, line)
          buffer.indentLine(line, true)
        }
        buffer.remove(range.start, range.length)
      }
    }
  }

  class Handler extends Active.Handler {
    override def handle(view: View, text: String, elem: XML.Elem,
      doc_view: Document_View, snapshot: Document.Snapshot): Boolean = {
      elem match {
        case XML.Elem(Markup(Linter_Markup.LINTER_SENDBACK, props), _) =>
          for {
            range <- Position.Range.unapply(props)
            replacement <- Markup.Content.unapply(props)
          } replace_range(snapshot, doc_view.text_area, range, replacement)
          true
        case XML.Elem(Markup(Linter_Markup.GOTO_POSITION, pos), _) =>
          val link = PIDE.editor.hyperlink_position(true, snapshot, pos)
          GUI_Thread.later { link.foreach(_.follow(view)) }
          true
        case _ => false
      }
    }
  }
}
