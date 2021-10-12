package isabelle.jedit_linter

import java.lang.invoke.{MethodHandles, VarHandle}
import java.lang.reflect.{Field, Modifier}

import isabelle._
import isabelle.jedit._
import isabelle.linter._

import org.gjt.sp.jedit.{Buffer, View, jEdit}
import org.gjt.sp.jedit.textarea.TextArea


// FIXME this should be code in upstream / unnecessary
object JEdit_Extension
{
  private val handle: VarHandle =
  {
    val lookup = MethodHandles.privateLookupIn(classOf[Field], MethodHandles.lookup)
    lookup.findVarHandle(classOf[Field], "modifiers", classOf[Int])
  }

  private def remove_final(field: Field) =
  {
    field.setAccessible(true)
    val mods = field.getModifiers
    jedit_extension.set_modifiers(handle, field, mods & ~Modifier.FINAL)
    field
  }

  def init: Unit =
  {
    val ext_elements = Markup.Elements(Linter_Markup.LINTER_SENDBACK, Linter_Markup.GOTO_POSITION)

    val active_elements = Rendering.getClass.getDeclaredField("active_elements")
    remove_final(active_elements).set(Rendering, Rendering.active_elements ++ ext_elements)
    val background_elements = Rendering.getClass.getDeclaredField("background_elements")
    remove_final(background_elements).set(Rendering, Rendering.background_elements ++ ext_elements)
  }

  private val path_ = "$LINTER_HOME/Linter.thy"
  lazy val path: String = Path.explode(path_).canonical.implode

  def open_linter_thy(view: View): Unit =
  {
    PIDE.editor.goto_file(true, view, path)
  }

  def on_linter_thy_open(view: View, buffer: Buffer): Unit =
  {
    val delay = PIDE.options.seconds("editor_input_delay") +
      PIDE.options.seconds("editor_generated_input_delay") +
      PIDE.options.seconds("editor_execution_delay") +
      PIDE.options.seconds("editor_output_delay")

    Delay.last(delay) {
      jEdit.closeBuffer(view, buffer)
    }.invoke()
  }

  def replace_range(snapshot: Document.Snapshot, text_area: TextArea,
    range: Symbol.Range, text: String): Unit =
  {
    val buffer = text_area.getBuffer
    if (!snapshot.is_outdated && text != "") {
      JEdit_Lib.buffer_edit(buffer) {
          buffer.remove(range.start, range.length)
          text_area.moveCaretPosition(range.start)
          text_area.setSelectedText(text)
        }
      }
  }

  class Handler extends Active.Handler {
    override def handle(view: View, text: String, elem: XML.Elem,
      doc_view: Document_View, snapshot: Document.Snapshot): Boolean =
    {
      elem match {
        case XML.Elem(Markup(Linter_Markup.LINTER_SENDBACK, Position.Range(range)), _) =>
          replace_range(snapshot, doc_view.text_area, range, text)
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
