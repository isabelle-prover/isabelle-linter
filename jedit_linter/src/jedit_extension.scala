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
  def load_linter_thy(): Unit = {
    val path = Path.explode("$JEDIT_LINTER_HOME/Linter.thy").canonical

    val node = path.implode
    val theory = PIDE.resources.theory_name(Sessions.DRAFT, Thy_Header.theory_name(node))
    val node_name = Document.Node.Name(node, theory)

    GUI_Thread.later {
      Document_Model.provide_files(PIDE.session, List(node_name -> File.read(path)))
      Document_Model.node_required(node_name, set = true)
    }
  }
}
