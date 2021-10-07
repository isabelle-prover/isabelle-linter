package isabelle.jedit_linter

import isabelle.jedit._

import org.gjt.sp.jedit.msg.{PluginUpdate, PropertiesChanged}
import org.gjt.sp.jedit.{EBMessage, EBPlugin}


class Linter_Plugin extends EBPlugin
{
  JEdit_Extension.init

  val linter = new PIDE_Linter_Variable(Overlay_Lint_Reporter)
  val overlays = new Linter_Overlay.Variable

  private def deactivate(): Unit =
  {
    linter.uninstall_handlers()
    Linter_Plugin._instance = None
    overlays.clear()
  }

  override def handleMessage(message: EBMessage): Unit =
  {
    message match {
      case _: PluginUpdate =>
        if (PIDE._plugin != null) {
          linter.update(PIDE.options.value)
          linter.install_handlers()
          Linter_Plugin._instance = Some(this)
        } else if (Linter_Plugin.instance.isDefined) {
          deactivate()
        }
      case _: PropertiesChanged =>
        if (PIDE._plugin != null) {
          linter.update(PIDE.options.value)
        }
      case _ =>
    }
  }

  override def stop(): Unit = deactivate()
}

object Linter_Plugin
{
  /* plugin instance */

  @volatile private var _instance: Option[Linter_Plugin] = None
  def instance: Option[Linter_Plugin] = _instance
}