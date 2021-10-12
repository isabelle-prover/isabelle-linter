package isabelle.jedit_linter

import isabelle.jedit._

import org.gjt.sp.jedit.msg.{BufferUpdate, PluginUpdate, PropertiesChanged}
import org.gjt.sp.jedit.{EBMessage, EBPlugin}


class Linter_Plugin extends EBPlugin
{
  JEdit_Extension.init

  val linter = new PIDE_Linter_Variable(Overlay_Lint_Reporter)
  val overlays = new Linter_Overlay.Variable
  private var shutdown = false
  private var started = false
  private var loaded = false

  private def deactivate(): Unit =
  {
    shutdown = true
    linter.uninstall_handlers()
    Linter_Plugin._instance = None
    overlays.clear()
  }

  override def handleMessage(message: EBMessage): Unit =
  {
    if (!shutdown && !started && PIDE._plugin != null && PIDE.session.is_ready) {
      started = true
      JEdit_Extension.open_linter_thy(JEdit_Lib.jedit_view())
    }

    message match {
      case _: PluginUpdate =>
        if (PIDE._plugin != null && Linter_Plugin.instance.isEmpty && !shutdown) {
          linter.update(PIDE.options.value)
          linter.install_handlers()
          Linter_Plugin._instance = Some(this)
        } else if (Linter_Plugin.instance.isDefined) {
          deactivate()
        }

      case msg: BufferUpdate if msg.getWhat == BufferUpdate.LOADED
        && msg.getBuffer.getPath == JEdit_Extension.path
        && !loaded =>
        loaded = true
        JEdit_Extension.on_linter_thy_open(msg.getView, msg.getBuffer)

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