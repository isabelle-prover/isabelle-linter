/* Author: Yecine Megdiche and Fabian Huch, TU Muenchen

Dockable window for the linter.
 */

package isabelle.jedit_linter


import isabelle._
import isabelle.jedit._
import isabelle.linter._

import scala.swing.event.ButtonClicked
import scala.swing.{Button, CheckBox}

import java.awt.BorderLayout

import org.gjt.sp.jedit.View


class Linter_Dockable(view: View, position: String) extends Dockable(view, position)
{
  /* text area */

  private var current_output: List[XML.Tree] = List(XML.Text("Press lint"))

  val pretty_text_area = new Pretty_Text_Area(view)
  set_content(pretty_text_area)

  override def detach_operation: Option[() => Unit] = pretty_text_area.detach_operation

  /* resize */

  private def handle_resize(): Unit =
  {
    GUI_Thread.require {}

    pretty_text_area.resize(
      Font_Info.main(PIDE.options.real("jedit_font_scale") * zoom.factor / 100))
  }


  /* update */

  val separator = XML_Presenter.text("----------------")
  val disabled = XML_Presenter.text("The linter plugin is disabled.")
  val empty = XML_Presenter.text("No lints found.")

  def handle_lint(): Unit =
  {
    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val new_output = Linter_Plugin.instance.map(_.linter) match {
        case Some(linter) if linter.enabled =>
          val current_command =
            PIDE.editor.current_command(view, snapshot).getOrElse(Command.empty)

          val report = linter.lint_report(snapshot)
          val snapshot_lints = XML_Presenter.present_for_snapshot(report, show_descriptions)
          val command_lints = XML_Presenter.present_for_command(report, current_command.id)

          val all_lints =
            if (lint_all && snapshot_lints.nonEmpty) separator ::: snapshot_lints
            else Nil

          val res = command_lints ::: all_lints
          if (res.nonEmpty) res else empty
        case _ => disabled
      }

      if (current_output != new_output) {
        pretty_text_area.update(snapshot, Command.Results.empty, Pretty.separate(new_output))
        current_output = new_output
      } else {
        pretty_text_area.refresh()
      }
    }
  }


  /* auto update */

  private var auto_lint_enabled: Boolean = true

  private def auto_lint(): Unit =
    GUI_Thread.require { if (auto_lint_enabled) handle_lint() }


  /* controls */

  private def lint_all: Boolean = PIDE.options.bool("lint_all")

  private def lint_all_=(b: Boolean): Unit =
  {
    if (lint_all != b) {
      PIDE.options.bool("lint_all") = b
      PIDE.editor.flush_edits(hidden = true)
      PIDE.editor.flush()
    }
  }

  private val lint_all_button = new CheckBox("Lint all") {
    tooltip = "Display lints of the whole document"
    reactions += { case ButtonClicked(_) =>
      lint_all = selected; handle_lint()
    }
    selected = lint_all
  }

  private def show_descriptions: Boolean = PIDE.options.bool("lint_descriptions")

  private def show_descriptions_=(b: Boolean): Unit =
  {
    if (show_descriptions != b) {
      PIDE.options.bool("lint_descriptions") = b
      PIDE.editor.flush_edits(hidden = true)
      PIDE.editor.flush()
    }
  }

  private val show_descriptions_checkbox = new CheckBox("Show Descriptions") {
    tooltip = "Whether to show lint descriptions"
    reactions += { case ButtonClicked(_) => show_descriptions = selected; handle_lint() }
    selected = show_descriptions
  }

  private val auto_lint_button = new CheckBox("Auto lint") {
    tooltip = "Indicate automatic lint following cursor movement"
    reactions += { case ButtonClicked(_) => auto_lint_enabled = selected; auto_lint() }
    selected = auto_lint_enabled
  }

  private val lint_button = new Button("Lint") {
    tooltip = "Lint and update display"
    reactions += { case ButtonClicked(_) => handle_lint() }
  }

  private def linter: Boolean = PIDE.options.bool("linter_enabled")

  private def linter_=(b: Boolean): Unit =
  {
    if (linter != b) {
      PIDE.options.bool("linter_enabled") = b
      Linter_Plugin.instance.foreach { plugin =>
        plugin.linter.update(PIDE.options.value)
        plugin.overlays.clear()
      }
      PIDE.editor.flush_edits(hidden = true)
      PIDE.editor.flush()
    }
  }

  private val linter_button = new CheckBox("Linter") {
    tooltip = "State of the linter"
    reactions += { case ButtonClicked(_) => linter = selected; handle_lint() }
    selected = linter
  }

  private val zoom = new Font_Info.Zoom_Box { def changed = handle_resize() }

  private val controls =
    Wrap_Panel(List(linter_button, auto_lint_button, lint_all_button, show_descriptions_checkbox, lint_button, zoom))

  add(controls.peer, BorderLayout.NORTH)


  /* main */

  private val main =
    Session.Consumer[Any](getClass.getName) {
      case _: Session.Global_Options =>
        GUI_Thread.later {
          handle_resize()
          auto_lint()
        }

      case _: Session.Commands_Changed =>
        GUI_Thread.later { auto_lint() }

      case Session.Caret_Focus =>
        GUI_Thread.later { auto_lint() }

      case _ =>
    }

  override def init(): Unit =
  {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
    PIDE.session.caret_focus += main
    handle_resize()
    auto_lint()
  }

  override def exit(): Unit =
  {
    PIDE.session.caret_focus -= main
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
  }
}
