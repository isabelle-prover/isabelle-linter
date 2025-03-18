/* Author: Yecine Megdiche and Fabian Huch, TU Muenchen

Dockable window for the linter.
 */

package isabelle.jedit_linter


import isabelle._
import isabelle.jedit._
import isabelle.linter._
import isabelle.linter.Lint_Description.XML_Presentation

import scala.swing.{Alignment, Button, CheckBox, Component, Label, ListView, ScrollPane}
import scala.swing.event.{ButtonClicked, MouseClicked}

import java.awt.BorderLayout
import javax.swing.{BorderFactory, JList}

import org.gjt.sp.jedit.View


class Linter_Dockable(view: View, position: String) extends Dockable(view, position) {
  /* entry */

  private object Result {
    class Renderer extends ListView.Renderer[Result] {
      private object component extends Label {
        opaque = false
        xAlignment = Alignment.Leading
        border = BorderFactory.createEmptyBorder(2, 2, 2, 2)
      }

      def componentFor(
        list: ListView[_ <: Linter_Dockable.this.Result],
        isSelected: Boolean,
        focused: Boolean,
        result: Result,
        index: Int
      ): Component = {
        component.text = result.gui_text
        component
      }
    }
  }

  private class Result(rep: Linter.Result, current: Boolean) {
    def encode(i: Int): Int = Symbol.explode(rep.node.source.take(i)).length
    val pos =
      Position.Offset(encode(rep.range.start + 1)) :::
        Position.End_Offset(encode(rep.range.stop)) :::
        Position.File(rep.commands.head.node_name.node)
    def gui_name: GUI.Name = GUI.Name(rep.lint_name, kind = "lint")
    def gui_style: String =
      HTML.background_property(
        PIDE.rendering(view).color(
          rep.severity match {
            case Severity.Warn => Rendering.Color.warning_message
            case Severity.Error => Rendering.Color.error_message
          }))

    def gui_text: String = {
      val style = GUI.Style_HTML
      val bullet = style.triangular_bullet
      style.enclose_style(gui_style,
        bullet + " " + (if (current) style.make_bold else identity)(gui_name.set_style(style).toString) + 
          "<br>" + style.spaces(4) + style.make_text(style.make_text(rep.message)))
    }
    def follow(snapshot: Document.Snapshot): Unit =
      PIDE.editor.hyperlink_position(true, snapshot, pos).foreach(_.follow(view))
  }


  /* lint view */

  private val lint_view = new ListView(List.empty[Result]) {
    listenTo(mouse.clicks)
    reactions += {
      case MouseClicked(_, point, _, clicks, _) if clicks == 2 =>
        val index = peer.locationToIndex(point)
        if (index >= 0) listData(index).follow(PIDE.session.snapshot())
    }
  }
  lint_view.peer.setLayoutOrientation(JList.VERTICAL_WRAP)
  lint_view.peer.setVisibleRowCount(0)
  lint_view.selection.intervalMode = ListView.IntervalMode.Single
  lint_view.renderer = new Result.Renderer

  set_content(new ScrollPane(lint_view))


  /* update */

  def handle_lint(): Unit = {
    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot(view)
      if !snapshot.is_outdated
    } {
      val results =
        Linter_Plugin.instance.map(_.linter) match {
          case Some(linter) if linter.enabled =>
            val current_command =
              PIDE.editor.current_command(view, snapshot).getOrElse(Command.empty)

            val snapshot_res = linter.lint_report(snapshot)
            val current_res = snapshot_res.command_lints(current_command.id)
            val is_current = current_res.results.toSet

            if (lint_all) 
              snapshot_res.results.map(res => Result(res, is_current(res)))
            else current_res.results.map(Result(_, true))

          case _ => Nil
        }

      if (lint_view.listData.toList != results) lint_view.listData = results
    }
  }


  /* auto update */

  private var auto_lint_enabled: Boolean = true

  private def auto_lint(): Unit =
    GUI_Thread.require { if (auto_lint_enabled) handle_lint() }


  /* controls */

  private def lint_all: Boolean = PIDE.options.bool("lint_all")

  private def lint_all_=(b: Boolean): Unit = {
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

  private def linter_=(b: Boolean): Unit = {
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

  private val controls =
    Wrap_Panel(List(linter_button, auto_lint_button, lint_all_button, lint_button))

  add(controls.peer, BorderLayout.NORTH)


  /* main */

  private val main =
    Session.Consumer[Any](getClass.getName) {
      case _: Session.Global_Options =>
        GUI_Thread.later {
          auto_lint()
        }

      case _: Session.Commands_Changed =>
        GUI_Thread.later { auto_lint() }

      case Session.Caret_Focus =>
        GUI_Thread.later { auto_lint() }

      case _ =>
    }

  override def init(): Unit = {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
    PIDE.session.caret_focus += main
    auto_lint()
  }

  override def exit(): Unit = {
    PIDE.session.caret_focus -= main
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
  }
}
