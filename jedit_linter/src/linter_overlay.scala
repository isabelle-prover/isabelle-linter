package isabelle.jedit_linter

import isabelle._
import isabelle.jedit._


object Linter_Overlay
{
  type State = Map[Command, List[String]]

  private val overlay_fn = "print_xml"

  private def add_overlay(command: Command, args: List[String]) =
    PIDE.editor.insert_overlay(command, overlay_fn, args)

  private def remove_overlay(command: Command, args: List[String]) =
    PIDE.editor.remove_overlay(command, overlay_fn, args)

  object State
  {
    def empty: State = Map.empty
  }

  class Variable
  {
    private var current: State = State.empty

    def clear(): Unit = this.synchronized {
      current.foreach { case (command, overlay) => remove_overlay(command, overlay) }
      current = State.empty
    }

    def update(state: State): Unit = this.synchronized {
      for {
        (command, overlay) <- current
        if !state.get(command).contains(overlay)
      } remove_overlay(command, overlay)

      for {
        (command, overlay) <- state
        if !current.get(command).contains(overlay)
      } add_overlay(command, overlay)

      current = state
    }
  }
}
