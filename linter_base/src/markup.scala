/* Author: Yecine Megdiche, TU Muenchen

Custom linter markup.
 */

package isabelle.linter


import isabelle._


object Linter_Markup {
  val LINTER_SENDBACK = "linter_sendback"
  val GOTO_POSITION = "goto_position"

  val LINT_RESULT = "lint_result"

  val LINT_NAME = "lint_name"
  val Lint_Name = new Properties.String(LINT_NAME)

  val LINT_MESSAGE = "lint_message"
  val Lint_Message = new Properties.String(LINT_MESSAGE)

  val LINT_SEVERITY = "lint_severity"

  object Lint_Severity {
    def apply(severity: Severity.Level): Properties.T =
      List((LINT_SEVERITY, severity.toString))

    def unapply(props: Properties.T): Option[Severity.Level] =
      props.find(_._1 == LINT_SEVERITY) match {
        case Some(p) => Severity.unapply(p._2)
        case _ => None
      }
  }

  val THEORY = "theory"
  val Theory = new Properties.String(THEORY)

  val TIMING = "timing"
  val Timing = new Properties.Long(TIMING)

  val LINT_COMMANDS = "lint_commands"

  object Lint_Commands {
    def apply(commands: List[Document_ID.Command]): Properties.T =
      List((LINT_COMMANDS, commands.mkString(",")))

    def unapply(props: Properties.T): Option[List[Document_ID.Command]] =
      props.find(_._1 == LINT_COMMANDS) match {
        case Some(p) =>
          try {
            Some(space_explode(',', p._2).map(Value.Long.parse))
          }
          catch {
            case ERROR(_) => None
          }
        case _ => None
      }
  }
}
