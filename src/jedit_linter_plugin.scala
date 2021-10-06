package isabelle.jedit_linter

import isabelle._

class JEdit_Linter_Plugin extends
  Scala_Project.Plugin(Path.explode("$LINTER_HOME/jedit_linter_plugin"))
