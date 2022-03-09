/* Author: Fabian Huch, TU Muenchen

JEdit plugin setup.
 */

package isabelle.jedit_linter

import isabelle._

class JEdit_Linter_Plugin extends
  Scala_Project.Plugin(Path.explode("$JEDIT_LINTER_HOME/jedit_linter_plugin"))
