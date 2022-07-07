package isabelle.jedit_linter;


import java.lang.invoke.VarHandle;
import java.lang.reflect.Field;


// FIXME this should be code in upstream / unnecessary
public final class Jedit_Extension {
  public static void set_modifiers(VarHandle handle, Field field, int modifiers) {
    handle.set(field, modifiers);
  }
}
