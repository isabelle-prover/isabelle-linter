package isabelle.jedit_linter;


import java.lang.invoke.VarHandle;
import java.lang.reflect.Field;


// FIXME this should be unnecessary
public final class Handle {
  public static void set_modifiers(VarHandle handle, Field field, int modifiers) {
    handle.set(field, modifiers);
  }
}
