open Command;

val _ =
  let
    fun get_out s = case s of
        "info" => SOME writeln
      | "warn" => SOME warning
      | "error" => SOME error
      | _ => NONE
    fun write_msgs args =
      case args of
       level :: msg :: args1 =>
          let
           val msg1 = (msg |> XML.parse |> YXML.string_of) handle _ => msg
            val _ = case get_out level of SOME out => out msg1 | NONE => ()
          in write_msgs args1 end
      | _ => ()
  in
    print_function "print_xml"
      (fn {args, ...} =>
        SOME {delay = NONE, pri = Task_Queue.urgent_pri + 1, persistent = false, strict = false,
          print_fn = fn _ => fn _ => write_msgs args})
  end;