(* Simple test theory *)
theory Test
  imports Pure
begin

text \<open>\<^term>\<open>True\<close>\<close>

lemma [simp]: "\<And>P. PROP P \<Longrightarrow> PROP P"
proof -
  fix P assume "PROP P" thus "PROP P" .
qed

end