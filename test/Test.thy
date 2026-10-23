(* Simple test theory *)
theory Test
  imports Pure
begin

lemma [simp]: "\<And>P. PROP P \<Longrightarrow> PROP P"
proof -
  fix P assume "PROP P" thus "PROP P" .
qed

end