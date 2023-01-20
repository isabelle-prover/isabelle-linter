# Isabelle Linter
![status](https://github.com/isabelle-prover/isabelle-linter/actions/workflows/build.yml/badge.svg)

Linter for Isabelle, with jEdit integration.

## Setup
Requires Isabelle >= `2021-1-RC0`.
The linter can be used as a stand-alone tool or as a jEdit component.

Install with: `isabelle components -u <DIR>`. On Windows, use the `Cygwin-Terminal`.

For stand-alone (cli) tool only, add the component `<REPO_DIR>/linter_base` **instead**.

## Usage
Automatically starts with jEdit. On windows, start jEdit in the `Cygwin-Terminal` with `isabelle jedit`. 
Configuration can be done via the `Linter` panel and/or Isabelle options ([base](linter_base/etc/options), [jedit](jedit_linter/etc/options)).
Without further configuration, the `default` [bundle](#bundles) is activated.

CLI usage: `isabelle lint -?`.

### Auxiliary tools
Run with `isabelle <tool> -?`. Available tools:
- `lint_descriptions`: prints information about all lints
- `lint_bundles`: prints information about available bundles

## Lints
<table><tr><th>Name</th><th>Description</th><th>Bundles</th></tr><tr><td>apply_isar_switch</td><td>Switching from an apply script to a structured Isar proof results in an overall proof that is hard to read without relying on Isabelle. The Isar proof is also sensitive to the output of the apply script, and might therefore break easily.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style.html">
Reference</a></td><td>foundational, default</td></tr><tr><td>auto_structural_composition</td><td>Using <code>apply (auto;…)</code> results in a behavior that is hard to predict, so it is discouraged.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style-part2.html">
Reference</a></td><td>foundational, default</td></tr><tr><td>axiomatization_with_where</td><td>Unless when creating a new logic or extending an existing one with new axioms,the <code>axiomatization</code> command, when used, should not include a <code>where</code> clause.<br/><br/>The problem with the <code>where</code> clause is that it can introduce inconsistenciesinto the logic, for example:

```isabelle
axiomatization
  P :: &quot;&apos;a ⇒ bool&quot;
where
  all_true: &quot;∀x. P x&quot; and 
all_false: &quot;∀x. ¬P x&quot;
```
<br/><br/><a href="http://proofcraft.org/blog/isabelle-style.html">
Reference</a></td><td>default</td></tr><tr><td>bad_style_command</td><td>This lint detects bad-style commands:
<code>back</code>, <code>apply_end</code></td><td>foundational, default, afp_mandatory</td></tr><tr><td>complex_isar_initial_method</td><td>Initial <code>proof</code> methods should be kept simple, in order to keep the goals of the proof clear. For example, simplifier calls should be avoided, and not many methods should be combined. This lint finds complex methods in proof commands.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style.html">
Reference</a></td><td>foundational, default</td></tr><tr><td>complex_method</td><td>Warns users from using overly complex methods, i.e. if one of the following holds:

- has more than one modifier (<code>?, +, or []</code>), for example <code>auto?[4]</code>
- has three or more combinators (<code>|, ;, ,</code>), for example <code>auto ; rule , (force | blast)</code>
</td><td>foundational, default</td></tr><tr><td>counter_example_finder</td><td>This lint detects counter-example finders with no specific purpose: <code>nitpick</code>, <code>nunchaku</code>, and <code>quickcheck</code> (without <code>expect</code> or <code>satisfy</code>.</td><td>non_interactive_addon, afp_mandatory</td></tr><tr><td>diagnostic_command</td><td>This lint finds diagnostic commands: 
<code>ML_val</code>, <code>class_deps</code>, <code>code_deps</code>, <code>code_thms</code>, <code>find_consts</code>, <code>find_theorems</code>, <code>find_unused_assms</code>, <code>full_prf</code>, <code>help</code>, <code>locale_deps</code>, <code>prf</code>, <code>print_ML_antiquotations</code>, <code>print_abbrevs</code>, <code>print_antiquotations</code>, <code>print_attributes</code>, <code>print_bnfs</code>, <code>print_bundles</code>, <code>print_case_translations</code>, <code>print_cases</code>, <code>print_claset</code>, <code>print_classes</code>, <code>print_codeproc</code>, <code>print_codesetup</code>, <code>print_coercions</code>, <code>print_commands</code>, <code>print_context</code>, <code>print_definitions</code>, <code>print_defn_rules</code>, <code>print_facts</code>, <code>print_induct_rules</code>, <code>print_inductives</code>, <code>print_interps</code>, <code>print_locale</code>, <code>print_locales</code>, <code>print_methods</code>, <code>print_options</code>, <code>print_orders</code>, <code>print_quot_maps</code>, <code>print_quotconsts</code>, <code>print_quotients</code>, <code>print_quotientsQ3</code>, <code>print_quotmapsQ3</code>, <code>print_record</code>, <code>print_rules</code>, <code>print_simpset</code>, <code>print_state</code>, <code>print_statement</code>, <code>print_syntax</code>, <code>print_term_bindings</code>, <code>print_theorems</code>, <code>print_theory</code>, <code>print_trans_rules</code>, <code>smt_status</code>, <code>thm_deps</code>, <code>thm_oracles</code>, <code>thy_deps</code>, <code>unused_thms</code>, <code>welcome</code>, <code>term</code>, <code>prop</code>, <code>thm</code>, <code>typ</code></td><td>non_interactive_addon</td></tr><tr><td>force_failure</td><td>Since some methods do not guarantee to solve all their goals, it might be helpful to consider forcing their failure (e.g. using <code>apply (simp; fail)</code> instead of just <code>apply simp</code>) in order to make debugging proofs easier.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style-part2.html">
Reference</a></td><td>pedantic_addon</td></tr><tr><td>global_attribute_changes</td><td>Changing lemma attributes (e.g. <code>simp</code>) to to accomodate to a local proof discouraged, as it is error-prone and might result in hard-to debug problems.<br/><br/>Concretely, the lints warns the users of using this pattern:

```isabelle
declare word_neq_0_conv [simp]

  lemma …
  lemma …


declare word_neq_0_conv [simp del]
```
Instead, users should use the context, notes or bundle commands.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style-part2.html">
Reference</a></td><td>foundational, default</td></tr><tr><td>global_attribute_on_unnamed_lemma</td><td>Setting a global attribute (like <code>simp</code> or <code>elim</code>) on an unnamed lemma should be avoided, since it can make debugging proofs and removing the effect of that attribute harder.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style.html">
Reference</a></td><td>foundational, default, afp_mandatory</td></tr><tr><td>implicit_rule</td><td>Using <code>apply rule</code> results in Isabelle finding the suitable rule for the given context. However, if the process for finding the rule changes in the future, the proof might break. Instead, users should explicitly state which rule is needed.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style.html">
Reference</a></td><td>foundational, default</td></tr><tr><td>lemma_transforming_attribute</td><td>This lint warns of using transforming attributes (<code>simplified</code>, <code>rule_format</code><code>, and</code><code>unfolded</code>) on lemmas. Instead, the user should write the transformed form.<br/><br/><a href="http://proofcraft.org/blog/isabelle-style-part2.html">
Reference</a></td><td>foundational, default</td></tr><tr><td>low_level_apply_chain</td><td>Using long apply-scripts with low-level methods can quickly make proofs unreadable and unnecessarily long. This lints flags such scripts that are longer than 100 commands.</td><td>foundational, default</td></tr><tr><td>proof_finder</td><td>This lint detects proof-finder commands:
<code>sledgehammer</code>, <code>solve_direct</code>, <code>try</code>, <code>try0</code></td><td>non_interactive_addon</td></tr><tr><td>short_name</td><td>Finds functions or definitions with short names (one character).</td><td>default</td></tr><tr><td>smt_oracle</td><td>Using <code>declare [[smt_oracle]]</code> will make all smt act as an oracle. This might prove to be problematic, as oracle proofs are usually not to be trusted.</td><td>default, afp_mandatory</td></tr><tr><td>tactic_proofs</td><td>When using tactics, avoid outdated <code>induct_tac</code>, and do not refer to system-generated names. The lints warns about using the following methods: <code>induct_tac, rule_tac, case_tac</code></td><td>foundational, default</td></tr><tr><td>unfinished_proof</td><td>This lint detects unfinished proofs, characterized by the following commands:
<code>sorry</code>, <code>\&lt;proof&gt;</code></td><td>non_interactive_addon, afp_mandatory</td></tr><tr><td>unrestricted_auto</td><td>Using auto in the middle of a proof on all goals (i.e. unrestricted) might produce an unpredictable proof state. It should rather be used as a terminal proof method, or be restricted to a set of goals that it fully solves. <br/><br/><a href="http://proofcraft.org/blog/isabelle-style.html">
Reference</a></td><td>foundational, default</td></tr><tr><td>use_apply</td><td>This lint is the inverse direction of the <code>use_by</code> lint: it identifies usages of the <code>by</code> command and suggests toexpand the methods. As an example, it helps transform

```isabelle
lemma …
  by (induction xs) auto
```
into
```isabelle
lemma …
  apply (induction xs)
  apply auto
done
```
</td><td/></tr><tr><td>use_by</td><td>The <code>by</code> command allows to express method applications using <code>apply</code> more concisely. For example, instead of

```isabelle
lemma …
  apply (induction xs)
  apply auto
done
```

<code>by</code>can be used:

```isabelle
lemma …
  by (induction xs) auto
```
</td><td/></tr><tr><td>use_isar</td><td>This lint triggers on every use of the <code>apply</code> command and suggests to use an Isar proof instead.</td><td>pedantic_addon</td></tr></table>


## Bundles
<table><tr><th>Bundle Name</th><th>Warnings</th><th>Errors</th></tr><tr><td>foundational</td><td>apply_isar_switch, auto_structural_composition, bad_style_command, complex_isar_initial_method, complex_method, global_attribute_changes, global_attribute_on_unnamed_lemma, implicit_rule, lemma_transforming_attribute, low_level_apply_chain, tactic_proofs, unrestricted_auto</td><td/></tr><tr><td>default</td><td>apply_isar_switch, auto_structural_composition, bad_style_command, complex_isar_initial_method, complex_method, global_attribute_changes, global_attribute_on_unnamed_lemma, implicit_rule, lemma_transforming_attribute, low_level_apply_chain, short_name, smt_oracle, tactic_proofs, unrestricted_auto</td><td>axiomatization_with_where</td></tr><tr><td>pedantic_addon</td><td>force_failure, use_isar</td><td/></tr><tr><td>non_interactive_addon</td><td>counter_example_finder, diagnostic_command, proof_finder</td><td>unfinished_proof</td></tr><tr><td>afp_mandatory</td><td/><td>bad_style_command, counter_example_finder, global_attribute_on_unnamed_lemma, smt_oracle, unfinished_proof</td></tr></table>
