---
name: rule-symbols-must-stay-clear
description: A label variable in a HypergraphRule is a global symbol — assigning to it later makes the rule hold itself and blows the recursion limit
metadata:
  type: reference
---

A `HypergraphRule` stores the literal symbols it was written with, including the ones serving as
label pattern variables. Giving one of those symbols a value *afterwards* substitutes that value
into every copy of the rule. Assigning a rewriting system built from a rule to a variable the
rule uses (`z = HypergraphRewritingSystem[zigRule, ...]` where `zigRule` has a label `z`) makes
the expression contain itself and dies with `$RecursionLimit::reclim`.

**Why:** it looks like a package bug and is not. It cost a confusing debugging detour on
2026-09-30, and it will recur, because short symbols like `x`, `y`, `z`, `f`, `g` are exactly
what one reaches for both as label variables and as scratch variables.

**How to apply:** in any script, test or notebook section that builds labelled rules, `ClearAll`
the label symbols first and never assign to them afterwards. `tests/CausalityExamples.wls`,
`tests/RewritingExamples.wls` §6 and every notebook generator do this at the top of the section,
with a comment. It is also written into `notebooks/CausalityBasics.nb` §7 as a design note.

See [[causal-dependence-is-an-option]].
