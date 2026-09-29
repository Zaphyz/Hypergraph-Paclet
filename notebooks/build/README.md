# Notebook generators

The three notebooks in `notebooks/` are generated, not edited by hand. Each script here builds
one of them:

| Script | Builds |
| --- | --- |
| `makenb.wls` | `notebooks/HypergraphBasics.nb` |
| `makehmnb.wls` | `notebooks/HypermatrixBasics.nb` |
| `makerwnb.wls` | `notebooks/RewritingBasics.nb` |

Run one with, from anywhere:

```
wolframscript -file notebooks/build/makerwnb.wls
```

Each script evaluates every input cell in its own kernel and attaches the result as a
`StandardForm` output cell, so the notebook always shows live output from `src/`. It reports
`failed: {}` when every example evaluated; a non-empty list names the ones that did not.

Two things to know before editing one.

**The examples share the build kernel with the script's own helpers.** An example that assigns to
a name the script uses — `sub`, `path`, `text` — silently replaces it, and the damage shows up as
an empty cell or a missing page rather than as an error. The helper names are `title`, `subtitle`,
`section`, `subsection`, `text`, `note`, `code`, `initCode`, `tidy`, `target`, `$root`, `$log`.
Sections that use short symbols as pattern variables clear them first, for the same reason in
reverse: a label symbol that has picked up a value stops being a variable.

**Regenerating rewrites every `ExpressionUUID` in the file**, so a notebook you did not change
still comes out as a whole-file diff. Run only the script whose notebook you actually edited.
