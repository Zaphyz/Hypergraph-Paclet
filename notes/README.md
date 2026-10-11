# Project notes

Working notes kept across sessions: decisions and their reasons, standing instructions, and the
traps that have already cost time once. Exported here from Claude Code's per-project memory so
that they travel with the repository and are readable by a person, rather than living only on the
machine they were written on.

**Snapshot taken 2026-10-11.** On the machine a session is running on there is also a live copy
under `~/.claude/projects/<path-slug>/memory/`. If the two drift, the live copy is the newer one;
re-export it here when it changes.

| note | what it says |
| --- | --- |
| [Show hypergraphs as pictures](show-hypergraphs-as-pictures.md) | **Standing instruction.** Notebook and documentation examples must render their hypergraphs, not print expressions. It is what makes the system reviewable at all. Also stated in `CLAUDE.md`. |
| [Keep docs in sync](keep-docs-in-sync.md) | **Standing instruction.** Consult `Documentation/English/` when using the package's own functions, and update it in the same change as the code for every new function, option, message or default. |
| [Commits are the user's call](commits-are-the-users-call.md) | **Standing instruction.** Do not commit unprompted. Finish the work, run the tests, report the state, say plainly that nothing is committed, offer, and stop. |
| [Causal dependence is an option](causal-dependence-is-an-option.md) | Why `"CausalDependence"` exists: whether two events meeting only at a vertex are causally connected is a fact about what the hypergraph stands for, not about the rewriting, so the package must not decide it. |
| [Multiway rendering](multiway-rendering.md) | How the causal graphs are drawn to match the reference notebooks, and the three measurement traps — the inset size is a radius, partial `VertexCoordinates` lays out nothing, and the reference's `VertexSize -> 64` is a scaled fraction. |
| [Rule symbols must stay clear](rule-symbols-must-stay-clear.md) | A label variable in a `HypergraphRule` is a global symbol. Assigning to it later makes the rule contain itself and blows the recursion limit. Looks like a package bug; is not. |
| [New package design](new-package-design.md) | The object model of the rewrite, which `System` names are reserved, and three Wolfram Language traps it hit. |
| [Hypergraph Paclet baseline](hypergraph-paclet-baseline.md) | What the two upstream paclets provide, the two label-related gaps this project was started to close, and the state `src/` began in. |
| [WL dev toolchain](wl-dev-toolchain.md) | How to run things on Windows and on the Mac: `.wls` files rather than a kernel session, where the notebook generators live, and the two traps in regenerating notebooks. |
| [Where things stand](where-things-stand.md) | A running account of what is done and what comes next. The git log is the authority on what changed; this is the authority on why. |

## Where the rest of the story is

- `CLAUDE.md` — what the package is for, the naming terms, the content index, and the guiding
  principle that it must be visual.
- `README.md` — the tour, the repository layout, and the roadmap.
- `experimental/README.md` — the parallel rewriting stack and tracelet analysis, what they are
  built on, and what they deliberately lack.
- `git log` — every decision that reached the code, with its reasoning in the commit message.
