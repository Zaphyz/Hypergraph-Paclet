---
name: keep-docs-in-sync
description: Standing instruction to consult and update the documentation for the rest of this project
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 10479b59-77e7-4aef-9db2-6f32fa61072a
  modified: 2026-09-18T09:39:18.490Z
---

For the remainder of the Hypergraph Paclet project: **consult `Documentation/English/` when using any
of the package's functions, and update it whenever a function is added or an option is added to an
existing one.** Given on 2026-09-18, right after the documentation was first built.

**Why:** the reference pages are the contract for the package. The user reviews behaviour through
them, so a page that disagrees with the code is worse than no page — and options in particular are
easy to add in code and forget in prose.

**How to apply:**
- Before calling one of the package's own functions, check its reference page for the signature,
  the option names and their defaults, rather than working from memory.
- A new exported symbol needs a new page in
  `Documentation/English/ReferencePages/Symbols/<Name>.nb`, plus an entry in the right section of
  `Documentation/English/Guides/Hypergraphs.nb`, plus `See Also` links from related pages.
- A new option, a new message, or a changed default needs the affected page's "Details and Options"
  updated in the same change as the code, not afterwards.
- Pages are built by `Documentation/build_docs.wls`, which is the source of truth: it holds the
  prose and the examples, evaluates each example in the kernel, and writes the notebooks. Editing a
  `.nb` by hand would be lost on the next run. Re-run it after touching the package.
- Each page's "Source" section names the defining file and its section heading, never a line
  number, since line numbers go stale immediately.

See [[where-things-stand]], [[new-package-design]].
