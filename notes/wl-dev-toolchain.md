---
name: wl-dev-toolchain
description: How to invoke git and wolframscript on this machine (git is not on PATH)
metadata: 
  node_type: memory
  type: project
  originSessionId: e3c3d21a-5f51-4cdd-ac11-634f3a2bc73c
  modified: 2026-09-17T19:09:11.311Z
---

On this Windows machine `git` is **not** on PATH. Use the GitHub Desktop bundled copy:
`C:\Users\czapa\AppData\Local\GitHubDesktop\app-3.3.8\resources\app\git\cmd` (an `app-3.3.6` copy also exists; prefer the newest).

`wolframscript` **is** on PATH (WolframScript 1.14.0, Wolfram 15.0.1 at `C:\Program Files\Wolfram Research\Wolfram\15.0`).

Two practical notes for running WL from the PowerShell tool:
- `wolframscript -code '...'` mangles quotes through PowerShell. Write a `.wls` file to the scratchpad and run `wolframscript -file <path>` instead.
- Set `$ProgressReporting = False` at the top of any script that calls `TestReport`, or progress lines flood the output.

See [[hypergraph-paclet-baseline]].

The three notebooks in `notebooks/` are **generated, not hand-edited**. Their scripts are tracked
at `notebooks/build/` (`makenb.wls`, `makehmnb.wls`, `makerwnb.wls`) — they lived in the session
scratchpad until 2026-09-29, where each session would have lost them. Run one with
`wolframscript -file notebooks/build/makerwnb.wls`. Two traps, also written into that folder's
README: the examples evaluate in the script's own kernel, so an example that assigns to a helper
name (`text`, `sub`, `path`, `section`) breaks it silently and shows up as an empty cell rather
than an error; and regenerating rewrites every `ExpressionUUID`, so a notebook you did not change
still comes out as a whole-file diff — run only the script whose notebook you edited.

`.gitignore` had an unanchored `build/` for paclet products, which matched `notebooks/build/` too
and made `git add -A` skip the scripts silently. Anchored to `/build/`.
