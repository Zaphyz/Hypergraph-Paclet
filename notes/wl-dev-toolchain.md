---
name: wl-dev-toolchain
description: How to invoke git and wolframscript on the Windows machine (git not on PATH) and the MacBook (both on PATH)
metadata: 
  node_type: memory
  type: project
  originSessionId: e3c3d21a-5f51-4cdd-ac11-634f3a2bc73c
  modified: 2026-10-11T00:00:00.000Z
---

## Windows

On the Windows machine `git` is **not** on PATH. Use the GitHub Desktop bundled copy:
`C:\Users\czapa\AppData\Local\GitHubDesktop\app-3.3.8\resources\app\git\cmd` (an `app-3.3.6` copy also exists; prefer the newest).

`wolframscript` **is** on PATH (WolframScript 1.14.0, Wolfram 15.0.1 at `C:\Program Files\Wolfram Research\Wolfram\15.0`).

Two practical notes for running WL from the PowerShell tool:
- `wolframscript -code '...'` mangles quotes through PowerShell. Write a `.wls` file to the scratchpad and run `wolframscript -file <path>` instead.
- Set `$ProgressReporting = False` at the top of any script that calls `TestReport`, or progress lines flood the output.

## macOS (MacBook, set up 2026-10-11)

Cloned from GitHub on 2026-10-11. `git` and `wolframscript` are both on PATH
(`/usr/local/bin/wolframscript`, WolframScript 1.14.0, Wolfram 15.0.0 for Mac OS X ARM — one patch
behind the Windows 15.0.1, which made no difference to any test). Run from zsh, so
`wolframscript -code '...'` quotes normally; a `.wls` file is still the better habit for anything
longer than a line. The `$ProgressReporting = False` note applies here too.

All seven suites passed on the first run here: `tests/*.wls` and `experimental/tests/*.wls`.
They locate `src/` from `$InputFileName`, so they run from any working directory, and the seven
can run in parallel, one kernel each.

Line endings: `.gitattributes` stores text as LF and leaves `.nb`/`.cdf` untouched, so the
notebooks keep the CRLF they were written with on Windows. That is expected, not drift.
`notebooks/legacy/*.nb` contain `C:\Users\czapa\...` paths in their saved output; harmless.

## Both machines

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
