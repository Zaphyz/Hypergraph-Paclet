---
name: commits-are-the-users-call
description: "Do not commit the Hypergraph Paclet repo unprompted; wait to be asked"
metadata:
  type: feedback
---

Do not commit unprompted, including at the end of a session. On 2026-09-24 the user stopped a
verification run that was heading towards an end-of-session commit and said the repo would be
committed in the next session instead.

**Why:** they treat committing as their decision about when a body of work is ready, not a
housekeeping step that follows automatically from the work passing its tests. An earlier
end-of-session commit on 2026-09-19 was accepted, so this is a correction to that precedent
rather than a rule that was always in force.

**How to apply:** finish the work, run the tests, report the state, and say plainly that nothing
is committed. Offer, then stop. Push is theirs regardless — `git push` from the Bash tool cannot
reach GitHub Desktop's credentials. See [[where-things-stand]], [[wl-dev-toolchain]].
