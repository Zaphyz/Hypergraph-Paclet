# A second take on rewriting systems

This folder is a parallel stack, written from the ground up, to be compared against
`src/Causality.wl` and either adopted or dropped. **Nothing outside this folder uses it, and it
changes nothing that already exists.**

```
experimental/
  RewritingSystems.wl                   the rewriting stack
  TraceletAnalysis.wl                   tracelets, after Behr
  notebooks/RewritingBasics.nb          the notebook to review the stack by
  notebooks/TraceletBasics.nb           the notebook for tracelets
  build/makenb.wls, build/maketraceletnb.wls
  tests/RewritingSystemsExamples.wls, tests/TraceletExamples.wls
```

## Running it

```bash
wolframscript -file experimental/build/makenb.wls
wolframscript -file experimental/build/maketraceletnb.wls
wolframscript -file experimental/tests/RewritingSystemsExamples.wls
wolframscript -file experimental/tests/TraceletExamples.wls
```

The notebook self-loads on open. In a kernel:

```wolfram
PacletDirectoryLoad["<repo>"];
Needs["WolframInstitute`Hypergraphs`"];
Get["<repo>/experimental/RewritingSystems.wl"];
```

It has its own context, `WolframInstitute`HypergraphRewriting``, so none of its names collide
with the existing stack and the two can be loaded side by side.

## What it takes as given

Everything up to and including rule application: `Hypergraph`, `HypergraphPlot`,
`HypergraphRule`, `HypergraphRuleApply`, `HypergraphRulePlot`, `HypergraphEventPlot`. Everything
above that — the evolution, causality, and all the graphs — is new.

## The one idea

**A hyperedge is a thing with an identity, not a value.** Every edge of every state is a
*token*: created by exactly one event, consumed by at most one along any branch, carrying no
other history. Once that is written down, nothing else has to be computed.

| graph | what it is |
| --- | --- |
| `"CausalGraph"` | the tokens, read as arrows from the event that made each to the event that used it |
| `"EventGraph"` | the states and the events, alternating — what the evolution literally did |
| `"StatesGraph"` | that, with the events contracted and equal states merged |
| `"TokenEventGraph"` | the tokens and events themselves, which the other three are projections of |
| `"BranchialGraph"` | the states of one step that share a parent |

The causal graph in particular needs no overlap test and no ancestry test, because a token
already knows both ends of its own story. That is the main structural difference from
`src/Causality.wl`, where causality is computed by intersecting sets of touched elements and
then filtering by whether one event lies on a path through the other.

## The surface

Two objects, fourteen properties, four options.

```wolfram
sys = RewritingSystem[{{1, 2}} -> {{1, 3}, {3, 2}}, {{1, 2}, {2, 3}}];
ev  = sys[2];
ev["CausalGraph"]
```

Options, given to the evolving or to any property: `"MaxStates"` (512), `"PanelSize"` (80),
`"Draw"` (Automatic), `"MaxDrawn"` (30), `"MergeStates"` (True), plus every option of `Graph`.

## What it deliberately does not have

**Causal dependence through a shared vertex.** Here a causal arrow is a hyperedge passing from
one event to another, which is the standard reading. `src/Causality.wl` also offers a weaker one
— two events related when their matches merely touch the same vertex — behind
`"CausalDependence"`. A token cannot express that; it would need vertices to be tokens too. This
is the real thing to weigh between the two stacks.

Also absent: a genuine multiway states graph with tokens shared across branches, and anything
about causal invariance or confluence. Both are natural next steps if this stack is the one
kept.

## Tracelet analysis

`TraceletAnalysis.wl` adds one function, after Nicolas Behr's *Tracelets and Tracelet Analysis
Of Compositional Rewriting Systems* (ACT 2019, EPTCS 323, 44–71, arXiv:1904.12829).

A **tracelet** is a derivation trace stripped of its context: the steps, and nothing of the
hypergraph they ran in except what they actually used. Behr's characterisation theorem says
every length-n trace is a tracelet applied to some ambient object.

The categorical account is pushouts and spans of monomorphisms. Here it is tokens, which the
rewriting stack already has:

| categorical | here |
| --- | --- |
| input interface | tokens the trace consumed and none of its steps created |
| output interface | tokens it created and none consumed |
| admissible match gluing step *i* to step *j* | a token created by *i* and consumed by *j* |
| context | everything else, which the tracelet drops |
| composite rule (concurrency theorem) | `HypergraphRule[input, output]` |
| shift equivalence | the linear extensions of the causal order |
| abstraction equivalence | canonical form of the two interfaces together, plus labels |

```wolfram
ts = TraceletAnalysis[sys, 2];     (* also [ev], [ev, n], [ev, {k1, k2, ...}] *)
ts[[1]]["Diagram"]                 (* the derivation, drawn, in minimal context *)
HypergraphRulePlot[ts[[1]]["Composite"]]
```

Subdividing an edge, two steps: **two** tracelets out of six runs. The causal one needs a
*single edge* as its whole context — not the two-edge path the system was run on. That gap is
the characterisation theorem made visible.

Options: `"Minimal"` (True), `"Causal"` (All, or True to keep only the connected ones),
`"Diagrams"` (True), `"PanelSize"` (90).

**Not implemented: FETA**, the generative half — building minimal traces ending in a chosen
event by composing rules along admissible matches, without running the system. That needs an
enumeration of overlaps between one rule's output and the next one's input. What is here is the
extraction half, which needs no search because the tokens already record it.

Two smaller limits, both stated in the notebook: abstraction equivalence is tested by an
invariant, not proved complete; and overlaps are hyperedges only, as causality is everywhere in
this stack.
