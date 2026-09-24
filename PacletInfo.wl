(* ::Package:: *)

(* The paclet's own description of itself. With this file in place the directory can be
   registered with PacletDirectoryLoad and then loaded with Needs, or installed outright with
   PacletInstall, after which the declared symbols load the paclet the first time one is used.

   The Kernel root is src/ rather than the usual Kernel/, and the context is paired with the file
   that opens it, because the entry file is Hypergraph.wl while the context is Hypergraphs` --
   singular and plural, deliberately, so that this paclet and the upstream one can share a
   kernel. *)

PacletObject[
  <|
    "Name" -> "WolframInstitute/Hypergraphs",
    "Description" -> "Hypergraph rewriting, and the hypermatrix algebra that goes with it",
    "Creator" -> "Carlos Zapata-Carratalá",
    "License" -> "MIT",
    "PublisherID" -> "WolframInstitute",
    "Version" -> "0.1.0",
    "WolframVersion" -> "14.0+",
    "PrimaryContext" -> "WolframInstitute`Hypergraphs`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "src",
        "Context" -> {{"WolframInstitute`Hypergraphs`", "Hypergraph.wl"}},
        "Symbols" -> {
          "WolframInstitute`Hypergraphs`AdjacencyHypermatrix",
          "WolframInstitute`Hypergraphs`ArrayAdd",
          "WolframInstitute`Hypergraphs`ArrayDimensions",
          "WolframInstitute`Hypergraphs`ArrayDomain",
          "WolframInstitute`Hypergraphs`ArrayEntries",
          "WolframInstitute`Hypergraphs`ArrayGraphics",
          "WolframInstitute`Hypergraphs`ArrayGraphics3D",
          "WolframInstitute`Hypergraphs`ArrayMultiply",
          "WolframInstitute`Hypergraphs`ArrayNesting",
          "WolframInstitute`Hypergraphs`ArrayObject",
          "WolframInstitute`Hypergraphs`ArrayObjectQ",
          "WolframInstitute`Hypergraphs`ArrayOrder",
          "WolframInstitute`Hypergraphs`ArraySymmetry",
          "WolframInstitute`Hypergraphs`ArrayTimes",
          "WolframInstitute`Hypergraphs`CanonicalHypergraph",
          "WolframInstitute`Hypergraphs`Edge",
          "WolframInstitute`Hypergraphs`EdgeLabel",
          "WolframInstitute`Hypergraphs`EdgeObjectQ",
          "WolframInstitute`Hypergraphs`EdgeSymmetry",
          "WolframInstitute`Hypergraphs`EdgeVertices",
          "WolframInstitute`Hypergraphs`FindArrayEquations",
          "WolframInstitute`Hypergraphs`FindHypergraphIsomorphism",
          "WolframInstitute`Hypergraphs`GenerateSymbolicArray",
          "WolframInstitute`Hypergraphs`Hypergraph",
          "WolframInstitute`Hypergraphs`HypergraphEmbedding",
          "WolframInstitute`Hypergraphs`HypergraphLargeQ",
          "WolframInstitute`Hypergraphs`HypergraphPlot",
          "WolframInstitute`Hypergraphs`HypergraphQ",
          "WolframInstitute`Hypergraphs`HypergraphRule",
          "WolframInstitute`Hypergraphs`HypergraphRuleApply",
          "WolframInstitute`Hypergraphs`HypergraphRuleMatches",
          "WolframInstitute`Hypergraphs`HypergraphRuleQ",
          "WolframInstitute`Hypergraphs`HypergraphSkeleton",
          "WolframInstitute`Hypergraphs`Hypermatrix",
          "WolframInstitute`Hypergraphs`HypermatrixArrays",
          "WolframInstitute`Hypergraphs`HypermatrixGraphics",
          "WolframInstitute`Hypergraphs`HypermatrixGraphics3D",
          "WolframInstitute`Hypergraphs`HypermatrixQ",
          "WolframInstitute`Hypergraphs`IdentityArray",
          "WolframInstitute`Hypergraphs`IsomorphicHypergraphQ",
          "WolframInstitute`Hypergraphs`OneArray",
          "WolframInstitute`Hypergraphs`PartialIdentityArray",
          "WolframInstitute`Hypergraphs`RegularArrayQ",
          "WolframInstitute`Hypergraphs`SetHypergraphPlotThresholds",
          "WolframInstitute`Hypergraphs`SimpleHypergraphQ",
          "WolframInstitute`Hypergraphs`Vertex",
          "WolframInstitute`Hypergraphs`VertexID",
          "WolframInstitute`Hypergraphs`VertexLabel",
          "WolframInstitute`Hypergraphs`VertexObjectQ",
          "WolframInstitute`Hypergraphs`ZeroArray"
        }
      },
      {
        "Documentation",
        "Language" -> "English"
      }
    }
  |>
]
