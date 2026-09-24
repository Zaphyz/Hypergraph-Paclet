(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- rendering arrays and hypermatrices*)


(* ::Text:: *)
(*Two families of pictures, and an array of any order may be drawn in either.*)
(**)
(*The flat family, ArrayGraphics, gives plane graphics throughout: a row of cells at order*)
(*one, a grid at order two, and above that a nesting of those -- a grid whose cells hold*)
(*grids. Nothing it draws is ever three-dimensional.*)
(**)
(*The spatial family, ArrayGraphics3D, gives cubes arranged in space throughout: a line of*)
(*cubes at order one, a plane of them at order two, a lattice at order three, and above*)
(*that the same lattices tiled through space, block beside block, all in one picture.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[ArrayGraphics, ArrayGraphics3D, HypermatrixGraphics, HypermatrixGraphics3D, ArrayNesting];


(* ::Section:: *)
(*Usage*)


ArrayGraphics::usage =
    "ArrayGraphics[arr] draws the array arr in the plane.\n" <>
    "An array of order one is drawn as a row of cells and one of order two as a grid; an array of " <>
    "order zero is a single cell.\n" <>
    "Only those orders have a flat picture of their own, so anything higher is drawn as a nesting " <>
    "of them: a 3-array as a row of grids, a 4-array as a grid of grids. \"Nesting\" -> {k1, k2, " <>
    "...} says how to split the indices, each block being one or two.\n" <>
    "The result is always a two-dimensional graphic. For cubes in space see ArrayGraphics3D.";

ArrayGraphics3D::usage =
    "ArrayGraphics3D[arr] draws the array arr as cubes arranged in space.\n" <>
    "An array of order one is drawn as a line of cubes, one of order two as a plane of them, and " <>
    "one of order three as a lattice.\n" <>
    "An array of higher order is drawn as a nesting: the blocks of the inner indices are laid out " <>
    "as lattices and those lattices are tiled through space by the outer ones, all in one picture. " <>
    "Each block may be one, two or three indices.\n" <>
    "The result is always a three-dimensional graphic. For plane pictures see ArrayGraphics.";

HypermatrixGraphics::usage =
    "HypermatrixGraphics[hm] draws each array of the hypermatrix hm in the plane, in canonical order.\n" <>
    "It takes the same options as ArrayGraphics.";

HypermatrixGraphics3D::usage =
    "HypermatrixGraphics3D[hm] draws each array of the hypermatrix hm as cubes in space, in canonical " <>
    "order.\n" <>
    "It takes the same options as ArrayGraphics3D.";

ArrayNesting::usage =
    "ArrayNesting[n] gives the way ArrayGraphics splits the indices of an array of order n by default: " <>
    "a list of block sizes, each at most two, summing to n.\n" <>
    "ArrayNesting[n, b] allows blocks of up to b indices; ArrayGraphics3D uses ArrayNesting[n, 3].\n" <>
    "ArrayNesting[arr] and ArrayNesting[arr, b] give the splitting for the order of arr.";


(* ::Section:: *)
(*Messages*)


ArrayGraphics::arr = "`1` is not an array.";
ArrayGraphics::nesting =
    "\"Nesting\" -> `1` is not a list of positive integers summing to `2`, the order of the array.";
ArrayGraphics::block =
    "\"Nesting\" -> `1` asks for a block of `2` indices, but a flat picture can hold only one or \
two; split the indices into smaller blocks, or draw it with ArrayGraphics3D.";
ArrayGraphics::big =
    "The array has `1` entries, more than the `2` that would be legible. Draw a part of it instead.";
ArrayGraphics::theme = "\"PlotTheme\" -> `1` is not one of \"Light\" or \"Dark\".";
ArrayGraphics::indices = "\"Indices\" -> `1` is not one of Automatic, None or \"Colored\".";

ArrayGraphics3D::arr = ArrayGraphics::arr;
ArrayGraphics3D::nesting = ArrayGraphics::nesting;
ArrayGraphics3D::block =
    "\"Nesting\" -> `1` asks for a block of `2` indices, but space holds only three at a time; \
split the indices into smaller blocks.";
ArrayGraphics3D::big = ArrayGraphics::big;
ArrayGraphics3D::theme = ArrayGraphics::theme;
ArrayGraphics3D::indices = ArrayGraphics::indices;

HypermatrixGraphics::hm = "`1` is not a hypermatrix.";
HypermatrixGraphics3D::hm = HypermatrixGraphics::hm;


Begin["`Private`"];


(* ::Section:: *)
(*Themes*)


(* The same blue the hypergraph drawing uses, so that the two halves of the package look like one
   thing. A dark theme is offered because the reference's own cubix pictures used one. *)

$arrayThemes = <|
    "Light" -> <|
        "Background" -> White,
        "Cell" -> Hue[0.63, 0.26, 0.89],
        "CellEdge" -> Hue[0.63, 0.7, 0.33],
        "Cube" -> Hue[0.63, 0.30, 0.86],
        "Entry" -> Black,
        "Index" -> Hue[0.63, 0.7, 0.5],
        "Frame" -> Hue[0.63, 0.7, 0.33],
        "Gnomon" -> Hue[0.63, 0.7, 0.5]
    |>,
    "Dark" -> <|
        "Background" -> GrayLevel[0.15],
        "Cell" -> Hue[0.63, 0.55, 0.45],
        "CellEdge" -> Hue[0.63, 0.35, 0.85],
        "Cube" -> Hue[0.63, 0.5, 0.6],
        "Entry" -> GrayLevel[0.97],
        "Index" -> Hue[0.63, 0.35, 0.85],
        "Frame" -> Hue[0.63, 0.35, 0.75],
        "Gnomon" -> Hue[0.63, 0.35, 0.85]
    |>
|>;

themeQ[t_] := KeyExistsQ[$arrayThemes, t];


(* ::Section:: *)
(*Nesting*)


(* How the indices of an array of order n are split into blocks when nothing is said. A flat
   picture holds one or two indices, a spatial one holds three, so the largest block differs
   between the two families and the rule is the same: blocks of the largest size allowed, with any
   remainder taken off the front.

   The remainder goes first rather than last so that a 3-array drawn flat comes out as a row of
   matrices -- the familiar reading, the first index choosing the matrix -- rather than as a
   matrix of rows. *)
ArrayNesting[n_Integer, b_Integer] /; n >= 0 && b >= 1 := Which[
    n === 0, {},
    n <= b, {n},
    Mod[n, b] === 0, ConstantArray[b, Quotient[n, b]],
    True, Prepend[ConstantArray[b, Quotient[n, b]], Mod[n, b]]
];
ArrayNesting[n_Integer] /; n >= 0 := ArrayNesting[n, 2];
ArrayNesting[a_, b_Integer] /; arrayLikeQ[a] := ArrayNesting[orderOf[a], b];
ArrayNesting[a_] /; arrayLikeQ[a] := ArrayNesting[orderOf[a], 2];
ArrayNesting[___] := $Failed;

(* An array object, or the plain nested list of one. *)
arrayLikeQ[a_] := ArrayObjectQ[a] || arrayDataQ[a];
entriesFor[a_] := If[ArrayObjectQ[a], ArrayEntries[a], a];
orderOf[a_] := If[ArrayObjectQ[a], ArrayOrder[a], dataOrder[a]];
dimsFor[a_] := If[ArrayObjectQ[a], ArrayDimensions[a], dataDims[a]];

validNestingQ[spec_, n_] :=
    VectorQ[spec, IntegerQ[#] && Positive[#] &] && Total[spec] === n;


(* ::Section:: *)
(*Options*)


Options[ArrayGraphics] = {
    "Nesting" -> Automatic,
    "Framed" -> True,
    "Indices" -> Automatic,
    "Gnomon" -> True,
    "PlotTheme" -> "Light",
    "MaxEntries" -> 512,
    ImageSize -> Automatic
};

Options[ArrayGraphics3D] = Options[ArrayGraphics];
Options[HypermatrixGraphics] = Options[ArrayGraphics];
Options[HypermatrixGraphics3D] = Options[ArrayGraphics];

(* The options every renderer needs, gathered once so that the recursive calls carry them down. *)
readOptions[head_, opts_List] := Module[{theme, indices},
    theme = OptionValue[ArrayGraphics, opts, "PlotTheme"];
    If[ ! themeQ[theme],
        Message[MessageName[head, "theme"], theme]; Return[$Failed, Module]];
    indices = OptionValue[ArrayGraphics, opts, "Indices"];
    If[ ! MatchQ[indices, Automatic | None | "Colored"],
        Message[MessageName[head, "indices"], indices]; Return[$Failed, Module]];
    <|
        "Theme" -> $arrayThemes[theme],
        "Framed" -> TrueQ @ OptionValue[ArrayGraphics, opts, "Framed"],
        "Indices" -> indices,
        "Gnomon" -> TrueQ @ OptionValue[ArrayGraphics, opts, "Gnomon"],
        "ImageSize" -> OptionValue[ArrayGraphics, opts, ImageSize]
    |>
];

(* Everything both families check before drawing anything: that it is an array, that the options
   parse, that the nesting accounts for every index in blocks the family can draw, and that the
   result would be legible. *)
prepare[head_, a_, maxBlock_, opts_List] := Module[{cfg, order, dims, nesting, count, limit},
    If[ ! arrayLikeQ[a], Message[MessageName[head, "arr"], HoldForm[a]]; Return[$Failed, Module]];

    cfg = readOptions[head, opts];
    If[cfg === $Failed, Return[$Failed, Module]];

    order = orderOf[a];
    dims = dimsFor[a];

    nesting = OptionValue[ArrayGraphics, opts, "Nesting"];
    If[nesting === Automatic, nesting = ArrayNesting[order, maxBlock]];
    If[ ! validNestingQ[nesting, order],
        Message[MessageName[head, "nesting"], nesting, order]; Return[$Failed, Module]];
    If[ AnyTrue[nesting, # > maxBlock &],
        Message[MessageName[head, "block"], nesting, First @ Select[nesting, # > maxBlock &]];
        Return[$Failed, Module]];

    count = If[dims === {}, 1, Times @@ dims];
    limit = OptionValue[ArrayGraphics, opts, "MaxEntries"];
    If[ IntegerQ[limit] && count > limit,
        Message[MessageName[head, "big"], count, limit]; Return[$Failed, Module]];

    <|"Config" -> cfg, "Order" -> order, "Dimensions" -> dims, "Nesting" -> nesting,
      "Entries" -> entriesFor[a]|>
];


(* ::Section:: *)
(*Cells and indices*)


(* One colour per axis, so that "Indices" -> "Colored" says which index is which rather than
   merely that there are indices. Beyond the third the colours repeat, which is harmless: no
   single block ever has more than three. *)
$axisColors = {Hue[0., 0.65, 0.72], Hue[0.33, 0.7, 0.55], Hue[0.63, 0.7, 0.5]};

indexColor[cfg_, axis_Integer] := Switch[cfg["Indices"],
    "Colored", $axisColors[[Mod[axis - 1, Length[$axisColors]] + 1]],
    _, cfg["Theme"]["Index"]
];

showIndicesQ[cfg_] := cfg["Indices"] =!= None;

$entryFontSize = 12;
$indexFontSize = 10;

(* Entries are not all one character wide: a symbolic array's entries read a[1, 2], and at order
   three they can be wider still. The cells are sized to the widest of them rather than to a
   number, so that nothing overflows its neighbour. A character count is a crude measure of width
   but a reliable one, and far cheaper than rasterizing to find out. *)
(* An element of a finite field is drawn as one compact glyph, but its InputForm runs to sixty
   characters or more, which would shrink the text to nothing and stretch the cell to its limit.
   Its width is what it looks like, not what it prints as. *)
entryWidth[_FiniteFieldElement] := 5;
entryWidth[e_] := StringLength[ToString[e, InputForm]];

cellWidthFor[entries_List] := With[{w = Max[entryWidth /@ entries, 1]},
    Clip[0.42 + 0.17 w, {1, 4.5}]
];

(* Among cubes the cells are fixed in space, so a wide entry is met by shrinking the text. *)
fontSizeFor[entries_List] := With[{w = Max[entryWidth /@ entries, 1]},
    Round @ Clip[$entryFontSize + 2 - w, {6, $entryFontSize}]
];

(* StandardForm, not the TraditionalForm that Graphics uses by default: an entry of a symbolic
   array is a[1, 2], and TraditionalForm prints that as a(1, 2), which is not what the array
   holds and is not something that could be typed back in. *)
entryText[e_, pos_, cfg_] := Text[
    Style[e, cfg["Theme"]["Entry"], FontSize -> Lookup[cfg, "FontSize", $entryFontSize]],
    pos
];

indexText[k_, pos_, cfg_, axis_] := Text[
    Style[k, indexColor[cfg, axis], FontSize -> $indexFontSize, FontSlant -> Italic],
    pos
];

(* A cell is a filled rounded rectangle when framed, and nothing at all when not: the entries then
   sit on the background, which is what the reference's unframed pictures look like. *)
cellShape[{x_, y_}, w_, cfg_] := With[{
    rect = Rectangle[{x - w/2 + 0.05, y - 0.45}, {x + w/2 - 0.05, y + 0.45},
                     RoundingRadius -> 0.08]
},
    Which[
        ! cfg["Framed"], {},
        (* a cell holding a picture is outlined, not filled: a filled one would show only at its
           corners, the picture inset over it covering the rest *)
        ! TrueQ @ Lookup[cfg, "CellFill", True],
            {FaceForm[None], EdgeForm[Directive[cfg["Theme"]["Frame"], Opacity[0.45]]], rect},
        True,
            {
                FaceForm[cfg["Theme"]["Cell"]],
                EdgeForm[Directive[cfg["Theme"]["CellEdge"], Opacity[0.9]]],
                rect
            }
    ]
];


(* ::Section:: *)
(*The flat family*)


(* Cell (i, j) sits at {j w, -i}, so that the first row is at the top and the first column at the
   left, as a matrix is written. Row indices go down the left margin and column indices along the
   top, whatever the order: a 1-array is a single row of cells. *)
cellCenter[i_, j_, w_] := {j w, -i};

gridGraphics[rows_, cols_, w_, content_, cfg_, opts_List] := Graphics[
    {
        Table[cellShape[cellCenter[i, j, w], w, cfg], {i, rows}, {j, cols}],
        Table[content[i, j], {i, rows}, {j, cols}],
        If[ showIndicesQ[cfg],
            {
                (* the column index, along the top *)
                Table[indexText[j, {j w, -0.02}, cfg, If[rows === 1, 1, 2]], {j, cols}],
                (* the row index, down the left, only when there is more than one row *)
                If[ rows > 1,
                    Table[indexText[i, {w/2 - 0.22, -i}, cfg, 1], {i, rows}],
                    Nothing
                ]
            },
            Nothing
        ]
    },
    FilterRules[opts, Options[Graphics]],
    FormatType -> StandardForm,
    (* The left margin has to hold the row indices, which sit outside the cells: without room
       reserved for them here they are simply clipped away. *)
    PlotRange -> {
        {w/2 - If[rows > 1, 0.55, 0.1], (cols + 0.5) w + 0.1},
        {-rows - 0.55, 0.25}
    },
    PlotRangePadding -> Scaled[0.02],
    ImagePadding -> 6,
    Background -> If[TrueQ @ Lookup[cfg, "Transparent", False], None, cfg["Theme"]["Background"]],
    ImageSize -> cfg["ImageSize"]
];

(* One block of indices in the plane, the content of each cell decided by the caller: an entry at
   the innermost block, and the picture of the indices that remain at every other. *)
renderBlock[dims_, 0, w_, content_, cfg_, opts_] :=
    gridGraphics[1, 1, w, content[{}, cellCenter[1, 1, w]] &, cfg, opts];

renderBlock[dims_, 1, w_, content_, cfg_, opts_] :=
    gridGraphics[1, dims[[1]], w, content[{#2}, cellCenter[#1, #2, w]] &, cfg, opts];

renderBlock[dims_, 2, w_, content_, cfg_, opts_] :=
    gridGraphics[dims[[1]], dims[[2]], w, content[{#1, #2}, cellCenter[#1, #2, w]] &, cfg, opts];

(* The innermost block holds entries, so its cells are sized to the widest of them. *)
renderNested[data_, {}, cfg_, opts_] := With[{
    leafCfg = Append[cfg, "FontSize" -> fontSizeFor[{data}]]
},
    renderBlock[{}, 0, cellWidthFor[{data}],
        Function[{idx, pos}, entryText[data, pos, leafCfg]], leafCfg, opts]
];

renderNested[data_, {k_}, cfg_, opts_] := With[{entries = entriesOf[data]},
    With[{leafCfg = Append[cfg, "FontSize" -> fontSizeFor[entries]]},
        renderBlock[
            dataDims[data], k, cellWidthFor[entries],
            Function[{idx, pos}, entryText[Extract[data, idx], pos, leafCfg]],
            leafCfg, opts
        ]
    ]
];

(* An outer block holds pictures, which are near enough square, so its cells are square too and
   the picture is inset into each. The inner picture is drawn on no background of its own: an
   opaque one would cover the cell it sits in, leaving only its rounded corners showing. *)
renderNested[data_, {k_, rest__}, cfg_, opts_] := With[{
    innerCfg = Join[cfg, <|"ImageSize" -> Automatic, "Transparent" -> True|>],
    outerCfg = Append[cfg, "CellFill" -> False]
},
    renderBlock[
        dataDims[data], k, 1,
        Function[{idx, pos},
            Inset[renderNested[Extract[data, idx], {rest}, innerCfg, opts], pos, Center, 0.86]
        ],
        outerCfg, opts
    ]
];


(* ::Section:: *)
(*The spatial family*)


(* Space holds three indices, so a block of one, two or three of them is a line, a plane or a
   lattice of cubes, all drawn the same way: the entry at index tuple (i, j, k) sits at the point
   {i, j, k}, an index the block does not have standing at one.

   Above three indices there is nowhere further to go, so the blocks are tiled instead: the inner
   block is drawn as a lattice, and the outer one repeats that lattice through space, shifted by
   its own extent and a gap. Everything ends up in a single picture rather than in pictures inset
   into pictures, which space does not allow. *)

$blockGap = 0.9;

(* The extent of a block's picture, in cell units, along the three axes. *)
extent3D[dims_, {}] := {1, 1, 1};
extent3D[dims_, {k_}] := PadRight[Take[dims, k], 3, 1];
extent3D[dims_, {k_, rest__}] := With[{
    inner = extent3D[Drop[dims, k], {rest}],
    outer = PadRight[Take[dims, k], 3, 1]
},
    outer (inner + $blockGap) - $blockGap
];

(* A cube with its entry at the centre. The faces are translucent so that the entries behind them
   can still be read, and carry no outline: an outline on every cube of a lattice reads as a mesh
   rather than as a set of cells. *)
cube3D[e_, pos_, cfg_] := {
    If[ cfg["Framed"],
        {
            FaceForm[Directive[cfg["Theme"]["Cube"], Opacity[0.22]]],
            EdgeForm[None],
            Cuboid[pos - 0.36, pos + 0.36]
        },
        {}
    ],
    entryText[e, pos, cfg]
};

(* The primitives of a block, shifted to its place in the tiling. *)
prims3D[data_, {}, offset_, cfg_] := cube3D[data, offset + {1, 1, 1}, cfg];

prims3D[data_, {k_}, offset_, cfg_] := With[{d = Take[dataDims[data], k]},
    Table[
        cube3D[Extract[data, idx], offset + PadRight[idx, 3, 1], cfg],
        {idx, Tuples[Range /@ d]}
    ]
];

prims3D[data_, {k_, rest__}, offset_, cfg_] := With[{
    outerDims = Take[dataDims[data], k],
    inner = extent3D[Drop[dataDims[data], k], {rest}]
},
    Table[
        prims3D[Extract[data, t], {rest},
                offset + (PadRight[t, 3, 1] - 1) (inner + $blockGap), cfg],
        {t, Tuples[Range /@ outerDims]}
    ]
];

(* A triad of arrows in the near corner, marking as many axes as the picture actually uses. Full
   axes would say more about the plot than about the array. *)
gnomonPrimitives[cfg_, axes_Integer] := With[{o = {0.06, 0.06, 0.06}, len = 0.55},
    {
        Thickness[0.006],
        Table[
            With[{dir = UnitVector[3, axis], label = {"i", "j", "k"}[[axis]]},
                {
                    indexColor[cfg, axis],
                    Arrow[Tube[{o, o + len dir}, 0.02]],
                    Text[
                        Style[label, indexColor[cfg, axis], FontSize -> $indexFontSize + 1,
                              FontSlant -> Italic],
                        o + (len + 0.23) dir
                    ]
                }
            ],
            {axis, axes}
        ]
    }
];

latticeGraphics[extent_, prims_, axes_, cfg_, opts_List] := Graphics3D[
    {
        prims,
        If[cfg["Gnomon"] && axes >= 1, gnomonPrimitives[cfg, axes], Nothing]
    },
    FilterRules[opts, Options[Graphics3D]],
    FormatType -> StandardForm,
    Boxed -> False,
    Lighting -> "Neutral",
    PlotRange -> Map[{0, # + 0.7} &, extent],
    PlotRangePadding -> Scaled[0.02],
    Background -> cfg["Theme"]["Background"],
    ImageSize -> cfg["ImageSize"]
];


(* ::Section:: *)
(*Public entry points*)


(* A sensible size: enough room per cell of the outer block, and more of it when the cells hold
   pictures rather than entries. *)
defaultImageSize[nesting_, dims_, entries_] := With[{
    outerCols = Which[
        nesting === {}, 1,
        First[nesting] === 1, dims[[1]],
        True, dims[[2]]
    ],
    perCell = If[Length[nesting] > 1, 100, 34],
    (* a leaf grid of wide entries needs a wider picture, in the same proportion as its cells *)
    widthFactor = If[Length[nesting] <= 1, cellWidthFor[entries], 1]
},
    Round @ Clip[perCell * outerCols * widthFactor, {90, 780}]
];

defaultImageSize3D[extent_] := Round @ Clip[54 * Max[extent], {120, 780}];


ArrayGraphics[a_, opts : OptionsPattern[]] := Module[{prep, cfg},
    prep = prepare[ArrayGraphics, a, 2, Flatten[{opts}]];
    If[prep === $Failed, Return[$Failed, Module]];

    cfg = prep["Config"];
    If[ cfg["ImageSize"] === Automatic,
        cfg = Append[cfg, "ImageSize" -> defaultImageSize[
            prep["Nesting"], prep["Dimensions"], entriesOf[prep["Entries"]]]]
    ];

    renderNested[prep["Entries"], prep["Nesting"], cfg, Flatten[{opts}]]
];


ArrayGraphics3D[a_, opts : OptionsPattern[]] := Module[{prep, cfg, nesting, extent, entries},
    prep = prepare[ArrayGraphics3D, a, 3, Flatten[{opts}]];
    If[prep === $Failed, Return[$Failed, Module]];

    cfg = prep["Config"];
    nesting = prep["Nesting"];
    entries = entriesOf[prep["Entries"]];
    extent = extent3D[prep["Dimensions"], nesting];

    cfg = Append[cfg, "FontSize" -> fontSizeFor[entries]];
    If[ cfg["ImageSize"] === Automatic,
        cfg = Append[cfg, "ImageSize" -> defaultImageSize3D[extent]]
    ];

    latticeGraphics[
        extent,
        prims3D[prep["Entries"], nesting, {0, 0, 0}, cfg],
        If[nesting === {}, 0, Max[nesting]],
        cfg, Flatten[{opts}]
    ]
];


HypermatrixGraphics[hm_Hypermatrix, opts : OptionsPattern[]] /; HypermatrixQ[hm] :=
    Row[Riffle[ArrayGraphics[#, opts] & /@ HypermatrixArrays[hm], Spacer[12]]];

HypermatrixGraphics[hm_, ___] := (Message[HypermatrixGraphics::hm, HoldForm[hm]]; $Failed);

HypermatrixGraphics3D[hm_Hypermatrix, opts : OptionsPattern[]] /; HypermatrixQ[hm] :=
    Row[Riffle[ArrayGraphics3D[#, opts] & /@ HypermatrixArrays[hm], Spacer[12]]];

HypermatrixGraphics3D[hm_, ___] := (Message[HypermatrixGraphics3D::hm, HoldForm[hm]]; $Failed);


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];
