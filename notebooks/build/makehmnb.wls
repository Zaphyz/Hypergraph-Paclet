#!/usr/bin/env wolframscript

(* Builds notebooks/HypermatrixBasics.nb. Each input cell is evaluated here in the kernel and its
   result attached as a StandardForm Output cell: NotebookEvaluate from a script formats results
   in OutputForm, which loses every graphic and every summary box. *)

$ProgressReporting = False;
(* The repository root, from where this script sits: <root>/notebooks/build/. *)
(* Forward slashes: $root is spliced into a string that is then parsed, and a Windows
   backslash there is read as a string escape. *)
$root = StringReplace[ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]],
                      "\\" -> "/"];
target = FileNameJoin[{$root, "notebooks", "HypermatrixBasics.nb"}];

tidy[s_String] := StringReplace[
    StringReplace[s, {"\r\n" -> " ", "\r" -> " ", "\n" -> " "}],
    RegularExpression[" {2,}"] -> " "
];

title[s_]    := Cell[tidy[s], "Title"];
subtitle[s_] := Cell[tidy[s], "Subtitle"];
section[s_]  := Cell[tidy[s], "Section"];
sub[s_]      := Cell[tidy[s], "Subsection"];
text[s_]     := Cell[tidy[s], "Text"];
note[s_]     := Cell[
    TextData[{StyleBox["Design note.  ", FontWeight -> "Bold", FontSlant -> "Italic"], tidy[s]}],
    "Text", CellFrame -> {{2, 0}, {0, 0}}, CellFrameColor -> GrayLevel[0.7],
    CellFrameMargins -> {{10, 0}, {6, 6}}, FontColor -> GrayLevel[0.25]
];

$log = {};
code[s_] := code[s, s];

(* An initialization cell: the front end evaluates these when the notebook is opened, so that the
   symbols are ready without anyone having to run anything. *)
initCode[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    AppendTo[$log, {"INIT: " <> StringTake[s, UpTo[50]], Head[res]}];
    {Cell[s, "Input", InitializationCell -> True]}
];
code[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    AppendTo[$log, {StringTake[StringReplace[s, "\n" -> " "], UpTo[56]], Head[res]}];
    If[ res === Null,
        {Cell[s, "Input"]},
        {Cell[s, "Input"], Cell[BoxData[ToBoxes[res, StandardForm]], "Output"]}
    ]
];

cells = Flatten[{

title["Arrays and Hypermatrices"],
subtitle["WolframInstitute`Hypergraphs` \[LongDash] the objects of the hypermatrix layer"],

text["This notebook shows the array object and the hypermatrix built out of them, together \
with the design choices behind them. Everything below is live output from src/."],

text["It goes on to the drawing of arrays, the special arrays, the contraction operation \
ArrayMultiply, the entry-by-entry operations that make arrays multilinear, and the search for \
equational identities between array operations."],

section["Loading the package"],

text["The cell below is an initialization cell, which the front end evaluates when this notebook \
is opened. By the time you read this the package is already loaded and every symbol is ready to \
use; the cell is shown for the record, and needs no running."],

initCode["PacletDirectoryLoad[ParentDirectory[NotebookDirectory[]]];\n\
Needs[\"WolframInstitute`Hypergraphs`\"]",
     "PacletDirectoryLoad[\"" <> $root <> "\"];Needs[\"WolframInstitute`Hypergraphs`\"]"],

note["PacletInfo.wl in the root of the repository describes the paclet: where its kernel sources \
are, what context they open, and which symbols belong to it. PacletDirectoryLoad points the \
kernel at the repository, and Needs then opens the context. Nothing has to be installed, and \
nothing is copied anywhere \[LongDash] the repository itself is what gets loaded, so an edit to \
src/ is picked up the next time the notebook is opened."],

note["Running PacletDirectoryAdd on the same directory, once, would register it permanently, and \
then any of the package's symbols would load it on first use in any notebook at all. That writes \
to the Wolfram user configuration rather than to this repository, so it is left as a choice \
rather than done here."],

section["1.  Arrays"],

text["An array in the Wolfram Language is a nesting of lists. ArrayObject wraps such a nesting, \
and that is all it is: one object for every array, whatever its entries are."],

code["ArrayObject[{{1, 2, 3}, {4, 5, 6}}]"],

text["It displays as a summary box rather than as its entries, which is what lets an array of any \
order and any size print in the same amount of space. Normal gives the entries back, and \
ArrayGraphics draws them:"],

code["Normal @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}]"],

note["There is one array object rather than one per kind of entry because nothing about the \
structure of an array depends on what sits in it. The order, the dimensions and the symmetry are \
the same questions whether the entries are integers, elements of a finite field, generated \
symbols or labels from a dictionary. An earlier design had a NumericalArray and a SymbolicArray \
as separate heads; the distinction turned out to buy nothing and cost a slot that was meaningless \
in each."],

sub["Order and dimensions"],

text["The dimensions of an array are the lengths of its indices, and its order is the number of \
indices. An array of order n is an n-array, so a component vector is a 1-array and a matrix is a \
2-array:"],

code["arrays = ArrayObject /@ {{1, 2, 3}, {{1, 2}, {3, 4}}, Array[#1 + #2 + #3 &, {2, 2, 2}]};\n\
{ArrayOrder /@ arrays, ArrayDimensions /@ arrays}"],

text["Dimensions works directly on an array object, as do Normal and the string properties:"],

code["m = ArrayObject[{{1, 2, 3}, {4, 5, 6}}];\n{Dimensions[m], m[\"Order\"], m[\"Properties\"]}"],

note["Dimensions and Normal are protected System` symbols, so they are extended with UpValues on \
the array head rather than redefined. That is the same treatment VertexList and EdgeList get on \
Hypergraph."],

sub["Regular and irregular"],

text["An array is regular when all of its indices range over the same length, and irregular when \
they do not:"],

code["{RegularArrayQ @ ArrayObject[{{1, 2}, {3, 4}}],\n \
 RegularArrayQ @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}]}"],

sub["Value domains"],

text["What an array is made of is not declared: it is asked. ArrayDomain reads the entries and \
gives the narrowest recognized domain containing every one of them:"],

code["ArrayDomain @ ArrayObject[#] & /@ {{True, False}, {1, 2}, {1/2, 3}, {1.5, 2}, {1 + I, 2}}"],

text["The elements of a finite field are recognized as that field:"],

code["ArrayDomain @ ArrayObject[{FiniteField[2, 1][0], FiniteField[2, 1][1]}]"],

text["Entries answering to none of them are simply expressions, and make a perfectly good array:"],

code["{ArrayDomain @ ArrayObject[{\"a\", \"b\"}], ArrayDomain @ ArrayObject[{x, y}],\n \
 ArrayDomain @ ArrayObject[{1, \"a\"}]}"],

note["A computed domain rather than a declared one is what lets the entries be anything. There is \
nothing to verify and nothing to reject, so an array of labels or of half-built expressions needs \
no special case: it is an array whose domain happens to be \"Expression\". \"Boolean\" means the \
entries are True and False; entries 0 and 1 are integers, and the two-element field is \
FiniteField[2, 1] \[LongDash] three different things that all get called boolean in casual use, \
kept apart here."],

section["2.  Generated entries"],

text["Entries do not have to be given. GenerateSymbolicArray builds them from a name and the \
indices, and gives back an array like any other:"],

code["GenerateSymbolicArray[a, {2, 3}]"],

code["Normal @ GenerateSymbolicArray[a, {2, 3}]"],

note["It is a function, not a kind of object. In that it sits beside ZeroArray, OneArray and \
IdentityArray, which build their entries from a shape as this one builds them from a name; what \
comes back from any of them is an ArrayObject. Naming it Generate\[Hyphen]something says that \
plainly, at the cost of being the only one of the four that does."],

text["Having been generated leaves no trace on the array, and its domain is read off its entries \
like anyone else's:"],

code["sa = GenerateSymbolicArray[a, {2, 3}];\n{Head[sa], ArrayDomain[sa], ArrayDomain[m]}"],

section["3.  Index symmetries"],

text["A regular array may declare an index symmetry: \"Rigid\" for no symmetry, which is the \
default, \"Cyclic\" for invariance under cyclic permutation of the indices, or \"Symmetric\" for \
invariance under all of them."],

sub["On a symbolic array the symmetry is honoured by construction"],

text["Index tuples that are equivalent under the symmetry name the same entry, so the generated \
array cannot fail to have the symmetry it claims:"],

code["Normal @ GenerateSymbolicArray[a, {3, 3}, \"Symmetric\"]"],

code["With[{e = Normal @ GenerateSymbolicArray[a, {3, 3}, \"Symmetric\"]}, e === Transpose[e]]"],

note["This is the same device that puts the vertices of a hyperedge into the least order of their \
symmetry orbit. Here the entry at a tuple of indices is the name applied to the least tuple in \
its orbit, which is why a symmetric or cyclic symbolic array needs no checking."],

text["The three symmetries really are three different things. Counting distinct entries of a \
3\[Times]3\[Times]3 array shows it:"],

code["Length @ DeleteDuplicates @ Flatten @ Normal @ GenerateSymbolicArray[a, {3, 3, 3}, #] & /@\n \
 {\"Rigid\", \"Cyclic\", \"Symmetric\"}"],

text["At order two, though, the cyclic group on the indices is the whole symmetric group, so \
there \"Cyclic\" and \"Symmetric\" coincide exactly:"],

code["Normal[GenerateSymbolicArray[a, {3, 3}, \"Cyclic\"]] === Normal[GenerateSymbolicArray[a, {3, 3}, \"Symmetric\"]]"],

sub["A declared symmetry is verified"],

text["An array given its entries has them already, so a declared symmetry is a claim about them, and \
it is checked. These entries are symmetric:"],

code["ArraySymmetry @ ArrayObject[{{1, 2}, {2, 1}}, \"Symmetric\"]"],

text["\[Ellipsis] and these are not, so the declaration is refused rather than the entries being \
quietly symmetrized:"],

code["Quiet @ ArrayObject[{{1, 2}, {3, 4}}, \"Symmetric\"]"],

text["The same entries are perfectly good as a rigid array:"],

code["ArraySymmetry @ ArrayObject[{{1, 2}, {3, 4}}]"],

note["Refusing is the honest answer: symmetrizing would silently return an array with different \
entries from the ones given. The symmetry is a declared property and ArraySymmetry reports what \
was declared, in the same way that an edge's symmetry type is declared rather than detected."],

text["Cyclic and symmetric are genuinely distinguished. The indicator of the cyclic orbit of \
(1, 2, 3) is invariant under rotating the indices but not under swapping two of them:"],

code["cyc = Array[If[MemberQ[{{1, 2, 3}, {2, 3, 1}, {3, 1, 2}}, {##}], 1, 0] &, {3, 3, 3}];\n\
{ArraySymmetry @ ArrayObject[cyc, \"Cyclic\"],\n \
 Quiet @ ArrayObject[cyc, \"Symmetric\"]}"],

text["A symmetry needs a regular array: permuting the indices of a 2\[Times]3 array would not \
give an array of the same shape, so the declaration is refused:"],

code["Quiet @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}, \"Symmetric\"]"],

section["4.  Hypermatrices"],

text["A hypermatrix is a list of arrays, held in a canonical order: lowest order first, then the \
shorter dimensions, then symmetry taking symmetric before cyclic before rigid. Here are six \
arrays given in no particular order:"],

code["hm = Hypermatrix[{\n \
  ArrayObject[Array[1 &, {2, 2, 2}], \"Symmetric\"],\n \
  ArrayObject[{{1, 2}, {3, 4}}],\n \
  ArrayObject[{1, 2}],\n \
  ArrayObject[{1, 2, 3}],\n \
  ArrayObject[{{1, 2}, {2, 1}}, \"Symmetric\"],\n \
  ArrayObject[{{1, 2}, {2, 1}}, \"Cyclic\"]}]"],

text["Reading off the three criteria in turn shows the order they came out in:"],

code["TableForm[\n \
 Transpose[{hm[\"Orders\"], hm[\"Dimensions\"], hm[\"Symmetries\"]}],\n \
 TableHeadings -> {Automatic, {\"order\", \"dimensions\", \"symmetry\"}}]"],

text["The two 1-arrays come first, shorter dimensions before longer. The three 2-arrays follow, \
all of dimensions 2\[Times]2, separated by symmetry. The 3-array comes last."],

text["Because the order is canonical, the same arrays given differently produce the identical \
expression:"],

code["Hypermatrix[{ArrayObject[{1, 2}], ArrayObject[{{1, 2}, {3, 4}}]}] ===\n \
Hypermatrix[{ArrayObject[{{1, 2}, {3, 4}}], ArrayObject[{1, 2}]}]"],

note["Two arrays alike in order, dimensions and symmetry are put in the Wolfram Language's own \
canonical order, so that a hypermatrix is a function of the set of arrays it was given and of \
nothing else. Without that last tiebreak the three 2\[Times]2 arrays above could come out in \
either of two orders depending on how they were typed."],

sub["What goes in"],

text["Raw entries are promoted to array objects, and arrays of any kind of entry sit together:"],

code["Hypermatrix[{{1, 2}, {1.5, 2.5}, GenerateSymbolicArray[a, {2, 2}]}][\"Domains\"]"],

text["Length gives the number of arrays, HypermatrixArrays gives them, and Normal gives their \
entries in the same order:"],

code["hm2 = Hypermatrix[{{{1, 2}, {3, 4}}, {1, 2}}];\n{Length[hm2], Normal[hm2]}"],

text["A sequence of arrays is accepted as well as a list:"],

code["Hypermatrix[ArrayObject[{1, 2}], ArrayObject[{1, 2, 3}]] ===\n \
Hypermatrix[{ArrayObject[{1, 2}], ArrayObject[{1, 2, 3}]}]"],

text["Anything that is not an array is refused:"],

code["Quiet @ Hypermatrix[{\"nope\"}]"],

section["5.  Drawing arrays"],

text["There are two families of pictures, and an array of any order may be drawn in either. The \
flat family gives plane graphics throughout; the spatial family gives cubes arranged in space. \
Which to use is a matter of what you want to see, not of what the array is."],

sub["The flat family: ArrayGraphics"],

text["At order one a row of cells, at order two a grid:"],

code["{ArrayGraphics[{1, 2, 3, 4}], ArrayGraphics[{{1, 2, 3}, {4, 5, 6}}]}"],

text["The row and column indices are written outside the cells, so that a picture can be read off \
against the entry it names:"],

code["ArrayGraphics[GenerateSymbolicArray[b, {2, 3}]]"],

note["The cells of a grid are sized to the widest entry rather than to a fixed width. A symbolic \
array's entries read b[1, 2] and would otherwise overflow one another. They are drawn in \
StandardForm, not the TraditionalForm that Graphics uses by default, which would print b[1, 2] as \
b(1, 2) \[LongDash] something the array does not contain and that could not be typed back in."],

text["Only those two orders have a flat picture of their own. Everything above them is drawn as a \
nesting: a 3-array is a row of matrices, the first index choosing the matrix, which is the \
familiar reading."],

code["ArrayGraphics[Array[#1 + #2 + #3 &, {2, 2, 2}]]"],

text["A 4-array is a grid of grids:"],

code["ArrayGraphics[Array[#1 + #2 + #3 + #4 &, {2, 2, 2, 2}]]"],

text["Which indices go outside and which inside is a choice, not a fact about the array. The same \
3-array read the other way round, as a matrix of rows:"],

code["ArrayGraphics[Array[#1 + #2 + #3 &, {2, 2, 2}], \"Nesting\" -> {2, 1}]"],

note["\"Nesting\" is a list of block sizes summing to the order of the array. In the flat family \
each block is one or two, since a plane picture holds no more; the first block is the outer \
picture and each block after it is drawn inside the cells of the one before. A block of three is \
refused rather than quietly turned into a lattice: this family draws plane graphics and nothing \
else."],

code["Quiet @ ArrayGraphics[Array[1 &, {2, 2, 2}], \"Nesting\" -> {3}]"],

text["The default splitting takes blocks of two with any remainder off the front, which is what \
ArrayNesting reports:"],

code["Table[ArrayNesting[n], {n, 0, 8}]"],

sub["The spatial family: ArrayGraphics3D"],

text["The same arrays, as cubes arranged in space. At order one a line of cubes, at order two a \
plane of them:"],

code["{ArrayGraphics3D[{1, 2, 3, 4}], ArrayGraphics3D[{{1, 2, 3}, {4, 5, 6}}]}"],

text["At order three, the full lattice. The entry at (i, j, k) sits at the point {i, j, k}, and a \
gnomon in the near corner says which way the three indices run:"],

code["ArrayGraphics3D[Array[#1 + #2 + #3 &, {3, 3, 3}]]"],


note["The cubes have translucent faces and no outline. An outline on every cube of a lattice reads \
as a mesh rather than as a set of cells, and the entries behind the nearer faces can be read \
through them as it is. The reference implementation this package grew out of drew only this one \
order, and called such a picture a cubix; the name has not been kept, but the look of it has."],

text["Colouring the indices makes the correspondence explicit, each axis taking a colour of its \
own:"],

code["ArrayGraphics3D[Array[#1 #2 #3 &, {2, 2, 2}], \"Indices\" -> \"Colored\"]"],

sub["Above order three: tiling rather than nesting"],

text["Space runs out at three indices just as the plane runs out at two. But where the flat family \
insets a picture into each cell, the spatial family has nowhere to inset anything: a Graphics3D \
cannot hold another Graphics3D. So the blocks are tiled instead. A 4-array is a line of lattices, \
shifted along by the extent of one and a gap:"],

code["ArrayGraphics3D[Array[#1 + #2 + #3 + #4 &, {2, 2, 2, 2}]]"],

text["A 5-array is a plane of them:"],

code["ArrayGraphics3D[Array[Mod[#1 + #2 + #3 + #4 + #5, 7] &, {2, 2, 2, 2, 2}]]"],

note["Everything ends up in one picture, which is the point: a tiled lattice can be turned and \
looked at as a single object, where a grid of separate 3D panes could not be. The spatial default \
takes blocks of three with any remainder off the front \[LongDash] ArrayNesting[n, 3] \[LongDash] \
and a block of four is refused, space holding only three indices at a time."],

code["Table[ArrayNesting[n, 3], {n, 0, 8}]"],

text["The splitting may be given here too. The same 4-array with the single index innermost \
instead, so that each cube of a lattice becomes a short line of cubes:"],

code["ArrayGraphics3D[Array[#1 + #2 + #3 + #4 &, {2, 2, 2, 2}], \"Nesting\" -> {3, 1}]"],

sub["Options, and hypermatrices"],

text["The options are shared between the families. The cells may be left undrawn, the indices \
dropped, and the whole thing put on a dark ground:"],

code["{ArrayGraphics[{{1, 2}, {3, 4}}, \"Framed\" -> False],\n \
 ArrayGraphics[{{1, 2}, {3, 4}}, \"Indices\" -> None],\n \
 ArrayGraphics[{{1, 2}, {3, 4}}, \"PlotTheme\" -> \"Dark\"]}"],

code["ArrayGraphics3D[Array[#1 + #2 + #3 &, {2, 2, 2}], \"PlotTheme\" -> \"Dark\"]"],

note["An array of more than a few hundred entries is refused rather than drawn, with \
ArrayGraphics::big. A picture of a thousand cells is not a picture of anything; \"MaxEntries\" raises \
the limit for anyone who disagrees."],

code["Quiet @ ArrayGraphics[Array[1 &, {12, 12, 12}]]"],

text["A hypermatrix draws each of its arrays, in its own canonical order, in either family:"],

code["HypermatrixGraphics @ Hypermatrix[{{1, 2, 3}, {{1, 2}, {3, 4}}, Array[1 &, {2, 2, 2}]}]"],

code["HypermatrixGraphics3D @ Hypermatrix[{{1, 2, 3}, {{1, 2}, {3, 4}}, Array[1 &, {2, 2, 2}]}]"],

section["6.  Special arrays"],

text["The constant arrays and the generalized Kronecker deltas are available in any order, \
dimension and value domain. Each declares the strongest symmetry that actually holds of it, which \
the ArrayObject constructor then verifies."],

code["{Normal @ ZeroArray[2, 3], Normal @ OneArray[2, 3]}"],

text["The identity array is one where all of its indices agree. At order two it is the identity \
matrix; at order three it is one only on the main diagonal:"],

code["{Normal @ IdentityArray[2, 3], Normal @ IdentityArray[3, 2]}"],

text["A partial identity is one where the indices at chosen positions agree, whatever the others \
do. These are the three partial identities of a 3-array, one per pair of indices:"],

code["Normal @ PartialIdentityArray[3, 2, #] & /@ {{1, 2}, {2, 3}, {1, 3}}"],

note["A constant array and a full identity are invariant under every permutation of their \
indices, so they are declared symmetric. A partial identity on only some positions is not, so it \
is declared rigid. Naming every position gives back the full identity exactly."],

code["{ArraySymmetry @ ZeroArray[3, 2], ArraySymmetry @ IdentityArray[3, 2],\n \
 ArraySymmetry @ PartialIdentityArray[3, 2, {1, 2}],\n \
 PartialIdentityArray[3, 2, {1, 2, 3}] === IdentityArray[3, 2]}"],

sub["Seeing them"],

text["These arrays are the ones whose shape is worth looking at, since what makes each of them \
special is where its entries sit rather than what they are. Both families of pictures are useful \
here, and at different orders."],

text["A constant array says the same thing at every position, whatever its dimensions. Order one, \
order two, and a rectangular one that is not regular at all:"],

code["{ArrayGraphics @ OneArray[{4}], ArrayGraphics @ ZeroArray[2, 3],\n \
 ArrayGraphics @ OneArray[{2, 5}]}"],

text["The identity matrix, and beside it the same thing one order up, where the ones sit on the \
main diagonal of the cube:"],

code["{ArrayGraphics @ IdentityArray[2, 4], ArrayGraphics3D @ IdentityArray[3, 3]}"],

note["At order three the flat picture and the spatial one say different things about the same \
array. Flat, the identity is a row of matrices, each with a single one in it, marching down the \
diagonal as the first index advances. In space it is the diagonal itself, which is the thing the \
array is actually named after."],

code["ArrayGraphics @ IdentityArray[3, 3]"],

text["A partial identity is one where the indices at chosen positions agree, whatever the others \
do. Each of the three is a plane through the cube, and which plane it is depends on the pair:"],

code["ArrayGraphics3D @ PartialIdentityArray[3, 3, #] & /@ {{1, 2}, {2, 3}, {1, 3}}"],

note["The picture is the argument for the name. Where the full identity picks out a line through \
the cube, a partial identity picks out a plane \[LongDash] and the three of them, one per pair of \
indices, are three planes meeting along that line. Naming every position leaves only the line, \
which is the full identity again."],

text["Dimension by dimension, the identity at order three grows as a diagonal should:"],

code["ArrayGraphics3D @ IdentityArray[3, #] & /@ {2, 3, 4}"],

text["The domains show up too. Over the two-element field the identity matrix has the same shape, \
with entries of that field rather than integers:"],

code["ArrayGraphics @ IdentityArray[2, 3, FiniteField[2, 1]]"],

section["7.  Contraction"],

text["ArrayMultiply contracts a list of arrays according to an index specification, in the manner \
of Einstein summation. Indices named in the output survive; the rest are summed over."],

code["a = {{1, 2}, {3, 4}}; b = {{0, 1}, {1, 0}};\nArrayMultiply[{a, b}, \"ij,jk->ik\"]"],

text["The specification may equally be given as a rule between lists of index labels:"],

code["{Normal @ ArrayMultiply[{a, b}, \"ij,jk->ik\"],\n \
 Normal @ ArrayMultiply[{a, b}, {{1, 2}, {2, 3}} -> {1, 3}]}"],

text["Repeating an index within one array takes a diagonal, and leaving it out of the output sums \
it, so a trace and a transpose are contractions too:"],

code["{Normal @ ArrayMultiply[{a}, \"ii->i\"], Normal @ ArrayMultiply[{a}, \"ii->\"],\n \
 Normal @ ArrayMultiply[{a}, \"ij->ji\"]}"],

note["An index may occur in any number of the arrays at once, and may occur in the output as well \
as in the inputs. That is what lets a single specification express the three-way contractions of \
the ternary products, which no sequence of pairwise contractions would give."],

text["The Bhattacharya-Mesner product of three 3-arrays joins all three on one index:"],

code["x = Array[#1 + #2 + #3 &, {2, 2, 2}];\nNormal @ ArrayMultiply[{x, x, x}, \"ijp,ipk,pjk->ijk\"]"],

note["The result comes back as an array object, whatever the entries turned out to be. \
Contracting every index away leaves a single entry, which is the array of order zero, and Normal \
of that gives the scalar itself. There is nothing here to decide: an array of arbitrary \
expressions is an array like any other, which is the whole point of there being one array \
object."],

text["The other side of the same coin: an array of order zero is named in a specification by an \
empty group of index labels, which is how a scalar enters a contraction."],

code["Normal @ ArrayMultiply[{3, a}, \",ij->ij\"]"],

sub["Numerical and symbolic arrays"],

text["A symbolic array is a list of lists of terms with explicit indices, and a contraction of \
symbolic arrays simply builds the corresponding expressions in a list of lists. The symbols behave \
as Wolfram Language symbols always do, which is to say as complex variables:"],

code["sa = Normal @ GenerateSymbolicArray[p, {2, 2}];\nsb = Normal @ GenerateSymbolicArray[q, {2, 2}];\n{sa, sb}"],

code["ArrayMultiply[{sa, sb}, \"ij,jk->ik\"]"],

text["It agrees with the built-in matrix product, as it should:"],

code["ArrayMultiply[{sa, sb}, \"ij,jk->ik\"] === sa . sb"],

note["Arrays with numeric entries and arrays with generated ones may be contracted together. That \
was once refused, on the grounds that the result would be neither kind of object; with one array \
object the objection has nothing left to stand on, and the entries of the result are simply the \
expressions the contraction builds."],

code["Normal @ ArrayMultiply[{{{1, 2}, {3, 4}}, sb}, \"ij,jk->ik\"]"],

sub["Contracting every index: the trace"],

text["An operation need not return an array. Contracting a single matrix onto no indices at all \
sums its diagonal, which is the trace, and works on numerical and symbolic entries alike:"],

code["trace[u_List] := Normal @ ArrayMultiply[{u}, \"pp->\"];\n\
{trace[{{1, 2}, {3, 4}}], trace[sa]}"],

code["{trace[{{1, 2}, {3, 4}}] === Tr[{{1, 2}, {3, 4}}], trace[sa] === Tr[sa]}"],

note["The Normal is doing something here. A contraction onto no indices gives back the 0-array \
rather than a bare number, and Normal takes its single entry. It does the same whatever the \
entries were, so the one definition covers every case."],

section["8.  Adding and multiplying entry by entry"],

text["Contraction is one of the two things one does with arrays. The other is to combine them \
entry by entry, which is what makes arrays of a given shape a vector space rather than merely a \
collection of tables. ArrayAdd and ArrayTimes do that:"],

code["u = {{1, 2}, {3, 4}}; v = {{5, 6}, {7, 8}};\n{Normal @ ArrayAdd[u, v], Normal @ ArrayTimes[u, v]}"],

text["Plus and Times on array objects are those same operations, so arrays may be combined in \
ordinary arithmetic notation:"],

code["{Normal[ArrayObject[u] + ArrayObject[v]],\n \
 Normal[ArrayObject[u] - ArrayObject[v]],\n \
 Normal[ArrayObject[u] ArrayObject[v]]}"],

text["An array of order zero has no shape of its own, so it combines with an array of any \
dimensions, reaching every entry:"],

code["{Normal @ ArrayTimes[2, u], Normal @ ArrayAdd[1, u]}"],

note["This is why scalar multiplication needs no name of its own. A scalar is the 0-array; \
multiplying an array by one is the same entry-by-entry multiplication as multiplying it by another \
array of its own shape, with the single entry spread over the whole shape rather than matched \
index for index. One operation, one rule, and 2 a is ArrayTimes[ArrayObject[2], a] written \
shorter."],

code["ArrayTimes[2, u] === ArrayTimes[ArrayObject[2], u]"],

text["Arrays of different dimensions cannot be combined this way, and are refused rather than \
padded or truncated:"],

code["Quiet @ ArrayAdd[{1, 2}, {1, 2, 3}]"],

note["Entries of any kind may be combined, a number meeting an expression having the obvious \
meaning, and the result is an array object like anything else here. ArrayMultiply once refused \
the same mixture and no longer does, for the same reason: with one array object there is nothing \
ambiguous about what comes back."],

code["{2 GenerateSymbolicArray[c, {2}], ArrayObject[{1, 2}] + GenerateSymbolicArray[c, {2}]}"],

text["A symmetry that every argument declares is carried over to the result, since an \
entry-by-entry operation treats each index tuple separately. Where the arguments disagree nothing \
is claimed:"],

code["{ArraySymmetry @ ArrayAdd[ArrayObject[{{1, 2}, {2, 3}}, \"Symmetric\"],\n \
                          ArrayObject[{{0, 1}, {1, 0}}, \"Symmetric\"]],\n \
 ArraySymmetry @ ArrayAdd[ArrayObject[{{1, 2}, {2, 3}}, \"Symmetric\"],\n \
                          ArrayObject[u]]}"],

note["The arguments are given directly rather than in a list, unlike ArrayMultiply, which takes a \
list of arrays and a specification alongside it. A list of arrays is itself an array, so \
ArrayAdd[{v1, v2}] would be ambiguous: it could be the sum of two vectors or the matrix they \
form. Given directly there is nothing to guess."],

section["9.  Searching for equational identities"],

text["FindArrayEquations takes a list of array operations and a list of generic arguments, forms \
every way of nesting the operations over those arguments, and returns the equations that hold \
between them. Operations are ordinary functions of arrays, so they are easily built out of \
ArrayMultiply:"],

code["dot[u_List, w_List] := Normal @ ArrayMultiply[{u, w}, \"ij,jk->ik\"];\n\
Fish[u_List, w_List, t_List] := Normal @ ArrayMultiply[{u, w, t}, \"ijp,qrp,qrk->ijk\"];\n\
BM[u_List, w_List, t_List] := Normal @ ArrayMultiply[{u, w, t}, \"ijp,ipk,pjk->ijk\"];"],

text["Ordinary matrix multiplication on three generic arguments finds associativity, and nothing \
else:"],

code["FindArrayEquations[{dot}, {a1, a2, a3}, \"Order\" -> 2, \"Dimension\" -> 3]"],

text["The Fish product on five generic arguments finds a twisted form of ternary associativity. \
One of the two equations below is plain associativity in the outer arguments; the other reverses \
the inner three:"],

code["FindArrayEquations[{Fish}, {a1, a2, a3, a4, a5}]"],

text["The Bhattacharya-Mesner product, on the same five arguments, satisfies nothing at all:"],

code["FindArrayEquations[{BM}, {a1, a2, a3, a4, a5}]"],

note["The search is in two stages, and that is what makes it quick. Every term is first evaluated \
on random integer arrays and only pairs that agree there are carried forward. Two terms equal as \
identities agree on any arrays, so nothing correct is ever thrown away; the numerical stage only \
proposes, and the symbolic stage disposes. The surviving candidates are then checked on symbolic \
arrays by expanding the difference of the two sides, which is exact for polynomials and far \
quicker than a general simplification."],

text["Whether the arguments may be permuted is an option. With a fixed order only the nesting \
varies, which for matrix multiplication makes no difference, since associativity does not move \
its arguments:"],

code["FindArrayEquations[{dot}, {a1, a2, a3}, \"Order\" -> 2, \"Dimension\" -> 3,\n \
                   \"Permute\" -> False]"],

note["An identity verified on symbolic arrays of a given dimension is proved at that dimension, \
not at every dimension. The default is 2, which is small and quick; raising \"Dimension\" checks \
more widely. The Fish equations above come out the same at dimension 3, which is reassuring \
without being a proof in general."],
sub["More than one operation at a time"],

text["Operations of different arities may be searched together. A transpose is unary and a matrix \
product binary; on two generic arguments they give the law that transposing a product reverses it, \
and the law that transposing twice does nothing:"],

code["perp[u_List] := Normal @ ArrayMultiply[{u}, \"ij->ji\"];\n\
FindArrayEquations[{perp, dot}, {a1, a2}]"],

note["An operation of arity one is the exception to the rule that a term uses exactly the given \
arguments. It takes a term to a term of the same shape, so it may be applied over and over at any \
point without consuming an argument, and every term is offered wrapped in up to \"UnaryDepth\" \
nested unary operations, two by default. That is what brings the involutivity within reach: it \
needs the transpose twice over on one argument, where the anti-homomorphism law uses it once on \
each side."],

text["Setting the depth to one puts the involutivity back out of reach \[LongDash] and with it \
gone, the remaining law is reported in each of its forms instead of once, since without \
perp[perp[a]] == a there is nothing to rewrite one form into another:"],

code["FindArrayEquations[{perp, dot}, {a1, a2}, \"UnaryDepth\" -> 1]"],

text["The anti-homomorphism law moves its arguments, so holding them in the order given leaves \
only the involutivity, which does not:"],

code["FindArrayEquations[{perp, dot}, {a1, a2}, \"Permute\" -> False]"],

note["Freely nested unary operations bring a flood with them. Every consequence of those two laws \
is an equation between terms of the search, and each would be reported in its own right: a hundred \
and more of them here, all saying the same two things over again. So an equation that follows from \
the others is left out. Each equation is read as a schema, its arguments standing for arbitrary \
terms, and turned into a rewrite rule that fires only when it makes a term strictly smaller; any \
number of such rules is then a terminating rewriting system, and an equation whose two sides \
rewrite to a common form is a consequence of the ones already kept. \"Reduce\" -> False reports \
everything found."],

code["Length @ FindArrayEquations[{perp, dot}, {a1, a2}, \"Reduce\" -> False]"],

note["What survives is a basis, not a canonical set. Which of several equivalent forms of a law is \
kept depends on the order the equations are considered in \[LongDash] the anti-homomorphism law \
above is reported with the transposes gathered on whichever side the ordering favours \[LongDash] \
and the reduction is by rewriting alone, so a consequence that rewriting cannot reach is still \
reported. A genuinely minimal set of axioms is the business of Knuth-Bendix completion, which this \
is not."],

text["A trace and a matrix product give the cyclic property of the trace:"],

code["FindArrayEquations[{trace, dot}, {a1, a2}]"],

note["The trace returns a scalar, so a nesting that feeds it back into a product cannot evaluate. \
Such a term is dropped rather than compared, which is what lets an operation of that shape sit at \
the top of a term without having to be excluded by hand."],

note["Neither of those searches was told the shape of its test arrays. With \"Order\" -> Automatic, \
the default, the smallest order on which every operation actually evaluates is used, which is 2 \
for matrix operations and 3 for the ternary array products. Operations are shape-specific enough \
that this is a search for the shape they are defined on rather than a guess."],

text["Forcing a shape the operations cannot take says so, rather than coming back empty:"],

code["Quiet @ FindArrayEquations[{perp, dot}, {a1, a2}, \"Order\" -> 4]"],

note["An empty result and a result that could not be computed are different answers, and \
conflating them would be the worst kind of quiet failure: it would read as a proof that no \
identity exists. Whenever no nesting evaluates, FindArrayEquations::eval is issued and $Failed \
returned."],

section["10.  The adjacency hypermatrix"],

text["The two halves of the package meet here. A hypergraph is a multiset of edges, each with an \
arity and a symmetry type; an array has an order and a symmetry. The two vocabularies line up one \
for one, and the passage from a hypergraph to a hypermatrix is what that correspondence looks like \
when it is written down."],

code["hAdj = Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"], Edge[{3, 4}, \"Directed\"],\n \
                  Edge[{1, 2}, \"Directed\"]}]"],

code["hmAdj = AdjacencyHypermatrix[hAdj];\n\
{hmAdj[\"Orders\"], hmAdj[\"Dimensions\"], hmAdj[\"Symmetries\"]}"],

note["An edge of arity k gives an array of order k, and an edge symmetry gives an array symmetry: \
directed to rigid, cyclic to cyclic, unordered to symmetric. An array holds one symmetry and one \
order, so the edges are gathered by arity and symmetry type together, and each group becomes one \
array. A Hypermatrix then sorts those arrays by order, then dimensions, then symmetry \[LongDash] \
which is exactly what the grouping was keyed on, so the result comes out sorted by the very thing \
that distinguishes its parts."],

text["The two directed edges land where they are, in a rigid matrix whose indices run over the \
vertices in the order VertexList gives them:"],

code["MatrixForm @ Normal @ First @ HypermatrixArrays[hmAdj]"],

text["The unordered edge fills the whole orbit of its three vertices, all six permutations, which \
is what makes that array symmetric:"],

code["Position[Normal @ Last @ HypermatrixArrays[hmAdj], 1]"],

text["A cyclic edge fills the three rotations, and a directed one the single tuple:"],

code["{Position[Normal @ First @ HypermatrixArrays @\n \
    AdjacencyHypermatrix @ Hypergraph[{Edge[{1, 2, 3}, \"Cyclic\"]}], 1],\n \
 Position[Normal @ First @ HypermatrixArrays @\n \
    AdjacencyHypermatrix @ Hypergraph[{Edge[{1, 2, 3}, \"Directed\"]}], 1]}"],

note["The entry is looked up against the canonical index of its tuple \[LongDash] the least one in \
its orbit under the symmetry, the same arrayCanonicalIndex by which a symbolic array decides which \
of its entries are the same entry. Every tuple of an orbit therefore reads the same count, and the \
array carries its declared symmetry by construction rather than by luck. That is the whole trick, \
and it is why the two definitions had to agree in the first place."],

text["Edges of one arity but different symmetry types cannot share an array, so they do not:"],

code["AdjacencyHypermatrix[Hypergraph[{Edge[{1, 2}, \"Directed\"], Edge[{1, 2}, \"Unordered\"],\n \
                                 Edge[{1, 2}, \"Cyclic\"]}]][\"Symmetries\"]"],

text["A hypergraph is a multiset, so a repeated edge counts twice:"],

code["Normal @ First @ HypermatrixArrays @\n \
 AdjacencyHypermatrix @ Hypergraph[{Edge[{1, 2}], Edge[{1, 2}], Edge[{2, 1}]}]"],

note["Nothing is lost in the passage. Reading every canonical entry back out names exactly the \
edges that went in, with their multiplicities, which the test file checks on three hundred random \
hypergraphs. Labels are the one thing that does not survive: this is the adjacency of the \
skeleton, and a labelled hypergraph gives the same hypermatrix as HypergraphSkeleton of it."],

text["Being ordinary arrays, they draw like ordinary arrays:"],

code["HypermatrixGraphics @ AdjacencyHypermatrix @\n \
 Hypergraph[{Edge[{1, 2}, \"Directed\"], Edge[{2, 3}, \"Directed\"], Edge[{1, 3}, \"Directed\"]}]"],

text["And an unordered edge of arity three, seen as the lattice it is:"],

code["ArrayGraphics3D @ First @ HypermatrixArrays @\n \
 AdjacencyHypermatrix @ Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"]}]"],

sub["Multiplicities"],

text["A hypergraph is a multiset of edges, and identical copies of an edge all want the same \
entry. They get it, and the entry counts them:"],

code["Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @\n \
 Hypergraph[{Edge[{1, 2}], Edge[{1, 2}], Edge[{2, 1}]}]"],

text["That holds at every symmetry type, over the whole orbit each time. Three copies of an \
unordered edge put a 3 at each of its six permutations; two copies of a cyclic one put a 2 at each \
of its three rotations:"],

code["{Union @ Cases[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @\n \
     Hypergraph[Table[Edge[{1, 2, 3}, \"Unordered\"], 3]], _?Positive, Infinity],\n \
 Union @ Cases[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @\n \
     Hypergraph[Table[Edge[{1, 2, 3}, \"Cyclic\"], 2]], _?Positive, Infinity]}"],

note["Edges are stored with their vertices in the canonical order of their symmetry type, so two \
spellings of one unordered edge are one edge written twice, not two edges. The entry is 2, as it \
should be."],

code["Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @\n \
 Hypergraph[{Edge[{1, 2}, \"Unordered\"], Edge[{2, 1}, \"Unordered\"]}]"],

sub["Labels as entries"],

text["If the edges carry labels, those labels can be the entries instead of the counts. With \
\"Labeled\" -> True the entry at a position is the label of the edge that sits there, and 0 where \
no edge does:"],

code["hLab = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"],\n \
                  Edge[{1, 2, 3}, \"Unordered\", \"c\"]}];\n\
Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[hLab, \"Labeled\" -> True]"],

text["A label fills the whole orbit of its edge, exactly as a count does, so the arrays carry \
their symmetries just the same. The unordered edge labelled c appears at all six permutations of \
its vertices:"],

code["{Length @ Position[Normal @ Last @ HypermatrixArrays @\n \
     AdjacencyHypermatrix[hLab, \"Labeled\" -> True], \"c\"],\n \
 ArraySymmetry @ Last @ HypermatrixArrays @ AdjacencyHypermatrix[hLab, \"Labeled\" -> True]}"],

note["The value domain of such an array is \"Label\". It is the one domain that is never inferred: \
its test admits anything at all, so were it in the list that inference walks, every array whose \
entries fitted nothing else would quietly become an array of labels. It has to be asked for, which \
is what \"Labeled\" -> True does."],

code["{AdjacencyHypermatrix[hLab][\"Domains\"],\n \
 AdjacencyHypermatrix[hLab, \"Labeled\" -> True][\"Domains\"]}"],

text["Counting remains what happens by default, so nothing changes for a hypergraph whose labels \
are beside the point:"],

code["AdjacencyHypermatrix[hLab] === AdjacencyHypermatrix[hLab, \"Labeled\" -> False]"],

sub["Why labels need a simple hypergraph"],

text["An entry can hold a count of any size, but it cannot hold two labels. So labels can only be \
the entries when no two edges want the same one \[LongDash] which is exactly what it means for the \
hypergraph to be simple. SimpleHypergraphQ tests for it:"],

code["{SimpleHypergraphQ[hLab],\n \
 SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"],\n \
                                 Edge[{1, 2}, \"Directed\", \"b\"]}]}"],

text["The second is two edges in one place. Counting them is no trouble; labelling them is \
refused:"],

code["Quiet @ AdjacencyHypermatrix[\n \
 Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{1, 2}, \"Directed\", \"b\"]}],\n \
 \"Labeled\" -> True]"],

code["Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @\n \
 Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{1, 2}, \"Directed\", \"b\"]}]"],

note["Simplicity is tested on the skeleton, which is the right place: two edges differing only by \
a label are the same edge of the same skeleton, and it is their sharing a position that matters, \
not their having different labels. A vertex repeated within one edge, by contrast, is allowed \
\[LongDash] that is a property of one edge rather than a collision between two, and it obstructs \
nothing."],

code["{SimpleHypergraphQ @ Hypergraph[{Edge[{1, 1, 2}, \"Unordered\"]}],\n \
 Union @ Flatten @ Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[\n \
    Hypergraph[{Edge[{1, 1, 2}, \"Unordered\", \"r\"]}], \"Labeled\" -> True]}"],

text["Edges of one arity but different symmetry types go to different arrays, so they never \
collide and a hypergraph holding both is simple:"],

code["AdjacencyHypermatrix[\n \
 Hypergraph[{Edge[{1, 2}, \"Directed\", \"d\"], Edge[{1, 2}, \"Unordered\", \"u\"]}],\n \
 \"Labeled\" -> True][\"Symmetries\"]"],

note["Vertex labels have no slot here. An entry of an adjacency array answers to an edge, so edge \
labels are what can go in it; the vertices are the indices rather than the entries, and their \
labels stay in VertexList. An edge carrying no label of its own gives None, which is not the same \
as the 0 that means no edge at all."],

code["Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[\n \
 Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 1}, \"Directed\"]}], \"Labeled\" -> True]"],

sub["Which order do the indices run in?"],

text["The vertices of a hypergraph are a set. They have no order of their own, and yet the indices \
of an array are numbered one to n. Something must be choosing, and it is worth being clear about \
what."],

text["The choice is made by the Hypergraph constructor, not by AdjacencyHypermatrix. A hypergraph \
is stored with its vertices sorted, so the order is a property of the object rather than of how it \
was typed. Giving the same edges in a different order changes nothing:"],

code["hOrdA = Hypergraph[{Edge[{3, 1}], Edge[{2, 3}]}];\n\
hOrdB = Hypergraph[{Edge[{2, 3}], Edge[{3, 1}]}];\n\
{VertexID /@ VertexList[hOrdA], hOrdA === hOrdB,\n \
 AdjacencyHypermatrix[hOrdA] === AdjacencyHypermatrix[hOrdB]}"],

text["So far so good. But the sort is on the vertex identifiers, and identifiers are whatever the \
user chose them to be. Here are two isomorphic hypergraphs \[LongDash] the same path on three \
vertices \[LongDash] whose vertices are named differently:"],

code["hNum = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}];\n\
hStr = Hypergraph[{Edge[{\"z\", \"a\"}], Edge[{\"a\", \"m\"}]}];\n\
{IsomorphicHypergraphQ[hNum, hStr],\n \
 VertexID /@ VertexList[hNum], VertexID /@ VertexList[hStr]}"],

text["Sorting puts a before m before z, so the vertex called z \[LongDash] the start of the path \
\[LongDash] ends up third. The two adjacency matrices are not equal:"],

code["mNum = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[hNum];\n\
mStr = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[hStr];\n\
{MatrixForm[mNum], MatrixForm[mStr], mNum === mStr}"],

note["This is the honest answer to the question: there is a choice, and renaming the vertices can \
change the hypermatrix. What the sort buys is not that the choice goes away but that it is made \
the same way every time. AdjacencyHypermatrix is a function of the hypergraph it is given, with \
nothing arbitrary left in it; what it is not is a function of the isomorphism class."],

text["The two matrices are of course related, by the permutation the renaming induces on the \
indices. Old vertex 1 is called z, which sorts third; 2 is called a, which sorts first; 3 is \
called m, which sorts second:"],

code["perm = {3, 1, 2};\n\
mNum[[InversePermutation[perm], InversePermutation[perm]]] === mStr"],

text["The same permutation, applied in every index slot at once, relates the higher-order arrays \
too. Simultaneous, because one vertex set is being renumbered, not three:"],

code["gNum = Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"], Edge[{1, 1, 2}, \"Unordered\"]}];\n\
gStr = Hypergraph[{Edge[{\"z\", \"a\", \"m\"}, \"Unordered\"], Edge[{\"z\", \"z\", \"a\"}, \"Unordered\"]}];\n\
aNum = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[gNum];\n\
aStr = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[gStr];\n\
{aNum === aStr,
 With[{q = InversePermutation[perm]}, aNum[[q, q, q]] === aStr]}"],

note["Mathematically this is the usual state of affairs: an adjacency array is attached to a \
hypergraph together with a numbering of its vertices, and as an invariant of the isomorphism class \
it is defined only up to simultaneous permutation of its indices. Nothing here is a defect of the \
implementation \[LongDash] it is what the object is."],

text["If what is wanted is an invariant, the package already has the missing half. \
CanonicalHypergraph picks a representative of the isomorphism class, relabelling the vertices \
one to n; composing the two gives a hypermatrix that depends on the isomorphism class alone:"],

code["{VertexID /@ VertexList @ CanonicalHypergraph[hNum],\n \
 VertexID /@ VertexList @ CanonicalHypergraph[hStr],\n \
 CanonicalHypergraph[hNum] === CanonicalHypergraph[hStr]}"],

code["AdjacencyHypermatrix[CanonicalHypergraph[hNum]] ===\n \
 AdjacencyHypermatrix[CanonicalHypergraph[hStr]]"],

note["So there are two functions here, and it is worth knowing which one is wanted. \
AdjacencyHypermatrix[hg] is the adjacency of the hypergraph as it stands, with its own vertex \
names, and is the one to use when those names mean something. AdjacencyHypermatrix[ \
CanonicalHypergraph[hg]] is the adjacency of its isomorphism class, and is the one to use when \
they do not. The second is not offered as a separate function because it is already a composition \
of two, and naming it would only hide which of the two was meant."],

section["11.  Scope and limits"],

text["A scalar is the array of order zero: no indices, no dimensions, one entry. That is what a \
full contraction leaves behind and what multiplies an array entry by entry, so the alternative \
\[LongDash] treating a plain value as something other than an array \[LongDash] would have needed \
a separate rule at each of those places."],

text["Only the three symmetries named in the plan are recognized: rigid, cyclic and symmetric. \
Arbitrary index symmetry groups, and antisymmetry with its sign, are not implemented."],

text["A declared symmetry is checked by testing invariance under a generating set \
of the group, one transposition and one rotation, rather than under every permutation. That is \
enough, and it keeps the check cheap for arrays of high order."],

text["The file tests/HypermatrixExamples.wls runs the examples in this notebook as assertions, \
plus 1300 randomized checks: that order, dimensions and regularity always agree with the entries; \
that a symmetrized array is always accepted as symmetric and a cyclically symmetrized one as \
cyclic; that the canonical order of a hypermatrix never depends on the order its arrays were \
given in; and that the result really is sorted by the three stated criteria."],

text["The file tests/ArrayAlgebraExamples.wls does the same for the special arrays, \
ArrayMultiply, the entry-by-entry operations and FindArrayEquations, checking the contraction \
against Dot, Tr, Diagonal, Transpose and Outer, and confirming that every equation reported really \
holds on arrays the search never saw."],

text["Next: the graphical rendering of 1-, 2- and 3-arrays."]

}];

(* InitializationCellEvaluation tells the front end to run the initialization cells when the
   notebook is opened, and InitializationCellWarning stops it asking first. Together they are what
   makes the package load of its own accord. *)
nb = Notebook[cells,
    StyleDefinitions -> "Default.nb",
    WindowSize -> {900, 800},
    InitializationCellEvaluation -> True,
    InitializationCellWarning -> False
];
Export[target, nb, "NB"];

Print["wrote ", target];
Print["cells: ", Length[cells], "  input: ", Count[cells, Cell[_, "Input", ___]],
      "  output: ", Count[cells, Cell[_, "Output", ___]]];
Print["failed: ", Cases[$log, {s_, $evalFailed} :> s]];





