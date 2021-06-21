(**
---
title: Basic functionality
category: ArrayAdjacencyGraph
categoryindex: 4
index: 1
---
*)

(*** hide ***) 
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/FSharp.FGL/netstandard2.0/"
#I @"../../bin/FSharp.Graph/netstandard2.0/"
#I @"../../bin/FSharp.ArrayAdjacencyGraph/netstandard2.0/"

#r "FSharp.FGL.dll"
#r "FSharp.Graph"
#r "FSharp.ArrayAdjacencyGraph"

(**
#ArrayAdjacencyGraph
<hr>
<a name="Introduction"></a>
## Introduction
FSharp.ArrayAdjacencyGraph is an adjacency list representation of a multigraph. It is a modified verion of the [YaccConstructor/QuickGraph/ArrayAdjacencyGraph](https://github.com/YaccConstructor/QuickGraph/blob/master/src/QuickGraph/ArrayAdjacencyGraph.cs). 

The model itself works on two different dictionaries, the label dictionary and the edge dictionary. Both feature vertices as keys. In case of the edge dictionary, the bound value is an array that includes all edges the vertex key is part of. 

Working with this graph model allows for fast caculations on the edges 
<a name="Features"></a>
## Features
The basic structure of the implementation is done as in [YaccConstructor/QuickGraph/ArrayAdjacencyGraph](https://github.com/YaccConstructor/QuickGraph/blob/master/src/QuickGraph/ArrayAdjacencyGraph.cs), with minor adjustments to fit FSharp better. 
<br>Building a second graph representation type into FSharp.FGL results in improvements like:
<p class="indent">- <b>streamlining</b> ArrayAdjacencyGraph is not limited to directed/undirected and therefore allows for a smoother workflow </p>
<p class="indent">- <b>readability</b>  ArrayAdjacencyGraph features an easy to read code that is easier to understand than the FSharp.FGL variant </p>
<p class="indent">- <b>speed</b>        Computation on the ArrayAdjacencyGraph model is generally faster compared to the inductive graph model </p>

*)
