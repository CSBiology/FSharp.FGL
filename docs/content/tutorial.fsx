(**
---
title: Introduction
category: FSharp.FGL 
categoryindex: 3
index: 1
---
*)
(*** hide ***) 
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
#Introduction
<hr>
<a name="InductiveGraph"></a>
## Inductive Graph
The general aim of this FSharp.FGL is to provide an environment for F# programmers to functionally work with graphs. Besides the basic functions a graph-structure has to fulfill (like adding/removing vertices or counting edges), there are also functionalities needed which are not as elegantly and efficiently implementable in functional graphs (e.g. marking vertices as visited). This is tackled by Martin Erwig's [Inductive Graph Model](https://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf). 

The inductive graph consists of so called contexts. Every context carries the information about one vertex in the form (pred, id, l, succ) where:<br>
<p class="indent">-<b>pred</b> is a collection of all vertices pointing to this vertex</p>
<p class="indent">-<b>id</b> is the identifier of the vertex</p>
<p class="indent">-<b>l</b> is the label of the vertex</p>
<p class="indent">-<b>succ</b> is a collection of all vertices this vertex points to</p>
In this structure, both pred and succ are of type adjacency. This type consists of the id of the other vertex and the edgelabel.

Working with the inductive graph model allows easy recursive walking through the graph because every vertex contains the information of it's edges to the other vertices. The so called [decompose-function]() then solves the problem of remembering which vertices were already visited by removing them from the graph.

<a name="Features"></a>
## Features
The basic structure of the implementation is done as in the [Hekate graph library](https://github.com/xyncro/hekate). Building a new library has many reasons:
<p class="indent">-FSharp.FGL was built with the intent to have easily readable code, so users can -if needed- design their own functions with the given structure more easily</p>
<p class="indent">-The aforementioned decompose function is publicly accessible in FSharp.FGL. This is important for using path search algorithms more efficiently</p>
<p class="indent">-The functions in FSharp.FGL are sorted into the Undirected and the Directed module. By this, users can more intuitively pick the right function for their purpose</p>
<p class="indent">-FSharp.FGL aims to provide a thorough set of graph related functions, including path search functions and different graph models.</p>

*)
