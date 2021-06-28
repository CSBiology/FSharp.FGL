(**
---
title: Random graph models
category: FSharp.FGL.ArrayAdjacencyGraph
categoryindex: 4
index: 3
---
*)

(*** hide ***) 
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/FSharp.FGL/netstandard2.0/"
#I @"../../bin/FSharp.FGL.ArrayAdjacencyGraph/netstandard2.0/"

(**
#Random graph models
<hr>
<a name="Random graph models"></a>
## Random graph models
*)

#r "FSharp.FGL"
#r "FSharp.FGL.ArrayAdjacencyGraph"

open FSharp.FGL
open FSharp.FGL.ArrayAdjacencyGraph
open ArrayAdjacencyGraph.Models

(**
To create random graph models, ArrayAdjacenyGraph includes three different methods:
<br>

*)

(**
<a name="Gilbert"></a>
## Gilbert
[The gilbert model](https://epubs.siam.org/doi/10.1137/0109045) (or G(N,p) model) was introduced by Edgar Gilbert in 1959. In this model, you assign a fixed amount of vertices N and a probability p. p denotes the probality, that edge between two vertices exists or not.
*)

open Gilbert.Gilbert

//Create a new random graph based on the gilbert graph model
let gilbertRandomGraph : ArrayAdjacencyGraph<int,string,float> =
    gilbert 100 0.1 false (id) (fun x -> sprintf "%i" x) (fun (a,b) -> 1.)


(**
<a name="Barabasi-Albert"></a>
## Barabasi-Albert
[The Barabasi-Albert model](https://science.sciencemag.org/content/286/5439/509) is an algorithm for creating scale-free graphes where on an existing graph n vertices with m edges that are added to the vertex each iteration.
*)

open BarabasiAlbert.BarabasiAlbert

//Take a existing graph (here, the gilbertRandomGraph) and add the specified amount of vertices (50) with at least 5 edges connected to the new vertices
let barabasiAlbertGraph : ArrayAdjacencyGraph<int,string,float> =
    barabasiAlbert gilbertRandomGraph 50 5 (id) (fun x -> sprintf "%i" x) (fun (a,b) -> 1.)

(**
<a name="Bollobas-Riordan"></a>
## Bollobas-Riordan
[The Bollobas-Riordan model](https://onlinelibrary.wiley.com/doi/abs/10.1002/3527602755.ch1), an implementation similar to barabasiAlbert, where the resulting graph can be easily influenced thanks to the three additional parameters.
*)

open Scale_free_graph.BollobasRiordan

//Create a graph following the bollobasRiordan graph model    
let bollobasRiordanGraph : ArrayAdjacencyGraph<int,int,float> =
    bollobasRiordan 100 0.6 0.1 0.3 0.5 0.5 (ArrayAdjacencyGraph())
