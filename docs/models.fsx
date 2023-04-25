(**
---
title: Random graph models
category: FSharp.FGL 
categoryindex: 2
index: 3
---
*)
(*** hide ***) 
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../src/FSharp.FGL/bin/Release/netstandard2.0/FSharp.FGL.dll"
#r "../src/FSharp.FGL.IO/bin/Release/netstandard2.0/FSharp.FGL.IO.dll"
#r "../src/FSharp.FGL.ArrayAdjacencyGraph/bin/Release/netstandard2.0/FSharp.FGL.ArrayAdjacencyGraph.dll"

(**
#Random graph models
*)
open FSharp.FGL.Directed
(**
## Motivation and overview
In every implementation workflow, there comes the point where you have to test wether everything works as expexted or not.
For this, a matching test set is necessary. In some cases (e.g. List sorting algorithms) creating those test sets is done in a matter of seconds.
In other cases, especially if the data you work with is more than one dimensional, it can get quite tedious.
To this effect, FSharp.FGl comes equipped with implementations of random graph generation models.  
In this tutorial I want to introduce you to the models implemented and how to generate graphs with the given functions:
    
* [Gilbert model](models.html#Gilbert-model) (not scale free)

* [Erdős–Rényi model](models.html#Erdős–Rényi-model) (not scale free)

* Barabási–Albert model (scale free; work in progress)  

Afterwards I'll give a quick intro about [how to use the implementations to create graphs of any type](models.html#Genericity).

## Models
### Gilbert model

The gilbert model (or G(N,p) model) was introduced by Edgar Gilbert in 1959. In this model, you assign a fixed amount of vertices N and a probability p.
p denotes the probality, that edge between two vertices exists or not.  
The function is located in the Models module and can be used in the following way:
*)

let N = 50
let p = 0.5
//This function takes an int and creates a labeled vertex ((id,label) tuple)
let vertexCreator i = (i,sprintf "Call me Vertex nr. %i" i)

let myGilbertGraph = Models.gilbert vertexCreator N p

(**
### Erdős–Rényi model

The Erdős–Rényi model (or G(N,L) model) as used by Pál Erdős and Alfréd Rényi is a graph model where you assign a fixed vertex count and also a random edge count.
Those edges then get randomly distributed between the given edges.
The function is located in the Models module and can be used in the following way:
*)

let L = 200
//We just use the vertexCreator and the nodeCount N assigned above
let myErdosRenyiGraph = Models.erdosRenyi vertexCreator N L

(**
## Genericity
work in progress
*)
