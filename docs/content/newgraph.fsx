(*** hide ***) 
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.FGL"

(**
#Working with FSharp.FGL

In FSharp.FGL, the basic functions are separated into the Directed and the Undirected namespaces. Both undirected and directed graph functions use the same input type, but the information is read differently, depending on wether you call a function from the undirected or the directed namespace. Therefore it is advised to decide which kind of graph you work with beforehand. 
In this quick tutorial we will work with directed Graphs.

*)

#r "FSharp.FGL.dll"
open FSharp.FGL
open FSharp.FGL.Directed

(**
## Creating a Graph

The Graph can be created by the "Graph.empty" function. The Graph can then be filled by the "Vertices.add"/"Vertices.addMany" and the "Edges.add"/"Edges.addMany" functions. 
Keep in mind that FSharp.FGL does not support unlabeled edges and unlabeled vertices

*)

//Creating a list of labeled vertices
let vertexList : LVertex<int,string> list = List.init 4 (fun i -> 
        i + 1,
        sprintf "VertexNr. %i" i + 1)

//Creating a list of labeled edges
let edgeList : LEdge<int,float> list = [(1,2,1.);(2,1,1.);(1,3,0.5);(3,4,0.8);(4,3,0.8)]

//Creating a graph out of the two lists can be done in different ways
let myGraph : Graph<int,string,float> = 
    Graph.create vertexList edgeList

let myGraph' : Graph<int,string,float> =
    Graph.empty
    |> Vertices.addMany vertexList
    |> Edges.addMany edgeList

(**
The Graph.create function is located in the Directed and in the Undirected namespaces. Therefore calling it while having one of those opened will result in building the according graph type.
Adding and Removing single vertices or edges can be done with the following functions:
*)

let myChangedGraph : Graph<int,string,float> =
    myGraph
    |> Vertices.add (5,"VertexNr. 5")
    |> Edges.add (5,1,0.7)
    |> Edges.remove (2,1)

(**

## Iteration

coming soon
*)
