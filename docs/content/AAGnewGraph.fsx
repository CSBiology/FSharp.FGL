(**
---
title: Graph creation
category: ArrayAdjacencyGraph
categoryindex: 4
index: 2
---
*)

(*** hide ***) 
#I @"../../bin/FSharp.Graph/netstandard2.0/"
#I @"../../bin/FSharp.ArrayAdjacencyGraph/netstandard2.0/"


(**
<a name="Creating a Graph"></a>
## Creating a Graph

The graph creation in ArrayAdjacencyGraph can be achived via two differnet approaches. 
<br> The easiest method is relies on a vertex list and an edge list like this :
*)

#r "FSharp.Graph"
#r "FSharp.ArrayAdjacencyGraph"

open FSharp.Graph
open FSharp.ArrayAdjacencyGraph


//Creating a list of labeled vertices
let vertexList : LVertex<int,string> list = 
    List.init 4 (fun i -> 
    i,
    sprintf "VertexNr. %i" i)

//Creating a list of labeled edges
let edgeList : LEdge<int,float> list = 
    [(1,2,1.);(2,1,1.);(1,3,0.5);(3,4,0.8);(4,3,0.8)]

//Creating a graph out of the two lists 
let myGraph : ArrayAdjacencyGraph<int,string,float> =
    ArrayAdjacencyGraph(vertexList,edgeList)

(**
Alternatively, an empty graph can be created and filled with vertices and edges after its creation.
*)

//Creating an array of the labeled vertices
let vertexArray : LVertex<int,string> [] = 
    vertexList
    |> Array.ofList

//Creating an array of the labeled edges
let edgeArray : LEdge<int,float> [] = 
    edgeList
    |> Array.ofList

//Creating an empty graph and filling it afterwards
let myGraph' : ArrayAdjacencyGraph<int,string,float> =
    
    //Create a new, empty graph
    let emptyGraph = ArrayAdjacencyGraph()
    
    //Add an array of vertices with the AddManyVertices method
    emptyGraph.AddManyVertices(vertexArray)
    
    //Add an array of edges with the AddManyEdges method
    emptyGraph.AddManyEdges(edgeArray)
    
    //Return the graph
    emptyGraph

(**
<a name="Adding/Removing Vertices and Edges"></a>
## Adding/Removing Vertices and Edges
The AddManyVertices and AddManyEdges, as well as their single counterparts AddVertex and AddEdge can be used at all times, allowing an easy modification of the graph. 
<br>Adding a vertex and an edge works like this:
*)

//New Vertex
let newVertex : LVertex<int,string> = 
    123,"123"

//Edge to new Vertex
let newEdge : LEdge<int,float> =
    1,123,10.

//Add newVertex to myGraph
myGraph.AddVertex(newVertex)

//Add newEdge to myGraph
myGraph.AddEdge(newEdge)

(**
<br>
<hr>
Removing vertices and edges is also very easy. If you remove a vertex from the Graph all edges featuring this vertex are also removed. The removal of edges themself is simple as well. 
*)

//Remove an edge from myGraph
myGraph.RemoveEdge(1,2,1.)

//Remove a vertex from myGraph
myGraph.RemoveVertex(123)
