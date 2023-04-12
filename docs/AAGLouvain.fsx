(**
---
title: Louvain method for community detection
category: FSharp.FGL.ArrayAdjacencyGraph
categoryindex: 4
index: 4
---
*)

(*** hide ***) 
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../src/FSharp.FGL/bin/Release/netstandard2.0/FSharp.FGL.dll"
#r "../src/FSharp.FGL.IO/bin/Release/netstandard2.0/FSharp.FGL.IO.dll"
#r "../src/FSharp.FGL.ArrayAdjacencyGraph/bin/Release/netstandard2.0/FSharp.FGL.ArrayAdjacencyGraph.dll"

(**
#Louvain method for community detection
<hr>
<a name="Louvain method for community detection"></a>
## Louvain method for community detection
*)

open FSharp.FGL
open FSharp.FGL.ArrayAdjacencyGraph
open ArrayAdjacencyGraph.Algorithms

(**
[The louvain method for communty detection](https://iopscience.iop.org/article/10.1088/1742-5468/2008/10/P10008) is a easy method to extract the community structure of large networks. It is based on the concept of modularity optimization.
The algorithm is divided into two phases that are repeated iteratively:

* <b>Modularity optimization</b>, in which every vertex is assigned its own community. After that, for each vertex its neighbours are used to calculate the gain of modularity by removing the vertex from its community and placing it in the community of a neighbour. The new commnuity for the vertex is the one with the biggest modularity gain. This is repeated until no furhter changes that increase the overall modularity can be done.
* <b>Community aggregation</b>, in which the communities found are aggregated in order to build a new, reduced network of communities.

These steps are repeated until no further increase of modularity is possible.
*)

(**
<a name="Example"></a>
# Example
## Example Graph
The first step of the tutorial is the creation of a graph that Louvain can analyse. In this case the graph was taken from [wikipedia](https://en.wikipedia.org/wiki/Community_structure#/media/File:Network_Community_Structure.svg).
*)

//Creating a list of labeled vertices
let vertexList : LVertex<int,string> list =
    [
        for i=0 to 13 do
            (i),(sprintf "%i"i)
    ]

//Creating a list of labeled edges
let edgeList : LEdge<int,float> list =
    [
        0,1,1.; 0,2,1.; 1,2,1.; 1,3,1.;2,3,1.; 3,4,1.; 3,13,1.; 4,5,1.; 4,8,1.; 
        5,6,1.; 5,8,1.; 5,7,1.; 6,7,1.; 7,8,1.; 7,10,1.; 8,9,1.; 9,10,1.; 
        9,12,1.; 9,13,1.; 10,11,1.; 10,13,1.; 11,12,1.; 11,13,1.; 12,13,1.
    ]

//Creating a graph out of the two lists 
let myGraph :ArrayAdjacencyGraph<int,string,float> =
    ArrayAdjacencyGraph(vertexList,edgeList)


(**
## Louvain algorithm

The application of the louvain algorithm on the example graph would look like this:

*)

//Returns the graph with the louvain calculation on top of it
let myGraphLouvain : ArrayAdjacencyGraph<int,string*int,float> =
    Louvain.louvain 0.00001 myGraph

(*** hide ***)
#r "nuget: Cyjs.NET, 0.0.3"
open Cyjs.NET
let inline toCyJS (g : ArrayAdjacencyGraph<int,string*int,float>) =
    let vertices = 
        Array.map2 (fun vertex label -> (vertex,label)) (g.GetVertices()) (g.GetLabels())
        |> Array.map (fun (id,(ogLabel,newLabel)) -> 
            match newLabel with
                | 0 -> Elements.node (string id) [CyParam.label (string ogLabel); CyParam.color "red" ]
                | 1 -> Elements.node (string id) [CyParam.label (string ogLabel); CyParam.color "green" ]
                | 2 -> Elements.node (string id) [CyParam.label (string ogLabel); CyParam.color "blue" ]
                | _ -> Elements.node (string id) [CyParam.label (string ogLabel)]
                )
    let edges =
        edgeList
        |> List.map (fun (v1,v2,weight) -> Elements.edge (string v1 + "_" + string v2) (string v1) (string v2) [CyParam.weight (weight)])
    CyGraph.initEmpty ()
    |> CyGraph.withElements edges
    |> CyGraph.withElements vertices    
    |> CyGraph.withStyle "node" 
        [
            CyParam.content =. CyParam.label
            CyParam.Text.Outline.color =. CyParam.color 
            CyParam.Background.color   =. CyParam.color
        ]

(**
## Visualization

The visualization of the graph is made possible by means of [Cyjs.NET](https://fslab.org/Cyjs.NET/) and looks like this.
The different colors each represent their own community.

*)

(*** hide ***)
myGraphLouvain
|> toCyJS
|> CyGraph.withSize(600, 400) 
|> CyGraph.withLayout (Layout.initCose id)
|> Cyjs.NET.HTML.toEmbeddedHTML
(*** include-it-raw ***)
