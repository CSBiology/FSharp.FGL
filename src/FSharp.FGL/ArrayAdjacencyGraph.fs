namespace FSharp.FGL.ArrayAdjacencyGraph

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Diagnostics.Contracts
open System.Diagnostics

open System.Runtime.InteropServices //for OutAttribute

open FSharp.FGL //For our Graph method

//  An immutable directed graph data structure efficient for large sparse
//  graph representation where out-edge need to be enumerated only.
//  Adaptation of https://github.com/YaccConstructor/QuickGraph.

module internal Dictionary = 

    let tryGetValue k (dict:Dictionary<'K,'V>) =
        let b,v = dict.TryGetValue(k)
        if b then Some v 
        else None

//----- Louvain ----------
//                         \
//----- Graph -> PR FGL ->  -> Louvain auf AA Graph
//

type internal ArrayAdjacencyGraph(vertexOutEdges:Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>, labels:Dictionary<'Vertex,'Label>) =
    
    //Number of edges in the graph
    let mutable edgeCount = 0

    //Dictonary of the lables belonging to the vertices with (k,v) k-> vertex, v-> lable.
    let labels = labels
    //
    let vertexOutEdges = vertexOutEdges

    new () = ArrayAdjacencyGraph(new Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>(), new Dictionary<'Vertex,'Label>())
        
    //new(vertices : LVertex<'Vertex,'Label> seq,edges : LEdge<'Vertex,'Edge> seq) =
    //    ArrayAdjacencyGraph
        
    
    //Number of edges in the graph
    member this.EdgeCount       = edgeCount

    //Lookup the first edge in the graph that matches the conditions, returning a Some value if it exists and None if not.
    member this.TryGetEdge((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> option =
        let tryGetSource =
            Dictionary.tryGetValue source vertexOutEdges

        if tryGetSource = None then None
        else
            let edges = tryGetSource |> Option.get
            let rec loop i = 
                if i >= edges.Length then None else 
                if (edges.[i]|>fun (s,t,w) -> t) = target then Some edges.[i]  else loop (i+1)
            loop 0                
    
    //Lookup all edges in the graph that matches the conditions, returning a Some value if it exists and None if not.
    member this.TryGetEdges((source:'Vertex),(target:'Vertex)) :'LEdge list option=
        let tryGetSource =
            Dictionary.tryGetValue source vertexOutEdges

        if tryGetSource = None then None
        else
            let edges = tryGetSource |> Option.get
            let rec loop i _edges= 
                if i >= edges.Length then _edges else 
                if (edges.[i]|>fun (s,t,w) -> t) = target then loop (i+1) ((edges.[i])::_edges)  else loop (i+1) (_edges)
            
            let _edges = loop 0 []
            if  _edges = List.empty then None
            else Some _edges              
    
    ///Returns true, if the edge from vertex cource to vertex target is contained in the graph. Otherwise, it returns false.
    member this.ContainsEdge((source:'Vertex),(target:'Vertex)) :bool =

        match this.TryGetEdge(source,target) with 
        | Some x    -> true
        | None      -> false
    
    //Returns the degree of the vertex v.
    member this.OutDegree(v:'Vertex) :int =
        let mutable edges = Unchecked.defaultof<_> 
        if (vertexOutEdges.TryGetValue(v, &edges) && edges <> null) then
            edges.Length
        else
            0
    
    //Returns true, if the vertex v does not have edges connected to it. Otherwise, it returns false.
    member this.IsOutEdgesEmpty(v:'Vertex) :bool =
        this.OutDegree(v) = 0
    
    //Returns all edges connected to the vertex v.
    member this.OutEdges(v : 'Vertex) : LEdge<'Vertex,'Edge> [] =       
        let tryGetSource =
            Dictionary.tryGetValue v vertexOutEdges
            
        match tryGetSource with
            | Some x -> x
            | None -> Array.empty
    
    //Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetOutEdges((v:'Vertex)) =
        Dictionary.tryGetValue v vertexOutEdges
                          
    member this.OutEdge(v:'Edge, index:int) :LEdge<'Vertex,'Edge>=
        vertexOutEdges.[v].[index]
        
    //Returns true, since the graph model is always directed.
    member this.IsDirected :bool =
        true

    //Returns true, since parallel edges are allowed
    member this.AllowParallelEdges :bool =
        true

    //Returns true, if the graph contains the vertex, else false.
    member this.ContainsVertex(vertex:'Vertex) :bool =
        vertexOutEdges.ContainsKey(vertex)

    //Returns true, if there are no vertices in the graph, else false.
    member this.IsVerticesEmpty :bool =
        vertexOutEdges.Count = 0

    //Returns the number of vertices of the graph.
    member this.VertexCount :int =
        vertexOutEdges.Count
    
    //Returns the vertices of the graph.
    member this.Vertices  =
        //for vertices in vertexOutEdges.Keys do
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Key
            i <- i+1
        result
        
    //Returns true, if there are no edges in the graph, else false.
    member this.IsEdgesEmpty :bool =
        edgeCount = 0
        
    //Returns all edges of the graph.
    member this.Edges =
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Value
            i <- i+1
        result
        |> Array.concat
        |> Array.distinct
    
    //Returns true, if the edge is found in the graph, else false.
    member this.ContainsEdge(edge:LEdge<'Vertex,'Edge>) :bool =     
        
        let (source,target,weight) = edge
        let edgeOptions = this.TryGetEdges(source,target)
        match edgeOptions with
        | Some x -> List.contains(edge) x
        | None -> false

        