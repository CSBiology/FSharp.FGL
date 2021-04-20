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

type internal ArrayAdjacencyGraph(vertexEdges:Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>, labels:Dictionary<'Vertex,'Label>) =
    
    // Number of edges in the graph
    let mutable edgeCount = 0

    // Dictonary of the lables belonging to the vertices with (k,v) k-> vertex, v-> lable.
    let labels = labels
    //
    let vertexOutEdges = vertexEdges

    new () = ArrayAdjacencyGraph(new Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>(), new Dictionary<'Vertex,'Label>())
        
    new(vertices : LVertex<'Vertex,'Label> list,edges : LEdge<'Vertex,'Edge> list) =
        let vertexKeys = vertices |> List.map fst
        let vertexEdges = System.Collections.Generic.Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>()
        let labelDict   = System.Collections.Generic.Dictionary<'Vertex,'Label>()
        
        for i=0 to vertexKeys.Length-1 do
            labelDict.Add (vertices.[i])
              
        for i=0 to vertexKeys.Length-1 do 
            let vertex  = vertexKeys.[i]
            let edges   = List.filter (fun (s, t, w) -> s=vertex ||t=vertex) edges |> Array.ofList
            
            vertexEdges.Add (vertex,edges)
        
        ArrayAdjacencyGraph(vertexEdges,labelDict)
                    
    // Number of edges in the graph
    member this.EdgeCount       = edgeCount

    // Lookup the first edge in the graph that matches the conditions, returning a Some value if it exists and None if not.
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
    
    // Lookup all edges in the graph that matches the conditions, returning a Some value if it exists and None if not.
    member this.TryGetEdges((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> list option=
        let tryGetSource =
            Dictionary.tryGetValue source vertexOutEdges

        if tryGetSource = None then None
        else
            let edges = tryGetSource |> Option.get
            
            let rec loop i _edges= 
                if i >= edges.Length then _edges else 
                if (edges.[i] |> fun (s,t,w) -> t) = target then 
                    loop (i+1) ((edges.[i])::_edges)  
                else 
                    loop (i+1) (_edges)
            
            let _edges = loop 0 []
            if  _edges = List.empty then None
            else Some _edges              
    
    // Returns true, if the edge is found in the graph, else false.
    member this.ContainsEdge(edge:LEdge<'Vertex,'Edge>) :bool =     
        
        let (source,target,weight) = edge
        let edgeOptions = this.TryGetEdges(source,target)
        
        match edgeOptions with
            | Some x -> List.contains(edge) x
            | None -> false

    // Returns the degree of the vertex v.
    member this.Degree(v:'Vertex) :int =
        let mutable edges = Unchecked.defaultof<_> 
        if (vertexOutEdges.TryGetValue(v, &edges) && edges <> null) then
            edges.Length
        else
            0
    
    // Returns true, if the vertex v does not have edges connected to it. Otherwise, it returns false.
    member this.IsOutEdgesEmpty(v:'Vertex) :bool =
        this.Degree(v) = 0
    
    //// Returns all edges connected to the vertex v.
    //member this.OutEdges(v : 'Vertex) : LEdge<'Vertex,'Edge> [] =       
    //    let tryGetSource =
    //        Dictionary.tryGetValue v vertexOutEdges
    //        
    //    match tryGetSource with
    //        | Some x -> x
    //        | None -> Array.empty
    
    // Lookup all edges connected to the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetConnectedEdges((v:'Vertex)) =
       (Dictionary.tryGetValue v vertexOutEdges)

    // Lookup all edges that target the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetInEdges((v:'Vertex)) =
        let tryGetSource = (Dictionary.tryGetValue v vertexOutEdges)
        
        if tryGetSource = None then None
        else
            let edges = tryGetSource |> Option.get
            
            let rec loop i _edges= 
                if i >= edges.Length then _edges else 
                if (edges.[i] |> fun (s,t,w) -> t) = v then 
                    loop (i+1) ((edges.[i])::_edges)  
                else 
                    loop (i+1) (_edges)
            
            let _edges = loop 0 []
            if  _edges = List.empty then None
            else Some _edges   
    
    // Lookup all edges that originate the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetOutEdges((v:'Vertex)) =
        let tryGetSource = (Dictionary.tryGetValue v vertexOutEdges)
        
        if tryGetSource = None then None
        else
            let edges = tryGetSource |> Option.get
            
            let rec loop i _edges= 
                if i >= edges.Length then _edges else 
                if (edges.[i] |> fun (s,t,w) -> s) = v then 
                    loop (i+1) ((edges.[i])::_edges)  
                else 
                    loop (i+1) (_edges)
            
            let _edges = loop 0 []
            if  _edges = List.empty then None
            else Some _edges   
    
    // Returns the number of edges that originate from the vertex v.
    member this.InDegree((v:'Vertex)) =
        this.TryGetInEdges v
        |> Option.map (List.length)
    
    // Returns the number of edges that target the vertex v.
    member this.OutDegree((v:'Vertex)) =
        this.TryGetOutEdges v
        |> Option.map (List.length)

    //member this.OutEdge(v:'Edge, index:int) :LEdge<'Vertex,'Edge>=
    //    vertexOutEdges.[v].[index]
        
    // Returns true, since the graph model is always directed.
    member this.IsDirected :bool =
        true

    // Returns true, since parallel edges are allowed
    member this.AllowParallelEdges :bool =
        true

    // Returns true, if the graph contains the vertex, else false.
    member this.ContainsVertex(vertex:'Vertex) :bool =
        vertexOutEdges.ContainsKey(vertex)

    // Returns true, if there are no vertices in the graph, else false.
    member this.IsVerticesEmpty :bool =
        vertexOutEdges.Count = 0

    // Returns the number of vertices of the graph.
    member this.VertexCount :int =
        vertexOutEdges.Count
    
    // Returns the vertices of the graph.
    member this.Vertices  =
        //for vertices in vertexOutEdges.Keys do
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Key
            i <- i+1
        result
        
    // Returns true, if there are no edges in the graph, else false.
    member this.IsEdgesEmpty :bool =
        this.EdgeCount = 0
        
    // Returns all edges of the graph.
    member this.Edges =
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Value
            i <- i+1
        result
        |> Array.concat
        |> Array.distinct
    
    //FGL Functions Vertices
    // Adds a labeled vertex to the graph.
    member this.AddVertex ((v, l): LVertex<'Vertex,'Label>)=
        (vertexOutEdges.Add (v,[||]))   |> ignore
        (labels.Add (v, l)          )   |> ignore
        this

    // Adds a list of labeled vertices to the graph.    
    member this.AddManyVertices (vertices:list<LVertex<'Vertex,'Label>>) =       
        for vertex in vertices do
            (this.AddVertex vertex) |> ignore
        
        this

    // Removes a vertex from the graph.
    member this.RemoveVertex (v:'Vertex) =       
        (vertexOutEdges.Remove (v)) |> ignore
        (labels.Remove (v))         |> ignore
        this

    // Removes a list of vertices from the graph.
    member this.RemoveManyVertices nList  =
        for vertex in nList do
            this.RemoveVertex vertex |> ignore

        this
    
    // Returns all neighbours of the vertex.
    member this.Neighbours(v:'Vertex) =
        match (this.TryGetConnectedEdges v) with
            | Some x    -> Array.fold (fun folder (s, t, w) -> (s::t::folder)) [] x |> List.distinct
            | None      -> []

    // FGL Functions Edges
    // Adds a labeled, undirected edge to the graph.
    member this.AddEdge((s, t, w): LEdge<'Vertex,'Edge>) =
        let edgeListSource = 
            match (Dictionary.tryGetValue s vertexOutEdges) with
                | Some x    -> x |> Array.toList
                | None      -> failwith "The source vertex of the edge does not exist in this graph."
                  
        let edgeListTarget = 
            match (Dictionary.tryGetValue t vertexOutEdges) with
                | Some x    -> x |> Array.toList
                | None      -> failwith "The target vertex of the edge does not exist in this graph."
        
        vertexOutEdges.Item s <- ((s,t,w)::edgeListSource |> Array.ofList)
        vertexOutEdges.Item t <- ((s,t,w)::edgeListTarget |> Array.ofList)
        this
    
    // Adds a list of labeled, undirected edges to the graph.
    member this.AddManyEdges edgeList=
        for edge in edgeList do
            this.AddEdge edge |> ignore
        
        this
    
    // Removes an edge from the graph.
    member this.RemoveEdge((s, t, w): LEdge<'Vertex,'Edge>) =
        let edgeListSource = 
            match (Dictionary.tryGetValue s vertexOutEdges) with
                | Some x    -> x |> Array.filter (fun x -> x <> (s, t, w) )
                | None      -> failwith "The source vertex of the edge does not exist in this graph."
                  
        let edgeListTarget = 
            match (Dictionary.tryGetValue t vertexOutEdges) with
                | Some x    -> x |> Array.filter (fun x -> x <> (s, t, w) )
                | None      -> failwith "The target vertex of the edge does not exist in this graph."
             
        vertexOutEdges.Item s <- (edgeListSource)
        vertexOutEdges.Item t <- (edgeListTarget)
        this

    // Removes a list of edges from the graph.
    member this.RemoveManyEdges edgeList =
        for edge in edgeList do
            this.RemoveEdge edge |> ignore

        this
