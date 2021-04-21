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

    // Dictionary of the lables belonging to the vertices with (k,v) k-> vertex, v-> lable.
    let labels :Dictionary<'Vertex,'Label> = labels
    // Dictionary of the edges belonging to the vertices with (k,v) k-> vertex, v-> edges.
    let vertexOutEdges :Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]> = vertexEdges

    new () = ArrayAdjacencyGraph(new Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>(), new Dictionary<'Vertex,'Label>())
        
    new(vertices : LVertex<'Vertex,'Label> list,edges : LEdge<'Vertex,'Edge> list) =
        let vertexKeys = vertices |> List.map fst
        let vertexEdges = System.Collections.Generic.Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>()
        let labelDict   = System.Collections.Generic.Dictionary<'Vertex,'Label>()
        
        for i=0 to vertexKeys.Length-1 do
            let vertex  = vertexKeys.[i]
            let edges   = List.filter (fun (s, t, w) -> s=vertex ||t=vertex) edges |> Array.ofList          
            
            labelDict.Add (vertices.[i])                                  
            vertexEdges.Add (vertex,edges)
        
        ArrayAdjacencyGraph(vertexEdges,labelDict)
                    
    ///Number of edges in the graph
    member this.EdgeCount       = edgeCount

    ///Returns Some label, if a label for the vertex v exists, else none.
    member this.TryGetLabel(v:'Vertex) =        
        Dictionary.tryGetValue v labels
    
    ///Returns the label for the vertex v.
    member this.GetLabel(v:'Vertex) = 
        match (this.TryGetLabel(v)) with
            |Some x     -> x
            |None       -> failwith("The vertex v does exist")
    
    ///Sets the label for the vertex v.
    member this.SetLabel((v:'Vertex),(l:'Label)) = 
        labels.Item v <- (l)
      
    ///Lookup the first edge in the graph that matches the conditions, returning a Some value if it exists and None if not.
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
    
    ///Return the first edge in the graph that matches the conditions.
    member this.getEdge((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> =
        match (this.TryGetEdge(source,target)) with
            |Some x -> x
            |None   -> failwith "Edge does not exist"

    ///Lookup all edges in the graph that matches the conditions, returning a Some value if it exists and None if not.
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
    
     ///Return all edges in the graph that matches the conditions
    member this.GetEdges((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        match (this.TryGetEdges(source,target)) with
                   |Some x -> (x |> Array.ofList)
                   |None   -> [||]

    ///Returns true, if the edge is found in the graph, else false.
    member this.ContainsEdge(edge:LEdge<'Vertex,'Edge>) :bool =     
        
        let (source,target,weight) = edge
        let edgeOptions = this.TryGetEdges(source,target)
        
        match edgeOptions with
            | Some x -> List.contains(edge) x
            | None -> false

    ///Returns the degree of the vertex v.
    member this.Degree(v:'Vertex) :int =
        let mutable edges = Unchecked.defaultof<_> 
        if (vertexOutEdges.TryGetValue(v, &edges) && edges <> null) then
            edges.Length
        else
            0
    
    ///Returns true, if the vertex v does not have edges connected to it. Otherwise, it returns false.
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
    
    ///Lookup all edges connected to the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetConnectedEdges((v:'Vertex)) =
       (Dictionary.tryGetValue v vertexOutEdges)

    ///Lookup all edges connected to the vertex v in the graph, returning an array of connected edges.
    member this.GetConnectedEdges((v:'Vertex)) =
        match (this.TryGetConnectedEdges(v)) with
            |Some x -> x
            |None   -> [||]

    ///Lookup all edges that target the vertex v in the graph, returning a Some value if a binding exists and None if not.
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
            else Some (_edges |> Array.ofList)
    
    ///Lookup all edges that target the vertex v in the graph, returning an array of connected edges.
    member this.GetInEdges((v:'Vertex)) =
        match (this.TryGetInEdges(v)) with
            |Some x -> x
            |None   -> [||]

    ///Lookup all edges that originate from the vertex v in the graph, returning a Some value if a binding exists and None if not.
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
            else Some (_edges |> Array.ofList)   
    
    ///Lookup all edges that originate from the vertex v in the graph, returning an array of connected edges.
    member this.GetOutEdges((v:'Vertex)) =
        match (this.TryGetOutEdges(v)) with
            |Some x -> x
            |None   -> [||]
    
    ///Lookup the weight of the edge defined by source and vertex. If it exists, return Some value, else none.
    member this.TryGetWeight((source:'Vertex),(target:'Vertex)) =
        match (this.TryGetEdge((source),(target))) with
            |Some x -> Some(fun (s, t, w) -> w)
            |None   -> None
    
    ///Return the weight of the edge defined by source and vertex
    member this.GetWeight((source:'Vertex),(target:'Vertex)) =
        match (this.TryGetEdge((source),(target))) with
            |Some x -> (fun (s, t, w) -> w)
            |None   -> failwith "No weight found"

    ///Set the weight of the LEdge defined by source and vertex to be equal weight.
    member this.setWeight((source:'Vertex),(target:'Vertex),(weight:'Edge)) =
        let edgeListSource = 
            match (Dictionary.tryGetValue source vertexOutEdges) with
                | Some x    -> x |> Array.map (fun (s, t, w) -> if t=target then (s, t, weight) else (s, t, w))
                | None      -> failwith "The source vertex of the edge does not exist in this graph."
                 
        let edgeListTarget = 
            match (Dictionary.tryGetValue target vertexOutEdges) with
                | Some x    -> x |> Array.map (fun (s, t, w) -> if s=source then (s, t, weight) else (s, t, w))
                | None      -> failwith "The target vertex of the edge does not exist in this graph."
        
        vertexOutEdges.Item source <- (edgeListSource)
        vertexOutEdges.Item target <- (edgeListTarget)
        this

    ///Returns the number of edges that originate from the vertex v.
    member this.InDegree((v:'Vertex)) =
        this.TryGetInEdges v
        |> Option.map (Array.length)
    
    ///Returns the number of edges that target the vertex v.
    member this.OutDegree((v:'Vertex)) =
        this.TryGetOutEdges v
        |> Option.map (Array.length)

    //member this.OutEdge(v:'Edge, index:int) :LEdge<'Vertex,'Edge>=
    //    vertexOutEdges.[v].[index]
        
    ///Returns true, since the graph model is always directed.
    member this.IsDirected :bool =
        true

    ///Returns true, since parallel edges are allowed
    member this.AllowParallelEdges :bool =
        true

    ///Returns true, if the graph contains the vertex, else false.
    member this.ContainsVertex(vertex:'Vertex) :bool =
        vertexOutEdges.ContainsKey(vertex)

    ///Returns true, if there are no vertices in the graph, else false.
    member this.IsVerticesEmpty :bool =
        vertexOutEdges.Count = 0

    ///Returns the number of vertices of the graph.
    member this.VertexCount :int =
        vertexOutEdges.Count
    
    ///Returns the vertices of the graph.
    member this.Vertices  =
        //for vertices in vertexOutEdges.Keys do
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Key
            i <- i+1
        result
        
    ///Returns true, if there are no edges in the graph, else false.
    member this.IsEdgesEmpty :bool =
        this.EdgeCount = 0
        
    ///Returns all edges of the graph.
    member this.Edges =
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Value
            i <- i+1
        result
        |> Array.concat
        |> Array.distinct
    
    // FGL Functions Edges
    ///Adds a labeled, undirected edge to the graph.
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
    
    ///Adds a list of labeled, undirected edges to the graph.
    member this.AddManyEdges edgeList=
        for edge in edgeList do
            this.AddEdge edge |> ignore
        
        this
    
    ///Removes an edge from the graph.
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

    ///Removes a list of edges from the graph.
    member this.RemoveManyEdges edgeList =
        for edge in edgeList do
            this.RemoveEdge edge |> ignore

        this
    
    //FGL Functions Vertices
    ///Adds a labeled vertex to the graph.
    member this.AddVertex ((v, l): LVertex<'Vertex,'Label>)=
        (vertexOutEdges.Add (v,[||]))   |> ignore
        (labels.Add (v, l)          )   |> ignore
        this

    ///Adds a list of labeled vertices to the graph.    
    member this.AddManyVertices (vertices:list<LVertex<'Vertex,'Label>>) =       
        for vertex in vertices do
            (this.AddVertex vertex) |> ignore
        
        this

    ///Removes a vertex from the graph.
    member this.RemoveVertex (v:'Vertex) =       
        (vertexOutEdges.Item v) 
        |> Array.toList 
        |> this.RemoveManyEdges 
        |> ignore
        
        (vertexOutEdges.Remove (v)) |> ignore
        (labels.Remove (v))         |> ignore
        this

    ///Removes a list of vertices from the graph.
    member this.RemoveManyVertices nList  =
        for vertex in nList do
            this.RemoveVertex vertex |> ignore

        this
    
    ///Returns Some edges if they are neighbours of the vertex, else None.
    member this.TryNeighbours(v:'Vertex) =
        match (this.TryGetConnectedEdges v) with
            | Some x    -> Some(Array.fold (fun folder (s, t, w) -> (s::t::folder)) [] x |> List.distinct)
            | None      -> None
    
    ///Returns the neighbouring edges of the vertex v.
    member this.Neighbours(v:'Vertex) =
        match (this.TryNeighbours (v)) with
            |Some x -> x
            |None   -> failwith "The vertex does not have any neighbours"
