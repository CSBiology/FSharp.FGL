namespace FSharp.FGL.ArrayAdjacencyGraph

open System.Collections.Generic
open FSharp.FGL //For our Graph method

//  An immutable directed graph data structure efficient for large sparse
//  graph representation where out-edge need to be enumerated only.
//  Adaptation of https://github.com/YaccConstructor/QuickGraph.

module internal Dictionary = 

    let tryGetValue k (dict:Dictionary<'K,'V>) =
        let b,v = dict.TryGetValue(k)
        if b then Some v 
        else None

    let getValue k (dict:Dictionary<'K,'V>) =
        match (tryGetValue k dict) with 
            |Some x -> x
            |None -> failwith "Error get"
         
    let copyRecursive (innerCopyF : 'V -> 'V) (dict:Dictionary<'K,'V>) =
        let newDict = Dictionary<'K,'V>()
        for kv in dict do
            newDict.Add(kv.Key,innerCopyF kv.Value)
        newDict

//----- Louvain ----------
//                         \
//----- Graph -> PR FGL ->  -> Louvain auf AA Graph
//

type ArrayAdjacencyGraph<'Vertex,'Label,'Edge when 'Vertex : equality and 'Edge : equality> internal (vertexEdges:Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>, labels:Dictionary<'Vertex,'Label>) =
    
    // Dictionary of the labels belonging to the vertices with (k,v) k-> vertex, v-> label.
    let labels : Dictionary<'Vertex,'Label> = labels
    // Dictionary of the edges belonging to the vertices with (k,v) k-> vertex, v-> edges.
    let vertexOutEdges :Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]> = vertexEdges

    new () = ArrayAdjacencyGraph(new Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>(), new Dictionary<'Vertex,'Label>())
        
    new(vertices : LVertex<'Vertex,'Label> list,edges : LEdge<'Vertex,'Edge> list) =
        let vertexArray = vertices  |> Array.ofList
        let edgeArray   = edges     |> Array.ofList
        
        let vertexEdges = System.Collections.Generic.Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>()
        let labelDict   = System.Collections.Generic.Dictionary<'Vertex,'Label>()

        for i=0 to vertexArray.Length-1 do
            let (vertex,label)  = vertexArray.[i]
            let edges           = Array.filter (fun (s, t, w) -> s=vertex || t=vertex) edgeArray         
            
            labelDict.Add (vertex,label)                                  
            vertexEdges.Add (vertex,edges)
        
        ArrayAdjacencyGraph(vertexEdges,labelDict)
       
    ///Returns true, since the graph model is always directed.
    member this.IsDirected :bool =
        true

    ///Returns true, since parallel edges are allowed
    member this.AllowParallelEdges :bool =
        true
                  
    ///Copys the given graph.
    member this.Copy() :ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        let copiedLabels = labels |> Dictionary.copyRecursive id
        let newEdges = vertexOutEdges |> Dictionary.copyRecursive Array.copy
        ArrayAdjacencyGraph(newEdges,copiedLabels)
    
    //
    //Edges
    ///Lookup the first edge in the graph that matches the conditions, returning a Some value if it exists and None if not.
    member this.TryGetEdge((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> option =
        Dictionary.tryGetValue source vertexOutEdges
        |> Option.bind (fun edges ->
            edges
            |> Array.tryFind (fun (s,t,e) -> s = source && t = target)
        )
             
    ///Return the first edge in the graph that matches the conditions.
    member this.GetEdge((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> =
        match (this.TryGetEdge(source,target)) with
            |Some x -> x
            |None   -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target

    ///Lookup all edges in the graph that matches the conditions, returning a Some value if it exists and None if not.
    member this.TryGetEdges((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> [] option=       
        Dictionary.tryGetValue source vertexOutEdges
        |> Option.map (fun edges ->
            edges
            |> Array.filter (fun (s,t,e) -> s = source && t = target)
        ) 
       
    ///Return all edges in the graph that matches the conditions
    member this.GetEdges((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        match (this.TryGetEdges(source,target)) with
        |Some x -> x
        |None   -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target

    ///Returns all edges of the graph.
    member this.Edges :LEdge<'Vertex,'Edge>[]=
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Value
            i <- i+1
        result
        |> Array.concat
        //|> Array.distinct

    ///Number of edges in the graph
    member this.EdgeCount    :int   = 
        this.Edges
        |> Array.length

    ///Returns true, if there are no edges in the graph, else false.
    member this.IsEdgesEmpty :bool =
        this.EdgeCount = 0
    
    ///Returns true, if the edge is found in the graph, else false.
    member this.ContainsEdge(edge:LEdge<'Vertex,'Edge>) :bool =            
        let (source,target,weight) = edge
        let edgeOptions = this.TryGetEdges(source,target)
           
        match edgeOptions with
        | Some x -> Array.contains(edge) x
        | None -> false

    ///Lookup all edges connected to the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetConnectedEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] option =
       Dictionary.tryGetValue v vertexOutEdges
       
    ///Lookup all edges connected to the vertex v in the graph, returning an array of connected edges.
    member this.GetConnectedEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        match (this.TryGetConnectedEdges(v)) with
        |Some x -> x
        |None   -> failwithf "The vertex %O does not exist in the graph." v

    ///Lookup all edges that target the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetInEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] option=      
        Dictionary.tryGetValue v vertexOutEdges
        |> Option.map (fun x -> 
            x
            |> Array.filter (fun (s, t, w) -> t=v)
        )

    ///Lookup all edges that target the vertex v in the graph, returning an array of connected edges.
    member this.GetInEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        match (this.TryGetInEdges(v)) with
        |Some x -> x
        |None   -> failwithf "The vertex %O does not exist in the graph." v

    ///Lookup all edges that originate from the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetOutEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] option=
        Dictionary.tryGetValue v vertexOutEdges    
        |> Option.map(fun x -> 
            x
            |> Array.filter (fun (s, t, w) -> s=v)
        )

    ///Lookup all edges that originate from the vertex v in the graph, returning an array of connected edges.
    member this.GetOutEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        match (this.TryGetOutEdges(v)) with
        |Some x -> x
        |None   -> failwithf "The vertex %O does not exist in the graph." v
    
    ///Lookup the weight of the edge defined by source and vertex. If it exists, return Some value, else none.
    member this.TryGetWeight((source:'Vertex),(target:'Vertex)) :'Edge option=
        this.TryGetEdge(source,target)     
        |> Option.map(fun (s, t, w) -> 
            w
        )

    ///Return the weight of the edge defined by source and vertex
    member this.GetWeight((source:'Vertex),(target:'Vertex)) :'Edge =
        match (this.TryGetEdge((source),(target))) with
        |Some x -> 
            x
            |> (fun (s, t, w) -> w) 
        |None   -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target

    //Problem, only returns first edge that equals the source and vertex
    ///Set the weight of the LEdge defined by source and vertex to be equal weight.
    member this.SetWeight((source:'Vertex),(target:'Vertex),(weight:'Edge)) =
        let sourceIndex = 
           match (Dictionary.tryGetValue source vertexOutEdges) with
           | Some x    -> 
                x 
                |> Array.findIndex (fun (s, t, w) -> t=target)
           | None      -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target
    
        let targetIndex = 
            match (Dictionary.tryGetValue target vertexOutEdges) with
            | Some x    -> 
                x 
                |> Array.findIndex (fun (s, t, w) -> s=source)
            | None      -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target
        
        vertexOutEdges.[source].[sourceIndex] <- (source,target,weight)
        vertexOutEdges.[target].[targetIndex] <- (source,target,weight)
        
        
        this
    
    ///Adds a labeled, edge to the graph.
    member this.AddEdge((s, t, w): LEdge<'Vertex,'Edge>) =
       let edgeArraySource = 
           match (Dictionary.tryGetValue s vertexOutEdges) with
               | Some x    -> x    
               | None      -> failwithf "The source vertex %O of the edge does not exist in this graph." s
             
       let edgeArrayTarget = 
           match (Dictionary.tryGetValue t vertexOutEdges) with
               | Some x    -> x                    
               | None      -> failwithf "The target vertex %O of the edge does not exist in this graph." t
   
       // Array.append benutzen
       vertexOutEdges.Item s <- (Array.concat [[|(s,t,w)|];edgeArraySource])
       vertexOutEdges.Item t <- (Array.concat [[|(s,t,w)|];edgeArrayTarget])
       this
    
    ///Adds an array of labeled, edges to the graph.
    member this.AddManyEdges edgeArray =
       for edge in edgeArray do
           this.AddEdge edge |> ignore 
       this
    
    ///Removes an edge from the graph.
    member this.RemoveEdge((s, t, w): LEdge<'Vertex,'Edge>) =
       let edgeArraySource = 
           match (Dictionary.tryGetValue s vertexOutEdges) with
           | Some x    -> 
                x 
                |> Array.filter (fun x -> x <> (s, t, w) )
           | None      -> failwith "The source vertex of the edge does not exist in this graph."
             
       let edgeArrayTarget = 
           match (Dictionary.tryGetValue t vertexOutEdges) with
           | Some x    -> 
                x 
                |> Array.filter (fun x -> x <> (s, t, w) )
           | None      -> failwith "The target vertex of the edge does not exist in this graph."
        
       vertexOutEdges.Item s <- (edgeArraySource)
       vertexOutEdges.Item t <- (edgeArrayTarget)
       this

    ///Removes an array of edges from the graph.
    member this.RemoveManyEdges (edges:LEdge<'Vertex,'Edge>[]) =
       for edge in edges do
           this.RemoveEdge edge |> ignore
       this
    
    //
    //Vertices
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
    member this.Vertices  :'Vertex[]=
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Key
            i <- i+1
        result
    
    ///Returns the degree of the vertex v.
    member this.Degree(v:'Vertex) :int =
        match (Dictionary.tryGetValue v vertexOutEdges) with
        |Some x -> 
            x 
            |> Array.length
        |None   -> failwithf "The vertex %O does not exist in the graph." v

    ///Returns the number of edges that originate from the vertex v.
    member this.InDegree((v:'Vertex)) =
        this.TryGetInEdges v
        |> Option.map (Array.length)

    ///Returns the number of edges that target the vertex v.
    member this.OutDegree((v:'Vertex)) =
        this.TryGetOutEdges v
        |> Option.map (Array.length)

    // Weight kann auch Union type sein -> get weight
    ///Returns the weighted degree of the vertex v.
    member this.WeightedDegree(weightingF : LEdge<'Vertex,'Edge> [] -> 'T,v:'Vertex) :'T =
        match (Dictionary.tryGetValue v vertexOutEdges) with
        |Some x -> 
            x 
            |> weightingF
        |None   -> failwithf "The vertex %O does not exist in the graph." v
   
    ///Returns true, if the vertex v does not have edges connected to it. Otherwise, it returns false.
    member this.IsOutEdgesEmpty(v:'Vertex) :bool =
        this.Degree(v) = 0

     ///Adds a labeled vertex to the graph.
    member this.AddVertex ((v, l): LVertex<'Vertex,'Label>)=
        vertexOutEdges.Add (v,[||])   |> ignore
        labels.Add (v, l)             |> ignore
        this

    ///Adds an array of labeled vertices to the graph.    
    member this.AddManyVertices (vertices:LVertex<'Vertex,'Label>[]) =       
        for vertex in vertices do
            this.AddVertex vertex |> ignore        
        this

    ///Removes a vertex from the graph.
    member this.RemoveVertex (v:'Vertex) =       
        vertexOutEdges.Item v 
        |> this.RemoveManyEdges 
        |> ignore
 
        vertexOutEdges.Remove (v) |> ignore
        labels.Remove (v)         |> ignore
        this

    ///Removes an array of vertices from the graph.
    member this.RemoveManyVertices (vertices:'Vertex[])  =
        for vertex in vertices do
            this.RemoveVertex vertex |> ignore
        this
 
    ///Returns Some vertices if they are predecessors of the vertex, else None.
    member this.TryPredecessors(v:'Vertex) :'Vertex[] option =
        this.TryGetConnectedEdges v
        |> Option.map (fun  x -> 
            x
            |> Array.choose (fun (s,t,w) -> if t=v then Some s else None)
            |> Array.distinct
        )
    
    ///Returns the preceding vertices of the vertex.
    member this.Predecessors(v:'Vertex) :'Vertex[] =
        match (this.TryPredecessors v) with
        |Some x -> x
        |None   -> failwithf "The vertex %O does not exist in the graph." v
    
    ///Returns Some vertices if they are successors of the vertex, else None.
    member this.TrySuccessors(v:'Vertex) :'Vertex[] option =
        this.TryGetConnectedEdges v
        |> Option.map (fun  x -> 
            x
            |> Array.choose (fun (s,t,w) -> if s=v then Some t else None)
            |> Array.distinct
        )

    ///Returns the succeeding vertices of the vertex.
    member this.Successors(v:'Vertex) :'Vertex[] =
        match (this.TrySuccessors v) with
        |Some x -> x
        |None   -> failwithf "The vertex %O does not exist in the graph." v

     ///Returns Some vertices if they are neighbours of the vertex, else None.
     member this.TryNeighbours(v:'Vertex) :'Vertex[] option =
        this.TryGetConnectedEdges v
        |> Option.map (fun  x -> 
            x
            |> Array.map (fun (s,t,w) -> if s = v then t else s)
            |> Array.distinct
        )
       
     ///Returns the neighbouring edges of the vertex v.
     member this.Neighbours(v:'Vertex) :'Vertex[] =
         match (this.TryNeighbours (v)) with
         |Some x -> x
         |None   -> failwithf "The vertex %O does not exist in the graph." v

    //
    //Label
    ///Returns Some label, if a label for the vertex v exists, else none.
    member this.TryGetLabel(v:'Vertex) :'Label option =        
        Dictionary.tryGetValue v labels
   
    ///Returns the label for the vertex v.
    member this.GetLabel(v:'Vertex) :'Label = 
        match (this.TryGetLabel(v)) with
            |Some x     -> x
            |None       -> failwithf "The vertex %O does not exist in the graph." v
   
    ///Sets the label for the vertex v.
    member this.SetLabel((v:'Vertex),(l:'Label)) = 
        labels.Item v <- (l)
        this

    ///Returns all labels of the graph.
    member this.Labels :'Label []=
        let result = Array.zeroCreate labels.Count
        let mutable i = 0
        for group in labels do
            result.[i] <- group.Value
            i <- i+1
        result       
