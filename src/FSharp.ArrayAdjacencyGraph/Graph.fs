﻿namespace FSharp.FGL.ArrayAdjacencyGraph

open System.Collections.Generic
open FSharp.FGL


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


///  A mutable directed graph data structure efficient for large sparse graph representations
//  Adaptation of https://github.com/YaccConstructor/QuickGraph.
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
            let edges           = 
                //Array.filter (fun (s, t, w) -> s=vertex || t=vertex) edgeArray         
                edgeArray
                |> Array.filter (fun (s, t, w) -> s=vertex || t=vertex) 
                //|> Array.map (fun (s,t,w) -> 
                //    if s=t then [|(s,t,w);(s,t,w)|]
                //    else [|(s,t,w)|])
                //|> Array.concat

            labelDict.Add (vertex,label)                                  
            vertexEdges.Add (vertex,edges)
        
        ArrayAdjacencyGraph(vertexEdges,labelDict)
       
    ///Copys the given graph.
    member this.Copy() :ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        let copiedLabels = labels |> Dictionary.copyRecursive id
        let newEdges = vertexOutEdges |> Dictionary.copyRecursive Array.copy
        ArrayAdjacencyGraph(newEdges,copiedLabels)
    
    member internal this.LabelMap() = labels

    member internal this.AdjacencyGraph() = vertexOutEdges

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
        try 
            vertexOutEdges.Item source
            |> Array.find (fun (s,t,e) -> s = source && t = target)
        with
        | _ -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target

    ///Lookup all edges in the graph that matches the conditions, returning a Some value if it exists and None if not.
    member this.TryGetEdges((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> [] option=       
        Dictionary.tryGetValue source vertexOutEdges
        |> Option.map (fun edges ->
            edges
            |> Array.filter (fun (s,t,e) -> s = source && t = target)
        ) 
       
    ///Return all edges in the graph that matches the conditions
    member this.GetEdges((source:'Vertex),(target:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        try 
            vertexOutEdges.Item source
            |> Array.filter (fun (s,t,e) -> s = source && t = target)
        with
        |_ -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target


    ///Returns all edges of the graph.
    member this.GetEdges() :LEdge<'Vertex,'Edge>[]=
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Value
            i <- i+1
        result
        |> Array.concat
        |> Array.distinct

    ///Number of edges in the graph
    member this.EdgeCount    : int   = 
        let mutable i = 0
        for group in vertexOutEdges do           
            i <- i + group.Value.Length
        i / 2

    /////Returns true, if there are no edges in the graph, else false.
    //member this.IsEdgesEmpty :bool =
    //    this.EdgeCount = 0
    
    ///Returns true, if the edge is found in the graph, else false.
    member this.ContainsEdge(edge:LEdge<'Vertex,'Edge>) :bool =            
        let (source,target,weight) = edge
        try 
            let array = vertexOutEdges.Item source
            (Array.contains (edge) array) || (Array.contains (target,source,weight) array)
        with 
        |_ -> false


    ///Lookup all edges connected to the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetConnectedEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] option =
       Dictionary.tryGetValue v vertexOutEdges
       
    ///Lookup all edges connected to the vertex v in the graph, returning an array of connected edges.
    member this.GetConnectedEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        try
           vertexOutEdges.Item v
        with
        | _ -> failwithf "The vertex %O does not exist in the graph." v


    ///Lookup all edges that target the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetInEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] option=      
        Dictionary.tryGetValue v vertexOutEdges
        |> Option.map (fun x -> 
            x
            |> Array.filter (fun (s, t, w) -> t=v)
        )

    ///Lookup all edges that target the vertex v in the graph, returning an array of connected edges.
    member this.GetInEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        try
            vertexOutEdges.Item v
            |> Array.filter (fun (s, t, w) -> t=v)
        with
        |_ -> failwithf "The vertex %O does not exist in the graph." v


    ///Lookup all edges that originate from the vertex v in the graph, returning a Some value if a binding exists and None if not.
    member this.TryGetOutEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] option=
        Dictionary.tryGetValue v vertexOutEdges    
        |> Option.map(fun x -> 
            x
            |> Array.filter (fun (s, t, w) -> s=v)
        )

    ///Lookup all edges that originate from the vertex v in the graph, returning an array of connected edges.
    member this.GetOutEdges((v:'Vertex)) :LEdge<'Vertex,'Edge> [] =
        try
            vertexOutEdges.Item v
            |> Array.filter (fun (s, t, w) -> s=v)
        with
        |_ -> failwithf "The vertex %O does not exist in the graph." v


    ///Lookup the weight of the edge defined by source and vertex. If it exists, return Some value, else none.
    member this.TryGetWeight((source:'Vertex),(target:'Vertex)) :'Edge option=
        this.TryGetEdge(source,target)     
        |> Option.map(fun (s, t, w) -> 
            w
        )

    ///Return the weight of the edge defined by source and vertex
    member this.GetWeight((source:'Vertex),(target:'Vertex)) :'Edge =
        try
            this.GetEdge (source,target)
            |> (fun (s, t, w) -> w)
        with
        |_ -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target


    //Problem, only returns first edge that equals the source and vertex
    ///Set the weight of the LEdge defined by source and vertex to be equal weight.
    member this.SetWeight((source:'Vertex),(target:'Vertex),(weight:'Edge)) =
        let sourceIndex = 
            try
                vertexOutEdges.Item source
                |> Array.findIndex (fun (s, t, w) -> t=target)
            with
            |_ -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target

        let targetIndex = 
            try
                vertexOutEdges.Item target
                |> Array.findIndex (fun (s, t, w) -> s=source)
            with
            |_ -> failwithf "An edge from vertex %O to vertex %O does not exist in the graph" source target

        vertexOutEdges.[source].[sourceIndex] <- (source,target,weight)
        vertexOutEdges.[target].[targetIndex] <- (source,target,weight)       
        
        this
    
    ///Adds a labeled, edge to the graph.
    member this.AddEdge((s, t, w): LEdge<'Vertex,'Edge>) =

        let edgeArraySource = 
            try
                vertexOutEdges.Item s
            with 
            |_ -> failwithf "The source vertex %O of the edge does not exist in this graph." s

        let edgeArrayTarget = 
            try
                vertexOutEdges.Item t
            with
            |_ -> failwithf "The target vertex %O of the edge does not exist in this graph." t

        
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
        if s <> t then 
            let edgeArraySource = 
                try 
                    vertexOutEdges.Item s
                    |> Array.filter (fun x -> x <> (s, t, w) )
                with
                |_ -> failwithf "The source vertex %O of the edge does not exist in this graph." s

            let edgeArrayTarget = 
                try
                    vertexOutEdges.Item t
                    |> Array.filter (fun x -> x <> (s, t, w) )
                with
                |_ -> failwithf "The target vertex %O of the edge does not exist in this graph." t        
        
            vertexOutEdges.Item s <- (edgeArraySource)
            vertexOutEdges.Item t <- (edgeArrayTarget)

        else
            let edgeArraySource = 
                try 
                    vertexOutEdges.Item s
                    |> Array.filter (fun x -> x <> (s, t, w) )
                with
                |_ -> failwithf "The source vertex %O of the edge does not exist in this graph." s 

            vertexOutEdges.Item s <- (edgeArraySource)

        this

    ///Removes an array of edges from the graph.
    member this.RemoveManyEdges (edges:LEdge<'Vertex,'Edge>[]) =
       for edge in edges do
           this.RemoveEdge edge |> ignore
       this
    
    //Vertices
    ///Returns true, if the graph contains the vertex, else false.
    member this.ContainsVertex(vertex:'Vertex) :bool =
        vertexOutEdges.ContainsKey(vertex)

    /////Returns true, if there are no vertices in the graph, else false.
    //member this.IsVerticesEmpty :bool =
    //    vertexOutEdges.Count = 0

    ///Returns the number of vertices of the graph.
    member this.VertexCount :int =
        vertexOutEdges.Count

    ///Returns the vertices of the graph.
    member this.GetVertices()  :'Vertex[]=
        let result = Array.zeroCreate vertexOutEdges.Count
        let mutable i = 0
        for group in vertexOutEdges do
            result.[i] <- group.Key
            i <- i+1
        result
    
    ///Returns the degree of the vertex v.
    member this.Degree(v:'Vertex) :int =
        try
           vertexOutEdges.Item v
           //|> Array.length
           |> Array.sumBy (fun (s,t,w) -> if s=t then 2 else 1)
        with
        |_ -> failwithf "The vertex %O does not exist in the graph." v

    ///Returns the number of edges that originate from the vertex v.
    member this.InDegree((v:'Vertex)) =
        //this.TryGetInEdges v
        //|> Option.map (Array.length)
        this.GetInEdges v
        |> Array.length
        
    ///Returns the number of edges that target the vertex v.
    member this.OutDegree((v:'Vertex)) =
        //this.TryGetOutEdges v
        //|> Option.map (Array.length)
        this.GetOutEdges v
        |> Array.length

    ///Returns the weighted degree of the vertex v.
    member this.WeightedDegree(weightingF : 'Edge -> float,v:'Vertex) :float =
        try
            vertexOutEdges.Item v
            |> Array.sumBy (fun (s,t,w) -> if s = t then (weightingF w) * 2. else weightingF w)
            //|> weightingF
        with
        | _ -> failwithf "The vertex %O does not exist in the graph." v

    ///Returns true, if the vertex v does not have edges connected to it. Otherwise, it returns false.
    member this.ConnectedEdgesEmpty(v:'Vertex) :bool =
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
        try
            this.GetConnectedEdges v
            |> Array.choose (fun (s,t,w) -> if t=v then Some s else None)
            |> Array.distinct
        with
        |_ -> failwithf "The vertex %O does not exist in the graph." v

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
        try
            this.GetConnectedEdges v
            |> Array.choose (fun (s,t,w) -> if s=v then Some t else None)
            |> Array.distinct
        with
        |_ -> failwithf "The vertex %O does not exist in the graph." v

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
        try
            this.GetConnectedEdges v
            |> Array.map (fun (s,t,w) -> if s = v then t else s)
            |> Array.distinct
        with
        |_ -> failwithf "The vertex %O does not exist in the graph." v

    //Label
    ///Returns Some label, if a label for the vertex v exists, else none.
    member this.TryGetLabel(v:'Vertex) :'Label option =        
        Dictionary.tryGetValue v labels
   
    ///Returns the label for the vertex v.
    member this.GetLabel(v:'Vertex) :'Label = 
        try 
            labels.Item v            
        with
        | _ -> failwithf "The vertex %O does not exist in the graph." v

    ///Sets the label for the vertex v.
    member this.SetLabel((v:'Vertex),(l:'Label)) = 
        labels.Item v <- (l)
        this

    ///Returns all labels of the graph.
    member this.GetLabels() :'Label []=
        let result = Array.zeroCreate labels.Count
        let mutable i = 0
        for group in labels do
            result.[i] <- group.Value
            i <- i+1
        result       

module Graph =
    ///Create an ArrayAdjacencyGraph based on the given vertex list and edge list.
    let create (vertexList : LVertex<'Vertex,'Label> list) (edgeList : LEdge<'Vertex,'Edge> list) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        ArrayAdjacencyGraph(vertexList,edgeList)

module Vertices =
    //Vertices
    ///Returns true, if the graph contains the vertex, else false.
    let contains (vertex:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :bool =
        graph.ContainsVertex vertex

    ///Returns the number of vertices of the graph.
    let count (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :int =
        graph.VertexCount

    ///Returns the vertices of the graph.
    let toVertexList (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Vertex[]=
        graph.GetVertices()
    
    ///Returns the degree of the vertex v.
    let degree (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :int =
        graph.Degree v

    ///Returns the number of edges that originate from the vertex v.
    let inDegree (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :int =
        graph.InDegree v
        
    ///Returns the number of edges that target the vertex v.
    let outDegree (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :int =
        graph.OutDegree v

    ///Returns the weighted degree of the vertex v.
    let weightedDegree (weightingF : 'Edge -> float) (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :float =
        graph.WeightedDegree (weightingF,v)

    ///Returns false if the vertex v does not have edges connected to it. Otherwise, it returns true.
    let isConnectedToEdges (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :bool =
        not (graph.ConnectedEdgesEmpty v)

    ///Returns a new graph with the given vertex added.
    let add ((v, l): LVertex<'Vertex,'Label>) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        (graph.Copy()).AddVertex(v, l)

    ///Returns a new graph with the given vertices added.    
    let addMany (vertices:LVertex<'Vertex,'Label>[]) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =       
        (graph.Copy()).AddManyVertices(vertices)

    ///Returns a new graph where the given vertex is removed.
    let remove (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =       
        (graph.Copy()).RemoveVertex(v)

    ///Returns a new graph where the given vertices are removed.
    let removeMany (vertices:'Vertex[]) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        (graph.Copy()).RemoveManyVertices(vertices)
 
    ///Returns Some vertices if they are predecessors of the vertex, else None.
    let tryPredecessors (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Vertex[] option =
        graph.TryPredecessors v
    
    ///Returns the preceding vertices of the vertex.
    let predecessors (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Vertex[] =
        graph.Predecessors v

    ///Returns Some vertices if they are successors of the vertex, else None.
    let trySuccessors (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Vertex[] option =
        graph.TrySuccessors v

    ///Returns the succeeding vertices of the vertex.
    let successors (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Vertex[] =
        graph.Successors v

    ///Returns Some vertices if they are neighbours of the vertex, else None.
    let tryNeighbours (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Vertex[] option =
        graph.TryNeighbours v
       
    ///Returns the neighbouring edges of the vertex v.
    let neighbours (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Vertex[] =
        graph.Neighbours v
      

    //Label
    ///Returns Some label, if a label for the vertex v exists, else none.
    let tryGetLabel (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Label option =        
        graph.TryGetLabel v
      
    ///Returns the label for the vertex v.
    let getLabel (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Label = 
        graph.GetLabel v

    ///Retunrs a new graph where the label for the vertex v is the given label.
    let setLabel (v:'Vertex) (l:'Label) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :ArrayAdjacencyGraph<'Vertex,'Label,'Edge> = 
        (graph.Copy()).SetLabel(v,l)

    ///Returns all labels of the graph.
    let getLabelList (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Label []=
        graph.GetLabels()    

    /////Returns modularity based on Louvain
    //let modularity(resolution: float) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : float =
    //    let totalWeight :float =
    //        let result = Array.zeroCreate (graph.AdjacencyGraph()).Count
    //        let mutable i = 0
    //        for group in graph.AdjacencyGraph() do
    //            result.[i] <- group.Value
    //            i <- i+1
    //        result
    //        |> Array.concat
    //        |> Array.sumBy (fun (source,target,weight) -> (weight))

    //    let communitySumtotalSumintern :Dictionary<'Label,float*float> =
    //        let output = System.Collections.Generic.Dictionary<'Label,float*float>() 
    //        let labels = graph.GetLabels()|> Array.distinct

    //        for i in labels do
    //            output.Add(i,(0.,0.))
    //        output

    //    for vertex in (graph.GetVertices()) do
    //        let label =
    //            graph.GetLabel vertex

    //        let connectedEdges =
    //            graph.GetConnectedEdges vertex

    //        let ki =
    //            connectedEdges
    //            |> Array.map(fun (s, t, w) ->
    //                if s=vertex then (t,w)
    //                else (s,w))
    //            |> Array.sumBy snd

    //        let selfLoops =                                                
    //            connectedEdges
    //            |>Array.sumBy(fun (s,t,w) -> if s=vertex&&t=vertex then w/2. else 0.) 


    //        let (kiNow,selfLoopsNow) =
    //            Dictionary.getValue label communitySumtotalSumintern

    //        communitySumtotalSumintern.Item label <- ((kiNow+ki),(selfLoopsNow+selfLoops))

    //    let mutable q = 0.
    //    for i in communitySumtotalSumintern do
    //        let (totalSumC,sumIntern) = i.Value
    //        if totalSumC > 0. then 
    //            let calculation = resolution*((sumIntern/2.)/(totalWeight/2.))-((totalSumC/totalWeight)**2.)
    //            q <- (q+(calculation))
    //    q

    //let mod2 (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : float =
    //    let totalW =
    //        (
    //            [|
    //                for i in graph.GetVertices() do
    //                    //printfn "vertex %A value %A " i (graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w))),i))
    //                    graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (*if s=t then (w/2.) else*) (w))),i)
                        
    //                    //graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w/2.))),i)

    //            |]
    //            |> Array.sum
    //        )/2.

    //    //printfn "totalw = %A" totalW
        
    //    //for i in graph.GetVertices() do
    //        //printfn "vertex %A value %A " i (graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w))),i))

    //    let calculation (vertex: 'Vertex) =
    //        //Limit the calculation to connected Verties
    //        let connectedEdges =
    //            graph.GetConnectedEdges(vertex)

    //        let edgesMinimal =
    //            connectedEdges
    //            |> Array.map (fun (s,t,w) -> if s = vertex then (t,w) else (s,w))

    //        let ki (i:'Vertex) =
    //            graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> w)),i)

    //        let k =
    //            ki vertex

    //        let community =
    //            graph.GetLabel vertex

    //        //Calculate the weight of the edge between the source vertex and the 
    //        let AVv2 (v2:'Vertex) =
    //            if Array.contains v2 (graph.Neighbours(vertex)) then

    //                edgesMinimal
    //                |> Array.filter(fun (v,w) -> (v=v2))
    //                |> Array.sumBy snd
    //            else
    //                0.

    //        let mutable sum = 0.

    //        for v2 in graph.GetVertices() do
    //            if community=(graph.GetLabel(v2)) then
    //                let newSum = sum + ((AVv2 v2) - ((k*(ki v2))/(2.*totalW)))
    //                sum <- newSum
    //        sum

    //    let allCalculations =
    //        [|   
    //            for i in graph.GetVertices() do
    //                //printfn "vertex %A value %A" i (calculation i)
    //                calculation i
    //        |]
    //        |> Array.sum

    //    //printfn "all calculations is %A"allCalculations

    //    (1./(2.*totalW))*allCalculations

    //let mod3 (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : float =
    //    let totalW =
    //        [|
    //            for i in graph.GetVertices() do
    //                //printfn "vertex %A value %A " i (graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w))),i))
    //                graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (*if s=t then (w/2.) else*) (w))),i)
                        
    //                //graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w/2.))),i)

    //        |]
    //        |> Array.sum

    //    printfn "total Weight =%A" totalW

    //    let tot i =
    //        graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w))),i)

    //    let In i =
    //        graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s=t then w else 0.)),i)

    //    let mutable q = 0. 

    //    for i in graph.GetVertices() do
    //        if (tot i) > 0. then
    //            let value = q
    //            //printfn "tot %A; in %A" (tot i) (In i)
    //            //printfn "calculation %A " ((In i) - ((tot i)*(tot i)) / totalW)
    //            q <- ((In i) - ((tot i)*(tot i)) / totalW)+value

    //    (q/totalW)

    //let mod4 (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : float =
    //    let totalW =      
    //            [|
    //                for i in graph.GetVertices() do
    //                    graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s = t then (w/2.) else w)),i)
    //                    //graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w))),i)

    //            |]
    //            |> Array.sum

    //    printfn "total Weight =%A" totalW

    //    let tot i =
    //        graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s=t then (w/2.) else w)),i)
            
    //    let In i =
    //        graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s=t then (w/2.) else 0.)),i)
            
    //    let mutable q = 0. 

    //    for i in graph.GetVertices() do
    //        if (tot i) > 0. then
    //            printfn "vertex: %A; tot %A; In: %A" i (graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s=t then (w/2.) else w)),i)) (graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s=t then (w/2.) else 0.)),i))
    //            q <- (q+((In i) - ((tot i)*(tot i)) / totalW))

    //    (q/totalW)
        
module Edges =
    //Edges
    ///Lookup the first edge in the graph that matches the conditions, returning a Some value if it exists and None if not.
    let tryGet (source:'Vertex) (target:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> option =
        graph.TryGetEdge(source,target)
             
    ///Return the first edge in the graph that matches the conditions.
    let get (source:'Vertex) (target:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> =
        graph.GetEdge(source,target)

    ///Lookup all edges in the graph that matches the conditions, returning a Some value if it exists and None if not.
    let tryGetMany (source:'Vertex) (target:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] option=       
        graph.TryGetEdges(source,target)
       
    ///Return all edges in the graph that matches the conditions
    let getMany (source:'Vertex) (target:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] =
        graph.GetEdges(source,target)

    ///Returns all edges of the graph.
    let toEdgeList (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge>[]=
        graph.GetEdges()

    ///Number of edges in the graph
    let count (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : int   = 
        graph.EdgeCount
     
    ///Returns true if the edge is found in the graph, else false.
    let contains (edge:LEdge<'Vertex,'Edge>) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :bool =            
        graph.ContainsEdge edge

    ///Lookup all edges connected to the vertex v in the graph, returning a Some value if a binding exists and None if not.
    let tryGetConnected (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] option =
       graph.TryGetConnectedEdges v
       
    ///Lookup all edges connected to the vertex v in the graph, returning an array of connected edges.
    let getConnected (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] =
        graph.GetConnectedEdges v

    ///Lookup all edges that target the vertex v in the graph, returning a Some value if a binding exists and None if not.
    let tryGetInEdges (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] option=      
        graph.TryGetInEdges v

    ///Lookup all edges that target the vertex v in the graph, returning an array of connected edges.
    let getInEdges (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] =
        graph.GetInEdges v

    ///Lookup all edges that originate from the vertex v in the graph, returning a Some value if a binding exists and None if not.
    let tryGetOutEdges (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] option=
        graph.TryGetOutEdges v

    ///Lookup all edges that originate from the vertex v in the graph, returning an array of connected edges.
    let getOutEdges (v:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :LEdge<'Vertex,'Edge> [] =
        graph.GetOutEdges v

    ///Lookup the weight of the edge defined by source and vertex. If it exists, return Some value, else none.
    let tryGetWeight (source:'Vertex) (target:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Edge option=
        graph.TryGetWeight(source,target)

    ///Return the weight of the edge defined by source and vertex
    let getWeight (source:'Vertex) (target:'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) :'Edge =
        graph.GetWeight(source,target)

    //Problem, only returns first edge that equals the source and vertex
    ///Return a new graph where the weight for the given edge was adapted.
    let setWeight ((source:'Vertex),(target:'Vertex),(weight:'Edge)) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        (graph.Copy()).SetWeight(source,target,weight)
  
    ///Returns a new graph where the given edge was added.
    let add ((s, t, w) : LEdge<'Vertex,'Edge>) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        (graph.Copy()).AddEdge(s,t,w)
    
    ///Returns a new graph where the given edges were added.
    let addMany edgeArray (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
       (graph.Copy()).AddManyEdges(edgeArray)
    
    ///Returns a new graph where the given edge was removed from the original graph.
    let remove ((s, t, w): LEdge<'Vertex,'Edge>) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
        (graph.Copy()).RemoveEdge(s, t, w)

    ///Returns a new graph where the given edges were removed from the original graph.
    let removeMany (edges:LEdge<'Vertex,'Edge>[]) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : ArrayAdjacencyGraph<'Vertex,'Label,'Edge> =
       (graph.Copy()).RemoveManyEdges(edges)
