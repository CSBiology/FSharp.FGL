namespace FSharp.FGL.ArrayAdjacencyGraph

open System.Collections.Generic
open System

module Measures = 
//Much of the logic is taken from the measure formulas detailed in the cytoscape documentation here 
//https://med.bioinf.mpi-inf.mpg.de/netanalyzer/help/2.7/index.html#figure7

    let private infinity = Double.PositiveInfinity 

    // dijkstra implementation is based on and extended from the psuedo code here https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
    // Didnt rewrite with recursion and immutability for performance and to keep it close to the psuedo code 
    let private dijkstra(graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) 
                        (edgeFinder: 'Vertex -> 'Vertex array) // used to filter directed/undirected edges
                        (weightCalc: 'Vertex * 'Vertex -> float ) // can be controled for unweighted paths or domain specific use cases.
                        (source: 'Vertex) = 

        let q = new ResizeArray<'Vertex>()
        let dist = new Dictionary<'Vertex, float>()

        graph.GetVertices()
        |> Array.iter(fun v ->   
            q.Add v
            if v <> source then 
                dist.Add(v, infinity) 
            else dist.Add(v, 0.0) )
    
        while q.Count > 0 do
            dist
            |> Seq.filter(fun (KeyValue(v,d)) -> q.Contains v)
            |> Seq.minBy(fun (KeyValue(_ ,d)) -> d)
            |> fun (KeyValue(v,_)) -> 
                q.Remove v |> ignore
                edgeFinder v
                |> Array.iter(fun n -> 
                    let alt = dist.[v] + (weightCalc (v, n))
                    if alt < dist.[n] then dist.[n] <- alt; 
                    ) 
        dist
        |> Seq.map(fun (KeyValue(v,d)) -> v, if d = infinity then None else Some d)
        |> Map 

    // all the weighted functions require Edge to be a float
    let private getWeight (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) (v, n) = 
        match graph.TryGetWeight(v, n) with
                | Some w -> w
                | None -> infinity 

    let private meanShortestPathBase (vertexs: 'Vertex array) (fn: 'Vertex -> Map<'Vertex,option<float>> ) = 
        vertexs
        |> Seq.map(fun v ->  
            fn v
            |> Map.toSeq
            |> Seq.choose(fun (_,v) -> v)
        )
        |> Seq.concat
        |> Seq.filter (fun v -> v > 0.0)
        |> Seq.average

    /// Returns a Some of the undirected shortest path from source to target vertices, else None.         
    let tryGetShortestPath (graph: ArrayAdjacencyGraph<'Vertex,'Label, 'Edge>) (source: 'Vertex) (target: 'Vertex) = 
        (dijkstra graph graph.Neighbours (fun (n, v) -> 1.0) source).[target]

    /// Returns a Some of the outward directed shortest path from source to target vertices, else None.       
    let tryGetShortestPathDirected (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) (target: 'Vertex) = 
        (dijkstra graph graph.Successors (fun (n, v) -> 1.0) source).[target]
    
    /// Returns a Some of the sum of edge weights along the outward directed shortest path from source to target vertices, else None.    
    let tryGetShortestPathDirectedhWeighted (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) (source: 'Vertex) (target: 'Vertex) = 
        (dijkstra graph graph.Successors (getWeight graph) source).[target]

    /// Returns the average of all the undirected shortest paths between connected vertices in the graph
    let meanShortestUnDirected (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        meanShortestPathBase (graph.GetVertices()) (dijkstra graph graph.Neighbours (fun (_, _) -> 1.0))
    
    /// Returns the average of all the directed shortest paths between connected vertices in the graph
    let meanShortestPathDirected (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        meanShortestPathBase (graph.GetVertices()) (dijkstra graph graph.Successors (fun (_, _) -> 1.0))

    /// Returns the average of all the summed weights on directed edges on shortest paths between connected vertices in the graph
    let meanShortestPathDirectedhWeighted (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) =
        meanShortestPathBase (graph.GetVertices()) (dijkstra graph graph.Successors (getWeight graph))
   
    let private meanShortestPathVertexBase (paths: Map<'Vertex,option<float>>) =
        paths
        |> Map.toSeq
        |> Seq.choose(fun (_,v) -> v)
        |> Seq.filter (fun v -> v > 0.0)
        |> Seq.average

    //Averages Shortest Paths
    //  Currently lib doesnt seem to suppor weighted undirected nerworks so have left out functions to cover these. 
    //  Support would require a new version of the getEdge functions.

    /// Returns the average of all the shortest paths from the source vertex to the connected vertices
    let meanShortestPathUnDirectedVertex (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) =
        (dijkstra graph graph.Neighbours (fun (_, _) -> 1.0) source)
        |> meanShortestPathVertexBase

    /// Returns the average of all the outward directed shortest paths from the source vertex to the connected vertices
    let meanShortestPathDirectedVertex (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) =
        (dijkstra graph graph.Successors (fun (_, _) -> 1.0) source)
        |> meanShortestPathVertexBase

     /// Returns the average of all the summed weights on outward directed edges on shortest paths from the source vertex to the connected vertices
    let meanShortestPathDirectedhWeightedVertex (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) (source: 'Vertex) =
        (dijkstra graph graph.Successors (getWeight graph) source)
        |> meanShortestPathVertexBase
      
    // Closeness
    let private getClosenessBase (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) 
                    (source: 'Vertex) 
                    (edgeFinder: 'Vertex -> 'Vertex array) = 
        dijkstra graph edgeFinder (fun (_, _) -> 1.0) source
        |> Map.toSeq
        |> Seq.choose(fun (k,v) -> v)
        |> Seq.filter (fun v -> v > 0.0)
        |> fun v -> 
            1.0 / (v |> Seq.average) 
    
    /// Returns closeness centrality of the source vertex
    let getClosenessUnDirected (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) =
        getClosenessBase graph source (graph.Neighbours)

    /// Returns outward directed closeness centrality of the source vertex
    let getClosenessOutward (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) =
        getClosenessBase graph source (graph.Successors)

    /// Returns inward directed closeness centrality of the source vertex
    let getClosenessInward (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) =
        getClosenessBase graph source (graph.Predecessors)

    /// Returns Neighborhood Connectivity as defined in cytoscape documentation for source vertex
    let getNeighborhoodConnectivity(graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) =
        graph.Neighbours source
        |> Seq.map(fun v -> graph.Degree v |> float)
        |> Seq.average

    // Clustering Coeffcient 
    let rec private combinations acc size set = seq {
        match size, set with 
        | n, x::xs -> 
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs 
        | 0, [] -> yield acc 
        | _, [] -> () }

    /// Returns Clustering Coeffcient as defined in cytoscape documentation for source vertex
    let getClusteringCoefficient (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (source: 'Vertex) =
        graph.Neighbours source
        |> Array.toList
        |> combinations [] 2
        |> Seq.map(fun l -> (l|> List.head), (l |> List.last))
        |> Seq.map(fun (v1, v2) -> 
            if graph.TryGetEdge(v1, v2).IsSome || (graph.TryGetEdge(v2, v1)).IsSome then 1.0 else 0.0 // ugly, lib needs a undirected TryGetEdge
            )
        |> fun s ->  (s |> Seq.sum) /(s |> Seq.length |> float)