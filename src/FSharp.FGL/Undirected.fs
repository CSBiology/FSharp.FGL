namespace FSharp.FGL.Undirected

open System
open Aether
open FSharp.FGL

///Functions for vertices of undirected Graphs
module Vertices = 



    (* Neighbours *)

    ///Lists the vertices which are connected to the vertex.
    let neighbours (context:Context<'Vertex,'Label,'Edge>) : 'Vertex list =
        context
        |> fun (_, _, _, s) -> List.map fst s


    (* Properties *)

    ///Evaluates the number of edges associated with the vertex.
    let degree (context:Context<'Vertex,'Label,'Edge>) : int =
        context
        |> fun (_, _, _, s) -> List.length s

    ///Evaluates the clustering coefficient of the vertex.
    let clusteringCoefficient (context:Context<'Vertex,'Label,'Edge>) (g: Graph<'Vertex,'Label,'Edge>) : float=
        context
        |> fun c ->     
            if degree c < 2 then 0.
            else        
                let add1IfInList acc x set = 
                    if List.contains x set then acc + 1
                    else acc
                let neighbours = neighbours c
                let neighbourEdges = 
                    List.fold (fun edgeAmount v' -> 
                        (Graph.getContext v' g
                        |> fun (p',_,_,_) ->
                            (p'
                            |> List.fold (fun acc (x,_) -> add1IfInList acc x neighbours) 0))
                        + edgeAmount
                    ) 0 neighbours
                let degree = List.length neighbours
                (float neighbourEdges) / (float (degree * (degree - 1)))


    (* General *)

    ///Creates a list of all vertices and their labels.
    [<Obsolete("Corrected Typo in function name: Use toVertexList instead")>]
    let tovertexList (g:Graph<'Vertex,'Label,'Edge>) : LVertex<'Vertex,'Label> list=
            Map.toList g
            |> List.map (fun (v, (_, l, _)) -> v, l)


///Functions for edges of undirected Graphs
 module Edges =
     (* Add and remove *)

    ///Adds a labeled, undirected edge to the graph.
    let add ((v1, v2, edge): LEdge<'Vertex,'Edge>) (g: Graph<'Vertex,'Label,'Edge>) =
        let g1 = 
            let composedPrism = Compose.prism (Map.key_ v1) Lenses.msucc_
            let adjListMapping = Map.add v2 edge
            (Optic.map composedPrism adjListMapping) g
        let composedPrism = Compose.prism (Map.key_ v2) Lenses.msucc_
        let adjListMapping = Map.add v1 edge
        (Optic.map composedPrism adjListMapping) g1

    ///Adds a list of labeled, undirected edges to the graph.
    let addMany (edges : list<LEdge<'Vertex,'Edge>>) (g: Graph<'Vertex,'Label,'Edge>) =
        List.fold (fun g e -> add e g) g edges

    ///Removes an edge from the graph.
    let remove ((v1, v2): Edge<'Vertex>) (g: Graph<'Vertex,'Label,'Edge>) =
        let g1 = 
            let composedPrism = Compose.prism (Map.key_ v1) Lenses.msucc_
            let adjListMapping = Map.remove v2
            (Optic.map composedPrism adjListMapping) g
        let composedPrism = Compose.prism (Map.key_ v2) Lenses.msucc_
        let adjListMapping = Map.remove v1
        (Optic.map composedPrism adjListMapping) g1
                
    ///Removes a list of edges from the graph.
    let removeMany (edges : list<Edge<'Vertex>>) (g: Graph<'Vertex,'Label,'Edge>) =
        List.fold (fun g e -> remove e g) g edges

    (* Properties *)

    ///Evaluates the number of edges in the graph.
    let count (g: Graph<'Vertex,'Label,'Edge>) : int = 
            Map.toArray g
            |> Array.fold (fun c (_,(_,_,s)) -> c + ((Map.toList s) |> List.length)) 0
            |> fun x -> x / 2


    (* General *)

    ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
    let contains v1 v2 (g: Graph<'Vertex,'Label,'Edge>) : bool =
        Map.tryFind v1 g
        |> Option.bind (fun (_, _, s) -> Map.tryFind v2 s)
        |> Option.isSome

    ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    let find (v1:'Vertex) (v2:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> =
            Map.find v1 g
            |> fun (_, _, s) -> Map.find v2 s
            |> fun e -> (v1,v2,e)

    ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    let tryFind (v1:'Vertex) (v2:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> option =
            Map.tryFind v1 g
            |> Option.bind (fun (_, _, s) -> Map.tryFind v2 s)
            |> Option.map (fun e -> (v1,v2,e))
    
    ///Creates a list of all edges and their labels.
    let toEdgeList (g:Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> list = 
        let sourceList = 
            g
            |> Graph.mapContexts (fun (p,v,l,s) -> s)
            |> Map.toList
            |> List.collect (fun (v,es) -> 
                es
                |> List.map (fun (v2,e) -> v,v2,e))
        List.fold (fun acc sL -> 
            match List.contains ((fun (a,b,c) -> (b,a,c)) sL) acc with
            |true -> acc
            |false ->sL::acc) [] sourceList
     
     
    (* Iterative *)

    ///Maps edgelabels of the graph.
    let map (mapping: 'Vertex -> 'Vertex -> 'Edge -> 'REdge) (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'REdge>=
            g
            |> Map.map (fun vertex (p, l, s) -> 
                Map.map (fun pvertex edge -> mapping pvertex vertex edge) p,
                l,
                Map.map (fun svertex edge -> mapping vertex svertex edge) s)  

    ///Performs a given function on every edge of the graph.                
    let iter (action: 'Vertex -> 'Vertex -> 'Edge -> unit) (graph:Graph<'Vertex,'Label,'Edge>) : unit =
        let rec recurse g =
            match Graph.decomposeFirst g with
            | (Some c,g') -> 
                c
                |> fun (_,v,_,s) -> 
                    List.iter (fun (v',e) -> action v v' e) s
                    recurse g'
            | (None,_) -> ()
        recurse graph   
               
    ///Performs a given function on every edge of the graph, which also receives an ascending integer index.                                    
    let iteri (action: int -> 'Vertex -> 'Vertex -> 'Edge -> unit) (graph:Graph<'Vertex,'Label,'Edge>) : unit =
        let rec recurse i g =
            match Graph.decomposeFirst g with
            | (Some c,g') -> 
                c
                |> fun (_,v,_,s) -> 
                    List.iteri (fun j (v',e) -> action (j+i) v v' e) s
                    recurse (i+s.Length) g'
            | (None,_) -> ()
        recurse 0 graph

    let fold (folder : 'State -> 'Vertex -> 'Vertex -> 'Edge -> 'State) (state: 'State) (graph:Graph<'Vertex,'Label,'Edge>) : 'State =
        let rec recurse st g =
            match Graph.decomposeFirst g with
            | (Some c,g') -> 
                c
                |> fun (_,v,_,s) ->
                    List.fold (fun state (v',e) -> folder state v v' e) st s
                    |> fun st -> 
                        recurse st g'
            | (None,_) -> st
        recurse state graph

module Graph =

    ///Creates an undirected graph from a list of vertices and a list of edges
    let create vertices edges : Graph<'Vertex,'Label,'Edge> =
        Graph.empty
        |> Vertices.addMany vertices
        |> Edges.addMany edges
    
    ///Transforms a graph into a adjacency matrix of its edges. If there is no edge between two vertices, the noEdgeValue is used.
    let inline toAdjacencyMatrix (g:Graph<'Vertex,'Label,'Edge>) =
        //Create a hashmap of the vertices
        let hashMap = System.Collections.Generic.Dictionary<'Vertex,int>()
        let n = 
            let rec loop g i= 
                match Graph.decomposeFirst g with
                | Some (_,v,_,_),g -> 
                    hashMap.[v] <- i
                    loop g (i+1)        
                | None, _ -> i
            loop g 0
        //Create the matrix
        let adj : 'Edge [][] = Array.init n (fun _ -> 
            Array.zeroCreate n)
        //Fill the matrix with values by using the hashmap as an index finder
        Edges.iter (fun v1 v2 e -> 
            adj.[hashMap.Item v1].[hashMap.Item v2] <- e
            adj.[hashMap.Item v2].[hashMap.Item v1] <- e
            ) g
        adj

    ///Transfroms a graph into a adjacency matrix, maps every edge using the projection.
    let inline toAdjacencyMatrixBy (projection : 'Edge -> 'REdge) (g:Graph<'Vertex,'Label,'Edge>) =
        //Create a hashmap of the vertices
        let hashMap = System.Collections.Generic.Dictionary<'Vertex,int>()
        let n = 
            let rec loop g i= 
                match Graph.decomposeFirst g with
                | Some (_,v,_,_),g -> 
                    hashMap.[v] <- i
                    loop g (i+1)        
                | None, _ -> i+1
            loop g 0
        //Create the matrix
        let adj : 'REdge [][] = Array.init n (fun _ -> 
            Array.zeroCreate n)
        //Fill the matrix with values by using the hashmap as an index finder
        Edges.iter (fun v1 v2 e -> 
            let e' = projection e
            adj.[hashMap.Item v1].[hashMap.Item v2] <- e'
            adj.[hashMap.Item v2].[hashMap.Item v1] <- e'
            ) g
        adj