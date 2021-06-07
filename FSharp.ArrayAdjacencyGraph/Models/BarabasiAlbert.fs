namespace Models.BarabasiAlbert

open FSharp.Graph
open FSharp.ArrayAdjacencyGraph.Graph


module BarabasiAlbert =
    
    let barabasiAlbert (startingGraph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (numberOfNodes: int) (numberOfEdgesPerIteration: int) (isDirected: bool) (fVertexKey: int -> 'Vertex) (fLabel: 'Vertex -> 'Label) (fWeight: 'Vertex*'Vertex -> 'Edge) =
        let rnd = new System.Random()
           
        let g = startingGraph
    
        let oldV = g.VertexCount
    
        for n=oldV to oldV+numberOfNodes do
            let vertex  = fVertexKey n
            printfn "Vertex %A" vertex
            let label   = fLabel vertex
            printfn "label %A" label
            g.AddVertex(vertex,label)|> ignore
            let mutable m = 0
    
            let p k = ( (float (g.Degree k)) / (float (g.EdgeCount)) )
            let possibleConnections = 
                [|
                    for i in g.GetVertices() do
                        (vertex,i),(p i)
                |]
                |> Array.sortBy snd
            
            let rec getEdges counter m edges =
     
                if m = 0 then
                    g.AddManyEdges edges
                else
                    
                    if counter = possibleConnections.Length then
                        getEdges 0 m edges
                    else
                        let (edge,probability) = possibleConnections.[counter]
                        let r = rnd.NextDouble()
                        if r > probability then
                            getEdges (counter+1) (m) (edges)
                        else
                            let addEdge = fst edge, snd edge,fWeight edge
                            getEdges (counter+1) (m-1) (Array.concat [[|addEdge|];edges])
            getEdges 0 numberOfEdgesPerIteration [||] |> ignore
    
        g
    