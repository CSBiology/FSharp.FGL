namespace ArrayAdjacencyGraph.Models

open FSharp.FGL.ArrayAdjacencyGraph


// Adaption of the Barabási–Albert model for generating random scale-free networks using a preferential attachment mechanism.
// Barabasi AL, Albert R. Emergence of scaling in random networks. Science. 1999 Oct 15;286(5439):509-12. doi: 10.1126/science.286.5439.509. PMID: 10521342.
module BarabasiAlbert =
    
    /// Returns an ArrayAdjacencyGraph that was randomly grown according to the Barabási–Albert model with the given parameters. 
    ///
    /// startingGraph is the original graph, that is used as the initial connected network. The rest of the calculations and growth of the network are performed on this graph.
    ///
    /// numberOfVertices specifies how many additional vertices the final graph will have.
    ///
    /// numberOfEdgesPerIteration specifies how many edges should be added to the graph per iteration.
    ///
    /// fVertexKey is a function that is used to transform an integer (the index of the vertex) into the 'Vertex type.  
    ///
    /// fLabel is a function that transforms the 'Vertex type into a label of the 'Label type.   
    ///
    /// fWeight is a funtion that takes two 'Vertices and returns a weight between them in form of an 'Edge type.    
    let barabasiAlbert (startingGraph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (numberOfVertices: int) (numberOfEdgesPerIteration: int) (fVertexKey: int -> 'Vertex) (fLabel: 'Vertex -> 'Label) (fWeight: 'Vertex*'Vertex -> 'Edge) =
        let rnd = new System.Random()
           
        let g = startingGraph
    
        let oldV = g.VertexCount
    
        for n=oldV to oldV+numberOfVertices do
            let vertex  = fVertexKey n
            let label   = fLabel vertex
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
    