namespace FSharp.FGL.Algorithm

///Contains generic functions for creating model graphs
module Models =
    
    let gilbert (newGraph:'Graph) (fVertexKey: int -> 'Vertex) (fAddVertex: 'Vertex -> 'Graph -> 'Graph) (fAddEdge: 'Vertex -> 'Vertex -> 'Graph -> 'Graph) (n: int) (p: float) = 
        let rnd = new System.Random()
        let rec addVertices i g =
            if i = n then 
                g
            else 
                addVertices (i+1) (fAddVertex (fVertexKey i) g)
        let rec innerAddEdges i j g =
            if j = n then g
            else 
                if rnd.NextDouble() > p then
                    innerAddEdges i (j+1) g
                elif i = j then
                    innerAddEdges i (j+1) g
                else 
                    innerAddEdges i (j+1) (fAddEdge (fVertexKey i) (fVertexKey j) g)
        let rec addEdges i g = 
            if i = n then g
            else addEdges (i+1) (innerAddEdges i 0 g)
        newGraph
        |> addVertices 0
        |> addEdges 0 