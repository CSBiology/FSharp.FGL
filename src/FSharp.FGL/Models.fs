namespace FSharp.FGL.Algorithm

///Contains generic functions for creating model graphs
module Models =
    
    let gilbert 
        (newGraph: 'Graph) (fVertexKey: int -> 'Vertex) (fAddVertex: 'Vertex -> 'Graph -> 'Graph) (fAddEdge: 'Vertex -> 'Vertex -> 'Graph -> 'Graph) 
        (vertexCount: int) (p: float) = 
            let rnd = new System.Random()
            let rec addVertices i g =
                if i = vertexCount then 
                    g
                else 
                    addVertices (i+1) (fAddVertex (fVertexKey i) g)
            let rec innerAddEdges i j g =
                if j = vertexCount then g
                else 
                    if rnd.NextDouble() > p then
                        innerAddEdges i (j+1) g
                    elif i = j then
                        innerAddEdges i (j+1) g
                    else 
                        innerAddEdges i (j+1) (fAddEdge (fVertexKey i) (fVertexKey j) g)
            let rec addEdges i g = 
                if i = vertexCount then g
                else addEdges (i+1) (innerAddEdges i 0 g)
            newGraph
            |> addVertices 0
            |> addEdges 0

    let erdosRenyi 
        (newGraph: 'Graph) (fVertexKey: int -> 'Vertex) (fAddVertex: 'Vertex -> 'Graph -> 'Graph) (fAddEdge: 'Vertex -> 'Vertex -> 'Graph -> 'Graph) 
        (vertexCount: int) (edgeCount: int) = 
            let rnd = new System.Random()
            let rec addVertices i g =
                if i = vertexCount then 
                    g
                else 
                    addVertices (i+1) (fAddVertex (fVertexKey i) g)
            let rec forceAddEdge g (s: Set<int*int>) =
                let r = rnd.Next(0,vertexCount),rnd.Next(0,vertexCount)
                if s.Contains r then
                    forceAddEdge g (s: Set<int*int>)
                else 
                    fAddEdge (fVertexKey (fst r)) (fVertexKey (snd r)) g, Set.add r s
            let rec addEdges i (g,(s: Set<int*int>)) = 
                if i = edgeCount then g
                else 
                    addEdges (i+1) (forceAddEdge g s)
            newGraph
            |> addVertices 0
            |> fun g -> addEdges 0 (g,Set.empty)
    
//    let barabasiAlbert 
//       (newGraph: 'Graph) (fVertexKey: int -> 'Vertex) (fAddVertex: 'Vertex -> 'Graph -> 'Graph) (fAddEdge: 'Vertex -> 'Vertex -> 'Graph -> 'Graph)
//       (fGetDegrees: 'Graph -> seq<int*int>) (vertexCount: int) (initialVertexCount: int) (edgesPerIteration: int) =
//            let addVertexWithEdges
            