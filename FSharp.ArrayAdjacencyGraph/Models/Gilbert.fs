namespace Models.Gilbert

open FSharp.Graph
open FSharp.ArrayAdjacencyGraph.Graph


module Gilbert =
    
    ///
    let gilbert (numberOfNodes: int) (probability: float) (isDirected: bool) (fVertexKey: int -> 'Vertex) (fLabel: 'Vertex -> 'Label) (fWeight: 'Vertex -> 'Edge) =
        if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let rnd         = new System.Random()
        let vertexEdges = System.Collections.Generic.Dictionary<'Vertex,LEdge<'Vertex,'Edge>[]>()
        let labelDict   = System.Collections.Generic.Dictionary<'Vertex,'Label>()

        let vertices = 
             [|
                for i=0 to (numberOfNodes-1) do
                    fVertexKey i
             |]

        for i=0 to (numberOfNodes-1) do
            //let vertex  = fVertexKey i
            let vertex = vertices.[i]
            let label   = fLabel vertex
            labelDict.Add(vertex,label)
            vertexEdges.Add(vertex,[||])


        if isDirected then
            for s in vertices do
        
                printfn "%A" s

                for t in vertices do

                    printfn "%A" t

                    if rnd.NextDouble() < probability then
                
                        printfn "rnd"

                        if s=t then                                            
                            let w       = fWeight s
                            let valueS  = vertexEdges.Item s 
                            vertexEdges.Item s <- (Array.concat[[|(s,s,w);(s,s,w)|];valueS])
                        else
                            let w       = fWeight s
                            let valueS  = vertexEdges.Item s 
                            let valueT  = vertexEdges.Item t
                            vertexEdges.Item s <- (Array.concat[[|s,t,w|];valueS]) 
                            vertexEdges.Item t <- (Array.concat[[|s,t,w|];valueT])                  
        else
            for i=0 to vertices.Length-1 do 
                let s = vertices.[i]

                for j=i to vertices.Length-1 do
                    if rnd.NextDouble() < probability then
                        let t = vertices.[j]
    
                        if s=t then 
                            let w       = fWeight s
                            let valueS  = vertexEdges.Item s 
                            vertexEdges.Item s <- (Array.concat[[|(s,s,w);(s,s,w)|];valueS]) 
                        else
                            let w       = fWeight s
                            let valueS  = vertexEdges.Item s 
                            let valueT  = vertexEdges.Item t
                            vertexEdges.Item s <- (Array.concat[[|s,t,w|];valueS]) 
                            vertexEdges.Item t <- (Array.concat[[|s,t,w|];valueT])      

        
        ArrayAdjacencyGraph(vertexEdges,labelDict)
