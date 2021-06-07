namespace Models.Scale_free_graph

open FSharp.Graph
open FSharp.ArrayAdjacencyGraph.Graph


module Scale_free_graph =
       
    let scale_free_graph (n: int) (alpha: float) (beta: float) (gamma: float) (delta_in: float) (delta_out: float) (create_using:ArrayAdjacencyGraph<int,int,float>)(* (seed)*) =
        //Parameters
        //   ----------
        //   n : integer
        //       Number of nodes in graph
        //   alpha : float
        //       Probability for adding a new node connected to an existing node
        //       chosen randomly according to the in-degree distribution.
        //   beta : float
        //       Probability for adding an edge between two existing nodes.
        //       One existing node is chosen randomly according the in-degree
        //       distribution and the other chosen randomly according to the out-degree
        //       distribution.
        //   gamma : float
        //       Probability for adding a new node connected to an existing node
        //       chosen randomly according to the out-degree distribution.
        //   delta_in : float
        //       Bias for choosing nodes from in-degree distribution.
        //   delta_out : float
        //       Bias for choosing nodes from out-degree distribution.
        //   create_using : NetworkX graph constructor, optional
        //       The default is a MultiDiGraph 3-cycle.
        //       If a graph instance, use it without clearing first.
        //       If a graph constructor, call it to construct an empty graph.
        //   seed : integer, random_state, or None (default)
        //       Indicator of random number generation state.
        //       See :ref:`Randomness<randomness>`.          
        //The sum of `alpha`, `beta`, and `gamma` must be 1.
        if alpha+beta+gamma <> 1. then 
            failwithf "The sum of alpha, beta, and gamma must be 1., but here is %A" (alpha+beta+gamma)
    
        if alpha <= 0. then 
               failwith "alpha must be > 0."
        if beta <= 0. then
            failwith "beta must be > 0."
        if gamma <= 0. then
            failwith "gamma must be > 0."
        
        let G = 
            if create_using.VertexCount < 3 then 
                create_using.AddManyVertices[|(0,0);(1,1);(2,2)|]|>ignore
                create_using.AddManyEdges[|(0, 1, 1.); (1, 2, 1.); (2, 0, 1.)|]
            else
                create_using
    
        let rnd = new System.Random()
    
        let _choose_node(distribution, delta, psum) =
            let mutable cumsum = 0.0
            //normalization
            let r = rnd.NextDouble()
            //for n, d in distribution do
            //    cumsum <- (cumsum)+((d+delta) / psum)
            //    if r < cumsum then break
            //n
            let mutable n = 0
            let mutable threshold = false
            if distribution = "in" then
                while (not threshold) do
                    let d = float (G.InDegree n)
                    cumsum <- (cumsum)+((d+delta) / psum)
                    if r < cumsum then
                        threshold <- true
                    else
                        n <- n+1      
                n
            elif distribution = "out" then
                while (not threshold) do
                    let d = float (G.OutDegree n)
                    cumsum <- (cumsum)+((d+delta) / psum)
                    if r < cumsum then
                        threshold <- true
                    else
                        n <- n+1     
                n
            else
                failwith "ERROR"
    
        while G.VertexCount < n do
            let psum_in     = float (G.EdgeCount) + delta_in  *  float (G.VertexCount)
            let psum_out    = float (G.EdgeCount) + delta_out * float (G.VertexCount)
            let r = rnd.NextDouble()
    
            if r < alpha then
                let v = (G.VertexCount)
                let w = _choose_node("in",delta_in,psum_in)
                G.AddVertex (v,v)|>ignore
                G.AddEdge (v,w,1.)
                |>ignore
    
            elif r < (alpha + beta) then
                let v = _choose_node("out",delta_out,psum_out)
                let w = _choose_node("in", delta_in, psum_in)
    
                G.AddEdge(v,w,1.)
                |>ignore
    
            else
                let v = _choose_node("out",delta_out,psum_out)
                let w = G.VertexCount
    
                G.AddVertex (w,w)|>ignore
                G.AddEdge (v,w,1.)
                |>ignore
    
        G
     