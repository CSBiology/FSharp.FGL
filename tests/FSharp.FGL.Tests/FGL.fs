module FGLTests

open FSharp.FGL
open FSharp.FGL.Graph
open Expecto


[<Tests>]
let testInitializeGraph =
    
    testList "FGL initialize Graph" [
        
        testCase "from LVertex and LEdge to Undirected Graph" (fun () ->
            let rnd = System.Random()
            
            let vertexList = 
                [
                    for i=0 to 100 do
                        (i,i)
                ]
            
            let edges =
                [
                    for i=0 to 100 do
                        i,rnd.Next(0,100),1  
                        i,rnd.Next(0,100),1
                        i,rnd.Next(0,100),1
                        i,rnd.Next(0,100),1
                ]
            
            let graph = FSharp.FGL.Undirected.Graph.create vertexList edges

            let verticesGraph   = Vertices.toVertexList graph

            let edgesGraph      = 
                Undirected.Edges.toEdgeList graph
                        
            Expect.sequenceEqual 
                verticesGraph
                vertexList
                "The vertexList of the graph does not match the data used to create the graph"
        
            Expect.sequenceEqual
                (edgesGraph|>List.distinct|>List.sort)              
                (edges|>List.distinct|>List.sort)
                "the edgeList of the graph does not match the data used to create the graph"
        )   
    ]   

[<Tests>]
let testFGLGraph =

    testList "FGL.Graph"[

        testCase "Decompose"(fun () ->

            let vertexList =
                [
                    for i=0 to 5 do
                        i,(sprintf"Label %A" i)
            
                ]
            
            let edgeList =
                [
                    0,1;
                    0,2;
                    0,3;
                    0,4;
                    0,5;
                    1,2;
                    1,3;
                    1,4;
                    1,5;
                    2,3;
                    2,4;
                    2,5;
                    3,4;
                    3,5;
                    4,5
                ]
                |> List.map (fun (s,t) ->(s,t,1))
                        
            
            let g = FSharp.FGL.Undirected.Graph.create vertexList edgeList 

            let toBeDecomposed =
                //rnd.Next(0,5)
                3

            let gDecomposed =
                FSharp.FGL.Graph.decompose toBeDecomposed g
                |> snd

            let vertexListDecomposed =
                Vertices.toVertexList gDecomposed

            let edgeListDecomposed =
                Undirected.Edges.toEdgeList gDecomposed


            let listTripelLowestSource (edgeList) =
                edgeList
                |> List.map(fun (s,t,w) ->
                    if s < t then
                        (s,t,w)
                    else
                        (t,s,w)
                )

            Expect.sequenceEqual
                (vertexList|> List.choose(fun (v,l) -> if v <> toBeDecomposed then Some (v,l) else None))
                vertexListDecomposed
                "vertexList of the decomposed Graph is equal to the expected values "

            Expect.isTrue
                ((edgeListDecomposed|>listTripelLowestSource)=(FSharp.FGL.Undirected.Edges.toEdgeList g|>listTripelLowestSource|> List.choose(fun (s,t,w) -> if s <> toBeDecomposed && t <> toBeDecomposed then Some (s,t,w) else None)))
                "edgeList of the decomposed Graph is equal to the expected values "

        )

    ]