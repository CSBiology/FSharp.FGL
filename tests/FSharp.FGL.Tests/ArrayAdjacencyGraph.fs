module ArrayAdjacencyGraphTests

open FSharp.ArrayAdjacencyGraph
open Expecto


[<Tests>]
let testInitializeGraph =
    
    let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
    let testReadFilePath = System.IO.Path.Combine(testDirectory,"TestFileRead.txt")

    testList "initialize Graph" [
        
        testCase "from LVertex and LEdge to Graph" (fun () ->
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
            
            let arrayAdjacencyGraph = FSharp.ArrayAdjacencyGraph.Graph.ArrayAdjacencyGraph(vertexList,edges)

            let graphVertices   = arrayAdjacencyGraph.GetVertices()
            let graphLabels     = arrayAdjacencyGraph.GetLabels()
            let verticesG       = Array.map2 (fun v l -> (v,l)) graphVertices graphLabels |> Array.sort |> Array.toList

            let edgesGraph      = 
                [|
                    for i in graphVertices do
                        arrayAdjacencyGraph.GetConnectedEdges i
                
                |]           
                |> Array.concat
                |> Array.toList

            
            Expect.isTrue 
                (((verticesG)=(vertexList))&&((edges|>List.distinct|>List.sort)=(edgesGraph|>List.distinct|>List.sort)))
                "The data used to generate the graph is not equal to the data in the graph"
        )   
    ]   

[<Tests>]
let testEdges =
    let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
    let rnd = System.Random()
              
    let arrayAdjacencyGraphRnd =       
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
        FSharp.ArrayAdjacencyGraph.Graph.ArrayAdjacencyGraph(vertexList,edges)
    
    let exampleGraphVertices =
        [ for i=0 to 9 do (i,i) ]
    
    let exampleGraphEdges =
        [
            0,2,1;
            0,6,1;
            0,7,1;
            0,9,1;
            1,2,1;
            1,5,1;
            1,6,1;
            1,7,1;
            1,9,1;
            2,5,1;
            2,8,1;
            3,5,1;
            3,6,1;
            4,5,1;
            4,9,1;
            8,9,1
        ]
    
    let testGraph =
    
        FSharp.ArrayAdjacencyGraph.Graph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

    testList "Edges" [
        
        testCase "TryGetEdge" (fun () ->
            
            let edgeTry =
                [
                    for i in exampleGraphEdges do
                        let (s,t,w) = i
                        testGraph.TryGetEdge (s,t)
                ]
                |> List.map (fun x -> match x with |Some y -> y|None -> failwith "ArrayAdjacencyGraph.TryGetEdge Error")

            Expect.equal
                edgeTry
                exampleGraphEdges
                "tryGetEdge does not yield the correct edge."
        )

        testCase "GetEdge" (fun () ->
            
            let getEdge =
                [
                    for i in exampleGraphEdges do
                        let (s,t,w) = i
                        testGraph.GetEdge (s,t)
                ]
            
            Expect.equal 
                getEdge
                exampleGraphEdges
                "getEdge does not yield the correct edge."
        )

        testCase "TryGetEdges" (fun () ->
            
            let getEdge =
                [
                    for i in exampleGraphEdges do
                        let (s,t,w) = i
                        testGraph.GetEdge (s,t)
                ]
            
            let getEdges = 
                [|
                    for i in exampleGraphEdges do
                        let (s,t,w) = i
                        testGraph.TryGetEdges (s,t)
                |]
                |>Array.map(fun x -> match x with |Some y -> y|None -> failwith "ArrayAdjacencyGraph.TryGetEdges Error")
                |>Array.concat
                |>Array.toList

            Expect.equal
                getEdge
                getEdges
                "tryGetEdges does not yield the correct edges."
        )
        
        testCase "GetEdges" (fun () ->
            
            let getEdge =
                [
                    for i in exampleGraphEdges do
                        let (s,t,w) = i
                        testGraph.GetEdge (s,t)
                ]
            
            let getEdges = 
                [|
                    for i in exampleGraphEdges do
                        let (s,t,w) = i
                        testGraph.GetEdges (s,t)
                |]
                |>Array.concat
                |>Array.toList

            Expect.equal
                getEdge
                getEdges
                "getEdges does not yield the correct edges."
        )

        testCase "GetEdges()" (fun () ->
                             
            Expect.equal
                (exampleGraphEdges|>List.distinct)
                (testGraph.GetEdges()|>Array.toList)
                "getEdges() does not yield the correct edges."
        )
    
        testCase "EdgeCount" (fun () ->
            
            Expect.equal
                (exampleGraphEdges|>List.length)
                (testGraph.EdgeCount)
                "EdgeCount does not yield the correct amount of edges."
            )
        
        testCase "ContainsEdgeFalse" (fun () ->
            
            Expect.isFalse 
                (testGraph.ContainsEdge(200,200,888))
                "ContainsEdge returns true for edges that don´t exist"
        )
        
        testCase "ContainsEdgeTrue" (fun () ->
            
            Expect.isTrue 
                (testGraph.ContainsEdge(0,2,1))
                "ContainsEdge returns true for edges that don´t exist"
        )

        testCase "TryGetConnectedEdges1" (fun () ->
            
            let trygetConnectedEdgesAll =
                [|
                    for i in exampleGraphVertices do
                        testGraph.TryGetConnectedEdges (fst i)
                |]
                |> Array.map (fun x -> match x with |Some y -> y |None -> failwith "TryGetConnectedEdges does not yield all expected edges")
                |> Array.concat 
                |> Array.distinct

            Expect.containsAll
                exampleGraphEdges
                trygetConnectedEdgesAll
                "TryGetConnectedEdges does not yield all expected edges"
        )
        
        testCase "TryGetConnectedEdgesFail" (fun () ->
            
            let vertex =
                exampleGraphVertices
                |> List.last
                |> fst
                |> (+) (1)                    

            Expect.isNone
                (testGraph.TryGetConnectedEdges vertex)
                "TryGetConnectedEdges does yield edges where none should exist"
        )
        
        testCase "GetConnectedEdges" (fun () ->
            
            let trygetConnectedEdgesAll =
                [|
                    for i in exampleGraphVertices do
                        testGraph.GetConnectedEdges (fst i)
                |]
                |> Array.concat 
                |> Array.distinct

            Expect.containsAll
                exampleGraphEdges
                trygetConnectedEdgesAll
                "GetConnectedEdges does not yield all expected edges"
        )

        //testCase "GetConnectedEdgesFail" (fun () ->
            
        //    let vertex =
        //        exampleGraphVertices
        //        |> List.last
        //        |> fst
        //        |> (+) (1)                    

        //    Expect.isEmpty
        //        (testGraph.GetConnectedEdges vertex)
        //        "GetConnectedEdges does yield edges where none should exist"
        //)

        testCase "TryGetInEdges None" (fun () ->
            
            Expect.equal
                (testGraph.TryGetInEdges 0)
                (Some [||])
                "TryGetInEdges was supposed to yield Some empty array, yet returned some array with values"
        )
        
        testCase "TryGetInEdges " (fun () ->
            
            let edges = 
                [|
                    for i in exampleGraphVertices do
                        let (v,l) = i
                        let x = (testGraph.TryGetInEdges v) 
                        if x <> None then
                            Option.get x                                         
                |]
                |> Array.concat
                |> Array.toList
                |> List.sort

            Expect.equal
                edges
                (exampleGraphEdges|>List.sort)
                "TryGetInEdges does not return the expected Value"
        )

        testCase "GetInEdges None" (fun () ->
            
            let edges = 
                [|
                    for i in exampleGraphVertices do
                        let (v,l) = i
                        (testGraph.GetInEdges v) 
                |]
                |> Array.concat
                |> Array.toList
                |> List.sort

            Expect.equal
                edges
                (exampleGraphEdges|>List.sort)
                "TryGetInEdges does not return the expected Value"
        )

        testCase "GetInEdges " (fun () ->
            
            Expect.equal
                (testGraph.GetInEdges 0)
                [||]
                "GetInEdges was supposed to yield an empty array, yet returned values"
        )

        testCase "TryGetOutEdges None" (fun () ->
            
            Expect.equal
                (testGraph.TryGetOutEdges 9)
                (Some [||])
                "TryGetOutEdges was supposed to yield Some empty array, yet returned some array with values"
        )
        
        testCase "TryGetOutEdges " (fun () ->
            
            let edges = 
                [|
                    for i in exampleGraphVertices do
                        let (v,l) = i
                        let x = (testGraph.TryGetOutEdges v) 
                        if x <> None then
                            Option.get x                                         
                |]
                |> Array.concat
                |> Array.toList
                |> List.sort

            Expect.equal
                edges
                (exampleGraphEdges|>List.sort)
                "TryGetOutEdges does not return the expected Value"
        )

        testCase "TryGetWeight" (fun () ->
            
            let (s,t,w) = 
                let position = rnd.Next((0),(exampleGraphEdges.Length-1))
                exampleGraphEdges.[position]

            Expect.equal
                ((testGraph.TryGetWeight (s,t)))
                (Some w)
                "TryGetWeight does not yield the expected weight"
        )
        
        testCase "GetWeight" (fun () ->
            
            let (s,t,w) = 
                let position = rnd.Next((0),(exampleGraphEdges.Length-1))
                exampleGraphEdges.[position]

            Expect.equal
                ((testGraph.GetWeight (s,t)))
                (w)
                "GetWeight does not yield the expected weight"
        )

        testCase "SetWeight" (fun () ->
            
            let (s,t,w) = 
                let position = rnd.Next((0),(exampleGraphEdges.Length-1))
                exampleGraphEdges.[position]
            


            Expect.equal
                ((testGraph.GetWeight (s,t)))
                (w)
                "GetWeight does not yield the expected weight"
        )

    
    ]

[<Tests>]
let testVertices =
    let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
    let testReadFilePath = System.IO.Path.Combine(testDirectory,"TestFileRead.txt")

    testList "Vertices" [
    
    
    ]

[<Tests>]
let testLabels =
    let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
    let testReadFilePath = System.IO.Path.Combine(testDirectory,"TestFileRead.txt")

    testList "Labels" [
    
    
    ]