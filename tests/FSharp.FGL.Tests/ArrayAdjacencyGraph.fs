module ArrayAdjacencyGraphTests

open FSharp.ArrayAdjacencyGraph
open Expecto


[<Tests>]
let testInitializeGraph =
    
    let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
    let testReadFilePath = System.IO.Path.Combine(testDirectory,"TestFileRead.txt")

    testList "AAG.initialize Graph" [
        
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
            
            let arrayAdjacencyGraph = FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)

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
        FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)
    
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
    
        FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

    testList "AAG.Edges" [
        
        testCase "TryGetEdge" (fun () ->
            
            let edgeTry =
                [
                    for i in exampleGraphEdges do
                        let (s,t,w) = i
                        testGraph.TryGetEdge (s,t)
                ]
                |> List.choose (id)
    
            Expect.sequenceEqual
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
            
            Expect.sequenceEqual 
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

            Expect.sequenceEqual
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

            Expect.sequenceEqual
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

        testCase "GetConnectedEdgesFail" (fun () ->
           
            let vertex =
                exampleGraphVertices
                |> List.last
                |> fst
                |> (+) (1)  

            let getConnectedEdgesFail =
                try
                    testGraph.GetConnectedEdges vertex |>ignore
                    Result.Ok "Did get connected Edges"
                with
                | err -> Result.Error "Could not get connected Edges"
                
            Expect.isError
                getConnectedEdgesFail
                "GetConnectedEdges does yield edges where none should exist"
        )

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
            
            testGraph.SetWeight (s,t,(w+w)) |> ignore

            Expect.equal
                ((testGraph.GetWeight (s,t)))
                (w+w)
                "GetWeight does not yield the expected weight"
        )

        testCase "AddEdge" (fun () ->
            
            let newGraph =
                FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)
            
            let createNewEdge weight =
                    
                let rec differentTarget source =
                    let v = rnd.Next((exampleGraphVertices|>List.head|>fst),(exampleGraphVertices|>List.last|>fst))
                    if v <> source && not (List.contains (source,v,weight) exampleGraphEdges) then
                        source,v,weight
                    else 
                        differentTarget source
            
                let s  = rnd.Next((exampleGraphVertices|>List.head|>fst),(exampleGraphVertices|>List.last|>fst))
                differentTarget s
                
            let (s,t,w) = 
                createNewEdge (rnd.Next((0),(10)))
            
            let oldSourceDegree = newGraph.Degree s
                
            let oldTargetDegree = newGraph.Degree t

            newGraph.AddEdge (s,t,w) |> ignore
            
            let newSourceDegree = newGraph.Degree s
                
            let newTargetDegree = newGraph.Degree t

            let isEdgeNew =
                not (List.contains (s,t,w) exampleGraphEdges)

            let inEdge =
                (newGraph.GetInEdges t)
                |> Array.contains (s,t,w)
            
            let outEdge =
                (newGraph.GetOutEdges s)
                |> Array.contains (s,t,w)

            let isDegreeChangeSame =
                (newSourceDegree-oldSourceDegree) = (newTargetDegree-oldTargetDegree)
            
            Expect.isTrue
                (isEdgeNew&&inEdge&&outEdge&&isDegreeChangeSame)
                "AddEdge does not add an edge correctly"
        )
        
        testCase "RemoveEdge" (fun () ->
            
            let newGraph =
                FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

            let (s,t,w) = 
                let position = rnd.Next((0),(exampleGraphEdges.Length-1))
                exampleGraphEdges.[position]

            let oldSourceDegree = newGraph.Degree s
                 
            let oldTargetDegree = newGraph.Degree t

            newGraph.RemoveEdge (s,t,w) |> ignore
             
            let newSourceDegree = newGraph.Degree s
                 
            let newTargetDegree = newGraph.Degree t          

            let inEdge =
                (newGraph.GetInEdges t)
                |> Array.contains (s,t,w)
                |> not

            let outEdge =
                (newGraph.GetOutEdges s)
                |> Array.contains (s,t,w)
                |> not 

            let isDegreeChangeSame =
                (newSourceDegree-oldSourceDegree) = (newTargetDegree-oldTargetDegree)
            
            Expect.isTrue
                (inEdge&&outEdge&&isDegreeChangeSame)
                "RemoveEdge does not remove an edge correctly"
        )

    ]

[<Tests>]
let testVertices =
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
        FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)
    
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
    
        FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

    testList "AAG.Vertices" [
        
        testCase "ContainsVertex" (fun () ->
        
            let allVerticesContained =
                [
                    for (v,l) in exampleGraphVertices do
                        testGraph.ContainsVertex v
                ]
            
            Expect.isFalse
                (List.contains false allVerticesContained)
                "ContainsVertex does not return the correct value"
        )

        testCase "VertexCount" (fun () ->
            
            Expect.equal
                testGraph.VertexCount
                exampleGraphVertices.Length
                "VertexCount does not return the correct number of vertices"
        )

        testCase "GetVertices()" (fun () ->
            
            Expect.equal
                (testGraph.GetVertices()|>Array.sort|>List.ofArray)
                (exampleGraphVertices|>List.map fst|>List.sort)
                "GetVertices() does not return the correct sequence"
        )
        
        testCase "Degree" (fun () ->
        
            let vertices =
                exampleGraphVertices
                |> List.map fst
            
            let degreeActual = 
                [
                    for i in vertices do
                        testGraph.Degree i
                ]

            let degreeExpected =
                [
                    for i in vertices do
                       
                        exampleGraphEdges
                        |> List.filter (fun (s,t,w) -> (s=i) || (t=i))
                        |> List.length
                        
                        
                ]

            Expect.equal
                degreeActual
                degreeExpected
                "degree does not return the correct value"

        )
        
        testCase "InDegree" (fun () ->
            
                let vertices =
                    exampleGraphVertices
                    |> List.map fst
                
                let inDegreeActual = 
                    [
                        for i in vertices do
                            testGraph.InDegree i
                    ]

                let inDegreeExpected =
                    [
                        for i in vertices do
                           
                            exampleGraphEdges
                            |> List.filter (fun (s,t,w) -> (t=i))
                            |> List.length
                            
                            
                    ]

                Expect.equal
                    inDegreeActual
                    inDegreeExpected
                    "InDegree does not return the correct value"

        )
            
        testCase "OutDegree" (fun () ->
                
            let vertices =
                exampleGraphVertices
                |> List.map fst
                    
            let outDegreeActual = 
                [
                    for i in vertices do
                        testGraph.OutDegree i
                ]

            let outDegreeExpected =
                [
                    for i in vertices do
                               
                        exampleGraphEdges
                        |> List.filter (fun (s,t,w) -> (s=i))
                        |> List.length
                                
                                
                ]

            Expect.equal
                outDegreeActual
                outDegreeExpected
                "OutDegree does not return the correct value"

        )

        //testCase "ConnectedEdgesEmpty" (fun () ->)
        //Based on Degree. As long as Degree works this will too.

        testCase "WeightedDegree Total" (fun () ->
        
            let weightedDegreeTotal = 
                [
                    for (v,l) in exampleGraphVertices do
                        (testGraph.WeightedDegree ((Array.sumBy (fun (s,t,w) -> w)),v))
                ]
                |> List.sum

            Expect.equal
                weightedDegreeTotal
                (exampleGraphEdges|>List.sumBy (fun (s,t,w) -> 2*w))
                "WeightedDegree Total does not equal the weight of all edges combined"
        )

        testCase "AddVertex" (fun () ->
        
            let newGraph =
                FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)
            
            let newVertex = (List.length exampleGraphVertices)*2

            let vertexCountOld = newGraph.VertexCount

            newGraph.AddVertex (newVertex,newVertex) |> ignore
          
            Expect.isTrue
                (((newGraph.VertexCount-vertexCountOld)=1)&&(newGraph.ContainsVertex newVertex))
                "AddVertex does not add vertices correctly"
        
        )

        testCase "RemoveVertex" (fun () ->
            
            let newGraph =
                FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)
                
            let toBeRemovesVertex = (List.last exampleGraphVertices)

            let vertexCountOld = newGraph.VertexCount

            newGraph.RemoveVertex (fst toBeRemovesVertex) |> ignore
            
            Expect.isTrue
                (((vertexCountOld-newGraph.VertexCount)=1)&&(not(newGraph.ContainsVertex (fst toBeRemovesVertex))))
                "RemoveVertex does not remove vertices correctly"
        ) 
                
    ]

[<Tests>]
let testLabels =
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
        FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)
   
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
   
        FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)


    testList "AAG.Labels" [
        
        testCase "TryGetLabel" (fun () ->
        
            let tryGetLabelAll = 
                exampleGraphVertices
                |> List.map (fun (v,l) -> (testGraph.TryGetLabel v)=(Some l))

            Expect.isFalse
                (List.contains false tryGetLabelAll)              
                "TryGetLabel does not return the correct label"
        )

        testCase "GetLabel" (fun () ->
            
            let getLabelAll = 
                exampleGraphVertices
                |> List.map (fun (v,l) -> (testGraph.GetLabel v)=(l))

            let x = 
               exampleGraphVertices
               |> List.map (fun (v,l) -> (v,l),(testGraph.GetLabel v))

            Expect.isFalse
                (List.contains false getLabelAll)
                (sprintf "GetLabel does not get the expected label %A" x)
        )

        testCase "SetLabel" (fun () ->
            
            let newGraph =
                FSharp.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

            exampleGraphVertices
            |> List.map(fun (v,l) -> newGraph.SetLabel ((v),(l*l)))
            |> ignore

            let getLabelAll = 
                exampleGraphVertices
                |> List.map (fun (v,l) -> (newGraph.GetLabel v)=((l*l)))
            
            let x = 
                exampleGraphVertices
                |> List.map (fun (v,l) -> (v,l),(newGraph.GetLabel v))

            Expect.isFalse
                (List.contains false getLabelAll)            
                (sprintf "SetLabel does not correctly change the label %A" x)
        )

    ]