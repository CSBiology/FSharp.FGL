module ArrayAdjacencyGraphTests

open FSharp.FGL
open FSharp.FGL.ArrayAdjacencyGraph
open Expecto


[<Tests>]
let testInitializeGraph =
    
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
            
            let arrayAdjacencyGraph = FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)

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
                        
            Expect.sequenceEqual 
                verticesG
                vertexList
                "The vertexList of the graph does not match the data used to create the graph"
        
            Expect.sequenceEqual
                (edgesGraph|>List.distinct|>List.sort)              
                (edges|>List.distinct|>List.sort)
                "the edgeList of the graph does not match the data used to create the graph"
        )   
    ]   

[<Tests>]
let testEdges =
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
        FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)
    
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
    
        FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

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
        
        testCase "GetEdge FAIL" (fun () ->
            
            let impossibleEdge =
                let x = 
                    exampleGraphVertices
                    |> List.last
                    |> fst

                x+1

            let GetEdgeFAIL =
                try
                    testGraph.GetEdge (impossibleEdge,impossibleEdge) |> ignore
                    Result.Ok "Did get connected Edges"
                with
                | err -> Result.Error "Could not get connected Edges"
    
            Expect.isError
                GetEdgeFAIL
                "getEdge does yield an edge that doesn´t exist"
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
                             
            Expect.sequenceEqual
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
            
            let (s,t,w) =
                exampleGraphEdges.[0]

            Expect.isTrue 
                (testGraph.ContainsEdge(s,t,w))
                "ContainsEdge returns true for edges that don´t exist"
        )

        testCase "TryGetConnectedEdges1" (fun () ->
            
            let trygetConnectedEdgesAll =
                [|
                    for i in exampleGraphVertices do
                        testGraph.TryGetConnectedEdges (fst i)
                |]
                |> Array.choose (id)
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

            Expect.sequenceEqual
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

            Expect.sequenceEqual
                edges
                (exampleGraphEdges|>List.sort)
                "TryGetInEdges does not return the expected Value"
        )

        testCase "GetInEdges " (fun () ->
            
            Expect.sequenceEqual
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

            Expect.sequenceEqual
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
                FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)
            
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
                (isEdgeNew)
                "The added edge is not new"
        
            Expect.isTrue
                (inEdge)
                "The added edge is not found as inEdge"

            Expect.isTrue
                (outEdge)
                "The added edge is not found as outEdge"

            Expect.isTrue
                (isDegreeChangeSame)
                "The added edge does not change the Degree of the connected vertices"
            
        )
        
        testCase "RemoveEdge" (fun () ->
            
            let newGraph =
                FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

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
                (inEdge)
                "RemoveEdge does not remove the inedge correctly"
            Expect.isTrue
                (outEdge)
                "RemoveEdge does not remove the outedge correctly"
            Expect.isTrue
                (isDegreeChangeSame)
                "RemoveEdge does change the degree of the connected vertices"
               
        )

    ]

[<Tests>]
let testVertices =
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
        FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)
    
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
    
        FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

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
            
            Expect.sequenceEqual
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

                Expect.sequenceEqual
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

            Expect.sequenceEqual
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
                        (testGraph.WeightedDegree ((fun x -> float x),v))
                ]
                |> List.sum
            
            Expect.equal
                weightedDegreeTotal
                (exampleGraphEdges|>List.sumBy (fun (s,t,w) -> 2.*float w))
                "WeightedDegree Total does not equal the weight of all edges combined"
        )

        testCase "AddVertex" (fun () ->
        
            let newGraph =
                FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)
            
            let newVertex = (List.length exampleGraphVertices)*2

            let vertexCountOld = newGraph.VertexCount

            newGraph.AddVertex (newVertex,newVertex) |> ignore
          
            Expect.isTrue
                ((newGraph.VertexCount-vertexCountOld)=1)
                "AddVertex does not add the correct amount of vertices"
            
            Expect.isTrue
                (newGraph.ContainsVertex newVertex)
                "AddVertex does not add the vertex correctly"

        )

        testCase "RemoveVertex" (fun () ->
            
            let newGraph =
                FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)
                
            let toBeRemovesVertex = (List.last exampleGraphVertices)

            let vertexCountOld = newGraph.VertexCount

            newGraph.RemoveVertex (fst toBeRemovesVertex) |> ignore
            
            Expect.isTrue
                (((vertexCountOld-newGraph.VertexCount)=1))
                "RemoveVertex does not remove the correct amount of vertices"
            
            Expect.isFalse
                (((newGraph.ContainsVertex (fst toBeRemovesVertex))))
                "RemoveVertex does not remove vertices correctly"
        )         

        testCase "TryPredecessors"  (fun () ->
            Expect.equal
                (testGraph.TryPredecessors 9)
                (Some [|0; 1; 4; 8|])
                "TryPredecessors does not return the expected values"
            
            Expect.equal
                (testGraph.TryPredecessors 2)
                (Some [|0;1|])
                "TryPredecessors does not return the expected values"

        )

        testCase "Predecessors"     (fun () ->
            Expect.equal
                (testGraph.Predecessors 9)
                ([|0; 1; 4; 8|])
                "Predecessors does not return the expected values"
            
            Expect.equal
                (testGraph.Predecessors 2)
                ([|0;1|])
                "Predecessors does not return the expected values"

        )
        
        testCase "TrySuccessors"    (fun () ->
            Expect.equal
                (testGraph.TrySuccessors 9)
                (Some [||])
                "TrySuccessors does not return the expected values"
            
            Expect.equal
                (testGraph.TrySuccessors 2)
                (Some [|5;8|])
                "TrySuccessors does not return the expected values"
        )

        testCase "Successors"       (fun () ->
            Expect.equal
                (testGraph.Successors 9)
                ([||])
                "Successors does not return the expected values"

            Expect.equal
                (testGraph.Successors 2)
                ([|5;8|])
                "Successors does not return the expected values"
        )

        testCase "TryNeighbours"    (fun () ->
            Expect.equal
                (testGraph.TryNeighbours 9)
                (Some [|0; 1; 4; 8|])
                "TryNeighbours does not return the expected values"
            
            Expect.equal
                (testGraph.TryNeighbours 2)
                (Some [|0; 1; 5; 8|])
                "TryNeighbours does not return the expected values"

        )


        testCase "Neighbours"       (fun () ->
            Expect.equal
                (testGraph.Neighbours 9)
                ([|0; 1; 4; 8|])
                "Neighbours does not return the expected values"
            
            Expect.equal
                (testGraph.Neighbours 2)
                ([|0; 1; 5; 8|])
                "Neighbours does not return the expected values"

        )
      

    ]
 
[<Tests>]
let testLabels =
    
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
        FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(vertexList,edges)
   
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
   
        FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)


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
                FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)

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


[<Tests>]
let testMeasures =
    let exampleGraphVertices =
        [ for i=0 to 5 do (i,i) ]
   
    let exampleGraphEdges =
        [
            1,2,1.0//1,2,3,4
            2,3,1.0//1,2,3
            3,4,1.0//1,2
            4,5,1.0//1
            4,1,1.0
        ]

    let exampleGraphEdgesWeighted =
        [
            1,2,2.0//1,2,3,4
            2,3,3.0//1,2,3
            3,2,1.0//1,2,3
            3,4,4.0//1,2
            4,5,1.0//1
            4,1,99.0
        ]
    let fig7Nodes = ["a","a"; "b","b";"c","c";"d","d";"e","e";]
    let fig7 = 
        [
            "a","b",10.0
            "b","c",1.0
            "b","d", 1.0
            "c","e",1.0
            "d","e",1.0
        ]

    let fig4Nodes = ["a","a"; "b","b";"c","c";"d","d";]
    let fig4 = 
        [
            "a","b",10.0
            "b","c",1.0
            "b","d", 1.0
            "c","d",1.0
        ]

    let fig4Connected = 
        [
        "a","b",10.0
        "b","c",1.0
        "b","d", 1.0
        "c","d",1.0
        "d","a",1.0
        ]

    let fig4Graph = FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(fig4Nodes,fig4)
    let fig4GraphConnected  = FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(fig4Nodes,fig4Connected)
    let fig7Graph = FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(fig7Nodes,fig7)
   
    let testGraph = FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdges)
    let weightedGraph = FSharp.FGL.ArrayAdjacencyGraph.ArrayAdjacencyGraph(exampleGraphVertices,exampleGraphEdgesWeighted)

    testList "Test Measures" [
        
        // Undirected / Unweighted 
        testCase "meanShortestPath" (fun () ->
            let msp = Measures.meanShortestUnDirected fig7Graph
            Expect.floatClose Accuracy.medium msp 1.6 "Incorrect mean shortest path does not return the correct label"
        )

        testCase "tryGetShortestPath" (fun () ->
            let sp = Measures.tryGetShortestPath  1 5 testGraph
            Expect.isSome sp "Incorrect shortest path"     
            Expect.equal sp.Value 2.0 "Incorrect shortest path"       
        )

        // Directed / Unweighted 
        testCase "meanShortestPathDirected" (fun () ->
            let msp = Measures.meanShortestPathDirected fig7Graph
            Expect.floatClose Accuracy.medium msp 1.5555555555 "Incorrect mean shortest path does not return the correct label"
        )

        testCase "tryGetShortestPathDirected" (fun () ->
            let sp = Measures.tryGetShortestPathDirected  1 5 testGraph
            Expect.isSome sp "Incorrect shortest path"     
            Expect.equal sp.Value 4.0 "Incorrect shortest path"       
        )

        testCase "tryGetShortestPathDirected - reverse path" (fun () ->
            let sp = Measures.tryGetShortestPathDirected  3 1 testGraph
            Expect.isSome sp "Incorrect shortest path"     
            Expect.equal sp.Value 2.0 "Incorrect shortest path"       
        )

        testCase "tryGetShortestPathDirected - No path exists" (fun () ->
            let sp = Measures.tryGetShortestPathDirected  5 1 testGraph
            Expect.isNone sp "Incorrect shortest path"       
        )

        // Directed / Weighted 
        testCase "meanShortestPathDirectedWeighted" (fun () ->
            let msp = Measures.meanShortestPathDirectedhWeighted fig7Graph
            Expect.floatClose Accuracy.medium msp 5.555555 "Incorrect mean shortest path does not return the correct label"
        )

        testCase "tryGetShortestPathDirectedWeighted" (fun () ->
            let sp = Measures.tryGetShortestPathDirectedhWeighted  "a" "c" fig7Graph
            Expect.isSome sp "Incorrect shortest path"     
            Expect.equal sp.Value 11.0 "Incorrect shortest path"       
        )

        //Vertex Mean shortest path
        testCase "meanShortestPathUnDirectedVertex - A" (fun () ->
            let sp = Measures.meanShortestPathUnDirectedVertex  "a" fig7Graph
            Expect.floatClose Accuracy.medium sp 2.0 "Incorrect shortest path" 
        )    
         
        testCase "meanShortestPathUnDirectedVertex - B" (fun () ->
            let sp = Measures.meanShortestPathUnDirectedVertex  "b" fig7Graph
            Expect.floatClose Accuracy.medium sp 1.25 "Incorrect shortest path"   
        )
        
        testCase "meanShortestPathUnDirectedVertex - C" (fun () ->
            let sp = Measures.meanShortestPathUnDirectedVertex  "c" fig7Graph   
            Expect.floatClose Accuracy.medium sp 1.5 "Incorrect shortest path"  
        )

        testCase "meanShortestPathUnDirectedVertex - D" (fun () ->
            let sp = Measures.meanShortestPathUnDirectedVertex  "d"  fig7Graph    
            Expect.floatClose Accuracy.medium sp 1.5 "Incorrect shortest path"  
        )

        testCase "meanShortestPathUnDirectedVertex - E" (fun () ->
            let sp = Measures.meanShortestPathUnDirectedVertex  "e" fig7Graph    
            Expect.floatClose Accuracy.medium sp 1.75 "Incorrect shortest path"   
        ) 

        testCase "meanShortestPathDirectedVertex - B" (fun () ->
            let sp = Measures.meanShortestPathDirectedVertex  "b" fig7Graph  
            Expect.floatClose Accuracy.medium sp 1.33333333333 "Incorrect shortest path"   
        ) 

        testCase "meanShortestPathDirectedhWeightedVertex - B" (fun () ->
            let sp = Measures.meanShortestPathDirectedhWeightedVertex  "a" fig7Graph    
            Expect.floatClose Accuracy.medium sp 11.0 "Incorrect shortest path"   
        ) 
       
        //Closeness
        testCase "getClosenessUnDirected-A" (fun () ->
            let sp = Measures.getClosenessUnDirected  "a" fig7Graph  
            Expect.floatClose Accuracy.medium sp 0.5 "Incorrect closeness"       
        )
        
        testCase "getClosenessUnDirected-B" (fun () ->
            let sp = Measures.getClosenessUnDirected  "b"   fig7Graph
            Expect.floatClose Accuracy.medium sp 0.8 "Incorrect closeness"       
        )

        testCase "getClosenessUnDirected-C" (fun () ->
            let sp = Measures.getClosenessUnDirected  "c"   fig7Graph
            Expect.floatClose Accuracy.medium sp 0.6666666666666666 "Incorrect closeness"       
        )

        testCase "getClosenessUnDirected-D" (fun () ->
            let sp = Measures.getClosenessUnDirected  "d"  fig7Graph
            Expect.floatClose Accuracy.medium sp 00.6666666666666666 "Incorrect closeness"       
        )

        testCase "getClosenessOutward" (fun () ->
            let sp = Measures.getClosenessOutward  "c"  fig7Graph
            Expect.floatClose Accuracy.medium sp 1.0 "Incorrect closeness"       
        )

        testCase "getClosenessInward" (fun () ->
            let sp = Measures.getClosenessInward  "e"  fig7Graph
            Expect.floatClose Accuracy.medium sp 0.5714285714285714 "Incorrect closeness"       
        )
        
        testCase "getNNeighborhoodConnectivity" (fun () ->
            let sp = Measures.getNeighborhoodConnectivity  "d"  fig7Graph
            Expect.floatClose Accuracy.medium sp 2.5 "Incorrect closeness"       
        )

        testCase "getClusteringCoefficient" (fun () ->
            let sp = Measures.getClusteringCoefficient  "b"  fig4Graph
            Expect.floatClose Accuracy.medium sp 0.333333 "ClusteringCoefficient"       
        )
   
        testCase "isNotStronglyConnected" (fun () ->
            let sp = Measures.isStronglyConnected  fig4Graph
            Expect.isFalse sp "depthFirstSearch"       
        )

        testCase "isStronglyConnected" (fun () ->
            let sp = Measures.isStronglyConnected  fig4GraphConnected
            Expect.isTrue sp "depthFirstSearch"       
        )

    ]
