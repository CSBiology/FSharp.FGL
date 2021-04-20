module IOTests

open FSharp.FGL
open FSharp.FGL.IO  
open Expecto


[<Tests>]
let testGDFReaderFuntions =
    
    let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
    let testReadFilePath = System.IO.Path.Combine(testDirectory,"TestFileRead.txt")

    testList "GDFReader" [
        testCase "fromFileEqualToFromArray" (fun () ->
            let testfromArray = 
                GDF.fromArray [|"nodedef>name VARCHAR,label VARCHAR,class VARCHAR,visible BOOLEAN,labelvisible BOOLEAN,width DOUBLE,height DOUBLE,x DOUBLE,y DOUBLE,color VARCHAR";"s1,SiteA,blog,true,true,10.0,10.0,-52.11296,-25.921143,'114,116,177'";"s2,SiteB,forum,true,true,10.986123,10.986123,-20.114172,25.740356,'219,116,251'";"s3,SiteC,webpage,true,true,10.986123,10.986123,8.598924,-26.867584,'192,208,223'";"edgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,color VARCHAR";"s1,s2,true,'114,116,177'";"s2,s3,true,'219,116,251'";"s3,s2,true,'192,208,223'";"s3,s1,true,'192,208,223'"|]
            
            let testFromFile =
                GDF.fromFile testReadFilePath
            
            Expect.equal
                testfromArray
                testFromFile
                "fromArray yields a differnt outcome as fromFile"
        )
        testCase "getTypeInfoMapperDefaultValues" (fun () ->
            let testCompleteArray = 
                [|
                    "nodedef>name VARCHAR,att1 VARCHAR,att2 BOOLEAN,att3 INT,att4 DOUBLE";
                    "s1, ,false,0,0.0";
                    "s2, ,true,1,2.0";
                    "s2,test2,false,1,2.0";
                    "s3,test3,true,0,3.0";
                    "s4,test4,false,3,0.0";
                    "s5,test5,true,3,2.0";
                    "edgedef>node1 VARCHAR,node2 VARCHAR,att2 BOOLEAN,att3 INT, att4 DOUBLE";
                    "s1,s2,true,1,1.0";
                    "s2,s3,false,1,1.0";
                    "s3,s2,true,2,2.0"
                |]
            
            let testEmptyArray =
                [|
                    "nodedef>name VARCHAR,att1 VARCHAR,att2 BOOLEAN,att3 INT,att4 DOUBLE";
                    "s1,,,,";
                    "s2,,true,1,2.0";
                    "s2,test2,,1,2.0";
                    "s3,test3,true, ,3.0";
                    "s4,test4,false,3, ";
                    "s5,test5,true,3,2.0";
                    "edgedef>node1 VARCHAR,node2 VARCHAR,att2 BOOLEAN,att3 INT, att4 DOUBLE";
                    "s1,s2,true,1,1.0";
                    "s2,s3,false,1,1.0";
                    "s3,s2,true,2,2.0"
                |]
        
            Expect.equal
                (GDF.fromArray testCompleteArray)
                (GDF.fromArray testEmptyArray)
                "getTypeInfoMapper does not fill in the expected default values"
        )
    ]

[<Tests>]
let testGDFWriterFuntions =
    
    let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
    let testReadFilePath = System.IO.Path.Combine(testDirectory,"TestFileRead.txt")
    let testWriteFilePath = System.IO.Path.Combine(testDirectory,"TestFileWrite.txt")
    let testWriteFilePath2 = System.IO.Path.Combine(testDirectory,"TestFileWrite2.txt")

    testList "GDFWriter"[
        testCase "IsWrittenFileIdenticalToInformationInSource" (fun () ->

            let testRead = GDF.fromFile testReadFilePath

            let testGraph = Directed.Graph.create (fst testRead) (snd testRead)

            let testWrite = GDF.toFile testGraph true testWriteFilePath

            let testWriteRead = GDF.fromFile testWriteFilePath

            let testGraphWritten = Directed.Graph.create (fst testWriteRead) (snd testWriteRead)

            Expect.equal
                testGraph
                testGraphWritten
                "written file does not equal the source file"
        )
        testCase "CanMissingValuesBeAdded" (fun () ->
            
            let testRead                = GDF.fromFile testReadFilePath
            let (vertexList,edgeList)   = testRead
            let testGraph               = Directed.Graph.create vertexList edgeList

            let (vertexListHead,vertexListTail) = (List.head vertexList),(List.tail vertexList)
            let (edgeListHead,edgeListTail) = (List.head edgeList),(List.tail edgeList)

            let modifiedVertexHead = 
                let (vertexId,lableMap) = vertexListHead 
                let updatedLableMap = lableMap |> Map.toList |> List.tail |> Map.ofList
                (vertexId,updatedLableMap)

            let modifiedEdgeHead =
                let (edgeId1,edgeId2,lableMap) = edgeListHead 
                let updatedLableMap = lableMap |> Map.toList |> List.tail |> Map.ofList
                (edgeId1,edgeId2,updatedLableMap)

            let vertexListUpdated   = modifiedVertexHead::vertexListTail
            let edgeListUpdated     = modifiedEdgeHead::edgeListTail

            let testGraphUpdated = Directed.Graph.create vertexListUpdated edgeListUpdated

            let testWrite = GDF.toFile testGraphUpdated true testWriteFilePath2
        
            let testRead2 = GDF.fromFile testWriteFilePath2
            let (vertexList2,edgeList2)   = testRead2



            Expect.equal
                (vertexList     |> List.head |> snd |> Map.toList |> List.head |> fst)
                (vertexList2    |> List.head |> snd |> Map.toList |> List.head |> fst)
                "written graph did not add missing values that were noted in the header"
        )
    ]
