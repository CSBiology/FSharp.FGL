module SomeTests

open FSharp.FGL
open FSharp.FGL.IO  
open Expecto


[<Tests>]
let testGDFReaderFuntions =
    testList "GDFReader" [
        testCase "fromFileEqualToFromArray" (fun () ->
            let testfromArray = 
                GDF.fromArray [|"nodedef>name VARCHAR,label VARCHAR,class VARCHAR,visible BOOLEAN,labelvisible BOOLEAN,width DOUBLE,height DOUBLE,x DOUBLE,y DOUBLE,color VARCHAR";"s1,SiteA,blog,true,true,10.0,10.0,-52.11296,-25.921143,'114,116,177'";"s2,SiteB,forum,true,true,10.986123,10.986123,-20.114172,25.740356,'219,116,251'";"s3,SiteC,webpage,true,true,10.986123,10.986123,8.598924,-26.867584,'192,208,223'";"edgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,color VARCHAR";"s1,s2,true,'114,116,177'";"s2,s3,true,'219,116,251'";"s3,s2,true,'192,208,223'";"s3,s1,true,'192,208,223'"|]
            
            let testDirectory = __SOURCE_DIRECTORY__ + @"/TestFiles/"
            let testReadPath = testDirectory+"/TestFileRead.txt"

            let testFromFile =
                GDF.fromFile testReadPath
            
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
                    "s1, , , , ";
                    "s2, ,true,1,2.0";
                    "s2,test2, ,1,2.0";
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
