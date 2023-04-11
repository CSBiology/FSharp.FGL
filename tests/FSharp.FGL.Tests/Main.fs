module FSharp.FGL.Tests.NetCore

open Expecto

[<EntryPoint>]
let main argv =

    //FSharp.FGL core tests
    Tests.runTestsWithCLIArgs [] argv IOTests.testGDFReaderFuntions                 |>ignore
    Tests.runTestsWithCLIArgs [] argv IOTests.testGDFWriterFuntions                 |>ignore
    Tests.runTestsWithCLIArgs [] argv ArrayAdjacencyGraphTests.testInitializeGraph  |>ignore
    Tests.runTestsWithCLIArgs [] argv ArrayAdjacencyGraphTests.testEdges            |>ignore
    Tests.runTestsWithCLIArgs [] argv ArrayAdjacencyGraphTests.testVertices         |>ignore
    Tests.runTestsWithCLIArgs [] argv ArrayAdjacencyGraphTests.testLabels           |>ignore
    Tests.runTestsWithCLIArgs [] argv ArrayAdjacencyGraphTests.testMeasures         |>ignore
    0
