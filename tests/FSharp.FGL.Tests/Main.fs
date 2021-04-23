module FSharp.FGL.Tests.NetCore

open Expecto

[<EntryPoint>]
let main argv =

    //FSharp.FGL core tests
    Tests.runTestsWithCLIArgs [] argv IOTests.testGDFReaderFuntions |>ignore
    Tests.runTestsWithCLIArgs [] argv IOTests.testGDFWriterFuntions |>ignore
    0
