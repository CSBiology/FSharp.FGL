module FSharp.FGL.Tests.NetCore

open Expecto

[<EntryPoint>]
let main argv =

    //FSharp.FGL core tests
    Tests.runTestsWithCLIArgs [] argv SomeTests.testGDFReaderFuntions |>ignore
    0