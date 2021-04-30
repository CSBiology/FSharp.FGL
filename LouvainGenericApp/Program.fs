// Learn more about F# at http://fsharp.org

open System
open FSharp.FGL
open Louvain
open Argu

type Files = 
    | Random50  
    | Random500
    | CElegans
    //| Marvel
    | Yeast

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Random50  _ -> "bla"
            | Random500 _ -> "bla"
            | CElegans  _ -> "bla"
            //| Marvel    _ -> "bla"
            | Yeast     _ -> "bla"

type LouvainArguments =
    | N of int
    | [<Mandatory>] File of Files

    interface IArgParserTemplate with
           member s.Usage =
               match s with
               | N _ -> "how often it runs"
               | File _ -> "which file to run"
              
[<EntryPoint>]
let main argv = 
    let parser =  ArgumentParser.Create<LouvainArguments>()
    // pass the reader to the Parse call
    let results = parser.Parse(argv)
     
    
    let n = results.TryGetResult(LouvainArguments.N) |> Option.defaultValue 1
    let network = 
        match results.TryGetResult(LouvainArguments.File).Value with
        | Random50  -> Sources.source50
        | Random500 -> Sources.source500
        | CElegans  -> Sources.sourceCelegans
        //| Marvel    -> Sources.sourceMarvel 
        | Yeast     -> Sources.sourceYeast

  
    for i = 0 to n do
        printfn "%i" i 
        Louvain.louvainInPlace network
        |> ignore
    
    0

