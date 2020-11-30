namespace FSharp.FGL.IO

open Aether
open FSharp.FGL
open System.Text


module GDF =
    
    //Typedefinition, later used to changes the type of the associated value to the correct type.
    type TypeInfo =
        | VARCHAR   of string
        | BOOLEAN   of bool
        | DOUBLE    of float
        | INT       of int

    //Transforms string into string [], deleting quotes in the progress and splitting at commas
    let private splitElementInfos (line:string) =
        let chars = line.ToCharArray()

        let rec stringDeconstruction insideQuote i vertexInfos (sb:StringBuilder) =
            match chars.[i] with
            // Handle quote marks
            | '\'' when i = chars.Length-1  -> sb.ToString() :: vertexInfos
            | '\'' when insideQuote         -> stringDeconstruction false  (i+1) vertexInfos (sb)
            | '\''                          -> stringDeconstruction true (i+1) vertexInfos (sb)
            // Handle commas
            | ',' when insideQuote          -> stringDeconstruction insideQuote (i+1) vertexInfos (sb.Append ',')
            | ','                           -> stringDeconstruction insideQuote (i+1) (sb.ToString() :: vertexInfos) (sb.Clear())
            // Handle every other symbol
            | c when i = chars.Length-1     -> sb.Append(c).ToString() :: vertexInfos
            | c                             -> stringDeconstruction insideQuote (i+1) vertexInfos (sb.Append c)

        stringDeconstruction false 0 [] (StringBuilder())
        |> List.rev
        |> List.toArray

    //Returns a appropite function based on the value of the header, to change the corresponding values to their respective TypeDefinitoions type.
    let private getTypeInfoMapper (headerValue:string) =
        if      headerValue.Trim().Contains "VARCHAR"   then (fun x -> match x with |" "| "" ->  VARCHAR "DefaultValue"     |_ -> VARCHAR x)
        elif    headerValue.Trim().Contains "INT"       then (fun x -> match x with |" "| "" ->  INT   1                    |_ -> INT    (int x))
        elif    headerValue.Trim().Contains "DOUBLE"    then (fun x -> match x with |" "| "" ->  DOUBLE   0.0               |_ -> DOUBLE (float x))
        elif    headerValue.Trim().Contains "BOOLEAN"   then (fun x -> match x with |" "| "" ->  BOOLEAN true               |"true" -> BOOLEAN      true|"false" -> BOOLEAN      false|_ -> failwith"unknown value in visible")
        else failwith "unknown typeAnnotation in header"

    //Returns header withput the type annotations or the def>
    let private headerTrim (header:string) =
        header.Split ','
        |>Array.map(fun x ->x.Split ' ')
        |>Array.map(Array.filter(fun x ->not (x.Contains "VARCHAR")))
        |>Array.map(Array.filter(fun x ->not (x.Contains "DOUBLE")))
        |>Array.map(Array.filter(fun x ->not (x.Contains "BOOLEAN")))
        |>Array.map(Array.filter(fun x ->not (x.Contains "INT")))
        |>Array.map(Array.item 0)
        |>Array.map(fun x ->x.Split '>')
        |>Array.map(Array.filter(fun x ->not (x.Contains "def")))
        |>Array.map(Array.item 0)

    //Reconstructs the intended Type of var x from TypeInfo         
    let private reconstructTypeInfo (value:TypeInfo) =    
           match value with
               | VARCHAR     x   -> x
               | BOOLEAN     x   -> sprintf "%b" x
               | DOUBLE      x   -> sprintf "%f" x
               | INT         x   -> sprintf "%i" x              

    //Searches for the "name" identifier of the Vertex Map(header,TypeDefiniton), if found returns (string name),Map tupel
    let private getVertexOfInfos (infos : Map<string,TypeInfo>) : LVertex<string,Map<string,TypeInfo>> =
        let vertexId = 
            infos.TryFind "name"

        match vertexId with
        | Some id -> (reconstructTypeInfo id),(infos)
        | None -> failwithf "vertex %O does not contain any identifier" infos

    //Takes header and a line of string, applies getTypeInfoMapper on header, and applies these functions on the line contents. Builds a Map(header,TypeInfo), which is used for getVertedOfInfos 
    let private getVertexByHeader (header:string) (line:string) = 
        header.Split ','
        |> Array.map (getTypeInfoMapper)
        |> fun f -> 
            line 
            |> splitElementInfos
            |> Array.map2 (fun f x -> f x) f
            |> Array.map2 (fun a b -> a,b) (headerTrim header)
            |> Map.ofArray
            |> getVertexOfInfos

    //Searches for the "node1" and "node2" identifier of the Edge Map(header,TypeDefiniton), if found returns (string node1)(string node2),Map tripel       
    let private getEdgeOfinfos (infos : Map<string,TypeInfo>) : LEdge<string,Map<string,TypeInfo>> =
        let vertexId1 = 
            infos.TryFind "node1" 

        let vertexId2 =
            infos.TryFind "node2" 

        match vertexId1,vertexId2 with
        | Some id1,Some id2 -> reconstructTypeInfo id1,reconstructTypeInfo id2,infos
        | Some _ , None     -> failwithf "edge %O does not contain a target vertex" infos
        | None, Some _      -> failwithf "edge %O does not contain a source vertex" infos
        | None,None         -> failwithf "edge %O does not contain any vertex ids" infos

    //Takes header and a line of string, applies getTypeInfoMapper on header, and applies these functions on the line contents. Builds a Map(header,TypeInfo), which is used for getEdgeOfInfos 
    let private getEdgeByHeader (header:string) (line:string) = 
        header.Split ','
        |> Array.map (getTypeInfoMapper)
        |> fun f -> 
            line 
            |>splitElementInfos
            |> Array.map2 (fun f x -> f x) f
            |> Array.map2 (fun a b -> a,b) (headerTrim header)
            |> Map.ofArray
            |> getEdgeOfinfos

    //activePatternMatching, recognizes VertexHeader and returns getVertexByHeader applied on the header
    let private (|VertexHeader|_|) (s:string) =
        let r = System.Text.RegularExpressions.Regex.Match(s,"(?<=nodedef>).*")
        if r.Success then Some(getVertexByHeader r.Value)
        else None

    //activePatternMatching, recognizes EdgeHeader and returns getEdgeByHeader applied on the header
    let private (|EdgeHeader|_|) (s:string) =
        let r = System.Text.RegularExpressions.Regex.Match(s,"(?<=edgedef>).*")
        if r.Success then Some(getEdgeByHeader r.Value)
        else None
    

    //Takes string [], recognizes if data belongs to 'Vertex or 'Edge, and returns a tupel of vertices and edges
    let private getVertexAndEdgeFromData (data: string []) =

        let rec loop vertexF edgeF (vertices: LVertex<string,Map<string,TypeInfo>>list) (edges: LEdge<string,Map<string,TypeInfo>>list) (i: int) =
            match data.[i],vertexF,edgeF with
                //Checking for header
                | VertexHeader  vertexF, _, _             ->  loop (Some vertexF) None     vertices edges (i+1)
                | EdgeHeader    edgeF, _, _               ->  loop None (Some edgeF)       vertices edges (i+1)
                
                //End of recursive function
                | vertex,Some f,None when i = (data.Length - 1)   ->  (f vertex :: vertices) |> List.rev,(edges) |> List.rev
                | edge,None,Some f   when i = (data.Length - 1)   ->  (vertices)|> List.rev,(f edge :: edges) |> List.rev
            
                //Applying chosen header function until new header is applied
                | vertex,Some f,None                                ->  loop vertexF edgeF (f vertex :: vertices) (edges) (i+1)
                | edge,None,Some f                                  ->  loop vertexF edgeF vertices (f edge :: edges) (i+1)
            
                | _                                                 -> failwith "Error in getVertexAndEdgeFromData"
    
        loop None None [] [] 0

    //Reads data from a string sequence and returns a tupel of vertices and edges 
    let fromSeq (sequence: string seq) =
        Array.ofSeq sequence
        |> getVertexAndEdgeFromData

    //Reads data from the given path txt file and returns a tupel of vertices and edges
    let fromFile (path:string) =
        path
        |> System.IO.File.ReadAllLines
        |> fromSeq
