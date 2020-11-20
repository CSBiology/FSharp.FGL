namespace FSharp.FGL.IO

open Aether
open FSharp.FGL
open System.Text

module GDF =

    //Typedefinitions to distinguish Vertex data and Edge data 
    type VertexInfo =
        | VertexId      of string
        | Lable         of string
        | Class         of string
        | Visible       of bool
        | LableVisible  of bool
        | Width         of float
        | Height        of float
        | X             of float
        | Y             of float
        | ColorVertex   of string
    type EdgesInfo =
        | EdgeId1           of string
        | EdgeId2           of string
        | IsEdgeDirected    of bool
        | ColorEdge         of string
        | Weight            of float

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

    //returns Function appropriate to the header string of Vertex
    let private getVertexInfoMapper (headerValue:string) =    
            match headerValue.Trim() with 
                |   "name VARCHAR"          ->  (fun x -> match x with |" "| "" ->  VertexId    "DefaultValue"  |_ -> VertexId x)      
                |   "label VARCHAR"         ->  (fun x -> match x with |" "| "" ->  Lable       "DefaultValue"  |_ -> Lable x)                                           
                |   "class VARCHAR"         ->  (fun x -> match x with |" "| "" ->  Class       "DefaultValue"  |_ -> Class x)                                                                           
                |   "color VARCHAR"         ->  (fun x -> match x with |" "| "" ->  ColorVertex "Default Value" |_ -> ColorVertex x) 
                |   "visible BOOLEAN"       ->  (fun x -> match x with |" "| "" ->  Visible false       |"true" -> Visible      true|"false" -> Visible      false|_ -> failwith"unknown value in visible")                                           
                |   "labelvisible BOOLEAN"  ->  (fun x -> match x with |" "| "" ->  LableVisible false  |"true" -> LableVisible true|"false" -> LableVisible false|_ -> failwith"unknown value in labelvisible")                                           
                |   "width DOUBLE"          ->  (fun x -> match x with |" "| "" ->  Width   0.0                 |_ -> Width (float x))                                            
                |   "height DOUBLE"         ->  (fun x -> match x with |" "| "" ->  Height  0.0                 |_ -> Height (float x))                                          
                |   "x DOUBLE"              ->  (fun x -> match x with |" "| "" ->  X       0.0                 |_ -> X (float x))                                            
                |   "y DOUBLE"              ->  (fun x -> match x with |" "| "" ->  Y       0.0                 |_ -> Y (float x))                                                                                       
                |   _                       ->  (failwith "unknown Variable in Vertex header")

    //returns Function appropriate to the header string of Edge
    let private getEdgeInfoMapper (headerValue:string) =    
        match headerValue with 
            |   "node1 VARCHAR"         ->  (fun x -> match x with |" "| "" ->  EdgeId1     "DefaultValue"  |_ -> EdgeId1 x)                                                   
            |   "node2 VARCHAR"         ->  (fun x -> match x with |" "| "" ->  EdgeId2     "DefaultValue"  |_ -> EdgeId2 x)        
            |   "color VARCHAR"         ->  (fun x -> match x with |" "| "" ->  ColorEdge   "DefaultValue"  |_ -> ColorEdge x)       
            |   "directed BOOLEAN"      ->  (fun x -> match x with |" "| "" ->  IsEdgeDirected false        |"true" -> IsEdgeDirected    true|"false" -> IsEdgeDirected      false| _ -> failwith "unknown value in directed")                                    
            |   "weight DOUBLE"         ->  (fun x -> match x with |" "| "" ->  Weight       0.0            |_ -> Weight (float x))                                
            |   _                       ->  (failwith "unknown Variable in Edge header")

    //Transforms VertexInfo[] into LVertex<string,VertexInfo[]>, fst-> VertexId as string, snd -> keeping type annotations in the VertexInfo[]          
    let private getVertexOfInfos (infos : VertexInfo []) : LVertex<string,VertexInfo []> =
        let vertexId = 
            infos 
            |> Array.tryPick (fun info -> 
                match info with 
                | VertexId x    -> Some x 
                | _             -> None
            )

        match vertexId with
        | Some id -> id,infos
        | None -> failwithf "vertex %O does not contain any identifier" infos

    //Uses the given header to apply getVertexInfoMapper functions on the chosen line
    let private getVertexByHeader (header:string) (line:string) = 
        header.Split ','
        |> Array.map (getVertexInfoMapper)
        |> fun f -> 
            line 
            |> splitElementInfos
            |> Array.map2 (fun f x -> f x) f
            |> getVertexOfInfos

    //Transforms EdgeInfo[] into LEdge<string,EdgesInfo[]>, fst-> VertexId of the source Vertex as string, snd -> VertexId of the target Vertex as string, trd -> keeping type annotations in the EdgeInfo[]          
    let private getEdgeOfinfos (infos : EdgesInfo[]) : LEdge<string,EdgesInfo[]> =
        let vertexId1 = 
            infos 
            |> Array.tryPick (fun info -> 
                match info with 
                | EdgeId1 x     -> Some x 
                | _             -> None
            )
        let vertexId2 =
           infos 
           |> Array.tryPick (fun info -> 
               match info with 
               | EdgeId2 x     -> Some x 
               | _             -> None
           ) 
        match vertexId1,vertexId2 with
        | Some id1,Some id2 -> id1,id2,infos
        | Some _ , None     -> failwithf "edge %O does not contain a target vertex" infos
        | None, Some _      -> failwithf "edge %O does not contain a source vertex" infos
        | None,None         -> failwithf "edge %O does not contain any vertex ids" infos

    //Uses the given header to apply getEdgeInfoMapper functions on the chosen line
    let private getEdgeByHeader (header:string) (line:string) = 
        header.Split ','
        |> Array.map (getEdgeInfoMapper)
        |> fun f -> 
            line 
            |> splitElementInfos
            |> Array.map2 (fun f x -> f x) f
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

        let rec loop vertexF edgeF (vertices: LVertex<string,VertexInfo[]>list) (edges: LEdge<string,EdgesInfo[]>list) (i: int) =
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
        |> Array.ofSeq
        |> getVertexAndEdgeFromData