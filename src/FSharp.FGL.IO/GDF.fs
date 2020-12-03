namespace FSharp.FGL.IO

open Aether
open FSharp.FGL
open System.Text


module GDF =
    
    //Typedefinition, later used to changes the type of the associated value to the correct type.
    type GDFValue =
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
        let headerValueId = (headerValue.Split ' '|> Array.item 0)
        if      headerValue.Trim().Contains "VARCHAR"   then (fun x -> match x with |" "| "" ->  headerValueId,(VARCHAR "DefaultValue")                |_ -> headerValueId,(VARCHAR x))
        elif    headerValue.Trim().Contains "INT"       then (fun x -> match x with |" "| "" ->  headerValueId,(INT 1)                                 |_ -> headerValueId,(INT (int x)))
        elif    headerValue.Trim().Contains "DOUBLE"    then (fun x -> match x with |" "| "" ->  headerValueId,(DOUBLE 0.0)                            |_ -> (headerValueId,(DOUBLE (float x))))
        elif    headerValue.Trim().Contains "BOOLEAN"   then (fun x -> match x with |" "| "" ->  headerValueId,(BOOLEAN true)                          |"true" -> headerValueId,(BOOLEAN true)|"false" -> headerValueId,(BOOLEAN false)|_ -> failwith"unknown value in visible")
        else failwith "unknown typeAnnotation in header"

    //Reconstructs the intended Type of var x from TypeInfo         
    let private gdfValueToString (value:GDFValue) =    
           match value with
               | VARCHAR     x   -> x
               | BOOLEAN     x   -> sprintf "%b" x
               | DOUBLE      x   -> sprintf "%f" x
               | INT         x   -> sprintf "%i" x              

    //Searches for the "name" identifier of the Vertex Map(header,TypeDefiniton), if found returns (string name),Map tupel
    let private getVertexOfInfos (infos : Map<string,GDFValue>) : LVertex<GDFValue,Map<string,GDFValue>> =
        let vertexId = 
            infos.TryFind "name"

        match vertexId with
        | Some id -> id,(infos)
        | None -> failwithf "vertex %O does not contain any identifier" infos

    //Takes header and a line of string, applies getTypeInfoMapper on header, and applies these functions on the line contents. Builds a Map(header,TypeInfo), which is used for getVertedOfInfos 
    let private getVertexByHeader (header:string) (line:string) = 
        header.Split ','
        |> Array.map (getTypeInfoMapper)
        |> fun f -> 
            line 
            |> splitElementInfos
            |> Array.map2 (fun f x -> f x) f
            |> Map.ofArray
            |> getVertexOfInfos

    //Searches for the "node1" and "node2" identifier of the Edge Map(header,TypeDefiniton), if found returns (string node1)(string node2),Map tripel       
    let private getEdgeOfinfos (infos : Map<string,GDFValue>) : LEdge<GDFValue,Map<string,GDFValue>> =
        let vertexId1 = 
            infos.TryFind "node1" 

        let vertexId2 =
            infos.TryFind "node2" 

        match vertexId1,vertexId2 with
        | Some id1,Some id2 -> id1, id2,infos
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
    
    //Reads data from a string sequence and returns a tupel of vertices and edges
    //Takes string [], recognizes if data belongs to 'Vertex or 'Edge, and returns a tupel of vertices and edges
    let fromArray (data: string []) =

        let rec loop vertexF edgeF (vertices: LVertex<GDFValue,Map<string,GDFValue>>list) (edges: LEdge<GDFValue,Map<string,GDFValue>>list) (i: int) =
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

    //Reads data from the given path txt file and returns a tupel of vertices and edges
    let fromFile (path:string) =
        path
        |> System.IO.File.ReadAllLines
        |> fromArray


    //Returns the GDFValue-type as string
    let typeOfGDFValue (value:GDFValue) =    
        match value with
            | VARCHAR     x   -> "VARCHAR"
            | BOOLEAN     x   -> "BOOLEAN"
            | DOUBLE      x   -> "DOUBLE"
            | INT         x   -> "INT"      

    //Applies typeOfGDFValue on all GDF Values of a given Map
    let private getGDFValue (value:Map<'key,GDFValue>) =
        let valueList = Map.toList value|>List.map(snd)
        List.map(typeOfGDFValue) valueList

    //Adds quotationmarks at the beginning and the end of strings
    let private addQuotationsToString (x:string) : string =
        let sb = StringBuilder()
        let charList = x.ToCharArray()|>List.ofArray
        let updatedCharList = [''']@charList@[''']
        for i in updatedCharList do sb.Append(i)
        sb.ToString()

    //Checks if a string contains commas, if true then applies addQuotationsToString
    let private addQuotationsIfNeeded (x:string) :string =
        if x.Contains "'" then addQuotationsToString x
        else x

    //Reconstructs the header of an edge or node based on a Map<string,GDFValue> 
    let private reconstructHeader (value:Map<string,GDFValue>) =
        let headerString    =   value|>Map.toList|>List.map(fst)
        let headerTypes     =   value|>getGDFValue 
        let header          =   List.map2 (fun a b -> (a)+" "+(b)) headerString headerTypes
        let defIndex        =   header|>List.findIndex(fun x -> x.Contains "def>")
        let headerSortedNew =   ((header.[defIndex])::header)|>List.distinct
        String.concat ";" headerSortedNew

    //Reconstructs vertices or edges based on LVertex or LEdge
    let private reconstructVertices (value:LVertex<GDFValue,Map<string,GDFValue>>)     =
        let index               =   ("nodedef>name",(fst value))        
        let valueList           =   index::(snd value|>Map.toList)|>List.distinct
        let updatedValueList    =   valueList|>List.map(snd)|>List.map(fun x -> gdfValueToString x)
        let nameIndex           =   valueList|>List.map(fst)|>List.findIndex (fun x -> x.Contains "def>")
        let valueListSortedNew  =   ((updatedValueList.[nameIndex])::updatedValueList)|>List.distinct|> List.map(addQuotationsIfNeeded)
        String.concat ";" valueListSortedNew
    let private reconstructEdges (value:LEdge<GDFValue,Map<string,GDFValue>>)     =
        let index1              =   ("edgedef>node1"),((fun (a,b,c) -> a) value)
        let index2              =   ("node2"),((fun (a,b,c) -> b) value)
        let valueList           =   index1::index2::((fun (a,b,c) -> c) value|>Map.toList)|>List.distinct
        let updatedValueList    =   valueList|>List.map(snd)|>List.map(fun x -> gdfValueToString x)
        let nameIndex           =   valueList|>List.map(fst)|>List.findIndex (fun x -> x.Contains "def>")
        let valueListSortedNew  =   ((updatedValueList.[nameIndex])::updatedValueList)|>List.distinct|> List.map(addQuotationsIfNeeded)
        String.concat ";" valueListSortedNew

    ////WIP, searches for values, that are described in the header but are not present in the value list and adds them as an empty string
    //let addEmptyValuesIfNeeded (value:(string*GDFValue) list) (header:string)=
    //    let headerList :string list     =   header.Split ";"|>List.ofArray|>List.sort
    //    let valueGDFTypes :string list  =   List.map fst value|>List.sort
    //    if headerList = valueGDFTypes then
    //        value
    //    else
    //       ((),())::value|>List.sort
       
    //Constructs an Array containing vertex header,vertices,edge header and edges based on an LVertex list and an LEdge list
    let private vertexListEdgeListToStringArray (vertexList:LVertex<GDFValue,Map<string,GDFValue>> list) (edgeList:LEdge<GDFValue,Map<string,GDFValue>> list) : string[]=
        let vertexHeader    = List.map snd vertexList|>List.map reconstructHeader|>List.distinct|>String.concat ";"
        let edgeHeader      = List.map (fun (a,b,c) ->c) edgeList|>List.map reconstructHeader|>List.distinct|>String.concat ";"
        let vertices        = List.map reconstructVertices vertexList
        let edges           = List.map reconstructEdges edgeList
        let verticesWithHeaders = vertexHeader::vertices
        let edgesWithHeaders   = edgeHeader::edges
        verticesWithHeaders@edgesWithHeaders|>List.toArray

    //Takes a graph, transforms it into an array containing all relevant information and writes it into a file indicated by the path
    let toFile (graph:Graph<'Vertex,'Lable,'Edge>) (edgesDirected:bool) (path:string) =
        let vertexList  = Vertices.toVertexList graph
        let edgeList    = match edgesDirected with 
            |true   -> Directed.Edges.toEdgeList graph
            |_      -> Directed.Edges.toEdgeList graph(***Add function, that removes the return edges?***)  
        let content = vertexListEdgeListToStringArray vertexList edgeList|> String.concat ";"
        System.IO.File.AppendAllText(path,content)
