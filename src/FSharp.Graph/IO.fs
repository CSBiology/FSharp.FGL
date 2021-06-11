namespace FSharp.Graph.IO

open System.Text
open FSharp.Graph

module GDF =
    
    //Typedefinition, later used to changes the type of the associated value to the correct type.
    type GDFValue =
        | VARCHAR   of string
        | BOOLEAN   of bool
        | DOUBLE    of float
        | INTEGER   of int

    //Transforms string into string [], deleting quotes in the progress and splitting at commas
    let private splitElementInfos (line:string) =
        let chars = line.ToCharArray()

        let rec stringDeconstruction insideQuote i vertexInfos (sb:StringBuilder) =
            match chars.[i] with
            // Handle quote marks
            | '\'' when i = chars.Length-1  -> sb.ToString() :: vertexInfos
            | '\'' when insideQuote         -> stringDeconstruction false  (i+1) vertexInfos (sb)
            | '\''                          -> stringDeconstruction true  (i+1) vertexInfos (sb)
            // Handle commas
            | ',' when insideQuote          -> stringDeconstruction insideQuote  (i+1) vertexInfos (sb.Append ',')
            | ',' when i = chars.Length-1   -> sb.ToString() :: "" :: vertexInfos
            | ','                           -> stringDeconstruction insideQuote  (i+1) (sb.ToString() :: vertexInfos) (sb.Clear())
            // Handle every other symbol
            | c when i = chars.Length-1     -> sb.Append(c).ToString() :: vertexInfos
            | c                             -> stringDeconstruction insideQuote  (i+1) vertexInfos (sb.Append c)

        stringDeconstruction false 0 [] (StringBuilder())
        |> List.rev
        |> List.toArray

   //Returns a appropite function based on the value of the header, to change the corresponding values to their respective TypeDefinitoions type.
    let private getTypeInfoMapper (headerValue:string) =
        let headerValueId = ( headerValue.Trim().Split ' ' |> Array.item 0)

        if      headerValue.Trim().Contains "VARCHAR"                                       then (fun x -> match x with |" "| "" ->  headerValueId,(VARCHAR "")                 |_ -> headerValueId,(VARCHAR x))
        elif    headerValue.Trim().Contains "INT"                                           then (fun x -> match x with |" "| "" ->  headerValueId,(INTEGER 0)                  |_ -> headerValueId,(INTEGER (int x)))
        elif    headerValue.Trim().Contains "DOUBLE"                                        then (fun x -> match x with |" "| "" ->  headerValueId,(DOUBLE 0.0)                 |_ -> (headerValueId,(DOUBLE (float x))))
        elif    headerValue.Trim().Contains "BOOLEAN"                                       then (fun x -> match x with |" "| "" ->  headerValueId,(BOOLEAN false)              |"true" -> headerValueId,(BOOLEAN true)|"false" -> headerValueId,(BOOLEAN false)|_ -> failwith"unknown value in visible")
        elif    headerValue.Trim().Contains "node1" ||headerValue.Trim().Contains "node2"   then (fun x -> match x with |" "| "" ->  headerValueId,(VARCHAR "")                 |_ -> headerValueId,(VARCHAR x))
        else failwith "unknown typeAnnotation in header"

    //Reconstructs the intended Type of var x from TypeInfo         
    let private gdfValueToString (value:GDFValue) =    
           match value with
               | VARCHAR    x   -> x
               | BOOLEAN    x   -> sprintf "%b" x
               | DOUBLE     x   -> sprintf "%f" x
               | INTEGER    x   -> sprintf "%i" x              

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
    let private gdfTypeToString (value:GDFValue) =    
        match value with
            | VARCHAR       x   -> "VARCHAR"
            | BOOLEAN       x   -> "BOOLEAN"
            | DOUBLE        x   -> "DOUBLE"
            | INTEGER       x   -> "INT"      

    //Applies typeOfGDFValue on all GDF Values of a given Map
    let private getGDFValue (value:Map<'key,GDFValue>) =
        let valueList = Map.toList value|>List.map(snd)
        List.map(gdfTypeToString) valueList    

    //Checks if a string contains commas, if true then applies addQuotationsToString
    let private addQuotationsIfNeeded (x:string) :string =
        if x.Contains "," then sprintf "'%s'" x
        else x

    //Reconstructs the header of an edge or node based on a Map<string,GDFValue> 
    let private reconstructVerticesHeader (vertices:LVertex<GDFValue,Map<string,GDFValue>>list) =
        vertices 
        |>List.collect (snd >> Map.toList)
        |>List.map(fun (a,b)-> (a,gdfTypeToString b))
        |>Map.ofList

    let private reconstructEdgesHeader (edges:LEdge<GDFValue,Map<string,GDFValue>>list) =
        edges 
        |>List.collect ((fun (a,b,c)->c) >> Map.toList)
        |>List.map(fun (a,b)-> (a,gdfTypeToString b))
        |>Map.ofList

    let private headerMapToString (headerMap:Map<string,string>)=     
        headerMap
        |>Map.toList
        |>List.map(fun (a,b) -> a+" "+b)  
            

    //Returns the appropriate default value to the given GDF Type
    let private getGDFDefaultValue (headerValue:string) =
        if      headerValue.Trim().Contains "VARCHAR"   then (VARCHAR " ") 
        elif    headerValue.Trim().Contains "INT"       then (INTEGER 0)
        elif    headerValue.Trim().Contains "DOUBLE"    then (DOUBLE 0.0)
        elif    headerValue.Trim().Contains "BOOLEAN"   then (BOOLEAN false)
        else failwith "unknown typeAnnotation in header"

    //Takes a Value Map(Vertex or Edge) and compares its keys with a Map of the header to find Missing keys. If there are any, it adds these values to the value Map.
    let private addEmptyValuesIfNeeded (value:Map<string,GDFValue>) (headerMap:Map<string,string>) =
        if (Map.toList value|>List.map fst|>List.sort) <> (Map.toList headerMap|>List.map fst|>List.sort) then
            let getMissingValues (a,b)  =   match a with|true ->false|_ ->true 
            let headerStrings           =   Map.toList headerMap|>List.map fst
            let missingValues           =   List.map (fun x -> Map.containsKey x value) headerStrings
                                                |>List.map2 (fun a b ->b,a) headerStrings
                                                |>List.filter(fun x -> getMissingValues x)
                                                |>List.map snd
            let missingValueWithType    =   List.map2(fun a b -> (a,b)) missingValues (List.map(fun x -> Map.find x headerMap) missingValues)
            let missingValuesUpdated    =   missingValueWithType|>List.map(fun (a,b)->(a,getGDFDefaultValue b))
            (missingValuesUpdated@(value|>Map.toList))|>Map.ofList
        else
            value

    //Reconstructs vertices or edges based on LVertex or LEdge and the associated header as Map
    let private reconstructVertex (value:LVertex<GDFValue,Map<string,GDFValue>>) (headerMap:Map<string,string>)    =
        let index               =   ("name",(fst value))
        let updateValueMap      =   addEmptyValuesIfNeeded (snd value) headerMap
        (index::(updateValueMap|>Map.toList))
            |>List.distinct
            |>List.map(snd)
            |>List.map(fun x -> gdfValueToString x)
            |>List.map(addQuotationsIfNeeded)
        
    let private reconstructEdge (value:LEdge<GDFValue,Map<string,GDFValue>>) (headerMap:Map<string,string>)    =
        let index1              =   ("node1"),((fun (a,b,c) -> a) value)
        let index2              =   ("node2"),((fun (a,b,c) -> b) value)
        let updateValueMap      =   addEmptyValuesIfNeeded ((fun (a,b,c)->c) value) headerMap
        (index1::index2::(updateValueMap|>Map.toList))
            |>List.distinct
            |>List.map(snd)
            |>List.map(fun x -> gdfValueToString x)
            |>List.map(addQuotationsIfNeeded)


    //Takes a vertexList and an edgeList and transform them into a .txt file that is readable with the fromFile function.
    let toFile  (vertexList: LVertex<GDFValue,Map<string,GDFValue>> list) (edgeList: LEdge<GDFValue,Map<string,GDFValue>> list) (path:string) =
        let vertexList  = vertexList 
        let edgeList    = edgeList

        let vertexHeaderMap = reconstructVerticesHeader vertexList
        let edgeHeaderMap   = reconstructEdgesHeader edgeList

        let vertexHeader    = 
            let headerStringList    = headerMapToString vertexHeaderMap 
            let name                = headerStringList|>List.findIndex(fun x -> x.Contains "name")
            let headerIndex         = "nodedef> name VARCHAR"
            headerStringList.[name]::headerIndex::headerStringList
                |>List.distinct
                |>List.tail
                |>String.concat ","
        
        let edgeHeader      = 
            let headerStringList    = headerMapToString edgeHeaderMap     
            let node1               = headerStringList|>List.findIndex(fun x -> x.Contains "node1")
            let node1Index          = "edgedef> node1"
            let node2               = headerStringList|>List.findIndex(fun x -> x.Contains "node2")
            let node2Index          = "node2"
            headerStringList.[node1]::headerStringList.[node2]::node1Index::node2Index::headerStringList
                |>List.distinct
                |>List.tail |> List.tail
                |>String.concat ","
        
        let vertexData      = List.map(fun x -> reconstructVertex x vertexHeaderMap) vertexList     |>List.map(String.concat ",")
        let edgeData        = List.map(fun x -> reconstructEdge x edgeHeaderMap) edgeList           |>List.map(String.concat ",")

        let vertices        = vertexHeader::vertexData
        let edges           = edgeHeader::edgeData

        let content         = vertices@edges|>String.concat "\n"
        System.IO.File.WriteAllText(path,content)    
        