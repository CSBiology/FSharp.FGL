namespace Louvain

open FSharp.FGL
open FSharp.FGL.ArrayAdjacencyGraph
open Aether
open System.Collections.Generic

    
module Louvain =                   
    module internal Dictionary = 
    
        let tryGetValue k (dict:Dictionary<'K,'V>) =
            let b,v = dict.TryGetValue(k)
            if b then Some v 
            else None
    
        let getValue' k (dict:Dictionary<'K,'V>) =
            match (tryGetValue k dict) with 
                |Some x -> x
                |None -> failwith "Error get"
        
        let getValue k (dict:Dictionary<'K,'V>) =
            try 
                dict.Item k
            with
            | _ -> failwith "Error get"

        let copyRecursive (innerCopyF : 'V -> 'V) (dict:Dictionary<'K,'V>) =
            let newDict = Dictionary<'K,'V>()
            for kv in dict do
                newDict.Add(kv.Key,innerCopyF kv.Value)
            newDict
 
    // Group values of an array by the groupingF and sum the values of each group after applying the valueF on each of them.
    let inline sumGroupBy (groupingF : 'T -> 'group) (valueF : 'T -> 'V) (input : ('T) []) =
        
        let length = input.Length
        let dict = System.Collections.Generic.Dictionary<'group,'V> ()
    
        // Build the groupings
        for i = 0 to length - 1 do

            let item = input.[i]
            let safeKey,v = groupingF item, valueF item
            let mutable prev = Unchecked.defaultof<'V>
            if dict.TryGetValue(safeKey, &prev) then
                dict.[safeKey] <- prev + v
            else 
                //dict.Add(safeKey,v)
                dict.[safeKey] <- v
             
        // Return the array-of-sums.
        let result = Array.zeroCreate dict.Count
        let mutable i = 0
        for group in dict do
            result.[i] <- group.Key, group.Value
            i <- i + 1
        result

    //Find the summed up weights to the original community of the vertex
    let findWeightofConnectionToOldCommunity connectedCommunities originalCommunity     =   
        
        match (Array.tryFind (fun (community,weight) -> community=originalCommunity) connectedCommunities) with
            | Some x    -> (x|> snd)
            | None      -> 0.
         
    let louvainInPlace (g:ArrayAdjacencyGraph<int,int,float>) =
               
        let louvainCycleInPlace (graph:ArrayAdjacencyGraph<int,int,float>)   =
            
               let verti =
                   graph.Vertices()

               for i in graph.Vertices() do
                   
                   graph.SetLabel (i,i) |> ignore

               //Total weight of all edges combined
               let totalWeight =
                   
                   graph.Edges()   
                   |> Array.sumBy (fun (source,target,weight) -> weight)
                                 
               //Neighbours as [|[|vertexPosition, weight|]|];Application of neighbours from before.
               let getNeighbours(vertex) =                  
                   
                   vertex
                   |> graph.GetConnectedEdges 
                   |> Array.map(fun (s, t, w) ->
                       if s=vertex then (t,w)
                       else (s,w))
                                   
               let neighbours =
                    [|
                        for i in verti do
                            graph.GetConnectedEdges i
                            |> Array.map(fun (s, t, w) ->
                                if s=i then (t,w)
                                else (s,w))
                            |> Array.sortBy fst
                    |]
                               
               //Leicht langsamer als neighbourArray,Test an 500Graph
               //let neighbourDict =
               //     let output = System.Collections.Generic.Dictionary<int,(int*float)[]>()
               //     for i in verti do
               //         let neighboursI = 
               //             graph.GetConnectedEdges i
               //             |> Array.map(fun (s, t, w) ->
               //                 if s=i then (t,w)
               //                 else (s,w))
               //             |> Array.sortBy fst
               //         output.Add(i,neighboursI)
               //     output
                    
               //All self-referencing loops of the vertices.
               let selfLoops vertex =
                     
                   graph.GetConnectedEdges(vertex)
                   |>Array.sumBy(fun (s,t,w) -> if s=vertex&&t=vertex then w else 0.)
                   //[|
                   //     for i=0 to (graph.VertexCount-1) do
                   //         neighbours.[i]
                   //         |> Array.sumBy(fun (v,w) -> if v=0 then w else 0.)                 
                   //|]

               
               //let selfLoopsArray =
               //    [|
               //         for i in verti do
               //             graph.GetConnectedEdges(i)
               //             |>Array.sumBy(fun (s,t,w) -> if s=i&&t=i then w else 0.)
               //    |]

               let communitySumtotalSumintern =
                   let output = System.Collections.Generic.Dictionary<int,float*float>() 
                   for i=0 to graph.VertexCount-1 do
                       let vertex = verti.[i]
                       let label = graph.GetLabel vertex
                       let communityWeightTotalStart = (graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> w)),vertex))
                       let selfLoopsStart = selfLoops vertex
                       output.Add(label,(communityWeightTotalStart,selfLoopsStart))
                   output       
               
               let modularityQuality startValue =
                   if startValue <> 0. then failwith "Wrong startValue"
                   let mutable q = startValue
                   for i in communitySumtotalSumintern do
                       let (totalSumC,sumIntern) = i.Value
                       if totalSumC > 0. then 
                           let calculation = (sumIntern - (totalSumC*totalSumC) / totalWeight)
                           q <- q + (calculation)
                   (q/totalWeight)
               //Minimal increase in modularity Quality that has to be achieved. If the increase in modularity is lower, then the first phase of the louvain Algorithm ends and a new iteration can begin.
               let increaseMin = 0.000001

               //Runs once over all vertices in the graph and move the vertex into the community to which the modularity gain is maximal. In case of no positive gain, the original community is kept.
               let rec louvainOneLevel (counter:int) (nbOfMoves:int) =
                   //Do until
                   if counter = graph.VertexCount then 
                       nbOfMoves > 0

                   else            
                       //Vertex that is looked at.
                       let node                            = verti.[counter]
                       
                       //The weighted degree of the node.
                       let ki                              = graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> w)),node)

                       //Community of the node before potential improvement.
                       let originalCommunity               = graph.GetLabel node

                       //Sum of all intern weights of the originalCommunity community.
                       let (originalCommunityTotalSum,originalCommunitySumIntern)       = Dictionary.getValue originalCommunity communitySumtotalSumintern
                       //Sum of all weights that are connected to the originalCommunity.
                                   
                       //Remove node from its original community.
                       graph.SetLabel(node,-1) |> ignore

                       //All neighbors of the node with their edgeWeight.         
                       let neighbors           = 
                           
                           getNeighbours(node)
                           |> Array.filter (fun (vertex,weight) -> vertex <> node)    
                           |> Array.sort
                           //neighbours.[counter]
                           //|> Array.filter (fun (vertex,weight) -> vertex <> node) 
                           //Dictionary.getValue counter neighbourDict 
                           //|> Array.filter (fun (vertex,weight) -> vertex <> node) 

                       //This if condition prevents problems If the node is isolated and has 0 edges. 
                       if neighbors = Array.empty then  
                               
                           graph.SetLabel(node,originalCommunity) |> ignore
                           louvainOneLevel (counter+1) (nbOfMoves)
                       
                       else
                                          
                           //All communities the node is connected to with their edgeweight.
                           let connectedCommunities     = 
                                                      
                               neighbors
                               |> Array.map (fun (vertex,weight) -> ((graph.GetLabel vertex),weight)) 
                               
                           //All communities the node is connected to with their edgeweight, removing duplicates. 
                           let connectedCommunitiesCondensed =
                               
                               sumGroupBy fst snd connectedCommunities        
                               
                           //All weights to the original community of the node.
                           let weightofConnectionToOldCommunity         =   
                               
                               findWeightofConnectionToOldCommunity connectedCommunitiesCondensed originalCommunity

                           //Removing the node from its community, updating community values communityWeightTotal and sumIntern.
                           let communityWeightTotalUpdate =  (originalCommunityTotalSum-ki)
                           let sumInternUpdate            =  (originalCommunitySumIntern-((2.*(weightofConnectionToOldCommunity))+(selfLoops node)))                  

                           communitySumtotalSumintern.Item originalCommunity <- (communityWeightTotalUpdate,sumInternUpdate)

                           let connectedCommunitiesCondensedNew =
                               Array.append [|originalCommunity,weightofConnectionToOldCommunity|] connectedCommunitiesCondensed
                               |> Array.distinct

                           //Calculating the best possible community for the node, based on modularity gain. 
                           //Outputs the bestCommunity, the gain acived by moving the node to that community and the weight of the connection to that new Community.  
                           let (bestCommunity,modularityGain,connectionToBestCommunity) =                        

                               let calculations = 
                                   connectedCommunitiesCondensedNew
                                   |> Array.map (fun (community,connectionToCommunity) -> 
                                           (
                                           community,
                                           (connectionToCommunity-((Dictionary.getValue community communitySumtotalSumintern|>fst)*ki/totalWeight)),
                                           connectionToCommunity
                                           )
                                       )

                               calculations
                               |> Array.maxBy (fun (community,modularityGain,connectionToCommunity) -> modularityGain)
                           
                           
                           if modularityGain < 0.  then 
                               
                               //Resetting the community to its original state.                       
                               graph.SetLabel (node,originalCommunity) |> ignore
                               communitySumtotalSumintern.Item originalCommunity <- (originalCommunityTotalSum,originalCommunitySumIntern)
                           
                               louvainOneLevel (counter+1) (nbOfMoves)

                           else                                           

                               let (communityNewSum,communityNewIn) = Dictionary.getValue bestCommunity communitySumtotalSumintern

                               //Moving the node to its new community.
                               let sumInternBestCommunity              =      (communityNewIn+((2.*(connectionToBestCommunity)+(selfLoops node))))
                               let communityWeightTotalBestCommunity   =      (communityNewSum+ki)
                               
                               graph.SetLabel (node,bestCommunity) |> ignore
                               communitySumtotalSumintern.Item bestCommunity <- (communityWeightTotalBestCommunity,sumInternBestCommunity)

                               (if bestCommunity <> originalCommunity then (nbOfMoves+1) else nbOfMoves)
                               |> louvainOneLevel (counter+1) 
             
               //A loop that executes louvainOneLevel as long as none of the exit conditions are met.
               //The exit conditions are
               // 1) No improvement was preformed 
               // 2) The increase in modularityQuality by preforming the louvainOneLevel results in a score lower than the increaseMin.
               let rec loop nbOfMoves currentQuality improvement =
                   
                   let qualityNew = modularityQuality 0.
                   if nbOfMoves = 0 then 
                     
                       let hasImProved = louvainOneLevel 0 0

                       loop (nbOfMoves+1) currentQuality hasImProved
               
                   elif improvement then 
                          
                       if (qualityNew-currentQuality) > increaseMin then 
                             
                               loop (nbOfMoves+1) (qualityNew) (louvainOneLevel 0 0)

                       else                    
                            let vertexToLabelMap =
                                let labelMap =
                                    graph.Labels()
                                    |> Array.distinct
                                    //|> Array.sort
                                    |> Array.mapi (fun i label -> (label,i))
                                    |> Map.ofArray
                          
                                let dict = System.Collections.Generic.Dictionary<int,int>()
                                for i in graph.Vertices() do
                                    dict.Add (i,(labelMap.[(graph.GetLabel i)]))
                                dict
                    
                            for i in g.Vertices() do
                                let previousLabel   = g.GetLabel(i)
                                let updateLabel     = Dictionary.getValue previousLabel vertexToLabelMap
                                g.SetLabel(i,(updateLabel))
                                |> ignore

                            let vertexList = 
                                let result = Array.zeroCreate vertexToLabelMap.Count
                                let mutable i = 0
                                for group in vertexToLabelMap do
                                    let v = group.Value
                                    result.[i] <- (v,v)
                                    i <- i+1
                                result
                                |> Array.distinct |> Array.toList

                            let edgeListUpdated :LEdge<'Vertex,'Edge>[]=
    
                                let getLabel vertex =
                                    Dictionary.getValue vertex vertexToLabelMap
   
                                let edgesToLabelEdges = 
                                    graph.Edges() 
                                    |> Array.map (fun (s,t,w) -> ((getLabel s),(getLabel t),w))

                                let output = System.Collections.Generic.Dictionary<'Vertex*'Vertex,'Edge>()
                                for (s,t,w) in edgesToLabelEdges do
                                    if output.ContainsKey (s,t) then 
                                        let value = Dictionary.getValue ((s,t)) output
                                        if s=t then 
                                            output.Item ((s,t)) <- (value+w)
                                        else
                                            output.Item ((s,t)) <- (value+(w/2.))
        
                                    elif output.ContainsKey (t,s) then
                                        let value = Dictionary.getValue ((t,s)) output
                                        if s=t then 
                                            output.Item ((t,s)) <- (value+w)
                                        else
                                            output.Item ((t,s)) <- (value+(w/2.))
        
                                    else
                                        if s=t then 
                                            output.Add ((s,t),w)
                                        else
                                            output.Add ((s,t),(w/2.))
    
                                let result = Array.zeroCreate output.Count
                                let mutable i = 0
                                for group in output do
                                    let (s,t)   = group.Key
                                    let (w)     = group.Value
                                    result.[i] <- (s,t,w)
                                    i <- i + 1
                                result
                            
                            //let edgeDict =
                            //    neighbours
                            //    |> Array.mapi (fun i (neighbours) ->
                            //        Dictionary.getValue i vertexToLabelMap,
                            //        Array.map(fun (v,w) -> ((Dictionary.getValue v vertexToLabelMap),w))neighbours
                            //        |> 
                            //        
                            //        
                            //        )

                            nbOfMoves,                                    
                            ArrayAdjacencyGraph(
                                (vertexList),
                                (edgeListUpdated |> Array.toList)
                            )
                   elif improvement = false && nbOfMoves = 1 then 
                                      
                       nbOfMoves,
                       graph               
                   
                   else 
                       
                        let vertexToLabelMap =
                            let labelMap =
                                graph.Labels()
                                |> Array.distinct
                                //|> Array.sort
                                |> Array.mapi (fun i label -> (label,i))
                                |> Map.ofArray
                          
                            let dict = System.Collections.Generic.Dictionary<int,int>()
                            for i in graph.Vertices() do
                                dict.Add (i,(labelMap.[(graph.GetLabel i)]))
                            dict
                    
                        for i in g.Vertices() do
                            let previousLabel   = g.GetLabel(i)
                            let updateLabel     = Dictionary.getValue previousLabel vertexToLabelMap
                            g.SetLabel(i,(updateLabel))
                            |> ignore

                        let vertexList = 
                            let result = Array.zeroCreate vertexToLabelMap.Count
                            let mutable i = 0
                            for group in vertexToLabelMap do
                                let v = group.Value
                                result.[i] <- (v,v)
                                i <- i+1
                            result
                            |> Array.distinct |> Array.toList

                        let edgeListUpdated :LEdge<'Vertex,'Edge>[]=
    
                            let getLabel vertex =
                                Dictionary.getValue vertex vertexToLabelMap
   
                            let edgesToLabelEdges = 
                                graph.Edges() 
                                |> Array.map (fun (s,t,w) -> ((getLabel s),(getLabel t),w))

                            let output = System.Collections.Generic.Dictionary<'Vertex*'Vertex,'Edge>()
                            for (s,t,w) in edgesToLabelEdges do
                                if output.ContainsKey (s,t) then 
                                    let value = Dictionary.getValue ((s,t)) output
                                    if s=t then 
                                        output.Item ((s,t)) <- (value+w)
                                    else
                                        output.Item ((s,t)) <- (value+(w/2.))
        
                                elif output.ContainsKey (t,s) then
                                    let value = Dictionary.getValue ((t,s)) output
                                    if s=t then 
                                        output.Item ((t,s)) <- (value+w)
                                    else
                                        output.Item ((t,s)) <- (value+(w/2.))
        
                                else
                                    if s=t then 
                                        output.Add ((s,t),w)
                                    else
                                        output.Add ((s,t),(w/2.))
    
                            let result = Array.zeroCreate output.Count
                            let mutable i = 0
                            for group in output do
                                let (s,t)   = group.Key
                                let (w)     = group.Value
                                result.[i] <- (s,t,w)
                                i <- i + 1
                            result
                        
                        //let edgeDict =
                        //    neighbours
                        //    |> Array.mapi (fun i (neighbours) ->
                        //        Dictionary.getValue i vertexToLabelMap,
                        //        Array.map(fun (v,w) -> ((Dictionary.getValue v vertexToLabelMap),w))neighbours
                        //        |> 
                        //        
                        //        
                        //        )

                        nbOfMoves,                                    
                        ArrayAdjacencyGraph(
                            (vertexList),
                            (edgeListUpdated |> Array.toList)
                        )
               
               //Start the louvainApplication
               loop 0 (modularityQuality 0.) false

        //The louvainLoop combines the two phases of the louvain Algorithm. As long as improvments can be performed, the louvainApplication is executed.
        let rec louvainInPlace_ nbOfLoops (newG:ArrayAdjacencyGraph<int,int,float>) =
            
            let (nbOfMoves,newGraph) = 
                
                louvainCycleInPlace newG           

            if nbOfMoves < 2 then 
                
                (nbOfLoops),g

            else 

                louvainInPlace_ (nbOfLoops+1) newGraph
        louvainInPlace_ 0 g

