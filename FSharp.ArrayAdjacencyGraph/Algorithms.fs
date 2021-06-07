namespace Algorithms

open FSharp.ArrayAdjacencyGraph.Graph
open Aether
open System.Collections.Generic

///Louvain method for community detection
module Louvain =                   
    
    //All functions connected to dictionaries used.
    module private Dictionary = 
        
        //Return the value to the key k if it is bound, else fail.        
        let getValue k (dict:Dictionary<'K,'V>) =
            try 
                dict.Item k
            with
            | _ -> failwithf "Error get k %O dict %O" k dict

    //All functions connected to the randomization progress.
    module private Randomize =
        let rand = new System.Random()

        //Swaps the position of item x and item y in the array a.
        let swap (a: _[]) x y =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp

        // shuffle an array (in-place)
        let shuffle a =
            Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a  
    
    //All functions connected to grouping values.
    module private GroupingFunctions =
        
        //Group values of an array by the groupingF and sum the values of each group after applying the valueF on each of them.
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
    
    //Contains the code for the Louvain method for community detection.
    //Blondel, Vincent D; Guillaume, Jean-Loup; Lambiotte, Renaud; Lefebvre, Etienne (9 October 2008). "Fast unfolding of communities in large networks". Journal of Statistical Mechanics: Theory and Experiment. 2008
    let private louvainMethod (g1:ArrayAdjacencyGraph<'Vertex,'Label,float>) (randomized:bool) (modularityIncreaseThreshold: float) : (ArrayAdjacencyGraph<'Vertex,'Label*int,float>) = 
        
        //Create the vertices for the output graph and a new one for further computation
        let vertices,vertices2 : Dictionary<'Vertex,'Label*int>*Dictionary<'Vertex,int*int>=
            let vertices = g1.LabelMap().Keys
            let newDictionary = System.Collections.Generic.Dictionary<'Vertex,'Label*int>()
            let newDictionary2 = System.Collections.Generic.Dictionary<'Vertex,int*int>()
            let mutable counter = 0
            for vertex in vertices do
                let newLabel = (g1.GetLabel vertex),counter
                newDictionary.Add (vertex,newLabel)
                newDictionary2.Add (vertex,(counter,counter))
                counter <- counter+1
            newDictionary,newDictionary2
        
        //Create the edges for the output graph
        let edges =
            g1.AdjacencyGraph()
        
        //Create the edges for the computation graph
        let edges2 =
            let newEdges = System.Collections.Generic.Dictionary<int,(int*int*float)[]>()
            for v in edges do
                let key     = vertices2.Item (v.Key) |> fst
                let edges   = v.Value |> Array.map (fun (s,t,w) -> ((vertices2.Item s |> fst),(vertices2.Item t |> fst),w))
                newEdges.Add (key,edges)
            newEdges

        //Update the vertices for the computation graph
        let verticesUpdated =
            let newVertices = System.Collections.Generic.Dictionary<int,int*int>()
            for i in vertices2 do
                let v = i.Value
                let key = fst v
                newVertices.Add (key,v)
            newVertices

        //The output graph
        let g : (ArrayAdjacencyGraph<'Vertex,'Label*int,float>) = ArrayAdjacencyGraph(edges,vertices)
        //The computation graph
        let g2 :(ArrayAdjacencyGraph<int,int*int,float>)        = ArrayAdjacencyGraph(edges2,verticesUpdated)


        let louvainCycleInPlace (graph:ArrayAdjacencyGraph<int,int*int,float>) (randomized:bool) (modularityIncreaseThreshold: float) (numberOfLoops:int) (previousModularity:float) :(int*ArrayAdjacencyGraph<int,int*int,float>*float)=
                
            //Array of all vertices in the graph
            let verti =
                graph.GetVertices()
            
            //Shuffles the verti array if radomize is true
            if randomized then
                Randomize.shuffle verti|>ignore

            //Total weight of all edges combined
            let totalWeight =
        
                let result = Array.zeroCreate (graph.AdjacencyGraph()).Count
                let mutable i = 0
                for group in (graph.AdjacencyGraph()) do
                    result.[i] <- group.Value
                    i <- i+1
                result
                |> Array.concat
                |> Array.sumBy (fun (source,target,weight) -> (weight))

               
            //Array of all neighbouring vertices, returned as (vertex,edgeweight) array. The index of the element is the same as the vertex in verti.
            let neighbours =
                [|
                    for i in verti do
                        graph.GetConnectedEdges i
                        |> Array.map(fun (s, t, w) ->
                            if s=i then (t,w)
                            else (s,w))
                        |> Array.sortBy fst
                        
                |]
            
            //weighted Degree of the vertex. The index of the element is the same as the vertex in verti.
            let ki =
                neighbours
                |> Array.map(Array.sumBy snd)
                                                                              
            //The weight of all self-referencing loops of the vertices. The index of the element is the same as the vertex in verti.
            let selfLoops =                                                
                [|
                    for vertex in verti do 
                        graph.GetConnectedEdges(vertex)
                        |>Array.sumBy(fun (s,t,w) -> if s=vertex&&t=vertex then w/2. else 0.) 
                |]
            
            //A Dictionary, where the key is the community and the value is a tupel of the weighted degree of the community and the sum of all internal edges.
            let communitySumtotalSumintern =
                let output = System.Collections.Generic.Dictionary<int,float*float>() 
                for i=0 to graph.VertexCount-1 do
                    let vertex = verti.[i]
                    let originalLabel,label = graph.GetLabel vertex
                    let communityWeightTotalStart =  ki.[i]
                    let selfLoopsStart = selfLoops.[i] 
                    output.Add(label,(communityWeightTotalStart,selfLoopsStart))
                output       
            
            //Function to calculate the modularity of the graph.
            let modularityQuality startValue =
                if startValue <> 0. then failwith "Wrong startValue"
                let mutable q = startValue
                for i in communitySumtotalSumintern do
                    let (totalSumC,sumIntern) = i.Value
                    if totalSumC > 0. then 
                        let calculation = (sumIntern - (totalSumC*totalSumC) / totalWeight)
                        //printfn "counter = %A, sumIntern = %A, totalSumC = %A, change = %A" i.Key sumIntern totalSumC calculation
                        q <- (q+(calculation))
                (q/totalWeight)

            //Minimal increase in modularity Quality that has to be achieved. If the increase in modularity is lower, then the first phase of the louvain Algorithm ends and a new iteration can begin.
            let increaseMin = modularityIncreaseThreshold //0.000001

            //Runs once over all vertices in the graph and move the vertex into the community to which the modularity gain is maximal. In case of no positive gain, the original community is kept.
            let rec louvainOneLevel (counter:int) (nbOfMoves:int) =
                
                //Do until
                if counter = graph.VertexCount then 

                    nbOfMoves > 0

                else            
                       
                    //Vertex that is looked at.
                    let node                                 = verti.[counter]
                    
                    //The weighted degree of the node.
                    let ki                                   = ki.[counter] //graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> w)),node)

                    //The weight of all self-referencing loops of the vertex.
                    let selfloopNode                         = selfLoops.[counter]
                    
                    //Community of the node before potential improvement.
                    let (fixedCommunity,originalCommunity)   = (graph.GetLabel node)

                    //Weighted degree of the community,the sum of all internal edges.
                    let (originalCommunityTotalSum,originalCommunitySumIntern)       = Dictionary.getValue originalCommunity communitySumtotalSumintern
                              
                    //Remove node from its original community.                   
                    graph.SetLabel(node,(fixedCommunity,-1)) |> ignore

                    //All neighbors of the node with their edgeWeight.         
                    let neighbors           = 
                       
                        neighbours.[counter]
                        |> Array.filter (fun (vertex,weight) -> vertex <> node) 
                   
                    //This if condition prevents problems If the node is isolated and has 0 edges. 
                    if neighbors = Array.empty then  
                           
                        graph.SetLabel(node,(fixedCommunity, originalCommunity))|> ignore
                        louvainOneLevel (counter+1) (nbOfMoves)
                   
                    else
                                      
                        //All communities the node is connected to with their edgeweight.
                        let connectedCommunities     = 
                                                  
                            neighbors
                            |> Array.map (fun (vertex,weight) -> (((graph.GetLabel vertex)|>snd),weight)) 
                           
                        //All communities the node is connected to with their edgeweight, removing duplicates. 
                        let connectedCommunitiesCondensed =
                           
                            GroupingFunctions.sumGroupBy fst snd connectedCommunities        
                           
                        //All weights to the original community of the node.
                        let weightofConnectionToOldCommunity         =   
                           
                            GroupingFunctions.findWeightofConnectionToOldCommunity connectedCommunitiesCondensed originalCommunity

                        //Removing the node from its community, updating community values communityWeightTotal and sumIntern.
                        let communityWeightTotalUpdate =  (originalCommunityTotalSum-ki)
                        let sumInternUpdate            =  (originalCommunitySumIntern-((2.*(weightofConnectionToOldCommunity))+(selfloopNode)))                  

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
                        
                        //If there is a gain in modularity bigger than 0.
                        if modularityGain < 0.  then 
                           
                            //Resetting the community to its original state.                       
                            graph.SetLabel (node,(fixedCommunity,originalCommunity)) |> ignore
                            communitySumtotalSumintern.Item originalCommunity <- (originalCommunityTotalSum,originalCommunitySumIntern)
                       
                            louvainOneLevel (counter+1) (nbOfMoves)

                        else                                           
                            let (communityNewSum,communityNewIn) = Dictionary.getValue bestCommunity communitySumtotalSumintern

                            //Moving the node to its new community.
                            let sumInternBestCommunity              =      (communityNewIn+((2.*(connectionToBestCommunity)+(selfloopNode))))
                            let communityWeightTotalBestCommunity   =      (communityNewSum+ki)
                           
                            graph.SetLabel (node,(fixedCommunity,bestCommunity)) |> ignore
                            communitySumtotalSumintern.Item bestCommunity <- (communityWeightTotalBestCommunity,sumInternBestCommunity)

                            (if bestCommunity <> originalCommunity then (nbOfMoves+1) else nbOfMoves)
                            |> louvainOneLevel (counter+1) 
         
            //A loop that executes louvainOneLevel as long as none of the exit conditions are met.
            //The exit conditions are
            // 1) No improvement was preformed 
            // 2) The increase in modularityQuality by preforming the louvainOneLevel results in a score lower than the increaseMin.
            let rec loop nbOfMoves currentQuality improvement :(int*ArrayAdjacencyGraph<int,int*int,float>*float)=
                
                let qualityNew = modularityQuality 0.
                   
                let build (shouldIBuild:bool) :int*ArrayAdjacencyGraph<int,(int*int),float>*float=

                    if not shouldIBuild then
                        failwith "ERROR"
                    else
                       
                       //Returns a Map oldCommunity -> updatedCommunity; Returns a dictionary where the key is the vertex and the value is the new community
                        let (vertexToLabelMap,vertexNewLabel) :((Map<int,int>)*(Dictionary<int,int>))=
                            let labelMap =
                                graph.GetLabels()
                                |> Array.map snd
                                |> Array.distinct
                                |> Array.mapi (fun i label -> (label,i))
                                |> Map.ofArray
                            let labelMap2 = 
                                [|
                                    for (oldCommunity,newCommunity) in graph.GetLabels() do
                                        oldCommunity,labelMap.[newCommunity]
                                |]
                                |> Map.ofArray

                            let vertexDict = System.Collections.Generic.Dictionary<int,int>()
                            for i in verti do
                                vertexDict.Add (i,(labelMap.[(graph.GetLabel i)|>snd]))

                            labelMap2,vertexDict                         
                        
                        //Updates the second set of labels in the outputgraph
                        for i in g.GetVertices() do
                            let (originalLabel,currentLabel) = g.GetLabel(i)
                            let updateLabel     = vertexToLabelMap.[currentLabel]
                            g.SetLabel(i,(originalLabel,updateLabel))
                            |> ignore
                        
                        //Returns the vertices for the next iteration of the louvain algorithm.
                        let vert = 
                            vertexToLabelMap
                            |> Map.toArray
                            |> Array.map snd
                            |> Array.distinct
                            |> Array.map (fun x -> (x,(x,x)))
                            |> Array.toList
                        
                        //Return the edgeList for the next iteration of the louvain algorithm.
                        let edgeListUpdated :(int*int*float)[]=

                            let getLabel vertex =
                                Dictionary.getValue vertex vertexNewLabel
   
                            let edgesToLabelEdges :(int*int*float)[] = 
                                let result = Array.zeroCreate (graph.AdjacencyGraph()).Count
                                let mutable i = 0
                                for group in (graph.AdjacencyGraph()) do
                                    result.[i] <- group.Value
                                    i <- i+1
                                result
                                |> Array.concat
                                |> Array.map (fun (s,t,w) -> ((getLabel s),(getLabel t),w))

                            let output = System.Collections.Generic.Dictionary<int*int,float>()
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
                           
                        nbOfMoves,                                    
                        ArrayAdjacencyGraph(
                            (vert),
                            (edgeListUpdated |> Array.toList)
                        ),
                        qualityNew
                
                //Start of the cycle
                if nbOfMoves = 0 then 
                 
                    let hasImProved = louvainOneLevel 0 0

                    loop (nbOfMoves+1) currentQuality hasImProved
           
                elif numberOfLoops > 0 && currentQuality < previousModularity then
                        
                    nbOfMoves,
                    graph,
                    qualityNew
                   
                elif improvement then 
                      
                    if (qualityNew-currentQuality) > increaseMin then 
                         
                            loop (nbOfMoves+1) (qualityNew) (louvainOneLevel 0 0)

                    else                    
                        build true
                elif improvement = false && nbOfMoves = 1 then 
                                  
                    nbOfMoves,
                    graph,
                    qualityNew
                    //build true

                else 
                    build true
                    
            //Start the louvainApplication
            loop 0 (modularityQuality 0.) false

        //The louvainLoop combines the two phases of the louvain Algorithm. As long as improvments can be performed, the louvainApplication is executed.
        let rec louvainInPlace_ nbOfLoops (newG:ArrayAdjacencyGraph<int,int*int,float>) (modularityIncreaseThreshold: float) (modulartiy:float) =
        
            let (nbOfMoves,newGraph,newModularity) = 
            
                louvainCycleInPlace newG randomized modularityIncreaseThreshold nbOfLoops modulartiy           

            if nbOfMoves < 2 || ((nbOfLoops>0) && (newModularity<modulartiy)) then 
            
                (*modulartiy,*)g

            else 

                louvainInPlace_ (nbOfLoops+1) newGraph modularityIncreaseThreshold newModularity


        louvainInPlace_ 0 g2 modularityIncreaseThreshold 0.
   
    ///
    let louvain (graph:ArrayAdjacencyGraph<'Vertex,'Label,float>) (modularityIncreaseThreshold: float) :(*(float)**)(ArrayAdjacencyGraph<'Vertex,'Label*int,float>)=
        louvainMethod graph false modularityIncreaseThreshold
    
    ///
    let louvainRandom (graph:ArrayAdjacencyGraph<'Vertex,'Label,float>) (modularityIncreaseThreshold: float) :(*(float)**)(ArrayAdjacencyGraph<'Vertex,'Label*int,float>)=
        louvainMethod graph true modularityIncreaseThreshold
