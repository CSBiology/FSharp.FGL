namespace FSharp.FGL.CommunityDetection

open Aether
open FSharp.FGL 
open FSharp.FGL.Undirected

//Take a graph that has nodes and communities as ints and returns a graph that has optimized communites based on Fast unfolding of communities in large networks;Vincent D Blondel Jean-Loup Guillaume Renaud Lambiotte Etienne Lefebvre; Journal of Statistical Mechanics: Theory and Experiment vol. 2008 issue 10 (2008) pp: P10008 
module CommunityDetection =
    let modularityOptimization (modularityThreshold : float) (graph : Graph<int,int,float>)   =
        
        //Array of all edges in the graph
        let edgeArray =
            graph |> Edges.toEdgeList |> List.toArray
        
        //Sum of all the edge-weights in the graph
        let m = graph |> Edges.toEdgeList |> List.sumBy(fun (v1,v2,e) -> e)
        
        //Array of all the edges sAdj that are connected to the vertex v
        let verticesConnectedTo =
            graph 
                |> Map.toArray
                |> Array.map(fun (v,(pAdj,c,sAdj)) -> (v,(sAdj)))
        
        //Array of all the nodes and their communities in the graph
        let nodeArray =      
            (graph |> Vertices.toVertexList)|> Array.ofList
        
        //Sum of all the edgeweights connected to node v
        let kiArray    = 
            nodeArray |> Array.map (fun (v,c) -> (v,((Array.sumBy(fun (v1,v2,w) -> if (v1 = v || v2 = v) then w else (w-w))edgeArray))))
        
        //Sum of all the edge-weights that are connected to community c
        let sumTotalArray    =
            Array.map2 (fun (v,c) (v1,ki) -> (c,ki)) nodeArray kiArray
        
        //Sum of all the edge-weights that are in communtiy c and are only connected to community c
        let sumInArray       =
            graph 
            |> Map.toArray
            |> Array.map(fun (v,(pAdj,c,sAdj)) -> 
                (c,(sAdj
                    |>Map.toList
                    |>List.filter(fun (v2,w) -> if (v2=v) then true else false)
                    |>List.sumBy(snd)))
            )
        
        //Find all nodes connected to community c
        let filterForCommunity c =
            Array.filter (fun (vertex,int) -> if int = c then true else false) nodeArray 
        
        //Find all edges connected to the node
        let filterEdgesForVertex ((vertex,int):int*int) =
            Array.filter(fun (v1,v2,w) -> (v1 = vertex || v2 = vertex)) edgeArray
        
        //Find all Edges that are connected to community c
        let filterEdgesForCommunity c =
            Array.map(filterEdgesForVertex) (filterForCommunity c)
            |> Seq.concat
            |> Seq.distinct |> Seq.toArray
        
        //Find all communities connected to the node
        let neighboringCommunities (node:int*int) =
            let connectedTo = Array.find (fun (v,sAdj) -> (v = fst node)) verticesConnectedTo |> snd |> Map.toArray |> Array.map fst
            Array.map (fun vOrigin -> Array.choose(fun (vertex,int) -> if vertex = vOrigin then Some int else None) nodeArray) connectedTo
            |> Array.concat |> Array.distinct 
        
        //Sum of all edge-weights connected to the node
        let ki (i:int*int) :(int*float)=
            let (vertex,int) = i
            Array.find(fun (v,ki) -> if (v = vertex) then true else false) kiArray
        
        //Array of the community and the sum of all connected edges
        let sumTotal (c:int)  =
            Array.find (fun (int,sumTot) -> if int = c then true else false) sumTotalArray        
        
        //Array of the community and all sum of all extern edges
        let sumIntern (c:int)  =
            Array.find (fun (int,sumIn) -> if int = c then true else false) sumInArray              
        
        //Return the sum of the edge-weights that are between the node and the chosen community
        let kiIn (c:int) (i:int*int) =
            filterEdgesForCommunity c
            |> Array.choose (fun (v1,v2,w) -> if (v1=fst i) then Some (v2,w) elif (v2=fst i) then Some (v1,w) else None)
            |> Array.map (fun (v,w) -> (((Array.find (fun (vO,c) -> vO = v)nodeArray )|>snd),w))
            |> Array.filter (fun (community,w) -> (community = c))
            |> Array.sumBy (fun (c,w) -> w)
        
        //Calculate the change in modularity if a node is moved into a new community
        let changeInMod input =
            let (c,kI,kiInNew,sumInNew,sumTotNew) = input
           
            let innewCommunity =
                let statusNow = 
                    ((sumInNew + 2.* (kiInNew))/(2.*m))-(((sumTotNew+ kI)/(2.*m))**2.)
                let statusWithOutNode =
                    ((sumInNew /(2.*m))-((sumTotNew/(2.*m))**2.)-(((kI )/(2.*m))**2.))
                statusNow - statusWithOutNode   
            
            c,kI,kiInNew,sumInNew,sumTotNew,(innewCommunity) //hier
    
        //Calculated the optimal community of each node based on the changeInMod, returns the number of changes preformed
        let rec calculateChangeInMod counter (changeCounter:int) =
            if counter = ((Array.length nodeArray)) then (changeCounter)
            else
                let node = (Array.item counter) nodeArray
                let oldintIndex = Array.findIndex  (fun (c,sumIn) -> (c = snd node)) sumInArray
                let ki                      = ki node |> snd
                let kiINOld                 = kiIn      (snd node) node      
                let sumInOld                = sumIntern (snd node)      |> snd
                let sumTotOld               = sumTotal  (snd node)       |> snd                      
                Array.set sumInArray oldintIndex (snd node,(sumInOld-kiINOld))
                Array.set sumTotalArray oldintIndex (snd node,(sumTotOld-ki))
                Array.set nodeArray counter ((fst node),(-1))
                let possibleCommunites      = neighboringCommunities node
                let communityInformation    = possibleCommunites    |> Array.map (fun c -> (c,(ki),(kiIn c node),(sumIntern c |> snd),(sumTotal c |> snd)))           //hier
                let (newCommunity,kI,kiInNew,sumInNew,sumTotNew,modulariyChange) =
                    communityInformation 
                    |> Array.map (fun (input) -> changeInMod input (*outOfOriginalint*))
                    |> Array.maxBy (fun (newCommunity,kI,kiInNew,sumInNew,sumTotNew,modulariyChange) -> modulariyChange)
                if (modulariyChange > (modularityThreshold)) then
                        if newCommunity = snd node then 
                            Array.set nodeArray counter node
                            calculateChangeInMod (counter+1) changeCounter
                        else
                            let newCommunityIndex = Array.findIndex  (fun (c,sumIn) -> (c = newCommunity)) sumInArray
                    
                            Array.set nodeArray counter (fst node,newCommunity)                    
                    
                            Array.set sumInArray newCommunityIndex (newCommunity,(sumInNew+kiInNew))
    
                            Array.set sumTotalArray newCommunityIndex (newCommunity,(sumTotNew+ki-kiInNew)) //HIER
                        
                            calculateChangeInMod (counter+1) (changeCounter+1) 
                else    
                    calculateChangeInMod (counter+1) changeCounter 
        
        //Call calculateChangeInMod until no furhter improvement can be done
        let rec loop changeCounter =
            if changeCounter = 0 then (Graph.create (nodeArray|>Array.toList) (edgeArray|>Array.toList))
            else loop (calculateChangeInMod 0 0)
        loop 1
    

