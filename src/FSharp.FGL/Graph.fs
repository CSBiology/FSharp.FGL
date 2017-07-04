namespace FSharp.FGL

open Aether

type LVertex<'Vertex,'Label> =
    'Vertex * 'Label

type Edge<'Vertex> =
    'Vertex * 'Vertex

type LEdge<'Vertex,'Edge> =
    'Vertex * 'Vertex * 'Edge

type Adj<'Vertex,'Edge> when 'Vertex: comparison=
    List<'Vertex*'Edge>

type Context<'Vertex,'Label,'Edge> when 'Vertex: comparison=
    Adj<'Vertex,'Edge>*'Vertex*'Label*Adj<'Vertex,'Edge>

type MAdj<'Vertex,'Edge> when 'Vertex: comparison =
    Map<'Vertex,'Edge>

type MContext<'Vertex,'Label,'Edge> when 'Vertex: comparison =
    MAdj<'Vertex,'Edge> * 'Label * MAdj<'Vertex,'Edge>

type Graph<'Vertex,'Label,'Edge> when 'Vertex: comparison =
    Map<'Vertex, MContext<'Vertex,'Label,'Edge>>


module Lenses = 
    /// Lens for predecessors in a context
    let pred_ : Lens<Context<'Vertex,_,'Edge>, Adj<'Vertex,'Edge>> =
        (fun (p, _, _, _) -> p), (fun p (_, v, l, s) -> (p, v, l, s))

    /// Lens for vertex in a context
    let val_ : Lens<Context<'Vertex,_,_>, 'Vertex> =
        (fun (_, v, _, _) -> v), (fun v (p, _, l, s) -> (p, v, l, s))

    /// Lens for successors in a context
    let succ_ : Lens<Context<'Vertex,_,'Edge>, Adj<'Vertex,'Edge>> =
        (fun (_, _, _, s) -> s), (fun s (p, v, l, _) -> (p, v, l, s))

    /// Lens for predecessors in a Mcontext
    let mpred_ : Lens<MContext<'Vertex,_,'Edge>, MAdj<'Vertex,'Edge>> =
        (fun (p, _, _) -> p), (fun p (_, l, s) -> (p, l, s))

    /// Lens for successors in a Mcontext
    let msucc_ : Lens<MContext<'Vertex,_,'Edge>, MAdj<'Vertex,'Edge>> =
        (fun (_, _, s) -> s), (fun s (p, l, _) -> (p, l, s))

module Graph =

    (* Transition functions *)

    let private fromAdj<'Vertex,'Edge when 'Vertex: comparison> : Adj<'Vertex,'Edge> -> MAdj<'Vertex,'Edge> =
        Map.ofList 

    let private fromContext<'Vertex,'Label,'Edge when 'Vertex: comparison> : Context<'Vertex,'Label,'Edge> -> MContext<'Vertex,'Label,'Edge> =
        fun (p, _, l, s) -> fromAdj p, l, fromAdj s

    let private toAdj<'Vertex,'Edge when 'Vertex: comparison> : MAdj<'Vertex,'Edge> -> Adj<'Vertex,'Edge> =
        Map.toList

    let private toContext (v:'Vertex) (mc : MContext<'Vertex,'Label,'Edge>) : Context<'Vertex,'Label,'Edge> =
        mc
        |> fun (p, l, s) -> toAdj p, v, l, toAdj s


    (* Compose Graphs *)

    let private composeGraph c v p s (g:Graph<'Vertex,'Label,'Edge>) =
        let g1 = (Optic.set (Map.value_ v) (Some (fromContext c))) g
        let g2 = 
            List.fold (fun g (value, edge) -> 
                let composedPrism = (Compose.prism (Map.key_ value) Lenses.msucc_)
                let adjListMapping = Map.add value edge
                let adjListInGraphMapping = Optic.map composedPrism adjListMapping
                adjListInGraphMapping g) 
                g1 p
        List.fold (fun g (edge, value) -> 
            let composedPrism = (Compose.prism (Map.key_ value) Lenses.mpred_)
            let adjListMapping = Map.add value edge
            let adjListInGraphMapping = Optic.map composedPrism adjListMapping
            adjListInGraphMapping g) 
            g2 s

    let private compose c g =
        composeGraph c (Optic.get Lenses.val_ c) (Optic.get Lenses.pred_ c) (Optic.get Lenses.succ_ c) g
      
    (* Decompose Graphs *)

    let private decomposeContext v c : Context<'Vertex,'Label,'Edge>=
        c
        |> Optic.map Lenses.mpred_ (Map.remove v)
        |> Optic.map Lenses.msucc_ (Map.remove v)
        |> toContext v

    let private decomposeGraph v p s g : Graph<'Vertex,'Label,'Edge>=
        let g1 = Map.remove v g
        let g2 =
            List.fold (fun g (value, _) ->
                let composedPrism = Compose.prism (Map.key_ value) Lenses.msucc_
                let adjListMapping = Map.remove v
                let adjListInGraphMapping = Optic.map composedPrism adjListMapping
                adjListInGraphMapping g)
                g1 p
        List.fold (fun g (value, _) ->
            let composedPrism = Compose.prism (Map.key_ value) Lenses.mpred_
            let adjListMapping = Map.remove v
            let adjListInGraphMapping = Optic.map composedPrism adjListMapping
            adjListInGraphMapping g)
            g2 s

    ///Lookup a context in the graph. If the binding exists, it returns the context and the graph minus the vertex and its edges. Raising KeyNotFoundException if no binding exists in the graph.
    let decompose (v:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) = 
        Map.find v g
        |> fun mc ->
            let c = decomposeContext v mc
            let g = decomposeGraph v (Optic.get Lenses.pred_ c) (Optic.get Lenses.succ_ c) g
            c, g

    ///Lookup a context in the graph. If the binding exists, it returns a Some value of the context and the graph minus the vertex and its edges. If it doesn't exist, returns None and the initial graph.
    let tryDecompose (v:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) = 
        match Map.tryFind v g with
        | Some mc ->
            let c = decomposeContext v mc
            let g = decomposeGraph v (Optic.get Lenses.pred_ c) (Optic.get Lenses.succ_ c) g
            Some c, g
        | _ ->
            None, g

    ///If the given graph contains at least one vertex, returns a Some value of the first context and the graph minus the associated vertex and its edges. If the graph is empty, returns None and the initial graph.
    let decomposeFirst g : Context<'Vertex,'Label,'Edge> option * Graph<'Vertex,'Label,'Edge> =
        match Map.tryFindKey (fun _ _ -> true) g with
        | Some v -> tryDecompose v g
        | _ -> None, g


    (* General *)

    ///Returns true, if the Graph does not contain any vertices. Returns false, if not.
    let isEmpty<'Vertex,'Label,'Edge when 'Vertex: comparison> : Graph<'Vertex,'Label,'Edge> -> bool =
        Map.isEmpty

    ///Creates a new, empty graph.
    let empty : Graph<'Vertex,'Label,'Edge> =
        Map.empty
    
    ///Lookup a context in the graph, returning a Some value if a binding exists and None if not.
    let tryGetContext v (g:Graph<'Vertex,'Label,'Edge>) : Context<'Vertex,'Label,'Edge> option =
            Map.tryFind v g
            |> Option.map (fun mc -> toContext v mc)

    ///Lookup a context in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    let getContext v (g:Graph<'Vertex,'Label,'Edge>) : Context<'Vertex,'Label,'Edge> =
            Map.find v g
            |> fun mc -> toContext v mc


    (* Iterative *)

    ///Maps contexts of the graph.
    let mapContexts (mapping : Context<'Vertex,'Label,'Edge> -> 'T) (g: Graph<'Vertex,'Label,'Edge>) : Map<'Vertex,'T>= 
        g
        |> Map.map (fun v mc ->  mapping (toContext v mc))
    
    ///Folds over the contexts in the graph.
    let foldContexts (state: 'State) (folder : 'State -> Context<'Vertex,'Label,'Edge> -> 'State) (g: Graph<'Vertex,'Label,'Edge>) : 'State =
        g
        |> Map.fold (fun s v mc -> folder s (toContext v mc)) state

    ///Performs a given function on every edge of the graph.
    let iterContexts (action : Context<'Vertex,'Label,'Edge> -> unit) (g: Graph<'Vertex,'Label,'Edge>) : unit = 
        g
        |> Map.iter (fun v mc ->  action (toContext v mc))

