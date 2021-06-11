namespace FSharp.Graph

///Labeled vertex
type LVertex<'Vertex,'Label> =
    'Vertex * 'Label

///Unlabeled edge
type Edge<'Vertex> =
    'Vertex * 'Vertex

///Labeled edge
type LEdge<'Vertex,'Edge> =
    'Vertex * 'Vertex * 'Edge
