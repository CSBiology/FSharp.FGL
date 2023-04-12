(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../src/FSharp.FGL/bin/Release/netstandard2.0/FSharp.FGL.dll"
#r "../src/FSharp.FGL.IO/bin/Release/netstandard2.0/FSharp.FGL.IO.dll"
#r "../src/FSharp.FGL.ArrayAdjacencyGraph/bin/Release/netstandard2.0/FSharp.FGL.ArrayAdjacencyGraph.dll"


(**
# FSharp.FGL
FSharp.FGL is a functional library to work with graphs. 
<br>
## Example

This example demonstrates using a function defined in this sample library.
<br>
*)

open FSharp.FGL 

Graph.empty
|> Vertices.addMany [(1,"Look At Me Im VertexOne");(2,"Look At Me Im VertexTwo")]
|> Undirected.Edges.add (1,2,"Im An Edge Between VertexOne And VertexTwo ")
|> Undirected.Edges.tryFind 1 2
//Returns Some (1,2,"Im An Edge Between VertexOne And VertexTwo ")

(**
## Overview

At the moment there are <b>2</b> different graph representations available, each with teir own functions:

* [FSharp.FGL](https://csbiology.github.io/FSharp.FGL/content/tutorial.html), an inductive graph model that consists of so called contexts.
* [ArrayAdjacencyGraph](https://csbiology.github.io/FSharp.FGL/content/ArrayAdjacencyGraph.html), an adjacency list representation of a multigraph.

*)

(**

## Samples & documentation

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from comments in the library implementation.

 * [Tutorial](https://csbiology.github.io/FSharp.FGL/content/tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
   
## Contributing and copyright


The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/FSharp.FGL/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharp.FGL
  [issues]: https://github.com/fsprojects/FSharp.FGL/issues
  [readme]: https://github.com/fsprojects/FSharp.FGL/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharp.FGL/blob/master/LICENSE
*)
