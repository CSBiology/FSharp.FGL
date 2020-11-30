(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.FGL/net47/"

(**
# GDF Format
GDF is the file formate used by [The Graph Exploration System (GUESS)](http://graphexploration.cond.org/) . It is build similar to a database table or <abbr title="Comma Seperated File">CSV</abbr>. Both edge and node data is defined in a single file, each initiated by their respective headers. Each element (i.e. node or edge) is on a line and values are separated by comma. Node definition is started by "nodedef>name" and edge definition by "edgedef>node1". For nodes, only the name is needed to build, but for edges both “node1” and “node2” are required, which are the names of the two nodes you are connecting.  
<br>
## Example

<br>
This example demonstrates a possible gdf data structure.
<br>
  nodedef>name VARCHAR,label VARCHAR
  <br>
  s1,Site number 1
  <br>
  s2,Site number 2
  <br>
  s3,Site number 3
  <br>
  edgedef>node1 VARCHAR,node2 VARCHAR, weight DOUBLE
  <br>
  s1,s2,1.2341
  <br>
  s2,s3,0.453
  <br>
  s3,s2, 2.34
  <br>
  s3,s1, 0.871
  <br>


## Reading GDF files
<br>

To read GDF files, just use the ofFile function located in the gdf module of the FSharp.FGL.IO namespace. It does not need anything but the file path ans will return the vertices and edges as a vertex list, edge list tupel.
*)
open Aether
open FSharp.FGL.IO
GDF.fromFile @"C:\YourPathString"
(**
Additionally, you can use the ofSeq function instead of ofFile to directly transform a sequence to a vertex list, edge list tupel.
<br>
*)
   
open Aether
open FSharp.FGL.IO
GDF.fromSeq (nodedef>name VARCHAR,label VARCHAR;s1,Site number 1;edgedef>node1 VARCHAR,node2 VARCHAR, weight DOUBLE;s1,s2,1.2341)
