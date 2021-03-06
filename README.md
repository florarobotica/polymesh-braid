# PolyMesh Braiding library

## Why to use this library ?

The methods and classes provided in this library can be used to generate a braiding-oriented directed graph, which can be then translated into mesh geometry using the MeshWrap Class. Finally you can use this geometry to obtain the braiding pattern from the Weaver Class.

## How can I create the graph ?

There are two ways to create such graphs: 
* Create a graph by "growing it" with the GraphBuilder.
* Change an undirected graph into a directed one with the GraphTracer.

## How can I create the braiding pattern ?

Once you have the PolyMesh geometry created with the MeshWrap Class, you can use the Weaver to create a braiding pattern. Simply create a SortedList(of TopologyEdge, EdgeColor), set the colors and pass it to the Weaver. This class can generate a range of geometries based on that information, from Polylines to PolyMeshes of different resolutions.

## Dependencies

This library is dependend on the PolyMeshLib.Core and the Related.Core libraries which is available [here](https://github.com/mateuszzwierzycki/PolyMesh) and [here](https://github.com/mateuszzwierzycki/Related) .
