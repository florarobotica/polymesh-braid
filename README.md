# PolyMesh Braiding library

## Why to use this library ?

The methods and classes provided in this library can be used both to generate a braiding-oriented directed graph, which can be then translated into mesh geometry using the MeshWrap Class.

## How can I create the graph ?

There are two ways to create such graphs: 
* Create a graph by "growing it" with the GraphBuilder.
* Change an undirected graph into a directed one with the GraphTracer.

## What else can it do ?

Once you have the PolyMesh geometry created with the MeshWrap Class, you can use the Weaver to create a braiding pattern. Simply create a SortedList(of TopologyEdge, EdgeColor), set the colors and pass it to the Weaver. This class can generate a range of geometries based on that information, from Polylines to PolyMeshes of different resolutions.

## Dependencies

This library is dependend on a PolyMesh library which is available [here](https://github.com/mateuszzwierzycki/PolyMesh).
