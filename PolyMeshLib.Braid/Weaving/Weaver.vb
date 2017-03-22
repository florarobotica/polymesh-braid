Imports PolyMeshLib.Core.Types
Imports PolyMeshLib.Braid.Types

Namespace Weaving

    ''' <summary>
    ''' ApplyColors -> BuildStuff
    ''' </summary>
    Public Class Weaver

        Private _owner As PolyMesh = Nothing
        Private _meshnormals() As Vector3d = Nothing
        Private _edges As List(Of TopologyEdge) = Nothing
        Private _faceRotation() As Integer = Nothing

        Private _colors As SortedList(Of TopologyEdge, EdgeColor) = Nothing
        Private _tiledictionary As New SortedList(Of TileHash, List(Of List(Of GridPoint)))

        ''' <summary>
        ''' The Weaver will be changing this mesh, provide a duplicate if you care.
        ''' </summary>
        ''' <param name="Mesh"></param>
        Sub New(Mesh As PolyMesh)
            _owner = Mesh
            _meshnormals = Owner.Vertices.GetVertexNormals
            _edges = Owner.GetTopologyEdges()

            ReDim _faceRotation(Owner.Faces.Count - 1)

            _colors = New SortedList(Of TopologyEdge, EdgeColor)

            For i As Integer = 0 To _edges.Count - 1 Step 1
                _colors(_edges(i)) = EdgeColor.None
            Next

            Dim hs As List(Of TileHash) = GetAllTileHashes()
            For i As Integer = 0 To hs.Count - 1 Step 1
                TileDictionary(hs(i)) = DefaultTiles.GetDefaultStripPlan(hs(i))
            Next
        End Sub

        Private Function GetFaceVertex(FaceIndex As Integer, Vertex As Integer) As Integer
            Return ((Vertex + _faceRotation(FaceIndex) + Owner.Faces(FaceIndex).Count) Mod Owner.Faces(FaceIndex).Count)
        End Function

        Public Property TileDictionary As SortedList(Of TileHash, List(Of List(Of GridPoint)))
            Get
                Return _tiledictionary
            End Get
            Set(value As SortedList(Of TileHash, List(Of List(Of GridPoint))))
                _tiledictionary = value
            End Set
        End Property

        Public Sub ApplyColor(Color As EdgeColor)
            For i As Integer = 0 To _colors.Keys.Count - 1 Step 1
                _colors(_colors.Keys(i)) = Color
            Next
        End Sub

        Public Sub ApplyColors(Colors As SortedList(Of TopologyEdge, EdgeColor))
            _colors = Colors

            For i As Integer = 0 To Me.Owner.Faces.Count - 1 Step 1
                OrientFace(i)
            Next
        End Sub

        ''' <summary>
        ''' If you want to change colors of the all edges, use the ApplyColors.
        ''' Returns a list of updated faces, so you know which one to rebuild.
        ''' </summary>
        ''' <param name="Edge"></param>
        ''' <param name="Color"></param>
        ''' <returns>A list of updated faces, so you know which one to rebuild.</returns>
        Public Function ChangeColor(Edge As TopologyEdge, Color As EdgeColor) As List(Of Integer)
            Dim thisreal As TopologyEdge = _edges(_edges.BinarySearch(Edge))
            Dim nl As New List(Of Integer)
            If thisreal.LeftFace <> -1 Then OrientFace(thisreal.LeftFace) : nl.Add(thisreal.LeftFace)
            If thisreal.RightFace <> -1 Then OrientFace(thisreal.RightFace) : nl.Add(thisreal.LeftFace)
            Return nl
        End Function

        Public Function GetTileHash(FaceIndex As Integer) As TileHash
            'get colors of this face
            Dim hash As TileHash = TileHash.InvalidHash
            Dim edge As List(Of TopologyEdge) = Owner.Faces(FaceIndex).GetEdges
            Dim cols(edge.Count - 1) As EdgeColor

            For i As Integer = 0 To edge.Count - 1 Step 1
                cols(GetFaceVertex(FaceIndex, i)) = _colors(edge(i))
            Next

            'construct hash
            If Owner.Faces(FaceIndex).Count = 3 Then
                hash = TileHash.CreateTriangle(cols)
            ElseIf Owner.Faces(FaceIndex).Count = 4 Then
                hash = TileHash.CreateQuad(cols)
            End If

            Return hash
        End Function

        Public Function BuildStripTopologies() As List(Of List(Of GridPoint))()
            Dim tops(Owner.Faces.Count - 1) As List(Of List(Of GridPoint))
            For i As Integer = 0 To Owner.Faces.Count - 1 Step 1
                tops(i) = TileDictionary(GetTileHash(i))
            Next
            Return tops
        End Function

        Public Function BuildPolylines(Depth As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of Polyline)()
            Dim p(Owner.Faces.Count - 1) As List(Of Polyline)
            Parallel.For(0, Owner.Faces.Count, Sub(i As Integer)
                                                   p(i) = BuildPolyline(i, Depth, Clockwise, BothSides)
                                               End Sub)
            Return p
        End Function

        Public Function BuildSimpleMeshes(Depth As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of PolyMesh)()
            Dim m(Owner.Faces.Count - 1) As List(Of PolyMesh)
            Parallel.For(0, Owner.Faces.Count, Sub(i As Integer)
                                                   m(i) = BuildSimpleMesh(i, Depth, Clockwise, BothSides)
                                               End Sub)
            Return m
        End Function

        Public Function BuildTriangulatedMeshes(Depth As Double, TrianglePerLength As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of PolyMesh)()
            Dim m(Owner.Faces.Count - 1) As List(Of PolyMesh)
            Parallel.For(0, Owner.Faces.Count, Sub(i As Integer)
                                                   m(i) = BuildTriangulatedMesh(i, Depth, TrianglePerLength, Clockwise, BothSides)
                                               End Sub)
            Return m
        End Function

        Public Function BuildTriangulatedMeshes(Depth As Double, TrianglePerLength() As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of PolyMesh)()
            Dim m(Owner.Faces.Count - 1) As List(Of PolyMesh)
            Parallel.For(0, Owner.Faces.Count, Sub(i As Integer)
                                                   m(i) = BuildTriangulatedMesh(i, Depth, TrianglePerLength(i), Clockwise, BothSides)
                                               End Sub)
            Return m
        End Function

        Public Function BuildPolyline(FaceIndex As Integer, Depth As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of Polyline)
            Dim nl As New List(Of Polyline)

            'get colors of this face
            Dim hash As TileHash = GetTileHash(FaceIndex)
            If hash = TileHash.InvalidHash Then Return Nothing

            'construct grids
            Dim grids(,,) As Point3d = BuildGrid(FaceIndex, Depth, Clockwise)

            'get strips and construct the polylines 
            Dim Strips As List(Of List(Of GridPoint)) = TileDictionary(hash)
            For i As Integer = 0 To Strips.Count - 1 Step 1
                Dim pl As New Polyline
                For j As Integer = 0 To Strips(i).Count - 1 Step 1
                    pl.Add(GetGridPoint(grids, Strips(i)(j)))
                Next
                nl.Add(pl)
            Next

            Return nl
        End Function

        Public Function BuildTriangulatedMesh(FaceIndex As Integer, Depth As Double, TrianglePerLength As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of PolyMesh)
            'get colors of this face
            Dim hash As TileHash = GetTileHash(FaceIndex)
            If hash = TileHash.InvalidHash Then Return Nothing

            'construct grids
            Dim grids(,,) As Point3d = BuildGrid(FaceIndex, Depth, Clockwise)

            'construct strips and return
            Return (ConstructTriangulatedMesh(hash, grids, TrianglePerLength))
        End Function

        Public Function BuildVariableMesh(FaceIndex As Integer, Depth As Double, TrianglePerLength As Double, Width As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of PolyMesh)
            'get colors of this face
            Dim hash As TileHash = GetTileHash(FaceIndex)
            If hash = TileHash.InvalidHash Then Return Nothing

            'construct grids
            Dim grids(,,) As Point3d = BuildGrid(FaceIndex, Depth, Clockwise)

            'construct strips and return
            Return (ConstructVariableMesh(hash, grids, TrianglePerLength, Width))
        End Function

        Public Function BuildSimpleMesh(FaceIndex As Integer, Depth As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As List(Of PolyMesh)
            'get colors of this face
            Dim hash As TileHash = GetTileHash(FaceIndex)
            If hash = TileHash.InvalidHash Then Return Nothing

            'construct grids
            Dim grids(,,) As Point3d = BuildGrid(FaceIndex, Depth, Clockwise, BothSides)

            'construct strips and return
            Return (ConstructSimpleMesh(hash, grids))
        End Function

        Public Function BuildGrid(FaceIndex As Integer, Depth As Double, Optional Clockwise As Boolean = True, Optional BothSides As Boolean = True) As Point3d(,,)
            'construct corners
            Dim corn() As Point3d = ConstructCorners(Depth, FaceIndex, BothSides)

            'construct grids
            Dim grids(,,) As Point3d = Nothing
            If Owner.Faces(FaceIndex).Count = 4 Then
                grids = ConstructQuadGrids(Clockwise, corn)
            ElseIf Owner.Faces(FaceIndex).Count = 3 Then
                grids = ConstructTriangleGrids(Clockwise, corn)
            End If

            Return grids
        End Function

        Private Function ConstructSimpleMesh(Hash As TileHash, Grid As Point3d(,,)) As List(Of PolyMesh)
            Dim Strips As List(Of List(Of GridPoint)) = TileDictionary(Hash)
            Dim nl As New List(Of PolyMesh)

            For i As Integer = 0 To Strips.Count - 1 Step 1

                Dim thislines As New List(Of Line)
                Dim thiss As List(Of GridPoint) = Strips(i)

                Dim spt As GridPoint = thiss(0)
                Dim ept As GridPoint = thiss(thiss.Count - 1)

                Dim Sneis As List(Of GridPoint) = GetEdgeNeighbors(Hash, spt)
                Dim Eneis As List(Of GridPoint) = GetEdgeNeighbors(Hash, ept)

                If Sneis.Count > 0 Then
                    Dim gps1 As Point3d = GetGridPoint(Grid, Sneis(0))
                    Dim gps2 As Point3d = GetGridPoint(Grid, Sneis(1))
                    thislines.Add(LineFromMiddle((gps1 + gps2) * 0.5, gps2 - gps1, gps1.DistanceTo(gps2) * 0.25))
                Else
                    thislines.Add(NonEdgeEndLines(Grid, thiss, 0))
                End If

                If Eneis.Count > 0 Then
                    Dim gpe1 As Point3d = GetGridPoint(Grid, Eneis(0))
                    Dim gpe2 As Point3d = GetGridPoint(Grid, Eneis(1))
                    thislines.Add(LineFromMiddle((gpe1 + gpe2) * 0.5, gpe2 - gpe1, gpe1.DistanceTo(gpe2) * 0.25))
                Else
                    thislines.Add(NonEdgeEndLines(Grid, thiss, thiss.Count - 1))
                End If

                Dim linelist As New List(Of Line)
                linelist.Add(thislines(0))
                If thiss.Count > 2 Then linelist.AddRange(ConstructMidLines(Grid, thiss))
                Dim lastl As Line = thislines(thislines.Count - 1)

                If (thiss(0) <> thiss(thiss.Count - 1)) Then
                    lastl.Flip()
                End If

                linelist.Add(lastl)

                nl.Add(LinesToSimpleMesh(linelist))
            Next

            Return nl
        End Function

        Private Function ConstructTriangulatedMesh(Hash As TileHash, Grid As Point3d(,,), TrianglePerLength As Double) As List(Of PolyMesh)
            Dim Strips As List(Of List(Of GridPoint)) = TileDictionary(Hash)
            Dim nl As New List(Of PolyMesh)

            For i As Integer = 0 To Strips.Count - 1 Step 1

                Dim thislines As New List(Of Line)
                Dim thiss As List(Of GridPoint) = Strips(i)

                Dim spt As GridPoint = thiss(0)
                Dim ept As GridPoint = thiss(thiss.Count - 1)

                Dim Sneis As List(Of GridPoint) = GetEdgeNeighbors(Hash, spt)
                Dim Eneis As List(Of GridPoint) = GetEdgeNeighbors(Hash, ept)

                If Sneis.Count > 0 Then
                    Dim gps1 As Point3d = GetGridPoint(Grid, Sneis(0))
                    Dim gps2 As Point3d = GetGridPoint(Grid, Sneis(1))
                    thislines.Add(LineFromMiddle((gps1 + gps2) * 0.5, gps2 - gps1, gps1.DistanceTo(gps2) * 0.25))
                Else
                    thislines.Add(NonEdgeEndLines(Grid, thiss, 0))
                End If

                If Eneis.Count > 0 Then
                    Dim gpe1 As Point3d = GetGridPoint(Grid, Eneis(0))
                    Dim gpe2 As Point3d = GetGridPoint(Grid, Eneis(1))
                    thislines.Add(LineFromMiddle((gpe1 + gpe2) * 0.5, gpe2 - gpe1, gpe1.DistanceTo(gpe2) * 0.25))
                Else
                    thislines.Add(NonEdgeEndLines(Grid, thiss, thiss.Count - 1))
                End If

                Dim linelist As New List(Of Line)
                linelist.Add(thislines(0))
                If thiss.Count > 2 Then linelist.AddRange(ConstructMidLines(Grid, thiss))
                Dim lastl As Line = thislines(thislines.Count - 1)
                If (thiss(0) <> thiss(thiss.Count - 1)) Then lastl.Flip()
                linelist.Add(lastl)

                nl.Add(LinesToMesh(linelist, TrianglePerLength))
            Next

            Return nl
        End Function

        Private Function ConstructVariableMesh(Hash As TileHash, Grid As Point3d(,,), TrianglePerLength As Double, Optional Width As Double = 1) As List(Of PolyMesh)
            Dim Strips As List(Of List(Of GridPoint)) = TileDictionary(Hash)
            Dim nl As New List(Of PolyMesh)

            For i As Integer = 0 To Strips.Count - 1 Step 1

                Dim thislines As New List(Of Line)
                Dim thiss As List(Of GridPoint) = Strips(i)

                Dim spt As GridPoint = thiss(0)
                Dim ept As GridPoint = thiss(thiss.Count - 1)

                Dim Sneis As List(Of GridPoint) = GetEdgeNeighbors(Hash, spt)
                Dim Eneis As List(Of GridPoint) = GetEdgeNeighbors(Hash, ept)

                If Sneis.Count > 0 Then
                    Dim gps1 As Point3d = GetGridPoint(Grid, Sneis(0))
                    Dim gps2 As Point3d = GetGridPoint(Grid, Sneis(1))
                    thislines.Add(LineFromMiddle((gps1 + gps2) * 0.5, gps2 - gps1, gps1.DistanceTo(gps2) * 0.25 * Width))
                Else
                    thislines.Add(NonEdgeEndLines(Grid, thiss, 0))
                End If

                If Eneis.Count > 0 Then
                    Dim gpe1 As Point3d = GetGridPoint(Grid, Eneis(0))
                    Dim gpe2 As Point3d = GetGridPoint(Grid, Eneis(1))
                    thislines.Add(LineFromMiddle((gpe1 + gpe2) * 0.5, gpe2 - gpe1, gpe1.DistanceTo(gpe2) * 0.25 * Width))
                Else
                    thislines.Add(NonEdgeEndLines(Grid, thiss, thiss.Count - 1))
                End If

                Dim linelist As New List(Of Line)
                linelist.Add(thislines(0))
                If thiss.Count > 2 Then linelist.AddRange(ConstructMidLinesVariable(Grid, thiss, Width))
                Dim lastl As Line = thislines(thislines.Count - 1)
                If (thiss(0) <> thiss(thiss.Count - 1)) Then lastl.Flip()
                linelist.Add(lastl)

                nl.Add(LinesToMesh(linelist, TrianglePerLength))
            Next

            Return nl
        End Function

        Private ReadOnly Property Owner As PolyMesh
            Get
                Return _owner
            End Get
        End Property

        Private Sub OrientFace(FaceIndex As Integer)
            Dim thisf As PolyFace = Owner.Faces(FaceIndex)

            Dim cols(thisf.Count - 1) As EdgeColor
            Dim edges As List(Of TopologyEdge) = thisf.GetEdges

            For i As Integer = 0 To edges.Count - 1 Step 1
                cols(i) = (_colors(edges(i)))
            Next

            Dim rots As Integer = TileHash.RotationsRequired(cols)
            _faceRotation(FaceIndex) = rots

            '       Owner.Faces(FaceIndex).Shift(-rots)
        End Sub

        Private Function ConstructCorners(Depth As Double, FaceIndex As Integer, Optional BothSides As Boolean = True) As Point3d()
            Dim corns(7) As Point3d

            If Owner.Faces(FaceIndex).Count = 4 Then
                For i As Integer = 0 To 3 Step 1
                    Dim thisvert As Integer = Owner.Faces(FaceIndex)(GetFaceVertex(FaceIndex, i))
                    If BothSides Then
                        corns(i) = Owner.Vertices(thisvert) - (_meshnormals(thisvert) * Depth)
                    Else
                        corns(i) = Owner.Vertices(thisvert)
                    End If
                    corns(i + 4) = Owner.Vertices(thisvert) + (_meshnormals(thisvert) * Depth)
                Next

            ElseIf Owner.Faces(FaceIndex).Count = 3 Then
                For i As Integer = 0 To 3 Step 1
                    Dim thisvert As Integer
                    If i <> 2 Then
                        If i = 3 Then
                            thisvert = Owner.Faces(FaceIndex)(GetFaceVertex(FaceIndex, 2))
                        Else
                            thisvert = Owner.Faces(FaceIndex)(GetFaceVertex(FaceIndex, i))
                        End If

                        If BothSides Then
                            corns(i) = Owner.Vertices(thisvert) - (_meshnormals(thisvert) * Depth)
                        Else
                            corns(i) = Owner.Vertices(thisvert)
                        End If

                        corns(i + 4) = Owner.Vertices(thisvert) + (_meshnormals(thisvert) * Depth)
                    End If
                Next
            End If

            Return corns
        End Function

        ''' <summary>
        ''' Useful if you want to construct a custom tile dictionary.
        ''' </summary>
        ''' <returns></returns>
        Public Shared Function GetAllTileHashes() As List(Of TileHash)
            Dim nl As New List(Of TileHash)

            'triangles
            nl.Add(TileHash.CreateTriangle({EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain}))
            nl.Add(TileHash.CreateTriangle({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain}))

            'quads
            nl.Add(TileHash.CreateQuad({EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Plain, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Braid, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Braid, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Braid, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid}))

            nl.Add(TileHash.CreateQuad({EdgeColor.Fifth, EdgeColor.Fourth, EdgeColor.Empty, EdgeColor.Fourth}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Sixth, EdgeColor.Fourth, EdgeColor.Empty, EdgeColor.Fourth}))
            nl.Add(TileHash.CreateQuad({EdgeColor.Fourth, EdgeColor.Empty, EdgeColor.Fourth, EdgeColor.Empty}))

            nl.Sort()
            Return nl
        End Function

    End Class
End Namespace
