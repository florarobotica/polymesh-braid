Imports PolyMeshLib.Braid.Types
Imports PolyMeshLib.Core.Types

Public Module Utils

    Function Curve2Strip(C As Curve) As List(Of GridPoint)
        Dim pl As New Polyline
        If Not C.TryGetPolyline(pl) Then Return Nothing
        Dim nl As New List(Of GridPoint)

        For i As Integer = 0 To pl.Count - 1 Step 1
            nl.Add(GridPoint.Create(pl(i).Z, pl(i).X, pl(i).Y))
        Next
        Return nl
    End Function

    Public Sub SubdivideWColors(ByRef P As PolyMesh, ByRef C As List(Of EdgeColor))

        Dim npm As New PolyMesh
        npm.Vertices.AddRange(P.Vertices)

        Dim te As List(Of TopologyEdge) = P.GetTopologyEdges

        'colors per edges
        Dim cols As New SortedList(Of TopologyEdge, EdgeColor)

        For i As Integer = 0 To te.Count - 1 Step 1
            cols.Add(te(i), C(i))
        Next

        'new points per edge and per face
        Dim pedge As New SortedList(Of TopologyEdge, Integer)
        Dim pface As New SortedList(Of Integer, Integer)

        For i As Integer = 0 To te.Count - 1 Step 1
            Dim this As Point3d = (P.Vertices(te(i).From) + P.Vertices(te(i).To)) * 0.5
            pedge.Add(te(i), npm.Vertices.Count)
            npm.Vertices.Add(this)
        Next

        Dim cents() As Point3d = P.Faces.GetCenters

        For i As Integer = 0 To P.Faces.Count - 1 Step 1
            pface.Add(i, npm.Vertices.Count)
            npm.Vertices.Add(cents(i))
        Next


        Dim colsout As New SortedList(Of TopologyEdge, EdgeColor)

        For i As Integer = 0 To P.Faces.Count - 1 Step 1

            Dim cent As Integer = pface(i)
            For j As Integer = 0 To P.Faces(i).Count - 1 Step 1

                Dim av As Integer = P.Faces(i)(j)
                Dim bv As Integer = P.Faces(i)((j + 1 + P.Faces(i).Count) Mod P.Faces(i).Count)
                Dim dv As Integer = P.Faces(i)((j - 1 + P.Faces(i).Count) Mod P.Faces(i).Count)

                Dim ae As Integer = pedge(New TopologyEdge(av, bv))
                Dim de As Integer = pedge(New TopologyEdge(dv, av))

                npm.Faces.Add({av, ae, cent, de})

                Dim ac As EdgeColor = cols(New TopologyEdge(av, bv))
                Dim dc As EdgeColor = cols(New TopologyEdge(dv, av))

                colsout(New TopologyEdge(cent, de)) = dc
                colsout(New TopologyEdge(de, av)) = dc
                colsout(New TopologyEdge(av, ae)) = ac

            Next

        Next

        P.Dispose()
        P = npm

        C.Clear()
        C = colsout.Values.ToList
    End Sub

End Module
