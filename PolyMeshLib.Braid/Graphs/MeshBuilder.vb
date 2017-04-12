Imports PolyMeshLib.Core.Types
Imports PolyMeshLib.Core.CommonTools
Imports PolyMeshLib.Core.Graphs

Namespace Graphs

    <Obsolete("Please use the MeshWrap Class instead.")>
    Public Class MeshBuilder

        Private _polylines As New List(Of Polyline)
        Private _thickness As New List(Of Integer)
        Private _facesCounts As New List(Of Integer)
        Private _sortThick As New SortedList(Of UndirectedEdge, Integer)
        Private _sortFaces As New SortedList(Of UndirectedEdge, Integer)
        Private _sortCircles As New SortedList(Of UndirectedEdge, Circle())
        Private _sortedPoints As New SortedList(Of Tuple(Of Integer, Integer, Integer), Point3d())

        Private _wrongVertex() As Boolean

        Private _sortEnds As New SortedList(Of Integer, Point3d)
        Private _proportion As Double = 0.1

        Sub New()
        End Sub

        Public Function BuildMesh(Polylines As List(Of Polyline), Strips As List(Of Integer), Faces As List(Of Integer), Proportion As Double) As PolyMesh

            Dim pm As New PolyMesh

            'clean everything 
            Me._polylines.Clear()
            _thickness.Clear()
            _facesCounts.Clear()
            _sortThick.Clear()
            _sortFaces.Clear()
            _sortCircles.Clear()
            _sortedPoints.Clear()
            _sortEnds.Clear()

            'assign variables
            Me._proportion = Proportion
            Me._polylines.AddRange(Polylines)
            _facesCounts.AddRange(Faces)
            _thickness.AddRange(Strips)

            For i As Integer = 1 To _facesCounts.Count - 2 Step 1
                Me._facesCounts(i) -= 6
            Next

            Dim Graph As UndirectedGraph(Of Point3d) = BuildGraph(Polylines)

            Dim Conn() As List(Of Integer) = Graph.GetAdjacencyMatrix

            ReDim _wrongVertex(Graph.Vertices.Count - 1)

            For i As Integer = 0 To _sortThick.Count - 1 Step 1
                Dim thisarr(5) As Circle
                _sortCircles(_sortThick.Keys(i)) = thisarr
            Next

            'build mesh

            For i As Integer = 0 To Graph.Vertices.Count - 1 Step 1

                Dim ThisConn As List(Of Integer) = Conn(i)

                If ThisConn.Count = 1 Then 'this means its a loose end
                    _sortEnds(i) = Graph.Vertices(i)
                    _sortEnds(ThisConn(0)) = Graph.Vertices(ThisConn(0))
                ElseIf ThisConn.Count = 3 Then
                    'get triangle
                    Dim Triangle As New List(Of Point3d)
                    Dim TriVec As New List(Of Vector3d) 'vector at each vertex of the triangle
                    Dim trithick As New List(Of Integer) 'number of strips per each edge 

                    For Each Index As Integer In ThisConn
                        Dim ThisCorn As Point3d = Graph.Vertices(Index)
                        ThisCorn.Transform(TMatrix.Scale(Graph.Vertices(i), Proportion))
                        Dim ThisVec As Vector3d = Graph.Vertices(i) - ThisCorn
                        TriVec.Add(ThisVec)
                        Triangle.Add(ThisCorn)

                        Dim thisedge As New UndirectedEdge(Index, i)
                        thisedge.Orient()

                        trithick.Add(_sortThick(thisedge))
                    Next

                    'get inscribed circle 
                    Dim incir As Circle = InscribedCircle(Triangle)

                    '   contruct circles
                    Dim primCir As List(Of Circle) = TriangleTangentCircles(Triangle, incir)
                    Dim secCir As List(Of Circle) = SecondaryCircles(Triangle, TriVec, primCir)
                    Dim triCir As List(Of Circle) = TertiaryCircles(Triangle, Graph.Vertices(i), primCir)

                    For j As Integer = 0 To primCir.Count - 1 Step 1
                        Dim ThisEdge As New UndirectedEdge(i, ThisConn(j))
                        ThisEdge.Orient()

                        If i = ThisEdge.PointA Then
                            _sortCircles(ThisEdge)(0) = primCir(j)
                            _sortCircles(ThisEdge)(1) = secCir(j)
                            _sortCircles(ThisEdge)(2) = triCir(j)
                        Else
                            _sortCircles(ThisEdge)(5) = primCir(j)
                            _sortCircles(ThisEdge)(4) = secCir(j)
                            _sortCircles(ThisEdge)(3) = triCir(j)
                        End If
                    Next

                    Dim tricheck As New List(Of Integer)(trithick)
                    tricheck.Sort()
                    If tricheck(0) + tricheck(1) <> tricheck(2) Then _wrongVertex(i) = True : Continue For
                    If tricheck(0) = 0 Then _wrongVertex(i) = True : Continue For

                    '   get connector
                    Dim nwo() As Integer = Nothing
                    Dim oriented As List(Of Circle) = OrientPrimary(primCir, trithick, nwo)
                    Dim orientedthickness As New List(Of Integer)

                    For j As Integer = 0 To nwo.Length - 1 Step 1
                        orientedthickness.Add(trithick(nwo(j)))

                        Dim Index As Integer = ThisConn(nwo(j))
                        Dim thisedge As New UndirectedEdge(Index, i)

                        thisedge.Orient()

                        If Index = thisedge.PointA Then
                            _sortCircles(thisedge)(5) = oriented(j)
                        Else
                            _sortCircles(thisedge)(0) = oriented(j)
                        End If

                    Next

                    Dim largecirpoints As List(Of Point3d) = Nothing
                    Dim mediumcirpoints As List(Of Point3d) = Nothing
                    Dim smallcirpoints As List(Of Point3d) = Nothing
                    Dim connector As PolyMesh = MeshConnector(oriented, orientedthickness, largecirpoints, mediumcirpoints, smallcirpoints)

                    For j As Integer = 0 To nwo.Length - 1 Step 1
                        Dim Index As Integer = ThisConn(nwo(j))
                        Dim thisedge As New UndirectedEdge(Index, i)

                        thisedge.Orient()

                        If Index = thisedge.PointA Then
                            Select Case j
                                Case 0
                                    _sortedPoints(New Tuple(Of Integer, Integer, Integer)(thisedge.PointA, thisedge.PointB, 1)) = smallcirpoints.ToArray
                                Case 1
                                    _sortedPoints(New Tuple(Of Integer, Integer, Integer)(thisedge.PointA, thisedge.PointB, 1)) = mediumcirpoints.ToArray
                                Case 2
                                    _sortedPoints(New Tuple(Of Integer, Integer, Integer)(thisedge.PointA, thisedge.PointB, 1)) = largecirpoints.ToArray
                            End Select
                        Else
                            Select Case j
                                Case 0
                                    _sortedPoints(New Tuple(Of Integer, Integer, Integer)(thisedge.PointA, thisedge.PointB, 0)) = smallcirpoints.ToArray
                                Case 1
                                    _sortedPoints(New Tuple(Of Integer, Integer, Integer)(thisedge.PointA, thisedge.PointB, 0)) = mediumcirpoints.ToArray
                                Case 2
                                    _sortedPoints(New Tuple(Of Integer, Integer, Integer)(thisedge.PointA, thisedge.PointB, 0)) = largecirpoints.ToArray
                            End Select
                        End If

                    Next

                    pm.Append(connector)

                End If
            Next

            'construct struts 
            For Each ed As UndirectedEdge In Graph.Edges
                If _wrongVertex(ed.PointA) Or _wrongVertex(ed.PointB) Then Continue For
                If Conn(ed.PointA).Count = 3 And Conn(ed.PointB).Count = 3 Then
                    pm.Append(BuildStrut(ed))
                ElseIf (Conn(ed.PointA).Count = 1 And Conn(ed.PointB).Count = 3) Or (Conn(ed.PointA).Count = 3 And Conn(ed.PointB).Count = 1) Then
                    pm.Append(BuildEndStrut(ed))
                End If
            Next

            pm.Weld(PolyMeshLib.Core.GeometrySettings.Tolerance)
            pm.UnifyFaceNormals()

            Return pm
        End Function

        Private Function BuildEndStrut(Edge As UndirectedEdge) As PolyMesh
            Dim pm As New PolyMesh

            Dim circles() As Circle = _sortCircles(Edge)
            Dim strings As Integer = _sortThick(Edge)

            If circles(0).Radius = 0 Then

                Dim gpoints() As Point3d = _sortedPoints(New Tuple(Of Integer, Integer, Integer)(Edge.PointA, Edge.PointB, 1))
                Dim pts()() As Point3d = CreateCirclePoints(circles(5), circles(4), circles(3), strings, gpoints)

                pm.Append(MeshPoints(pts(0), pts(1)))
                pm.Append(MeshPoints(pts(1), pts(2)))

                Dim ls As Point3d = _sortEnds(Edge.PointA)
                Dim le As Point3d = _sortEnds(Edge.PointB)

                Dim pte(pts(0).Length - 1) As Point3d

                Dim l As New Line(ls, le)
                Dim cpp As Point3d = l.ClosestPoint(pts(2)(0), False)

                For i As Integer = 0 To pte.Length - 1 Step 1
                    pte(i) = pts(2)(i) + (ls - cpp)
                Next

                pm.Append(MeshPoints(pts(2), pte))

            Else

                Dim gpoints() As Point3d = _sortedPoints(New Tuple(Of Integer, Integer, Integer)(Edge.PointA, Edge.PointB, 0))
                Dim pts()() As Point3d = CreateCirclePoints(circles(0), circles(1), circles(2), strings, gpoints)

                pm.Append(MeshPoints(pts(0), pts(1)))
                pm.Append(MeshPoints(pts(1), pts(2)))

                Dim ls As Point3d = _sortEnds(Edge.PointA)
                Dim le As Point3d = _sortEnds(Edge.PointB)

                Dim pte(pts(0).Length - 1) As Point3d

                Dim l As New Line(ls, le)
                Dim cpp As Point3d = l.ClosestPoint(pts(2)(0), False)

                For i As Integer = 0 To pte.Length - 1 Step 1
                    pte(i) = pts(2)(i) + (le - cpp)
                Next

                pm.Append(MeshPoints(pts(2), pte))

            End If

            Return pm
        End Function

        Private Function BuildStrut(Edge As UndirectedEdge) As PolyMesh
            Dim circles() As Circle = _sortCircles(Edge)
            Dim strings As Integer = _sortThick(Edge)

            Dim t0 As New Tuple(Of Integer, Integer, Integer)(Edge.PointA, Edge.PointB, 0)
            Dim t1 As New Tuple(Of Integer, Integer, Integer)(Edge.PointA, Edge.PointB, 1)

            Dim points1()() As Point3d = CreateCirclePoints(circles(0), circles(1), circles(2), strings, _sortedPoints(t0))
            Dim points2()() As Point3d = CreateCirclePoints(circles(5), circles(4), circles(3), strings, _sortedPoints(t1))

            Dim pm As New PolyMesh

            pm.Append(MeshPoints(points1(0), points1(1)))
            pm.Append(MeshPoints(points1(1), points1(2)))
            pm.Append(MeshPoints(points2(0), points2(1)))
            pm.Append(MeshPoints(points2(1), points2(2)))

            Dim inter()() As Point3d = InterpolatePointArrays(points1(2), FixOrientation(points1(2), points2(2)), _sortFaces(Edge))

            For i As Integer = 0 To inter.Length - 2 Step 1
                pm.Append(MeshPoints(inter(i), inter(i + 1)))
            Next

            Return pm
        End Function

        Private Function FixOrientation(Guide() As Point3d, ToOrient() As Point3d) As Point3d()
            Dim cnt As Integer = ToOrient.Length

            Dim Arev(ToOrient.Length - 1) As Point3d
            Dim idx As Integer = 0

            For i As Integer = ToOrient.Length - 1 To 0 Step -1
                Arev(idx) = ToOrient(i)
                idx += 1
            Next

            Dim Asum(cnt - 1) As Double
            Dim Aind(cnt - 1) As Integer

            Dim ArevSum(cnt - 1) As Double
            Dim ArevInd(cnt - 1) As Integer

            For i As Integer = 0 To cnt - 1 Step 1
                For j As Integer = 0 To cnt - 1 Step 1
                    Asum(i) += ToOrient((j + i + cnt) Mod cnt).DistanceTo(Guide(j))
                    Aind(i) = i
                Next
            Next

            For i As Integer = 0 To cnt - 1 Step 1
                For j As Integer = 0 To cnt - 1 Step 1
                    ArevSum(i) += Arev((j + i + cnt) Mod cnt).DistanceTo(Guide(j))
                    ArevInd(i) = i
                Next
            Next

            Array.Sort(Asum, Aind)
            Array.Sort(ArevSum, ArevInd)

            Dim AFinale(cnt - 1) As Point3d

            If Asum(0) < ArevSum(0) Then
                For i As Integer = 0 To cnt - 1 Step 1
                    AFinale(i) = ToOrient((i + Aind(0) + cnt) Mod cnt)
                Next
            Else
                For i As Integer = 0 To cnt - 1 Step 1
                    AFinale(i) = Arev((i + ArevInd(0) + cnt) Mod cnt)
                Next
            End If

            Return AFinale
        End Function

        Private Function MeshPoints(PointsDown() As Point3d, PointsUp() As Point3d) As PolyMesh
            Dim pm As New PolyMesh
            pm.Vertices.AddRange(PointsDown)
            pm.Vertices.AddRange(PointsUp)

            For i As Integer = 0 To PointsDown.Length - 1 Step 1
                Dim a As Integer = i
                Dim b As Integer = (i + 1 + PointsDown.Length) Mod PointsDown.Length
                Dim c As Integer = i + PointsDown.Length
                Dim d As Integer = ((c + 1 + PointsDown.Length) Mod PointsDown.Length) + PointsDown.Length


                pm.Faces.Add({a, b, d, c})


            Next

            Return pm
        End Function

        Private Function CreateCirclePoints(CGuide As Circle, CMiddle As Circle, CLast As Circle, Strings As Integer, GuidePoints() As Point3d) As Point3d()()
            Dim circles() As Circle = {CGuide, CMiddle, CLast}
            Dim ppc(2)() As Point3d

            For j As Integer = 0 To 2 Step 1
                Dim pts(Strings - 1) As Point3d
                ppc(j) = pts
            Next

            ppc(0) = GuidePoints

            For i As Integer = 0 To 1 Step 1
                Dim thisg As Integer = i
                Dim thisi As Integer = i + 1

                Dim thisset() As Point3d = ppc(thisg)

                Dim cg As Circle = circles(thisg)
                Dim ci As Circle = circles(thisi)

                Dim trans As TMatrix = TMatrix.PlaneToPlane(cg.Plane, ci.Plane)
                Dim scale As TMatrix = TMatrix.Scale(ci.Center, ci.Radius / cg.Radius)

                Dim totl(Strings * 2 - 1) As Double
                Dim totj(Strings * 2 - 1) As Integer

                For j As Integer = 0 To Strings * 2 - 1 Step 1
                    Dim rotate As TMatrix = TMatrix.Rotate(ci.Plane.Origin, ci.Plane.ZAxis, ((j / (Strings * 2)) * Math.PI * 2))
                    Dim sum As Double = 0

                    For k As Integer = 0 To thisset.Length - 1 Step 1
                        Dim tp As Point3d = thisset(k)
                        tp.Transform(trans)
                        tp.Transform(scale)
                        tp.Transform(rotate)
                        sum += tp.DistanceTo(thisset(k))
                    Next

                    totl(j) = sum
                    totj(j) = j
                Next

                Array.Sort(totl, totj)

                Dim rotateFinal As TMatrix = TMatrix.Rotate(ci.Plane.Origin, ci.Plane.ZAxis, ((totj(0) / (Strings * 2)) * Math.PI * 2))

                For j As Integer = 0 To thisset.Length - 1 Step 1
                    Dim tp As Point3d = thisset(j)
                    tp.Transform(trans)
                    tp.Transform(scale)
                    tp.Transform(rotateFinal)
                    ppc(thisi)(j) = tp
                Next

            Next

            Return ppc
        End Function

        Private Function BuildGraph(Polylines As List(Of Polyline)) As UndirectedGraph(Of Point3d)
            Dim Graph As UndirectedGraph(Of Point3d) = Nothing
            Dim l As New List(Of Line)

            For Each pl As Polyline In Polylines
                l.Add(New Line(pl(0), pl(pl.Count - 1)))
            Next

            Dim nind As New SortedList(Of UndirectedEdge, Integer)
            Graph = GraphFactory.LinesToUGraph(l, PolyMeshLib.Core.GeometrySettings.Tolerance * 2, nind)

            Dim grc As New UndirectedGraph(Of Point3d)(Graph.Vertices)

            For Each ed As UndirectedEdge In Graph.Edges
                Dim thised As UndirectedEdge = ed.Duplicate
                thised.Orient()
                grc.Edges.Add(thised)

                _sortThick(thised) = _thickness(nind(ed))
                _sortFaces(thised) = _facesCounts(nind(ed))
            Next

            Graph = grc

            Return Graph
        End Function

        Private Function MeshConnector(Circles As List(Of Circle), Values As List(Of Integer), ByRef LargeCircleDivision As List(Of Point3d), ByRef MiddleCircleDivision As List(Of Point3d), ByRef SmallCircleDivision As List(Of Point3d)) As PolyMesh

            For i As Integer = 0 To 1 Step 1
                Dim cir As Circle = Circles(i)
                cir.Flip()
                Circles(i) = cir
            Next

            Dim half1 As New PolyMesh
            Dim half2 As New PolyMesh

            Dim Lpt As New List(Of Point3d)
            Dim Mpt As New List(Of Point3d)
            Dim Spt As New List(Of Point3d)

            For i As Integer = 0 To Values(0) Step 1
                half1.Vertices.Add(Circles(0).PointAt(i / (Values(0)) * Math.PI * 2))
                If i < Values(0) Then Spt.Add(Circles(0).PointAt(i / ((Values(0))) * Math.PI * 2))
            Next

            For i As Integer = 0 To Values(0) Step 1
                half1.Vertices.Add(Circles(2).PointAt(i / (Values(0)) * Math.PI))
            Next

            For i As Integer = 0 To Values(0) - 1 Step 1
                half1.Faces.Add({i, i + Values(0) + 1, i + Values(0) + 2, i + 1})
            Next

            For i As Integer = 0 To Values(1) Step 1
                half2.Vertices.Add(Circles(1).PointAt(i / (Values(1)) * Math.PI * 2))
                If i < Values(1) Then Mpt.Add((Circles(1).PointAt(i / ((Values(1))) * Math.PI * 2)))
            Next

            For i As Integer = 0 To Values(1) Step 1
                half2.Vertices.Add(Circles(2).PointAt(i / (Values(1)) * Math.PI + Math.PI))
            Next

            For i As Integer = 0 To Values(0) - 1 Step 1
                Lpt.Add((Circles(2).PointAt(i / ((Values(0))) * Math.PI)))
            Next

            For i As Integer = 0 To Values(1) - 1 Step 1
                Lpt.Add((Circles(2).PointAt(i / ((Values(1))) * Math.PI + Math.PI)))
            Next

            For i As Integer = 0 To Values(1) - 1 Step 1
                half2.Faces.Add({i, i + Values(1) + 1, i + Values(1) + 2, i + 1})
            Next

            half1.Append(half2)
            LargeCircleDivision = Lpt
            MiddleCircleDivision = Mpt
            SmallCircleDivision = Spt

            Return half1
        End Function

        ''' <summary>
        ''' Split the biggest one 
        ''' </summary>
        ''' <param name="Circles"></param>
        ''' <returns></returns>
        Private Function OrientPrimary(Circles As List(Of Circle), Values As List(Of Integer), ByRef NewOrder() As Integer) As List(Of Circle)
            Dim idx(2) As Integer
            For i As Integer = 0 To Circles.Count - 1 Step 1
                idx(i) = i
            Next

            Dim val() As Integer = Values.ToArray
            Array.Sort(val, idx)
            Dim cir(2) As Circle

            For i As Integer = 0 To cir.Length - 1 Step 1
                cir(i) = Circles(idx(i))
            Next

            NewOrder = idx

            Dim large As Circle = cir(2)
            Dim medium As Circle = cir(1)
            Dim small As Circle = cir(0)

            Dim cp As Double = large.ClosestParameter(cir(0).Center)

            cp -= Math.PI * 0.5
            large.Transform(TMatrix.Rotate(large.Plane.Origin, large.Plane.ZAxis, cp))

            'smallest circle gets 0->Pi, medium goes from Pi->2*Pi

            cp = medium.ClosestParameter(large.Center)
            cp += Math.PI
            medium.Transform(TMatrix.Rotate(medium.Plane.Origin, medium.Plane.ZAxis, cp))

            cp = small.ClosestParameter(large.Center)
            cp += Math.PI
            small.Transform(TMatrix.Rotate(small.Plane.Origin, small.Plane.ZAxis, cp))

            Return {small, medium, large}.ToList
        End Function

        Private Function TertiaryCircles(Triangle As List(Of Point3d), Tip As Point3d, PrimaryCircles As List(Of Circle)) As List(Of Circle)
            Dim circles As New List(Of Circle)

            For i As Integer = 0 To Triangle.Count - 1 Step 1
                Dim thist As Point3d = Triangle(i)
                Dim unit As Vector3d = Triangle(i) - Tip
                unit.Normalize()
                thist += unit * PrimaryCircles(i).Radius * 4
                circles.Add(New Circle(New Plane(thist, Tip - thist), PrimaryCircles(i).Radius))
            Next

            Return circles
        End Function

        Private Function SecondaryCircles(Triangle As List(Of Point3d), TriVec As List(Of Vector3d), PrimaryCircles As List(Of Circle)) As List(Of Circle)
            Dim Circles As New List(Of Circle)

            For i As Integer = 0 To Triangle.Count - 1 Step 1
                Dim v1 As Vector3d = PrimaryCircles(i).Center - Triangle(i)
                Dim v2 As Vector3d = TriVec(i)

                v1.Normalize()
                v2.Normalize()

                Dim cen As Point3d = Triangle(i)

                Dim v2long As Vector3d = v2 * PrimaryCircles(i).Radius * 2
                'some threshold value here would be nice 

                cen -= v2long

                Dim nc As New Circle(New Plane(cen, v1 + v2), PrimaryCircles(i).Radius)
                Circles.Add(nc)
            Next

            Return Circles
        End Function

    End Class
End Namespace
