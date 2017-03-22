﻿'Imports PolyMesh_Core.Geometry
'Imports PolyMesh_Core.Graphs
'Imports PolyMesh_Core.CommonTools
'Imports Rhino.Geometry
'Imports PolyMesh_Core


'Namespace Graphs


'    Public Class MeshWrap

'        Private _nodes As New SortedList(Of Integer, PolyMesh)
'        Private _struts As New SortedList(Of UndirectedEdge, PolyMesh)
'        Private _others As New List(Of PolyMesh)

'        Private _graph As UndirectedGraph(Of Point3d) = Nothing
'        Private _Vconn() As List(Of Integer)

'        Private _strutPoints As New SortedList(Of UndirectedEdge(Of Integer), Point3d())

'        Private _stripsEdge As New SortedList(Of UndirectedEdge, Integer)
'        Private _facesEdge As New SortedList(Of UndirectedEdge, Integer)

'        Private _singRadius As Double = 1
'        Private _looseCount As Integer = 5
'        Private _truncation As Double = 0.2

'        Sub New(G As UndirectedGraph(Of Point3d), StripsPerEdge As SortedList(Of UndirectedEdge, Integer), FacesPerEdge As SortedList(Of UndirectedEdge, Integer))
'            Graph = G
'            VertexConnections = G.GetAdjacencyMatrix
'            FacesEdge = FacesPerEdge
'            StripsEdge = StripsPerEdge
'        End Sub

'        Public Property Graph As UndirectedGraph(Of Point3d)
'            Get
'                Return _graph
'            End Get
'            Set(value As UndirectedGraph(Of Point3d))
'                _graph = value
'            End Set
'        End Property

'        Public Property VertexConnections As List(Of Integer)()
'            Get
'                Return _Vconn
'            End Get
'            Set(value As List(Of Integer)())
'                _Vconn = value
'            End Set
'        End Property

'        Public Property StripsEdge As SortedList(Of UndirectedEdge, Integer)
'            Get
'                Return _stripsEdge
'            End Get
'            Set(value As SortedList(Of UndirectedEdge, Integer))
'                _stripsEdge = value
'            End Set
'        End Property

'        Public Property FacesEdge As SortedList(Of UndirectedEdge, Integer)
'            Get
'                Return _facesEdge
'            End Get
'            Set(value As SortedList(Of UndirectedEdge, Integer))
'                _facesEdge = value
'            End Set
'        End Property

'        Public Property LooseCount As Integer
'            Get
'                Return _looseCount
'            End Get
'            Set(value As Integer)
'                _looseCount = value
'            End Set
'        End Property

'        Public Property LooseCirclesRadius As Double
'            Get
'                Return _singRadius
'            End Get
'            Set(value As Double)
'                _singRadius = value
'            End Set
'        End Property

'        Public Property Truncation As Double
'            Get
'                Return _truncation
'            End Get
'            Set(value As Double)
'                _truncation = value
'            End Set
'        End Property

'        Public Property Nodes As SortedList(Of Integer, PolyMesh)
'            Get
'                Return _nodes
'            End Get
'            Set(value As SortedList(Of Integer, PolyMesh))
'                _nodes = value
'            End Set
'        End Property

'        Public Property StrutPoints As SortedList(Of UndirectedEdge(Of Integer), Point3d())
'            Get
'                Return _strutPoints
'            End Get
'            Set(value As SortedList(Of UndirectedEdge(Of Integer), Point3d()))
'                _strutPoints = value
'            End Set
'        End Property

'        Public Property Struts As SortedList(Of UndirectedEdge, PolyMesh)
'            Get
'                Return _struts
'            End Get
'            Set(value As SortedList(Of UndirectedEdge, PolyMesh))
'                _struts = value
'            End Set
'        End Property

'        Public Property OtherMeshes As List(Of PolyMesh)
'            Get
'                Return _others
'            End Get
'            Set(value As List(Of PolyMesh))
'                _others = value
'            End Set
'        End Property

'        Public Function GetStruts() As PolyMesh
'            Dim alln As New List(Of PolyMesh)
'            For i As Integer = 0 To Struts.Keys.Count - 1 Step 1
'                If Struts(Struts.Keys(i)) Is Nothing Then Continue For
'                alln.Add(Struts(Struts.Keys(i)))
'            Next
'            Return PolyMesh.Join(alln)
'        End Function

'        Public Function GetNodes() As PolyMesh
'            Dim alln As New List(Of PolyMesh)
'            For i As Integer = 0 To Nodes.Keys.Count - 1 Step 1
'                If Nodes(Nodes.Keys(i)) Is Nothing Then Continue For
'                alln.Add(Nodes(Nodes.Keys(i)))
'            Next
'            Return PolyMesh.Join(alln)
'        End Function


'        'returns "" if everything is ok , otherwise returns a message of what went wrong.
'        Public Function Build() As String
'            ClearEverything()

'            Dim rep As New String("")
'            If Not ConstructNodes(LooseCirclesRadius) Then rep &= "Failed to construct the nodes" & vbCrLf
'            If Not ConstructStruts() Then rep &= "Failed to construct the struts" & vbCrLf
'            Return rep
'        End Function

'        Public Function ClearEverything() As Boolean
'            If VertexConnections IsNot Nothing Then _Vconn = Graph.GetAdjacencyMatrix
'            StrutPoints.Clear()
'            If Nodes IsNot Nothing Then Nodes.Clear()
'            Return True
'        End Function

'#Region "Meshes"

'        Private Function ConstructStruts() As Boolean

'            Dim success As Boolean = True

'            For Each ed As UndirectedEdge In Graph.Edges
'                If StripsEdge(ed) < 1 Then Continue For
'                Dim thiskeyfrom As New UndirectedEdge(Of Integer)(ed.PointA, ed.PointB, ed.PointA)
'                Dim thiskeyto As New UndirectedEdge(Of Integer)(ed.PointA, ed.PointB, ed.PointB)

'                Dim divs As Integer = FacesEdge(New UndirectedEdge(thiskeyfrom.PointA, thiskeyfrom.PointB))
'                If Not StrutPoints.ContainsKey(thiskeyfrom) Or Not StrutPoints.ContainsKey(thiskeyto) Then Continue For

'                Dim pt1 As List(Of Point3d) = StrutPoints(thiskeyfrom).ToList
'                Dim pt2 As List(Of Point3d) = StrutPoints(thiskeyto).ToList

'                pt1.RemoveAt(pt1.Count - 1)
'                pt2.RemoveAt(pt2.Count - 1)

'                pt2.Reverse()
'                pt2 = AlignPoints(pt1, pt2)
'                pt1.Add(pt1(0))
'                pt2.Add(pt2(0))


'                If pt1.Count <> pt2.Count Then
'                    success = False
'                    Continue For
'                End If

'                Dim pl As New List(Of Polyline)
'                Dim all()() As Point3d = InterpolatePointArrays(pt1.ToArray, pt2.ToArray, divs)

'                For i As Integer = 0 To all.Length - 1 Step 1
'                    pl.Add(New Polyline(all(i)))
'                Next

'                Struts(ed) = PolyMesh.LoftPolylines(pl)
'            Next

'            Return success
'        End Function

'        Private Function ConstructNodes(SingularRadius As Double) As Boolean

'            For i As Integer = 0 To Graph.VertexCount - 1 Step 1
'                'all the connections
'                Dim thisconn As List(Of Integer) = VertexConnections(i)

'                'connections with more than 0 strips 
'                Dim thisconnclean As New List(Of Integer)

'                For j As Integer = 0 To thisconn.Count - 1 Step 1
'                    Dim thisedge As New UndirectedEdge(i, thisconn(j))
'                    If StripsEdge(thisedge) > 0 Then thisconnclean.Add(thisconn(j))
'                Next

'                thisconn = thisconnclean

'                Select Case thisconn.Count
'                    Case 0 'nothing

'                    Case 1 'pointed stick, but still has to add the points to the strut
'                        Dim thisedge As UndirectedEdge = New UndirectedEdge(i, thisconn(0))
'                        Dim thiscircle As New Circle(New Plane(Graph.Vertices(i), Graph.Vertices(i) - Graph.Vertices(thisconn(0))), LooseCirclesRadius)
'                        Dim strips As Integer = StripsEdge(thisedge)

'                        Dim pts() As Point3d = DivideCircle(thiscircle, strips, True)
'                        StrutPoints(New UndirectedEdge(Of Integer)(i, thisconn(0), i)) = pts

'                    Case 2 'elbow
'                        Dim strips1 As Integer = StripsEdge(New UndirectedEdge(i, thisconn(0)))
'                        Dim strips2 As Integer = StripsEdge(New UndirectedEdge(i, thisconn(1)))

'                        Dim thisnode As PolyMesh = Nothing

'                        If strips1 = strips2 Then
'                            Dim elbow As New List(Of Point3d)

'                            For j As Integer = 0 To thisconn.Count - 1 Step 1
'                                Dim vec As Vector3d = (Graph.Vertices(thisconn(j)) - Graph.Vertices(i)) * Truncation
'                                elbow.Add(Graph.Vertices(i) + vec)
'                            Next

'                            elbow.Insert(1, Graph.Vertices(i))

'                            Dim circles As List(Of Circle) = MinimizeCircles(OrientCircles(ElbowTangentCircles(elbow), 0))
'                            Dim ptss As New List(Of Point3d())

'                            For j As Integer = 0 To thisconn.Count - 1 Step 1
'                                Dim pts() As Point3d = DivideCircle(circles(j), strips1, True)
'                                ptss.Add(pts)
'                                StrutPoints(New UndirectedEdge(Of Integer)(i, thisconn(j), i)) = pts
'                            Next

'                            Dim pls As New List(Of Polyline)
'                            pls.Add(New Polyline(ptss(0)))
'                            pls.Add(New Polyline(ptss(1)))
'                            pls(1).Reverse()
'                            thisnode = PolyMesh.LoftPolylines(pls)
'                        Else
'                            Dim triangle As New List(Of Point3d)
'                            Dim strips(2) As Integer
'                            Dim trianglepoints()() As Point3d = Nothing

'                            For j As Integer = 0 To VertexConnections(i).Count - 1 Step 1
'                                Dim vec As Vector3d = Graph.Vertices(VertexConnections(i)(j)) - Graph.Vertices(i)
'                                vec *= Truncation
'                                triangle.Add(Graph.Vertices(i) + vec)

'                                Dim thisedge As UndirectedEdge = New UndirectedEdge(i, VertexConnections(i)(j))
'                                strips(j) = StripsEdge(thisedge)
'                            Next

'                            Dim fakepoint As Point3d = Graph.Vertices(i)
'                            Dim oldfake As Point3d = fakepoint
'                            fakepoint += (Graph.Vertices(i) - triangle(0))
'                            fakepoint += (Graph.Vertices(i) - triangle(1))
'                            Dim trans As Vector3d = fakepoint - oldfake
'                            trans.Unitize()
'                            trans *= LooseCirclesRadius * 2
'                            fakepoint = oldfake + trans

'                            triangle.Add(fakepoint)
'                            strips(2) = strips(0) + strips(1)
'                            thisnode = MakeTriangleNode(triangle, strips, trianglepoints)

'                            Dim loosepts() As Point3d = trianglepoints(2)

'                            Dim otherpts(loosepts.Length - 1) As Point3d
'                            For j As Integer = 0 To loosepts.Length - 1 Step 1
'                                otherpts(j) = loosepts(j) + (fakepoint - Graph.Vertices(i))
'                            Next

'                            Dim pl As New List(Of Polyline)
'                            Dim all()() As Point3d = InterpolatePointArrays(loosepts, otherpts, LooseCount)

'                            For j As Integer = 0 To all.Length - 1 Step 1
'                                pl.Add(New Polyline(all(j)))
'                            Next

'                            OtherMeshes.Add(PolyMesh.LoftPolylines(pl))

'                            For j As Integer = 0 To 1 Step 1
'                                StrutPoints(New UndirectedEdge(Of Integer)(i, VertexConnections(i)(j), i)) = trianglepoints(j)
'                            Next

'                        End If

'                        If thisnode IsNot Nothing Then Nodes.Add(i, thisnode)

'                        'Case 3 'triangle
'                        '    Dim triangle As New List(Of Point3d)
'                        '    Dim strips(2) As Integer
'                        '    Dim trianglepoints()() As Point3d = Nothing

'                        '    For j As Integer = 0 To VertexConnections(i).Count - 1 Step 1
'                        '        Dim vec As Vector3d = Graph.Vertices(VertexConnections(i)(j)) - Graph.Vertices(i)
'                        '        vec *= Truncation
'                        '        triangle.Add(Graph.Vertices(i) + vec)

'                        '        Dim thisedge As UndirectedEdge = New UndirectedEdge(i, VertexConnections(i)(j))
'                        '        strips(j) = StripsEdge(thisedge)
'                        '    Next

'                        '    Nodes.Add(i, MakeTriangle(triangle, strips, trianglepoints))
'3
'                    '    For j As Integer = 0 To trianglepoints.Length - 1 Step 1
'                    '        StrutPoints(New UndirectedEdge(Of Integer)(i, VertexConnections(i)(j), i)) = trianglepoints(j)
'                    '    Next

'                    Case > 2

'                        Dim thisn As New PolyMesh
'                        Dim hullpoints As New List(Of Point3d)
'                        Dim vals As New List(Of Integer)

'                        For j As Integer = 0 To thisconn.Count - 1 Step 1
'                            Dim thisedge As UndirectedEdge = New UndirectedEdge(i, thisconn(j))
'                            Dim vec As Vector3d = Graph.Vertices(thisconn(j)) - Graph.Vertices(i)
'                            vec *= Truncation
'                            hullpoints.Add(Graph.Vertices(i) + vec)
'                            vals.Add(StripsEdge(thisedge))
'                        Next

'                        Dim masterindex As Integer = -1
'                        Dim maxval As Integer = -1
'                        Dim sumall As Integer = 0

'                        For j As Integer = 0 To vals.Count - 1 Step 1
'                            sumall += vals(j)
'                            If vals(j) > maxval Then
'                                masterindex = j
'                                maxval = vals(j)
'                            End If
'                        Next

'                        sumall -= maxval

'                        Dim circles As List(Of Circle) = MinimizeCircles(OrientCircles(MultipleCircles(hullpoints), masterindex))

'                        If sumall = maxval Then
'                            Dim masterindexs() As Integer = Nothing
'                            Dim masterparams() As Double = DivideMasterCircle(circles, masterindex, masterindexs)

'                            Dim allp As New List(Of Point3d)

'                            For j As Integer = 0 To masterindexs.Length - 1 Step 1
'                                Dim childedge As UndirectedEdge(Of Integer) = New UndirectedEdge(Of Integer)(i, thisconn(masterindexs(j)), i)

'                                Dim parfrom As Double = masterparams((j - 1 + masterparams.Length) Mod masterparams.Length) Mod (2 * Math.PI)
'                                Dim parto As Double = masterparams(j) Mod (2 * Math.PI)
'                                Dim mp As New List(Of Point3d)
'                                Dim cp As New List(Of Point3d)
'                                Dim thisnodepart As PolyMesh = NodeToMesh(circles(masterindexs(j)), circles(masterindex), parfrom, parto, vals(masterindexs(j)), mp, cp)

'                                StrutPoints(childedge) = cp.ToArray

'                                mp.RemoveAt(0)
'                                allp.AddRange(mp)
'                                thisn.Append(thisnodepart)
'                            Next

'                            allp = PointCleanup(allp, circles(masterindex))
'                            allp.Add(allp(0))
'                            StrutPoints(New UndirectedEdge(Of Integer)(i, thisconn(masterindex), i)) = allp.ToArray
'                        Else


'                        End If



'                        Nodes.Add(i, thisn)
'                End Select

'            Next

'            Return True
'        End Function

'        ''' <summary>
'        ''' 
'        ''' </summary>
'        ''' <param name="Triangle"></param>
'        ''' <param name="Strips"></param>
'        ''' <param name="PointsForStruts">Points for struts in the Triangle order.</param>
'        ''' <returns></returns>
'        Private Function MakeTriangleNode(Triangle As IEnumerable(Of Point3d), Strips As IEnumerable(Of Integer), ByRef PointsForStruts()() As Point3d) As PolyMesh
'            Dim MasterIndex As Integer = -1
'            Dim MaxCount As Integer = -1
'            ReDim PointsForStruts(2)

'            For j As Integer = 0 To Strips.Count - 1 Step 1
'                If Strips(j) > MaxCount Then
'                    MasterIndex = j
'                    MaxCount = Strips(j)
'                End If
'            Next

'            Dim Circles As List(Of Circle) = MinimizeCircles(OrientCircles(TriangleTangentCircles(Triangle, InscribedCircle(Triangle)), MasterIndex))

'            Dim Indices() As Integer = Nothing
'            Dim Params() As Double = DivideMasterCircle(Circles, MasterIndex, Indices)

'            Dim ThisNode As New PolyMesh
'            Dim AllPoints As New List(Of Point3d)

'            For j As Integer = 0 To Indices.Length - 1 Step 1

'                Dim parfrom As Double = Params((j - 1 + Params.Length) Mod Params.Length) Mod (2 * Math.PI)
'                Dim parto As Double = Params(j) Mod (2 * Math.PI)
'                Dim mp As New List(Of Point3d)
'                Dim cp As New List(Of Point3d)
'                Dim thisnodepart As PolyMesh = NodeToMesh(Circles(Indices(j)), Circles(MasterIndex), parfrom, parto, Strips(Indices(j)), mp, cp)

'                PointsForStruts(Indices(j)) = cp.ToArray

'                mp.RemoveAt(0)
'                AllPoints.AddRange(mp)
'                ThisNode.Append(thisnodepart)
'            Next

'            AllPoints = PointCleanup(AllPoints, Circles(MasterIndex))
'            AllPoints.Add(AllPoints(0))

'            PointsForStruts(MasterIndex) = AllPoints.ToArray

'            Return ThisNode
'        End Function

'        Public Function MinimizeCircles(C As IEnumerable(Of Circle)) As List(Of Circle)
'            Dim nl As New List(Of Circle)
'            For Each cir As Circle In C
'                Dim cc As New Circle(cir.Plane, Math.Min(LooseCirclesRadius, cir.Radius))
'                nl.Add(cc)
'            Next
'            Return nl
'        End Function

'        Public Function PointCleanup(Points As IEnumerable(Of Point3d), C As Circle) As List(Of Point3d)
'            Dim par(Points.Count - 1) As Double
'            Dim np As New List(Of Point3d)

'            For i As Integer = 0 To Points.Count - 1 Step 1
'                Dim p As Double
'                C.ClosestParameter(Points(i), p)
'                par(i) = p
'            Next

'            Array.Sort(par)
'            Dim hs As New HashSet(Of Double)(par)
'            Dim nl As New List(Of Point3d)

'            For Each param As Double In hs
'                nl.Add(C.PointAt(param))
'            Next

'            Return nl
'        End Function

'        Public Function NodeToMesh(Child As Circle,
'                               MasterCircle As Circle,
'                               MasterParamFrom As Double,
'                               MasterParamTo As Double,
'                               Strips As Integer,
'                               ByRef MasterPoints As List(Of Point3d),
'                               ByRef ChildPoints As List(Of Point3d)) As PolyMesh

'            Dim Master As Curve = Nothing
'            If MasterPoints IsNot Nothing Then MasterPoints.Clear()
'            If MasterPoints Is Nothing Then MasterPoints = New List(Of Point3d)

'            If ChildPoints IsNot Nothing Then ChildPoints.Clear()
'            If ChildPoints Is Nothing Then ChildPoints = New List(Of Point3d)

'            Dim cparam As Double
'            MasterCircle.ClosestParameter(Child.Center, cparam)

'            Dim ar As New Arc(MasterCircle.PointAt(MasterParamTo), MasterCircle.PointAt(cparam), MasterCircle.PointAt(MasterParamFrom))
'            Master = ar.ToNurbsCurve
'            Master.Domain = New Interval(0, 1)

'            Dim itC As New Interval(0, Math.PI * 2)
'            Dim nm As New PolyMesh

'            'vertices 
'            For i As Integer = 0 To Strips Step 1
'                Dim cp As Point3d = Child.PointAt(itC.ParameterAt(i / Strips))
'                Dim mp As Point3d = (Master.PointAt(i / Strips))

'                nm.Vertices.Add(cp)
'                nm.Vertices.Add(mp)

'                MasterPoints.Add(mp)
'                ChildPoints.Add(cp)
'            Next

'            'faces
'            For i As Integer = 0 To Strips - 1 Step 1
'                Dim a As Integer = i * 2
'                Dim b As Integer = a + 2
'                Dim c As Integer = a + 3
'                Dim d As Integer = a + 1
'                nm.Faces.Add({a, b, c, d})
'            Next

'            Return nm
'        End Function

'#End Region

'    End Class

'End Namespace
