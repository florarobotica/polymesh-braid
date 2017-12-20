Imports Polys.Core.Types
Imports Polys.Core.CommonTools
Imports Related.Graphs

Namespace Graphs

    Public Class MeshWrap

        Private _nodes As New SortedList(Of Integer, PolyMesh)
        Private _struts As New SortedList(Of UEdge, PolyMesh)
        Private _others As New SortedList(Of Integer, PolyMesh)
        Private _naked As New SortedList(Of Integer, Polyline)

        Private _graph As UGraph(Of Point3d) = Nothing
        Private _Vconn() As List(Of Integer)

        Private _strutPoints As New SortedList(Of UEdge(Of Integer), Point3d())

        Private _stripsEdge As New SortedList(Of UEdge, Integer)
        Private _facesEdge As New SortedList(Of UEdge, Integer)

        Private _singRadius As Double = 1
        Private _looseCount As Integer = 5
        Private _truncation As Double = 0.2

        Private _allowsingular As Integer = 0

        Private _rnd As New Random(123)


        ''' <summary>
        ''' Use LenghtEqualizer to get FacesPerEdge and StripCountSolver to get the StripsPerEdge.
        ''' </summary>
        ''' <param name="G"></param>
        ''' <param name="StripsPerEdge"></param>
        ''' <param name="FacesPerEdge"></param>
        Sub New(G As UGraph(Of Point3d), StripsPerEdge As SortedList(Of UEdge, Integer), FacesPerEdge As SortedList(Of UEdge, Integer))
            Graph = G
            VertexConnections = G.GetAdjacencyMatrix
            FacesEdge = FacesPerEdge
            StripsEdge = StripsPerEdge
        End Sub

        'returns "" if everything is ok , otherwise returns a message of what went wrong.
        Public Function Build(Optional ThoseAreOutputs As IEnumerable(Of Integer) = Nothing) As String
            ClearEverything()

            Dim rep As New String("")
            If Not ConstructNodes(LooseCirclesRadius, ThoseAreOutputs) Then rep &= "Failed to construct the nodes" & vbCrLf
            If Not ConstructStruts() Then rep &= "Failed to construct the struts" & vbCrLf
            Return rep
        End Function

        ''' <summary>
        ''' The resulting node geometry, sorted by vertex index.
        ''' </summary>
        ''' <returns></returns>
        Public Property Nodes As SortedList(Of Integer, PolyMesh)
            Get
                Return _nodes
            End Get
            Set(value As SortedList(Of Integer, PolyMesh))
                _nodes = value
            End Set
        End Property

        ''' <summary>
        ''' The resulting strut geometry, sorted by edge.
        ''' </summary>
        ''' <returns></returns>
        Public Property Struts As SortedList(Of UEdge, PolyMesh)
            Get
                Return _struts
            End Get
            Set(value As SortedList(Of UEdge, PolyMesh))
                _struts = value
            End Set
        End Property

        ''' <summary>
        ''' The points used to construct the geometry.
        ''' </summary>
        ''' <returns></returns>
        Public Property StrutPoints As SortedList(Of UEdge(Of Integer), Point3d())
            Get
                Return _strutPoints
            End Get
            Set(value As SortedList(Of UEdge(Of Integer), Point3d()))
                _strutPoints = value
            End Set
        End Property

        ''' <summary>
        ''' If a node becomes an output then the newly introduced mesh is stored here. Sorted by the vertices.
        ''' </summary>
        ''' <returns></returns>
        Public Property HelperMeshes As SortedList(Of Integer, PolyMesh)
            Get
                Return _others
            End Get
            Set(value As SortedList(Of Integer, PolyMesh))
                _others = value
            End Set
        End Property

        ''' <summary>
        ''' Naked edges which are not a part of nodes, sorted by the vertices.
        ''' </summary>
        ''' <returns></returns>
        Public Property EndEdges As SortedList(Of Integer, Polyline)
            Get
                Return _naked
            End Get
            Set(value As SortedList(Of Integer, Polyline))
                _naked = value
            End Set
        End Property

        Public Function GetStrutsAsOneMesh() As PolyMesh
            Dim alln As New List(Of PolyMesh)
            For i As Integer = 0 To Struts.Keys.Count - 1 Step 1
                If Struts(Struts.Keys(i)) Is Nothing Then Continue For
                alln.Add(Struts(Struts.Keys(i)))
            Next
            Return PolyMesh.Join(alln)
        End Function

        Public Function GetNodesAsOneMesh() As PolyMesh
            Dim alln As New List(Of PolyMesh)
            For i As Integer = 0 To Nodes.Keys.Count - 1 Step 1
                If Nodes(Nodes.Keys(i)) Is Nothing Then Continue For
                alln.Add(Nodes(Nodes.Keys(i)))
            Next
            Return PolyMesh.Join(alln)
        End Function

        ''' <summary>
        ''' Introduces singular meshes in 2-connected vertices with uneven amount of strips in each connected edge.
        ''' The value is the tolerance for the difference between the strip count of the edges.
        ''' </summary>
        ''' <returns></returns>
        Public Property AllowSingular As Integer
            Get
                Return _allowsingular
            End Get
            Set(value As Integer)
                _allowsingular = value
            End Set
        End Property

        Public Property Graph As UGraph(Of Point3d)
            Get
                Return _graph
            End Get
            Set(value As UGraph(Of Point3d))
                _graph = value
            End Set
        End Property

        Public Property VertexConnections As List(Of Integer)()
            Get
                Return _Vconn
            End Get
            Set(value As List(Of Integer)())
                _Vconn = value
            End Set
        End Property

        Public Property StripsEdge As SortedList(Of UEdge, Integer)
            Get
                Return _stripsEdge
            End Get
            Set(value As SortedList(Of UEdge, Integer))
                _stripsEdge = value
            End Set
        End Property

        Public Property FacesEdge As SortedList(Of UEdge, Integer)
            Get
                Return _facesEdge
            End Get
            Set(value As SortedList(Of UEdge, Integer))
                _facesEdge = value
            End Set
        End Property

        ''' <summary>
        ''' How many faces are created along the loose end edges.
        ''' </summary>
        ''' <returns></returns>
        Public Property LooseCount As Integer
            Get
                Return _looseCount
            End Get
            Set(value As Integer)
                _looseCount = value
            End Set
        End Property

        Public Property LooseCirclesRadius As Double
            Get
                Return _singRadius
            End Get
            Set(value As Double)
                _singRadius = value
            End Set
        End Property

        Public Property Truncation As Double
            Get
                Return _truncation
            End Get
            Set(value As Double)
                _truncation = value
            End Set
        End Property

        Private Function ClearEverything() As Boolean
            If VertexConnections IsNot Nothing Then _Vconn = Graph.GetAdjacencyMatrix
            StrutPoints.Clear()
            If Nodes IsNot Nothing Then Nodes.Clear()
            Return True
        End Function

        ''' <summary>
        ''' Given as list of points on a sphere, tries to find a new one which is approximately the furthest one possible.
        ''' </summary>
        ''' <param name="x"></param>
        ''' <param name="c"></param>
        ''' <returns></returns>
        Function InventPoint(x As List(Of Point3d), c As Point3d) As Point3d
            If x.Count = 1 Then
                Return -x(0)
            ElseIf x.Count = 2 Then
                Dim pl As New Plane(AveragePoints(x), x(1) - x(0))
                Dim cir As New Circle(pl, c.DistanceTo(x(0)))

                Return cir.PointAt(0)
            ElseIf x.Count = 3 Then
                Dim av As Point3d = AveragePoints(x)

                Dim vecdir As Vector3d = c - av
                vecdir.Normalize()
                vecdir *= c.DistanceTo(x(0))

                Return c + vecdir
            Else
                Dim av As Point3d = AveragePoints(x)

                Dim vec As Vector3d = av - c
                vec.Normalize()
                vec *= x(0).DistanceTo(c)
                av = c - vec

                Return av
            End If
        End Function

#Region "Meshes"

        Private Function ConstructStruts() As Boolean

            Dim success As Boolean = True

            For Each ed As UEdge In Graph.Edges
                If StripsEdge(ed) < 1 Then Continue For
                Dim thiskeyfrom As New UEdge(Of Integer)(ed.PointA, ed.PointB, ed.PointA)
                Dim thiskeyto As New UEdge(Of Integer)(ed.PointA, ed.PointB, ed.PointB)

                Dim divs As Integer = FacesEdge(New UEdge(thiskeyfrom.PointA, thiskeyfrom.PointB))
                If (Not StrutPoints.ContainsKey(thiskeyfrom)) Or (Not StrutPoints.ContainsKey(thiskeyto)) Then Continue For

                Dim pt1 As List(Of Point3d) = StrutPoints(thiskeyfrom).ToList
                Dim pt2 As List(Of Point3d) = StrutPoints(thiskeyto).ToList

                If pt1.Count <> pt2.Count Then
                    success = False
                    Continue For
                End If

                pt1.RemoveAt(pt1.Count - 1)
                pt2.RemoveAt(pt2.Count - 1)

                pt1.Reverse()
                pt2 = AlignPoints(pt1, pt2)
                pt1.Add(pt1(0))
                pt2.Add(pt2(0))

                Dim pl As New List(Of Polyline)
                Dim all()() As Point3d = InterpolatePointArrays(pt1.ToArray, pt2.ToArray, divs)

                For i As Integer = 0 To all.Length - 1 Step 1
                    pl.Add(New Polyline(all(i)))
                Next

                Struts(ed) = PolyMesh.LoftPolylines(pl)
            Next

            Return success
        End Function

        Private Function ConstructNodes(SingularRadius As Double, Optional ThoseAreOutputs As IEnumerable(Of Integer) = Nothing) As Boolean
            If ThoseAreOutputs Is Nothing Then ThoseAreOutputs = New List(Of Integer)

            Dim outs(Graph.Vertices.Count - 1) As Boolean
            For Each ind As Integer In ThoseAreOutputs
                outs(ind) = True
            Next

            For i As Integer = 0 To Graph.VertexCount - 1 Step 1
                'all the connections
                Dim thisconn As List(Of Integer) = VertexConnections(i)

                'connections with more than 0 strips 
                Dim thisconnclean As New List(Of Integer)

                For j As Integer = 0 To thisconn.Count - 1 Step 1
                    Dim thisedge As New UEdge(i, thisconn(j))
                    If StripsEdge(thisedge) > 0 Then thisconnclean.Add(thisconn(j))
                Next

                thisconn = thisconnclean

                Select Case thisconn.Count
                    Case 0 'nothing

                    Case 1 'pointed stick, but still has to add the points to the strut
                        Dim thisedge As UEdge = New UEdge(i, thisconn(0))
                        Dim thiscircle As New Circle(New Plane(Graph.Vertices(i), Graph.Vertices(i) - Graph.Vertices(thisconn(0))), LooseCirclesRadius)
                        Dim strips As Integer = StripsEdge(thisedge)

                        Dim pts() As Point3d = DivideCircle(thiscircle, strips, True)
                        StrutPoints(New UEdge(Of Integer)(i, thisconn(0), i)) = pts

                        Dim aspl As New Polyline(pts)
                        '   aspl.Add(aspl(0))
                        EndEdges(i) = aspl

                    Case 2 'elbow
                        Dim strips1 As Integer = StripsEdge(New UEdge(i, thisconn(0)))
                        Dim strips2 As Integer = StripsEdge(New UEdge(i, thisconn(1)))

                        Dim thisnode As PolyMesh = Nothing

                        If strips1 = strips2 And (Not (outs(i))) Then
                            Dim elbow As New List(Of Point3d)

                            For j As Integer = 0 To thisconn.Count - 1 Step 1
                                Dim vec As Vector3d = (Graph.Vertices(thisconn(j)) - Graph.Vertices(i)) * Truncation
                                elbow.Add(Graph.Vertices(i) + vec)
                            Next

                            elbow.Insert(1, Graph.Vertices(i))

                            Dim circles As List(Of Circle) = MinimizeCircles(OrientCircles(ElbowTangentCircles(elbow), 0))
                            Dim ptss As New List(Of Point3d())

                            For j As Integer = 0 To thisconn.Count - 1 Step 1
                                Dim pts() As Point3d = DivideCircle(circles(j), strips1, True)
                                ptss.Add(pts)
                                StrutPoints(New UEdge(Of Integer)(i, thisconn(j), i)) = pts
                            Next

                            Dim pls As New List(Of Polyline)
                            pls.Add(New Polyline(ptss(0)))
                            pls.Add(New Polyline(ptss(1)))
                            pls(1).Reverse()
                            thisnode = PolyMesh.LoftPolylines(pls)

                        ElseIf ((Math.Max(strips1, strips2) - Math.Min(strips1, strips2)) <= AllowSingular) And (Not (outs(i))) Then

                            Dim triangle As New List(Of Point3d)
                            Dim strips(2) As Integer
                            Dim trianglepoints()() As Point3d = Nothing

                            For j As Integer = 0 To VertexConnections(i).Count - 1 Step 1
                                Dim vec As Vector3d = Graph.Vertices(VertexConnections(i)(j)) - Graph.Vertices(i)
                                vec *= Truncation
                                triangle.Add(Graph.Vertices(i) + vec)
                                Dim thisedge As UEdge = New UEdge(i, VertexConnections(i)(j))
                                strips(j) = StripsEdge(thisedge)
                            Next

                            Dim fakepoint As Point3d = Graph.Vertices(i)
                            Dim oldfake As Point3d = fakepoint
                            fakepoint += (Graph.Vertices(i) - triangle(0))
                            fakepoint += (Graph.Vertices(i) - triangle(1))
                            Dim trans As Vector3d = fakepoint - oldfake
                            trans.Normalize()
                            trans *= LooseCirclesRadius * 2
                            fakepoint = oldfake + trans

                            triangle.Add(fakepoint)
                            strips(2) = (Math.Max(strips1, strips2) - Math.Min(strips1, strips2))
                            thisnode = MakeTriangleNode(triangle, strips, trianglepoints)

                            Dim loosepts() As Point3d = trianglepoints(2)

                            Dim otherpts(loosepts.Length - 1) As Point3d
                            For j As Integer = 0 To loosepts.Length - 1 Step 1
                                Dim vv As Vector3d = (fakepoint - Graph.Vertices(i))
                                vv.Normalize()
                                vv *= LooseCirclesRadius * 2
                                otherpts(j) = loosepts(j) + vv
                            Next

                            Dim pl As New List(Of Polyline)
                            Dim all()() As Point3d = InterpolatePointArrays(loosepts, otherpts, LooseCount)

                            For j As Integer = 0 To all.Length - 1 Step 1
                                Dim thispl As New Polyline(all(j))
                                thispl.Reverse()
                                pl.Add(thispl)
                            Next

                            HelperMeshes.Add(i, PolyMesh.LoftPolylines(pl))
                            EndEdges(i) = pl(pl.Count - 1)

                            For j As Integer = 0 To 1 Step 1
                                StrutPoints(New UEdge(Of Integer)(i, VertexConnections(i)(j), i)) = trianglepoints(j)
                            Next


                        Else
                            Dim triangle As New List(Of Point3d)
                            Dim strips(2) As Integer
                            Dim trianglepoints()() As Point3d = Nothing

                            For j As Integer = 0 To VertexConnections(i).Count - 1 Step 1
                                Dim vec As Vector3d = Graph.Vertices(VertexConnections(i)(j)) - Graph.Vertices(i)
                                vec *= Truncation
                                triangle.Add(Graph.Vertices(i) + vec)

                                Dim thisedge As UEdge = New UEdge(i, VertexConnections(i)(j))
                                strips(j) = StripsEdge(thisedge)
                            Next

                            Dim fakepoint As Point3d = Graph.Vertices(i)
                            Dim oldfake As Point3d = fakepoint
                            fakepoint += (Graph.Vertices(i) - triangle(0))
                            fakepoint += (Graph.Vertices(i) - triangle(1))
                            Dim trans As Vector3d = fakepoint - oldfake
                            trans.Normalize()
                            trans *= LooseCirclesRadius * 2
                            fakepoint = oldfake + trans

                            triangle.Add(fakepoint)
                            strips(2) = strips(0) + strips(1)
                            thisnode = MakeTriangleNode(triangle, strips, trianglepoints)

                            Dim loosepts() As Point3d = trianglepoints(2)

                            Dim otherpts(loosepts.Length - 1) As Point3d
                            For j As Integer = 0 To loosepts.Length - 1 Step 1
                                Dim vv As Vector3d = (fakepoint - Graph.Vertices(i))
                                vv.Normalize()
                                vv *= LooseCirclesRadius * 2
                                otherpts(j) = loosepts(j) + vv
                            Next

                            Dim pl As New List(Of Polyline)
                            Dim all()() As Point3d = InterpolatePointArrays(loosepts, otherpts, LooseCount)

                            For j As Integer = 0 To all.Length - 1 Step 1
                                Dim thispl As New Polyline(all(j))
                                thispl.Reverse()
                                pl.Add(thispl)
                            Next

                            HelperMeshes.Add(i, PolyMesh.LoftPolylines(pl))
                            EndEdges(i) = pl(pl.Count - 1)

                            For j As Integer = 0 To 1 Step 1
                                StrutPoints(New UEdge(Of Integer)(i, VertexConnections(i)(j), i)) = trianglepoints(j)
                            Next

                        End If

                        If thisnode IsNot Nothing Then Nodes.Add(i, thisnode)

                    Case > 2

                        Dim thisn As New PolyMesh
                        Dim hullpoints As New List(Of Point3d)
                        Dim vals As New List(Of Integer)

                        For j As Integer = 0 To thisconn.Count - 1 Step 1
                            Dim thisedge As UEdge = New UEdge(i, thisconn(j))
                            Dim vec As Vector3d = Graph.Vertices(thisconn(j)) - Graph.Vertices(i)
                            vec *= Truncation
                            hullpoints.Add(Graph.Vertices(i) + vec)
                            vals.Add(StripsEdge(thisedge))
                        Next

                        Dim masterindex As Integer = -1
                        Dim maxval As Integer = -1
                        Dim sumall As Integer = 0

                        For j As Integer = 0 To vals.Count - 1 Step 1
                            sumall += vals(j)
                            If vals(j) > maxval Then
                                masterindex = j
                                maxval = vals(j)
                            End If
                        Next

                        If Not (outs(i)) Then sumall -= maxval
                        hullpoints = FixCoplanar(hullpoints, Polys.Core.GeometrySettings.Tolerance)

                        If sumall = maxval And Not outs(i) Then
                            Dim circles As List(Of Circle) = MinimizeCircles(OrientCircles(MultipleCircles(hullpoints), masterindex))
                            Dim masterindexs() As Integer = Nothing
                            Dim masterparams() As Double = DivideMasterCircle(circles, masterindex, masterindexs)

                            Dim allp As New List(Of Point3d)

                            For j As Integer = 0 To masterindexs.Length - 1 Step 1
                                Dim childedge As UEdge(Of Integer) = New UEdge(Of Integer)(i, thisconn(masterindexs(j)), i)

                                Dim parfrom As Double = masterparams((j - 1 + masterparams.Length) Mod masterparams.Length) Mod (2 * Math.PI)
                                Dim parto As Double = masterparams(j) Mod (2 * Math.PI)
                                Dim mp As New List(Of Point3d)
                                Dim cp As New List(Of Point3d)
                                Dim thisnodepart As PolyMesh = NodeToMesh(circles(masterindexs(j)), circles(masterindex), parfrom, parto, vals(masterindexs(j)), mp, cp)

                                StrutPoints(childedge) = cp.ToArray

                                mp.RemoveAt(0)
                                allp.AddRange(mp)
                                thisn.Append(thisnodepart)
                            Next

                            allp = PointCleanup(allp, circles(masterindex))
                            allp.Add(allp(0))
                            StrutPoints(New UEdge(Of Integer)(i, thisconn(masterindex), i)) = allp.ToArray
                        Else
                            Dim fakepoint As Point3d = InventPoint(hullpoints, Graph.Vertices(i))
                            Dim fakesum As Integer = sumall

                            hullpoints.Add(fakepoint)

                            vals.Add(fakesum)

                            masterindex = hullpoints.Count - 1

                            Dim circles As List(Of Circle) = MinimizeCircles(OrientCircles(MultipleCircles(hullpoints), masterindex))

                            Dim masterindexs() As Integer = Nothing
                            Dim masterparams() As Double = DivideMasterCircle(circles, masterindex, masterindexs)

                            Dim allp As New List(Of Point3d)

                            For j As Integer = 0 To masterindexs.Length - 1 Step 1
                                Dim childedge As UEdge(Of Integer) = New UEdge(Of Integer)(i, thisconn(masterindexs(j)), i)

                                Dim parfrom As Double = masterparams((j - 1 + masterparams.Length) Mod masterparams.Length) Mod (2 * Math.PI)
                                Dim parto As Double = masterparams(j) Mod (2 * Math.PI)
                                Dim mp As New List(Of Point3d)
                                Dim cp As New List(Of Point3d)
                                Dim thisnodepart As PolyMesh = NodeToMesh(circles(masterindexs(j)), circles(masterindex), parfrom, parto, vals(masterindexs(j)), mp, cp)

                                StrutPoints(childedge) = cp.ToArray

                                mp.RemoveAt(0)
                                allp.AddRange(mp)
                                thisn.Append(thisnodepart)
                            Next

                            allp = PointCleanup(allp, circles(masterindex))
                            allp.Add(allp(0))

                            Dim loosepts() As Point3d = allp.ToArray

                            Dim otherpts(loosepts.Length - 1) As Point3d
                            For j As Integer = 0 To loosepts.Length - 1 Step 1
                                otherpts(j) = loosepts(j) + (fakepoint - Graph.Vertices(i))
                            Next

                            Dim pl As New List(Of Polyline)
                            Dim all()() As Point3d = InterpolatePointArrays(loosepts, otherpts, LooseCount)

                            For j As Integer = 0 To all.Length - 1 Step 1
                                Dim thispl As New Polyline(all(j))
                                thispl.Reverse()
                                pl.Add(thispl)
                            Next

                            EndEdges(i) = pl(pl.Count - 1)
                            HelperMeshes.Add(i, PolyMesh.LoftPolylines(pl))

                        End If

                        Nodes.Add(i, thisn)
                End Select

            Next

            Return True
        End Function

        Function FixCoplanar(Points As List(Of Point3d), Tolerance As Double) As List(Of Point3d)
            If Points.Count < 3 Then Return Points

            Dim tp As Plane = Plane.XYPlane
            tp = Plane.FitPlane(Points)
            Dim maxd As Double = 0

            For Each p In Points
                maxd = Math.Max(maxd, Math.Abs(tp.DistanceTo(p)))
            Next

            If maxd > Tolerance Then Return Points


            For i As Integer = 0 To Points.Count - 1 Step 1
                Dim c As Point3d = Points(i)

                Dim nv As New Vector3d(_rnd.NextDouble - 0.5, _rnd.NextDouble - 0.5, _rnd.NextDouble - 0.5)
                nv.normalize()
                nv *= Tolerance * 10
                c = c + nv
                Points(i) = c
            Next

            Return Points
        End Function

        Private Function MakeTriangleNode(Triangle As IEnumerable(Of Point3d), Strips As IEnumerable(Of Integer), ByRef PointsForStruts()() As Point3d) As PolyMesh
            Dim MasterIndex As Integer = -1
            Dim MaxCount As Integer = -1
            ReDim PointsForStruts(2)

            For j As Integer = 0 To Strips.Count - 1 Step 1
                If Strips(j) > MaxCount Then
                    MasterIndex = j
                    MaxCount = Strips(j)
                End If
            Next

            Dim Circles As List(Of Circle) = MinimizeCircles(OrientCircles(TriangleTangentCircles(Triangle, InscribedCircle(Triangle)), MasterIndex))

            Dim Indices() As Integer = Nothing
            Dim Params() As Double = DivideMasterCircle(Circles, MasterIndex, Indices)

            Dim ThisNode As New PolyMesh
            Dim AllPoints As New List(Of Point3d)

            For j As Integer = 0 To Indices.Length - 1 Step 1
                Dim parfrom As Double = Params((j - 1 + Params.Length) Mod Params.Length) Mod (2 * Math.PI)
                Dim parto As Double = Params(j) Mod (2 * Math.PI)
                Dim mp As New List(Of Point3d)
                Dim cp As New List(Of Point3d)
                Dim thisnodepart As PolyMesh = NodeToMesh(Circles(Indices(j)), Circles(MasterIndex), parfrom, parto, Strips(Indices(j)), mp, cp)

                PointsForStruts(Indices(j)) = cp.ToArray

                mp.RemoveAt(0)
                AllPoints.AddRange(mp)
                ThisNode.Append(thisnodepart)
            Next

            AllPoints = PointCleanup(AllPoints, Circles(MasterIndex))
            AllPoints.Add(AllPoints(0))

            PointsForStruts(MasterIndex) = AllPoints.ToArray

            Return ThisNode
        End Function

        Public Function MinimizeCircles(C As IEnumerable(Of Circle)) As List(Of Circle)
            Dim nl As New List(Of Circle)
            For Each cir As Circle In C
                Dim cc As New Circle(cir.Plane, Math.Min(LooseCirclesRadius, cir.Radius))
                nl.Add(cc)
            Next
            Return nl
        End Function

        Public Function PointCleanup(Points As IEnumerable(Of Point3d), C As Circle) As List(Of Point3d)
            Dim par(Points.Count - 1) As Double
            Dim np As New List(Of Point3d)

            For i As Integer = 0 To Points.Count - 1 Step 1
                Dim p As Double
                p = C.ClosestParameter(Points(i))
                par(i) = p
            Next

            Array.Sort(par)
            Dim hs As New HashSet(Of Double)(par)
            Dim nl As New List(Of Point3d)

            For Each param As Double In hs
                nl.Add(C.PointAt(param))
            Next

            Return nl
        End Function

        Public Function NodeToMesh(Child As Circle,
                               MasterCircle As Circle,
                               MasterParamFrom As Double,
                               MasterParamTo As Double,
                               Strips As Integer,
                               ByRef MasterPoints As List(Of Point3d),
                               ByRef ChildPoints As List(Of Point3d)) As PolyMesh

            Dim Master As Arc = Nothing
            If MasterPoints IsNot Nothing Then MasterPoints.Clear()
            If MasterPoints Is Nothing Then MasterPoints = New List(Of Point3d)

            If ChildPoints IsNot Nothing Then ChildPoints.Clear()
            If ChildPoints Is Nothing Then ChildPoints = New List(Of Point3d)

            Dim cparam As Double = MasterCircle.ClosestParameter(Child.Center)

            Dim ar As New Arc(MasterCircle.PointAt(MasterParamTo), MasterCircle.PointAt(cparam), MasterCircle.PointAt(MasterParamFrom))
            Master = ar

            Dim itC As New Range(0, Math.PI * 2)
            Dim nm As New PolyMesh

            If Strips = 1 Then
                Dim cp1 As Point3d = Child.PointAt(itC.ValueAtParameter(0.25))
                Dim mp1 As Point3d = (Master.PointAtNormalizedParameter(0))
                Dim cp2 As Point3d = Child.PointAt(itC.ValueAtParameter(0.75))
                Dim mp2 As Point3d = (Master.PointAtNormalizedParameter(1))

                nm.Vertices.Add(cp1)
                nm.Vertices.Add(mp1)
                nm.Vertices.Add(cp2)
                nm.Vertices.Add(mp2)

                MasterPoints.Add(mp1)
                ChildPoints.Add(cp1)
                MasterPoints.Add(mp2)
                ChildPoints.Add(cp2)

            ElseIf Strips = 2 Then

                Dim cp1 As Point3d = Child.PointAt(itC.ValueAtParameter(0.25))
                Dim mp1 As Point3d = (Master.PointAtNormalizedParameter(0))

                Dim cp2 As Point3d = (Child.PointAt(itC.ValueAtParameter(0.25)) + Child.PointAt(itC.ValueAtParameter(0.75))) / 2
                Dim mp2 As Point3d = (Master.PointAtNormalizedParameter(0.5))

                Dim cp3 As Point3d = Child.PointAt(itC.ValueAtParameter(0.75))
                Dim mp3 As Point3d = (Master.PointAtNormalizedParameter(1))

                nm.Vertices.Add(cp1)
                nm.Vertices.Add(mp1)
                nm.Vertices.Add(cp2)
                nm.Vertices.Add(mp2)
                nm.Vertices.Add(cp3)
                nm.Vertices.Add(mp3)

                MasterPoints.Add(mp1)
                ChildPoints.Add(cp1)
                MasterPoints.Add(mp2)
                ChildPoints.Add(cp2)
                MasterPoints.Add(mp3)
                ChildPoints.Add(cp3)
            Else
                'vertices 
                For i As Integer = 0 To Strips Step 1
                    Dim cp As Point3d = Child.PointAt(itC.ValueAtParameter(i / Strips))
                    Dim mp As Point3d = (Master.PointAtNormalizedParameter(i / Strips))

                    nm.Vertices.Add(cp)
                    nm.Vertices.Add(mp)

                    MasterPoints.Add(mp)
                    ChildPoints.Add(cp)
                Next

            End If

            'faces
            For i As Integer = 0 To Strips - 1 Step 1
                Dim a As Integer = i * 2
                Dim b As Integer = a + 2
                Dim c As Integer = a + 3
                Dim d As Integer = a + 1
                nm.Faces.Add({a, b, c, d})
            Next

            Return nm
        End Function

#End Region



    End Class

End Namespace
