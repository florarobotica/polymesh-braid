Imports PolyMeshLib.Core.Graphs

Namespace Solvers

    Public Class StripCountSolver

        Public Sub New()

        End Sub

        Private _solvedvalues As SortedList(Of DirectedEdge, Integer)

        Public Property EdgeValues As SortedList(Of DirectedEdge, Integer)
            Get
                Return _solvedvalues
            End Get
            Set(value As SortedList(Of DirectedEdge, Integer))
                _solvedvalues = value
            End Set
        End Property

        ''' <summary>
        ''' Enable 2 vertices with the same source distance value to connect to each other.
        ''' </summary>
        ''' <returns></returns>
        Public Property EqualDistanceInterconnect As Boolean = False

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="Graph"></param>
        ''' <param name="Sources"></param>
        ''' <param name="MinimalValue"></param>
        ''' <param name="SolverType">0:VertexOrder 1:BFS 2:ReverseBFS 3+:Random</param>
        ''' <returns></returns>
        Public Function Solve(Graph As UndirectedGraph(Of Point3d), Sources As IEnumerable(Of Integer), Optional MinimalValue As Integer = 3, Optional SolverType As Integer = 0) As DirectedGraph(Of Point3d)
            Dim Adjacent() As List(Of Integer) = Graph.GetAdjacencyMatrix()
            Dim Dir As New SortedList(Of UndirectedEdge, Integer)
            Dim paths As List(Of List(Of Integer)) = GraphBase.BreadthFirstTree(Sources, Adjacent)

            Dim ord As New List(Of Integer)

            Select Case SolverType
                Case 0
                    'def
                Case 1, 1
                    Dim cnt As Integer = 0
                    Dim added(Graph.VertexCount - 1) As Boolean

                    Do
                        Dim ex As Boolean = True
                        For i As Integer = 0 To paths.Count - 1 Step 1
                            If paths(i).Count > cnt Then
                                If added(paths(i)(cnt)) Then Continue For
                                added(paths(i)(cnt)) = True
                                ord.Add(paths(i)(cnt))
                                ex = False
                            End If
                        Next
                        If ex Then Exit Do
                        cnt += 1
                    Loop

                    If SolverType = 2 Then ord.Reverse()

                Case > 2

                    Dim vals(Graph.VertexCount - 1) As Double
                    Dim ids(Graph.VertexCount - 1) As Integer

                    Dim rnd As New Random(SolverType)

                    For i As Integer = 0 To vals.Length - 1 Step 1
                        ids(i) = i
                        vals(i) = rnd.NextDouble
                    Next

                    Array.Sort(vals, ids)
                    ord = ids.ToList
            End Select

            Dim rows() As Integer = AssignRow(Graph.VertexCount, paths)

            For Each pth As List(Of Integer) In paths
                For i As Integer = 0 To pth.Count - 2 Step 1
                    Dir(New UndirectedEdge(pth(i), pth(i + 1))) = If(pth(i) < pth(i + 1), 1, -1)
                Next
            Next

            DirectByRow(Dir, rows, Adjacent, ord)

            Dim subgr As New DirectedGraph(Of Double)
            For Each p As Point3d In Graph.Vertices
                subgr.Vertices.Add(0)
            Next

            For Each k As UndirectedEdge In Dir.Keys
                If Dir(k) = 1 Then
                    If k.PointA < k.PointB Then subgr.Edges.Add(New DirectedEdge(k.PointA, k.PointB))
                    If k.PointA > k.PointB Then subgr.Edges.Add(New DirectedEdge(k.PointB, k.PointA))
                Else
                    If k.PointA > k.PointB Then subgr.Edges.Add(New DirectedEdge(k.PointA, k.PointB))
                    If k.PointA < k.PointB Then subgr.Edges.Add(New DirectedEdge(k.PointB, k.PointA))
                End If
            Next

            Dim values As SortedList(Of DirectedEdge, Integer) = PropagateValues(subgr, Sources, MinimalValue)

            Dim dirvals As New List(Of Integer)

            If EdgeValues Is Nothing Then EdgeValues = New SortedList(Of DirectedEdge, Integer)
            EdgeValues.Clear()

            For Each k As DirectedEdge In subgr.Edges
                EdgeValues(k) = (values(k))
            Next

            Dim outgr As New DirectedGraph(Of Point3d)
            outgr.Vertices.AddRange(Graph.Vertices)

            For Each ed As DirectedEdge In subgr.Edges
                outgr.Edges.Add(ed)
            Next

            Return outgr
        End Function

        Function AssignRow(VertexCount As Integer, Paths As List(Of List(Of Integer))) As Integer()
            Dim row(VertexCount - 1) As Integer
            For Each pth As List(Of Integer) In Paths
                For i As Integer = 0 To pth.Count - 1 Step 1
                    row(pth(i)) = i
                Next
            Next
            Return row
        End Function

        Sub DirectByRow(ByRef dir As SortedList(Of UndirectedEdge, Integer), rows() As Integer, adj() As List(Of Integer), Optional SolveOrder As List(Of Integer) = Nothing)
            Dim pos() As Integer = Nothing
            Dim neg() As Integer = Nothing
            GetStates(dir, adj, pos, neg)

            If SolveOrder Is Nothing Then
                SolveOrder = New List(Of Integer)
                For i As Integer = 0 To adj.Length - 1 Step 1
                    SolveOrder.Add(i)
                Next
            ElseIf SolveOrder.Count = 0 Then
                For i As Integer = 0 To adj.Length - 1 Step 1
                    SolveOrder.Add(i)
                Next
            End If

            For i As Integer = 0 To SolveOrder.Count - 1 Step 1
                Dim thisv As Integer = SolveOrder(i)
                Dim thisnei As List(Of Integer) = adj(thisv)
                Dim myrow As Integer = rows(thisv)

                For j As Integer = 0 To thisnei.Count - 1 Step 1
                    Dim thisn As Integer = thisnei(j)
                    Dim neirow As Integer = rows(thisn)

                    If EqualDistanceInterconnect Then
                        If neirow < myrow Then Continue For
                    Else
                        If neirow <= myrow Then Continue For
                    End If

                    Dim thisk As New UndirectedEdge(thisv, thisn)
                    If dir.ContainsKey(thisk) Then Continue For

                    Dim vertstate As Integer = EvaluateState(pos(thisv), neg(thisv))
                    Dim neistate As Integer = EvaluateState(pos(thisn), neg(thisn))

                    If (vertstate = 0 Or vertstate = 1) And (neistate = 0 Or neistate = 2) Then
                        If thisv < thisn Then
                            dir(thisk) = 1
                        Else
                            dir(thisk) = -1
                        End If

                        pos(thisv) += 1
                        neg(thisn) += 1
                    End If
                Next
            Next
        End Sub

        Function PropagateValues(gr As DirectedGraph(Of Double), sources As IEnumerable(Of Integer), minvalue As Integer) As SortedList(Of DirectedEdge, Integer)
            Dim adj() As List(Of Integer) = gr.GetAdjacencyMatrix
            Dim srt As New SortedList(Of DirectedEdge, Integer)

            Dim paths As New List(Of List(Of Integer))
            For Each src As Integer In sources
                paths.Add(New List(Of Integer)({src}))
            Next

            Dim runit As Boolean = True

            Do
                runit = False
                Dim np As New List(Of List(Of Integer))
                For i As Integer = 0 To paths.Count - 1 Step 1

                    Dim thisp As List(Of Integer) = paths(i)
                    Dim thisadj As List(Of Integer) = adj(thisp(thisp.Count - 1))

                    Dim added As Boolean = False

                    For k As Integer = 0 To thisadj.Count - 1 Step 1
                        Dim thisbranch As New List(Of Integer)(thisp)
                        thisbranch.Add(thisadj(k))
                        added = True

                        np.Add(thisbranch)
                        runit = True
                    Next

                    If Not added Then np.Add(thisp)
                Next
                paths = np
                If Not runit Then Exit Do
            Loop

            For Each ed As DirectedEdge In gr.Edges
                srt(ed) = 0
            Next

            For i As Integer = 0 To paths.Count - 1 Step 1
                For j As Integer = 0 To paths(i).Count - 2 Step 1
                    Dim thiskey As DirectedEdge = New DirectedEdge(paths(i)(j), paths(i)(j + 1))
                    srt(thiskey) = srt(thiskey) + minvalue
                Next
            Next

            Return srt
        End Function

        Private Function EvaluateState(pos As Integer, neg As Integer) As Integer
            If pos = 0 And neg > 0 Then Return 0
            If pos = 1 And neg = 1 Then Return 0
            If pos > 1 And neg = 1 Then Return 1
            If pos = 0 And neg > 1 Then Return 0
            If pos = 1 And neg > 1 Then Return 2
            Return -1
        End Function

        Private Sub GetStates(dir As SortedList(Of UndirectedEdge, Integer), adj() As List(Of Integer), ByRef positive() As Integer, ByRef negative() As Integer)
            ReDim positive(adj.Length - 1)
            ReDim negative(adj.Length - 1)

            For i As Integer = 0 To adj.Length - 1 Step 1
                Dim thisv As Integer = i
                Dim thisnei As List(Of Integer) = adj(i)

                For j As Integer = 0 To thisnei.Count - 1 Step 1
                    Dim thisn As Integer = thisnei(j)
                    Dim thisk As New UndirectedEdge(thisv, thisn)
                    If Not dir.ContainsKey(thisk) Then Continue For

                    If thisn < thisv Then

                        If dir(thisk) = 1 Then
                            negative(i) += 1
                        Else
                            positive(i) += 1
                        End If

                    Else

                        If dir(thisk) = 1 Then
                            positive(i) += 1
                        Else
                            negative(i) += 1
                        End If

                    End If

                Next
            Next
        End Sub

    End Class
End Namespace
