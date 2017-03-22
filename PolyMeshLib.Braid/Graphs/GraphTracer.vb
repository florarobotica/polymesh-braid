Imports PolyMeshLib.Core.Graphs

Namespace Graphs

    Public Class GraphTracer

        Public Structure EdgeState
            Implements IComparable(Of EdgeState)

            Private _value As Integer
            Private _direction As Integer

            Public Overrides Function ToString() As String
                Return "EdgeState (" & Me.Value & ", " & Me.Direction & ")"
            End Function

            Public Property Value As Integer
                Get
                    Return _value
                End Get
                Set(value As Integer)
                    _value = value
                End Set
            End Property

            ''' <summary>
            ''' 1 if the indices follow the edge direction, 0 if unassigned, -1 if go agains the indices
            ''' </summary>
            ''' <returns></returns>
            Public Property Direction As Integer
                Get
                    Return _direction
                End Get
                Set(value As Integer)
                    _direction = value
                End Set
            End Property


            Sub New(Value As Integer, Direction As Boolean)
                Me.Value = Value
                Me.Direction = Direction
            End Sub

            Public Shared Function Empty() As EdgeState
                Return New EdgeState(0, 0)
            End Function

            Public Function CompareTo(other As EdgeState) As Integer Implements IComparable(Of EdgeState).CompareTo
                'If Me.Value < other.Value Then Return -1
                'If Me.Value > other.Value Then Return 1
                'If Me.Direction < other.Direction Then Return -1
                'If Me.Direction > other.Direction Then Return 1
                Return 0
            End Function
        End Structure

        Private _Points As New List(Of Point3d)
        Private _Graph As UndirectedGraphEdgeT(Of EdgeState)
        Private _Connections() As List(Of Integer)
        Private _Paths As New List(Of List(Of Integer))
        Private _Polylines As New List(Of Polyline)
        Private _oneinmultipleout As Boolean = True

        Sub New(G As UndirectedGraphEdgeT(Of EdgeState), VertexPoints As IEnumerable(Of Point3d))

            Graph = G.Duplicate

            Reset()
            Connections = Graph.GetAdjacencyMatrix()
            Points.AddRange(VertexPoints)
        End Sub

        Public Property OneInMultipleOut As Boolean
            Get
                Return _oneinmultipleout
            End Get
            Set(value As Boolean)
                _oneinmultipleout = value
            End Set
        End Property

        Public Property Graph As UndirectedGraphEdgeT(Of EdgeState)
            Get
                Return _Graph
            End Get
            Set(value As UndirectedGraphEdgeT(Of EdgeState))
                _Graph = value
            End Set
        End Property

        Private Property Connections As List(Of Integer)()
            Get
                Return _Connections
            End Get
            Set(value As List(Of Integer)())
                _Connections = value
            End Set
        End Property

        Public Property Paths As List(Of List(Of Integer))
            Get
                Return _Paths
            End Get
            Set(value As List(Of List(Of Integer)))
                _Paths = value
            End Set
        End Property

        Public Property Polylines As List(Of Polyline)
            Get
                Return _Polylines
            End Get
            Set(value As List(Of Polyline))
                _Polylines = value
            End Set
        End Property

        Public Property Points As List(Of Point3d)
            Get
                Return _Points
            End Get
            Set(value As List(Of Point3d))
                _Points = value
            End Set
        End Property

        Public Sub Reset()
            For Each k As UndirectedEdge(Of EdgeState) In Graph.Edges
                k.Value = New EdgeState(0, 0)
            Next

            Paths.Clear()
            Polylines.Clear()
        End Sub

        ''' <summary>
        ''' Find paths from the Source via each vertex to the Target.
        ''' </summary>
        ''' <param name="Source"></param>
        ''' <param name="Target"></param>
        ''' <returns>True if all paths were created</returns>
        Public Function SourceTargetAll(Source As Integer, Target As Integer, Optional DontUseThoseVertices As IEnumerable(Of Integer) = Nothing) As Boolean
            Dim allok As Boolean = True
            For i As Integer = 0 To Graph.Vertices.Count - 1 Step 1
                If i = Source Then Continue For
                If i = Target Then Continue For
                If Not TryAdd3PtPath(Source, i, Target, DontUseThoseVertices) Then allok = False
            Next
            Return allok
        End Function

        Public Function SourceTargetAllEdges(Source As Integer, Target As Integer, Optional DontUseThoseVertices As IEnumerable(Of Integer) = Nothing) As Boolean
            Dim allok As Boolean = True
            Dim eds As New List(Of EdgeBase)
            For Each ed As UndirectedEdge(Of EdgeState) In Graph.Edges
                If ed.PointA = Source Or ed.PointA = Target Or ed.PointB = Source Or ed.PointB = Target Then Continue For

                eds.Add(ed)
            Next

            For i As Integer = 0 To eds.Count - 1 Step 1
                If Not TryAddEdgePath(Source, Target, eds(i).PointA, eds(i).PointB, DontUseThoseVertices) Then allok = False
            Next

            Return allok
        End Function

        Public Function TryAddEdgePath(FromVertex As Integer, ToVertex As Integer, EdgePtA As Integer, EdgePtB As Integer, Optional DontUseThoseVertices As IEnumerable(Of Integer) = Nothing) As Boolean
            Dim p1 As Integer = FromVertex
            Dim p2 As Integer = EdgePtA
            Dim p3 As Integer = EdgePtB
            Dim p4 As Integer = ToVertex

            Dim thisedge As UndirectedEdge(Of EdgeState) = Graph.Edges(New UndirectedEdge(Of EdgeState)(EdgePtA, EdgePtB, EdgeState.Empty))

            Select Case thisedge.Value.Direction
                Case 0
                    Dim t1 As List(Of Integer) = TryEdgeThisWay(p1, p2, p3, p4, DontUseThoseVertices)
                    If t1 Is Nothing Then t1 = TryEdgeThisWay(p1, p3, p2, p4, DontUseThoseVertices)
                    If t1 IsNot Nothing Then AddPath(t1) : Return True
                Case -1
                    If p2 > p3 Then
                        Dim t1 As List(Of Integer) = TryEdgeThisWay(p1, p2, p3, p4, DontUseThoseVertices)
                        If t1 IsNot Nothing Then AddPath(t1) : Return True
                    ElseIf p2 < p3 Then
                        Dim t1 As List(Of Integer) = TryEdgeThisWay(p1, p3, p2, p4, DontUseThoseVertices)
                        If t1 IsNot Nothing Then AddPath(t1) : Return True
                    End If
                Case 1
                    If p2 > p3 Then
                        Dim t1 As List(Of Integer) = TryEdgeThisWay(p1, p3, p2, p4, DontUseThoseVertices)
                        If t1 IsNot Nothing Then AddPath(t1) : Return True
                    ElseIf p2 < p3 Then
                        Dim t1 As List(Of Integer) = TryEdgeThisWay(p1, p2, p3, p4, DontUseThoseVertices)
                        If t1 IsNot Nothing Then AddPath(t1) : Return True
                    End If
            End Select

            Return False
        End Function

        Private Function TryEdgeThisWay(P1 As Integer, P2 As Integer, P3 As Integer, P4 As Integer, Optional donts As IEnumerable(Of Integer) = Nothing) As List(Of Integer)
            Dim p12Skip(Graph.Vertices.Count - 1) As Boolean
            p12Skip(P3) = True
            p12Skip(P4) = True
            If donts IsNot Nothing Then SetToTrue(p12Skip, donts)

            Dim p12 As List(Of Integer) = FindPath(P1, P2, p12Skip)

            If p12 IsNot Nothing Then
                Dim p34Skip(Graph.Vertices.Count - 1) As Boolean
                If donts IsNot Nothing Then SetToTrue(p34Skip, donts)
                SetToTrue(p34Skip, p12)

                Dim p34 As List(Of Integer) = FindPath(P3, P4, p34Skip)
                If p34 IsNot Nothing Then
                    Dim path As New List(Of Integer)(p12)
                    path.AddRange(p34)
                    Return path
                End If
            End If

            Return Nothing
        End Function


        Private Sub SetToTrue(Bools() As Boolean, Indices As IEnumerable(Of Integer))
            For i As Integer = 0 To Indices.Count - 1 Step 1
                Bools(Indices(i)) = True
            Next
        End Sub

        Public Function TryAdd2PtPath(FromVertex As Integer, ToVertex As Integer, Optional DontUseThoseVertices As IEnumerable(Of Integer) = Nothing) As Boolean
            Dim visited(Graph.Vertices.Count - 1) As Boolean

            If DontUseThoseVertices IsNot Nothing Then
                For Each ind As Integer In DontUseThoseVertices
                    visited(ind) = True
                Next
            End If

            Dim path1 As List(Of Integer) = FindPath(FromVertex, ToVertex, visited)
            If path1 Is Nothing Then Return False
            AddPath(path1)
            Return True
        End Function

        Public Function TryAdd3PtPath(FromVertex As Integer, ViaVertex As Integer, ToVertex As Integer, Optional DontUseThoseVertices As IEnumerable(Of Integer) = Nothing) As Boolean
            Dim visited(Graph.Vertices.Count - 1) As Boolean
            visited(ToVertex) = True
            visited(FromVertex) = True
            visited(ViaVertex) = True

            If DontUseThoseVertices IsNot Nothing Then
                For i As Integer = 0 To DontUseThoseVertices.Count - 1 Step 1
                    visited(DontUseThoseVertices(i)) = True
                Next
            End If

            Dim path1 As List(Of Integer) = FindPath(FromVertex, ViaVertex, visited)
            If path1 Is Nothing Then Return False

            For i As Integer = 0 To visited.Length - 1 Step 1
                visited(i) = False
            Next

            For i As Integer = 0 To path1.Count - 1 Step 1
                visited(path1(i)) = True
            Next

            Dim path2 As List(Of Integer) = FindPath(ViaVertex, ToVertex, visited)
            If path2 Is Nothing Then Return False

            For i As Integer = 1 To path2.Count - 1 Step 1
                path1.Add(path2(i))
            Next

            AddPath(path1)

            Return True
        End Function

        Private Sub AddPath(Path As IEnumerable(Of Integer))

            For i As Integer = 0 To Path.Count - 2 Step 1
                Dim thiskey As New UndirectedEdge(Of EdgeState)(Path(i), Path(i + 1), EdgeState.Empty)
                thiskey = Graph.Edges(thiskey) 'get the actual edge and values
                Dim thisstate As EdgeState = thiskey.Value
                thisstate.Value = thisstate.Value + 1
                thiskey.Value = thisstate
                Graph.Edges(thiskey) = thiskey
            Next

            Dim pl As New Polyline
            For i As Integer = 0 To Path.Count - 1 Step 1
                pl.Add(Points(Path(i)))
            Next
            Polylines.Add(pl)

            For i As Integer = 0 To Path.Count - 2 Step 1
                Dim thiskey As New UndirectedEdge(Of EdgeState)(Path(i), Path(i + 1), EdgeState.Empty)
                Dim thisstate As EdgeState = Graph.Edges(thiskey).Value
                If Path(i) < Path(i + 1) Then
                    thisstate.Direction = 1
                Else
                    thisstate.Direction = -1
                End If
                thiskey.Value = thisstate
                Graph.Edges(thiskey) = thiskey

                '  If OneInMultipleOut Then DirectEdgesAround(Path(i + 1))
            Next

            If OneInMultipleOut Then DirectEdgesAllAround(Path)

        End Sub

        Private Sub DirectEdgesAllAround(Path As IEnumerable(Of Integer))

            'For i As Integer = 1 To Path.Count - 1 Step 1
            '    Dim prevp As Integer = Path(i - 1)
            '    Dim thisp As Integer = Path(i)
            '    Dim thiscon As List(Of Integer) = Connections(thisp)

            '    For j As Integer = 0 To thiscon.Count - 1 Step 1
            '        If thiscon(j) = prevp Then Continue For
            '        Dim ed As UndirectedEdge(Of EdgeState) = Graph.Edges.Item(New UndirectedEdge(Of EdgeState)(thisp, thiscon(j), EdgeState.Empty))

            '        Dim thisstate As EdgeState = ed.Value

            '        If thisstate.Direction = 0 Then
            '            If ed.PointA = thisp Then
            '                If ed.IsAscending Then
            '                    thisstate.Direction = 1
            '                Else
            '                    thisstate.Direction = -1
            '                End If
            '            ElseIf ed.PointB = thisp Then
            '                If ed.IsAscending Then
            '                    thisstate.Direction = -1
            '                Else
            '                    thisstate.Direction = 1
            '                End If
            '            End If
            '        End If

            '        ed.Value = thisstate

            '        Graph.Edges.Item(ed) = ed
            '    Next
            'Next

            For i As Integer = 0 To Graph.VertexCount - 1 Step 1
                Dim thiscon As List(Of Integer) = Connections(i)

                Dim outwards As Integer = 0
                Dim inwards As Integer = 0
                Dim neutral As Integer = 0

                For j As Integer = 0 To thiscon.Count - 1 Step 1
                    Dim ed As UndirectedEdge(Of EdgeState) = Graph.Edges.Item(New UndirectedEdge(Of EdgeState)(i, thiscon(j), EdgeState.Empty))
                    Select Case ed.Value.Direction
                        Case -1
                            If i < thiscon(j) Then inwards += 1
                            If i > thiscon(j) Then outwards += 1
                        Case 0
                            neutral += 1
                        Case 1
                            If i < thiscon(j) Then outwards += 1
                            If i > thiscon(j) Then inwards += 1
                    End Select
                Next

                If neutral > 0 Then
                    If outwards > 1 And inwards = 1 Then
                        'everything outwards

                        For j As Integer = 0 To thiscon.Count - 1 Step 1
                            Dim ed As UndirectedEdge(Of EdgeState) = Graph.Edges.Item(New UndirectedEdge(Of EdgeState)(i, thiscon(j), EdgeState.Empty))
                            If ed.Value.Direction <> 0 Then Continue For

                            Dim thisstate As EdgeState = ed.Value
                            If i < thiscon(j) Then thisstate.Direction = 1
                            If i > thiscon(j) Then thisstate.Direction = -1
                            ed.Value = thisstate

                            Graph.Edges(ed) = ed
                        Next

                    ElseIf inwards > 1 And outwards = 1 Then
                        'everything inwards 

                        For j As Integer = 0 To thiscon.Count - 1 Step 1
                            Dim ed As UndirectedEdge(Of EdgeState) = Graph.Edges.Item(New UndirectedEdge(Of EdgeState)(i, thiscon(j), EdgeState.Empty))
                            If ed.Value.Direction <> 0 Then Continue For

                            Dim thisstate As EdgeState = ed.Value
                            If i > thiscon(j) Then thisstate.Direction = 1
                            If i < thiscon(j) Then thisstate.Direction = -1
                            ed.Value = thisstate

                            Graph.Edges(ed) = ed
                        Next

                    End If
                End If
            Next

        End Sub


        'Private Sub DirectEdgesAround(Vertex As Integer)
        '    Dim thiscon As List(Of Integer) = Connections(Vertex)

        '    Dim pos As Integer = 0
        '    Dim neg As Integer = 0

        '    For i As Integer = 0 To thiscon.Count - 1 Step 1
        '        If thiscon(i) = ComingFromVertex Then Continue For
        '        Dim ed As UndirectedEdge(Of EdgeState) = Graph.Edges.Item(New UndirectedEdge(Of EdgeState)(Vertex, thiscon(i), EdgeState.Empty))
        '        If ed.Value.Direction <> 0 Then Continue For

        '        If Vertex < thiscon(i) Then
        '            pos += 1
        '        Else
        '            neg += 1
        '        End If
        '    Next

        '    If pos Then



        '    'If Vertex < thiscon(i) Then
        '    '    Dim thisval As EdgeState = ed.Value
        '    '    thisval.Direction = -1
        '    '    ed.Value = thisval
        '    'Else
        '    '    Dim thisval As EdgeState = ed.Value
        '    '    thisval.Direction = 1
        '    '    ed.Value = thisval
        '    'End If

        '    'Graph.Edges(ed) = ed


        'End Sub

        Private Function FindPath(Source As Integer, Target As Integer, ByRef SkipVertices() As Boolean) As List(Of Integer)
            Dim collections As New List(Of List(Of Integer))
            collections.Add(New List(Of Integer)({Source}))

            Do
                Dim nextcols As New List(Of List(Of Integer))

                For i As Integer = 0 To collections.Count - 1 Step 1
                    Dim thiscol As List(Of Integer) = collections(i)
                    Dim thisind As Integer = thiscol(thiscol.Count - 1)
                    If thisind = Target Then Return thiscol 'for single point cases

                    Dim thisadj As List(Of Integer) = Connections(thisind)
                    Dim thisadjcorrect As New List(Of Integer)

                    For j As Integer = 0 To thisadj.Count - 1 Step 1
                        Dim thiskey As UndirectedEdge(Of EdgeState) = New UndirectedEdge(Of EdgeState)(thisind, thisadj(j), EdgeState.Empty)
                        thiskey = Graph.Edges(thiskey)
                        Dim thisnei As Integer = thisadj(j)

                        Select Case thiskey.Value.Direction
                            Case 0 'go
                                thisadjcorrect.Add(thisnei)
                            Case -1
                                If thisind > thisnei Then thisadjcorrect.Add(thisnei)
                            Case 1
                                If thisind < thisnei Then thisadjcorrect.Add(thisnei)
                        End Select
                    Next

                    thisadj = thisadjcorrect

                    For j As Integer = 0 To thisadj.Count - 1 Step 1
                        Dim thisnei As Integer = thisadj(j)

                        If thisnei = Target Then thiscol.Add(thisnei) : Return thiscol

                        If Not SkipVertices(thisnei) Then
                            SkipVertices(thisnei) = True
                            Dim ncol As New List(Of Integer)(thiscol)
                            ncol.Add(thisnei)
                            nextcols.Add(ncol)
                        End If
                    Next

                    thiscol.Clear()
                Next

                If nextcols.Count = 0 Then Return Nothing
                collections = nextcols
            Loop
        End Function

    End Class

End Namespace