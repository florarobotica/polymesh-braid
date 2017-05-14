Imports Related.Abstract

Namespace Solvers

    Public Class EdgeLengthEqualizer

        Sub New()

        End Sub

        Public Property AllPathsEqual As Boolean = True
        Public Property Values As SortedList(Of DirectedEdge, Integer)
        Public Property Extension As Integer = 0

        Public Sub Solve(Graph As DirectedGraph(Of Point3d))
            Values = New SortedList(Of DirectedEdge, Integer)

            Dim adj() As List(Of Integer) = Graph.GetAdjacencyMatrix
            Dim rev() As List(Of Integer) = GraphBase.TransposeMatrix(adj)
            Dim sources As List(Of Integer) = DirectedGraphBase.FindSources(adj)

            Dim walks As New List(Of List(Of Integer))

            For i As Integer = 0 To sources.Count - 1 Step 1
                walks.AddRange(DirectedGraphBase.FindAllWalks(sources(i), adj))
            Next

            Dim vertexvalues(Graph.VertexCount - 1) As Integer

            Dim longest As Integer = -1

            For Each l As List(Of Integer) In walks
                For i As Integer = 0 To l.Count - 1 Step 1
                    Dim thisv As Integer = vertexvalues(l(i))
                    vertexvalues(l(i)) = Math.Max(i, thisv)
                    longest = Math.Max(longest, i)
                Next
            Next

            For i As Integer = 0 To longest Step 1
                For j As Integer = 0 To vertexvalues.Length - 1 Step 1
                    If vertexvalues(j) <> i Then Continue For
                    Dim connected As List(Of Integer) = rev(j)
                    Dim thisvalue As Integer = vertexvalues(j)

                    For k As Integer = 0 To connected.Count - 1 Step 1
                        Dim thiscon As Integer = connected(k)
                        Dim thisconvalue As Integer = vertexvalues(thiscon) + 1
                        thisvalue = Math.Max(thisconvalue, thisvalue)
                    Next

                    vertexvalues(j) = thisvalue
                Next
            Next

            If AllPathsEqual Then
                For i As Integer = 0 To vertexvalues.Length - 1 Step 1
                    Dim con As List(Of Integer) = adj(i)
                    Dim revcon As List(Of Integer) = rev(i)

                    If con.Count = 0 Then
                        vertexvalues(i) = longest
                        If revcon.Count = 1 Then
                            vertexvalues(i) += 1
                        End If
                    End If
                Next

                For Each ed As DirectedEdge In Graph.Edges
                    Dim d As Integer = vertexvalues(ed.To) - vertexvalues(ed.From)
                    d = 1 + ((d - 1) * 2)
                    Values.Add(ed, d)
                Next

                For Each ed As DirectedEdge In Graph.Edges
                    Dim con As List(Of Integer) = adj(ed.To)
                    Dim revcon As List(Of Integer) = rev(ed.To)

                    If con.Count = 0 Then
                        If revcon.Count = 1 Then
                            Dim d As Integer = Values(ed)
                            d += Extension - 1
                            Values(ed) = d
                        End If
                    End If
                Next
            Else
                For Each ed As DirectedEdge In Graph.Edges
                    Dim d As Integer = vertexvalues(ed.To) - vertexvalues(ed.From)
                    d = 1 + ((d - 1) * 2)
                    Values.Add(ed, d)
                Next
            End If

        End Sub

    End Class

End Namespace
