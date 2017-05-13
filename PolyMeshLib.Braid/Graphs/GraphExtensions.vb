Imports System.Globalization
Imports System.Runtime.CompilerServices
Imports Related.Graphs

Namespace Graphs

    Public Module GraphOperations

        Public Enum CollapseMethod
            Invalid = -1
            LowestHighest = 0
            LowestLowest = 1
        End Enum

        <Extension()>
        Public Function CollapseGraph(Graph As Related.Graphs.NestedGraph, Optional Collapse As CollapseMethod = CollapseMethod.LowestHighest, Optional CollapseValues() As Double = Nothing) As Boolean

            If CollapseValues Is Nothing Then

                Select Case Collapse
                    Case CollapseMethod.LowestHighest, CollapseMethod.LowestLowest

                        Dim Leafs As List(Of NestedGraph) = Graph.GetLeafs()
                        Dim Index As Integer = -1

                        While Leafs.Count > 1
                            Dim Indices(Leafs.Count - 1) As Integer
                            Dim Values(Leafs.Count - 1) As Double

                            For i As Integer = 0 To Leafs.Count - 1 Step 1
                                Indices(i) = i
                                Values(i) = Leafs(i).Value
                            Next

                            Array.Sort(Values, Indices)

                            Dim Leaf1 As NestedGraph = Leafs(Indices(0))
                            Dim Leaf2 As NestedGraph = Nothing

                            If Collapse = CollapseMethod.LowestLowest Then
                                Leaf2 = Leafs(Indices(1))
                            ElseIf Collapse = CollapseMethod.LowestHighest Then
                                Leaf2 = Leafs(Indices(Indices.Length - 1))
                            End If

                            Dim NewKid As New NestedGraph(Index, Leaf1.Value + Leaf2.Value)
                            Index -= 1

                            Leaf1.AddChild(NewKid)
                            Leaf2.AddChild(NewKid)

                            Leafs.Clear()
                            Leafs = Graph.GetLeafs()

                            Dim NextLeafs As New List(Of NestedGraph)
                            Dim Ids As New HashSet(Of Integer)

                            For i As Integer = 0 To Leafs.Count - 1 Step 1
                                If Not Ids.Contains(Leafs(i).ID) Then
                                    NextLeafs.Add(Leafs(i))
                                    Ids.Add(Leafs(i).ID)
                                End If
                            Next

                            Leafs = NextLeafs
                        End While


                End Select
            Else

                Select Case Collapse
                    Case CollapseMethod.LowestHighest, CollapseMethod.LowestLowest

                        Dim Leafs As List(Of NestedGraph) = Graph.GetLeafs()
                        Dim Index As Integer = -1

                        While Leafs.Count > 1
                            Dim Indices(Leafs.Count - 1) As Integer
                            Dim Values(Leafs.Count - 1) As Double

                            For i As Integer = 0 To Leafs.Count - 1 Step 1
                                Indices(i) = i
                                Values(i) = CollapseValues(i)
                            Next

                            Array.Sort(Values, Indices)

                            Dim Leaf1 As NestedGraph = Leafs(Indices(0))
                            Dim Leaf2 As NestedGraph = Nothing

                            If Collapse = CollapseMethod.LowestLowest Then
                                Leaf2 = Leafs(Indices(1))
                            ElseIf Collapse = CollapseMethod.LowestHighest Then
                                Leaf2 = Leafs(Indices(Indices.Length - 1))
                            End If

                            Dim NewKid As New NestedGraph(Index, Leaf1.Value + Leaf2.Value)
                            Index -= 1

                            Leaf1.AddChild(NewKid)
                            Leaf2.AddChild(NewKid)

                            Leafs.Clear()
                            Leafs = Graph.GetLeafs()

                            Dim NextLeafs As New List(Of NestedGraph)
                            Dim Ids As New HashSet(Of Integer)

                            For i As Integer = 0 To Leafs.Count - 1 Step 1
                                If Not Ids.Contains(Leafs(i).ID) Then
                                    NextLeafs.Add(Leafs(i))
                                    Ids.Add(Leafs(i).ID)
                                End If
                            Next

                            Leafs = NextLeafs
                        End While

                End Select
            End If

            Return True
        End Function

        <Extension()>
        Public Sub GrowGraph(Graph As NestedGraph, Instructions As IEnumerable(Of Double))
            Dim Leafs As List(Of NestedGraph) = Graph.GetLeafs()
            Dim Current As Integer = 0
            Dim ID As Integer = 1

            For i As Integer = 0 To Instructions.Count - 1 Step 1
                Dim Instruction As Double = Instructions(i)
                Dim intInstr As Integer = CType(Instruction, Integer)

                Select Case Instruction
                    Case 0
                        Leafs.Clear()
                        Leafs = Graph.GetLeafs
                        Current = 0
                    Case < 1
                        Dim Node As NestedGraph = Leafs(Current)
                        Node.AddChild(New NestedGraph(ID, Instruction * Node.Value))
                        ID += 1
                        Node.AddChild(New NestedGraph(ID, (1 - Instruction) * Node.Value))
                        ID += 1
                        Current += 1
                    Case Else
                        Leafs.Clear()
                        Leafs = Graph.GetLeafs
                        Current = (Current + intInstr + Leafs.Count) Mod Leafs.Count
                End Select

                If Current > (Leafs.Count - 1) Then
                    Leafs.Clear()
                    Leafs = Graph.GetLeafs
                    Current = 0
                End If

            Next

        End Sub

        Public Function CleanupTreeInstructions(Instructions As List(Of Double)) As List(Of Double)
            Dim CleanUp As New List(Of Double)
            For i As Integer = 0 To Instructions.Count - 1 Step 1
                If Instructions(i) > 1 Then
                    CleanUp.Add(Instructions(i) - (Instructions(i) Mod 1))

                    If Instructions(i) Mod 1 > 0 Then
                        CleanUp.Add(Instructions(i) Mod 1)
                    End If
                Else
                    CleanUp.Add(Instructions(i))
                End If
            Next
            Return CleanUp
        End Function

#Region "MeshCount"

        Public Sub ComputeMeshCounts(Graph As NestedGraph, Optional AddFaces As Integer = 0)
            Graph.Value = 4
            Dim Leafs As List(Of NestedGraph) = Graph.GetLeafs()
            Leafs(0).Value = 3
            Accumulate(Graph, Leafs(0).ID, AddFaces)
            CorrectValues(Graph, Leafs(0).ID)
            Leafs(0).Value = 3
            Dim Flat As New SortedList(Of Integer, NestedGraph)
            NestedGraph.FlatTree(Graph, Flat)
            For i As Integer = 0 To Flat.Count - 1 Step 1
                Flat(Flat.Keys(i)).Value += AddFaces
            Next
        End Sub

        Private Sub Accumulate(Graph As NestedGraph, LastID As Integer, Addition As Integer)
            'accumulate in endpoints
            For i As Integer = 0 To Graph.Children.Count - 1 Step 1
                If Graph.Children(i).ID = LastID Then
                    Graph.Children(i).Value = Math.Max(Graph.Children(i).Value, Graph.Value + 3 + Addition)
                Else
                    Graph.Children(i).Value = Math.Max(Graph.Children(i).Value, Graph.Value + 6 + Addition)
                End If
                Accumulate(Graph.Children(i), LastID, Addition)
            Next
        End Sub

        Private Sub CorrectValues(Graph As NestedGraph, LastID As Integer)
            Dim Sorted As New SortedList(Of Integer, NestedGraph)
            NestedGraph.FlatTree(Graph, Sorted)
            Correction(Sorted(LastID))
        End Sub

        Private Sub Correction(Graph As NestedGraph)
            If Graph.ID = 0 Then Graph.Value = 4 : Return
            Dim LargestValue As Integer = 0
            For i As Integer = 0 To Graph.Parents.Count - 1 Step 1
                LargestValue = Math.Max(LargestValue, Graph.Parents(i).Value)
            Next
            For i As Integer = 0 To Graph.Parents.Count - 1 Step 1
                Graph.Parents(i).Value = (LargestValue - Graph.Parents(i).Value) + 6
                Correction(Graph.Parents(i))
            Next
        End Sub

#End Region

        ''' <summary>
        ''' After growing, before collapsing. 
        ''' </summary>
        ''' <param name="Graph"></param>
        ''' <param name="MinimalValue"></param>
        Public Sub OptimizeSplits(Graph As NestedGraph, MinimalValue As Integer, Minimize As Boolean, ByRef CollapseValues() As Double)
            Dim ls As New SortedList(Of Integer, NestedGraph)
            NestedGraph.FlatTree(Graph, ls)
            Dim l As New List(Of NestedGraph)(ls.Values)

            Dim cv(l.Count - 1) As Double

            Dim srt(l.Count - 1) As NestedGraph
            Dim smallest As Double = Double.MaxValue
            For i As Integer = 0 To l.Count - 1 Step 1
                srt(l(i).ID) = l(i)
                smallest = Math.Min(smallest, l(i).Value)
            Next

            Dim val(l.Count - 1) As Double
            Dim one(l.Count - 1) As Double
            Dim two(l.Count - 1) As Double

            For i As Integer = 0 To srt.Length - 1 Step 1
                val(i) = srt(i).Value
                one(i) = -1
                two(i) = -1
            Next

            Dim vc(val.Length - 1) As Double
            Dim vi(val.Length - 1) As Integer

            For i As Integer = 0 To val.Length - 1 Step 1
                cv(i) = val(i)
                vc(i) = val(i)
                vi(i) = i
            Next

            CollapseValues = cv

            Array.Sort(vc, vi)

            If Minimize Then
                For i As Integer = 0 To val.Length - 1 Step 1
                    val(i) = 1
                    srt(i).Value = val(i)
                Next
            Else
                For i As Integer = 0 To val.Length - 1 Step 1
                    val(i) = Math.Round(val(i) / smallest)
                    srt(i).Value = val(i)
                Next
            End If


            For i As Integer = 0 To vi.Length - 1 Step 1
                Dim thisindex As Integer = vi(i)
                Dim thisnode As NestedGraph = srt(thisindex)

                If thisindex = 0 Then
                    thisnode.Value = val(thisnode.Children(0).ID) + val(thisnode.Children(1).ID)
                Else

                    Dim thispar As NestedGraph = thisnode.Parents(0)
                    If one(thisindex) <> -1 And two(thisindex) <> -1 Then
                        srt(thisindex).Value = one(thisnode.ID) + two(thisnode.ID)
                        val(thisindex) = srt(thisindex).Value
                    End If

                    If one(thispar.ID) = -1 Then
                        one(thispar.ID) = thisnode.Value
                    ElseIf two(thispar.ID) = -1 Then
                        two(thispar.ID) = thisnode.Value
                    End If


                End If
            Next

            For Each n As NestedGraph In l
                n.Value *= MinimalValue
            Next
        End Sub

    End Module

End Namespace
