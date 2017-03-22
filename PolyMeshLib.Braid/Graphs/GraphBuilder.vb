Imports PolyMeshLib.Core.Graphs

Namespace Graphs

    ''' <summary>
    ''' Creates geometry representing a directed graph. Use with DirectedNode.
    ''' </summary>
    Public Class GraphBuilder

        Private _Graph As DirectedNode = Nothing
        Private _NodeIndex As New SortedList(Of Integer, Integer)

        Private _Levels As New List(Of Point3d())
        Private _NodeLevel() As Integer

        Private _Radius As Double = 10
        Private _Height As Double = 1
        Private _NodeCount As Integer = 1

        Private _Lines As New List(Of Line)
        Private _LinesIndex As New List(Of Integer)

        ''' <summary>
        ''' After creating a new instance, you want to specify the class properties (Radius, Height etc.) and then use the Build function.
        ''' </summary>
        Sub New()
        End Sub

        Public Property Radius As Double
            Get
                Return _Radius
            End Get
            Set(value As Double)
                _Radius = value
            End Set
        End Property

        Public Property Height As Double
            Get
                Return _Height
            End Get
            Set(value As Double)
                _Height = value
            End Set
        End Property

        Private Property Level(index As Integer) As Point3d()
            Get
                Return _Levels(index)
            End Get
            Set(value As Point3d())
                _Levels(index) = value
            End Set
        End Property

        Private Property PointAt(level As Integer, index As Integer) As Point3d
            Get
                Return _Levels(level)(index)
            End Get
            Set(value As Point3d)
                _Levels(level)(index) = value
            End Set
        End Property

        Private Property PointAt(index As Integer) As Point3d
            Get
                Return _Levels(_NodeLevel(index))(index)
            End Get
            Set(value As Point3d)
                _Levels(_NodeLevel(index))(index) = value
            End Set
        End Property

        Private ReadOnly Property LevelCount() As Integer
            Get
                Return _Levels.Count
            End Get
        End Property

        Private ReadOnly Property Ids As SortedList(Of Integer, Integer)
            Get
                Return _NodeIndex
            End Get
        End Property


        ''' <summary>
        ''' Returns a SortedList of polylines, where the key is the index of the node.
        ''' </summary>
        ''' <param name="Graph"></param>
        ''' <returns></returns>
        Public Function Build(Graph As DirectedNode) As SortedList(Of Integer, Polyline)
            _Levels.Clear()
            _Lines.Clear()
            _LinesIndex.Clear()
            _NodeIndex.Clear()

            _Graph = Graph
            _NodeCount = TotalNodeCount(_Graph)
            _NodeLevel = PolyMeshLib.Core.CommonTools.CreateArray(_NodeCount, 0)
            AddLevel()

            Dim ls As New List(Of DirectedNode)
            ls.Add(_Graph)

            While ls.Count > 0
                Dim nt As New List(Of DirectedNode)
                For i As Integer = 0 To ls.Count - 1 Step 1
                    nt.AddRange(BuildSplitNode(ls(i)))
                Next
                ls = nt
            End While
            For i As Integer = _NodeIndex.Keys.Count - 1 To 0 Step -1
                If _NodeIndex.Keys(i) < 0 Then
                    BuildMergeNode(_Graph.FindNode(_NodeIndex.Keys(i)), i = 0)
                End If
            Next

            Dim srt As New SortedList(Of Integer, Polyline)

            Dim presrt As New SortedList(Of Integer, List(Of Line))

            For i As Integer = 0 To _LinesIndex.Count - 1 Step 1
                presrt(_LinesIndex(i)) = New List(Of Line)
            Next

            For i As Integer = 0 To _Lines.Count - 1 Step 1
                presrt(_LinesIndex(i)).Add(_Lines(i))
            Next

            For i As Integer = 0 To presrt.Keys.Count - 1 Step 1
                Dim thisl As List(Of Line) = presrt(presrt.Keys(i))
                Dim thispl As New Polyline
                thispl.Add(thisl(0).From)
                For j As Integer = 0 To thisl.Count - 1 Step 1
                    thispl.Add(thisl(j).To)
                Next
                srt(presrt.Keys(i)) = thispl
            Next

            Return srt
        End Function


#Region "Privates"

        Private Sub BuildMergeNode(Node As DirectedNode, LastOne As Boolean)
            Dim Index As Integer = _NodeIndex(Node.ID)

            If Node IsNot Nothing Then

                MoveUp(Index, _Levels.Count - 1)
                Dim pp1 As Point3d = PointAt(Index)
                AddLevel()
                MoveUp(Index, _Levels.Count - 1)
                Dim pp2 As Point3d = PointAt(Index)

                Dim thislevel As Integer = _NodeLevel(Index)

                For j As Integer = 0 To Node.Parents.Count - 1 Step 1
                    Dim thisc As DirectedNode = Node.Parents(j)
                    Dim thiscidx As Integer = _NodeIndex(thisc.ID)

                    Dim pp3 As Point3d = PointAt(thiscidx)
                    MoveUp(thiscidx, _Levels.Count - 1)
                    Dim pp4 As Point3d = PointAt(thiscidx)

                    _Lines.Add(New Line(pp3, pp4))
                    _Lines.Add(New Line(pp4, pp2))
                    _LinesIndex.Add(thisc.ID)
                    _LinesIndex.Add(thisc.ID)
                Next

                If LastOne Then
                    Dim thispoint As Point3d = PointAt(Index)
                    AddLevel()
                    MoveUp(Index, _NodeLevel(Index) + 1)
                    _Lines.Add(New Line(thispoint, PointAt(Index)))
                    _LinesIndex.Add(Node.ID)
                End If

            End If

        End Sub

        Private Function BuildSplitNode(Node As DirectedNode) As List(Of DirectedNode)
            Dim thisidx As Integer = _NodeIndex(Node.ID)
            Dim thislist As New List(Of DirectedNode)

            AddLevel()
            Dim pp1 As Point3d = PointAt(thisidx)
            MoveUp(thisidx, _Levels.Count - 1)
            Dim pp2 As Point3d = PointAt(thisidx)

            _Lines.Add(New Line(pp1, pp2))
            _LinesIndex.Add(Node.ID)

            For i As Integer = 0 To Node.Children.Count - 1 Step 1
                If Node.Children(i).ID < 0 Then
                    Continue For
                Else
                    Dim thisc As DirectedNode = Node.Children(i)
                    Dim thiscidx As Integer = _NodeIndex(thisc.ID)
                    MoveUp(thiscidx, _Levels.Count - 1)
                    _Lines.Add(New Line(pp2, PointAt(thiscidx)))
                    _LinesIndex.Add(Node.Children(i).ID)
                    thislist.Add(Node.Children(i))
                End If

            Next

            Return thislist
        End Function

        Private Sub MoveUp(PointIdx As Integer, ToLevel As Integer)
            _NodeLevel(PointIdx) = ToLevel
        End Sub



        Private Sub AddLevel()
            _Levels.Add(ConstructLevel)
        End Sub

        Private Function ConstructLevel() As Point3d()
            Dim pts(_NodeCount - 1) As Point3d

            For i As Integer = 0 To _NodeCount - 1 Step 1
                Dim pt As New Point3d(_Radius, 0, _Levels.Count * Height)
                Dim ang As Double = Math.PI * 2 * (i / _NodeCount)
                pt.Transform(Transform.Rotation(ang, Vector3d.ZAxis, Point3d.Origin))
                pts(i) = pt
            Next

            Return pts
        End Function

        Private Function TotalNodeCount(Tree As DirectedNode) As Integer
            Dim cnt As Integer = 0
            Dim cntd As New HashSet(Of Integer)
            CountAllUniqueChildren(Tree, cnt, cntd)

            Dim asl As New List(Of Integer)
            asl.AddRange(cntd.ToList)
            asl.Sort()

            For i As Integer = 0 To asl.Count - 1 Step 1
                _NodeIndex.Add(asl(i), i)
            Next

            Return cnt
        End Function

        Private Sub CountAllUniqueChildren(ByRef tree As DirectedNode, ByRef count As Integer, ByRef counted As HashSet(Of Integer))
            If Not counted.Contains(tree.ID) Then
                counted.Add(tree.ID)
                count += 1

                For i As Integer = 0 To tree.Children.Count - 1 Step 1
                    CountAllUniqueChildren(tree.Children(i), count, counted)
                Next
            End If
        End Sub

#End Region

    End Class

End Namespace
