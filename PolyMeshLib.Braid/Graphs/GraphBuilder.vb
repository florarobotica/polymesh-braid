Imports Rhino.Geometry
Imports PolyMeshLib.Core.Graphs

Namespace Graphs

    ''' <summary>
    ''' Creates geometry out of directed graphs. Use with DirectedNode.
    ''' </summary>
    Public Class GraphBuilder

        Private m_Graph As DirectedNode = Nothing
        Private m_NodeIndex As New SortedList(Of Integer, Integer)

        Private m_Levels As New List(Of Point3d())
        Private m_NodeLevel() As Integer

        Private m_Radius As Double = 10
        Private m_Height As Double = 1
        Private m_NodeCount As Integer = 1

        Private m_Lines As New List(Of Line)
        Private m_LinesIndex As New List(Of Integer)

        Sub New()
        End Sub

        Public Property Radius As Double
            Get
                Return m_Radius
            End Get
            Set(value As Double)
                m_Radius = value
            End Set
        End Property

        Public Property Height As Double
            Get
                Return m_Height
            End Get
            Set(value As Double)
                m_Height = value
            End Set
        End Property

        Public Function Build(Graph As DirectedNode) As SortedList(Of Integer, Polyline)
            m_Levels.Clear()
            m_Lines.Clear()
            m_LinesIndex.Clear()
            m_NodeIndex.Clear()

            m_Graph = Graph
            m_NodeCount = TotalNodeCount(m_Graph)
            m_NodeLevel = PolyMeshLib.Core.CommonTools.CreateArray(m_NodeCount, 0)
            AddLevel()

            Dim ls As New List(Of DirectedNode)
            ls.Add(m_Graph)

            While ls.Count > 0
                Dim nt As New List(Of DirectedNode)
                For i As Integer = 0 To ls.Count - 1 Step 1
                    nt.AddRange(BuildSplitNode(ls(i)))
                Next
                ls = nt
            End While
            For i As Integer = m_NodeIndex.Keys.Count - 1 To 0 Step -1
                If m_NodeIndex.Keys(i) < 0 Then
                    BuildMergeNode(m_Graph.FindNode(m_NodeIndex.Keys(i)), i = 0)
                End If
            Next

            Dim srt As New SortedList(Of Integer, Polyline)

            Dim presrt As New SortedList(Of Integer, List(Of Line))

            For i As Integer = 0 To m_LinesIndex.Count - 1 Step 1
                presrt(m_LinesIndex(i)) = New List(Of Line)
            Next

            For i As Integer = 0 To m_Lines.Count - 1 Step 1
                presrt(m_LinesIndex(i)).Add(m_Lines(i))
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

        Private ReadOnly Property LevelCount() As Integer
            Get
                Return m_Levels.Count
            End Get
        End Property

        Private ReadOnly Property Ids As SortedList(Of Integer, Integer)
            Get
                Return m_NodeIndex
            End Get
        End Property

        Private Sub BuildMergeNode(Node As DirectedNode, LastOne As Boolean)
            Dim Index As Integer = m_NodeIndex(Node.ID)

            If Node IsNot Nothing Then

                MoveUp(Index, m_Levels.Count - 1)
                Dim pp1 As Point3d = PointAt(Index)
                AddLevel()
                MoveUp(Index, m_Levels.Count - 1)
                Dim pp2 As Point3d = PointAt(Index)

                Dim thislevel As Integer = m_NodeLevel(Index)

                For j As Integer = 0 To Node.Parents.Count - 1 Step 1
                    Dim thisc As DirectedNode = Node.Parents(j)
                    Dim thiscidx As Integer = m_NodeIndex(thisc.ID)

                    Dim pp3 As Point3d = PointAt(thiscidx)
                    MoveUp(thiscidx, m_Levels.Count - 1)
                    Dim pp4 As Point3d = PointAt(thiscidx)

                    m_Lines.Add(New Line(pp3, pp4))
                    m_Lines.Add(New Line(pp4, pp2))
                    m_LinesIndex.Add(thisc.ID)
                    m_LinesIndex.Add(thisc.ID)
                Next

                If LastOne Then
                    Dim thispoint As Point3d = PointAt(Index)
                    AddLevel()
                    MoveUp(Index, m_NodeLevel(Index) + 1)
                    m_Lines.Add(New Line(thispoint, PointAt(Index)))
                    m_LinesIndex.Add(Node.ID)
                End If

            End If

        End Sub

        Private Function BuildSplitNode(Node As DirectedNode) As List(Of DirectedNode)
            Dim thisidx As Integer = m_NodeIndex(Node.ID)
            Dim thislist As New List(Of DirectedNode)

            AddLevel()
            Dim pp1 As Point3d = PointAt(thisidx)
            MoveUp(thisidx, m_Levels.Count - 1)
            Dim pp2 As Point3d = PointAt(thisidx)

            m_Lines.Add(New Line(pp1, pp2))
            m_LinesIndex.Add(Node.ID)

            For i As Integer = 0 To Node.Children.Count - 1 Step 1
                If Node.Children(i).ID < 0 Then
                    Continue For
                Else
                    Dim thisc As DirectedNode = Node.Children(i)
                    Dim thiscidx As Integer = m_NodeIndex(thisc.ID)
                    MoveUp(thiscidx, m_Levels.Count - 1)
                    m_Lines.Add(New Line(pp2, PointAt(thiscidx)))
                    m_LinesIndex.Add(Node.Children(i).ID)
                    thislist.Add(Node.Children(i))
                End If

            Next

            Return thislist
        End Function

        Private Sub MoveUp(PointIdx As Integer, ToLevel As Integer)
            m_NodeLevel(PointIdx) = ToLevel
        End Sub

        Private Property Level(index As Integer) As Point3d()
            Get
                Return m_Levels(index)
            End Get
            Set(value As Point3d())
                m_Levels(index) = value
            End Set
        End Property

        Private Property PointAt(level As Integer, index As Integer) As Point3d
            Get
                Return m_Levels(level)(index)
            End Get
            Set(value As Point3d)
                m_Levels(level)(index) = value
            End Set
        End Property

        Private Property PointAt(index As Integer) As Point3d
            Get
                Return m_Levels(m_NodeLevel(index))(index)
            End Get
            Set(value As Point3d)
                m_Levels(m_NodeLevel(index))(index) = value
            End Set
        End Property

        Private Sub AddLevel()
            m_Levels.Add(ConstructLevel)
        End Sub

        Private Function ConstructLevel() As Point3d()
            Dim pts(m_NodeCount - 1) As Point3d

            For i As Integer = 0 To m_NodeCount - 1 Step 1
                Dim pt As New Point3d(m_Radius, 0, m_Levels.Count * Height)
                Dim ang As Double = Math.PI * 2 * (i / m_NodeCount)
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
                m_NodeIndex.Add(asl(i), i)
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

    End Class

End Namespace
