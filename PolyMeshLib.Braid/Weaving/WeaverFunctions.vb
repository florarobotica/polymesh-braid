Imports PolyMeshLib.Core.Types
Imports PolyMeshLib.Braid.Types
Imports PolyMeshLib.Braid.Types.GridPoint

Namespace Weaving


    ''' <summary>
    ''' A set of functions for the Weaver.
    ''' </summary>
    Public Module WeaverFunctions

        Public Function EvaluateTriangle(P1 As Point3d, P2 As Point3d, P3 As Point3d, U As Double, V As Double) As Point3d
            Dim ep01 As Point3d = ((1 - U) * P1) + (U * P2)
            Dim ep31 As Point3d = ((1 - V) * P1) + (V * P3)
            Dim eval As Point3d = P1 + (ep01 - P1) + (ep31 - P1)
            Return eval
        End Function

        Public Function EvaluateQuad(P1 As Point3d, P2 As Point3d, P3 As Point3d, P4 As Point3d, U As Double, V As Double) As Point3d
            Dim ep01 As Point3d = ((1 - U) * P1) + (U * P2)
            Dim ep32 As Point3d = ((1 - U) * P4) + (U * P3)
            Dim eval As Point3d = ((1 - V) * ep01) + (V * ep32)
            Return eval
        End Function

        Public Function ConstructTriangleGrids(Clockwise As Boolean, Corners() As Point3d) As Point3d(,,)

            Dim pointgrids(1, 8, 8) As Point3d

            For level As Integer = 0 To 1 Step 1
                Dim creationlevel As Integer = If(Clockwise, If(level = 0, 0, 1), If(level = 0, 1, 0))

                If creationlevel = 0 Then
                    For i As Integer = 0 To 8 Step 1
                        For j As Integer = 0 To 8 Step 1
                            pointgrids(level, j, i) = EvaluateTriangle(Corners(0), Corners(1), Corners(3), j * 0.125, i * 0.125)
                        Next
                    Next
                Else
                    For i As Integer = 0 To 8 Step 1
                        For j As Integer = 0 To 8 Step 1
                            pointgrids(level, j, i) = EvaluateTriangle(Corners(4), Corners(5), Corners(7), j * 0.125, i * 0.125)
                        Next
                    Next
                End If
            Next

            Return pointgrids
        End Function

        Public Function ConstructQuadGrids(Clockwise As Boolean, Corners As Point3d()) As Point3d(,,)

            Dim pointgrids(1, 8, 8) As Point3d

            For level As Integer = 0 To 1 Step 1
                Dim creationlevel As Integer = If(Clockwise, If(level = 0, 0, 1), If(level = 0, 1, 0))

                If creationlevel = 0 Then
                    For i As Integer = 0 To 8 Step 1
                        For j As Integer = 0 To 8 Step 1
                            pointgrids(level, j, i) = EvaluateQuad(Corners(0), Corners(1), Corners(2), Corners(3), j * 0.125, i * 0.125)
                        Next
                    Next
                Else
                    For i As Integer = 0 To 8 Step 1
                        For j As Integer = 0 To 8 Step 1
                            pointgrids(level, j, i) = EvaluateQuad(Corners(4), Corners(5), Corners(6), Corners(7), j * 0.125, i * 0.125)
                        Next
                    Next
                End If
            Next

            Return pointgrids
        End Function

        Friend Function LineFromMiddle(MiddlePt As Point3d, Direction As Vector3d, Length As Double) As Line
            Direction.Unitize()
            Return New Line(MiddlePt - (Direction * Length * 0.5), Direction, Length)
        End Function

        Friend Function GetEdgeNeighbors(Hash As TileHash, Pt As GridPoint) As List(Of GridPoint)
            Dim nl As New List(Of GridPoint)

            If Hash.IsQuad Then
                If Pt.U = 0 Then
                    nl.Add(Create(Pt.Level, Pt.U, Pt.V + 1))
                    nl.Add(Create(Pt.Level, Pt.U, Pt.V - 1))
                ElseIf Pt.U = 8 Then
                    nl.Add(Create(Pt.Level, Pt.U, Pt.V - 1))
                    nl.Add(Create(Pt.Level, Pt.U, Pt.V + 1))
                ElseIf Pt.V = 0 Then
                    nl.Add(Create(Pt.Level, Pt.U - 1, Pt.V))
                    nl.Add(Create(Pt.Level, Pt.U + 1, Pt.V))
                ElseIf Pt.V = 8 Then
                    nl.Add(Create(Pt.Level, Pt.U + 1, Pt.V))
                    nl.Add(Create(Pt.Level, Pt.U - 1, Pt.V))
                End If
            Else
                If Pt.U = 0 Then
                    nl.Add(Create(Pt.Level, Pt.U, Pt.V + 1))
                    nl.Add(Create(Pt.Level, Pt.U, Pt.V - 1))
                ElseIf Pt.V = 0 Then
                    nl.Add(Create(Pt.Level, Pt.U - 1, Pt.V))
                    nl.Add(Create(Pt.Level, Pt.U + 1, Pt.V))
                ElseIf (Pt.U = 2 And Pt.V = 6) Or (Pt.U = 4 And Pt.V = 4) Or (Pt.U = 6 And Pt.V = 2) Then
                    nl.Add(Create(Pt.Level, Pt.U + 1, Pt.V - 1))
                    nl.Add(Create(Pt.Level, Pt.U - 1, Pt.V + 1))
                End If
            End If

            Return nl
        End Function

        Friend Function LinesToSimpleMesh(Lines As IEnumerable(Of Line)) As PolyMesh
            Dim pm As New PolyMesh
            pm.Vertices.Add(Lines(0).From)
            pm.Vertices.Add(Lines(0).To)

            For i As Integer = 1 To Lines.Count - 1 Step 1
                pm.Vertices.Add(Lines(i).From)
                pm.Vertices.Add(Lines(i).To)

                Dim thisc As Integer = pm.Vertices.Count

                pm.Faces.Add(New PolyFace({thisc - 1, thisc - 2, thisc - 4, thisc - 3}))

            Next

            Return pm
        End Function

        Friend Function LinesToMesh(Lines As IEnumerable(Of Line)) As PolyMesh
            Dim pm As New PolyMesh
            pm.Vertices.Add(Lines(0).From)
            pm.Vertices.Add(Lines(0).To)

            For i As Integer = 1 To Lines.Count - 1 Step 1
                pm.Vertices.Add(Lines(i).From)
                pm.Vertices.Add(Lines(i).To)

                Dim thisc As Integer = pm.Vertices.Count

                pm.Faces.Add(New PolyFace({thisc - 2, thisc - 4, thisc - 3}))
                pm.Faces.Add(New PolyFace({thisc - 3, thisc - 1, thisc - 2}))
            Next

            Return pm
        End Function

        Friend Function LinesToMesh(Lines As IEnumerable(Of Line), TrianglePerLength As Double) As PolyMesh
            Dim pm As New PolyMesh
            pm.Vertices.Add(Lines(0).From)
            pm.Vertices.Add(Lines(0).To)

            For i As Integer = 1 To Lines.Count - 1 Step 1
                Dim value As Double = Lines(i - 1).PointAt(0.5).DistanceTo(Lines(i).PointAt(0.5))
                value /= TrianglePerLength
                value = Math.Ceiling(value)

                For j As Integer = 1 To value Step 1
                    Dim thisparam As Double = j / value

                    pm.Vertices.Add(EvaluateQuad(Lines(i - 1).From, Lines(i - 1).To, Lines(i).To, Lines(i).From, 0, thisparam))
                    pm.Vertices.Add(EvaluateQuad(Lines(i - 1).From, Lines(i - 1).To, Lines(i).To, Lines(i).From, 1, thisparam))

                    Dim thisc As Integer = pm.Vertices.Count
                    '2, 0, 1
                    '1, 3, 2 

                    pm.Faces.Add(New PolyFace({thisc - 2, thisc - 4, thisc - 3}))
                    pm.Faces.Add(New PolyFace({thisc - 3, thisc - 1, thisc - 2}))
                Next

            Next

            Return pm
        End Function

        Friend ReadOnly Property GetGridPoint(Grid As Point3d(,,), Point As GridPoint) As Point3d
            Get
                Return Grid(Point.Level, Point.U, Point.V)
            End Get
        End Property

        Friend Function ConstructMidLines(Grid As Point3d(,,), Strip As List(Of GridPoint)) As Line()

            Dim mids(Strip.Count - 3) As Line

            For i As Integer = 1 To Strip.Count - 2 Step 1
                Dim prev As GridPoint = Strip(i - 1)
                Dim this As GridPoint = Strip(i)
                Dim post As GridPoint = Strip(i + 1)

                Dim du As Integer = post.U - prev.U
                Dim dv As Integer = post.V - prev.V

                Dim p1 As GridPoint = Create(this.Level, this.U, this.V)
                Dim p2 As GridPoint = Create(this.Level, this.U, this.V)

                'straight 
                If (du = 0) Then
                    If dv > 0 Then
                        p1.U -= 1 : p2.U += 1
                    Else
                        p1.U += 1 : p2.U -= 1
                    End If
                ElseIf (dv = 0) Then
                    If du > 0 Then
                        p1.V += 1 : p2.V -= 1
                    Else
                        p1.V -= 1 : p2.V += 1
                    End If
                ElseIf (du > 0 And dv > 0) Then
                    p1.U -= 1 : p2.U += 1
                    p1.V += 1 : p2.V -= 1
                ElseIf (du < 0 And dv < 0) Then
                    p1.U += 1 : p2.U -= 1
                    p1.V -= 1 : p2.V += 1
                ElseIf (du > 0 And dv < 0) Then
                    p1.U += 1 : p2.U -= 1
                    p1.V += 1 : p2.V -= 1
                ElseIf (du < 0 And dv > 0) Then
                    p1.U -= 1 : p2.U += 1
                    p1.V -= 1 : p2.V += 1
                End If

                mids(i - 1) = GridBasedMidline(Grid, this, p1, p2)

            Next

            Return mids
        End Function

        Friend Function ConstructMidLinesVariable(Grid As Point3d(,,), Strip As List(Of GridPoint), Width As Double) As Line()

            Dim mids(Strip.Count - 3) As Line

            For i As Integer = 1 To Strip.Count - 2 Step 1
                Dim prev As GridPoint = Strip(i - 1)
                Dim this As GridPoint = Strip(i)
                Dim post As GridPoint = Strip(i + 1)

                Dim du As Integer = post.U - prev.U
                Dim dv As Integer = post.V - prev.V

                Dim p1 As GridPoint = Create(this.Level, this.U, this.V)
                Dim p2 As GridPoint = Create(this.Level, this.U, this.V)

                'straight 
                If (du = 0) Then
                    If dv > 0 Then
                        p1.U -= 1 : p2.U += 1
                    Else
                        p1.U += 1 : p2.U -= 1
                    End If
                ElseIf (dv = 0) Then
                    If du > 0 Then
                        p1.V += 1 : p2.V -= 1
                    Else
                        p1.V -= 1 : p2.V += 1
                    End If
                ElseIf (du > 0 And dv > 0) Then
                    p1.U -= 1 : p2.U += 1
                    p1.V += 1 : p2.V -= 1
                ElseIf (du < 0 And dv < 0) Then
                    p1.U += 1 : p2.U -= 1
                    p1.V -= 1 : p2.V += 1
                ElseIf (du > 0 And dv < 0) Then
                    p1.U += 1 : p2.U -= 1
                    p1.V += 1 : p2.V -= 1
                ElseIf (du < 0 And dv > 0) Then
                    p1.U -= 1 : p2.U += 1
                    p1.V -= 1 : p2.V += 1
                End If

                mids(i - 1) = GridBasedMidlineVariable(Grid, this, p1, p2, Width)

            Next

            Return mids
        End Function

        Friend Function NonEdgeEndLines(Grid As Point3d(,,), Strip As List(Of GridPoint), Point As Integer) As Line
            Dim this As GridPoint = Strip(Point)
            Dim prev As GridPoint = Strip((Point - 1 + Strip.Count) Mod Strip.Count)
            Dim post As GridPoint = Strip((Point + 1 + Strip.Count) Mod Strip.Count)

            Dim p1 As GridPoint = GridPoint.Create(this.Level, this.U, this.V)
            Dim p2 As GridPoint = GridPoint.Create(this.Level, this.U, this.V)
            Dim du As Integer
            Dim dv As Integer

            If Strip(0) = Strip(Strip.Count - 1) Then 'is a closed curve

                If Point = 0 Then
                    prev = Strip((Point - 2 + Strip.Count) Mod Strip.Count)
                ElseIf Point = Strip.Count - 1 Then
                    post = Strip((Point + 2 + Strip.Count) Mod Strip.Count)
                End If

                du = post.U - prev.U
                dv = post.V - prev.V
            ElseIf Point = 0 Then 'its a start point
                du = post.U - this.U
                dv = post.V - this.V
            ElseIf Point = Strip.Count - 1 Then 'its an end point
                du = this.U - prev.U
                dv = this.V - prev.V
            End If

            If (du = 0) Then
                If dv > 0 Then
                    p1.U -= 1 : p2.U += 1
                Else
                    p1.U += 1 : p2.U -= 1
                End If
            ElseIf (dv = 0) Then
                If du > 0 Then
                    p1.V += 1 : p2.V -= 1
                Else
                    p1.V -= 1 : p2.V += 1
                End If
            ElseIf (du > 0 And dv > 0) Then
                p1.U -= 1 : p2.U += 1
                p1.V += 1 : p2.V -= 1
            ElseIf (du < 0 And dv < 0) Then
                p1.U += 1 : p2.U -= 1
                p1.V -= 1 : p2.V += 1
            ElseIf (du > 0 And dv < 0) Then
                p1.U += 1 : p2.U -= 1
                p1.V += 1 : p2.V -= 1
            ElseIf (du < 0 And dv > 0) Then
                p1.U -= 1 : p2.U += 1
                p1.V -= 1 : p2.V += 1
            End If

            Return GridBasedMidline(Grid, this, p1, p2)
        End Function

        Friend Function GridBasedMidline(Grid As Point3d(,,), ThisP As GridPoint, P1 As GridPoint, P2 As GridPoint) As Line
            Dim v1 As Vector3d = (GetGridPoint(Grid, P1) - GetGridPoint(Grid, ThisP)) * 0.25
            Dim v2 As Vector3d = (GetGridPoint(Grid, P2) - GetGridPoint(Grid, ThisP)) * 0.25
            Return New Line(GetGridPoint(Grid, ThisP) + v1, GetGridPoint(Grid, ThisP) + v2)
        End Function

        Friend Function GridBasedMidlineVariable(Grid As Point3d(,,), ThisP As GridPoint, P1 As GridPoint, P2 As GridPoint, Width As Double) As Line
            Dim v1 As Vector3d = (GetGridPoint(Grid, P1) - GetGridPoint(Grid, ThisP)) * 0.25 * Width
            Dim v2 As Vector3d = (GetGridPoint(Grid, P2) - GetGridPoint(Grid, ThisP)) * 0.25 * Width
            Return New Line(GetGridPoint(Grid, ThisP) + v1, GetGridPoint(Grid, ThisP) + v2)
        End Function

    End Module
End Namespace
