Imports PolyMeshLib.Core
Imports PolyMeshLib.Core.CommonTools.Geometrical
Imports PolyMeshLib.Core.PointProcessing

Public Module BraidUtils

    Public Function HullTruncatedCircles(Points As IEnumerable(Of Point3d)) As List(Of Circle)
        Dim cirs As New List(Of Circle)

        Dim sphaver As Point3d = AveragePoints(Points)
        Dim mindist As Double = Double.MaxValue

        For i As Integer = 0 To Points.Count - 1 Step 1
            mindist = Math.Min(mindist, sphaver.DistanceTo(Points(i)))
        Next

        Dim npoints As New List(Of Point3d)

        For i As Integer = 0 To Points.Count - 1 Step 1
            Dim thisvec As Vector3d = Points(i) - sphaver
            thisvec.Normalize()
            npoints.Add(sphaver + thisvec * mindist)
        Next

        Points = npoints

        Dim qh As New QuickHull(Points)
        If Not qh.Solve(PolyMeshLib.Core.GeometrySettings.Tolerance) Then Return cirs
        Dim conn() As List(Of Integer) = qh.Hull.Vertices.GetAdjacencyMatrix

        Dim ints As New List(Of Integer)
        For i As Integer = 0 To Points.Count - 1 Step 1
            ints.Add(ClosestPoint(qh.Hull.Vertices, Points(i)))
        Next

        Dim center As Point3d = AveragePoints(Points)

        For i As Integer = 0 To Points.Count - 1 Step 1
            Dim thisconn As List(Of Integer) = conn(i)

            Dim thish As New List(Of Point3d)

            For j As Integer = 0 To thisconn.Count - 1 Step 1
                Dim thisvert As Point3d = (qh.Hull.Vertices(thisconn(j)))
                thisvert += (Points(i) - thisvert) / 2
                thish.Add(thisvert)
            Next

            Dim fplane As New Plane(center, center - Points(i))

            Dim farfar As Integer = -1
            Dim fardist As Double = Double.MaxValue

            For j As Integer = 0 To thish.Count - 1 Step 1
                Dim thisdist As Double = fplane.DistanceTo(thish(j))
                If thisdist < fardist Then
                    fardist = thisdist
                    farfar = j
                End If
            Next

            fplane.Origin = thish(farfar)
            Dim almostthere As New List(Of Point3d)

            For j As Integer = 0 To thish.Count - 1 Step 1
                Dim thisline As New Line(Points(i), thish(j))
                Dim param As Double
                Dim plpar As Point3d
                Intersections.LinePlane(thisline, fplane, param, plpar)
                almostthere.Add(thisline.PointAt(param))
            Next

            Dim ch2 As New QuickHull2d(almostthere, fplane) : ch2.Solve(PolyMeshLib.Core.GeometrySettings.Tolerance)
            Dim hull As Polyline = ch2.HullPolyline

            Dim aver As Point3d = AveragePoints(almostthere)
            Dim cpp As Point3d = hull.ClosestPoint(aver)
            cirs.Add(New Circle(fplane, aver, cpp.DistanceTo(aver)))
        Next

        Return cirs
    End Function

    Public Function TangentPlanes(Tetrahedron As List(Of Point3d), S As Sphere) As List(Of Plane)
        Dim npl As New List(Of Plane)
        For i As Integer = 0 To Tetrahedron.Count - 1 Step 1
            Dim dist As Double = Tetrahedron(i).DistanceTo(S.Center)
            Dim vec As Vector3d = S.Center - Tetrahedron(i)
            vec.Normalize()
            vec *= dist - S.Radius
            npl.Add(New Plane(Tetrahedron(i) + vec, vec))
        Next
        Return npl
    End Function

    Public Function TetrahedronTruncatedCircles(Tetrahedron As List(Of Point3d)) As List(Of Circle)
        Dim cirs As New List(Of Circle)

        For i As Integer = 0 To Tetrahedron.Count - 1 Step 1
            Dim thisorder As New List(Of Point3d)({Tetrahedron(i)})
            thisorder.Add(New Point3d(Tetrahedron((i + 1 + 4) Mod 4)))
            thisorder.Add(New Point3d(Tetrahedron((i + 2 + 4) Mod 4)))
            thisorder.Add(New Point3d(Tetrahedron((i + 3 + 4) Mod 4)))

            Dim min As Double = Double.MaxValue
            For j As Integer = 1 To thisorder.Count - 1 Step 1
                min = Math.Min(min, thisorder(0).DistanceTo(thisorder(j)))
            Next

            For j As Integer = 1 To thisorder.Count - 1 Step 1
                Dim tp As Point3d = thisorder(j)
                Dim vec As Vector3d = tp - thisorder(0)
                vec.Normalize()
                vec *= min
                thisorder(j) = thisorder(0) + vec
            Next

            Dim thispl As New Plane(thisorder(1), thisorder(2), thisorder(3))
            If thispl.DistanceTo(thisorder(0)) < 0 Then thisorder = New List(Of Point3d)({thisorder(0), thisorder(3), thisorder(2), thisorder(1)})

            Dim cir As Circle = InscribedCircle(New List(Of Point3d)({thisorder(1), thisorder(2), thisorder(3)}))
            cir.Transform(TMatrix.Scale(thisorder(0), 0.5))
            cirs.Add(cir)
        Next
        Return cirs
    End Function

    Public Function TangentPlanesCircles(Tetrahedron As List(Of Point3d), Planes As List(Of Plane)) As List(Of Circle)
        Dim cirs As New List(Of Circle)
        For i As Integer = 0 To Tetrahedron.Count - 1 Step 1

            Dim thisorder As New List(Of Point3d)({Tetrahedron(i)})
            thisorder.Add(Tetrahedron((i + 1 + 4) Mod 4))
            thisorder.Add(Tetrahedron((i + 2 + 4) Mod 4))
            thisorder.Add(Tetrahedron((i + 3 + 4) Mod 4))

            Dim pl As Plane = Planes(i)

            Dim ints As New List(Of Point3d)

            For j As Integer = 1 To thisorder.Count - 1 Step 1
                Dim thisl As New Line(thisorder(j), thisorder(0))
                Dim parpl As Point3d
                Dim param As Double
                Intersections.LinePlane(thisl, pl, param, parpl)
                ints.Add(thisl.PointAt(param))
            Next

            If Polyline.CreateFromPoints(ints).IsClockwise(Planes(i)) Then ints.Reverse()

            cirs.Add(InscribedCircle(ints))
        Next
        Return cirs
    End Function

    Function CutPolyline(P As Polyline, pl As Plane) As Polyline
        Dim nseg As New List(Of Line)
        nseg.Clear()

        For i As Integer = 0 To P.SegmentCount - 1 Step 1
            Dim l As Line = P.SegmentAt(i)
            Dim param As Double = -1
            Dim plpar As Point3d = Point3d.Zero

            If Intersections.LinePlane(l, pl, param, plpar) Then
                If param > 0 And param < 1 Then
                    Dim l1 As New Line(l.From, l.PointAt(param))
                    Dim l2 As New Line(l.PointAt(param), l.To)

                    Dim mid1 As Point3d = l1.PointAt(0.5)
                    Dim mid2 As Point3d = l2.PointAt(0.5)

                    If pl.DistanceTo(mid1) < pl.DistanceTo(mid2) Then
                        nseg.Add(l1)
                    Else
                        nseg.Add(l2)
                    End If
                Else
                    Dim midpt As Point3d = (l.From + l.To) / 2

                    If pl.DistanceTo(midpt) < 0 Then
                        nseg.Add(l)

                    End If
                End If
            Else
                Dim midpt As Point3d = l.PointAt(0.5)

                If pl.DistanceTo(midpt) < 0 Then
                    nseg.Add(l)
                End If
            End If
        Next

        Dim outpl As New Polyline

        outpl.Add(nseg(0).From)
        outpl.Add(nseg(0).To)

        For i As Integer = 1 To nseg.Count - 1 Step 1
            If outpl(outpl.Count - 1).DistanceTo(nseg(i).From) > 0.001 Then outpl.Add(nseg(i).From)
            outpl.Add(nseg(i).To)
        Next

        outpl.Add(outpl(0))

        Return outpl
    End Function

    ''' <summary>
    ''' Elbow points = Arm1, Elbow point, Arm2 
    ''' </summary>
    ''' <param name="Elbow"></param>
    ''' <returns></returns>
    Public Function ElbowTangentCircles(Elbow As List(Of Point3d)) As List(Of Circle)
        Dim nl As New List(Of Circle)

        Dim av As Point3d = AveragePoints(Elbow)

        Dim v1 As Vector3d = Elbow(0) - av
        Dim v2 As Vector3d = Elbow(2) - av

        Dim ang As Double = Vector3d.Angle(v1, v2)
        ang *= 0.5

        Dim dist As Double = Math.Min(v1.Length, v2.Length)

        Dim r As Double = Math.Tan(ang) * dist * 0.5

        nl.Add(New Circle(New Plane(Elbow(0), -v1), r))
        nl.Add(New Circle(New Plane(Elbow(2), -v2), r))

        Return nl
    End Function

    ''' <summary>
    ''' Constructs 3 non-intersecting circles, perpendicular to the plane of the triangle, tangent to the InCircle.
    ''' </summary>
    ''' <param name="Triangle"></param>
    ''' <param name="InCircle"></param>
    ''' <returns></returns>
    Public Function TriangleTangentCircles(Triangle As List(Of Point3d), InCircle As Circle) As List(Of Circle)
        Dim Circles As New List(Of Circle)

        For i As Integer = 0 To Triangle.Count - 1 Step 1
            Dim alpha As Double = Vector3d.Angle(Triangle((i + 1 + 3) Mod 3) - Triangle(i), Triangle((i - 1 + 3) Mod 3) - Triangle(i)) / 2
            Dim rad As Double = InCircle.Radius
            Dim dist As Double = InCircle.Center.DistanceTo(Triangle(i))

            Dim norm As Vector3d = InCircle.Center - Triangle(i)
            norm.Normalize()
            norm *= (dist - InCircle.Radius)
            Dim thiscenter As Point3d = Triangle(i) + norm

            Dim r As Double = Math.Tan(alpha) * (dist - rad)

            Circles.Add(New Circle(New Plane(thiscenter, norm), r))
        Next

        Return Circles
    End Function

    ''' <summary>
    ''' Base sphere radius = shortest arm * 0.25
    ''' </summary>
    ''' <param name="Points"></param>
    ''' <returns></returns>
    Public Function MultipleCircles(Points As IEnumerable(Of Point3d)) As List(Of Circle)
        Dim av As Point3d = AveragePoints(Points)

        Dim angles As New List(Of Double)
        Dim shortest As Double = Double.MaxValue

        For i As Integer = 0 To Points.Count - 1 Step 1
            shortest = Math.Min(Points(i).DistanceTo(av), shortest)

            Dim angle As Double = Double.MaxValue

            For j As Integer = 0 To Points.Count - 1 Step 1
                If i = j Then Continue For
                angle = Math.Min(angle, Vector3d.Angle(Points(j) - av, Points(i) - av))
            Next

            angles.Add(angle / 2)
        Next

        Dim nc As New List(Of Circle)

        For i As Integer = 0 To Points.Count - 1 Step 1
            Dim thisv As Vector3d = Points(i) - av
            thisv.Normalize()
            thisv *= shortest

            Dim thisc As Point3d = av + thisv
            Dim cir As New Circle(New Plane(thisc, -thisv), Math.Tan(angles(i)) * shortest * 0.5)
            nc.Add(cir)
        Next

        Return nc
    End Function

    Public Function DivideCircle(C As Circle, Count As Integer, Optional DuplicateFirst As Boolean = False) As Point3d()

        If DuplicateFirst Then
            Dim pts As New List(Of Point3d)
            For i As Integer = 0 To Count - 1 Step 1
                pts.Add(C.PointAt((i / Count) * Math.PI * 2))
            Next
            pts.Add(pts(0))
            Return pts.ToArray
        Else
            Dim pts(Count - 1) As Point3d
            For i As Integer = 0 To Count - 1 Step 1
                pts(i) = C.PointAt((i / Count) * Math.PI * 2)
            Next
            Return pts
        End If

    End Function

    Public Function OrientCircles(Circles As List(Of Circle), MasterIndex As Integer) As List(Of Circle)
        Dim nl As New List(Of Circle)

        If Circles.Count = 2 Then
            nl.AddRange(Circles)
            For i As Integer = 0 To 1 Step 1
                Dim c1 As Circle = nl(i)
                Dim c2 As Circle = nl((i + 1 + 2) Mod 2)
                Dim par As Double = -1
                par = c1.ClosestParameter(c2.Center)
                Dim tr As TMatrix = TMatrix.Rotate(c1.Center, c1.Normal, par + Math.PI)
                c1.Transform(tr)
                nl(i) = c1
            Next
        ElseIf Circles.Count > 2 Then
            For i As Integer = 0 To Circles.Count - 1 Step 1
                If i = MasterIndex Then nl.Add(Circles(i)) : Continue For
                Dim c As Circle = Circles(i)
                Dim mas As Circle = Circles(MasterIndex)
                Dim par As Double = -1
                par = c.ClosestParameter(mas.Center)
                Dim tr As TMatrix = TMatrix.Rotate(c.Center, c.Normal, par + Math.PI)
                c.Transform(tr)
                nl.Add(c)
            Next
        ElseIf Circles.Count = 1 Then
            nl.AddRange(Circles)
        End If

        Return nl
    End Function

    Public Function DivideMasterCircle(Circles As List(Of Circle), MasterIndex As Integer, ByRef Indices() As Integer) As Double()
        Dim ids(Circles.Count - 2) As Integer
        Dim par(Circles.Count - 2) As Double
        Dim cnt As Integer = 0

        For i As Integer = 0 To Circles.Count - 1 Step 1
            If i = MasterIndex Then Continue For
            ids(cnt) = i

            Dim param As Double = -1
            param = Circles(MasterIndex).ClosestParameter(Circles(i).Center)
            par(cnt) = param

            cnt += 1
        Next

        Array.Sort(par, ids)

        Dim pp(par.Length - 1) As Double

        For i As Integer = 0 To par.Length - 1 Step 1
            Dim thisp As Double = par(i)
            Dim nextp As Double = par((i + 1 + par.Length) Mod par.Length)

            If i = par.Length - 1 Then
                pp(i) = thisp + (((2 * Math.PI) - (thisp - nextp)) * 0.5)
            Else
                pp(i) = (nextp + thisp) * 0.5
            End If

        Next

        Indices = ids
        Return pp
    End Function
End Module
