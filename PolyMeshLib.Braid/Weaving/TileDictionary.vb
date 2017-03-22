Imports PolyMeshLib.Braid.Types
Imports PolyMeshLib.Braid.Types.GridPoint

Namespace Weaving

    Public Module DefaultTiles

        ''' <summary>
        ''' ALWAYS go FROM 0 TO 1 level. not yet done, but works anyway.
        ''' </summary>
        ''' <param name="Hash"></param>
        ''' <returns></returns>
        Public Function GetDefaultStripPlan(Hash As TileHash) As List(Of List(Of GridPoint))
            Dim l As New List(Of List(Of GridPoint))

            Select Case Hash
                Case {EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}
                    'ok
                Case {EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty}
                    l.Add({Create(0, 2, 0), Create(0, 2, 2), Create(0, 4, 2), Create(0, 6, 0)}.ToList)
                    'ok
                Case {EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty}
                    l.Add({Create(0, 2, 0), Create(0, 2, 2), Create(0, 4, 2), Create(0, 6, 2)}.ToList)
                    l.Add({Create(0, 6, 0), Create(1, 4, 2), Create(1, 2, 4), Create(0, 2, 6)}.ToList)
                    'ok
                Case {EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain}
                    'l.Add({Create(0, 2, 0), Create(0, 2, 2), Create(1, 2, 4), Create(0, 2, 6)}.ToList)
                    'l.Add({Create(0, 0, 6), Create(0, 2, 4), Create(1, 4, 2), Create(0, 6, 0)}.ToList)
                    'l.Add({Create(0, 0, 2), Create(1, 2, 2), Create(0, 4, 2), Create(0, 6, 2)}.ToList)

                    l.Add({Create(0, 0, 6), Create(0, 1, 5), Create(0, 1, 2), Create(0, 2, 0)}.ToList)
                    l.Add({Create(0, 0, 2), Create(1, 1, 2), Create(0, 5, 2), Create(0, 6, 2)}.ToList)
                    l.Add({Create(0, 6, 0), Create(0, 6, 1), Create(1, 5, 2), Create(0, 2, 5), Create(0, 2, 6)}.ToList)

                    'ok
                Case {EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty}
                    l.Add({Create(1, 4, 0), Create(0, 2, 2), Create(0, 2, 4), Create(0, 4, 2), Create(0, 4, 0)}.ToList)
                    'ok
                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty}
                    l.Add({Create(1, 4, 0), Create(0, 3, 2), Create(0, 4, 4)}.ToList)
                    l.Add({Create(0, 4, 0), Create(0, 5, 2), Create(1, 4, 4)}.ToList)
                    'ok
                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid}
                    l.Add({Create(0, 0, 4), Create(1, 4, 0)}.ToList)
                    l.Add({Create(1, 0, 4), Create(0, 4, 4)}.ToList)
                    l.Add({Create(0, 4, 0), Create(1, 4, 4)}.ToList)
                    'ok
                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty}
                    l.Add({Create(0, 4, 0), Create(0, 6, 2)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 2, 6)}.ToList)
                    'ok
                Case {EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain}
                    l.Add({Create(1, 4, 0), Create(0, 0, 2)}.ToList)
                    l.Add({Create(0, 4, 0), Create(0, 0, 6)}.ToList)
                    'ok
                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain}
                    l.Add({Create(0, 0, 6), Create(0, 2, 6)}.ToList)
                    l.Add({Create(0, 0, 2), Create(0, 2, 2), Create(1, 4, 0)}.ToList)
                    l.Add({Create(0, 4, 0), Create(0, 6, 2)}.ToList)
                    'ok
                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain}
                    l.Add({Create(0, 4, 0), Create(1, 4, 4)}.ToList)
                    l.Add({Create(0, 0, 2), Create(1, 2, 4), Create(0, 4, 4)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 2, 4), Create(0, 2, 2), Create(1, 4, 0)}.ToList)
                    'ok
                Case {EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}
                'empty list, no strips 

                Case {EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}
                    l.Add({Create(0, 2, 0), Create(0, 2, 2), Create(0, 4, 4), Create(0, 6, 2), Create(0, 6, 0)}.ToList)

                Case {EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Plain, EdgeColor.Empty}
                    l.Add({Create(0, 2, 0), Create(0, 2, 8)}.ToList)
                    l.Add({Create(0, 6, 8), Create(0, 6, 0)}.ToList)

                Case {EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty}
                    l.Add({Create(0, 2, 0), Create(0, 2, 2), Create(0, 6, 2), Create(0, 8, 2)}.ToList)
                    l.Add({Create(0, 6, 0), Create(1, 6, 2), Create(1, 6, 6), Create(0, 8, 6)}.ToList)

                Case {EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty}
                    l.Add({Create(0, 2, 0), Create(0, 2, 8)}.ToList)
                    l.Add({Create(0, 6, 0), Create(0, 8, 2)}.ToList)
                    l.Add({Create(0, 6, 8), Create(0, 8, 6)}.ToList)

                Case {EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain}
                    l.Add({Create(0, 2, 8), Create(1, 2, 6), Create(0, 2, 2), Create(0, 2, 0)}.ToList)
                    l.Add({Create(0, 6, 0), Create(1, 6, 2), Create(0, 6, 6), Create(0, 6, 8)}.ToList)
                    l.Add({Create(0, 8, 6), Create(1, 6, 6), Create(0, 2, 6), Create(0, 0, 6)}.ToList)
                    l.Add({Create(0, 0, 2), Create(1, 2, 2), Create(0, 6, 2), Create(0, 8, 2)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Empty}
                    l.Add({Create(1, 4, 0), Create(0, 2, 2), Create(0, 4, 4), Create(0, 6, 2), Create(0, 4, 0)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty, EdgeColor.Plain}
                    l.Add({Create(0, 0, 2), Create(1, 4, 0)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 4, 0)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain, EdgeColor.Empty}
                    l.Add({Create(0, 2, 8), Create(1, 4, 0)}.ToList)
                    l.Add({Create(0, 6, 8), Create(0, 4, 0)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain, EdgeColor.Plain}
                    l.Add({Create(0, 4, 0), Create(0, 6, 8)}.ToList)
                    l.Add({Create(0, 0, 2), Create(1, 4, 0)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 2, 8)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Braid, EdgeColor.Empty}
                    l.Add({Create(1, 4, 0), Create(0, 2, 4), Create(0, 4, 8)}.ToList)
                    l.Add({Create(0, 4, 0), Create(0, 6, 4), Create(1, 4, 8)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Empty}
                    l.Add({Create(0, 8, 2), Create(0, 4, 0)}.ToList)
                    l.Add({Create(0, 8, 6), Create(1, 4, 0)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty, EdgeColor.Plain}
                    l.Add({Create(0, 0, 2), Create(1, 2, 2), Create(0, 6, 2), Create(0, 8, 2)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 2, 6), Create(0, 2, 2), Create(1, 4, 0)}.ToList)
                    l.Add({Create(0, 8, 6), Create(0, 6, 6), Create(1, 6, 2), Create(0, 4, 0)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Empty}
                    l.Add({Create(1, 4, 0), Create(0, 2, 8)}.ToList)
                    l.Add({Create(0, 8, 2), Create(0, 4, 0)}.ToList)
                    l.Add({Create(0, 8, 6), Create(0, 6, 8)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain, EdgeColor.Plain}
                    l.Add({Create(0, 4, 0), Create(1, 6, 2), Create(0, 6, 6), Create(0, 6, 8)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 2, 2), Create(1, 2, 6), Create(0, 2, 8)}.ToList)
                    l.Add({Create(0, 0, 2), Create(1, 2, 2), Create(0, 6, 2), Create(0, 8, 2)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 2, 6), Create(1, 6, 6), Create(0, 8, 6)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Braid, EdgeColor.Empty}
                    l.Add({Create(0, 4, 0), Create(0, 8, 2)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 4, 8)}.ToList)
                    l.Add({Create(1, 4, 8), Create(0, 8, 6)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Braid, EdgeColor.Plain}
                    l.Add({Create(0, 4, 0), Create(1, 6, 2), Create(0, 6, 6), Create(1, 4, 8)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 2, 2), Create(1, 2, 6), Create(0, 4, 8)}.ToList)
                    l.Add({Create(0, 0, 2), Create(1, 2, 2), Create(0, 6, 2), Create(0, 8, 2)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 2, 6), Create(1, 6, 6), Create(0, 8, 6)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Empty}
                    l.Add({Create(1, 8, 4), Create(0, 4, 0)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 4, 4), Create(0, 8, 4)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty, EdgeColor.Plain}
                    l.Add({Create(0, 4, 0), Create(1, 8, 4)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 0, 2)}.ToList)
                    l.Add({Create(0, 8, 4), Create(0, 0, 6)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Empty}
                    l.Add({Create(0, 4, 0), Create(1, 8, 4)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 2, 8)}.ToList)
                    l.Add({Create(0, 8, 4), Create(0, 6, 8)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain, EdgeColor.Plain}
                    l.Add({Create(0, 4, 0), Create(1, 6, 2), Create(0, 6, 6), Create(0, 6, 8)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 2, 2), Create(1, 2, 6), Create(0, 2, 8)}.ToList)

                    l.Add({Create(0, 0, 2), Create(1, 2, 2), Create(0, 6, 2), Create(1, 8, 4)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 2, 6), Create(1, 6, 6), Create(0, 8, 4)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Empty}
                    l.Add({Create(1, 4, 0), Create(0, 4, 8)}.ToList)
                    l.Add({Create(0, 4, 0), Create(1, 8, 4)}.ToList)
                    l.Add({Create(0, 8, 4), Create(1, 4, 8)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Plain}
                    l.Add({Create(0, 0, 2), Create(0, 2, 2), Create(1, 4, 0)}.ToList)
                    l.Add({Create(0, 0, 6), Create(0, 2, 6), Create(0, 4, 8)}.ToList)

                    l.Add({Create(0, 4, 0), Create(1, 8, 4)}.ToList)
                    l.Add({Create(1, 4, 8), Create(0, 8, 4)}.ToList)

                Case {EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid, EdgeColor.Braid}
                    l.Add({Create(0, 4, 0), Create(1, 8, 4)}.ToList)
                    l.Add({Create(0, 8, 4), Create(1, 4, 8)}.ToList)
                    l.Add({Create(0, 4, 8), Create(1, 0, 4)}.ToList)
                    l.Add({Create(0, 0, 4), Create(1, 4, 0)}.ToList)

               'braid groups 

                Case {EdgeColor.Fifth, EdgeColor.Fourth, EdgeColor.Empty, EdgeColor.Fourth}
                    l.Add({Create(0, 0, 4), Create(0, 4, 0)}.ToList)
                    l.Add({Create(1, 4, 0), Create(0, 8, 4)}.ToList)

                Case {EdgeColor.Sixth, EdgeColor.Fourth, EdgeColor.Empty, EdgeColor.Fourth}
                    l.Add({Create(0, 0, 4), Create(1, 4, 0)}.ToList)
                    l.Add({Create(0, 4, 0), Create(0, 8, 4)}.ToList)

                Case {EdgeColor.Fourth, EdgeColor.Empty, EdgeColor.Fourth, EdgeColor.Empty}
                    l.Add({Create(0, 4, 0), Create(0, 4, 8)}.ToList)

            End Select

            Return l
        End Function

    End Module

End Namespace
