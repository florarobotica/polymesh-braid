Namespace Types

    Public Enum EdgeColor As Integer
        None = -1
        Empty = 0
        Plain = 1
        Braid = 2
        Fourth = 3
        Fifth = 4
        Sixth = 5
        Seventh = 6
        Eighth = 7
        Ninth = 8
        Tenth = 9
    End Enum

    Public Structure TileHash
        Implements IComparable(Of TileHash)

        Private _Value0 As EdgeColor
        Private _Value1 As EdgeColor
        Private _Value2 As EdgeColor
        Private _Value3 As EdgeColor
        Private _IsQuad As Boolean

        Public ReadOnly Property Value0 As EdgeColor
            Get
                Return _Value0
            End Get
        End Property

        Public ReadOnly Property Value1 As EdgeColor
            Get
                Return _Value1
            End Get
        End Property

        Public ReadOnly Property Value2 As EdgeColor
            Get
                Return _Value2
            End Get
        End Property

        Public ReadOnly Property Value3 As EdgeColor
            Get
                Return _Value3
            End Get
        End Property

        Public ReadOnly Property IsQuad As Boolean
            Get
                Return _IsQuad
            End Get
        End Property

        Sub New(Value0 As EdgeColor, Value1 As EdgeColor, Value2 As EdgeColor, Value3 As EdgeColor)
            _Value0 = Value0
            _Value1 = Value1
            _Value2 = Value2
            _Value3 = Value3
            _IsQuad = True
            ComputeHash()
        End Sub

        Sub New(Value0 As EdgeColor, Value1 As EdgeColor, Value2 As EdgeColor)
            _Value0 = Value0
            _Value1 = Value1
            _Value2 = Value2
            _Value3 = 0
            _IsQuad = False
            ComputeHash()
        End Sub

        Sub New(Values As IEnumerable(Of EdgeColor))
            _Value0 = Values(0)
            _Value1 = Values(1)
            _Value2 = Values(2)

            If Values.Count = 3 Then
                _IsQuad = False
            Else
                _Value3 = Values(3)
                _IsQuad = True
            End If

            ComputeHash()
        End Sub

        Private Sub New(Values As IEnumerable(Of EdgeColor), Compute As Boolean)
            _Value0 = Values(0)
            _Value1 = Values(1)
            _Value2 = Values(2)

            If Values.Count = 3 Then
                _IsQuad = False
            Else
                _Value3 = Values(3)
                _IsQuad = True
            End If

            If Compute Then ComputeHash()
        End Sub

        Public Overrides Function ToString() As String
            If Me = InvalidHash() Then
                Return ("Invalid TileHash")
            Else
                If IsQuad Then
                    Return ("<" & Value0 & ":" & Value1 & ":" & Value2 & ":" & Value3 & ">")
                Else
                    Return ("<" & Value0 & ":" & Value1 & ":" & Value2 & ">")
                End If
            End If
        End Function

        Private Function Sum() As Integer
            Dim int As Integer = 0

            If IsQuad Then
                int += Value0 * (10 ^ 3)
                int += Value1 * (10 ^ 2)
                int += Value2 * (10 ^ 1)
                int += Value3 * (10 ^ 0)
            Else
                int += Value0 * (10 ^ 2)
                int += Value1 * (10 ^ 1)
                int += Value2 * (10 ^ 0)
            End If

            Return int
        End Function

        Public Shared Function InvalidHash() As TileHash
            Return New TileHash({-1, -1, -1, -1}, False)
        End Function

        Public Shared Function CreateTriangle(ThreeValues As IEnumerable(Of EdgeColor)) As TileHash
            If ThreeValues.Count <> 3 Then Return InvalidHash()
            Return New TileHash(ThreeValues(0), ThreeValues(1), ThreeValues(2))
        End Function

        Public Shared Function CreateQuad(FourValues As IEnumerable(Of EdgeColor)) As TileHash
            If FourValues.Count <> 4 Then Return InvalidHash()
            Return New TileHash(FourValues(0), FourValues(1), FourValues(2), FourValues(3))
        End Function

        Default Public ReadOnly Property Item(Index As Integer) As EdgeColor
            Get
                Select Case Index
                    Case 0
                        Return Value0
                    Case 1
                        Return Value1
                    Case 2
                        Return Value2
                    Case 3
                        Return Value3
                End Select
                Return EdgeColor.None
            End Get
        End Property

        Private Sub ComputeHash()
            Dim scmax As Integer = 0
            Dim imax As Integer = 0

            If IsQuad Then
                For i As Integer = 0 To 3 Step 1
                    Dim score As Integer = 0
                    Dim thispow As Integer = 0

                    For j As Integer = i + 3 To i Step -1
                        score += Me((j + 4) Mod 4) * (10 ^ thispow)
                        thispow += 1
                    Next

                    If score > scmax Then
                        scmax = score
                        imax = i
                    End If
                Next

                Dim rotated As New List(Of EdgeColor)
                For i As Integer = imax To imax + 3 Step 1
                    rotated.Add(Me((i + 4) Mod 4))
                Next

                Me._Value0 = rotated(0)
                Me._Value1 = rotated(1)
                Me._Value2 = rotated(2)
                Me._Value3 = rotated(3)
            Else
                For i As Integer = 0 To 2 Step 1
                    Dim score As Integer = 0
                    Dim thispow As Integer = 0

                    For j As Integer = i + 2 To i Step -1
                        score += Me((j + 3) Mod 3) * (10 ^ thispow)
                        thispow += 1
                    Next

                    If score > scmax Then
                        scmax = score
                        imax = i
                    End If
                Next

                Dim rotated As New List(Of EdgeColor)
                For i As Integer = imax To imax + 2 Step 1
                    rotated.Add(Me((i + 3) Mod 3))
                Next

                Me._Value0 = rotated(0)
                Me._Value1 = rotated(1)
                Me._Value2 = rotated(2)
                Me._Value3 = 0
            End If
        End Sub

        ''' <summary>
        ''' Counts rotation required to convert the Numbers into a TileHash
        ''' </summary>
        ''' <param name="Numbers"></param>
        ''' <returns></returns>
        Public Shared Function RotationsRequired(Numbers As IEnumerable(Of EdgeColor)) As Integer
            Dim scmax As Integer = 0
            Dim imax As Integer = 0

            If Numbers.Count = 4 Then
                For i As Integer = 0 To Numbers.Count - 1 Step 1
                    Dim score As Integer = 0
                    Dim thispow As Integer = 0

                    For j As Integer = i + 3 To i Step -1
                        score += Numbers((j + 4) Mod 4) * (10 ^ thispow)
                        thispow += 1
                    Next

                    If score > scmax Then
                        scmax = score
                        imax = i
                    End If
                Next
            ElseIf Numbers.Count = 3 Then
                For i As Integer = 0 To Numbers.Count - 1 Step 1
                    Dim score As Integer = 0
                    Dim thispow As Integer = 0

                    For j As Integer = i + 2 To i Step -1
                        score += Numbers((j + 3) Mod 3) * (10 ^ thispow)
                        thispow += 1
                    Next

                    If score > scmax Then
                        scmax = score
                        imax = i
                    End If
                Next
            Else
                Return -1
            End If


            Return imax
        End Function

        Public Function CompareTo(other As TileHash) As Integer Implements IComparable(Of TileHash).CompareTo

            If (Not Me.IsQuad) And other.IsQuad Then
                Return -1
            ElseIf Me.IsQuad And (Not other.IsQuad) Then
                Return 1
            Else
                If Me.Sum < other.Sum Then
                    Return -1
                ElseIf Me.Sum > other.Sum Then
                    Return 1
                End If
            End If

            Return 0
        End Function

        Public Shared Operator =(A As TileHash, B As TileHash) As Boolean
            If A.IsQuad <> B.IsQuad Then Return False
            Return A.Sum = B.Sum
        End Operator

        Public ReadOnly Property Count As Integer
            Get
                If IsQuad Then Return 4
                Return 3
            End Get
        End Property

        Public Shared Operator <>(A As TileHash, B As TileHash) As Boolean
            Return Not (A = B)
        End Operator

        Public Shared Operator =(Hash As TileHash, Values As IEnumerable(Of EdgeColor)) As Boolean
            If Hash.IsQuad And Values.Count <> 4 Then Return False
            If (Not Hash.IsQuad) And Values.Count <> 3 Then Return False

            For i As Integer = 0 To Hash.Count - 1 Step 1
                If Hash(i) <> Values(i) Then Return False
            Next

            Return True
        End Operator

        Public Shared Operator <>(Hash As TileHash, Values As IEnumerable(Of EdgeColor)) As Boolean
            Return Not (Hash = Values)
        End Operator

    End Structure

End Namespace
