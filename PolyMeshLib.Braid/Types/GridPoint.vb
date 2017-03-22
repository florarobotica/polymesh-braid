Namespace Types

    Public Structure GridPoint
        Private _level As Integer
        Private _u As Integer
        Private _v As Integer

        Sub New(Level As Integer, U As Integer, V As Integer)
            _level = Level
            _u = U
            _v = V
        End Sub

        Public Property Level As Integer
            Get
                Return _level
            End Get
            Set(value As Integer)
                _level = value
            End Set
        End Property

        Public Property U As Integer
            Get
                Return _u
            End Get
            Set(value As Integer)
                _u = value
            End Set
        End Property

        Public Property V As Integer
            Get
                Return _v
            End Get
            Set(value As Integer)
                _v = value
            End Set
        End Property

        Public Shared Operator =(A As GridPoint, B As GridPoint) As Boolean
            If A.Level <> B.Level OrElse A.U <> B.U OrElse A.V <> B.V Then Return False
            Return True
        End Operator

        Public Shared Operator <>(A As GridPoint, B As GridPoint) As Boolean
            Return Not (A = B)
        End Operator

        Public Overrides Function ToString() As String
            Return "GridPoint (Level: " & Level & " U: " & U & " V :" & V & ")"
        End Function

        Public Shared Function Create(Level As Integer, U As Integer, V As Integer) As GridPoint
            Return New GridPoint(Level, U, V)
        End Function

    End Structure

End Namespace
