Sub Split(s As String, arr() As Integer)
	Dim cnt As Integer
	Dim c As Integer
	Dim n As Integer
	Dim m As Integer

	cnt = 0
	For i As Integer = 1 To Len(s)
		If Mid(s, i, 1) = "," Then
			cnt += 1
		End If
	Next

	ReDim arr(cnt)

	cnt = 0
	n = 0
	m = 1
	cnt = 0
	For i As Integer = 1 To Len(s)+1
		c = Asc(Mid(s, i, 1))
		Select Case c
		Case 45
			m = -1
		Case 48 To 57
			n = n * 10 + c - 48
		Case Else '' comma or end of string
			n *= m
			arr(cnt) = n
''			Print n
			n = 0
			m = 1
			cnt += 1
		End Select
	Next
End Sub

Sub ReadMemory(filename As String, memory() As Integer)
	Dim l As String

	Open filename For Input As #1
	Line Input #1, l
	Close

	Split(l, memory())
End Sub

Sub PrintMemory(memory() As Integer)
	Print "Mem Start"
	For i As Integer = LBound(memory) To UBound(memory)
		Print memory(i)
	Next
	Print "Mem End"
End Sub

Function Arg(memory() As Integer, inst As Integer, op As Integer, a As Integer) As Integer
	For i As Integer = 0 To a
		op \= 10
	Next
	
	Select Case op Mod 10
	Case 0
		Return memory(memory(inst + a))
	Case 1
		Return memory(inst + a)
	Case Else
		Print "Arg Error", op Mod 10
	End Select
End Function

Sub Sto(memory() As Integer, inst As Integer, op As Integer, a As Integer, v As Integer)
	For i As Integer = -1 To a
		op \= 10
	Next
	
	Select Case op Mod 10
	Case 0
		memory(memory(inst + a)) = v
	End Select
End Sub

'' 1
Function Add(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim a1 As Integer
	Dim a2 As Integer

	a1 = Arg(memory(), inst, op, 1)
	a2 = Arg(memory(), inst, op, 2)
	Print "Add", a1, a2
	Sto(memory(), inst, op, 3, a1 + a2)

	Return inst + 4
End Function

'' 2
Function Mul(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim a1 As Integer
	Dim a2 As Integer

	a1 = Arg(memory(), inst, op, 1)
	a2 = Arg(memory(), inst, op, 2)
	Print "Mul", a1, a2
	Sto(memory(), inst, op, 3, a1 * a2)

	Return inst + 4
End Function

'' 3
Function Inpu(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim i As Integer
	Input "< Inpu         ", i

	Sto(memory(), inst, op, 1, i)

	Return inst + 2
End Function

'' 4
Function Outp(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim a As Integer

	a = Arg(memory(), inst, op, 1)
	Print "> Outp", a

	Return inst + 2
End Function

'' 5
Function JmpT(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim a1 As Integer
	Dim a2 As Integer

	a1 = Arg(memory(), inst, op, 1)
	a2 = Arg(memory(), inst, op, 2)
	Print "JmpT", a1, a2

	If a1 <> 0 Then
		Return a2
	End If

	Return inst + 3
End Function

'' 6
Function JmpF(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim a1 As Integer
	Dim a2 As Integer

	a1 = Arg(memory(), inst, op, 1)
	a2 = Arg(memory(), inst, op, 2)
	Print "JmpF", a1, a2

	If a1 = 0 Then
		Return a2
	End If

	Return inst + 3
End Function

'' 7
Function IsLt(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim a1 As Integer
	Dim a2 As Integer

	a1 = Arg(memory(), inst, op, 1)
	a2 = Arg(memory(), inst, op, 2)
	Print "IsLt", a1, a2

	If a1 < a2 Then
		Sto(memory(), inst, op, 3, 1)
	Else
		Sto(memory(), inst, op, 3, 0)
	End If

	Return inst + 4
End Function

'' 8
Function IsEq(memory() As Integer, inst As Integer, op As Integer) As Integer
	Dim a1 As Integer
	Dim a2 As Integer

	a1 = Arg(memory(), inst, op, 1)
	a2 = Arg(memory(), inst, op, 2)
	Print "IsEq", a1, a2

	If a1 = a2 Then
		Sto(memory(), inst, op, 3, 1)
	Else
		Sto(memory(), inst, op, 3, 0)
	End If

	Return inst + 4
End Function

'' 99
Function Halt(memory() As Integer, inst As Integer, op As Integer) As Integer
	Print "Halt"

	Return -1
End Function

Function InstStep(memory() As Integer, inst As Integer) As Integer
	Dim op As Integer

	op = memory(inst)
	
	Select Case op Mod 100
	Case 1
		Return Add(memory(), inst, op)
	Case 2
		Return Mul(memory(), inst, op)
	Case 3
		Return Inpu(memory(), inst, op)
	Case 4
		Return Outp(memory(), inst, op)
	Case 5
		Return JmpT(memory(), inst, op)
	Case 6
		Return JmpF(memory(), inst, op)
	Case 7
		Return IsLt(memory(), inst, op)
	Case 8
		Return IsEq(memory(), inst, op)
	Case 99
		Return Halt(memory(), inst, op)
	Case Else
		Print "Inst Error", op Mod 100
	End Select
End Function

Sub InstLoop(memory() As Integer)
	Dim inst As Integer = 0

	While inst >= 0
		Print inst
		inst = InstStep(memory(), inst)
	WEnd
	Print "End"
End Sub

Dim mem(Any) As Integer
ReadMemory(*__FB_ARGV__[1], mem())

InstLoop(mem())

'' PrintMemory(mem())


