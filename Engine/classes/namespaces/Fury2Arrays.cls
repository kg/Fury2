VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Arrays"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Sub ArrayAdd(Arr As Variant, Value)
On Error Resume Next
    Add Arr, Value
End Sub

Sub Add(Arr As Variant, Value)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long
Dim m_varNull As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    Err.Clear
    If (VarType(Value) And vbArray) = vbArray Then
        m_lngUB = m_lngUB + (UBound(Value) - LBound(Value) + 1)
        ReDim Preserve Arr(m_lngLB To m_lngUB) As Variant
        Copy Arr, Value, m_lngUB - (UBound(Value) - LBound(Value)), 0, (UBound(Value) - LBound(Value) + 1)
    Else
        m_lngUB = m_lngUB + 1
        ReDim Preserve Arr(m_lngLB To m_lngUB) As Variant
        Assign Arr(m_lngUB), Value
    End If
    Err.Clear
End Sub

Sub ArrayAssign(ByRef aTo, ByRef aFrom)
On Error Resume Next
    If VarType(aFrom) = vbObject Then
        Set aTo = aFrom
    Else
        aTo = aFrom
    End If
End Sub

Sub Assign(ByRef aTo, ByRef aFrom)
On Error Resume Next
    If VarType(aFrom) = vbObject Then
        Set aTo = aFrom
    Else
        aTo = aFrom
    End If
End Sub

Sub ArrayCopy(Target As Variant, Source As Variant, Optional ByVal Position As Long = 0, Optional ByVal SourcePosition As Long = 0, Optional ByVal Count As Long = -1)
On Error Resume Next
    Copy Target, Source, Position, SourcePosition, Count
End Sub

Sub Copy(Target As Variant, Source As Variant, Optional ByVal Position As Long = 0, Optional ByVal SourcePosition As Long = 0, Optional ByVal Count As Long = -1)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngSourceLB As Long, m_lngSourceUB As Long
Dim m_lngCopy As Long
    Err.Clear
    If (VarType(Target) And vbArray) <> vbArray Then Exit Sub
    If (VarType(Source) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Target)
    m_lngUB = UBound(Target)
    m_lngSourceLB = LBound(Source)
    m_lngSourceUB = UBound(Source)
    If Count = -1 Then Count = (m_lngSourceUB - m_lngSourceLB + 1)
    If Position < m_lngLB Then
        Count = Count - (m_lngLB - Position)
        Position = m_lngLB
    End If
    If Position > m_lngUB Then
        Count = 1
        Position = m_lngUB
    End If
    If SourcePosition < m_lngSourceLB Then
        Count = Count - (m_lngSourceLB - SourcePosition)
        SourcePosition = m_lngSourceLB
    End If
    If SourcePosition > m_lngSourceUB Then
        Count = 1
        SourcePosition = m_lngSourceUB
    End If
    If Count > (m_lngUB - Position) + 1 Then
        Count = (m_lngUB - Position) + 1
    End If
    If Count > (m_lngSourceUB - SourcePosition) + 1 Then
        Count = (m_lngSourceUB - SourcePosition) + 1
    End If
    If Count < 1 Then
        Exit Sub
    End If
    For m_lngCopy = 0 To Count - 1
        Assign Target(m_lngCopy + Position), Source(m_lngCopy + SourcePosition)
    Next m_lngCopy
    Err.Clear
End Sub

Sub ArrayInsert(Arr As Variant, Value, ByVal Index As Long)
On Error Resume Next
    Insert Arr, Value, Index
End Sub

Sub Insert(Arr As Variant, Value, ByVal Index As Long)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then
        ReDim Arr(0 To Index)
        m_lngUB = Index
        Assign Arr(m_lngUB), Value
        Exit Sub
    End If
    Err.Clear
    If Index < m_lngLB Then Index = m_lngLB
    If Index > (m_lngUB + 1) Then
        ReDim Preserve Arr(m_lngLB To Index)
        m_lngUB = Index
    Else
        ReDim Preserve Arr(m_lngLB To m_lngUB + 1)
        m_lngUB = m_lngUB + 1
    End If
    If Index = m_lngLB Then
        For m_lngItems = m_lngUB - 1 To m_lngLB Step -1
            Assign Arr(m_lngItems + 1), Arr(m_lngItems)
        Next m_lngItems
    ElseIf Index = m_lngUB Then
    Else
        For m_lngItems = m_lngUB - 1 To Index Step -1
            Assign Arr(m_lngItems + 1), Arr(m_lngItems)
        Next m_lngItems
    End If
    Assign Arr(Index), Value
    Err.Clear
End Sub

Sub ArrayRemove(Arr As Variant, ByVal Index As Long)
On Error Resume Next
    Remove Arr, Index
End Sub

Sub Remove(Arr As Variant, ByVal Index As Long)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If Index < m_lngLB Then Exit Sub
    If Index > m_lngUB Then Exit Sub
    If m_lngLB >= m_lngUB Then
        Erase Arr
        Exit Sub
    End If
    If Index = m_lngLB Then
        For m_lngItems = m_lngLB + 1 To m_lngUB
            Arr(m_lngItems - 1) = Arr(m_lngItems)
        Next m_lngItems
    ElseIf Index = m_lngUB Then
    Else
        For m_lngItems = Index + 1 To m_lngUB
            Arr(m_lngItems - 1) = Arr(m_lngItems)
        Next m_lngItems
    End If
    ReDim Preserve Arr(m_lngLB To m_lngUB - 1)
    Err.Clear
End Sub

Sub ArrayReplace(Arr As Variant, Match, Replace)
On Error Resume Next
    Me.Replace Arr, Match, Replace
End Sub

Sub Replace(Arr As Variant, Match, Replace)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    For m_lngItems = m_lngLB To m_lngUB
        If Arr(m_lngItems) = Match Then Assign Arr(m_lngItems), Replace
    Next m_lngItems
    Err.Clear
End Sub

Sub ReplaceEx(Arr As Variant, ByVal MatchFunction As Fury2Event, ByVal ReplaceFunction As Fury2Event)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    If MatchFunction Is Nothing Then Exit Sub
    If ReplaceFunction Is Nothing Then Exit Sub
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    For m_lngItems = m_lngLB To m_lngUB
        If CBool(MatchFunction.Invoke(Arr(m_lngItems))) Then
            ReplaceFunction.Invoke Arr(m_lngItems)
        End If
    Next m_lngItems
    Err.Clear
End Sub

Sub ArrayResize(Arr As Variant, ByVal NewSize As Long)
On Error Resume Next
    Resize Arr, NewSize
End Sub

Sub Resize(Arr As Variant, ByVal NewSize As Long)
On Error Resume Next
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    ReDim Preserve Arr(0 To NewSize - 1)
    Err.Clear
End Sub

Sub ArraySort(Arr As Variant, Optional ByVal Descending As Boolean = False)
On Error Resume Next
    Sort Arr, Descending
End Sub

Sub Sort(Arr As Variant, Optional ByVal Descending As Boolean = False)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim P As Long, K As Long, H As Long, I As Long, J As Long
Dim Temp As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If m_lngCount < 2 Then Exit Sub
    For P = m_lngLB To ClipValue(m_lngUB - 1, m_lngLB, m_lngUB)
        H = P
        If Descending Then
            For K = P + 1 To m_lngUB
                If Arr(K) > Arr(H) Then H = K
            Next K
        Else
            For K = P + 1 To m_lngUB
                If Arr(K) < Arr(H) Then H = K
            Next K
        End If
        If P <> H Then
            I = H
            J = P
            Assign Temp, Arr(I)
            Assign Arr(I), Arr(J)
            Assign Arr(J), Temp
        End If
    Next P
    Err.Clear
End Sub

Sub SortEx(Arr As Variant, Optional ByVal TestFunction As Fury2Event)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim P As Long, K As Long, H As Long, I As Long, J As Long
Dim Temp As Variant
    If TestFunction Is Nothing Then Exit Sub
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If m_lngCount < 2 Then Exit Sub
    For P = m_lngLB To ClipValue(m_lngUB - 1, m_lngLB, m_lngUB)
        H = P
        For K = P + 1 To m_lngUB
            If CBool(TestFunction.Invoke(Arr(K), Arr(H))) Then H = K
        Next K
        If P <> H Then
            I = H
            J = P
            Temp = Arr(I)
            Arr(I) = Arr(J)
            Arr(J) = Temp
        End If
    Next P
    Err.Clear
End Sub

Function ArrayCount(Arr As Variant, Value) As Long
On Error Resume Next
    ArrayCount = Count(Arr, Value)
End Function

Function Count(Arr As Variant, Value) As Long
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
    Count = 0
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Function
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Function
    For m_lngItems = m_lngLB To m_lngUB
        If Arr(m_lngItems) = Value Then
            Count = Count + 1
        End If
    Next m_lngItems
    Err.Clear
End Function

Function ArrayFind(Arr As Variant, ByRef Value) As Long
On Error Resume Next
    ArrayFind = Find(Arr, Value)
End Function

Function Find(Arr As Variant, ByRef Value) As Long
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
Dim l_objValue As Object
    Find = -1
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Function
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Function
    Set l_objValue = Value
    If l_objValue Is Nothing Then
        For m_lngItems = m_lngLB To m_lngUB
            If Arr(m_lngItems) = Value Then
                Find = m_lngItems
                Exit Function
            End If
        Next m_lngItems
    Else
        For m_lngItems = m_lngLB To m_lngUB
            If Arr(m_lngItems) Is l_objValue Then
                Find = m_lngItems
                Exit Function
            End If
        Next m_lngItems
    End If
    Err.Clear
End Function

Function ArrayJoin(ParamArray Arrays() As Variant)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long
Dim m_lngArrays As Long, m_lngSize As Long
Dim m_varArray As Variant, m_lngPosition As Long
    m_lngLB = LBound(Arrays)
    m_lngUB = UBound(Arrays)
    m_lngSize = 0
    For m_lngArrays = m_lngLB To m_lngUB
        m_lngSize = m_lngSize + (UBound(Arrays(m_lngArrays)) - LBound(Arrays(m_lngArrays)) + 1)
    Next m_lngArrays
    m_varArray = Array()
    ReDim m_varArray(0 To m_lngSize - 1)
    m_lngPosition = 0
    For m_lngArrays = m_lngLB To m_lngUB
        Copy m_varArray, Arrays(m_lngArrays), m_lngPosition, 0
        m_lngPosition = m_lngPosition + (UBound(Arrays(m_lngArrays)) - LBound(Arrays(m_lngArrays)) + 1)
    Next m_lngArrays
    ArrayJoin = m_varArray
End Function

Function Join(ParamArray Arrays() As Variant)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long
Dim m_lngArrays As Long, m_lngSize As Long
Dim m_varArray As Variant, m_lngPosition As Long
    m_lngLB = LBound(Arrays)
    m_lngUB = UBound(Arrays)
    m_lngSize = 0
    For m_lngArrays = m_lngLB To m_lngUB
        m_lngSize = m_lngSize + (UBound(Arrays(m_lngArrays)) - LBound(Arrays(m_lngArrays)) + 1)
    Next m_lngArrays
    m_varArray = Array()
    ReDim m_varArray(0 To m_lngSize - 1)
    m_lngPosition = 0
    For m_lngArrays = m_lngLB To m_lngUB
        Copy m_varArray, Arrays(m_lngArrays), m_lngPosition, 0
        m_lngPosition = m_lngPosition + (UBound(Arrays(m_lngArrays)) - LBound(Arrays(m_lngArrays)) + 1)
    Next m_lngArrays
    Join = m_varArray
End Function

Function ArraySlice(Source As Variant, ByVal Length As Long, Optional ByVal Start As Long = 0)
On Error Resume Next
    ArraySlice = Slice(Source, Length, Start)
End Function

Function ArrayRotate(Source As Variant, ByVal Shift As Long)
On Error Resume Next
    ArrayRotate = Rotate(Source, Shift)
End Function

Function Rotate(Source As Variant, ByVal Shift As Long)
On Error Resume Next
Dim l_varNew As Variant
Dim m_lngSourceLB As Long, m_lngSourceUB As Long
Dim m_lngCopy As Long
    If (VarType(Source) And vbArray) <> vbArray Then Exit Function
    Err.Clear
    m_lngSourceLB = LBound(Source)
    m_lngSourceUB = UBound(Source)
    ReDim l_varNew(m_lngSourceLB To m_lngSourceUB)
    For m_lngCopy = m_lngSourceLB To m_lngSourceUB
        Assign l_varNew(m_lngCopy), Source(WrapValue(m_lngCopy + Shift, m_lngSourceLB, m_lngSourceUB))
    Next m_lngCopy
    Err.Clear
    Rotate = l_varNew
    Err.Clear
End Function

Function Slice(Source As Variant, ByVal Length As Long, Optional ByVal Start As Long = 0)
On Error Resume Next
Dim l_varNew As Variant
    ReDim l_varNew(0 To Length - 1)
    Copy l_varNew, Source, 0, Start, Length
    Slice = l_varNew
    Err.Clear
End Function

Function ArraySum(Arr As Variant) As Variant
On Error Resume Next
    ArraySum = Sum(Arr)
End Function

Function Sum(Arr As Variant) As Variant
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim m_lngItems As Long
Dim m_varValue As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Function
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Function
    For m_lngItems = m_lngLB To m_lngUB
        m_varValue = m_varValue + Arr(m_lngItems)
    Next m_lngItems
    Sum = m_varValue
    Err.Clear
End Function

Public Function Range(ByVal StartValue As Long, ByVal EndValue As Long) As Variant
On Error Resume Next
Dim l_varValue As Variant
Dim l_lngIndex As Long
    ReDim l_varValue(0 To (EndValue - StartValue))
    For l_lngIndex = LBound(l_varValue) To UBound(l_varValue)
        l_varValue(l_lngIndex) = l_lngIndex + StartValue
    Next l_lngIndex
    Range = l_varValue
End Function

