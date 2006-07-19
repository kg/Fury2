VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Event"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Public Blocking As Boolean
Public Object As Object
Public Method As String
Public MethodID As Long
Public BoundParameters As Variant
Private m_lngBlocked As Long
Private m_engEngine As Fury2Engine
Private m_seEngine As ScriptEngine

Public Function Duplicate() As Fury2Event
On Error Resume Next
    Set Duplicate = New Fury2Event
    With Duplicate
        .SetEngine m_engEngine
        Set .Object = Object
        .Method = Method
        .MethodID = MethodID
        .BoundParameters = BoundParameters
        .Blocking = Blocking
    End With
End Function

Public Property Get Blocked() As Boolean
On Error Resume Next
    Blocked = m_lngBlocked > 0
End Property

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
    Set m_seEngine = m_engEngine.ScriptEngine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Sub ResolveMethod()
On Error Resume Next
    MethodID = m_seEngine.ResolveMember(Object, Method)
End Sub

Public Sub Bind(ByRef Meth As String, Optional ByVal Obj = Nothing, Optional ByRef BindParameters As Variant = Nothing)
On Error Resume Next
    Method = Meth
    If VarType(Obj) = vbString Then
        Object = CStr(Obj)
    Else
        Set Object = Obj
    End If
    If VarType(BindParameters) = vbObject Then
        Set BoundParameters = Nothing
    ElseIf (VarType(BindParameters) And vbArray) = vbArray Then
        BoundParameters = BindParameters
    End If
    ResolveMethod
End Sub

Public Function Invoke(ParamArray Parameters() As Variant) As Variant
On Error Resume Next
Dim l_objObject As Object
Dim l_varParameters() As Variant
Dim l_lngParameterOffset As Long
Dim l_lngParameters As Long
Dim l_lngParameterCount As Long
Dim l_lngIndex As Long, l_lngIncrement As Long
    If Blocking And Blocked Then Exit Function
    If VarType(Object) = vbString Then
        Set l_objObject = Engine.Evaluate(Object)
    Else
        If Object Is Nothing Then Exit Function
        Set l_objObject = Object
    End If
    Engine.ContextLevelAdd "Event(" & TypeName(l_objObject) & "." & Method & ")"
    If VarType(BoundParameters) = vbObject Then
        If (UBound(Parameters) >= 0) Then
            ReDim l_varParameters(0 To UBound(Parameters))
            l_lngParameterOffset = 0
        Else
            ReDim l_varParameters(0 To 0)
            Erase l_varParameters
            l_lngParameterOffset = 0
        End If
        If (MethodID <> -1) Then
            l_lngIndex = UBound(l_varParameters)
            l_lngIncrement = -1
        Else
            l_lngIndex = LBound(l_varParameters)
            l_lngIncrement = 1
        End If
    Else
        If (UBound(Parameters) >= 0) Then
            ReDim l_varParameters(0 To UBound(Parameters) + (UBound(BoundParameters) - LBound(BoundParameters) + 1))
        Else
            ReDim l_varParameters(0 To UBound(BoundParameters))
        End If
        l_lngParameterOffset = (UBound(BoundParameters) - LBound(BoundParameters) + 1)
        If (MethodID <> -1) Then
            l_lngIndex = UBound(l_varParameters) - l_lngParameterOffset
            l_lngIncrement = -1
        Else
            l_lngIndex = LBound(l_varParameters) + l_lngParameterOffset
            l_lngIncrement = 1
        End If
        For l_lngParameters = LBound(BoundParameters) To UBound(BoundParameters)
            If VarType(BoundParameters(l_lngParameters)) = vbObject Then
                Set l_varParameters(l_lngIndex) = BoundParameters(l_lngParameters)
            Else
                l_varParameters(l_lngIndex) = BoundParameters(l_lngParameters)
            End If
            l_lngIndex = l_lngIndex + l_lngIncrement
        Next l_lngParameters
    End If
    l_lngParameterCount = -1
    l_lngParameterCount = UBound(l_varParameters)
    If (UBound(Parameters) >= 0) Then
        For l_lngParameters = LBound(Parameters) To UBound(Parameters)
            If VarType(Parameters(l_lngParameters)) = vbObject Then
                Set l_varParameters(l_lngIndex) = Parameters(l_lngParameters)
            Else
                l_varParameters(l_lngIndex) = Parameters(l_lngParameters)
            End If
            l_lngIndex = l_lngIndex + l_lngIncrement
        Next l_lngParameters
    End If
    Err.Clear
    m_lngBlocked = m_lngBlocked + 1
    If (MethodID <> -1) Then
        Invoke = m_seEngine.InvokeMember(l_objObject, MethodID, VbMethod, l_varParameters)
    Else
        Select Case l_lngParameterCount
        Case 0
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0))
        Case 1
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1))
        Case 2
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2))
        Case 3
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3))
        Case 4
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4))
        Case 5
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5))
        Case 6
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6))
        Case 7
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7))
        Case 8
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8))
        Case 9
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9))
        Case 10
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9), l_varParameters(10))
        Case 11
            Invoke = CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9), l_varParameters(10), l_varParameters(11))
        Case Else
            Invoke = CallByName(l_objObject, Method, VbMethod)
        End Select
    End If
    m_lngBlocked = m_lngBlocked - 1
    If Err = 438 Then
        Engine.DebugOut "Event failed to invoke: " & TypeName(l_objObject) & "." & Method
    End If
    Engine.ContextLevelRemove
End Function

Private Sub Class_Initialize()
On Error Resume Next
    Method = ""
    Set Object = Nothing
    BoundParameters = Array()
End Sub
