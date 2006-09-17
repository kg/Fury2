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
Implements Fury2Object
Implements ICallable

Public Blocking As Boolean
Public Object
Attribute Object.VB_VarDescription = "The object that contains the method to invoke."
Public Method As String
Attribute Method.VB_VarDescription = "The name of the method to invoke."
Public MethodID As Long
Public BoundParameters As Variant
Attribute BoundParameters.VB_VarDescription = "The parameters to pass at the end of the parameter list when invoking the method."
Private m_lngBlocked As Long
Private m_engEngine As Fury2Engine
Private m_seEngine As ScriptEngine
Private m_strContextName As String

Public Function Duplicate() As Fury2Event
Attribute Duplicate.VB_Description = "Creates a copy of this event."
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
Dim l_objObject As Object
    If m_seEngine Is Nothing Then
        MethodID = -1
    ElseIf VarType(Object) = vbString Then
        MethodID = -1
    Else
        Set l_objObject = Object
        If l_objObject Is Nothing Then
            MethodID = -1
        Else
            MethodID = m_seEngine.ResolveMember(l_objObject, Method)
        End If
    End If
End Sub

Public Sub Bind(ByRef Meth As String, Optional ByVal Obj = Nothing, Optional ByRef BindParameters As Variant = Nothing)
Attribute Bind.VB_Description = "Binds this event to a new method."
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
    m_strContextName = ""
End Sub

Public Function Invoke(ParamArray Parameters() As Variant) As Variant
Attribute Invoke.VB_Description = "Invokes this event, optionally passing some parameters."
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
            ReDim l_varParameters(0 To (UBound(Parameters) - LBound(Parameters) + 1) + (UBound(BoundParameters) - LBound(BoundParameters) + 1) - 1)
            l_lngParameterOffset = UBound(Parameters) - LBound(Parameters) + 1
        Else
            ReDim l_varParameters(0 To UBound(BoundParameters))
            l_lngParameterOffset = 0
        End If
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
        If (MethodID <> -1) Then
            l_lngIndex = UBound(l_varParameters)
            l_lngIncrement = -1
        Else
            l_lngIndex = LBound(l_varParameters)
            l_lngIncrement = 1
        End If
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
    If Len(m_strContextName) = 0 Then
        m_strContextName = "Event(" & TypeName(l_objObject) & "." & Method & ")"
    End If
    Engine.ContextLevelAdd m_strContextName
    m_lngBlocked = m_lngBlocked + 1
    If (MethodID <> -1) Then
        VarCopy m_seEngine.InvokeMember(l_objObject, MethodID, VbMethod, l_varParameters), Invoke
    Else
        Select Case l_lngParameterCount
        Case 0
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0)), Invoke
        Case 1
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1)), Invoke
        Case 2
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2)), Invoke
        Case 3
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3)), Invoke
        Case 4
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4)), Invoke
        Case 5
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5)), Invoke
        Case 6
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6)), Invoke
        Case 7
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7)), Invoke
        Case 8
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8)), Invoke
        Case 9
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9)), Invoke
        Case 10
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9), l_varParameters(10)), Invoke
        Case 11
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9), l_varParameters(10), l_varParameters(11)), Invoke
        Case Else
            VarCopy CallByName(l_objObject, Method, VbMethod), Invoke
        End Select
    End If
    Erase l_varParameters
    m_lngBlocked = m_lngBlocked - 1
    If Err = 438 Then
        Engine.DebugOut "Invoke failed: " & m_strContextName
    End If
    Engine.ContextLevelRemove
End Function

Private Sub Class_Initialize()
On Error Resume Next
    MethodID = -1
    Method = ""
    Set Object = Nothing
    BoundParameters = Array()
End Sub

Private Function ICallable_Invoke(ParamArray Parameters() As Variant) As Variant
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
            ReDim l_varParameters(0 To (UBound(Parameters) - LBound(Parameters) + 1) + (UBound(BoundParameters) - LBound(BoundParameters) + 1) - 1)
            l_lngParameterOffset = UBound(Parameters) - LBound(Parameters) + 1
        Else
            ReDim l_varParameters(0 To UBound(BoundParameters))
            l_lngParameterOffset = 0
        End If
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
        If (MethodID <> -1) Then
            l_lngIndex = UBound(l_varParameters)
            l_lngIncrement = -1
        Else
            l_lngIndex = LBound(l_varParameters)
            l_lngIncrement = 1
        End If
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
    If Len(m_strContextName) = 0 Then
        m_strContextName = "Event(" & TypeName(l_objObject) & "." & Method & ")"
    End If
    Engine.ContextLevelAdd m_strContextName
    m_lngBlocked = m_lngBlocked + 1
    If (MethodID <> -1) Then
        VarCopy m_seEngine.InvokeMember(l_objObject, MethodID, VbMethod, l_varParameters), ICallable_Invoke
    Else
        Select Case l_lngParameterCount
        Case 0
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0)), ICallable_Invoke
        Case 1
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1)), ICallable_Invoke
        Case 2
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2)), ICallable_Invoke
        Case 3
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3)), ICallable_Invoke
        Case 4
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4)), ICallable_Invoke
        Case 5
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5)), ICallable_Invoke
        Case 6
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6)), ICallable_Invoke
        Case 7
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7)), ICallable_Invoke
        Case 8
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8)), ICallable_Invoke
        Case 9
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9)), ICallable_Invoke
        Case 10
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9), l_varParameters(10)), ICallable_Invoke
        Case 11
            VarCopy CallByName(l_objObject, Method, VbMethod, l_varParameters(0), l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6), l_varParameters(7), l_varParameters(8), l_varParameters(9), l_varParameters(10), l_varParameters(11)), ICallable_Invoke
        Case Else
            VarCopy CallByName(l_objObject, Method, VbMethod), ICallable_Invoke
        End Select
    End If
    Erase l_varParameters
    m_lngBlocked = m_lngBlocked - 1
    If Err = 438 Then
        Engine.DebugOut "Invoke failed: " & m_strContextName
    End If
    Engine.ContextLevelRemove
End Function
