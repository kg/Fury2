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
Public Object As Variant
Public Method As String
Public BoundParameters As Variant
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Sub Bind(ByRef Meth As String, Optional ByRef Obj = Nothing, Optional BindParameters As Variant = Nothing)
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
End Sub

Public Function Invoke(ParamArray Parameters() As Variant) As Variant
On Error Resume Next
Dim l_objObject As Object
Dim l_varParameters() As Variant
Dim l_lngParameterOffset As Long
Dim l_lngParameters As Long
Dim l_lngParameterCount As Long
    If VarType(Object) = vbString Then
        Set l_objObject = Engine.Evaluate(Object)
    Else
        If Object Is Nothing Then Exit Function
        Set l_objObject = Object
    End If
    Engine.ContextLevelAdd "Bound(" & TypeName(l_objObject) & "." & Method & ")"
    If VarType(BoundParameters) = vbObject Then
        If (UBound(Parameters) >= 0) Then
            ReDim l_varParameters(0 To UBound(Parameters))
            l_lngParameterOffset = 0
        Else
            ReDim l_varParameters(0 To 0)
            Erase l_varParameters
            l_lngParameterOffset = 0
        End If
    Else
        If (UBound(Parameters) >= 0) Then
            ReDim l_varParameters(0 To UBound(Parameters) + (UBound(BoundParameters) - LBound(BoundParameters) + 1))
        Else
            ReDim l_varParameters(0 To UBound(BoundParameters))
        End If
        l_lngParameterOffset = (UBound(BoundParameters) - LBound(BoundParameters) + 1)
        For l_lngParameters = LBound(BoundParameters) To UBound(BoundParameters)
            If VarType(BoundParameters(l_lngParameters)) = vbObject Then
                Set l_varParameters(l_lngParameters - LBound(BoundParameters)) = BoundParameters(l_lngParameters)
            Else
                l_varParameters(l_lngParameters - LBound(BoundParameters)) = BoundParameters(l_lngParameters)
            End If
        Next l_lngParameters
    End If
    l_lngParameterCount = -1
    l_lngParameterCount = UBound(l_varParameters)
    If (UBound(Parameters) >= 0) Then
        For l_lngParameters = LBound(Parameters) To UBound(Parameters)
            If VarType(Parameters(l_lngParameters)) = vbObject Then
                Set l_varParameters(l_lngParameters - LBound(Parameters) + l_lngParameterOffset) = Parameters(l_lngParameters)
            Else
                l_varParameters(l_lngParameters - LBound(Parameters) + l_lngParameterOffset) = Parameters(l_lngParameters)
            End If
        Next l_lngParameters
    End If
    Err.Clear
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
    Case Else
        Invoke = CallByName(l_objObject, Method, VbMethod)
    End Select
    If Err = 438 Then
        Engine.DebugOut "Bound event failed to invoke: " & TypeName(l_objObject) & "." & Method
    End If
    Engine.ContextLevelRemove
End Function

Private Sub Class_Initialize()
On Error Resume Next
    Method = ""
    Set Object = Nothing
    BoundParameters = Array()
End Sub
