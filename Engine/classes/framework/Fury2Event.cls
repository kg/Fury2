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
Public Object As Object
Public Method As String

Public Sub Bind(ByRef Meth As String, Optional ByRef Obj As Object = Nothing)
On Error Resume Next
    Method = Meth
    Set Object = Obj
End Sub

Public Function Invoke(ParamArray Parameters() As Variant) As Variant
On Error Resume Next
    Select Case UBound(Parameters)
    Case 0
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0))
    Case 1
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0), Parameters(1))
    Case 2
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0), Parameters(1), Parameters(2))
    Case 3
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3))
    Case 4
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4))
    Case 5
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5))
    Case 6
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6))
    Case 7
        Invoke = VBA.CallByName(Object, Method, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6), Parameters(7))
    Case Else
        Invoke = VBA.CallByName(Object, Method, VbMethod)
    End Select
End Function
