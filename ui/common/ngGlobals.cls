VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngGlobals"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Function BindEvent(ByRef Obj As Object, ByRef Method As String, Optional ByRef Parameters = Nothing) As ngEvent
On Error Resume Next
Dim l_evtEvent As ngEvent
    Set l_evtEvent = New ngEvent
    l_evtEvent.Bind Obj, Method, Parameters
    Set BindEvent = l_evtEvent
End Function
