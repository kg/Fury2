VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAccelerator"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public KeyCode As Long
Public Shift As Boolean
Public Ctrl As Boolean
Public Alt As Boolean
Public Hook As ngEvent

Public Property Get KeyString() As String
On Error Resume Next
    KeyString = Format(KeyCode, "000") & IIf(Shift, "1", "0") & IIf(Ctrl, "1", "0") & IIf(Alt, "1", "0")
End Property
