VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iCustomMenuHandler"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' Public Function MenuString(ByRef Name As String, Optional ByRef Accelerator As String = "", Optional ByRef key As String = "", Optional ByVal Icon = "",
' Optional ByRef HelpText As String = "", Optional ByVal Checked As Boolean = False, Optional ByVal Enabled As Boolean = True,
' Optional ByVal ItemData As Long = -1) As String

Public Sub DefineMenu(Caption As String, Key As String, Optional ParentKey As String, Optional AcceleratorString As String = "", Optional Icon As IPictureDisp = Nothing, Optional HelpText As String = "", Optional ByVal Checked As Boolean = False, Optional ByVal Enabled As Boolean = True)
On Error Resume Next
End Sub

Public Sub DestroyMenu(Key As String)
On Error Resume Next
End Sub
