VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IVirtualFileCollection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
'
'   VirtualFile Saveable Collection Interface
'

Public Sub Reset()
On Error Resume Next
End Sub

Public Function CreateNew() As Object
On Error Resume Next
End Function

Public Property Get Count() As Long
On Error Resume Next
End Property

Public Function GetItem(ByVal Index As Long) As Object
On Error Resume Next
End Function

Public Sub AddItem(ByRef Item As Object)
On Error Resume Next
End Sub

Public Sub SaveHeader(ByRef File As VirtualFile)
End Sub

Public Sub LoadHeader(ByRef File As VirtualFile)
End Sub

