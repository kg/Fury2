VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iFileTypePlugin"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Sub FixUpSaveFilename(ByRef Filename As String)
End Sub

Public Function CanOpenFile(ByRef Filename As String) As Boolean
End Function

Public Property Get ShowInNewMenu() As Boolean
End Property

Public Property Get FileTypeName() As String
End Property

Public Function CreateNew() As iDocument
End Function

Public Function LoadFromFilename(ByRef Filename As String) As iDocument
End Function

Public Function LoadFromFile(ByRef File As VirtualFile) As iDocument
End Function

Public Property Get FilterString() As String
End Property
