VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iDocument"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Property Get Filename() As String
End Property

Public Property Get Typename() As String
End Property

Public Function Save(Filename As String) As Boolean
End Function

Public Property Get CanSave() As Boolean
End Property

Public Property Get Plugin() As iFileTypePlugin
End Property

Public Property Set Plugin(NewPlugin As iFileTypePlugin)
End Property
