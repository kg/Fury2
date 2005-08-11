VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngMenuHost"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Property Get This() As Object
End Property

Public Property Get hwnd() As Long
End Property

Public Property Get Left() As Long
End Property

Public Property Get Top() As Long
End Property

Public Property Get Width() As Long
End Property

Public Property Get Height() As Long
End Property

Public Sub SetSize(ByVal Width As Long, ByVal Height As Long)
End Sub

Public Sub Redraw(Optional ByVal Area As Fury2Rect = Nothing)
End Sub

