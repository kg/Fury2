VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iContainer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Sub Sync()
End Sub

Public Property Get ActiveWidget() As iWidget
End Property

Public Property Get HasChild(ByRef Widget As iWidget) As Boolean
End Property

Public Property Get Children() As WidgetCollection
End Property

Public Function WidgetFromPoint(ByVal X As Long, ByVal Y As Long) As iWidget
End Function

Public Sub SetCapture(ByRef Widget As iWidget)
End Sub

Public Sub ReleaseCapture(ByRef Widget As iWidget)
End Sub

Public Sub SetFocus(ByRef Widget As iWidget)
End Sub

Public Property Get Captured(ByRef Widget As iWidget) As Boolean
End Property
