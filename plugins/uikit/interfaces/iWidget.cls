VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iWidget"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Property Get CanGetFocus() As Boolean
End Property

Public Sub LostCapture(ByVal Parent As iContainer)
End Sub

Public Function HitTest(ByVal X As Long, ByVal Y As Long) As Boolean
End Function

Public Sub MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Public Sub MouseMove(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Public Sub MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Public Sub MouseEnter(ByVal Parent As iContainer)
End Sub

Public Sub MouseLeave(ByVal Parent As iContainer)
End Sub

Public Sub GotFocus(ByVal Parent As iContainer, ByVal FromWidget As iWidget)
End Sub

Public Sub LostFocus(ByVal Parent As iContainer, ByVal ToWidget As iWidget)
End Sub

Public Sub KeyDown(ByVal Parent As iContainer, ByRef KeyCode As Long, ByVal Shift As Long)
End Sub

Public Sub KeyPress(ByVal Parent As iContainer, ByRef Character As Long)
End Sub

Public Sub KeyUp(ByVal Parent As iContainer, ByRef KeyCode As Long, ByVal Shift As Long)
End Sub

Public Sub Redraw(ByVal Parent As iContainer, ByVal Surface As Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
End Sub
