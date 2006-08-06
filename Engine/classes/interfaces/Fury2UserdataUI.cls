VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2UserdataUI"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Sub Redraw(ByVal Surface As Fury2Image)
Attribute Redraw.VB_Description = "Called when the UI needs to be redrawn."
On Error Resume Next
End Sub

Public Sub MouseDown(ByVal Button As Long, ByVal Shift As Long, ByVal X As Long, ByVal Y As Long)
Attribute MouseDown.VB_Description = "Called when the UI recieves a mouse click."
On Error Resume Next
End Sub

Public Sub MouseMove(ByVal Button As Long, ByVal Shift As Long, ByVal X As Long, ByVal Y As Long)
Attribute MouseMove.VB_Description = "Called when the mouse moves over the UI."
On Error Resume Next
End Sub

Public Sub MouseUp(ByVal Button As Long, ByVal Shift As Long, ByVal X As Long, ByVal Y As Long)
Attribute MouseUp.VB_Description = "Called when the mouse is released on the UI."
On Error Resume Next
End Sub

Public Sub KeyDown(ByVal Key As Long, ByVal Shift As Long)
Attribute KeyDown.VB_Description = "Called when the UI recieves a keypress."
On Error Resume Next
End Sub

Public Sub KeyPress(ByVal Character As Long)
Attribute KeyPress.VB_Description = "Called when the UI recieves a text character."
On Error Resume Next
End Sub

Public Sub KeyUp(ByVal Key As Long, ByVal Shift As Long)
Attribute KeyUp.VB_Description = "Called when a key is released."
On Error Resume Next
End Sub

Public Property Get This() As Object
Attribute This.VB_Description = "Returns the UI's base interface."
On Error Resume Next
End Property

