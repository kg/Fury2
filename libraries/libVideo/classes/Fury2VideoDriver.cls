VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2VideoDriver"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Sub GoFullscreen()
End Sub

Sub GoWindowed()
End Sub

Sub Flip()
End Sub

Sub Init(ByVal XRes As Long, ByVal YRes As Long, ByRef Caption As String, Optional ByVal Windowed As Boolean = False)
End Sub

Sub ResizeWindow(ByVal XSize As Long, ByVal YSize As Long)
End Sub

Sub Shutdown()
End Sub

Function CreateImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
End Function

Sub SetCaption(ByRef Text As String)
End Sub

Sub HookEvents(ByVal Obj As Fury2VideoEventSink)
End Sub

Property Get Window() As Object
End Property

Property Get Closed() As Boolean
End Property

Property Get MouseVisible() As Boolean
End Property

Property Let MouseVisible(ByVal NewValue As Boolean)
End Property

Property Get ScaleRatio() As Single
End Property

Property Let ScaleRatio(ByVal NewValue As Single)
End Property

Property Get ScaleMode() As Long
End Property

Property Let ScaleMode(ByVal NewValue As Long)
End Property

Property Get Fullscreen() As Boolean
End Property

Property Get Backbuffer() As Fury2Image
End Property
