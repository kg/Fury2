VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IViewportHandler"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Sub Redraw(ByVal Surface As Fury2Image, ByVal ScrollX As Long, ByVal ScrollY As Long)
End Sub

Public Function MouseDown(ByVal Button As Long, ByVal Shift As Long, ByVal X As Single, ByVal Y As Single) As Boolean
End Function

Public Function MouseMove(ByVal Button As Long, ByVal Shift As Long, ByVal X As Single, ByVal Y As Single) As Boolean
End Function

Public Function MouseUp(ByVal Button As Long, ByVal Shift As Long, ByVal X As Single, ByVal Y As Single) As Boolean
End Function

