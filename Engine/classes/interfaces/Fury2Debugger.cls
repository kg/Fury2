VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Debugger"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Sub MapSwitch(ByRef NewMap As String)
End Sub

Public Sub EventBound(ByRef NewEvent As Fury2Event)
End Sub

Public Sub Error(Error As String, Details As String)
End Sub

Public Sub CriticalError(Source As String, Location As String, Description As String)
End Sub

Public Function DebugCreateOutputPlugin() As Object
End Function

Public Sub DebugHalted()
End Sub

Public Sub DebugUnhalted()
End Sub

Public Sub DebugOut(ByRef Text As String)
End Sub

Public Sub DebugTextOut(ByRef Text As String)
End Sub

Public Sub DebugSubthreadStart()
End Sub

Public Sub DebugSubthreadEnd()
End Sub

Public Sub DebugGameStart()
End Sub

Public Sub DebugGameEnd()
End Sub

Public Sub DebugFrameStart()
End Sub

Public Sub DebugFrameEnd()
End Sub
