VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "GameDebugger"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Hook As Object
Public Engine As Fury2Engine
Private m_engDefault As Fury2Engine

Public Property Get GameEngine() As Object
On Error Resume Next
    Set GameEngine = Engine
End Property

Public Sub LoadGame(ByRef Path As String)
On Error Resume Next
    Set m_engDefault = DefaultEngine
    Fury2Load Path, EM_Normal, Me, Hook, Engine
End Sub

Public Sub Begin()
On Error Resume Next
    Set DefaultEngine = m_engDefault
End Sub
