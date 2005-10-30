VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SequenceEvent"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Script As String
Private m_skfKeyframe As Fury2SequenceKeyframe
Private m_booTriggered As Boolean

Friend Sub SetKeyframe(Keyframe As Fury2SequenceKeyframe)
On Error Resume Next
    Set m_skfKeyframe = Keyframe
End Sub

Public Property Get Keyframe() As Fury2SequenceKeyframe
On Error Resume Next
    Set Keyframe = m_skfKeyframe
End Property

Public Sub Initialize()
On Error Resume Next
    m_booTriggered = False
End Sub

Public Sub Reinitialize()
On Error Resume Next
    m_booTriggered = False
End Sub

Public Sub Trigger()
On Error Resume Next
    If m_booTriggered Then
    Else
        m_booTriggered = True
        m_skfKeyframe.Sequence.Engine.RunCode Script
    End If
End Sub

Public Property Get Triggered() As Boolean
On Error Resume Next
    Triggered = m_booTriggered
End Property

Public Sub Update(ByVal Progress As Double)
On Error Resume Next
    If Progress >= 1 Then
        Trigger
    End If
End Sub

Private Sub Class_Initialize()
On Error Resume Next
End Sub

