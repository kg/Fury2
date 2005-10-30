VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SequenceKeyframe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Tweens As Fury2Collection
Public Events As Fury2Collection
Public Time As Double
Private m_seqSequence As Fury2Sequence

Public Property Get PreviousKeyframe() As Fury2SequenceKeyframe
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
Dim l_skfPrevious As Fury2SequenceKeyframe
    Set l_skfPrevious = m_seqSequence.Keyframes(1)
    For Each l_skfKeyframe In m_seqSequence.Keyframes
        If l_skfKeyframe Is Me Then
        Else
            If l_skfKeyframe.Time > l_skfPrevious.Time Then
                If l_skfKeyframe.Time < Me.Time Then
                    Set l_skfPrevious = l_skfKeyframe
                End If
            End If
        End If
    Next l_skfKeyframe
    If l_skfPrevious Is Me Then
        Set l_skfPrevious = Nothing
    End If
    Set PreviousKeyframe = l_skfPrevious
End Property

Public Function AddTween(Optional ByRef ElementID As String = "") As Fury2SequenceTween
On Error Resume Next
    Set AddTween = New Fury2SequenceTween
    AddTween.SetKeyframe Me
    AddTween.ElementID = ElementID
    Tweens.Add AddTween
End Function

Public Function AddEvent() As Fury2SequenceEvent
On Error Resume Next
    Set AddEvent = New Fury2SequenceEvent
'    CreateEvent.SetKeyframe Me
    Events.Add AddEvent
End Function

Friend Sub SetSequence(Sequence As Fury2Sequence)
On Error Resume Next
    Set m_seqSequence = Sequence
    If Tweens Is Nothing Then Set Tweens = m_seqSequence.Engine.F2Collection()
    If Events Is Nothing Then Set Events = m_seqSequence.Engine.F2Collection()
End Sub

Public Property Get Sequence() As Fury2Sequence
On Error Resume Next
    Set Sequence = m_seqSequence
End Property

Public Sub Initialize()
On Error Resume Next
Dim l_sevEvent As Fury2SequenceEvent
Dim l_stwTween As Fury2SequenceTween
    For Each l_stwTween In Tweens
        l_stwTween.SetKeyframe Me
        l_stwTween.Initialize
    Next l_stwTween
    For Each l_sevEvent In Events
        l_sevEvent.SetKeyframe Me
        l_sevEvent.Initialize
    Next l_sevEvent
End Sub

Public Sub Reinitialize()
On Error Resume Next
Dim l_sevEvent As Fury2SequenceEvent
Dim l_stwTween As Fury2SequenceTween
    For Each l_stwTween In Tweens
        l_stwTween.Reinitialize
    Next l_stwTween
    For Each l_sevEvent In Events
        l_sevEvent.Reinitialize
    Next l_sevEvent
End Sub

Public Sub Update(ByVal Progress As Double)
On Error Resume Next
Dim l_stwTween As Fury2SequenceTween
Dim l_sevEvent As Fury2SequenceEvent
    If Progress < 0 Then
        Progress = 0
    ElseIf Progress > 1 Then
        Progress = 1
    End If
    For Each l_stwTween In Tweens
        l_stwTween.Update Progress
    Next l_stwTween
    If Progress = 1 Then
        For Each l_sevEvent In Events
            'l_sevEvent.Trigger
        Next l_sevEvent
    End If
End Sub

