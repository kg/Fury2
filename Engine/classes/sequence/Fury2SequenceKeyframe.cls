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
Implements Fury2Object
Implements IVirtualFileSaveable
Implements IInspectable
Public Tweens As Fury2Collection
Public Events As Fury2Collection
Private m_dblTime As Double
Private m_seqSequence As WeakReference

Public Property Get Time() As Double
On Error Resume Next
    Time = m_dblTime
End Property

Friend Sub SetTime(ByVal NewTime As Double)
On Error Resume Next
    m_dblTime = NewTime
End Sub

Public Property Let Time(ByVal NewTime As Double)
On Error Resume Next
    If NewTime < 0 Then NewTime = 0
    If NewTime <> m_dblTime Then
        m_dblTime = NewTime
        Sequence.Keyframes.Remove Me
        Sequence.InsertKeyframe Me
        Sequence.Rewind
    End If
End Property

Public Function Duplicate() As Fury2SequenceKeyframe
On Error Resume Next
Dim l_kfNew As Fury2SequenceKeyframe
Dim l_twTween As Fury2SequenceTween
Dim l_evtEvent As Fury2SequenceEvent
    Set l_kfNew = New Fury2SequenceKeyframe
    With l_kfNew
        .SetTime Time
        Set .Tweens = Sequence.Engine.F2Collection()
        Set .Events = Sequence.Engine.F2Collection()
        For Each l_twTween In Tweens
            .AddTween l_twTween.Duplicate
        Next l_twTween
        For Each l_evtEvent In Events
            .AddEvent l_evtEvent.Duplicate
        Next l_evtEvent
    End With
    Set Duplicate = l_kfNew
End Function

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Keyframe: " & Time
End Function

Public Sub Free()
On Error Resume Next
Dim l_twnTween As Fury2SequenceTween
Dim l_evtEvent As Fury2SequenceEvent
    If Not (Tweens Is Nothing) Then
        For Each l_twnTween In Tweens
            l_twnTween.Free
        Next l_twnTween
        Tweens.Clear
        Set Tweens = Nothing
    End If
    If Not (Events Is Nothing) Then
        For Each l_evtEvent In Events
            l_evtEvent.Free
        Next l_evtEvent
        Events.Clear
        Set Events = Nothing
    End If
    WRFree m_seqSequence
End Sub

Public Property Get PreviousKeyframe() As Fury2SequenceKeyframe
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
Dim l_skfPrevious As Fury2SequenceKeyframe
    Set l_skfPrevious = Sequence.Keyframes(1)
    For Each l_skfKeyframe In Sequence.Keyframes
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

Public Sub AddTween(ByVal TheTween As Fury2SequenceTween)
On Error Resume Next
    TheTween.SetKeyframe Me
    Tweens.Add TheTween
End Sub

Public Sub AddEvent(ByVal TheEvent As Fury2SequenceEvent)
On Error Resume Next
    TheEvent.SetKeyframe Me
    Events.Add TheEvent
End Sub

Public Function AddNewTween(Optional ByRef Property As String = "", Optional ByVal Value = -32767, Optional ByVal ValueType As Fury2TweenTypes = F2TT_Number, Optional ByVal Variable As String = "") As Fury2SequenceTween
On Error Resume Next
    Set AddNewTween = New Fury2SequenceTween
    AddNewTween.SetKeyframe Me
    If InStr(Property, ".") Then
        AddNewTween.ElementID = Split(Property, ".")(0)
        AddNewTween.PropertyName = Split(Property, ".")(1)
    Else
        AddNewTween.ElementID = Property
    End If
    AddNewTween.ValueType = ValueType
    If Value <> -32767 Then AddNewTween.Value = Value
    If Len(Variable) > 0 Then AddNewTween.Variable = Variable
    Tweens.Add AddNewTween
End Function

Public Function AddNewEvent(Optional ByVal Script As String = "") As Fury2SequenceEvent
On Error Resume Next
    Set AddNewEvent = New Fury2SequenceEvent
    AddNewEvent.SetKeyframe Me
    AddNewEvent.Script = Script
    Events.Add AddNewEvent
End Function

Friend Sub SetSequence(Sequence As Fury2Sequence)
On Error Resume Next
    WRSet m_seqSequence, Sequence
    If Tweens Is Nothing Then Set Tweens = Sequence.Engine.F2Collection()
    If Events Is Nothing Then Set Events = Sequence.Engine.F2Collection()
End Sub

Public Property Get Sequence() As Fury2Sequence
On Error Resume Next
    Set Sequence = WRGetSequence(m_seqSequence)
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
            l_sevEvent.Trigger
        Next l_sevEvent
        Err.Clear
    End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngCount As Long, l_lngItems As Long
Dim l_twTween As Fury2SequenceTween, l_evtEvent As Fury2SequenceEvent
    With File
        .Load m_dblTime
        .ReadSegment_Begin
            Tweens.Clear
            .Load l_lngCount
            If l_lngCount > 0 Then
                For l_lngItems = 1 To l_lngCount
                    Set l_twTween = New Fury2SequenceTween
                    l_twTween.SetKeyframe Me
                    .Load l_twTween
                    Tweens.Add l_twTween
                Next l_lngItems
            End If
        .ReadSegment_End
        .ReadSegment_Begin
            Events.Clear
            .Load l_lngCount
            If l_lngCount > 0 Then
                For l_lngItems = 1 To l_lngCount
                    Set l_evtEvent = New Fury2SequenceEvent
                    l_evtEvent.SetKeyframe Me
                    .Load l_evtEvent
                    Events.Add l_evtEvent
                Next l_lngItems
            End If
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_twTween As Fury2SequenceTween, l_evtEvent As Fury2SequenceEvent
    With File
        .Save m_dblTime
        .WriteSegment_Begin
            .Save Tweens.Count
            For Each l_twTween In Tweens
                .Save l_twTween
            Next l_twTween
        .WriteSegment_End
        .WriteSegment_Begin
            .Save Events.Count
            For Each l_evtEvent In Events
                .Save l_evtEvent
            Next l_evtEvent
        .WriteSegment_End
    End With
End Sub

