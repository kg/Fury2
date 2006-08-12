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
Implements Fury2Object
Implements IVirtualFileSaveable
Implements IInspectable
Private m_strScript As String
Private m_evtScript As Fury2Event
Private m_skfKeyframe As WeakReference
Private m_booTriggered As Boolean

Public Property Get Script() As String
    Script = m_strScript
End Property

Public Property Let Script(ByRef NewScript As String)
On Error Resume Next
    If NewScript <> m_strScript Then
        m_strScript = NewScript
        Set m_evtScript = Nothing
    End If
End Property

Public Function Duplicate() As Fury2SequenceEvent
On Error Resume Next
Dim l_evtNew As Fury2SequenceEvent
    Set l_evtNew = New Fury2SequenceEvent
    With l_evtNew
        .Script = Script
    End With
    Set Duplicate = l_evtNew
End Function

Public Sub Free()
On Error Resume Next
    WRFree m_skfKeyframe
    Set m_evtScript = Nothing
End Sub

Friend Sub SetKeyframe(Keyframe As Fury2SequenceKeyframe)
On Error Resume Next
    WRSet m_skfKeyframe, Keyframe
End Sub

Public Property Get Keyframe() As Fury2SequenceKeyframe
Attribute Keyframe.VB_Description = "*"
On Error Resume Next
    Set Keyframe = WRGetKeyframe(m_skfKeyframe)
End Property

Public Sub Initialize()
On Error Resume Next
    m_booTriggered = False
    Set m_evtScript = Keyframe.Sequence.Engine.CompileScriptBlock(Script, "Sequence, Keyframe")
End Sub

Public Sub Reinitialize()
On Error Resume Next
    m_booTriggered = False
    Set m_evtScript = Keyframe.Sequence.Engine.CompileScriptBlock(Script, "Sequence, Keyframe")
End Sub

Public Sub Trigger()
On Error Resume Next
    If m_booTriggered Then
    Else
        m_booTriggered = True
        m_evtScript.Invoke Keyframe.Sequence, Keyframe
        Err.Clear
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

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load m_strScript
    End With
    Set m_evtScript = Nothing
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save m_strScript
    End With
End Sub


