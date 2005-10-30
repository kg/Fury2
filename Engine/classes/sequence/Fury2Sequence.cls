VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Sequence"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements Fury2Object
Implements Fury2Plugin
Public Elements As Fury2Collection
Public Keyframes As Fury2Collection
Public Time As Fury2Counter
Public Playing As Boolean
Public Filename As String
Private m_skfPreviousKeyframe As Fury2SequenceKeyframe
Private m_skfCurrentKeyframe As Fury2SequenceKeyframe
Private m_skfNextKeyframe As Fury2SequenceKeyframe
Private m_engEngine As Fury2Engine

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Sequence: " & Filename
End Function

Public Function AddKeyframe(Optional ByVal Time As Double = 0) As Fury2SequenceKeyframe
On Error Resume Next
Dim l_lngPos As Long, l_lngIndex As Long
Dim l_skfKeyframe As Fury2SequenceKeyframe
    Set AddKeyframe = New Fury2SequenceKeyframe
    AddKeyframe.SetSequence Me
    AddKeyframe.Time = Time
    l_lngPos = -1
    l_lngIndex = 1
    For Each l_skfKeyframe In Keyframes
        If l_skfKeyframe.Time < Time Then
            l_lngPos = l_lngIndex + 1
        ElseIf l_skfKeyframe.Time > Time Then
            l_lngPos = l_lngIndex
            Exit For
        End If
    Next l_skfKeyframe
    Keyframes.Add AddKeyframe, , l_lngPos
End Function

Public Function AddElement(ByRef ID As String) As Fury2SequenceElement
On Error Resume Next
    Set AddElement = New Fury2SequenceElement
    AddElement.ID = ID
    AddElement.SetSequence Me
    Elements.Add AddElement, ID
End Function

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
    If Elements Is Nothing Then Set Elements = m_engEngine.F2Collection()
    If Keyframes Is Nothing Then Set Keyframes = m_engEngine.F2Collection()
End Sub

Public Property Get Engine() As Fury2Engine
On Error Resume Next
    Set Engine = m_engEngine
End Property

Public Sub Play(Optional ByVal Speed As Double = 1#)
On Error Resume Next
    Set Time = m_engEngine.F2Counter()
    Playing = True
    Time.Start Speed
    Initialize
End Sub

Public Sub Pause()
On Error Resume Next
    Time.Paused = True
    Playing = False
End Sub

Public Sub Initialize()
On Error Resume Next
Dim l_selElement As Fury2SequenceElement
Dim l_skfKeyframe As Fury2SequenceKeyframe
    Debug.Print "Sequence::Initialize"
    For Each l_skfKeyframe In Keyframes
        l_skfKeyframe.SetSequence Me
        l_skfKeyframe.Initialize
    Next l_skfKeyframe
    Set m_skfPreviousKeyframe = Nothing
    Set m_skfCurrentKeyframe = Nothing
    Set m_skfNextKeyframe = Keyframes(1)
    m_skfNextKeyframe.Reinitialize
    For Each l_selElement In Elements
        l_selElement.SetSequence Me
        l_selElement.Initialize
    Next l_selElement
End Sub

Friend Sub NextKeyframe()
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
    Debug.Print "Sequence::NextKeyframe"
    Set m_skfPreviousKeyframe = m_skfCurrentKeyframe
    Set m_skfCurrentKeyframe = m_skfNextKeyframe
    Set m_skfNextKeyframe = Nothing
    For Each l_skfKeyframe In Keyframes
        If l_skfKeyframe.Time > m_skfCurrentKeyframe.Time Then
            Set m_skfNextKeyframe = l_skfKeyframe
            Exit For
        End If
    Next l_skfKeyframe
    If m_skfNextKeyframe Is Nothing Then
        Pause
    Else
        m_skfNextKeyframe.Reinitialize
    End If
End Sub

Public Sub Update(Optional ByVal ManualTime As Double = -1)
On Error Resume Next
Dim l_dblTime As Double
Dim l_dblCurrentTime As Double, l_dblNextTime As Double
    If ManualTime < 0 Then
        l_dblTime = Time.Value
    Else
        l_dblTime = ManualTime
    End If
    l_dblCurrentTime = 0
    l_dblCurrentTime = m_skfCurrentKeyframe.Time
    l_dblNextTime = 999999999
    l_dblNextTime = m_skfNextKeyframe.Time
    If l_dblTime < l_dblCurrentTime Then
        ' Backwards seeking not implemented yet
    ElseIf l_dblTime >= l_dblNextTime Then
        Do While l_dblTime >= l_dblNextTime
            m_skfNextKeyframe.Update 1
            NextKeyframe
            l_dblNextTime = 999999999
            l_dblNextTime = m_skfNextKeyframe.Time
        Loop
    End If
    m_skfNextKeyframe.Update (l_dblTime - l_dblCurrentTime) / (l_dblNextTime - l_dblCurrentTime)
End Sub

Private Sub Fury2Plugin_ControlActivate(Control As Fury2InputControl)
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Fury2InputControl)
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
End Property

Private Property Set Fury2Plugin_Engine(ByVal RHS As Fury2Engine)
End Property

Private Property Get Fury2Plugin_Engine() As Fury2Engine
End Property

Private Sub Fury2Plugin_Init()
End Sub

Private Sub Fury2Plugin_KeyDown(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_KeyPress(KeyAscii As Long)
End Sub

Private Sub Fury2Plugin_KeyUp(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_MapLoad(Filename As String)
End Sub

Private Sub Fury2Plugin_MouseDown(ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
End Sub

Private Sub Fury2Plugin_OutputPluginChanged()
End Sub

Private Sub Fury2Plugin_Redraw()
End Sub

Private Sub Fury2Plugin_Shutdown()
End Sub

Private Sub Fury2Plugin_Startup()
End Sub

Private Sub Fury2Plugin_SyncUpdate()
End Sub

Private Sub Fury2Plugin_Unload()
End Sub

Private Sub Fury2Plugin_Update()
On Error Resume Next
    Me.Update
End Sub
