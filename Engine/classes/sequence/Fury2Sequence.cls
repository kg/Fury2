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
Implements IVirtualFileSaveable
Implements IInspectable
Public Elements As Fury2Collection
Public Keyframes As Fury2Collection
Public Variables As Fury2Collection
Public Filename As String
Public PlaybackStart As String
Attribute PlaybackStart.VB_VarDescription = "The keyframe executed before beginning playback."
Public OnPlaybackStart As Fury2Event
Public PlaybackHalt As String
Attribute PlaybackHalt.VB_VarDescription = "The keyframe executed before halting playback."
Public OnPlaybackHalt As Fury2Event
Public Looping As Boolean
Attribute Looping.VB_VarDescription = "If True, this sequence will play infinitely."
Public AutoHook As Boolean
Private m_booPlaying As Boolean
Private m_cntTime As Fury2Counter
Private m_dblInterval As Double
Private m_booPaused As Boolean
Private m_skfPreviousKeyframe As Fury2SequenceKeyframe
Private m_skfCurrentKeyframe As Fury2SequenceKeyframe
Private m_skfNextKeyframe As Fury2SequenceKeyframe
Private m_engEngine As Fury2Engine

Public Property Get PreviousKeyframe() As Fury2SequenceKeyframe
On Error Resume Next
    Set PreviousKeyframe = m_skfPreviousKeyframe
End Property

Public Property Get CurrentKeyframe() As Fury2SequenceKeyframe
On Error Resume Next
    Set CurrentKeyframe = m_skfCurrentKeyframe
End Property

Public Property Get NextKeyframe() As Fury2SequenceKeyframe
On Error Resume Next
    Set NextKeyframe = m_skfNextKeyframe
End Property

Public Property Get X() As Single
On Error Resume Next
    X = CSng(Variable("X"))
End Property

Public Property Let X(ByVal NewValue As Single)
On Error Resume Next
    SetVariable "X", NewValue
End Property

Public Property Get Y() As Single
On Error Resume Next
    Y = CSng(Variable("Y"))
End Property

Public Property Let Y(ByVal NewValue As Single)
On Error Resume Next
    SetVariable "Y", NewValue
End Property

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Sequence: " & Filename
End Function

Public Sub Free()
On Error Resume Next
Dim l_kfKeyframe As Fury2SequenceKeyframe
Dim l_eleElement As Fury2SequenceElement
    If Not (Keyframes Is Nothing) Then
        For Each l_kfKeyframe In Keyframes
            l_kfKeyframe.Free
        Next l_kfKeyframe
        Keyframes.Clear
        Set Keyframes = Nothing
    End If
    If Not (Elements Is Nothing) Then
        For Each l_eleElement In Elements
            l_eleElement.Free
        Next l_eleElement
        Elements.Clear
        Set Elements = Nothing
    End If
    Set m_skfPreviousKeyframe = Nothing
    Set m_skfCurrentKeyframe = Nothing
    Set m_skfNextKeyframe = Nothing
    Set m_cntTime = Nothing
    Set m_engEngine = Nothing
    m_booPlaying = False
End Sub

Public Property Get Variable(ByVal Key As String) As Variant
On Error Resume Next
    If VarType(Variables(Key)) = vbObject Then
        Set Variable = Variables(Key)
    Else
        Variable = Variables(Key)
    End If
    Err.Clear
End Property

Public Property Get Item(ByVal ElementNameOrTime) As Object
Attribute Item.VB_UserMemId = 0
Attribute Item.VB_MemberFlags = "200"
On Error Resume Next
    If VarType(ElementNameOrTime) = vbString Then
    Dim l_strKey As String
        l_strKey = CStr(ElementNameOrTime)
        Set Item = Elements(l_strKey)
    Else
        Set Item = GetKeyframe(CDbl(ElementNameOrTime))
    End If
End Property

Public Function Duplicate() As Fury2Sequence
On Error Resume Next
Dim l_seqNew As Fury2Sequence
Dim l_eleElement As Fury2SequenceElement
Dim l_kfKeyframe As Fury2SequenceKeyframe
    Set l_seqNew = New Fury2Sequence
    With l_seqNew
        .Filename = Filename
        .SetEngine m_engEngine
        For Each l_eleElement In Elements
            .AddElement l_eleElement.Duplicate
        Next l_eleElement
        For Each l_kfKeyframe In Keyframes
            .InsertKeyframe l_kfKeyframe.Duplicate
        Next l_kfKeyframe
        .PlaybackStart = PlaybackStart
        .PlaybackHalt = PlaybackHalt
        .AutoHook = AutoHook
    End With
    Set Duplicate = l_seqNew
End Function

Public Function AddNewKeyframe(Optional ByVal Time As Double = 0) As Fury2SequenceKeyframe
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
    For Each l_skfKeyframe In Keyframes
        If l_skfKeyframe.Time = Time Then
            Set AddNewKeyframe = l_skfKeyframe
            Exit Function
        End If
    Next l_skfKeyframe
    Set l_skfKeyframe = New Fury2SequenceKeyframe
    l_skfKeyframe.SetSequence Me
    l_skfKeyframe.SetTime Time
    InsertKeyframe l_skfKeyframe
    Set AddNewKeyframe = l_skfKeyframe
End Function

Public Function GetKeyframeIndex(ByVal Time As Double) As Long
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
Dim l_lngIndex As Long
    l_lngIndex = 1
    For Each l_skfKeyframe In Keyframes
        If l_skfKeyframe.Time >= Time Then
            GetKeyframeIndex = l_lngIndex
            Exit Function
        End If
        l_lngIndex = l_lngIndex + 1
    Next l_skfKeyframe
End Function

Public Function GetKeyframe(ByVal Time As Double) As Fury2SequenceKeyframe
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
    For Each l_skfKeyframe In Keyframes
        If l_skfKeyframe.Time >= Time Then
            Set GetKeyframe = l_skfKeyframe
            Exit Function
        End If
    Next l_skfKeyframe
End Function

Public Function GetKeyframeByTag(ByVal Value As String) As Fury2SequenceKeyframe
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
    For Each l_skfKeyframe In Keyframes
        If LCase(Trim(l_skfKeyframe.Tag)) = LCase(Trim(Value)) Then
            Set GetKeyframeByTag = l_skfKeyframe
            Exit Function
        End If
    Next l_skfKeyframe
End Function

Public Sub InsertKeyframe(ByVal Keyframe As Fury2SequenceKeyframe)
On Error Resume Next
Dim l_lngPos As Long, l_lngIndex As Long
Dim l_skfKeyframe As Fury2SequenceKeyframe
    l_lngPos = -1
    l_lngIndex = 1
    For Each l_skfKeyframe In Keyframes
        If l_skfKeyframe.Time < Keyframe.Time Then
            l_lngPos = l_lngIndex + 1
        ElseIf l_skfKeyframe.Time > Keyframe.Time Then
            l_lngPos = l_lngIndex
            Exit For
        End If
        l_lngIndex = l_lngIndex + 1
    Next l_skfKeyframe
    Keyframes.Add Keyframe, , l_lngPos
End Sub

Public Function AddNewElement(ByRef ID As String) As Fury2SequenceElement
On Error Resume Next
    If Elements.Contains(ID) Then
        Set AddNewElement = Elements(ID)
        Exit Function
    End If
    Set AddNewElement = New Fury2SequenceElement
    AddNewElement.ID = ID
    AddNewElement.SetSequence Me
    Elements.Add AddNewElement, ID
End Function

Public Property Get Playing() As Boolean
On Error Resume Next
    Playing = m_booPlaying
End Property

Public Property Get Time() As Double
On Error Resume Next
    Time = m_cntTime.Value
End Property

Public Property Let Time(ByVal NewTime As Double)
On Error Resume Next
    m_cntTime.Offset = NewTime
    m_cntTime.Restart
End Property

Public Sub SetVariable(ByVal Name As String, ByRef Value)
On Error Resume Next
    Variables.Remove Name
    Variables.Add Value, Name
End Sub

Public Sub AddElement(ByVal Element As Fury2SequenceElement)
On Error Resume Next
    If Elements.Contains(Element.ID) Then Elements.Remove Element.ID
    Element.SetSequence Me
    Elements.Add Element, Element.ID
End Sub

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
    If Elements Is Nothing Then Set Elements = m_engEngine.F2Collection()
    If Keyframes Is Nothing Then Set Keyframes = m_engEngine.F2Collection()
    If Variables Is Nothing Then Set Variables = m_engEngine.F2Collection()
End Sub

Public Property Get Length() As Double
On Error Resume Next
    If Keyframes.Count > 0 Then
        Length = Keyframes(Keyframes.Count).Time
    End If
End Property

Public Property Get Engine() As Fury2Engine
Attribute Engine.VB_Description = "*"
On Error Resume Next
    Set Engine = m_engEngine
End Property

Public Sub PlayAt(ByVal X As Single, ByVal Y As Single, Optional ByVal Speed As Double = 1#)
On Error Resume Next
    SetVariable "X", X
    SetVariable "Y", Y
    Play Speed
End Sub

Public Sub Play(Optional ByVal Speed As Double = 1#)
On Error Resume Next
Dim l_seqMe As Fury2Sequence
    If m_booPlaying Then Halt
    If AutoHook Then
        m_engEngine.HookEvents Me
    End If
    If Speed < 0 Then Speed = 0
    Set m_cntTime = m_engEngine.F2Counter()
    m_booPaused = False
    m_booPlaying = True
    m_dblInterval = Speed
    If Speed = 0 Then
        m_cntTime.Start 1
        m_cntTime.Reset
    Else
        m_cntTime.Start 1 / Speed
    End If
    Initialize
    Set l_seqMe = Me
    m_engEngine.CompileScriptBlock(PlaybackStart, "Sequence").Invoke l_seqMe
    OnPlaybackStart.Invoke
    BeginPlayback
End Sub

Public Sub Halt()
On Error Resume Next
Dim l_selElement As Fury2SequenceElement
Dim l_seqMe As Fury2Sequence
    m_cntTime.Paused = True
    m_booPlaying = False
    m_booPaused = False
    Set l_seqMe = Me
    m_engEngine.CompileScriptBlock(PlaybackHalt, "Sequence").Invoke l_seqMe
    OnPlaybackHalt.Invoke
    If AutoHook Then
        m_engEngine.UnHookEvents Me
    End If
End Sub

Public Sub Unpause()
On Error Resume Next
    If m_booPaused Then
        m_cntTime.Paused = False
        m_booPlaying = True
        m_booPaused = False
    End If
End Sub

Public Property Get Paused() As Boolean
On Error Resume Next
    Paused = m_cntTime.Paused
End Property

Public Sub Pause()
On Error Resume Next
    If m_booPlaying Then
        m_cntTime.Paused = True
        m_booPlaying = False
        m_booPaused = True
    End If
End Sub

Public Sub Rewind()
On Error Resume Next
    Play m_dblInterval
End Sub

Friend Sub BeginPlayback()
On Error Resume Next
    Set m_skfPreviousKeyframe = Nothing
    Set m_skfCurrentKeyframe = Nothing
    Set m_skfNextKeyframe = Keyframes(1)
    m_skfNextKeyframe.Reinitialize
End Sub

Public Sub Initialize()
On Error Resume Next
Dim l_selElement As Fury2SequenceElement
Dim l_skfKeyframe As Fury2SequenceKeyframe
    For Each l_selElement In Elements
        l_selElement.SetSequence Me
        l_selElement.Initialize
    Next l_selElement
    For Each l_skfKeyframe In Keyframes
        l_skfKeyframe.SetSequence Me
        l_skfKeyframe.Initialize
    Next l_skfKeyframe
End Sub

Friend Function GoToPreviousKeyframe(Time As Double) As Boolean
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
Dim l_lngKeyframe As Long
Dim l_dblCurrentTime As Double
'    Debug.Print "GoToPreviousKeyframe"
    Set m_skfNextKeyframe = m_skfCurrentKeyframe
    Set m_skfCurrentKeyframe = m_skfPreviousKeyframe
    Set m_skfPreviousKeyframe = Nothing
    l_dblCurrentTime = Time
    l_dblCurrentTime = m_skfCurrentKeyframe.Time
    For l_lngKeyframe = Keyframes.Count To 1 Step -1
        Set l_skfKeyframe = Keyframes(l_lngKeyframe)
        If l_skfKeyframe.Time < l_dblCurrentTime Then
            Set m_skfPreviousKeyframe = l_skfKeyframe
            Exit For
        End If
    Next l_lngKeyframe
    If m_skfPreviousKeyframe Is Nothing Then
        GoToPreviousKeyframe = Looping
        ' Halt
    Else
        ' m_skfPreviousKeyframe.Reinitialize
    End If
End Function

Friend Function GoToNextKeyframe(Time As Double) As Boolean
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
Dim l_dblCurrentTime As Double
'    Debug.Print "GoToNextKeyframe"
    Set m_skfPreviousKeyframe = m_skfCurrentKeyframe
    Set m_skfCurrentKeyframe = m_skfNextKeyframe
    Set m_skfNextKeyframe = Nothing
    l_dblCurrentTime = Time
    l_dblCurrentTime = m_skfCurrentKeyframe.Time
    For Each l_skfKeyframe In Keyframes
        If l_skfKeyframe.Time > l_dblCurrentTime Then
            Set m_skfNextKeyframe = l_skfKeyframe
            Exit For
        End If
    Next l_skfKeyframe
    If m_skfNextKeyframe Is Nothing Then
        GoToNextKeyframe = Looping
        Halt
    Else
        m_skfNextKeyframe.Reinitialize
    End If
End Function

Public Sub Update(Optional ByVal ManualTime As Double = -1)
On Error Resume Next
Dim l_dblTime As Double
Dim l_dblCurrentTime As Double, l_dblNextTime As Double, l_dblPreviousTime As Double
Dim l_booLooped As Boolean
Dim l_sngLength As Single
    If Not Playing Then Exit Sub
    l_sngLength = Length
    If ManualTime < 0 Then
        l_dblTime = m_cntTime.Value
    Else
        l_dblTime = ManualTime
    End If
    If (Looping) Then
        If l_dblTime > l_sngLength Then
            l_dblTime = l_dblTime - (Floor(l_dblTime / l_sngLength) * l_sngLength)
        End If
    End If
    l_dblCurrentTime = 0
    l_dblCurrentTime = m_skfCurrentKeyframe.Time
    l_dblPreviousTime = -999999999
    l_dblPreviousTime = m_skfPreviousKeyframe.Time
    l_dblNextTime = 999999999
    l_dblNextTime = m_skfNextKeyframe.Time
    If l_dblTime < l_dblCurrentTime Then
        Do While l_dblTime < l_dblCurrentTime
            m_skfPreviousKeyframe.Update 1
            If GoToPreviousKeyframe(l_dblTime) Then
                l_booLooped = True
                Exit Do
            End If
            If m_booPlaying = False Then Exit Do
            l_dblCurrentTime = 0
            l_dblCurrentTime = m_skfCurrentKeyframe.Time
'            l_dblPreviousTime = -999999999
'            l_dblPreviousTime = m_skfPreviousKeyframe.Time
        Loop
    ElseIf l_dblTime > l_dblNextTime Then
        Do While l_dblTime > l_dblNextTime
            m_skfNextKeyframe.Update 1
            If GoToNextKeyframe(l_dblTime) Then
                l_booLooped = True
                Exit Do
            End If
            If m_booPlaying = False Then Exit Do
            l_dblNextTime = 999999999
            l_dblNextTime = m_skfNextKeyframe.Time
        Loop
    End If
    l_dblPreviousTime = -999999999
    l_dblPreviousTime = m_skfPreviousKeyframe.Time
    l_dblNextTime = 999999999
    l_dblNextTime = m_skfNextKeyframe.Time
    l_dblCurrentTime = 0
    l_dblCurrentTime = m_skfCurrentKeyframe.Time
    m_skfNextKeyframe.Update (l_dblTime - l_dblCurrentTime) / (l_dblNextTime - l_dblCurrentTime)
End Sub

Private Sub Class_Initialize()
On Error Resume Next
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
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
    If m_booPlaying Then Me.Update
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngCount As Long, l_lngItems As Long
Dim l_eleElement As Fury2SequenceElement, l_kfKeyframe As Fury2SequenceKeyframe
    Halt
    With File
        .ReadSegment_Begin
            .Load Looping
        .ReadSegment_End
        .ReadSegment_Begin
            Elements.Clear
            .Load l_lngCount
            If l_lngCount > 0 Then
                For l_lngItems = 1 To l_lngCount
                    Set l_eleElement = New Fury2SequenceElement
                    l_eleElement.SetSequence Me
                    .Load l_eleElement
                    Elements.Add l_eleElement, l_eleElement.ID
                Next l_lngItems
            End If
        .ReadSegment_End
        .ReadSegment_Begin
            Keyframes.Clear
            .Load l_lngCount
            If l_lngCount > 0 Then
                For l_lngItems = 1 To l_lngCount
                    Set l_kfKeyframe = New Fury2SequenceKeyframe
                    l_kfKeyframe.SetSequence Me
                    .Load l_kfKeyframe
                    InsertKeyframe l_kfKeyframe
                Next l_lngItems
            End If
        .ReadSegment_End
        .ReadSegment_Begin
            .Load PlaybackStart
            .Load PlaybackHalt
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_eleElement As Fury2SequenceElement, l_kfKeyframe As Fury2SequenceKeyframe
    With File
        .WriteSegment_Begin
            .Save Looping
        .WriteSegment_End
        .WriteSegment_Begin
            .Save Elements.Count
            For Each l_eleElement In Elements
                .Save l_eleElement
            Next l_eleElement
        .WriteSegment_End
        .WriteSegment_Begin
            .Save Keyframes.Count
            For Each l_kfKeyframe In Keyframes
                .Save l_kfKeyframe
            Next l_kfKeyframe
        .WriteSegment_End
        .WriteSegment_Begin
            .Save PlaybackStart
            .Save PlaybackHalt
        .WriteSegment_End
    End With
End Sub

Function Load(ByVal Filename As String) As Boolean
On Error Resume Next
Dim l_strExtension As String
    Filename = Engine.Filesystem.AutoSelectExtension(Filename, "f2seq")
    If Engine.Filesystem.FileExists(Filename) Then
    Else
        Exit Function
    End If
    Load = Engine.LoadFromFile(Me, Engine.Filesystem.File(Filename))
End Function

Function Save(ByVal Filename As String) As Boolean
On Error Resume Next
    Save = Engine.SaveToFile(Me, Engine.Filesystem.CreateFile(Filename))
End Function

