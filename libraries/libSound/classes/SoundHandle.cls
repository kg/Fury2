VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SoundHandle"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury� Sound Handle"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'   ::fury� sound handle::
'
Option Explicit

Public Enum F2SEHandleTypes
    SoundType_Sample = 0
    SoundType_Stream = 1
    SoundType_Music = 2
End Enum

Private m_Engine As SoundEngine
Private m_strName As String
Private m_HandleType As F2SEHandleTypes
Private m_Handle As Long
Private m_Loaded As Boolean
Private m_Channel As Long

Public Property Get Length() As Single
On Error Resume Next
    Select Case m_HandleType
    Case SoundType_Sample
        Length = FSound_Sample.GetLength(m_Handle) / 1000
    Case SoundType_Stream
        Length = FSound_Stream.GetLength(m_Handle) / 1000
    Case Else
        Length = -1
    End Select
End Property

Public Function Class_ToString() As String
    Class_ToString = "Sound: " & m_strName
End Function

Public Property Get Name() As String
    Name = m_strName
End Property

Friend Property Let Name(NewValue As String)
    m_strName = NewValue
End Property

Public Property Get HandleType() As F2SEHandleTypes
On Error Resume Next
    HandleType = m_HandleType
End Property

Sub Fade(Amount)
On Error Resume Next
    Volume = ClipValue((Volume - CLng(Amount)), 0, 255)
End Sub

Public Property Get Paused() As Boolean
On Error Resume Next
    If m_Loaded = False Then Exit Property
    If m_HandleType = SoundType_Sample Then
        Paused = CBool(FSound_Channel.GetPaused(m_Channel))
    ElseIf m_HandleType = SoundType_Stream Then
        Paused = CBool(FSound_Channel.GetPaused(m_Channel))
    ElseIf m_HandleType = SoundType_Music Then
        Paused = CBool(FMusic.GetPaused(m_Handle))
    End If
End Property

Public Property Let Paused(NewValue As Boolean)
On Error Resume Next
    If m_Loaded = False Then Exit Property
    If m_HandleType = SoundType_Sample Then
        FSound_Channel.SetPaused m_Channel, Abs(CLng(NewValue))
    ElseIf m_HandleType = SoundType_Stream Then
        FSound_Channel.SetPaused m_Channel, Abs(CLng(NewValue))
    ElseIf m_HandleType = SoundType_Music Then
        FMusic.SetPaused m_Handle, Abs(CLng(NewValue))
    End If
End Property

Public Property Let Volume(Optional Channel As Long = -1, ByVal NewVolume As Long)
On Error Resume Next
    If m_Loaded = False Then Exit Property
    If Channel = -1 Then Channel = m_Channel
    If Channel = -1 Then Exit Property
    If NewVolume < 0 Then NewVolume = 0
    If NewVolume > 255 Then NewVolume = 255
    If m_HandleType = SoundType_Sample Then
        FSound_Channel.SetVolume m_Channel, NewVolume
    ElseIf m_HandleType = SoundType_Stream Then
        FSound_Channel.SetVolume m_Channel, NewVolume
    ElseIf m_HandleType = SoundType_Music Then
        FMusic.SetMasterVolume m_Handle, NewVolume
    End If
End Property

Public Property Get Volume(Optional Channel As Long = -1) As Long
On Error Resume Next
    If m_Loaded = False Then Exit Function
    If Channel = -1 Then Channel = m_Channel
    If Channel = -1 Then Exit Property
    If m_HandleType = SoundType_Sample Then
        Volume = FSound_Channel.GetVolume(m_Channel)
    ElseIf m_HandleType = SoundType_Stream Then
        Volume = FSound_Channel.GetVolume(m_Channel)
    ElseIf m_HandleType = SoundType_Music Then
        Volume = FMusic.GetMasterVolume(m_Handle)
    End If
End Property

Public Property Let Pan(Optional Channel As Long = -1, ByVal NewPan As Long)
On Error Resume Next
    If m_Loaded = False Then Exit Property
    If Channel = -1 Then Channel = m_Channel
    If Channel = -1 Then Exit Property
    If NewPan = -1 Then
    Else
        If NewPan < 0 Then NewPan = 0
        If NewPan > 255 Then NewPan = 255
    End If
    If m_HandleType = SoundType_Sample Then
        FSound_Channel.SetPan m_Channel, NewPan
    ElseIf m_HandleType = SoundType_Stream Then
        FSound_Channel.SetPan m_Channel, NewPan
    ElseIf m_HandleType = SoundType_Music Then
    End If
End Property

Public Property Get Pan(Optional Channel As Long = -1) As Long
On Error Resume Next
    If m_Loaded = False Then Exit Property
    If Channel = -1 Then Channel = m_Channel
    If Channel = -1 Then Exit Property
    If m_HandleType = SoundType_Sample Then
        Pan = FSound_Channel.GetPan(m_Channel)
    ElseIf m_HandleType = SoundType_Stream Then
        Pan = FSound_Channel.GetPan(m_Channel)
    ElseIf m_HandleType = SoundType_Music Then
    End If
End Property

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_Handle
End Property

Public Property Get Playing(Optional ByVal Channel As Long = -1) As Boolean
On Error Resume Next
    If m_Loaded = False Then Exit Function
    If Channel = -1 Then Channel = m_Channel
    If Channel = -1 Then Exit Property
    If m_HandleType = SoundType_Sample Then
        Playing = FSound_Channel.IsPlaying(Channel)
    ElseIf m_HandleType = SoundType_Stream Then
        Playing = True
    ElseIf m_HandleType = SoundType_Music Then
        Playing = Not FMusic.IsFinished(m_Handle)
    End If
End Property

Friend Sub SetValues(HandleType As F2SEHandleTypes, Handle As Long, Loaded As Boolean)
On Error Resume Next
    m_HandleType = HandleType
    m_Handle = Handle
    m_Loaded = Loaded
    m_Channel = -1
End Sub

Friend Sub SetEngine(Eng As SoundEngine)
    Set m_Engine = Eng
End Sub

Public Function Play(Optional Channel As Long = -1) As Long
On Error Resume Next
    If m_Loaded = False Then Exit Function
    If m_HandleType = SoundType_Sample Then
        Play = FSound_Channel.PlaySound(Channel, m_Handle)
        m_Channel = Play
        Me.Volume = m_Engine.SampleVolume
    ElseIf m_HandleType = SoundType_Stream Then
        Play = FSound_Stream.Play(Channel, m_Handle)
        m_Channel = Play
        Me.Volume = m_Engine.StreamVolume
    ElseIf m_HandleType = SoundType_Music Then
        Play = FMusic.PlaySong(m_Handle)
        m_Channel = Play
        Me.Volume = m_Engine.MusicVolume
    End If
End Function

Public Function PlayPanned(Optional ByVal Pan As Long = -1, Optional ByVal Volume As Long = 255, Optional Channel As Long = -1) As Long
Dim m_lngHandle As Long
    m_lngHandle = Play(Channel)
    FSound_Channel.SetPan m_lngHandle, Pan
    If m_HandleType = SoundType_Sample Then
        FSound_Channel.SetVolume m_lngHandle, (Volume * m_Engine.SampleVolume) \ 255
    ElseIf m_HandleType = SoundType_Stream Then
        FSound_Channel.SetVolume m_lngHandle, (Volume * m_Engine.StreamVolume) \ 255
    ElseIf m_HandleType = SoundType_Music Then
        FSound_Channel.SetVolume m_lngHandle, (Volume * m_Engine.MusicVolume) \ 255
    End If
    PlayPanned = m_lngHandle
End Function

Public Function PlayAt(x, y, Optional Channel As Long = -1) As Long
Dim m_lngHandle As Long, m_sngPan As Single
Dim m_sngVolume As Single
    m_sngVolume = 255
    m_sngVolume = m_sngVolume - Abs((y - (m_Engine.m_Engine.BufferHeight / 2)) / (CSng(m_Engine.m_Engine.BufferHeight / 2) / 255!))
    m_sngPan = x
    m_sngPan = m_sngPan / (CSng(m_Engine.m_Engine.BufferWidth) / 255!)
    m_lngHandle = Play(Channel)
    FSound_Channel.SetPan m_lngHandle, ClipValue(CLng(m_sngPan), 0, 255)
    FSound_Channel.SetVolume m_lngHandle, ClipValue(CLng(m_sngVolume), 0, 255)
    PlayAt = m_lngHandle
End Function

Public Function PlayEx(Volume, Optional Channel As Long = -1) As Long
On Error Resume Next
    If m_Loaded = False Then Exit Function
    If m_HandleType = SoundType_Sample Then
        PlayEx = FSound_Channel.PlaySound(Channel, m_Handle)
        FSound_Channel.SetVolume PlayEx, CLng(Volume * m_Engine.SampleVolume \ 255)
        m_Channel = PlayEx
    ElseIf m_HandleType = SoundType_Stream Then
        PlayEx = FSound_Stream.Play(Channel, m_Handle)
        FSound_Channel.SetVolume PlayEx, CLng(Volume * m_Engine.StreamVolume \ 255)
        m_Channel = PlayEx
    ElseIf m_HandleType = SoundType_Music Then
        PlayEx = FMusic.PlaySong(m_Handle)
        FMusic.SetMasterVolume m_Handle, CLng(Volume * m_Engine.MusicVolume \ 255)
        m_Channel = PlayEx
    End If
End Function

Public Function Kill(Optional ByVal Channel As Long = -1)
On Error Resume Next
    If m_Loaded = False Then Exit Function
    If Channel = -1 Then Channel = m_Channel
    If Channel = -1 Then Exit Function
    If m_HandleType = SoundType_Sample Then
        Kill = FSound_Channel.StopSound(Channel)
    ElseIf m_HandleType = SoundType_Stream Then
        Kill = FSound_Stream.Stop(m_Handle)
    ElseIf m_HandleType = SoundType_Music Then
        Kill = FMusic.StopSong(m_Handle)
    End If
    m_Channel = -1
End Function

Public Sub Free()
On Error Resume Next
    If m_Loaded = False Then Exit Sub
    If m_HandleType = SoundType_Sample Then
        FSound_Sample.Free m_Handle
        m_Handle = 0
        m_Channel = -1
    ElseIf m_HandleType = SoundType_Stream Then
        FSound_Stream.Close m_Handle
        m_Handle = 0
        m_Channel = -1
    ElseIf m_HandleType = SoundType_Music Then
        FMusic.FreeSong m_Handle
        m_Handle = 0
        m_Channel = -1
    End If
    m_Loaded = False
End Sub

Private Sub Class_Initialize()
On Error Resume Next
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub