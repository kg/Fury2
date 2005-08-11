VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Channel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_booDisposed As Boolean
Private m_sndSound As Sound
Private m_seEngine As SoundEngine
Private m_lngHandle As Long

Public Property Get ChannelGroup() As ChannelGroup
On Error Resume Next
Dim l_lngGroup As Long
    ResultCheck fmodex.FMOD_Channel_GetChannelGroup(m_lngHandle, l_lngGroup)
    Set ChannelGroup = m_seEngine.CreateChannelGroupH(l_lngGroup)
End Property

Public Property Get Disposed() As Boolean
On Error Resume Next
    Disposed = m_booDisposed
End Property

Public Property Get Sound() As Sound
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_GetCurrentSound(m_lngHandle, l_lngValue)
    If l_lngValue = m_sndSound.Handle Then
        Set Sound = m_sndSound
    Else
        Set m_sndSound = Nothing
    End If
End Property

Public Property Get Parent() As SoundEngine
On Error Resume Next
    Set Parent = m_seEngine
End Property

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_lngHandle
End Property

Friend Sub Init(ByVal Parent As SoundEngine, ByVal Handle As Long, ByVal Sound As Sound)
On Error Resume Next
    Set m_seEngine = Parent
    Set m_sndSound = Sound
    m_lngHandle = Handle
End Sub

Friend Sub Free()
On Error Resume Next
    Set m_seEngine = Nothing
    Set m_sndSound = Nothing
    m_lngHandle = 0
    m_booDisposed = True
End Sub

Friend Sub ResultCheck(ByVal result As FMOD_RESULT)
On Error Resume Next
    If result = FMOD_ERR_CHANNEL_STOLEN Then Free
End Sub

Public Property Get Playing() As Boolean
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_IsPlaying(m_lngHandle, l_lngValue)
    Playing = l_lngValue
End Property

Public Property Get Virtual() As Boolean
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_IsVirtual(m_lngHandle, l_lngValue)
    Virtual = l_lngValue
End Property

Public Property Get Muted() As Boolean
On Error Resume Next
Dim l_lngMuted As Long
    ResultCheck fmodex.FMOD_Channel_GetMute(m_lngHandle, l_lngMuted)
    Muted = l_lngMuted
End Property

Public Property Let Muted(ByVal NewValue As Boolean)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetMute(m_lngHandle, NewValue)
End Property

Public Property Get Paused() As Boolean
On Error Resume Next
Dim l_lngPaused As Long
    ResultCheck fmodex.FMOD_Channel_GetPaused(m_lngHandle, l_lngPaused)
    Paused = l_lngPaused
End Property

Public Property Let Paused(ByVal NewValue As Boolean)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetPaused(m_lngHandle, NewValue)
End Property

Public Property Get Audibility() As Single
On Error Resume Next
Dim l_sngValue As Single
    ResultCheck fmodex.FMOD_Channel_GetAudibility(m_lngHandle, l_sngValue)
    Audibility = l_sngValue
End Property

Public Property Get Volume() As Single
On Error Resume Next
Dim l_sngValue As Single
    ResultCheck fmodex.FMOD_Channel_GetVolume(m_lngHandle, l_sngValue)
    Volume = l_sngValue
End Property

Public Property Let Volume(ByVal NewValue As Single)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetVolume(m_lngHandle, NewValue)
End Property

Public Property Get Frequency() As Single
On Error Resume Next
Dim l_sngValue As Single
    ResultCheck fmodex.FMOD_Channel_GetFrequency(m_lngHandle, l_sngValue)
    Frequency = l_sngValue
End Property

Public Property Let Frequency(ByVal NewValue As Single)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetFrequency(m_lngHandle, NewValue)
End Property

Public Property Get Pan() As Single
On Error Resume Next
Dim l_sngValue As Single
    ResultCheck fmodex.FMOD_Channel_GetPan(m_lngHandle, l_sngValue)
    Pan = l_sngValue
End Property

Public Property Let Pan(ByVal NewValue As Single)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetPan(m_lngHandle, NewValue)
End Property

Public Property Get Priority() As Long
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_GetPriority(m_lngHandle, l_lngValue)
    Priority = l_lngValue
End Property

Public Property Let Priority(ByVal NewValue As Long)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetPriority(m_lngHandle, NewValue)
End Property

Public Property Get Position() As Double
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_GetPosition(m_lngHandle, l_lngValue, FMOD_TIMEUNIT_MS)
    Position = CDbl(l_lngValue) / 1000#
End Property

Public Sub Kill()
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_Stop(m_lngHandle)
End Sub
