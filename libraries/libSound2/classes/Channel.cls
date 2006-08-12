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
Public StopEvent As Object
Public BeforeEndEvent As Object
Public EndEvent As Object
Public StartEvent As Object
Private m_booDisposed As Boolean
Private m_sndSound As Sound
Private m_seEngine As SoundEngine
Private m_lngHandle As Long

Friend Sub FireStartCallback()
On Error Resume Next
    Debug.Print "Channel.FireStartCallback"
    StartEvent.Invoke
    m_sndSound.StartEvent.Invoke
End Sub

Friend Sub FireBeforeEndCallback()
On Error Resume Next
    Debug.Print "Channel.FireBeforeEndCallback"
    BeforeEndEvent.Invoke
    m_sndSound.BeforeEndEvent.Invoke
End Sub

Friend Sub FireEndCallback()
On Error Resume Next
    Debug.Print "Channel.FireEndCallback"
    EndEvent.Invoke
    m_sndSound.EndEvent.Invoke
End Sub

Friend Sub FireStopCallback()
On Error Resume Next
    Debug.Print "Channel.FireStopCallback"
    StopEvent.Invoke
    m_sndSound.StopEvent.Invoke
End Sub

Public Property Get ChannelGroup() As ChannelGroup
Attribute ChannelGroup.VB_Description = "The channel group this channel is in."
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
Attribute Sound.VB_Description = "The sound this channel is currently playing."
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
    FMOD_Channel_SetUserData m_lngHandle, ObjPtr(Me)
    FMOD_Channel_SetCallback m_lngHandle, FMOD_CHANNEL_CALLBACKTYPE_END, AddressOf FMod_Channel_Callback, 0
    FMOD_Channel_SetCallback m_lngHandle, FMOD_CHANNEL_CALLBACKTYPE_SYNCPOINT, AddressOf FMod_Channel_Callback, 0
End Sub

Friend Sub Free()
On Error Resume Next
    FMOD_Channel_SetCallback m_lngHandle, FMOD_CHANNEL_CALLBACKTYPE_END, 0, 0
    FMOD_Channel_SetUserData m_lngHandle, 0
    Set m_seEngine = Nothing
    Set m_sndSound = Nothing
    m_lngHandle = 0
    m_booDisposed = True
End Sub

Private Sub ResultCheck(ByVal Result As FMOD_RESULT)
On Error Resume Next
Dim l_lngHandle As Long
    If Result = FMOD_ERR_CHANNEL_STOLEN Then Free
    If (Result = FMOD_OK) Then
    Else
    End If
End Sub

Public Property Get Playing() As Boolean
Attribute Playing.VB_Description = "If True this channel is currently playing a sound."
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_IsPlaying(m_lngHandle, l_lngValue)
    Playing = l_lngValue
End Property

Public Property Get Virtual() As Boolean
Attribute Virtual.VB_Description = "If True this channel is currently virtualized."
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_IsVirtual(m_lngHandle, l_lngValue)
    Virtual = l_lngValue
End Property

Public Property Get Muted() As Boolean
Attribute Muted.VB_Description = "If True this channel is muted."
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
Attribute Paused.VB_Description = "If True this channel is paused."
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
Attribute Audibility.VB_Description = "The current output volume of the channel."
On Error Resume Next
Dim l_sngValue As Single
    ResultCheck fmodex.FMOD_Channel_GetAudibility(m_lngHandle, l_sngValue)
    Audibility = l_sngValue
End Property

Public Property Get Volume() As Single
Attribute Volume.VB_Description = "The volume factor of the channel."
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
Attribute Frequency.VB_Description = "The playback frequency of the channel."
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
Attribute Pan.VB_Description = "The pan position of the channel (-1.0 - 1.0)"
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
Attribute Priority.VB_Description = "The playback priority of this channel. Determines whether the channel will be virtualized if the sound engine runs out of available playback channels."
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_GetPriority(m_lngHandle, l_lngValue)
    Priority = l_lngValue
End Property

Public Property Let Priority(ByVal NewValue As Long)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetPriority(m_lngHandle, NewValue)
End Property

Public Property Get Location() As Variant
On Error Resume Next
Dim l_vecLocation As FMOD_VECTOR, l_vecVelocity As FMOD_VECTOR
    ResultCheck fmodex.FMOD_Channel_Get3DAttributes(m_lngHandle, l_vecLocation, l_vecVelocity)
    Location = Array(l_vecLocation.X, l_vecLocation.Y, l_vecLocation.Z)
End Property

Public Property Let Location(ByRef NewLocation As Variant)
On Error Resume Next
Dim l_vecLocation As FMOD_VECTOR, l_vecVelocity As FMOD_VECTOR
    ResultCheck fmodex.FMOD_Channel_Get3DAttributes(m_lngHandle, l_vecLocation, l_vecVelocity)
    l_vecLocation = FModVector(NewLocation)
    ResultCheck fmodex.FMOD_Channel_Set3DAttributes(m_lngHandle, l_vecLocation, l_vecVelocity)
End Property

Public Property Get Velocity() As Variant
On Error Resume Next
Dim l_vecLocation As FMOD_VECTOR, l_vecVelocity As FMOD_VECTOR
    ResultCheck fmodex.FMOD_Channel_Get3DAttributes(m_lngHandle, l_vecLocation, l_vecVelocity)
    Velocity = Array(l_vecVelocity.X, l_vecVelocity.Y, l_vecVelocity.Z)
End Property

Public Property Let Velocity(ByRef NewVelocity As Variant)
On Error Resume Next
Dim l_vecLocation As FMOD_VECTOR, l_vecVelocity As FMOD_VECTOR
    ResultCheck fmodex.FMOD_Channel_Get3DAttributes(m_lngHandle, l_vecLocation, l_vecVelocity)
    l_vecVelocity = FModVector(NewVelocity)
    ResultCheck fmodex.FMOD_Channel_Set3DAttributes(m_lngHandle, l_vecLocation, l_vecVelocity)
End Property

Public Property Let Position(ByVal NewPosition As Double)
Attribute Position.VB_Description = "The playback position of the channel."
On Error Resume Next
Dim l_lngValue As Long
    l_lngValue = Floor(NewPosition * 1000)
    ResultCheck fmodex.FMOD_Channel_SetPosition(m_lngHandle, l_lngValue, FMOD_TIMEUNIT_MS)
End Property

Public Property Get Position() As Double
On Error Resume Next
Dim l_lngValue As Long
    ResultCheck fmodex.FMOD_Channel_GetPosition(m_lngHandle, l_lngValue, FMOD_TIMEUNIT_MS)
    Position = CDbl(l_lngValue) / 1000#
End Property

Public Sub Kill()
Attribute Kill.VB_Description = "Stops playback."
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_Stop(m_lngHandle)
End Sub
