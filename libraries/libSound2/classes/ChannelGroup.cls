VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ChannelGroup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_booDisposed As Boolean
Private m_sndSound As Sound
Private m_seEngine As SoundEngine
Private m_lngHandle As Long

Public Property Get Disposed() As Boolean
On Error Resume Next
    Disposed = m_booDisposed
End Property

Friend Property Let Disposed(ByVal NewValue As Boolean)
On Error Resume Next
    m_booDisposed = NewValue
End Property
'
'Public Property Get Sound() As Sound
'On Error Resume Next
'Dim l_lngValue As Long
'    ResultCheck fmodex.FMOD_Channel_GetCurrentSound(m_lngHandle, l_lngValue)
'    If l_lngValue = m_sndSound.Handle Then
'        Set Sound = m_sndSound
'    Else
'        Set m_sndSound = Nothing
'    End If
'End Property

Public Property Get Parent() As SoundEngine
On Error Resume Next
    Set Parent = m_seEngine
End Property

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_lngHandle
End Property

Friend Sub Init(ByVal Parent As SoundEngine, ByVal Handle As Long)
On Error Resume Next
    Set m_seEngine = Parent
    m_lngHandle = Handle
End Sub

Friend Sub Free()
On Error Resume Next
    If Not Disposed Then
        fmodex.FMOD_ChannelGroup_Release m_lngHandle
        m_lngHandle = 0
    End If
    Set m_seEngine = Nothing
    Set m_sndSound = Nothing
    m_lngHandle = 0
    m_booDisposed = True
End Sub

Friend Sub ResultCheck(ByVal Result As FMOD_RESULT)
On Error Resume Next
End Sub

Public Property Let Muted(ByVal NewValue As Boolean)
On Error Resume Next
    ResultCheck fmodex.FMOD_ChannelGroup_OverrideMute(m_lngHandle, NewValue)
End Property

Public Property Let Paused(ByVal NewValue As Boolean)
On Error Resume Next
    ResultCheck fmodex.FMOD_ChannelGroup_OverridePaused(m_lngHandle, NewValue)
End Property

Public Property Get Volume() As Single
On Error Resume Next
Dim l_sngValue As Single
    ResultCheck fmodex.FMOD_ChannelGroup_GetVolume(m_lngHandle, l_sngValue)
    Volume = l_sngValue
End Property

Public Property Let Volume(ByVal NewValue As Single)
On Error Resume Next
    ResultCheck fmodex.FMOD_ChannelGroup_SetVolume(m_lngHandle, NewValue)
End Property

Public Property Get Pitch() As Single
On Error Resume Next
Dim l_sngValue As Single
    ResultCheck fmodex.FMOD_ChannelGroup_GetPitch(m_lngHandle, l_sngValue)
    Pitch = l_sngValue
End Property

Public Property Let Pitch(ByVal NewValue As Single)
On Error Resume Next
    ResultCheck fmodex.FMOD_ChannelGroup_SetPitch(m_lngHandle, NewValue)
End Property

Public Property Let Frequency(ByVal NewValue As Single)
On Error Resume Next
    ResultCheck fmodex.FMOD_ChannelGroup_OverrideFrequency(m_lngHandle, NewValue)
End Property

Public Property Let Pan(ByVal NewValue As Single)
On Error Resume Next
    ResultCheck fmodex.FMOD_ChannelGroup_OverridePan(m_lngHandle, NewValue)
End Property

Public Property Get Count() As Long
On Error Resume Next
Dim l_lngCount As Long
    ResultCheck fmodex.FMOD_ChannelGroup_GetNumChannels(m_lngHandle, l_lngCount)
    Count = l_lngCount
End Property

Public Sub Add(ByVal Channel As Channel)
On Error Resume Next
    ResultCheck fmodex.FMOD_Channel_SetChannelGroup(Channel.Handle, m_lngHandle)
End Sub

Public Sub Remove(IndexOrChannel)
On Error Resume Next
Dim l_lngChannel As Long, l_lngIndex As Long, l_chnChannel As Channel
    If VarType(IndexOrChannel) = vbObject Then
        Set l_chnChannel = IndexOrChannel
        l_lngChannel = l_chnChannel.Handle
    Else
        l_lngIndex = CLng(IndexOrChannel)
        ResultCheck fmodex.FMOD_ChannelGroup_GetChannel(m_lngHandle, l_lngIndex, l_lngChannel)
    End If
    ResultCheck fmodex.FMOD_Channel_SetChannelGroup(l_lngChannel, 0)
End Sub

Public Sub Kill()
On Error Resume Next
    ResultCheck fmodex.FMOD_ChannelGroup_Stop(m_lngHandle)
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub
