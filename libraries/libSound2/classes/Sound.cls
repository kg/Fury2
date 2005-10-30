VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Sound"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Sound Handle"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Private Const c_lngEndOffset As Long = 10

Public StopEvent As Object
Public BeforeEndEvent As Object
Public EndEvent As Object
Public StartEvent As Object
Private m_sngFrequency As Single
Private m_strFilename As String
Private m_seEngine As SoundEngine
Private m_lngHandle As Long

Friend Property Get SyncPointCount() As Long
On Error Resume Next
Dim l_lngCount As Long
    FMOD_Sound_GetNumSyncPoints m_lngHandle, l_lngCount
    SyncPointCount = l_lngCount
End Property

Friend Sub AddDefaultSyncPoints()
On Error Resume Next
Dim l_lngLength As Long
Dim l_lngHandle As Long
    AddSyncPoint 0, "start"
    FMOD_Sound_GetLength m_lngHandle, l_lngLength, FMOD_TIMEUNIT_MS
    l_lngHandle = FreeFile
    Open "C:\fmod.log" For Append As #l_lngHandle
    Print #l_lngHandle, "Adding end-1 sync point to track " & m_strFilename & " @ " & (l_lngLength - 1000) & "ms, sound ID " & m_lngHandle
    Close #l_lngHandle
    AddSyncPoint l_lngLength - 1000, "end-1"
    AddSyncPoint l_lngLength - c_lngEndOffset, "end"
End Sub

Friend Sub AddSyncPoint(ByVal Time As Long, Optional ByRef Name As String = "")
On Error Resume Next
Dim l_lngPoint As Long
    FMOD_Sound_AddSyncPoint m_lngHandle, Time, FMOD_TIMEUNIT_MS, Name, l_lngPoint
End Sub

Friend Sub RemoveSyncPoint(ByVal Index As Long)
On Error Resume Next
Dim l_lngPoint As Long
    FMOD_Sound_GetSyncPoint m_lngHandle, Index, l_lngPoint
    FMOD_Sound_DeleteSyncPoint m_lngHandle, l_lngPoint
End Sub

Friend Sub RemoveSyncPoints()
On Error Resume Next
    Do While SyncPointCount > 0
        RemoveSyncPoint 1
    Loop
End Sub

Public Property Get Filename() As String
On Error Resume Next
    Filename = m_strFilename
End Property

Public Sub Free()
On Error Resume Next
    FMOD_Sound_SetUserData m_lngHandle, 0
    If m_seEngine Is Nothing Then
    Else
        m_seEngine.Sounds.Remove m_seEngine.Find(Me)
    End If
    If m_lngHandle <> 0 Then fmodex.FMOD_Sound_Release m_lngHandle
    m_lngHandle = 0
    Set m_seEngine = Nothing
    m_strFilename = ""
End Sub

Friend Sub Init(ByVal Parent As SoundEngine, ByVal Handle As Long, ByRef Filename As String)
On Error Resume Next
    Set m_seEngine = Parent
    m_lngHandle = Handle
    m_strFilename = Filename
    m_sngFrequency = Frequency
    FMOD_Sound_SetUserData m_lngHandle, ObjPtr(Me)
    RemoveSyncPoints
    AddDefaultSyncPoints
End Sub

Public Property Get Parent() As SoundEngine
On Error Resume Next
    Set Parent = m_seEngine
End Property

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_lngHandle
End Property

Public Property Get LoopCount() As Long
On Error Resume Next
Dim l_lngValue As Long
    fmodex.FMOD_Sound_GetLoopCount m_lngHandle, l_lngValue
    LoopCount = l_lngValue
End Property

Public Property Let LoopCount(ByVal NewCount As Long)
On Error Resume Next
    fmodex.FMOD_Sound_SetLoopCount m_lngHandle, NewCount
End Property

Public Property Get LoopMode() As SELoopModes
On Error Resume Next
Dim l_lngValue As FMOD_MODE
    fmodex.FMOD_Sound_GetMode m_lngHandle, l_lngValue
    If (l_lngValue And FMOD_LOOP_OFF) = FMOD_LOOP_OFF Then
        LoopMode = LoopMode_None
    ElseIf (l_lngValue And FMOD_LOOP_NORMAL) = FMOD_LOOP_NORMAL Then
        LoopMode = LoopMode_Normal
    ElseIf (l_lngValue And FMOD_LOOP_BIDI) = FMOD_LOOP_BIDI Then
        LoopMode = LoopMode_Bidirectional
    End If
End Property

Public Property Let LoopMode(ByVal NewMode As SELoopModes)
On Error Resume Next
Dim l_lngValue As FMOD_MODE
    fmodex.FMOD_Sound_GetMode m_lngHandle, l_lngValue
    l_lngValue = l_lngValue And (Not (FMOD_LOOP_OFF Or FMOD_LOOP_NORMAL Or FMOD_LOOP_BIDI))
    Select Case NewMode
    Case LoopMode_None
        l_lngValue = l_lngValue Or FMOD_LOOP_OFF
    Case LoopMode_Normal
        l_lngValue = l_lngValue Or FMOD_LOOP_NORMAL
    Case LoopMode_Bidirectional
        l_lngValue = l_lngValue Or FMOD_LOOP_BIDI
    Case Else
    End Select
    fmodex.FMOD_Sound_SetMode m_lngHandle, l_lngValue
End Property

Public Property Get Volume() As Single
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    Volume = l_sngVolume
End Property

Public Property Get OriginalFrequency() As Single
On Error Resume Next
    OriginalFrequency = m_sngFrequency
End Property

Public Property Get Frequency() As Single
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    Volume = l_sngFrequency
End Property

Public Property Get Pan() As Single
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    Pan = l_sngPan
End Property

Public Property Get Priority() As Long
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    Priority = l_lngPriority
End Property

Public Property Let Volume(ByVal NewValue As Single)
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    fmodex.FMOD_Sound_SetDefaults m_lngHandle, l_sngFrequency, NewValue, l_sngPan, l_lngPriority
End Property

Public Property Let Frequency(ByVal NewValue As Single)
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    fmodex.FMOD_Sound_SetDefaults m_lngHandle, NewValue, l_sngVolume, l_sngPan, l_lngPriority
End Property

Public Property Let Pan(ByVal NewValue As Single)
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    fmodex.FMOD_Sound_SetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, NewValue, l_lngPriority
End Property

Public Property Let Priority(ByVal NewValue As Long)
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single, l_lngPriority As Long
    fmodex.FMOD_Sound_GetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, l_lngPriority
    fmodex.FMOD_Sound_SetDefaults m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan, NewValue
End Property

Public Property Get VolumeVariation() As Single
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single
    fmodex.FMOD_Sound_GetVariations m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan
    VolumeVariation = l_sngVolume
End Property

Public Property Get FrequencyVariation() As Single
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single
    fmodex.FMOD_Sound_GetVariations m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan
    FrequencyVariation = l_sngFrequency
End Property

Public Property Get PanVariation() As Single
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single
    fmodex.FMOD_Sound_GetVariations m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan
    PanVariation = l_sngPan
End Property

Public Property Let VolumeVariation(ByVal NewValue As Single)
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single
    fmodex.FMOD_Sound_GetVariations m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan
    fmodex.FMOD_Sound_SetVariations m_lngHandle, l_sngFrequency, NewValue, l_sngPan
End Property

Public Property Let FrequencyVariation(ByVal NewValue As Single)
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single
    fmodex.FMOD_Sound_GetVariations m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan
    fmodex.FMOD_Sound_SetVariations m_lngHandle, NewValue, l_sngVolume, l_sngPan
End Property

Public Property Let PanVariation(ByVal NewValue As Single)
On Error Resume Next
Dim l_sngVolume As Single, l_sngFrequency As Single, l_sngPan As Single
    fmodex.FMOD_Sound_GetVariations m_lngHandle, l_sngFrequency, l_sngVolume, l_sngPan
    fmodex.FMOD_Sound_SetVariations m_lngHandle, l_sngFrequency, l_sngVolume, NewValue
End Property

Public Property Get Length() As Double
On Error Resume Next
Dim l_lngValue As Long
    fmodex.FMOD_Sound_GetLength m_lngHandle, l_lngValue, FMOD_TIMEUNIT_MS
    Length = CDbl(l_lngValue) / 1000#
End Property

Public Property Get Name() As String
On Error Resume Next
Dim l_bytName() As Byte
Dim l_strName As String
    ReDim l_bytName(0 To 512)
    fmodex.FMOD_Sound_GetName m_lngHandle, ByVal VarPtr(l_bytName(0)), 511
    l_strName = StrConv(l_bytName, vbUnicode)
    Name = Left(l_strName, InStr(l_strName, Chr(0)) - 1)
    Err.Clear
End Property

Public Function Play(Optional ByVal Paused As Boolean = False) As Channel
On Error Resume Next
Dim l_lngChannel As Long
    Call fmodex.FMOD_System_PlaySound(m_seEngine.Handle, FMOD_CHANNEL_FREE, m_lngHandle, Paused, l_lngChannel)
    Set Play = m_seEngine.CreateChannel(l_lngChannel, Me)
End Function

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub
