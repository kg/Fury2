VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SoundEngine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Sound Engine"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'   ::fury² Sound engine::
'
Private Const c_lngMaxChannels = 512
Private Const c_lngMaxSoftwareChannels = 24

Private m_colGroups As Collection
Private m_colSounds As Collection
Private m_lngFMod As Long
Private m_cgMaster As Channelgroup
Public Enabled As Boolean
Public m_Engine As Object

Public Property Get CPUUsage() As Variant
On Error Resume Next
Dim l_sngDSP As Single
Dim l_sngStream As Single
Dim l_sngUpdate As Single
Dim l_sngTotal As Single
    Call fmodex.FMOD_System_GetCPUUsage(m_lngFMod, l_sngDSP, l_sngStream, l_sngUpdate, l_sngTotal)
    CPUUsage = Array(l_sngTotal, l_sngDSP, l_sngStream, l_sngUpdate)
End Property

Friend Function CreateChannelGroupH(ByVal Handle As Long) As Channelgroup
On Error Resume Next
Dim l_cgNew As Channelgroup
    If Handle = 0 Then Exit Function
    For Each l_cgNew In m_colGroups
        If l_cgNew.Handle = Handle Then
            Set CreateChannelGroup = l_cgNew
            Exit Function
        End If
    Next l_cgNew
    Set l_cgNew = New Channelgroup
    l_cgNew.Init Me, Handle
    m_colGroups.Add l_cgNew
    Set CreateChannelGroupH = l_cgNew
End Function

Public Function CreateChannelGroup(Optional ByRef name As String = "Channel Group") As Channelgroup
On Error Resume Next
Dim l_lngHandle As Long
    Call FMOD_System_CreateChannelGroup(m_lngFMod, name, l_lngHandle)
    Set CreateChannelGroup = CreateChannelGroupH(l_lngHandle)
End Function

Friend Function CreateChannel(ByVal Handle As Long, ByVal Sound As Sound) As Channel
On Error Resume Next
Dim l_chaNew As Channel
    If Handle = 0 Then Exit Function
    Set l_chaNew = New Channel
    l_chaNew.Init Me, Handle, Sound
    Set CreateChannel = l_chaNew
End Function

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_lngFMod
End Property

Public Sub FlushCache()
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown, m_lngCount As Long
Dim m_sndObj As Sound, m_lngObj As Long
    If m_colSounds Is Nothing Then Exit Sub
    If m_colSounds.Count < 1 Then Exit Sub
    m_Engine.DebugOut "Flushing Sound cache"
    m_lngObj = 1
    Do While m_lngObj < m_colSounds.Count
        Set m_objObj = Nothing
        Set m_objObj = m_colSounds(m_lngObj)
        If m_objObj Is Nothing Then
            m_lngObj = m_lngObj + 1
        Else
            m_objObj.AddRef
            m_lngCount = m_objObj.release
            Set m_objObj = Nothing
            If m_lngCount < 2 Then
                m_colSounds.Remove m_lngObj
            Else
                m_lngObj = m_lngObj + 1
            End If
            m_lngCount = 0
        End If
    Loop
    Err.Clear
End Sub

Public Sub Init()
On Error Resume Next
Dim l_lngHandle As Long
    Set m_colSounds = New Collection
    If Enabled = False Then Exit Sub
    fmodex.FMOD_System_Create m_lngFMod
    fmodex.FMOD_System_Init m_lngFMod, c_lngMaxSoftwareChannels, FMOD_INIT_NORMAL, 0
    Debug.Print fmodex.FMOD_System_GetMasterChannelGroup(m_lngFMod, l_lngHandle)
    Set m_cgMaster = CreateChannelGroupH(l_lngHandle)
    m_cgMaster.Disposed = True
End Sub

Public Sub Update()
On Error Resume Next
    If Enabled = False Then Exit Sub
    fmodex.FMOD_System_Update m_lngFMod
End Sub

Public Sub Shutdown()
On Error Resume Next
    If m_lngFMod <> 0 Then
        FlushCache
        Set m_colGroups = Nothing
        Set m_colSounds = Nothing
        fmodex.FMOD_System_Close m_lngFMod
        fmodex.FMOD_System_Release m_lngFMod
        m_lngFMod = 0
    End If
End Sub

Friend Sub Freed(ByVal Obj As Sound)
On Error Resume Next
Dim l_lngIndex As Long
    If m_colSounds Is Nothing Then Exit Sub
    l_lngIndex = Find(Obj)
    If (l_lngIndex > 0) Then
        m_colSounds.Remove l_lngIndex
    End If
End Sub

Public Function Find(ByVal Obj As Sound) As Long
On Error Resume Next
Dim m_lngItems As Long
    If m_colSounds Is Nothing Then Exit Function
    If m_colSounds.Count <= 0 Then Exit Function
    For m_lngItems = 1 To m_colSounds.Count
        If m_colSounds(m_lngItems) Is Obj Then
            Find = m_lngItems
            Exit Function
        End If
    Next m_lngItems
End Function

Public Function Load(ByVal Filename As String, Optional ByRef Soundtype As String = "Stream") As Sound
On Error Resume Next
Dim l_lngHandle As Long
Dim l_sndSound As Sound
Dim l_strFilename As String
    Filename = Replace(Filename, "\", "/")
    If Enabled = False Then
        Set l_sndSound = m_colSounds(Filename)
        If l_sngSound Is Nothing Then
            For Each l_sndSound In m_colSounds
                If LCase(Trim(l_sndSound.Filename)) = LCase(Trim(Filename)) Then
                    Set Load = l_sndSound
                    Exit Function
                End If
            Next l_sndSound
            Set l_sndSound = New Sound
            l_sndSound.Init Me, 0, Filename
            m_colSounds.Add l_sndSound, Filename
        End If
        Set Load = l_sndSound
        Exit Function
    End If
    Set l_sndSound = m_colSounds(Filename)
    If l_sndSound Is Nothing Then
        For Each l_sndSound In m_colSounds
            If LCase(Trim(l_sndSound.Filename)) = LCase(Trim(Filename)) Then
                Set Load = l_sndSound
                Exit Function
            End If
        Next l_sndSound
        Filename = m_Engine.FileSystem.AutoSelectExtension(Filename, "it;oxm;xm;s3m;mod;mid;midi;wav;aiff;aif;flac;ogg;aac;wma;mp3;mp2")
        Set l_sndSound = m_colSounds(Filename)
        If l_sndSound Is Nothing Then
            If m_Engine.FileSystem.FileExists(Filename) Then
                l_strFilename = m_Engine.FileSystem.File(Filename).GetRealFilename
                For Each l_sndSound In m_colSounds
                    If l_sndSound.name = l_strFilename Then
                        Set Load = l_sndSound
                        Exit Function
                    End If
                Next l_sndSound
                ' todo: FMOD_UNICODE? Not sure if this works on win9x/FAT32 so i'm not adding it yet
                Select Case LCase(Trim(Soundtype))
                Case "stream"
                    Call fmodex.FMOD_System_CreateSound(m_lngFMod, l_strFilename, FMOD_SOFTWARE Or FMOD_2D Or FMOD_CREATESTREAM Or FMOD_ACCURATETIME, l_lngHandle)
                Case "sample"
                    Call fmodex.FMOD_System_CreateSound(m_lngFMod, l_strFilename, FMOD_SOFTWARE Or FMOD_2D Or FMOD_CREATESAMPLE, l_lngHandle)
                Case Else
                End Select
                If l_lngHandle Then
                    Set l_sndSound = New Sound
                    l_sndSound.Init Me, l_lngHandle, Filename
                    m_colSounds.Add l_sndSound, Filename
                End If
            End If
        End If
    End If
    Set Load = l_sndSound
    If l_sndSound Is Nothing Then
        m_Engine.DebugOut "Unable to load audio file: """ + Filename + """"
    End If
End Function

Public Property Get Initialized() As Boolean
On Error Resume Next
    If m_lngFMod Then Initialized = False Else Initialized = True
End Property

Public Property Get Master() As Channelgroup
On Error Resume Next
    Set Master = m_cgMaster
End Property

Public Property Let MasterVolume(ByVal NewValue As Single)
On Error Resume Next
    Master.Volume = NewValue
End Property

Public Property Get MasterVolume() As Single
On Error Resume Next
    MasterVolume = Master.Volume
End Property

Public Property Get ChannelGroups() As Collection
On Error Resume Next
    Set ChannelGroups = m_colGroups
End Property

Public Property Get Sounds() As Collection
On Error Resume Next
    Set Sounds = m_colSounds
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Enabled = True
    Set m_colSounds = New Collection
    Set m_colGroups = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Shutdown
End Sub
