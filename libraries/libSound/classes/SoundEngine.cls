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
'   ::fury² sound engine::
'

Private m_lngMusicVolume As Long, m_lngSampleVolume As Long, m_lngStreamVolume As Long
Private m_EnableMusic As Boolean
Private m_Handles As Collection
Private m_FMod As Long
Public m_Engine As Object

Public Sub FlushCache()
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown, m_lngCount As Long
Dim m_sndObj As SoundHandle, m_lngObj As Long
    If m_Handles.Count < 1 Then Exit Sub
    m_lngObj = 1
    Do While m_lngObj < m_Handles.Count
        Set m_objObj = Nothing
        Set m_objObj = m_Handles(m_lngObj)
        If m_objObj Is Nothing Then
            m_lngObj = m_lngObj + 1
        Else
            m_objObj.AddRef
            m_lngCount = m_objObj.Release
            Set m_objObj = Nothing
            If m_lngCount <= 2 Then
                Set m_sndObj = m_Handles(m_lngObj)
                'TextOut "Flushed " & m_sndObj.Name
                Set m_sndObj = Nothing
                FreeSound m_lngObj
                m_lngObj = m_lngObj + 1
            Else
                m_lngObj = m_lngObj + 1
            End If
        End If
    Loop
End Sub

Public Property Get MusicVolume() As Long
On Error Resume Next
    MusicVolume = m_lngMusicVolume
End Property

Public Property Let MusicVolume(NewVolume As Long)
On Error Resume Next
Dim l_hndHandle As SoundHandle
    m_lngMusicVolume = NewVolume
    If m_Handles.Count = 0 Then Exit Property
    For Each l_hndHandle In m_Handles
        If l_hndHandle.HandleType = SoundType_Music Then
            l_hndHandle.Volume = NewVolume
        End If
    Next l_hndHandle
End Property

Public Property Get SampleVolume() As Long
On Error Resume Next
    SampleVolume = m_lngSampleVolume
End Property

Public Property Let SampleVolume(NewVolume As Long)
On Error Resume Next
Dim l_hndHandle As SoundHandle
    m_lngSampleVolume = NewVolume
End Property

Public Property Get StreamVolume() As Long
On Error Resume Next
    StreamVolume = m_lngStreamVolume
End Property

Public Property Let StreamVolume(NewVolume As Long)
On Error Resume Next
Dim l_hndHandle As SoundHandle
    m_lngStreamVolume = NewVolume
    If m_Handles.Count = 0 Then Exit Property
    For Each l_hndHandle In m_Handles
        If l_hndHandle.HandleType = SoundType_Stream Then
            l_hndHandle.Volume = NewVolume
        End If
    Next l_hndHandle
End Property

Public Sub Init()
On Error Resume Next
    If m_Engine.DisableSound Then Exit Sub
    m_FMod = LoadFMod
    'FSound.SetBufferSize 50
    FSound.Init 44100, 32, FSOUND_Init_GlobalFocus
End Sub

Public Sub Shutdown()
On Error Resume Next
    If m_FMod <> 0 Then
        FSound.Close
        UnloadFMod m_FMod
    End If
End Sub

Public Function Find(Obj) As Long
On Error Resume Next
Dim m_lngItems As Long
    If m_Handles.Count <= 0 Then Exit Function
    For m_lngItems = 1 To m_Handles.Count
        If m_Handles(m_lngItems) Is Obj Then
            Find = m_lngItems
            Exit Function
        End If
    Next m_lngItems
End Function

Public Function Load(ByVal Filename As String, Optional Looping As Boolean = False) As Long
On Error Resume Next
    Load = -1
    Filename = m_Engine.FileSystem.AutoSelectExtension(Filename, "it;xm;s3m;mod;mid;midi;ogg;wma;mp2;mp3;wav")
    If m_Engine.FileSystem.FileExists(Filename) Then
        Load = LoadMusic(Filename)
        If Load <> -1 Then Exit Function
        Load = LoadStream(Filename, Looping)
        If Load <> -1 Then Exit Function
        Load = LoadSample(Filename, Looping)
        If Load <> -1 Then Exit Function
    Else
        m_Engine.TextOut "Unable to load audio file: " + Filename
    End If
End Function

Private Function CheckExt(ByRef Fn As String, ByVal Ext As String) As Boolean
On Error Resume Next
Dim m_lngLength As Long
    Err.Clear
    m_lngLength = FileLen(Fn + "." + Ext)
    If (Err = 0) And (m_lngLength > 0) Then
        Fn = Fn + "." + Ext
        CheckExt = True
    Else
        CheckExt = False
    End If
End Function

Private Sub EnsureSFXExtension(ByRef Filename As String)
On Error Resume Next
    If InStr(Filename, ".") Then Exit Sub
    If CheckExt(Filename, "ogg") Then Exit Sub
    If CheckExt(Filename, "wma") Then Exit Sub
    If CheckExt(Filename, "mp2") Then Exit Sub
    If CheckExt(Filename, "mp3") Then Exit Sub
    If CheckExt(Filename, "oxm") Then Exit Sub
    If CheckExt(Filename, "it") Then Exit Sub
    If CheckExt(Filename, "xm") Then Exit Sub
    If CheckExt(Filename, "mid") Then Exit Sub
    If CheckExt(Filename, "s3m") Then Exit Sub
    If CheckExt(Filename, "mod") Then Exit Sub
    If CheckExt(Filename, "wav") Then Exit Sub
    If CheckExt(Filename, "aiff") Then Exit Sub
End Sub

Sub FreeAll()
On Error Resume Next
Dim Snd As Object
    For Each Snd In m_Handles
        Snd.Kill
        Snd.Free
    Next Snd
    Set m_Handles = New Collection
End Sub

Sub StopAll()
On Error Resume Next
Dim Snd As Object
    For Each Snd In m_Handles
        Snd.Kill
    Next Snd
End Sub

Private Function m_Initialized() As Boolean
On Error Resume Next
    If m_FMod = 0 Then m_Initialized = False Else m_Initialized = True
End Function

Public Property Let Volume(NewVolume As Long)
On Error Resume Next
    If Not m_Initialized Then Exit Property
    If m_Engine.DisableSound Then Exit Property
    FSound.SetSFXMasterVolume CLng(NewVolume)
End Property

Public Property Get Volume() As Long
On Error Resume Next
    If Not m_Initialized Then Exit Property
    If m_Engine.DisableSound Then Exit Property
    Volume = FSound.GetSFXMasterVolume
End Property

Public Sub FreeSound(Handle)
On Error Resume Next
    If VarType(Handle) = vbString Then
        m_Handles.Remove Handle
        m_Handles.Add Nothing, Handle
    Else
        m_Handles.Add Nothing, , , Handle
        m_Handles.Remove Handle
    End If
End Sub

Public Function LoadStream(ByVal Filename As String, Optional Looping As Boolean = False) As Long
On Error Resume Next
Dim m_Handle As Long
Dim m_Obj As SoundHandle
Dim m_filFile As Object
    LoadStream = -1
    If m_Engine.DisableSound Then m_Handles.Add New SoundHandle: LoadStream = m_Handles.Count: Exit Function
    Set m_Obj = New SoundHandle
    Filename = m_Engine.FileSystem.AutoSelectExtension(Filename, "wav;aiff;aif;ogg;mp3;mp2;wma")
    If Not m_Engine.FileSystem.FileExists(Filename) Then
        m_Engine.TextOut "Unable to load stream file: " + Filename
        Exit Function
    End If
    Set m_filFile = m_Engine.FileSystem.File(Filename)
    If m_filFile Is Nothing Then Exit Function
    If Looping Then
        m_Handle = FSound_Stream.Open(m_filFile.GetRealFilename, FSound_Loop_Normal Or FSound_HW2D, 0, 0)
    Else
        m_Handle = FSound_Stream.Open(m_filFile.GetRealFilename, FSound_Loop_Off Or FSound_HW2D, 0, 0)
    End If
    If m_Handle = 0 Then Exit Function
    m_Obj.SetValues SoundType_Stream, m_Handle, True
    m_Obj.SetEngine Me
    m_Obj.Name = Filename
    m_Handles.Add m_Obj
    Set m_Obj = Nothing
    LoadStream = m_Handles.Count
End Function

Public Function LoadSample(ByVal Filename As String, Optional Looping As Boolean = False) As Long
On Error Resume Next
Dim m_Handle As Long
Dim m_Obj As SoundHandle
Dim m_lngHandles As Long
Dim m_filFile As Object
    LoadSample = -1
    Err.Clear
    Set m_Obj = m_Handles(Filename)
    If Err.Number <> 0 Or m_Obj Is Nothing Then
        Err.Clear
        If m_Engine.DisableSound Then m_Handles.Add New SoundHandle: LoadSample = m_Handles.Count: Exit Function
        Filename = m_Engine.FileSystem.AutoSelectExtension(Filename, "wav;aiff;aif;ogg;mp3;mp2;wma")
        Err.Clear
        Set m_Obj = m_Handles(Filename)
        If (Err.Number = 0) And (Not (m_Obj Is Nothing)) Then
            If m_Handles.Count > 0 Then
                For m_lngHandles = 1 To m_Handles.Count
                    If m_Handles(m_lngHandles) Is m_Obj Then
                        LoadSample = m_lngHandles
                        Err.Clear
                        Exit Function
                    End If
                Next m_lngHandles
            End If
        End If
        If Not m_Engine.FileSystem.FileExists(Filename) Then
            m_Engine.TextOut "Unable to load sound file: " + Filename
            Exit Function
        End If
        Set m_filFile = m_Engine.FileSystem.File(Filename)
        If m_filFile Is Nothing Then Exit Function
        Set m_Obj = New SoundHandle
        If Looping Then
            m_Handle = FSound_Sample.Load(-1, m_filFile.GetRealFilename, FSound_Sample_16Bits Or FSound_Sample_Stereo Or FSound_Sample_Loop_Normal, 0, 0)
        Else
            m_Handle = FSound_Sample.Load(-1, m_filFile.GetRealFilename, FSound_Sample_16Bits Or FSound_Sample_Stereo Or FSound_Sample_Loop_Off, 0, 0)
        End If
        If m_Handle = 0 Then Exit Function
        m_Obj.SetValues SoundType_Sample, m_Handle, True
        m_Obj.SetEngine Me
        m_Obj.Name = Filename
        m_Handles.Add m_Obj, Filename
        Set m_Obj = Nothing
        LoadSample = m_Handles.Count
        Err.Clear
        Exit Function
    Else
        If m_Handles.Count > 0 Then
            For m_lngHandles = 1 To m_Handles.Count
                If m_Handles(m_lngHandles) Is m_Obj Then
                    LoadSample = m_lngHandles
                    Err.Clear
                    Exit Function
                End If
            Next m_lngHandles
        End If
    End If
End Function

Public Function LoadMusic(ByVal Filename As String) As Long
On Error Resume Next
Dim m_Handle As Long
Dim m_Obj As SoundHandle
Dim m_filFile As Object
    LoadMusic = -1
    If m_Engine.DisableSound Then m_Handles.Add New SoundHandle: LoadMusic = m_Handles.Count: Exit Function
    Set m_Obj = New SoundHandle
    Filename = m_Engine.FileSystem.AutoSelectExtension(Filename, "oxm;it;xm;s3m;mod;mid;midi")
    If Not m_Engine.FileSystem.FileExists(Filename) Then
        m_Engine.TextOut "Unable to load music file: " + Filename
        Exit Function
    End If
    Set m_filFile = m_Engine.FileSystem.File(Filename)
    If m_filFile Is Nothing Then Exit Function
    m_Handle = FMusic.LoadSong(m_filFile.GetRealFilename)
    If m_Handle = 0 Then Exit Function
    m_Obj.SetValues SoundType_Music, m_Handle, True
    m_Obj.SetEngine Me
    m_Obj.Name = Filename
    m_Handles.Add m_Obj
    Set m_Obj = Nothing
    LoadMusic = m_Handles.Count
End Function

Public Property Get Handles() As Collection
On Error Resume Next
    Set Handles = m_Handles
End Property

Public Property Set Handles(NewHandles As Collection)
On Error Resume Next
    Set m_Handles = NewHandles
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_Handles = New Collection
    Me.SampleVolume = 255
    Me.StreamVolume = 255
    Me.MusicVolume = 255
    m_EnableMusic = True
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set m_Handles = Nothing
    Shutdown
End Sub

Public Property Get EnableMusic() As Boolean
On Error Resume Next
    EnableMusic = m_EnableMusic
End Property

Public Property Let EnableMusic(NewEnable As Boolean)
On Error Resume Next
    m_EnableMusic = NewEnable
End Property
