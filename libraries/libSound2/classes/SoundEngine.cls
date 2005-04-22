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
Private Const c_lngMaxChannels = 512
Private Const c_lngMaxSoftwareChannels = 32

Private m_colChannels As Collection
Private m_colSounds As Collection
Private m_lngFMod As Long
Public m_Engine As Object

Public Sub FlushCache()
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown, m_lngCount As Long
Dim m_sndObj As Sound, m_lngObj As Long
    If m_colSounds.Count < 1 Then Exit Sub
    m_Engine.DebugOut "Flushing sound cache"
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
            If m_lngCount <= 2 Then
                Set m_sndObj = m_colSounds(m_lngObj)
                Set m_sndObj = Nothing
                FreeSound m_lngObj
                m_lngObj = m_lngObj + 1
            Else
                m_lngObj = m_lngObj + 1
            End If
        End If
    Loop
End Sub

Public Sub Init()
On Error Resume Next
    If m_Engine.DisableSound Then Exit Sub
    fmodex.FMOD_System_Create m_lngFMod
    fmodex.FMOD_System_Init m_lngFMod, c_lngMaxChannels, c_lngMaxSoftwareChannels, FMOD_INIT_NORMAL, 0
    InitChannels
End Sub

Friend Sub InitChannels()
On Error Resume Next
End Sub

Public Sub Shutdown()
On Error Resume Next
    If m_lngFMod <> 0 Then
        fmodex.FMOD_System_Close m_lngFMod
    End If
End Sub

Public Function Find(Obj) As Long
On Error Resume Next
Dim m_lngItems As Long
    If m_colSounds.Count <= 0 Then Exit Function
    For m_lngItems = 1 To m_colSounds.Count
        If m_colSounds(m_lngItems) Is Obj Then
            Find = m_lngItems
            Exit Function
        End If
    Next m_lngItems
End Function

Public Function Load(ByVal filename As String, Optional Looping As Boolean = False) As Long
On Error Resume Next
    Load = -1
    filename = m_Engine.FileSystem.AutoSelectExtension(filename, "it;xm;s3m;mod;mid;midi;ogg;wma;mp2;mp3;wav")
    If m_Engine.FileSystem.FileExists(filename) Then
    Else
        m_Engine.DebugOut "Unable to load audio file: """ + filename + """"
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

Private Sub EnsureSFXExtension(ByRef filename As String)
On Error Resume Next
    If InStr(filename, ".") Then Exit Sub
    If CheckExt(filename, "ogg") Then Exit Sub
    If CheckExt(filename, "wma") Then Exit Sub
    If CheckExt(filename, "mp2") Then Exit Sub
    If CheckExt(filename, "mp3") Then Exit Sub
    If CheckExt(filename, "oxm") Then Exit Sub
    If CheckExt(filename, "it") Then Exit Sub
    If CheckExt(filename, "xm") Then Exit Sub
    If CheckExt(filename, "mid") Then Exit Sub
    If CheckExt(filename, "s3m") Then Exit Sub
    If CheckExt(filename, "mod") Then Exit Sub
    If CheckExt(filename, "wav") Then Exit Sub
    If CheckExt(filename, "aiff") Then Exit Sub
End Sub

Sub FreeAll()
On Error Resume Next
Dim Snd As Object
    For Each Snd In m_colSounds
        Snd.Kill
        Snd.Free
    Next Snd
    Set m_colSounds = New Collection
End Sub

Sub StopAll()
On Error Resume Next
Dim Snd As Object
    For Each Snd In m_colSounds
        Snd.Kill
    Next Snd
End Sub

Public Property Get Initialized() As Boolean
On Error Resume Next
    If m_lngFMod Then Initialized = False Else Initialized = True
End Property

Public Property Let volume(ByVal NewVolume As Single)
On Error Resume Next
    If Not Initialized Then Exit Property
    fmodex.FMOD_System_SetMasterVolume m_lngFMod, NewVolume
End Property

Public Property Get volume() As Single
On Error Resume Next
Dim l_sngVolume As Single
    If Not Initialized Then Exit Property
    fmodex.FMOD_System_GetMasterVolume m_lngFMod, l_sngVolume
    volume = l_sngVolume
End Property

Public Sub FreeSound(Handle)
On Error Resume Next
    If VarType(Handle) = vbString Then
        m_colSounds.Remove Handle
        m_colSounds.Add Nothing, Handle
    Else
        m_colSounds.Add Nothing, , , Handle
        m_colSounds.Remove Handle
    End If
End Sub

Public Function LoadStream(ByVal filename As String, Optional Looping As Boolean = False) As Long
On Error Resume Next
Dim m_Handle As Long
Dim m_Obj As SoundHandle
Dim m_filFile As Object
    LoadStream = -1
    If m_Engine.DisableSound Then m_colSounds.Add New SoundHandle: LoadStream = m_colSounds.Count: Exit Function
    Set m_Obj = New SoundHandle
    filename = m_Engine.FileSystem.AutoSelectExtension(filename, "wav;aiff;aif;ogg;mp3;mp2;wma")
    If Not m_Engine.FileSystem.FileExists(filename) Then
        m_Engine.DebugOut "Unable to load stream: """ + filename + """"
        Exit Function
    End If
    Set m_filFile = m_Engine.FileSystem.File(filename)
    If m_filFile Is Nothing Then Exit Function
    If Looping Then
        m_Handle = FSound_Stream.Open(m_filFile.GetRealFilename, FSound_Loop_Normal Or FSound_HW2D, 0, 0)
    Else
        m_Handle = FSound_Stream.Open(m_filFile.GetRealFilename, FSound_Loop_Off Or FSound_HW2D, 0, 0)
    End If
    If m_Handle = 0 Then Exit Function
    m_Obj.SetValues SoundType_Stream, m_Handle, True
    m_Obj.SetEngine Me
    m_Obj.name = filename
    m_colSounds.Add m_Obj
    Set m_Obj = Nothing
    LoadStream = m_colSounds.Count
End Function

Public Function LoadSample(ByVal filename As String, Optional Looping As Boolean = False) As Long
On Error Resume Next
Dim m_Handle As Long
Dim m_Obj As SoundHandle
Dim m_lngHandles As Long
Dim m_filFile As Object
    LoadSample = -1
    Err.Clear
    Set m_Obj = m_colSounds(filename)
    If Err.Number <> 0 Or m_Obj Is Nothing Then
        Err.Clear
        If m_Engine.DisableSound Then m_colSounds.Add New SoundHandle: LoadSample = m_colSounds.Count: Exit Function
        filename = m_Engine.FileSystem.AutoSelectExtension(filename, "wav;aiff;aif;ogg;mp3;mp2;wma")
        Err.Clear
        Set m_Obj = m_colSounds(filename)
        If (Err.Number = 0) And (Not (m_Obj Is Nothing)) Then
            If m_colSounds.Count > 0 Then
                For m_lngHandles = 1 To m_colSounds.Count
                    If m_colSounds(m_lngHandles) Is m_Obj Then
                        LoadSample = m_lngHandles
                        Err.Clear
                        Exit Function
                    End If
                Next m_lngHandles
            End If
        End If
        If Not m_Engine.FileSystem.FileExists(filename) Then
            m_Engine.DebugOut "Unable to load sample: """ + filename + """"
            Exit Function
        End If
        Set m_filFile = m_Engine.FileSystem.File(filename)
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
        m_Obj.name = filename
        m_colSounds.Add m_Obj, filename
        Set m_Obj = Nothing
        LoadSample = m_colSounds.Count
        Err.Clear
        Exit Function
    Else
        If m_colSounds.Count > 0 Then
            For m_lngHandles = 1 To m_colSounds.Count
                If m_colSounds(m_lngHandles) Is m_Obj Then
                    LoadSample = m_lngHandles
                    Err.Clear
                    Exit Function
                End If
            Next m_lngHandles
        End If
    End If
End Function

Public Property Get Handles() As Collection
On Error Resume Next
    Set Handles = m_colSounds
End Property

Public Property Set Handles(NewHandles As Collection)
On Error Resume Next
    Set m_colSounds = NewHandles
End Property

Public Property Get Sounds() As Collection
On Error Resume Next
    Set Sounds = m_colSounds
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colSounds = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    FlushCache
    Set m_colSounds = Nothing
    Shutdown
End Sub
