VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "AudioFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements iPlugin
Implements iFileTypePlugin
Private m_objEditor As Object

Private Sub iFileTypePlugin_FixUpSaveFilename(ByRef Filename As String)
End Sub

Private Function iFileTypePlugin_CanOpenFile(Filename As String) As Boolean
On Error Resume Next
    Select Case LCase(Trim(GetExtension(Filename)))
    Case "wav", "mp2", "mp3", "ogg", "wma", "xm", "s3m", "it", "mod", "oxm", "mid", "rmi", "aiff", "aif"
        iFileTypePlugin_CanOpenFile = True
    Case Else
    End Select
End Function

Private Function iFileTypePlugin_CreateNew() As iDocument
On Error Resume Next
End Function

Private Property Get iFileTypePlugin_FileTypeName() As String
On Error Resume Next
    iFileTypePlugin_FileTypeName = "Audio File/Music"
End Property

Private Function iFileTypePlugin_LoadFromFile(File As libGraphics.VirtualFile) As iDocument
On Error Resume Next
End Function

Private Function iFileTypePlugin_LoadFromFilename(Filename As String) As iDocument
On Error Resume Next
Dim l_frmNew As frmAudio
Dim l_hndHandle As SoundHandle, l_lngHandle As Long
Dim l_strRoot As String
    l_strRoot = m_objEditor.Engine.FileSystem.Root
    m_objEditor.Engine.FileSystem.Root = GetPath(Filename)
    l_lngHandle = m_objEditor.Engine.SoundEngine.Load(GetTitle(Filename), False)
    m_objEditor.Engine.FileSystem.Root = l_strRoot
    If l_lngHandle Then
        Set l_frmNew = New frmAudio
        l_frmNew.SetFilename Filename
        l_frmNew.SetHandle m_objEditor.Engine.SoundEngine.Handles(l_lngHandle)
        Set iFileTypePlugin_LoadFromFilename = l_frmNew
        Set iFileTypePlugin_LoadFromFilename.Plugin = Me
    End If
End Function

Private Property Get iFileTypePlugin_ShowInNewMenu() As Boolean
On Error Resume Next
    iFileTypePlugin_ShowInNewMenu = False
End Property

Private Sub iPlugin_Activate()
End Sub

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As stdole.Picture
    Set iPlugin_Icon = LoadResPicture("AUDIO", vbResIcon)
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Enables playback of audio and music formats supported by the engine."
End Property

Private Property Get iPlugin_PluginName() As String
On Error Resume Next
    iPlugin_PluginName = "Audio & Music Player Plugin"
End Property

Private Property Get iPlugin_PluginPath() As String
On Error Resume Next
    iPlugin_PluginPath = App.Path & IIf(Right(App.Path, 1) = "\", "ng.dll", "\ng.dll")
End Property

Private Property Get iPlugin_ShowInPluginMenu() As Boolean
    iPlugin_ShowInPluginMenu = False
End Property

Private Sub iPlugin_Shutdown()
End Sub

Private Property Get iFileTypePlugin_FilterString() As String
On Error Resume Next
    iFileTypePlugin_FilterString = "Audio Files|*.wav;*.mp2;*.mp3;*.ogg;*.wma;*.aiff;*.aif|Music Files|*.xm;*.s3m;*.it;*.mod;*.oxm;*.mid;*.rmi"
End Property

