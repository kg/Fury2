VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SpriteEditor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements iPlugin
Implements iFileTypePlugin
Public Enum SpriteEditorClipboardFormats
    SCF_MIN_
    SCF_Sprite
    SCF_SpriteState
    SCF_SpritePose
    SCF_SpriteFrame
    SCF_MAX_
End Enum
Private m_lngClipboardFormats(SCF_MIN_ To SCF_MAX_)
Private m_objEditor As Object
Private m_clpClipboard As cCustomClipboard

Friend Property Get CustomClipboard() As cCustomClipboard
On Error Resume Next
    Set CustomClipboard = m_clpClipboard
End Property

Public Property Get ClipboardFormat(Format As SpriteEditorClipboardFormats) As Long
On Error Resume Next
    ClipboardFormat = m_lngClipboardFormats(Format)
End Property

Public Property Get Editor() As Object
    Set Editor = m_objEditor
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_clpClipboard = New cCustomClipboard
    m_lngClipboardFormats(SCF_Sprite) = m_clpClipboard.AddFormat("Fury2Sprite")
    m_lngClipboardFormats(SCF_SpriteState) = m_clpClipboard.AddFormat("Fury2State")
    m_lngClipboardFormats(SCF_SpritePose) = m_clpClipboard.AddFormat("Fury2Pose")
    m_lngClipboardFormats(SCF_SpriteFrame) = m_clpClipboard.AddFormat("Fury2PoseFrame")
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    m_clpClipboard.ClipboardClose
End Sub

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
On Error Resume Next
    iPlugin_CanConfigure = False
End Property

Private Sub iFileTypePlugin_FixUpSaveFilename(ByRef Filename As String)
On Error Resume Next
    If Len(Trim(GetExtension(Filename))) > 0 Then
    Else
        Filename = Filename + ".f2sprites"
    End If
End Sub

Private Function iFileTypePlugin_CanOpenFile(Filename As String) As Boolean
On Error Resume Next
    Select Case LCase(Trim(GetExtension(Filename)))
    Case "f2sprite", "f2sprites", "f2sc"
        iFileTypePlugin_CanOpenFile = True
    Case Else
    End Select
End Function

Private Function iFileTypePlugin_CreateNew() As iDocument
On Error Resume Next
Dim l_frmNew As New frmSprites
    l_frmNew.SetFilename ""
    Set iFileTypePlugin_CreateNew = l_frmNew
    Set iFileTypePlugin_CreateNew.Plugin = Me
    l_frmNew.RefreshAll
End Function

Private Property Get iFileTypePlugin_FileTypeName() As String
On Error Resume Next
    iFileTypePlugin_FileTypeName = "Sprite Collection"
End Property

Private Function iFileTypePlugin_LoadFromFile(File As libGraphics.VirtualFile) As iDocument
On Error Resume Next
End Function

Private Function iFileTypePlugin_LoadFromFilename(Filename As String) As iDocument
On Error Resume Next
Dim l_frmNew As New frmSprites
Dim l_scSprites As Fury2Sprites
    Set l_scSprites = New Fury2Sprites
    If LoadFromFile(l_scSprites, F2File(Filename)) Then
        l_frmNew.SetFilename Filename
        l_frmNew.SetSprites l_scSprites
        Set iFileTypePlugin_LoadFromFilename = l_frmNew
        Set iFileTypePlugin_LoadFromFilename.Plugin = Me
    Else
        MsgBox "Unable to load sprite collection."
    End If
End Function

Private Property Get iFileTypePlugin_ShowInNewMenu() As Boolean
On Error Resume Next
    iFileTypePlugin_ShowInNewMenu = True
End Property

Private Sub iPlugin_Activate()
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As stdole.Picture
On Error Resume Next
    Set iPlugin_Icon = LoadResPicture("SPRITES", vbResIcon)
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Provides support for creating, loading, editing, and saving Fury² sprite collections."
End Property

Private Property Get iPlugin_PluginName() As String
On Error Resume Next
    iPlugin_PluginName = "Sprite Editor Plugin"
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
    iFileTypePlugin_FilterString = "Sprite Collections|*.f2sprites;*.f2sc|Sprites|*.f2sprite"
End Property
