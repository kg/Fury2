VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "MapEditor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements iPlugin
Implements iFileTypePlugin
Public Enum MapEditorClipboardFormats
    CF_MIN_
    CF_MapLayer
    CF_Area
    CF_Sprite
    CF_LightSource
    CF_Tileset
    CF_MapSelection
    CF_MapObstructions
    CF_LightObstructions
    CF_LightPlanes
    CF_MAX_
End Enum
Private m_lngClipboardFormats(CF_MIN_ To CF_MAX_)
Private m_objEditor As Object
Private m_clpClipboard As cCustomClipboard

Friend Property Get CustomClipboard() As cCustomClipboard
On Error Resume Next
    Set CustomClipboard = m_clpClipboard
End Property

Public Property Get ClipboardFormat(Format As MapEditorClipboardFormats) As Long
On Error Resume Next
    ClipboardFormat = m_lngClipboardFormats(Format)
End Property

Public Property Get Editor() As Object
    Set Editor = m_objEditor
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_clpClipboard = New cCustomClipboard
    m_lngClipboardFormats(CF_MapLayer) = m_clpClipboard.AddFormat("Fury2MapLayer")
    m_lngClipboardFormats(CF_Area) = m_clpClipboard.AddFormat("Fury2Area")
    m_lngClipboardFormats(CF_Sprite) = m_clpClipboard.AddFormat("Fury2Sprite")
    m_lngClipboardFormats(CF_LightSource) = m_clpClipboard.AddFormat("Fury2LightSource")
    m_lngClipboardFormats(CF_Tileset) = m_clpClipboard.AddFormat("Fury2Tileset")
    m_lngClipboardFormats(CF_MapObstructions) = m_clpClipboard.AddFormat("Fury2MapObstructions")
    m_lngClipboardFormats(CF_LightObstructions) = m_clpClipboard.AddFormat("Fury2LightObstructions")
    m_lngClipboardFormats(CF_LightPlanes) = m_clpClipboard.AddFormat("Fury2LightPlanes")
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
        Filename = Filename + ".f2map"
    End If
End Sub

Private Function iFileTypePlugin_CanOpenFile(Filename As String) As Boolean
On Error Resume Next
    Select Case LCase(Trim(GetExtension(Filename)))
    Case "f2m", "f2map"
        iFileTypePlugin_CanOpenFile = True
    Case Else
    End Select
End Function

Private Function iFileTypePlugin_CreateNew() As iDocument
On Error Resume Next
Dim l_frmNew As New frmMap
Dim l_mapMap As Fury2Map
    l_frmNew.SetFilename ""
    l_frmNew.RefreshAll
    Set l_mapMap = New Fury2Map
    l_mapMap.EditMode = True
    l_mapMap.CreateNew "Untitled Map", 20, 20, 1, New Fury2Tileset
    l_mapMap.Init
    l_mapMap.Load
    l_frmNew.SetMap l_mapMap
    m_objEditor.LoadSettings "MapEditor\View", l_frmNew.ViewOptions
    l_frmNew.ViewChanged
    Set iFileTypePlugin_CreateNew = l_frmNew
    Set iFileTypePlugin_CreateNew.Plugin = Me
End Function

Private Property Get iFileTypePlugin_FileTypeName() As String
On Error Resume Next
    iFileTypePlugin_FileTypeName = "Map"
End Property

Private Function iFileTypePlugin_LoadFromFile(File As libGraphics.VirtualFile) As iDocument
On Error Resume Next
End Function

Private Function iFileTypePlugin_LoadFromFilename(Filename As String) As iDocument
On Error Resume Next
Dim l_frmNew As New frmMap
Dim l_mapMap As Fury2Map
    Set l_mapMap = New Fury2Map
    l_mapMap.EditMode = True
    If Engine.LoadFromFile(l_mapMap, F2File(Filename)) Then
        l_mapMap.Init
        l_mapMap.Load
        l_frmNew.SetFilename Filename
        l_frmNew.SetMap l_mapMap
        m_objEditor.LoadSettings "MapEditor\View", l_frmNew.ViewOptions
        l_frmNew.ViewChanged
        Set iFileTypePlugin_LoadFromFilename = l_frmNew
        Set iFileTypePlugin_LoadFromFilename.Plugin = Me
    Else
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
    Set iPlugin_Icon = LoadResPicture("MAP", vbResIcon)
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Provides support for creating, loading, editing, and saving Fury² maps."
End Property

Private Property Get iPlugin_PluginName() As String
On Error Resume Next
    iPlugin_PluginName = "Map Editor Plugin"
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
    iFileTypePlugin_FilterString = "Maps|*.f2m;*.f2map"
End Property
