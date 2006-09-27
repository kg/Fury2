VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TKTilesetImporter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements iPlugin
Private m_objEditor As Object

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Sub iPlugin_Activate()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
Dim l_docDocument As iDocument
Dim l_tstTileset As TSTFile
    Load frmImportTKTileset
    m_objEditor.LoadSettings "TKTilesetImporter\Settings", frmImportTKTileset.Options
    frmImportTKTileset.RefreshSettings
    frmImportTKTileset.Show vbModal
    If Not (frmImportTKTileset.Options Is Nothing) Then
        m_objEditor.SaveSettings "TKTilesetImporter\Settings", frmImportTKTileset.Options
        Set l_plgPlugin = m_objEditor.FindFileTypePlugin("Image")
        With frmImportTKTileset.Options
            Set l_tstTileset = New TSTFile
            l_tstTileset.Load .Filename
        End With
        Set l_docDocument = l_plgPlugin.CreateNew
        l_docDocument.Object.SetImage l_tstTileset.Image
        m_objEditor.NewDocument l_docDocument
    End If
    Unload frmImportTKTileset
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As Fury2Image
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Imports an RPG Toolkit tileset"
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "RPG Toolkit Tileset Importer"
End Property

Private Property Get iPlugin_PluginPath() As String
On Error Resume Next
    iPlugin_PluginPath = App.Path & IIf(Right(App.Path, 1) = "\", "tk.dll", "\tk.dll")
End Property

Private Property Get iPlugin_ShowInPluginMenu() As Boolean
    iPlugin_ShowInPluginMenu = True
End Property

Private Sub iPlugin_Shutdown()
End Sub

Private Property Get iPlugin_ToolbarIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iPlugin_ToolbarIcon = m_objEditor.LoadResources("tk").ItemData("icons\tileset importer.png")
End Property

