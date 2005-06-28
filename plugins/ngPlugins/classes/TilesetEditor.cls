VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TilesetEditor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Implements iPlugin
Implements iFileTypePlugin
Public Enum TilesetEditorClipboardFormats
    TCF_MIN_
    TCF_Tile
    TCF_MAX_
End Enum
Private m_lngClipboardFormats(TCF_MIN_ To TCF_MAX_)
Private m_objEditor As Object
Private m_clpClipboard As cCustomClipboard

Friend Property Get CustomClipboard() As cCustomClipboard
On Error Resume Next
    Set CustomClipboard = m_clpClipboard
End Property

Public Property Get ClipboardFormat(Format As TilesetEditorClipboardFormats) As Long
On Error Resume Next
    ClipboardFormat = m_lngClipboardFormats(Format)
End Property

Public Property Get Editor() As Object
    Set Editor = m_objEditor
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_clpClipboard = New cCustomClipboard
    m_lngClipboardFormats(TCF_Tile) = m_clpClipboard.AddFormat("Fury2Image")
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
        Filename = Filename + ".f2tileset"
    End If
End Sub

Private Function iFileTypePlugin_CanOpenFile(Filename As String) As Boolean
On Error Resume Next
    Select Case LCase(Trim(GetExtension(Filename)))
    Case "f2tileset", "vsp", "rts"
        iFileTypePlugin_CanOpenFile = True
    Case Else
    End Select
End Function

Private Function iFileTypePlugin_CreateNew(Optional ByVal EnableWizard As Boolean = True) As iDocument
On Error Resume Next
Dim l_frmNew As New frmTileset
Dim l_tstTileset As Fury2Tileset
Dim l_imgImage As Fury2Image
    If EnableWizard Then
        Load frmCreateTileset
        m_objEditor.LoadSettings "TilesetEditor\DefaultSettings", frmCreateTileset.Options
        frmCreateTileset.RefreshSettings
        frmCreateTileset.Show vbModal
        If Not (frmCreateTileset.Options Is Nothing) Then
            m_objEditor.SaveSettings "TilesetEditor\DefaultSettings", frmCreateTileset.Options
            With frmCreateTileset.Options
                l_frmNew.SetFilename ""
                l_frmNew.RefreshAll
                Set l_tstTileset = DefaultEngine.F2Tileset()
                l_tstTileset.TileWidth = .TileWidth
                l_tstTileset.TileHeight = .TileHeight
                l_tstTileset.MatteColor = .MatteColor
                l_tstTileset.Filename = .Filename
                l_tstTileset.Load
                Set l_imgImage = l_tstTileset.Buffer
                l_imgImage.ReplaceColor .MatteColor, 0
                l_imgImage.AlphaChannel = True
                l_tstTileset.MatteColor = 0
                Set l_tstTileset.Buffer = l_imgImage
                l_frmNew.SetTileset l_tstTileset
                l_tstTileset.Embed = True
                Set iFileTypePlugin_CreateNew = l_frmNew
                Set iFileTypePlugin_CreateNew.Plugin = Me
            End With
        End If
        Unload frmCreateTileset
        Err.Clear
    Else
        l_frmNew.SetTileset DefaultEngine.F2Tileset()
        Set iFileTypePlugin_CreateNew = l_frmNew
        Set iFileTypePlugin_CreateNew.Plugin = Me
        Err.Clear
    End If
End Function

Private Property Get iFileTypePlugin_FileTypeName() As String
On Error Resume Next
    iFileTypePlugin_FileTypeName = "Tileset"
End Property

Private Function iFileTypePlugin_LoadFromFile(File As libGraphics.VirtualFile) As iDocument
On Error Resume Next
End Function

Private Function iFileTypePlugin_LoadFromFilename(Filename As String) As iDocument
On Error Resume Next
Dim l_frmNew As frmTileset
Dim l_tsTileset As Fury2Tileset
    Set l_tsTileset = DefaultEngine.F2Tileset()
    Set l_frmNew = New frmTileset
    If Engine.LoadFromFile(l_tsTileset, F2File(Filename)) Then
        l_frmNew.SetTileset l_tsTileset
        l_frmNew.SetFilename Filename
        Set iFileTypePlugin_LoadFromFilename = l_frmNew
        Set iFileTypePlugin_LoadFromFilename.Plugin = Me
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

Private Property Get iPlugin_Icon() As Fury2Image
On Error Resume Next
    Set iPlugin_Icon = m_objEditor.LoadResources("ng").ItemData("icons\tileset.png")
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Provides support for creating, loading, editing, and saving Fury², Verge, ika, and Sphere Tilesets."
End Property

Private Property Get iPlugin_PluginName() As String
On Error Resume Next
    iPlugin_PluginName = "Tileset Editor Plugin"
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
    iFileTypePlugin_FilterString = "Tilesets|*.f2tileset;*.vsp;*.rts"
End Property

Private Property Get iPlugin_ToolbarIcon() As libGraphics.Fury2Image
On Error Resume Next
End Property

Private Property Get iFileTypePlugin_SupportedExtensions() As Variant
On Error Resume Next
    iFileTypePlugin_SupportedExtensions = Array("f2tileset")
End Property

