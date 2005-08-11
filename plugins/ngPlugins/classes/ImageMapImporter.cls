VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ImageMapImporter"
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
Private m_objEditor As Object

Public Sub ImportMap(ByRef Options As ImageMapImporterOptions)
On Error Resume Next
Dim l_imgImage As Fury2Image
Dim l_imgTile As Fury2Image, l_rctTile As Fury2Rect
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngTilesX As Long, l_lngTilesY As Long
Dim l_lngIndex As Long
Dim l_lngX As Long, l_lngY As Long
Dim l_plgPlugin As iFileTypePlugin
Dim l_docMap As frmMap
Dim l_mapNew As Fury2Map
Dim l_tstTileset As Fury2Tileset
    With Options
'        Set l_imgImage = F2LoadImage(Options.Image)
        Set l_imgImage = DefaultEngine.LoadImage(Options.Image, True)
        If l_imgImage Is Nothing Then
            m_objEditor.ShowNotice "Error", "Unable to load image.", m_objEditor.NoticeIcon("error")
            Exit Sub
        End If
        l_lngWidth = l_imgImage.Width - .MarginLeft - .MarginRight
        l_lngHeight = l_imgImage.Height - .MarginTop - .MarginBottom
        If .GenerateTileset Then
            l_lngTilesX = Ceil(l_lngWidth / (.TileWidth))
            l_lngTilesY = Ceil(l_lngHeight / (.TileHeight))
            Set l_tstTileset = DefaultEngine.F2Tileset(, .TileWidth, .TileHeight)
            With l_tstTileset
                .TileWidth = .TileWidth
                .TileHeight = .TileHeight
                .HasAlpha = True
                .TransparentTile = -1
                .Embed = True
            End With
        Else
            l_lngTilesX = 1
            l_lngTilesY = 1
            Set l_tstTileset = DefaultEngine.F2Tileset(.Image, l_lngWidth, l_lngHeight)
            With l_tstTileset
                .Filename = Options.Image
                .TileWidth = l_lngWidth
                .TileHeight = l_lngHeight
                .HasAlpha = True
                .TransparentTile = -1
                .Embed = False
            End With
        End If
        Set l_docMap = m_objEditor.ActiveDocument
        If l_docMap Is Nothing Then
            Set l_plgPlugin = m_objEditor.FindFileTypePlugin("Map")
            Set l_docMap = l_plgPlugin.CreateNew(False)
            m_objEditor.NewDocument l_docMap
            Set l_mapNew = DefaultEngine.F2Map()
            l_mapNew.EditMode = True
            l_mapNew.CreateNew "Imported Map", l_lngTilesX, l_lngTilesY, 1, l_tstTileset
            l_mapNew.Layers(1).Name = "Imported Layer"
            l_docMap.SetFilename ""
            l_docMap.SetMap l_mapNew
            m_objEditor.LoadSettings "MapEditor\View", l_docMap.ViewOptions
        Else
            Set l_mapNew = l_docMap.Map
            l_mapNew.Layers.Add l_mapNew.Layers(l_mapNew.Layers.Count).Duplicate
            With l_mapNew.Layers(l_mapNew.Layers.Count)
                .Width = l_lngTilesX
                .Height = l_lngTilesY
                .Clear -1
                Set .Tileset = l_tstTileset
                .Name = "Imported Layer"
            End With
        End If
        If .GenerateTileset Then
            For l_lngY = 0 To l_lngTilesY - 1
                For l_lngX = 0 To l_lngTilesX - 1
                    Set l_rctTile = F2Rect(l_lngX * (.TileWidth) + .MarginLeft, l_lngY * (.TileHeight) + .MarginTop, .TileWidth, .TileHeight, False)
                    If l_imgTile Is Nothing Then
                        Set l_imgTile = l_imgImage.Subimage(l_rctTile)
                    Else
                        l_imgTile.Blit , l_rctTile, l_imgImage
                    End If
                    With l_mapNew.Layers(l_mapNew.Layers.Count)
                        l_lngIndex = l_tstTileset.FindTile(l_imgTile)
                        If l_lngIndex = 0 Then
                            l_tstTileset.Add l_imgTile
                            l_lngIndex = l_tstTileset.TileCount
                            Set l_imgTile = Nothing
                        End If
                        .Tile(l_lngX, l_lngY) = l_lngIndex - 1
                    End With
                Next l_lngX
            Next l_lngY
        Else
            l_mapNew.Layers(l_mapNew.Layers.Count).Tile(0, 0) = 0
            l_mapNew.Layers(l_mapNew.Layers.Count).Prerendered = Not Options.GenerateTileset
        End If
        l_mapNew.Layers(l_mapNew.Layers.Count).Effect = F2LE_Alpha
    End With
    l_docMap.RefreshAll
    l_docMap.ViewChanged
End Sub

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Sub iPlugin_Activate()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
Dim l_docDocument As iDocument
    Load frmImageMapImporter
    m_objEditor.LoadSettings "ImageMapImporter\Settings", frmImageMapImporter.Options
    frmImageMapImporter.RefreshSettings
    frmImageMapImporter.Show vbModal
    If Not (frmImageMapImporter.Options Is Nothing) Then
        m_objEditor.SaveSettings "ImageMapImporter\Settings", frmImageMapImporter.Options
        ImportMap frmImageMapImporter.Options
    End If
    Unload frmImageMapImporter
    Err.Clear
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As Fury2Image
    Set iPlugin_Icon = LoadResPicture("MAP", vbResIcon)
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Imports a map and tileset from an image"
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "Image Map Importer"
End Property

Private Property Get iPlugin_PluginPath() As String
On Error Resume Next
    iPlugin_PluginPath = App.Path & IIf(Right(App.Path, 1) = "\", "ng.dll", "\ng.dll")
End Property

Private Property Get iPlugin_ShowInPluginMenu() As Boolean
    iPlugin_ShowInPluginMenu = True
End Property

Private Sub iPlugin_Shutdown()
End Sub

Private Property Get iPlugin_ToolbarIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iPlugin_ToolbarIcon = m_objEditor.LoadResources("ng").ItemData("icons\image map importer.png")
End Property

