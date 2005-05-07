VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CreateMapOptions"
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

Implements IVirtualFileSaveable
Implements IInspectable

Private mvarLayers As Long
Private mvarWidth As Long
Private mvarHeight As Long
Private mvarTileset As String
Private mvarTileWidth As Long
Private mvarTileHeight As Long
Private mvarTileMatteColor As Long
Private mvarLayerEffect As Fury2LayerEffects
Private mvarName As String

Public Property Get TileMatteColor() As Long
Attribute TileMatteColor.VB_Description = "The color to use as the new map's transparent color.{Color}"
    TileMatteColor = mvarTileMatteColor
End Property

Public Property Let TileMatteColor(ByVal vData As Long)
    mvarTileMatteColor = vData
End Property

Public Property Let Name(ByVal vData As String)
Attribute Name.VB_Description = "The name of the new map."
    mvarName = vData
End Property

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Let LayerEffect(ByVal vData As Fury2LayerEffects)
Attribute LayerEffect.VB_Description = "The rendering effect to use on the map's layers."
    mvarLayerEffect = vData
End Property

Public Property Get LayerEffect() As Fury2LayerEffects
    LayerEffect = mvarLayerEffect
End Property

Public Property Let TileHeight(ByVal vData As Long)
Attribute TileHeight.VB_Description = "The height of the tiles in the tileset."
    mvarTileHeight = vData
End Property

Public Property Get TileHeight() As Long
    TileHeight = mvarTileHeight
End Property

Public Property Let TileWidth(ByVal vData As Long)
Attribute TileWidth.VB_Description = "The width of the tiles in the tileset."
    mvarTileWidth = vData
End Property

Public Property Get TileWidth() As Long
    TileWidth = mvarTileWidth
End Property

Public Property Let Tileset(ByVal vData As String)
Attribute Tileset.VB_Description = "The filename of the tileset to use.{ImageFilename}"
    mvarTileset = vData
End Property

Public Property Get Tileset() As String
    Tileset = mvarTileset
End Property

Public Property Let Height(ByVal vData As Long)
Attribute Height.VB_Description = "The height of the map's layers."
    mvarHeight = vData
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Public Property Let Width(ByVal vData As Long)
Attribute Width.VB_Description = "The width of the map's layers."
    mvarWidth = vData
End Property

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Let Layers(ByVal vData As Long)
Attribute Layers.VB_Description = "The number of layers to create."
    mvarLayers = vData
End Property

Public Property Get Layers() As Long
    Layers = mvarLayers
End Property

Private Sub Class_Initialize()
    mvarLayers = 2
    mvarWidth = 20
    mvarHeight = 20
    mvarTileset = ""
    mvarTileWidth = 16
    mvarTileHeight = 16
    mvarLayerEffect = BlitMode_Normal
    mvarName = "Untitled Map"
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
    With File
        .Load mvarLayers
        .Load mvarWidth
        .Load mvarHeight
        .Load mvarTileset
        .Load mvarTileWidth
        .Load mvarTileHeight
        .Load mvarLayerEffect
        .Skip 4
        .Load mvarName
        .Load mvarTileMatteColor
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
    With File
        .Save mvarLayers
        .Save mvarWidth
        .Save mvarHeight
        .Save mvarTileset
        .Save mvarTileWidth
        .Save mvarTileHeight
        .Save mvarLayerEffect
        .Save CLng(2)
        .Save mvarName
        .Save mvarTileMatteColor
    End With
End Sub
