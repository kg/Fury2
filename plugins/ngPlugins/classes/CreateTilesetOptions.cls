VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CreateTilesetOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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

Private mvarFilename As String
Private mvarTileWidth As Long
Private mvarTileHeight As Long
Private mvarMatteColor As Long

Public Property Get Filename() As String
Attribute Filename.VB_Description = "The filename of an (optional) image to import tiles from.{ImageFilename}"
    Filename = mvarFilename
End Property

Public Property Let Filename(ByRef vData As String)
    mvarFilename = vData
End Property

Public Property Get TileWidth() As Long
Attribute TileWidth.VB_Description = "The width of tiles in the tileset."
    TileWidth = mvarTileWidth
End Property

Public Property Let TileWidth(ByVal vData As Long)
    mvarTileWidth = vData
End Property

Public Property Get TileHeight() As Long
Attribute TileHeight.VB_Description = "The height of tiles in the tileset."
    TileHeight = mvarTileHeight
End Property

Public Property Let TileHeight(ByVal vData As Long)
    mvarTileHeight = vData
End Property

Public Property Get MatteColor() As Long
Attribute MatteColor.VB_Description = "The color to make transparent when importing a tileset.{Color}"
    MatteColor = mvarMatteColor
End Property

Public Property Let MatteColor(ByVal vData As Long)
    mvarMatteColor = vData
End Property

Private Sub Class_Initialize()
    mvarFilename = ""
    mvarTileWidth = 16
    mvarTileHeight = 16
    mvarMatteColor = 0
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
    With File
        .Load mvarFilename
        .Load mvarTileWidth
        .Load mvarTileHeight
        .Load mvarMatteColor
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
    With File
        .Save mvarFilename
        .Save mvarTileWidth
        .Save mvarTileHeight
        .Save mvarMatteColor
    End With
End Sub

