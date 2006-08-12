VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ImageMapImporterOptions"
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
Implements IVirtualFileSaveable
Implements IInspectable
Private mvarMarginLeft As Long
Private mvarMarginTop As Long
Private mvarMarginRight As Long
Private mvarMarginBottom As Long
Private mvarTileWidth As Long
Private mvarTileHeight As Long
Private mvarImage As String
Private mvarGenerateTileset As Boolean
Private mvarMatteColor As Long

Public Property Let GenerateTileset(ByVal vData As Boolean)
Attribute GenerateTileset.VB_Description = "If True, a new tileset containing tiles will be generated."
    mvarGenerateTileset = vData
End Property

Public Property Get GenerateTileset() As Boolean
    GenerateTileset = mvarGenerateTileset
End Property

Public Property Let MarginBottom(ByVal vData As Long)
Attribute MarginBottom.VB_Description = "The number of margin pixels on the bottom of the image."
    mvarMarginBottom = vData
End Property

Public Property Get MarginBottom() As Long
    MarginBottom = mvarMarginBottom
End Property

Public Property Let MarginRight(ByVal vData As Long)
Attribute MarginRight.VB_Description = "The number of margin pixels on the right of the image."
    mvarMarginRight = vData
End Property

Public Property Get MarginRight() As Long
    MarginRight = mvarMarginRight
End Property

Public Property Let MarginTop(ByVal vData As Long)
Attribute MarginTop.VB_Description = "The number of margin pixels on the top of the image."
    mvarMarginTop = vData
End Property

Public Property Get MarginTop() As Long
    MarginTop = mvarMarginTop
End Property

Public Property Let MarginLeft(ByVal vData As Long)
Attribute MarginLeft.VB_Description = "The number of margin pixels on the left of the image."
    mvarMarginLeft = vData
End Property

Public Property Get MarginLeft() As Long
    MarginLeft = mvarMarginLeft
End Property

Public Property Let MatteColor(ByVal vData As Long)
Attribute MatteColor.VB_Description = "The color that is shown as transparent in non-alpha-channel blit modes.{Color}"
    mvarMatteColor = vData
End Property

Public Property Get MatteColor() As Long
    MatteColor = mvarMatteColor
End Property

Public Property Let Image(ByVal vData As String)
Attribute Image.VB_Description = "The filename of the image.{ImagePath}"
    mvarImage = vData
End Property

Public Property Get Image() As String
    Image = mvarImage
End Property

Public Property Let TileHeight(ByVal vData As Long)
Attribute TileHeight.VB_Description = "The height of each cell in pixels, not including the grid."
    mvarTileHeight = vData
End Property

Public Property Get TileHeight() As Long
    TileHeight = mvarTileHeight
End Property

Public Property Let TileWidth(ByVal vData As Long)
Attribute TileWidth.VB_Description = "The width of each cell in pixels, not including the grid."
    mvarTileWidth = vData
End Property

Public Property Get TileWidth() As Long
    TileWidth = mvarTileWidth
End Property

Private Sub Class_Initialize()
On Error Resume Next
    TileWidth = 16
    TileHeight = 16
    MatteColor = 0
    GenerateTileset = True
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load mvarTileWidth
        .Load mvarTileHeight
        .Load mvarMatteColor
        .Load mvarImage
        .Load mvarMarginLeft
        .Load mvarMarginTop
        .Load mvarMarginRight
        .Load mvarMarginBottom
        .Load mvarGenerateTileset
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save mvarTileWidth
        .Save mvarTileHeight
        .Save mvarMatteColor
        .Save mvarImage
        .Save mvarMarginLeft
        .Save mvarMarginTop
        .Save mvarMarginRight
        .Save mvarMarginBottom
        .Save mvarGenerateTileset
    End With
End Sub


