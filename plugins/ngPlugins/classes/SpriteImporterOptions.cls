VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SpriteImporterOptions"
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
Private mvarCellWidth As Long
Private mvarCellHeight As Long
Private mvarAlignmentX As Single
Private mvarAlignmentY As Single
Private mvarFrameDelay As Long
Private mvarImage As String
Private mvarMatteColor As Long
Private mvarDuplicateFirstFrame As Boolean

Public Property Let DuplicateFirstFrame(ByVal vData As Boolean)
Attribute DuplicateFirstFrame.VB_Description = "Imports the first frame of each imported pose as both the stopped frame and the first animation frame."
    mvarDuplicateFirstFrame = vData
End Property

Public Property Get DuplicateFirstFrame() As Boolean
    DuplicateFirstFrame = mvarDuplicateFirstFrame
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

Public Property Let CellHeight(ByVal vData As Long)
Attribute CellHeight.VB_Description = "The height of each cell in pixels, not including the grid."
    mvarCellHeight = vData
End Property

Public Property Get CellHeight() As Long
    CellHeight = mvarCellHeight
End Property

Public Property Let CellWidth(ByVal vData As Long)
Attribute CellWidth.VB_Description = "The width of each cell in pixels, not including the grid."
    mvarCellWidth = vData
End Property

Public Property Get CellWidth() As Long
    CellWidth = mvarCellWidth
End Property

Public Property Let AlignmentX(ByVal vData As Single)
    mvarAlignmentX = vData
End Property

Public Property Get AlignmentX() As Single
    AlignmentX = mvarAlignmentX
End Property

Public Property Let AlignmentY(ByVal vData As Single)
    mvarAlignmentY = vData
End Property

Public Property Get AlignmentY() As Single
    AlignmentY = mvarAlignmentY
End Property

Public Property Let FrameDelay(ByVal vData As Long)
    mvarFrameDelay = vData
End Property

Public Property Get FrameDelay() As Long
    FrameDelay = mvarFrameDelay
End Property

Private Sub Class_Initialize()
    CellWidth = 16
    CellHeight = 32
    FrameDelay = 5
    AlignmentX = 8
    AlignmentY = 32
    MatteColor = 0
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load mvarCellWidth
        .Load mvarCellHeight
        .Load mvarFrameDelay
        .Load mvarAlignmentX
        .Load mvarAlignmentY
        .Load mvarMatteColor
        .Load mvarImage
        .Load mvarDuplicateFirstFrame
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save mvarCellWidth
        .Save mvarCellHeight
        .Save mvarFrameDelay
        .Save mvarAlignmentX
        .Save mvarAlignmentY
        .Save mvarMatteColor
        .Save mvarImage
        .Save mvarDuplicateFirstFrame
    End With
End Sub

