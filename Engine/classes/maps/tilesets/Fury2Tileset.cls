VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Tileset"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Tileset Object"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    Engine (Fury² Game Creation System Runtime Engine)
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

Implements IInspectable
Implements Fury2Object
Implements IVirtualFileSaveable

Public Handle As Long
Attribute Handle.VB_VarDescription = "*Internal value"
Private m_mapMap As Fury2Map
Private m_booOwnsTileset As Boolean
Private m_strOldFilename As String
Private mvarTSFilename As String
Private mvarTilesPerRow As Long
Private mvarTilesPerColumn As Long
Private mvarTileCount As Long
Private mvarFilename As String
Private mvarBuffer As Fury2Image
Private mvarTileWidth As Long
Private mvarTileHeight As Long
Private mvarMaskColor As Long
Private mvarHasAlpha As Boolean
Private mvarTransparentTile As Long

Friend Sub SetData(Width As Long, Height As Long)
On Error Resume Next
    mvarTileWidth = Width
    mvarTileHeight = Height
    RecalculateSizes
End Sub

Public Sub DeallocateTileset()
Attribute DeallocateTileset.VB_MemberFlags = "40"
On Error Resume Next
    If (Handle <> 0) Then
        If CLng(mvarBuffer.Tag(1)) <= 1 Then
            'm_Engine.TextOut "Deallocated " & mvarBuffer.Name & " (" & m_Engine.ToString(mvarBuffer.Tag) & ")"
            SoftFX.DeallocateTileset Handle
            mvarBuffer.Tag = Array(0, 0)
        Else
            'm_Engine.TextOut "Released " & mvarBuffer.Name & " (" & m_Engine.ToString(mvarBuffer.Tag) & ")"
            mvarBuffer.Tag = Array(mvarBuffer.Tag(0), mvarBuffer.Tag(1) - 1)
        End If
        Handle = 0
    End If
End Sub

Public Sub AllocateTileset(Optional ForceNew As Boolean = False)
Attribute AllocateTileset.VB_MemberFlags = "40"
On Error Resume Next
Dim l_lngLayers As Long
    If Handle <> 0 Then DeallocateTileset
    If m_booEditor Then ForceNew = True
    If mvarBuffer Is Nothing Then
        Load
    End If
    If mvarBuffer.Handle = 0 Then
        Exit Sub
    End If
    If Handle = 0 Then
        Handle = 0
        Handle = CLng(mvarBuffer.Tag(0))
        Err.Clear
        If (Handle = 0) Or (ForceNew) Then
            Handle = SoftFX.AllocateTileset(mvarBuffer.Handle, mvarTileWidth, mvarTileHeight)
            If Handle = 0 Then
            Else
                mvarBuffer.Tag = Array(Handle, 1)
            End If
            'm_Engine.TextOut "Allocated " & mvarBuffer.Name & " (" & m_Engine.ToString(mvarBuffer.Tag) & ")"
        Else
            mvarBuffer.Tag = Array(mvarBuffer.Tag(0), mvarBuffer.Tag(1) + 1)
            Handle = CLng(mvarBuffer.Tag(0))
            'm_Engine.TextOut "Referenced " & mvarBuffer.Name & " (" & m_Engine.ToString(mvarBuffer.Tag) & ")"
        End If
    End If
End Sub

Sub Free()
On Error Resume Next
    DeallocateTileset
    Set mvarBuffer = New Fury2Image
End Sub

Public Function TSFilename() As String
    TSFilename = mvarTSFilename
End Function

Friend Sub SetTSFilename(St As String)
    mvarTSFilename = St
End Sub

Public Function Duplicate() As Fury2Tileset
On Error Resume Next
Dim m_tstNew As Fury2Tileset
    Set m_tstNew = New Fury2Tileset
    With m_tstNew
        .Filename = mvarFilename
        .SetTSFilename mvarTSFilename
        .HasAlpha = mvarHasAlpha
        Err.Clear
        .MaskColor = Buffer.MaskColor
        If Err.Number <> 0 Then .MaskColor = mvarMaskColor
        .TransparentTile = mvarTransparentTile
        .SetData mvarTileWidth, mvarTileHeight
        .Load
    End With
    Set Duplicate = m_tstNew
    Set m_tstNew = Nothing
End Function

Public Sub RecalculateSizes()
On Error Resume Next
    If mvarBuffer Is Nothing Then Exit Sub
    With mvarBuffer
        If mvarTileWidth >= .Width Then
            mvarTilesPerRow = 1
        Else
            mvarTilesPerRow = .Width \ mvarTileWidth
        End If
        If mvarTileHeight >= .Height Then
            mvarTilesPerColumn = 1
        Else
            mvarTilesPerColumn = .Height \ mvarTileHeight
        End If
        If mvarTileHeight >= .Height Then
            If mvarTileWidth >= .Width Then
                mvarTileCount = 1
            Else
                mvarTileCount = (.Width \ mvarTileWidth)
            End If
        Else
            If mvarTileWidth >= .Width Then
                mvarTileCount = 1
            Else
                mvarTileCount = (.Width \ mvarTileWidth) * (.Height \ mvarTileHeight)
            End If
        End If
    End With
End Sub

Public Property Let TransparentTile(ByVal vData As Long)
Attribute TransparentTile.VB_Description = "The tile index in this tileset which will be invisible."
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.TransparentTile = 5
    mvarTransparentTile = vData
End Property

Public Property Get TransparentTile() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.TransparentTile
    TransparentTile = mvarTransparentTile
End Property

Public Property Let HasAlpha(ByVal vData As Boolean)
Attribute HasAlpha.VB_Description = "This flag determines whether or not the tileset's alpha channel is enabled."
    mvarHasAlpha = vData
End Property

Public Property Get HasAlpha() As Boolean
    HasAlpha = mvarHasAlpha
End Property

Public Property Get MaskColor() As Long
Attribute MaskColor.VB_Description = "The color in the tileset which will be transparent.{Color}"
On Error Resume Next
    MaskColor = mvarBuffer.MaskColor
End Property

Public Property Let MaskColor(ByVal vData As Long)
On Error Resume Next
    mvarBuffer.MaskColor = vData
    mvarMaskColor = vData
End Property

Public Property Let TileHeight(ByVal vData As Long)
Attribute TileHeight.VB_Description = "The height of a tile."
On Error Resume Next
    mvarTileHeight = ClipValue(vData, 2, 256)
    RecalculateSizes
    DeallocateTileset
    AllocateTileset True
End Property

Public Property Get TileHeight() As Long
    TileHeight = mvarTileHeight
End Property

Public Property Let TileWidth(ByVal vData As Long)
Attribute TileWidth.VB_Description = "The width of a tile."
On Error Resume Next
    mvarTileWidth = ClipValue(vData, 2, 256)
    RecalculateSizes
    DeallocateTileset
    AllocateTileset True
End Property

Public Property Get TileWidth() As Long
    TileWidth = mvarTileWidth
End Property

Public Property Set Buffer(vData As Fury2Image)
Attribute Buffer.VB_Description = "*The image containing the tileset's tiles."
    Set mvarBuffer = vData
End Property

Public Property Get Buffer() As Fury2Image
    Set Buffer = mvarBuffer
End Property

Sub Reload()
On Error Resume Next
    m_Engine.ReLoadPicture mvarFilename
    Set mvarBuffer = m_Engine.LoadImage(mvarFilename)
    AllocateTileset
    mvarBuffer.MaskColor = mvarMaskColor
    With mvarBuffer
        RecalculateSizes
    End With
End Sub

Public Sub Render(Surface As Fury2Image, Optional Area As Fury2Rect = Nothing, Optional Alpha As Single = 1, Optional XOffset As Long = 0, Optional YOffset As Long = 0)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngTile As Long, l_rctDest As Fury2Rect, l_rctSource As Fury2Rect
Dim l_imgHandle As Fury2Image
    If Handle = 0 Then Exit Sub
    If Surface Is Nothing Then Exit Sub
    If Area Is Nothing Then Set Area = Surface.Rectangle
    l_lngX = Area.Left - XOffset
    l_lngY = Area.Top - YOffset
    Set l_rctDest = New Fury2Rect
    Set l_rctSource = F2Rect(0, 0, mvarTileWidth, mvarTileHeight, False)
    For l_lngTile = 0 To mvarTileCount - 1
        If (l_lngY + mvarTileHeight) >= Area.Top Then
            If (l_lngY < Area.Bottom) Then
                l_rctDest.SetValues l_lngX, l_lngY, l_lngX + mvarTileWidth, l_lngY + mvarTileHeight
                l_imgHandle.SetHandle SoftFX.GetTile(Handle, l_lngTile)
                Surface.Blit l_rctDest, l_rctSource, l_imgHandle, Alpha, BlitMode_Normal
            End If
        End If
        l_lngX = l_lngX + mvarTileWidth
        If (l_lngX + mvarTileWidth) > Area.Right Then
            l_lngX = Area.Left - XOffset
            l_lngY = l_lngY + mvarTileHeight
        End If
        If l_lngY > Area.Bottom Then
            Exit For
        End If
    Next l_lngTile
End Sub

Sub Load(Optional DetectMask As Boolean = False)
On Error Resume Next
Dim m_glbGlobals As Fury2Globals
Dim m_strDir As String
Dim l_lngIndex As Long
Dim l_booNew As Boolean
Static m_lngRecurse As Long
    If Trim(mvarFilename) = "" Then
        Exit Sub
    End If
    m_Engine.ContextLevelAdd "Tileset.Load(""" + CStr(mvarFilename) + """)"
    m_lngRecurse = m_lngRecurse + 1
    If LCase(Right(Trim(mvarFilename), 4)) = "f2ts" Then
        Set m_glbGlobals = New Fury2Globals
        m_strOldFilename = mvarFilename
        Err.Clear
'        If FileLen(mvarFilename) = 0 Then CriticalError "Fury2Tileset.Load", "Sanity Check", "Tileset """ + mvarFilename + """ does not exist or is empty.": m_lngRecurse = m_lngRecurse - 1: Exit Sub
        m_Engine.LoadFromFile Me, m_Engine.Filesystem.File(mvarFilename)
        Set m_glbGlobals = Nothing
    Else
        If m_booEditor Then
            Set mvarBuffer = m_Engine.LoadImage(mvarFilename)
        Else
            l_lngIndex = m_Engine.iLoadImage(mvarFilename)
            If l_lngIndex = m_Engine.Images.Count Then
                l_booNew = True
            End If
            Set mvarBuffer = m_Engine.Images(l_lngIndex)
        End If
        If mvarBuffer Is Nothing Then CriticalError "Fury2Tileset.Load", "Sanity Check", "Tileset """ + mvarFilename + """ could not be loaded.": m_lngRecurse = m_lngRecurse - 1: Exit Sub
        If mvarBuffer.Width <= 0 Or mvarBuffer.Height <= 0 Then CriticalError "Fury2Tileset.Load", "Sanity Check", "Tileset """ + mvarFilename + """ could not be loaded.": m_lngRecurse = m_lngRecurse - 1: Exit Sub
        mvarBuffer.MaskColor = mvarMaskColor
        With mvarBuffer
            RecalculateSizes
            If DetectMask Then
                .MaskColor = .GetPixel(.Width - 1, .Height - 1)
                mvarMaskColor = .MaskColor
            Else
            End If
        End With
        AllocateTileset
        If m_strOldFilename <> "" Then
            mvarTSFilename = mvarFilename
            mvarFilename = m_strOldFilename
            m_strOldFilename = ""
        Else
            mvarTSFilename = mvarFilename
        End If
    End If
    m_lngRecurse = m_lngRecurse - 1
    m_Engine.ContextLevelRemove
End Sub

Public Property Let Filename(ByVal vData As String)
Attribute Filename.VB_Description = "The name of the image the tileset's tiles will be loaded from.{ImagePath}"
On Error Resume Next
    mvarFilename = vData
    If LCase(Right(Trim(mvarFilename), 4)) = "f2ts" Then
    Else
        mvarTSFilename = vData
    End If
    If (Buffer Is Nothing) Then
    Else
        Set Buffer = Nothing
    End If
    If (Handle = 0) Then
    Else
        DeallocateTileset
        Load
        RecalculateSizes
        AllocateTileset True
    End If
End Property

Public Property Get Filename() As String
    Filename = mvarFilename
End Property

Public Property Get Tile(ByVal Index As Long) As Fury2Image
On Error Resume Next
    If Handle <> 0 Then
        Set Tile = New Fury2Image
        Tile.Deallocate
        Tile.SetHandle SoftFX.GetTile(Handle, Index)
    End If
End Property

Public Property Get TileCount() As Long
Attribute TileCount.VB_Description = "The number of tiles in the tileset."
    TileCount = mvarTileCount
End Property

Public Property Get TilesPerColumn() As Long
Attribute TilesPerColumn.VB_Description = "The number of rows in the tileset."
    TilesPerColumn = mvarTilesPerColumn
End Property

Public Property Get TilesPerRow() As Long
Attribute TilesPerRow.VB_Description = "The number of columns in the tileset."
    TilesPerRow = mvarTilesPerRow
End Property

Public Property Get Height() As Long
Attribute Height.VB_Description = "The height of the tileset's source image."
    Height = mvarBuffer.Height
End Property

Public Property Get Width() As Long
Attribute Width.VB_Description = "The width of the tileset's source image."
    Width = mvarBuffer.Width
End Property

Private Sub Class_Initialize()
On Error Resume Next
    If mvarBuffer Is Nothing Then
        Set mvarBuffer = New Fury2Image
        mvarBuffer.Resize 1, 1
    End If
    mvarTileWidth = 16
    mvarTileHeight = 16
    mvarTransparentTile = -1
    mvarMaskColor = c_lngNullColor
    mvarBuffer.MaskColor = c_lngNullColor
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    DeallocateTileset
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarFilename
            .Save mvarTileWidth
            .Save mvarTileHeight
            .Save mvarMaskColor
            .Save mvarHasAlpha
            .Save mvarTileCount
            .Save mvarTilesPerRow
            .Save mvarTilesPerColumn
            .Save mvarTransparentTile
        .WriteSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As VirtualFile)
On Error Resume Next
    mvarTransparentTile = -1
    With File
        .ReadSegment_Begin
            .Load mvarFilename
            .Load mvarTileWidth
            .Load mvarTileHeight
            .Load mvarMaskColor
            .Load mvarHasAlpha
            .Load mvarTileCount
            .Load mvarTilesPerRow
            .Load mvarTilesPerColumn
            .Load mvarTransparentTile
        .ReadSegment_End
    End With
    If mvarBuffer Is Nothing Then
        Set mvarBuffer = New Fury2Image
        mvarBuffer.MaskColor = mvarMaskColor
    Else
        mvarBuffer.MaskColor = mvarMaskColor
    End If
    Load
    Err.Clear
End Sub
