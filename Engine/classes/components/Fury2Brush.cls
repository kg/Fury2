VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Brush"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
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
Private m_intTiles() As Integer
Private m_strName As String

Public Property Get XCenter() As Long
On Error Resume Next
    XCenter = Floor(Me.Width / 2)
End Property

Public Property Get YCenter() As Long
On Error Resume Next
    YCenter = Floor(Me.Height / 2)
End Property

Public Function GetRectangle() As Fury2Rect
On Error Resume Next
    Set GetRectangle = F2Rect(0, 0, Me.Width, Me.Height, False)
End Function

Public Property Get Name() As String
    Name = m_strName
End Property

Public Property Let Name(NewName As String)
    m_strName = NewName
End Property

Public Property Get Width() As Long
On Error Resume Next
    Width = (UBound(m_intTiles, 1) - LBound(m_intTiles, 1)) + 1
End Property

Public Property Get Height() As Long
On Error Resume Next
    Height = (UBound(m_intTiles, 2) - LBound(m_intTiles, 2)) + 1
End Property

Public Property Get Tile(X, Y) As Integer
On Error Resume Next
    Tile = m_intTiles(ClipValue(X, LBound(m_intTiles, 1), UBound(m_intTiles, 1)), ClipValue(Y, LBound(m_intTiles, 1), UBound(m_intTiles, 1)))
End Property

Public Property Let Tile(X, Y, Value As Integer)
On Error Resume Next
    If X < LBound(m_intTiles, 1) Then Exit Property
    If Y < LBound(m_intTiles, 2) Then Exit Property
    If X > UBound(m_intTiles, 1) Then Exit Property
    If Y > UBound(m_intTiles, 2) Then Exit Property
    m_intTiles(X, Y) = Value
End Property

Public Property Get Tiles() As Integer()
    Tiles = m_intTiles
End Property

Public Property Let Tiles(NewTiles() As Integer)
    m_intTiles = NewTiles
End Property

Public Sub Resize(ByVal Width As Long, ByVal Height As Long)
On Error Resume Next
    If Width < 1 Then Width = 1
    If Height < 1 Then Height = 1
    ReDim m_intTiles(0 To Width - 1, 0 To Height - 1)
End Sub

Public Sub Fill(Value As Integer)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    For l_lngY = LBound(m_intTiles, 2) To UBound(m_intTiles, 2)
        For l_lngX = LBound(m_intTiles, 1) To UBound(m_intTiles, 1)
            m_intTiles(l_lngX, l_lngY) = Value
        Next l_lngX
    Next l_lngY
End Sub

Public Sub Draw(ByVal Map As Fury2Map, Layer As Long, Optional ByVal X As Long = 0, Optional ByVal Y As Long = 0, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal SourceX As Long = 0, Optional ByVal SourceY As Long = 0, Optional ByVal ScrollX As Long = 0, Optional ByVal ScrollY As Long = 0, Optional ByVal DrawTransparentValues As Boolean = True)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_intValue As Integer
    If Map Is Nothing Then Exit Sub
    If Width < 1 Then Width = Me.Width
    If Height < 1 Then Height = Me.Height
    If Width > Me.Width Then Width = Me.Width
    If Height > Me.Height Then Height = Me.Height
    If (X < 0) Then
        SourceX = SourceX - X
        Width = Width + X
        'x = 0
    End If
    If (Y < 0) Then
        SourceY = SourceY - Y
        Height = Height + Y
        'y = 0
    End If
    If ScrollX < 0 Then
        ScrollX = Me.Width - (Abs(ScrollX) Mod Me.Width)
    Else
        ScrollX = (ScrollX Mod Me.Width)
    End If
    If ScrollY < 0 Then
        ScrollY = Me.Height - (Abs(ScrollY) Mod Me.Height)
    Else
        ScrollY = (ScrollY Mod Me.Height)
    End If
    SourceX = ClipValue(SourceX, 0, Me.Width - Width)
    SourceY = ClipValue(SourceY, 0, Me.Height - Height)
    Width = ClipValue(Width, 0, ClipValue(Map.Width - X, 0, Me.Width - SourceX))
    Height = ClipValue(Height, 0, ClipValue(Map.Height - Y, 0, Me.Height - SourceY))
    If Width < 1 Or Height < 1 Then Exit Sub
    With Map.Layers(Layer)
        For l_lngY = SourceY To SourceY + Height - 1
            For l_lngX = SourceX To SourceX + Width - 1
                l_intValue = m_intTiles(WrapValue(l_lngX + ScrollX, 0, Me.Width - 1), WrapValue(l_lngY + ScrollY, 0, Me.Height - 1))
                If (l_intValue <> -1) Or (DrawTransparentValues = True) Then
                    .Tile(l_lngX + X, l_lngY + Y) = l_intValue
                End If
            Next l_lngX
        Next l_lngY
    End With
End Sub

Public Sub Grab(ByVal Map As Fury2Map, Layer As Long, Optional ByVal X As Long = 0, Optional ByVal Y As Long = 0, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal SourceX As Long = 0, Optional ByVal SourceY As Long = 0, Optional SizeToFit As Boolean = False)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    If Map Is Nothing Then Exit Sub
    If Width < 1 Then Width = Me.Width
    If Height < 1 Then Height = Me.Height
    If (X < 0) Then
        SourceX = SourceX - X
        Width = Width + X
        X = 0
    End If
    If (Y < 0) Then
        SourceY = SourceY - Y
        Height = Height + Y
        Y = 0
    End If
    If SizeToFit Then Resize Width, Height
    Width = ClipValue(Width, 0, ClipValue(Map.Width - X, 0, Me.Width - SourceX))
    Height = ClipValue(Height, 0, ClipValue(Map.Height - Y, 0, Me.Height - SourceY))
    If Width < 1 Or Height < 1 Then Exit Sub
    With Map.Layers(Layer)
        For l_lngY = SourceY To SourceY + Height - 1
            For l_lngX = SourceX To SourceX + Width - 1
                m_intTiles(l_lngX, l_lngY) = .Tile(l_lngX + X, l_lngY + Y)
            Next l_lngX
        Next l_lngY
    End With
End Sub

Public Sub GrabFromArray(ByRef TileArray() As Integer, Optional ByVal X As Long = 0, Optional ByVal Y As Long = 0, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal SourceX As Long = 0, Optional ByVal SourceY As Long = 0, Optional SizeToFit As Boolean = False)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngArrayWidth As Long, l_lngArrayHeight As Long
    If Width < 1 Then Width = Me.Width
    If Height < 1 Then Height = Me.Height
    If (X < 0) Then
        SourceX = SourceX - X
        Width = Width + X
        X = 0
    End If
    If (Y < 0) Then
        SourceY = SourceY - Y
        Height = Height + Y
        Y = 0
    End If
    l_lngArrayWidth = UBound(TileArray, 1) + 1
    l_lngArrayHeight = UBound(TileArray, 2) + 1
    If SizeToFit Then Resize Width, Height
    Width = ClipValue(Width, 0, ClipValue(l_lngArrayWidth - X, 0, l_lngArrayWidth - SourceX))
    Height = ClipValue(Height, 0, ClipValue(l_lngArrayHeight - Y, 0, l_lngArrayHeight - SourceY))
    If Width < 1 Or Height < 1 Then Exit Sub
        For l_lngY = SourceY To SourceY + Height - 1
            For l_lngX = SourceX To SourceX + Width - 1
                m_intTiles(l_lngX, l_lngY) = TileArray(l_lngX + X, l_lngY + Y)
            Next l_lngX
        Next l_lngY
End Sub

Public Sub Render(ByVal Image As Fury2Image, ByVal Layer As Fury2MapLayer, Optional ByVal Alpha As Single = 1)
On Error Resume Next
Dim l_tmpTilemap As TilemapLayerParam
Dim l_camCamera As CameraParam
Dim l_tstTileset As Fury2Tileset
Dim l_lngTileWidth As Long, l_lngTileHeight As Long
Dim l_lngBrushWidth As Long, l_lngBrushHeight As Long
Dim l_lngTargets(0 To 0) As Long
    If Layer Is Nothing Then Exit Sub
    If Layer.Tileset Is Nothing Then Exit Sub
    l_lngTileWidth = Layer.Tileset.TileWidth
    l_lngTileHeight = Layer.Tileset.TileHeight
    l_lngBrushWidth = Me.Width * l_lngTileWidth
    l_lngBrushHeight = Me.Height * l_lngTileHeight
    With l_camCamera
        .Rectangle = Image.Rectangle.GetRectangle
        .Alpha = 255
        l_lngTargets(0) = Image.Handle
        .pRenderTargets = VarPtr(l_lngTargets(0))
        .RenderTargetCount = 1
        .ViewportX = 0
        .ViewportY = 0
    End With
    With l_tmpTilemap
        .Alpha = 255 * Alpha
        .Effect = Layer.Effect
        .MaskedTile = Layer.Tileset.TransparentTile
        .pAnimationMap = 0
        .pData = VarPtr(m_intTiles(0, 0))
        .pTileset = Layer.Tileset.Handle
        .Width = Me.Width
        .Height = Me.Height
        .x1 = 0
        .y1 = 0
        .x2 = Me.Width
        .y2 = Me.Height
    End With
    RenderTilemapLayer l_tmpTilemap, l_camCamera
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    ReDim m_intTiles(0 To 0, 0 To 0)
    m_strName = "Untitled"
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long
    With File
        .Load m_strName
        .Load l_lngWidth
        .Load l_lngHeight
        Resize l_lngWidth, l_lngHeight
        .RawLoad VarPtr(m_intTiles(0, 0)), Width * Height * 2
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save m_strName
        .Save Width
        .Save Height
        .RawSave VarPtr(m_intTiles(0, 0)), Width * Height * 2
    End With
End Sub
