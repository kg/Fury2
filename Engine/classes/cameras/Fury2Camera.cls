VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Camera"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Camera (Viewport) Object"
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
Implements Fury2Object
Implements IVirtualFileSaveable

Public LightingScaleRatio As Single
Public EnableParallax As Boolean
Public EnableWrapping As Boolean
Public EnableLighting As Boolean
Public AutoTintLayers As Boolean
Public ShowSprites As Boolean
Public ShowTiles As Boolean
Public DisableBuffer As Boolean

Private mvarLightmapScratch As Fury2Image
Private mvarLightmap As Fury2Image
Private mvarBuffer As Fury2Image
Private mvarWidth As Long
Private mvarHeight As Long
Private mvarSubthreaded As Boolean
Private mvarViewportXTarget As Single
Private mvarViewportYTarget As Single
Private mvarViewportXVelocity As Single
Private mvarViewportYVelocity As Single
Private mvarViewportX As Single
Private mvarViewportY As Single
Private mvarViewingMap As Fury2Map
Private mvarX As Long
Private mvarY As Long
Private mvarOldX As Long, mvarOldY As Long
Private mvarXBuffer As Long, mvarYBuffer As Long
Private mvarDirty As Boolean
Private mvarAlpha As Single
Private mvarGamma As Long
Private mvarMaskColor As Long
Private mvarTransparent As Boolean
Private mvarMap As Long
Private mvarConstrainToMap As Boolean
Private mvarUnGhostLayer As Long

Public Property Get UnGhostLayer() As Long
On Error Resume Next
    UnGhostLayer = mvarUnGhostLayer
End Property

Public Property Let UnGhostLayer(NewLayer As Long)
On Error Resume Next
    mvarUnGhostLayer = ClipValue(NewLayer, 0, mvarViewingMap.Layers.Count)
End Property

Public Function XPad() As Long
On Error Resume Next
    XPad = 0
End Function

Public Function YPad() As Long
On Error Resume Next
    YPad = 0
End Function

Public Sub SetMap(Obj As Fury2Map)
On Error Resume Next
    Set mvarViewingMap = Obj
    mvarMap = 0
End Sub

Public Property Get Gamma() As Long
    Gamma = mvarGamma
End Property

Public Property Let Gamma(NewGamma As Long)
On Error Resume Next
    mvarGamma = ClipValue(NewGamma, -255, 255)
End Property

Public Property Get ConstrainToMap() As Boolean
    ConstrainToMap = mvarConstrainToMap
End Property

Public Property Let ConstrainToMap(NewValue As Boolean)
    mvarConstrainToMap = NewValue
End Property

Public Property Get Dirty() As Boolean
    Dirty = mvarDirty
End Property

Public Property Let Dirty(NewDirty As Boolean)
    mvarDirty = NewDirty
End Property

'Public Function PixelsToTilesX(x)
'On Error Resume Next
'    If (x < 0) Or (x >= (mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth)) Then
'        PixelsToTilesX = -1
'    Else
'        PixelsToTilesX = (x) \ (mvarViewingMap.Layers(1).Tileset.TileWidth)
'    End If
'End Function
'
'Public Function PixelsToTilesY(y)
'On Error Resume Next
'    If (y < 0) Or (y >= (mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight)) Then
'        PixelsToTilesY = -1
'    Else
'        PixelsToTilesY = (y) \ (mvarViewingMap.Layers(1).Tileset.TileHeight)
'    End If
'End Function
'
'Public Function TilesToPixelsX(x)
'On Error Resume Next
'    TilesToPixelsX = ((x) * (mvarViewingMap.Layers(1).Tileset.TileWidth))
'End Function
'
'Public Function TilesToPixelsY(y)
'On Error Resume Next
'    TilesToPixelsY = ((y) * (mvarViewingMap.Layers(1).Tileset.TileHeight))
'End Function

Sub FlushBuffer()
On Error Resume Next
Dim m_sngNewX As Single, m_sngNewY As Single, m_booAnimated As Boolean
Dim m_lngLayers As Long
Dim m_lngRects As Long
Dim m_rctObj As Fury2Rect
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    If (m_Engine.Cameras.Count = 1) And (m_Engine.DisableCameraBuffers) Then Exit Sub
    For m_lngLayers = 1 To mvarViewingMap.Layers.Count
        m_booAnimated = m_booAnimated Or (mvarViewingMap.Layers(m_lngLayers).TileAnimations.Count > 0)
    Next m_lngLayers
    m_sngNewX = mvarViewportX
    m_sngNewY = mvarViewportY
    If (((mvarOldX <> m_sngNewX) Or (mvarOldY <> m_sngNewY)) Or (mvarDirty = True)) Then
        Redraw
        mvarXBuffer = 0
        mvarYBuffer = 0
    End If
    mvarOldX = m_sngNewX
    mvarOldY = m_sngNewY
End Sub

Sub Init()
On Error Resume Next
    Err.Clear
    If mvarMap <> 0 Then Set mvarViewingMap = m_Engine.Maps(mvarMap)
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count <= 0 Then Exit Sub
    If ((m_Engine.DisableCameraBuffers = True) And (m_Engine.Cameras.Count = 1)) Or (DisableBuffer) Then
    Else
        Set mvarBuffer = F2Image(mvarWidth, mvarHeight)
        If Err.Number <> 0 Then CriticalError "Fury2Camera.Init", "Set mvarBuffer = F2Image(mvarWidth, mvarHeight)"
    End If
    mvarDirty = True
    Err.Clear
End Sub

Public Property Get Alpha() As Single
    Alpha = mvarAlpha
End Property

Public Property Let Alpha(ByVal NewAlpha As Single)
On Error Resume Next
    mvarAlpha = Abs(NewAlpha)
    If mvarAlpha > 1 Then mvarAlpha = 1
End Property

Public Property Let Y(ByVal vData As Long)
    mvarY = ClipValue(vData, 0, m_Engine.ScreenHeight - mvarHeight)
End Property

Public Property Get Y() As Long
    Y = mvarY
End Property

Public Property Let X(ByVal vData As Long)
    mvarX = ClipValue(vData, 0, m_Engine.ScreenWidth - mvarWidth)
End Property

Public Property Get X() As Long
    X = mvarX
End Property

Public Property Let Map(NewMap As Long)
On Error Resume Next
    mvarMap = NewMap
    If mvarMap = 0 Then
        Set mvarViewingMap = Nothing
    Else
        Set mvarViewingMap = m_Engine.Maps(NewMap)
    End If
End Property

Public Property Get Map() As Long
    Map = mvarMap
End Property

Public Property Get ViewingMap() As Fury2Map
    Set ViewingMap = mvarViewingMap
End Property

Friend Property Set ViewingMap(NewMap As Fury2Map)
    Set mvarViewingMap = NewMap
End Property

Public Property Get ViewportRectangle() As Fury2Rect
On Error Resume Next
    Set ViewportRectangle = F2Rect(ViewportX, ViewportY, Width, Height, False)
End Property

Public Sub Move(ByVal XDistance As Single, ByVal YDistance As Single)
On Error Resume Next
Dim m_sngOldX As Single, m_sngOldY As Single
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    If XDistance = 0 And YDistance = 0 Then Exit Sub
    m_sngOldX = mvarViewportX
    m_sngOldY = mvarViewportY
    mvarViewportX = mvarViewportX + XDistance
    mvarViewportY = mvarViewportY + YDistance
    mvarXBuffer = mvarXBuffer + XDistance
    mvarYBuffer = mvarYBuffer + YDistance
    If mvarConstrainToMap Then
        If mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth <= mvarWidth Then
            mvarViewportX = -(mvarWidth - (mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth)) \ 2
        Else
            mvarViewportX = m_Engine.ClipNumber(mvarViewportX, 0, Abs((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) - mvarWidth))
        End If
        If mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight <= mvarHeight Then
            mvarViewportY = -(mvarHeight - (mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight)) \ 2
        Else
            mvarViewportY = m_Engine.ClipNumber(mvarViewportY, 0, Abs((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) - mvarHeight))
        End If
    End If
End Sub

Public Sub CenterOn(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
Dim m_sngNewX As Single, m_sngNewY As Single
    m_sngNewX = X - (mvarWidth \ 2)
    m_sngNewY = Y - (mvarHeight \ 2)
    Move m_sngNewX - mvarViewportX, m_sngNewY - mvarViewportY
End Sub

Public Sub ScrollTo(ByVal NewX As Single, ByVal NewY As Single, Optional ByVal Speed As Single = 1)
On Error Resume Next
    mvarViewportXVelocity = Speed
    mvarViewportYVelocity = Speed
    mvarViewportXTarget = X
    mvarViewportYTarget = Y
    mvarSubthreaded = True
    m_Engine.Subthread
End Sub

Public Sub Refresh()
On Error Resume Next
Dim m_lngEffects As Long
    If mvarMap <> 0 Then Set mvarViewingMap = m_Engine.Maps(mvarMap)
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    If (m_Engine.Cameras.Count = 1) And (m_Engine.DisableCameraBuffers) Then
        If mvarAlpha <= 0 Then Exit Sub
        Render , m_Engine.Backbuffer
        Exit Sub
    End If
    If mvarBuffer Is Nothing Then Exit Sub
    If mvarBuffer.Width < mvarWidth Then Exit Sub
    If mvarBuffer.Height < mvarHeight Then Exit Sub
    If mvarBuffer.Width <= 0 Then Exit Sub
    If mvarBuffer.Height <= 0 Then Exit Sub
    If mvarDirty Then FlushBuffer
    Err.Clear
    If mvarX < 0 Then mvarX = 0
    If mvarY < 0 Then mvarY = 0
    If mvarWidth > m_Backbuffer.Width - mvarX Then mvarWidth = m_Backbuffer.Width - mvarX
    If mvarHeight > m_Backbuffer.Height - mvarY Then mvarHeight = m_Backbuffer.Height - mvarY
    Set m_Backbuffer.ClipRectangle = F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False)
    If mvarAlpha = 0 Then
    ElseIf mvarAlpha = 1 Then
        'If (mvarGamma = 0) Or (m_Engine.SpecialFX = False) Then
            m_Backbuffer.Blit F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), mvarBuffer.Rectangle, mvarBuffer, 1, BlitMode_Normal
            If Err.Number <> 0 Then m_Backbuffer.Fill F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), F2Black
        'Else
        '    m_Backbuffer.Blit F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), mvarBuffer.Rectangle, mvarBuffer, ((mvarGamma) / 255), BlitMode_Matte_Tint, F2White
        '    If Err.Number <> 0 Then m_Backbuffer.Fill F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), F2Black
        'End If
    Else
        m_Backbuffer.Blit F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), mvarBuffer.Rectangle, mvarBuffer, mvarAlpha, BlitMode_Normal
        If Err.Number <> 0 Then m_Backbuffer.Fill F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), F2RGB(0, 0, 0, mvarAlpha * 255)
    End If
    Set m_Backbuffer.ClipRectangle = m_Backbuffer.Rectangle
End Sub

Public Sub CopyToDC(hdc As Long)
    CopyImageToDC hdc, F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), mvarBuffer
End Sub

Private Function SelectTintColor(ByVal Index As Long, ByVal Count As Long) As Long
On Error Resume Next
    Select Case Index Mod 6
    Case 0
        SelectTintColor = BGRA(160, 0, 0, 127)
    Case 1
        SelectTintColor = BGRA(160, 160, 0, 127)
    Case 2
        SelectTintColor = BGRA(0, 160, 0, 127)
    Case 3
        SelectTintColor = BGRA(0, 160, 160, 127)
    Case 4
        SelectTintColor = BGRA(0, 0, 160, 127)
    Case Else
        SelectTintColor = BGRA(160, 0, 160, 127)
    End Select
End Function

Public Sub Render(Optional Area As Fury2Rect, Optional ByVal Output As Fury2Image = Nothing)
On Error Resume Next
Dim cX As Long, cY As Long, cTile As Long
Dim StartX As Long, StartY As Long, EndX As Long, EndY As Long
Dim Layers As Long, XOffset As Long, YOffset As Long
Dim Tilemap As TilemapLayerParam
Dim CameraParam As CameraParam
Dim DestTileRect As Fury2Rect, SourceTileRect As Fury2Rect
Dim m_lngViewX As Long, m_lngViewY As Long
Dim m_lngXOff As Long, m_lngYOff As Long
Dim m_lngVPX As Long, m_lngVPY As Long
Dim m_lngBGX As Long, m_lngBGY As Long
Dim FullRedraw As Boolean, m_lngAreas As Long
Dim m_rctSprite As Fury2Rect
Dim m_lngSprites As Long, m_lngPath As Long
Dim gr As Win32.Rect
Dim m_lnLines() As FLine, m_lngLines As Long
Dim l_imgBuffer As Fury2Image
Dim l_sngAlpha As Single
Dim l_lcaCamera As LightingCamera
Dim mvarViewportTrueX As Long, mvarViewportTrueY As Long
    Err.Clear
    If mvarMap <> 0 Then Set mvarViewingMap = m_Engine.Maps(mvarMap)
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    Set l_imgBuffer = Output
    If Not l_imgBuffer Is Nothing Then
        If (l_imgBuffer.Width < 1) Or (l_imgBuffer.Height < 1) Or (l_imgBuffer.Handle = 0) Then
            Set l_imgBuffer = Nothing
        End If
    End If
    If l_imgBuffer Is Nothing Then
        If (m_Engine.Cameras.Count = 1) And (m_Engine.DisableCameraBuffers) Then
            Set l_imgBuffer = m_Engine.Backbuffer
        Else
            Set l_imgBuffer = mvarBuffer
        End If
    End If
    If EnableLighting Then
        If mvarLightmap Is Nothing Then
            Set mvarLightmap = m_Engine.CreateHardwareImage(Ceil(l_imgBuffer.Width * LightingScaleRatio), Ceil(l_imgBuffer.Height * LightingScaleRatio))
        End If
        If mvarLightmapScratch Is Nothing Then
            Set mvarLightmapScratch = m_Engine.CreateHardwareImage(mvarLightmap.Width, mvarLightmap.Height)
        End If
        If (mvarLightmap.Width <> Ceil(l_imgBuffer.Width * LightingScaleRatio)) Or (mvarLightmap.Height <> Ceil(l_imgBuffer.Height * LightingScaleRatio)) Then
            Set mvarLightmap = m_Engine.CreateHardwareImage(Ceil(l_imgBuffer.Width * LightingScaleRatio), Ceil(l_imgBuffer.Height * LightingScaleRatio))
        End If
        If (mvarLightmapScratch.Width <> mvarLightmap.Width) Or (mvarLightmapScratch.Height <> mvarLightmap.Height) Then
            Set mvarLightmapScratch = m_Engine.CreateHardwareImage(mvarLightmap.Width, mvarLightmap.Height)
        End If
    End If
    With mvarViewingMap.Layers(1)
    With CameraParam
        .Alpha = Me.Alpha * 255
        .pImage = l_imgBuffer.Handle
        .Rectangle = l_imgBuffer.Rectangle.GetRectangle
    End With
    If (l_imgBuffer Is mvarBuffer) And (Output Is Nothing) Then
        If (mvarBuffer.Width <> mvarWidth) Or (mvarBuffer.Height <> mvarHeight) Then
            Init
        End If
    End If
    If Area Is Nothing Then
        If mvarConstrainToMap Then
            If mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth <= mvarWidth Then
                mvarViewportX = -(mvarWidth - (mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth)) \ 2
            Else
                mvarViewportX = m_Engine.ClipNumber(mvarViewportX, 0, Abs((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) - mvarWidth))
            End If
            If mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight <= mvarHeight Then
                mvarViewportY = -(mvarHeight - (mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight)) \ 2
            Else
                mvarViewportY = m_Engine.ClipNumber(mvarViewportY, 0, Abs((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) - mvarHeight))
            End If
        End If
        FullRedraw = True
        Set Area = F2Rect(0, 0, l_imgBuffer.Width, l_imgBuffer.Height, False)
        If (m_Engine.ClearMapBG) Or ((l_imgBuffer Is m_Engine.Backbuffer) And (((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) < l_imgBuffer.Width) Or ((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) < l_imgBuffer.Height))) Then
            If Output Is Nothing Then l_imgBuffer.Clear 0
        End If
    Else
        If (m_Engine.ClearMapBG) Or ((l_imgBuffer Is m_Engine.Backbuffer) And (((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) < l_imgBuffer.Width) Or ((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) < l_imgBuffer.Height))) Then
            If Output Is Nothing Then l_imgBuffer.Fill Area, 0
        End If
    End If
    If Area.Width <= 0 Then Exit Sub
    If Area.Height <= 0 Then Exit Sub
    If Area.Right <= 0 Then Exit Sub
    If Area.Bottom <= 0 Then Exit Sub
    If Area.Left >= l_imgBuffer.Width Then Exit Sub
    If Area.Top >= l_imgBuffer.Height Then Exit Sub
    l_imgBuffer.ClipRect Area
    Set l_imgBuffer.ClipRectangle = Area
    End With
    With mvarViewingMap
    For Layers = 1 To .Layers.Count
        If mvarUnGhostLayer = 0 Then
            l_sngAlpha = 1
        Else
            l_sngAlpha = IIf(Layers = mvarUnGhostLayer, 1, 0.5)
        End If
        .Layers(Layers).Tileset.RecalculateSizes
        If (.Layers(Layers).Tileset.Buffer.Width > 0) And .Layers(Layers).Visible Then
            If .Layers(Layers).Prerendered = True Then
                With .Layers(Layers)
                    If EnableParallax Then
                        m_lngVPX = (mvarViewportX * .ParallaxX)
                        m_lngVPY = (mvarViewportY * .ParallaxY)
                    Else
                        m_lngVPX = (mvarViewportX)
                        m_lngVPY = (mvarViewportY)
                    End If
                    With CameraParam
                        .ViewportX = m_lngVPX
                        .ViewportY = m_lngVPY
                    End With
                    m_lngViewX = ClipValue(.Tileset.Buffer.Width - m_lngVPX, 0, mvarWidth)
                    m_lngViewY = ClipValue(.Tileset.Buffer.Height - m_lngVPY, 0, mvarHeight)
                    If m_lngVPX < 0 Then
                        m_lngXOff = Abs(m_lngVPX)
                        m_lngVPX = 0
                    End If
                    If m_lngVPY < 0 Then
                        m_lngYOff = Abs(m_lngVPY)
                        m_lngVPY = 0
                    End If
                    m_lngViewX = ClipValue(m_lngViewX, 0, mvarWidth - m_lngXOff)
                    m_lngViewY = ClipValue(m_lngViewY, 0, mvarHeight - m_lngYOff)
                    m_lngViewX = ClipValue(m_lngViewX, 0, (.Width * .Tileset.TileWidth) - (m_lngXOff + m_lngVPX))
                    m_lngViewY = ClipValue(m_lngViewY, 0, (.Height * .Tileset.TileHeight) - (m_lngYOff + m_lngVPY))
                    Set DestTileRect = F2Rect((m_lngXOff), (m_lngYOff), m_lngViewX, m_lngViewY, False)
                    Set SourceTileRect = F2Rect(m_lngVPX, m_lngVPY, m_lngViewX + m_lngBGX, m_lngViewY + m_lngBGY, False)
                    If ShowTiles Then
                        If .Tileset.HasAlpha Then
                            l_imgBuffer.Blit DestTileRect, SourceTileRect, .Tileset.Buffer, .Alpha * l_sngAlpha, BlitMode_SourceAlpha
                        Else
                            If .Alpha >= 1 Then
                                If .Tileset.MaskColor = c_lngNullColor Then
                                    l_imgBuffer.Blit DestTileRect, SourceTileRect, .Tileset.Buffer, l_sngAlpha, BlitMode_Normal
                                Else
                                    l_imgBuffer.Blit DestTileRect, SourceTileRect, .Tileset.Buffer, l_sngAlpha, BlitMode_Matte
                                End If
                            Else
                                l_imgBuffer.Blit DestTileRect, SourceTileRect, .Tileset.Buffer, .Alpha * l_sngAlpha, BlitMode_Matte
                            End If
                        End If
                    End If
                End With
            Else
                XOffset = 0
                YOffset = 0
                StartX = 0
                StartY = 0
                EndX = 0
                EndY = 0
                With .Layers(Layers)
                    If EnableParallax Then
                        m_lngVPX = (mvarViewportX * .ParallaxX) + Area.Left
                        m_lngVPY = (mvarViewportY * .ParallaxY) + Area.Top
                    Else
                        m_lngVPX = (mvarViewportX) + Area.Left
                        m_lngVPY = (mvarViewportY) + Area.Top
                    End If
                    With CameraParam
                        .ViewportX = m_lngVPX
                        .ViewportY = m_lngVPY
                    End With
                    mvarViewportTrueX = ((m_lngVPX) \ .Tileset.TileWidth) * .Tileset.TileWidth
                    mvarViewportTrueY = ((m_lngVPY) \ .Tileset.TileHeight) * .Tileset.TileHeight
'                StartX = ((((m_lngVPX))) \ .Tileset.TileWidth)
'                StartY = ((((m_lngVPY))) \ .Tileset.TileHeight)
'                EndX = StartX + Ceil(CSng(Area.Width) / CSng(.Tileset.TileWidth)) + 1
'                EndY = StartY + Ceil(CSng(Area.Height) / CSng(.Tileset.TileHeight)) + 1
'                If StartX < 0 Then XOffset = Abs(StartX): StartX = 0
'                If StartY < 0 Then YOffset = Abs(StartY): StartY = 0
'                If EndX <= StartX Then EndX = StartX
'                If EndY <= StartY Then EndY = StartY
'                If EndX > (.Width) Then EndX = (.Width)
'                If EndY > (.Height) Then EndY = (.Height)
                End With
                With Tilemap
                    If mvarViewingMap.Layers(Layers).Effect <> BlitMode_Default Then
                        .Effect = mvarViewingMap.Layers(Layers).Effect
                    ElseIf mvarViewingMap.Layers(Layers).Tileset.HasAlpha Then
                        .Effect = 2
                    Else
                        .Effect = 1
                    End If
                    .Alpha = CLng(mvarViewingMap.Layers(Layers).Alpha * 255 * l_sngAlpha)
                    .Height = mvarViewingMap.Layers(Layers).Height
                    .Width = mvarViewingMap.Layers(Layers).Width
                    .pData = mvarViewingMap.Layers(Layers).TilePointer(0, 0)
                    .MaskedTile = mvarViewingMap.Layers(Layers).Tileset.TransparentTile
                    .pTileset = mvarViewingMap.Layers(Layers).Tileset.Handle
                    .pAnimationMap = 0
                    If EnableWrapping Then
                        .WrapX = CByte(Abs(mvarViewingMap.Layers(Layers).WrapX))
                        .WrapY = CByte(Abs(mvarViewingMap.Layers(Layers).WrapY))
                    Else
                        .WrapX = False
                        .WrapY = False
                    End If
                    If AutoTintLayers Then
                        .TintColor = SelectTintColor(Layers - 1, mvarViewingMap.Layers.Count)
                    Else
                        .TintColor = 0
                    End If
                    .x1 = 0
                    .y1 = 0
                    .x2 = .Width
                    .y2 = .Height
                End With
                Err.Clear
                If ShowTiles Then RenderTilemapLayer Tilemap, CameraParam
            End If
            If m_Engine.ShowCollision Then
                Err.Clear
                m_lnLines() = .Layers(Layers).CollisionLines
                If UBound(m_lnLines) > 0 Then
                    For m_lngLines = LBound(m_lnLines) To UBound(m_lnLines)
                        With m_lnLines(m_lngLines)
                            l_imgBuffer.AntiAliasLine Array(.Start.X - (mvarViewportX), .Start.Y - (mvarViewportY), .end.X - (mvarViewportX), .end.Y - (mvarViewportY)), F2RGB(0, 255, 0, 127)
                        End With
                    Next m_lngLines
                End If
            End If
            If ShowSprites Then
                .Layers(Layers).Sprites.Cull CameraParam
                .Layers(Layers).Sprites.Sort
                If Err.Number <> 0 Then CriticalError "Fury2Camera.Redraw", "Layers(" + CStr(Layers) + ").Sprites.Sort Me": Exit Sub
            End If
            If EnableLighting And .Layers(Layers).Lighting.ResetLight Then
                mvarLightmap.Clear .Layers(Layers).Lighting.AmbientLight
            End If
            If EnableLighting And .Layers(Layers).Lit Then
                With l_lcaCamera
                    .OutputBuffer = mvarLightmap.Handle
                    .OutputRectangle = mvarLightmap.Rectangle.GetRectangle
                    .ScratchBuffer = mvarLightmapScratch.Handle
                    .OutputScaleRatio = 1
                    .ScrollX = mvarViewportX
                    .ScrollY = mvarViewportY
                End With
                If ShowSprites Then
                    .Layers(Layers).Lighting.SetFirstSprite .Layers(Layers).Sprites.Pointer
                Else
                    .Layers(Layers).Lighting.SetFirstSprite 0
                End If
                .Layers(Layers).Lighting.Render l_lcaCamera
            End If
            If EnableLighting And .Layers(Layers).Lighting.RenderLight Then
'                l_imgBuffer.Blit Area, , mvarLightmap, 1, BlitMode_Normal
                l_imgBuffer.Blit Area, , mvarLightmap, 1, BlitMode_Lightmap_RGB
            End If
            If ShowSprites Then
                .Layers(Layers).Sprites.Redraw Me, l_imgBuffer
                If Err.Number <> 0 Then CriticalError "Fury2Camera.Redraw", "Layers(" + CStr(Layers) + ").Sprites.Redraw l_imgBuffer, Area": Exit Sub
                Err.Clear
            End If
        End If
    Next Layers
    End With
    Err.Clear
    If mvarViewingMap.Script_Redraw = True Then
        If mvarViewingMap.Script Is Nothing Then Else mvarViewingMap.Script.Redraw l_imgBuffer, Area
    End If
    l_imgBuffer.UpdateClipRect
    mvarDirty = False
End Sub

Public Sub Redraw(Optional Area As Fury2Rect)
On Error Resume Next
Dim m_lngLayers As Long
    If (m_Engine.Cameras.Count = 1) And (m_Engine.DisableCameraBuffers) Then
        Exit Sub
    End If
    If Area Is Nothing Then
        Render Buffer.Rectangle
    Else
        Render Area.Copy
    End If
End Sub

Public Property Let ViewportY(ByVal vData As Single)
    If mvarConstrainToMap Then
        If mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight <= mvarHeight Then
            mvarViewportY = -(mvarHeight - (mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight)) \ 2
        Else
            mvarViewportY = m_Engine.ClipNumber(vData, 0, Abs((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) - mvarHeight))
        End If
    Else
        mvarViewportY = vData
    End If
End Property

Public Property Get ViewportY() As Single
    ViewportY = mvarViewportY
End Property

Public Property Let ViewportX(ByVal vData As Single)
    If mvarConstrainToMap Then
        If mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth <= mvarWidth Then
            mvarViewportX = -(mvarWidth - (mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth)) \ 2
        Else
            mvarViewportX = m_Engine.ClipNumber(vData, 0, Abs((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) - mvarWidth))
        End If
    Else
        mvarViewportX = vData
    End If
End Property

Public Property Get ViewportX() As Single
    ViewportX = mvarViewportX
End Property

Public Property Let Height(ByVal vData As Long)
On Error Resume Next
    mvarHeight = vData
    mvarDirty = True
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Public Property Let Width(ByVal vData As Long)
On Error Resume Next
    mvarWidth = vData
    mvarDirty = True
End Property

Sub Reposition(L, T, W, H, Optional Absolute = True)
On Error Resume Next
    If mvarMap <> 0 Then Set mvarViewingMap = m_Engine.Maps(mvarMap)
    If CBool(Absolute) = True Then
        mvarWidth = ClipValue(CLng(W - L), 1, m_Engine.ScreenWidth)
        mvarHeight = ClipValue(CLng(H - T), 1, m_Engine.ScreenHeight)
    Else
        mvarWidth = ClipValue(CLng(W), 1, m_Engine.ScreenWidth)
        mvarHeight = ClipValue(CLng(H), 1, m_Engine.ScreenHeight)
    End If
    mvarX = ClipValue(CLng(L), 0, m_Engine.ScreenWidth - mvarWidth)
    mvarY = ClipValue(CLng(T), 0, m_Engine.ScreenHeight - mvarHeight)
    mvarDirty = True
End Sub

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Set Buffer(vData As Fury2Image)
    Set mvarBuffer = vData
End Property

Public Property Get Buffer() As Fury2Image
    Set Buffer = mvarBuffer
End Property

Private Sub Class_Initialize()
On Error Resume Next
    mvarAlpha = 1
    Set mvarBuffer = New Fury2Image
    mvarDirty = True
    ShowSprites = True
    ShowTiles = True
    EnableParallax = True
    EnableWrapping = True
    LightingScaleRatio = 1
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Err.Clear
    Set mvarBuffer = Nothing
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
        .WriteSegment_End
    End With
End Sub

Public Sub Update()
On Error Resume Next
Dim l_sngXV As Single, l_sngYV As Single
    If mvarViewportXVelocity Then
        If mvarViewportX < mvarViewportXTarget Then
            l_sngXV = m_Engine.ClipNumber(mvarViewportXVelocity, 0, mvarViewportXTarget - mvarViewportX)
        ElseIf mvarViewportX > mvarViewportXTarget Then
            l_sngXV = m_Engine.ClipNumber(mvarViewportXVelocity, 0, mvarViewportX - mvarViewportXTarget)
        End If
    End If
    If mvarViewportYVelocity Then
        If mvarViewportY < mvarViewportYTarget Then
            l_sngYV = m_Engine.ClipNumber(mvarViewportYVelocity, 0, mvarViewportYTarget - mvarViewportY)
        ElseIf mvarViewportY > mvarViewportYTarget Then
            l_sngYV = m_Engine.ClipNumber(mvarViewportYVelocity, 0, mvarViewportY - mvarViewportYTarget)
        End If
    End If
    Move l_sngXV, l_sngYV
    If Abs(mvarViewportX - mvarViewportXTarget) < 0.1 And Abs(mvarViewportY - mvarViewportYTarget) < 0.1 Then
        mvarViewportX = mvarViewportXTarget
        mvarViewportY = mvarViewportYTarget
        mvarViewportXVelocity = 0
        mvarViewportYVelocity = 0
        mvarViewportXTarget = 0
        mvarViewportYTarget = 0
        If mvarSubthreaded Then
            mvarSubthreaded = False
            m_Engine.Break
        End If
    End If
End Sub
