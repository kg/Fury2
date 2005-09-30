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
Attribute VB_Creatable = False
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

Public EnableHardwareSurfaces As Boolean
Public OutputSurface As Fury2Image
Public AntiAliasLighting As Boolean
Public LightingScaleRatio As Single
Public EnableParallax As Boolean
Public EnableWrapping As Boolean
Public EnableLighting As Boolean
Public AutoTintLayers As Boolean
Public ShowSprites As Boolean
Public ShowTiles As Boolean
Public DisableBuffer As Boolean
Public LightmapBlitMode As SFXBlitModes

Private mvarRenderTargets As Fury2Collection
Private mvarLightingSoftness As Single
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
Private mvarGamma As Single
Private mvarMaskColor As Long
Private mvarTransparent As Boolean
Private mvarMap As Long
Private mvarConstrainToMap As Boolean
Private mvarUnGhostLayer As Long
Private m_engEngine As Fury2Engine
Private m_filBlur As Fury2ConvolutionFilter

Public Function AddRenderTarget(Optional ByVal AutoSize As Boolean = False, Optional ByVal Index As Long = -1) As Fury2Image
On Error Resume Next
    If EnableHardwareSurfaces Then
        Set AddRenderTarget = m_engEngine.CreateHardwareImage(mvarWidth, mvarHeight)
    Else
        Set AddRenderTarget = F2Image(mvarWidth, mvarHeight)
    End If
    If AutoSize Then AddRenderTarget.Tag = "AutoSize"
    mvarRenderTargets.Add AddRenderTarget, , Index
End Function

Public Property Get Alpha() As Single
    Alpha = mvarAlpha
End Property

Public Property Let Alpha(ByVal NewAlpha As Single)
On Error Resume Next
    mvarAlpha = Abs(NewAlpha)
    If mvarAlpha > 1 Then mvarAlpha = 1
End Property

Public Property Get Buffer() As Fury2Image
On Error Resume Next
    Set Buffer = GetRenderTarget(1)
End Property

Public Sub CenterOn(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
Dim m_sngNewX As Single, m_sngNewY As Single
    m_sngNewX = Ceil(X - (mvarWidth / 2))
    m_sngNewY = Ceil(Y - (mvarHeight / 2))
    Move m_sngNewX - mvarViewportX, m_sngNewY - mvarViewportY
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    mvarAlpha = 1
    EnableHardwareSurfaces = True
    mvarDirty = True
    ShowSprites = True
    ShowTiles = True
    EnableParallax = True
    EnableWrapping = True
    LightingScaleRatio = 1
    LightmapBlitMode = BlitMode_Lightmap_RGB
    Gamma = 1#
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Err.Clear
    Set Buffer = Nothing
End Sub

Friend Sub ClearRenderTargets()
On Error Resume Next
Dim l_imgTarget As Fury2Image
    For Each l_imgTarget In mvarRenderTargets
        l_imgTarget.Clear
    Next l_imgTarget
End Sub

Public Property Get ConstrainToMap() As Boolean
    ConstrainToMap = mvarConstrainToMap
End Property

Public Property Let ConstrainToMap(ByVal NewValue As Boolean)
    mvarConstrainToMap = NewValue
End Property

Public Sub CopyToDC(ByVal hdc As Long)
On Error Resume Next
    CopyImageToDC hdc, F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), Buffer
End Sub

Public Property Get Dirty() As Boolean
    Dirty = mvarDirty
End Property

Public Property Let Dirty(ByVal NewDirty As Boolean)
    mvarDirty = NewDirty
End Property

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Sub FlushBuffer()
On Error Resume Next
Dim m_sngNewX As Single, m_sngNewY As Single, m_booAnimated As Boolean
Dim m_lngLayers As Long
Dim m_lngRects As Long
Dim m_rctObj As Fury2Rect
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    If (Engine.Cameras.Count = 1) And (Engine.DisableCameraBuffers) Then Exit Sub
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

Sub Free()
On Error Resume Next
Dim l_imgTarget As Fury2Image
    For Each l_imgTarget In mvarRenderTargets
        l_imgTarget.Unsize
    Next l_imgTarget
End Sub

Public Property Get Gamma() As Single
    Gamma = mvarGamma
End Property

Public Property Let Gamma(ByVal NewGamma As Single)
On Error Resume Next
    mvarGamma = NewGamma
End Property

Public Property Let Height(ByVal vData As Long)
On Error Resume Next
    mvarHeight = vData
    mvarDirty = True
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Sub Init()
On Error Resume Next
    Err.Clear
    If mvarMap <> 0 Then Set mvarViewingMap = Engine.Maps(mvarMap)
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count <= 0 Then Exit Sub
    If ((Engine.DisableCameraBuffers = True) And (Engine.Cameras.Count = 1)) Or (DisableBuffer) Then
    Else
        Buffer.Resize mvarWidth, mvarHeight
    End If
    mvarDirty = True
    Err.Clear
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

Public Property Get LightingSoftness() As Single
    LightingSoftness = mvarLightingSoftness
End Property

Public Property Let LightingSoftness(ByVal NewValue As Single)
    mvarLightingSoftness = NewValue
    Set m_filBlur = Nothing
End Property

Public Property Get Lightmap() As Fury2Image
On Error Resume Next
    Set Lightmap = GetRenderTarget(2)
End Property

Public Property Let Map(ByVal NewMap As Long)
On Error Resume Next
    mvarMap = NewMap
    If mvarMap = 0 Then
        Set mvarViewingMap = Nothing
    Else
        Set mvarViewingMap = Engine.Maps(NewMap)
    End If
End Property

Public Property Get Map() As Long
    Map = mvarMap
End Property

Public Sub Move(ByVal XDistance As Single, ByVal YDistance As Single)
On Error Resume Next
Dim m_sngOldX As Single, m_sngOldY As Single
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    If (Abs(XDistance) < 1) And (Abs(YDistance) < 1) Then Exit Sub
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
            mvarViewportX = Engine.ClipNumber(mvarViewportX, 0, Abs((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) - mvarWidth))
        End If
        If mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight <= mvarHeight Then
            mvarViewportY = -(mvarHeight - (mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight)) \ 2
        Else
            mvarViewportY = Engine.ClipNumber(mvarViewportY, 0, Abs((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) - mvarHeight))
        End If
    End If
End Sub

Public Sub Redraw(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
Dim m_lngLayers As Long
    If (Engine.Cameras.Count = 1) And (Engine.DisableCameraBuffers) Then
        Exit Sub
    End If
    If Area Is Nothing Then
        Render F2Rect(Me.X, Me.Y, Me.Width, Me.Height)
    Else
        Render Area.Copy
    End If
End Sub

Public Sub Refresh()
On Error Resume Next
Dim m_lngEffects As Long
    If mvarMap <> 0 Then Set mvarViewingMap = Engine.Maps(mvarMap)
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    If (Engine.Cameras.Count = 1) And (Engine.DisableCameraBuffers) Then
        If mvarAlpha <= 0 Then Exit Sub
        Render , Engine.Backbuffer
        Exit Sub
    End If
    'If mvarDirty Then FlushBuffer
    Render , Buffer
    Err.Clear
    If mvarX < 0 Then mvarX = 0
    If mvarY < 0 Then mvarY = 0
    If mvarWidth > Engine.Backbuffer.Width - mvarX Then mvarWidth = Engine.Backbuffer.Width - mvarX
    If mvarHeight > Engine.Backbuffer.Height - mvarY Then mvarHeight = Engine.Backbuffer.Height - mvarY
    Engine.Backbuffer.ResetClipRectangle
    If mvarAlpha = 0 Then
    ElseIf mvarAlpha = 1 Then
        'If (mvarGamma = 0) Or (Engine.SpecialFX = False) Then
            Engine.Backbuffer.Blit F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), Buffer.Rectangle, Buffer, 1, BlitMode_Normal
            If Err.Number <> 0 Then Engine.Backbuffer.Fill F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), F2Black
        'Else
        '    Engine.Backbuffer.Blit F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), Buffer.Rectangle, Buffer, ((mvarGamma) / 255), BlitMode_Matte_Tint, F2White
        '    If Err.Number <> 0 Then Engine.Backbuffer.Fill F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), F2Black
        'End If
    Else
        Engine.Backbuffer.Blit F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), Buffer.Rectangle, Buffer, mvarAlpha, BlitMode_Normal
        If Err.Number <> 0 Then Engine.Backbuffer.Fill F2Rect(mvarX, mvarY, mvarWidth, mvarHeight, False), F2RGB(0, 0, 0, mvarAlpha * 255)
    End If
    Engine.Backbuffer.ResetClipRectangle
End Sub

Public Sub Render(Optional ByVal Area As Fury2Rect = Nothing, Optional ByVal Output As Fury2Image = Nothing)
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
Dim l_imgTileset As Fury2Image
Dim l_lngLayer As Long
Dim l_imgBuffer As Fury2Image
Dim l_imgTarget As Fury2Image
Dim l_lyrLayer As Fury2MapLayer
Dim l_sngAlpha As Single
Dim l_lcaCamera As LightingCamera
Dim l_sprSprite As Fury2Sprite
Dim l_lngTargets() As Long
Dim mvarViewportTrueX As Long, mvarViewportTrueY As Long
Dim l_mopObject As Fury2MapObject
    Err.Clear
    If mvarMap <> 0 Then Set mvarViewingMap = Engine.Maps(mvarMap)
    If mvarViewingMap Is Nothing Then Exit Sub
    If mvarViewingMap.Layers.Count < 1 Then Exit Sub
    Set l_imgBuffer = Output
    If Not l_imgBuffer Is Nothing Then
        If (l_imgBuffer.Width < 1) Or (l_imgBuffer.Height < 1) Or (l_imgBuffer.Handle = 0) Then
            Set l_imgBuffer = Nothing
        End If
    End If
    If l_imgBuffer Is Nothing Then
        If (Engine.Cameras.Count = 1) And (Engine.DisableCameraBuffers) Then
            Set l_imgBuffer = Engine.Backbuffer
        Else
            Set l_imgBuffer = Buffer
        End If
    End If
    Set OutputSurface = l_imgBuffer
    If EnableLighting Then
        If (Lightmap.Width <> Ceil(l_imgBuffer.Width * LightingScaleRatio)) Or (Lightmap.Height <> Ceil(l_imgBuffer.Height * LightingScaleRatio)) Then
            Lightmap.Resize Ceil(l_imgBuffer.Width * LightingScaleRatio), Ceil(l_imgBuffer.Height * LightingScaleRatio)
        End If
        If (ScratchBuffer.Width <> Lightmap.Width) Or (ScratchBuffer.Height <> Lightmap.Height) Then
            ScratchBuffer.Resize Ceil(l_imgBuffer.Width * LightingScaleRatio), Ceil(l_imgBuffer.Height * LightingScaleRatio)
        End If
    End If
    l_lngTargets = RenderTargetArray()
    l_lngTargets(0) = l_imgBuffer.Handle
    With mvarViewingMap.Layers(1)
    With CameraParam
        .Alpha = Me.Alpha * 255
        .pRenderTargets = VarPtr(l_lngTargets(0))
        .RenderTargetCount = mvarRenderTargets.Count
        .Rectangle = l_imgBuffer.Rectangle.GetRectangle
    End With
    If Area Is Nothing Then
        If mvarConstrainToMap Then
            If mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth <= mvarWidth Then
                mvarViewportX = -(mvarWidth - (mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth)) \ 2
            Else
                mvarViewportX = Engine.ClipNumber(mvarViewportX, 0, Abs((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) - mvarWidth))
            End If
            If mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight <= mvarHeight Then
                mvarViewportY = -(mvarHeight - (mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight)) \ 2
            Else
                mvarViewportY = Engine.ClipNumber(mvarViewportY, 0, Abs((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) - mvarHeight))
            End If
        End If
        FullRedraw = True
        Set Area = F2Rect(0, 0, l_imgBuffer.Width, l_imgBuffer.Height, False)
        If (Engine.ClearMapBG) Or ((l_imgBuffer Is Engine.Backbuffer) And (((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) < l_imgBuffer.Width) Or ((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) < l_imgBuffer.Height))) Then
            If Output Is Nothing Then l_imgBuffer.Clear 0
        End If
    Else
        If (Engine.ClearMapBG) Or ((l_imgBuffer Is Engine.Backbuffer) And (((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) < l_imgBuffer.Width) Or ((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) < l_imgBuffer.Height))) Then
            If Output Is Nothing Then l_imgBuffer.Fill Area, 0
        End If
    End If
    If Area.Width <= 0 Then Exit Sub
    If Area.Height <= 0 Then Exit Sub
    If Area.right <= 0 Then Exit Sub
    If Area.bottom <= 0 Then Exit Sub
    If Area.left >= l_imgBuffer.Width Then Exit Sub
    If Area.top >= l_imgBuffer.Height Then Exit Sub
    l_imgBuffer.ClipRect Area
    Set l_imgBuffer.ClipRectangle = Area
    End With
    With mvarViewingMap
    For Layers = 1 To .Layers.Count
        Set l_lyrLayer = .Layers(Layers)
        If mvarUnGhostLayer = 0 Then
            l_sngAlpha = 1
        Else
            l_sngAlpha = IIf(Layers = mvarUnGhostLayer, 1, 0.5)
        End If
        l_lyrLayer.Tileset.RecalculateSizes
        If l_lyrLayer.Visible Then
            If l_lyrLayer.Prerendered = True Then
                With l_lyrLayer
                    Set l_imgTileset = .Tileset.GetBuffer
                    If Not (l_imgTileset Is Nothing) Then
                        If EnableParallax Then
                            m_lngVPX = (mvarViewportX * .ParallaxX)
                            m_lngVPY = (mvarViewportY * .ParallaxY)
                        Else
                            m_lngVPX = (mvarViewportX)
                            m_lngVPY = (mvarViewportY)
                        End If
                        If .WrapX Or .WrapY Then
                            Dim l_lngW As Long, l_lngH As Long
                            l_lngW = (l_lyrLayer.MaxX)
                            l_lngH = (l_lyrLayer.MaxY)
                            Set DestTileRect = F2Rect(-m_lngVPX, -m_lngVPY, IIf(.WrapX, (Ceil(m_lngVPX / l_lngW) + 1) * l_lngW, l_lngW), IIf(.WrapY, (Ceil(m_lngVPY / l_lngH) + 1) * l_lngH, l_lngH), False)
                            Set SourceTileRect = l_imgTileset.Rectangle
                            If ShowTiles Then
                                If .RenderTarget = 1 Then
                                    Set l_imgTarget = l_imgBuffer
                                Else
                                    Set l_imgTarget = RenderTargets(.RenderTarget)
                                End If
                                Select Case .Effect
                                Case F2LE_Matte
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Matte
                                Case F2LE_Alpha
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_SourceAlpha
                                Case F2LE_Additive
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Additive
                                Case F2LE_Subtractive
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Subtractive
                                Case F2LE_Screen
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Screen
                                Case F2LE_Multiply
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Multiply
                                Case F2LE_Lightmap
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Lightmap
                                Case Else
                                    l_imgTarget.TileBlit DestTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Normal
                                End Select
                            End If
                        Else
                            Set DestTileRect = F2Rect(-m_lngVPX, -m_lngVPY, l_lyrLayer.MaxX, l_lyrLayer.MaxY, False)
                            Set SourceTileRect = l_imgTileset.Rectangle
                            If ShowTiles Then
                                If .RenderTarget = 1 Then
                                    Set l_imgTarget = l_imgBuffer
                                Else
                                    Set l_imgTarget = RenderTargets(.RenderTarget)
                                End If
                                Select Case .Effect
                                Case F2LE_Matte
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Matte
                                Case F2LE_Alpha
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_SourceAlpha
                                Case F2LE_Additive
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Additive
                                Case F2LE_Subtractive
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Subtractive
                                Case F2LE_Screen
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Screen
                                Case F2LE_Multiply
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Multiply
                                Case F2LE_Lightmap
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Lightmap
                                Case Else
                                    l_imgTarget.Blit DestTileRect, SourceTileRect, l_imgTileset, .Alpha * l_sngAlpha, BlitMode_Normal
                                End Select
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
                With l_lyrLayer
                    If EnableParallax Then
                        m_lngVPX = (mvarViewportX * .ParallaxX) + Area.left
                        m_lngVPY = (mvarViewportY * .ParallaxY) + Area.top
                    Else
                        m_lngVPX = (mvarViewportX) + Area.left
                        m_lngVPY = (mvarViewportY) + Area.top
                    End If
                    With CameraParam
                        .ViewportX = m_lngVPX
                        .ViewportY = m_lngVPY
                        .ParallaxX = l_lyrLayer.ParallaxX
                        .ParallaxY = l_lyrLayer.ParallaxY
                    End With
                    mvarViewportTrueX = ((m_lngVPX) \ .Tileset.TileWidth) * .Tileset.TileWidth
                    mvarViewportTrueY = ((m_lngVPY) \ .Tileset.TileHeight) * .Tileset.TileHeight
                End With
                With Tilemap
                    .Effect = l_lyrLayer.Effect
                    .Alpha = CLng(l_lyrLayer.Alpha * 255 * l_sngAlpha)
                    .Height = l_lyrLayer.Height
                    .Width = l_lyrLayer.Width
                    .pData = l_lyrLayer.TilePointer(0, 0)
                    .RenderTarget = l_lyrLayer.RenderTarget - 1
                    .MaskedTile = l_lyrLayer.Tileset.TransparentTile
                    .pTileset = l_lyrLayer.Tileset.Handle
                    .pAnimationMap = 0
                    If EnableWrapping Then
                        .WrapX = CByte(Abs(l_lyrLayer.WrapX))
                        .WrapY = CByte(Abs(l_lyrLayer.WrapY))
                    Else
                        .WrapX = False
                        .WrapY = False
                    End If
                    If AutoTintLayers Then
                        .TintColor = SelectTintColor(Layers - 1, mvarViewingMap.Layers.Count)
                    Else
                        .TintColor = l_lyrLayer.Color
                    End If
                    .x1 = 0
                    .y1 = 0
                    .x2 = .Width
                    .y2 = .Height
                End With
                Err.Clear
                If ShowTiles Then RenderTilemapLayer Tilemap, CameraParam
            End If
            If ShowSprites Then
                CameraParam.ParallaxX = l_lyrLayer.ParallaxX
                CameraParam.ParallaxY = l_lyrLayer.ParallaxY
                CameraParam.ViewportX = (mvarViewportX * l_lyrLayer.ParallaxX)
                CameraParam.ViewportY = (mvarViewportY * l_lyrLayer.ParallaxY)
                l_lyrLayer.Sprites.Cull CameraParam
                l_lyrLayer.Sprites.Sort
                If Err.Number <> 0 Then Engine.CriticalError "Fury2Camera.Redraw", "Layers(" + CStr(Layers) + ").Sprites.Sort Me": Exit Sub
            End If
            If EnableLighting And l_lyrLayer.Lighting.ResetLight Then
                Lightmap.ResetClipRectangle
                Lightmap.Clear l_lyrLayer.Lighting.AmbientLight
            End If
            If EnableLighting And l_lyrLayer.Lit Then
                With l_lcaCamera
                    .OutputBuffer = Lightmap.Handle
                    .OutputRectangle = Lightmap.Rectangle.GetRectangle
                    .ScratchBuffer = ScratchBuffer.Handle
                    .OutputScaleRatio = 1
                    .ScrollX = mvarViewportX
                    .ScrollY = mvarViewportY
                    .AntiAlias = CByte(IIf(AntiAliasLighting, 255, 0))
                End With
                If ShowSprites Then
                    l_lyrLayer.Lighting.SetFirstSprite l_lyrLayer.Sprites.Pointer
                Else
                    l_lyrLayer.Lighting.SetFirstSprite 0
                End If
                l_lyrLayer.Lighting.Render l_lcaCamera
            End If
            If ShowSprites Then
                l_lyrLayer.Sprites.Redraw CameraParam, l_imgBuffer
                If Err.Number <> 0 Then Engine.CriticalError "Fury2Camera.Redraw", "Layers(" + CStr(Layers) + ").Sprites.Redraw l_imgBuffer, Area": Exit Sub
                Err.Clear
            End If
            For Each l_mopObject In .Objects
                l_lngLayer = l_mopObject.RenderLayer
                If l_lngLayer = -1 Then l_lngLayer = .Layers.Count
                If (Layers = l_lngLayer) Then
                    l_mopObject.Render Me
                End If
            Next l_mopObject
            If EnableLighting And l_lyrLayer.Lighting.RenderLight Then
                Lightmap.ResetClipRectangle
                ScratchBuffer.ResetClipRectangle
                If mvarGamma <> 1 Then
                    Lightmap.AdjustGamma mvarGamma
                End If
                If mvarLightingSoftness > 0 Then
                    If m_filBlur Is Nothing Then
                        Set m_filBlur = F2GaussianBlurFilter(mvarLightingSoftness)
                    End If
                    m_filBlur.Filter ScratchBuffer, Lightmap, , , RenderMode_Normal
                    l_imgBuffer.Blit Area, , ScratchBuffer, 1, LightmapBlitMode
                Else
                    l_imgBuffer.Blit Area, , Lightmap, 1, LightmapBlitMode
                End If
            End If
            With l_lyrLayer
                If .RenderEvent Is Nothing Then
                Else
                    .RenderEvent.Invoke
                End If
            End With
        End If
    Next Layers
    For Each l_sprSprite In mvarViewingMap.Sprites
        If l_sprSprite.AttachedGraphic Is Nothing Then
        Else
            l_sprSprite.AttachedGraphic.Draw l_imgBuffer, l_sprSprite.X - ViewportX + Area.left, l_sprSprite.yTop - ViewportY + Area.top
        End If
    Next l_sprSprite
    End With
    Err.Clear
    If mvarViewingMap.Script_Redraw = True Then
        If mvarViewingMap.Script Is Nothing Then Else mvarViewingMap.Script.Redraw l_imgBuffer, Area
    End If
    Set OutputSurface = Nothing
    l_imgBuffer.UpdateClipRect
    mvarDirty = False
End Sub

Public Function RenderTargetArray() As Long()
On Error Resume Next
Dim l_lngTargets() As Long
Dim l_lngIndex As Long
Dim l_imgTarget As Fury2Image
    ReDim l_lngTargets(0 To mvarRenderTargets.Count - 1)
    For Each l_imgTarget In mvarRenderTargets
        If l_imgTarget Is Nothing Then
            l_lngTargets(l_lngIndex) = 0
        Else
            l_lngTargets(l_lngIndex) = l_imgTarget.Handle
        End If
        l_lngIndex = l_lngIndex + 1
    Next l_imgTarget
    RenderTargetArray = l_lngTargets
End Function

Public Function RenderTargetCount() As Long
On Error Resume Next
    RenderTargetCount = mvarRenderTargets.Count
End Function

Friend Property Get GetRenderTarget(ByVal Index As Long) As Fury2Image
On Error Resume Next
    Set GetRenderTarget = mvarRenderTargets(Index)
    If GetRenderTarget Is Nothing Then
    Else
        If GetRenderTarget.Width <> Me.Width Or GetRenderTarget.Height <> Me.Height Then
            Set GetRenderTarget = Nothing
        End If
    End If
    If GetRenderTarget Is Nothing Then
        mvarRenderTargets.Remove Index
        AddRenderTarget True, Index
    End If
    Set GetRenderTarget = mvarRenderTargets(Index)
End Property

Public Property Get RenderTargets(ByVal Index As Long) As Fury2Image
On Error Resume Next
    Set RenderTargets = mvarRenderTargets(Index)
End Property

Sub Reposition(ByVal L As Long, ByVal T As Long, ByVal W As Long, ByVal H As Long, Optional ByVal Absolute = True)
On Error Resume Next
    If mvarMap <> 0 Then Set mvarViewingMap = Engine.Maps(mvarMap)
    If CBool(Absolute) = True Then
        mvarWidth = ClipValue(W - L, 1, Engine.ScreenWidth)
        mvarHeight = ClipValue(H - T, 1, Engine.ScreenHeight)
    Else
        mvarWidth = ClipValue(W, 1, Engine.ScreenWidth)
        mvarHeight = ClipValue(H, 1, Engine.ScreenHeight)
    End If
    mvarX = ClipValue(L, 0, Engine.ScreenWidth - mvarWidth)
    mvarY = ClipValue(T, 0, Engine.ScreenHeight - mvarHeight)
    mvarDirty = True
End Sub

Friend Sub ResizeRenderTargets()
On Error Resume Next
Dim l_imgTarget As Fury2Image
    For Each l_imgTarget In mvarRenderTargets
        If l_imgTarget.Tag = "AutoSize" Then
        Else
            l_imgTarget.Resize mvarWidth, mvarHeight
        End If
    Next l_imgTarget
End Sub

Friend Property Get ScratchBuffer() As Fury2Image
On Error Resume Next
    Set ScratchBuffer = GetRenderTarget(3)
End Property

Public Sub ScrollTo(ByVal NewX As Single, ByVal NewY As Single, Optional ByVal Speed As Double = 1)
On Error Resume Next
    mvarViewportXVelocity = Speed
    mvarViewportYVelocity = Speed
    mvarViewportXTarget = X
    mvarViewportYTarget = Y
    mvarSubthreaded = True
    Engine.Subthread
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

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
    Set mvarRenderTargets = New Fury2Collection
    mvarRenderTargets.Add Nothing
    mvarRenderTargets.Add Nothing
    mvarRenderTargets.Add Nothing
End Sub

Public Sub SetMap(Obj As Fury2Map)
On Error Resume Next
    Set mvarViewingMap = Obj
    mvarMap = 0
End Sub

Public Property Get UnGhostLayer() As Long
On Error Resume Next
    UnGhostLayer = mvarUnGhostLayer
End Property

Public Property Let UnGhostLayer(ByVal NewLayer As Long)
On Error Resume Next
    If mvarViewingMap Is Nothing Then Exit Property
    If mvarViewingMap.Layers.Count < 0 Then Exit Property
    mvarUnGhostLayer = ClipValue(NewLayer, 0, mvarViewingMap.Layers.Count)
End Property

Public Sub Update()
On Error Resume Next
Dim l_sngXV As Single, l_sngYV As Single
    If mvarViewportXVelocity Then
        If mvarViewportX < mvarViewportXTarget Then
            l_sngXV = Engine.ClipNumber(mvarViewportXVelocity, 0, mvarViewportXTarget - mvarViewportX)
        ElseIf mvarViewportX > mvarViewportXTarget Then
            l_sngXV = Engine.ClipNumber(mvarViewportXVelocity, 0, mvarViewportX - mvarViewportXTarget)
        End If
    End If
    If mvarViewportYVelocity Then
        If mvarViewportY < mvarViewportYTarget Then
            l_sngYV = Engine.ClipNumber(mvarViewportYVelocity, 0, mvarViewportYTarget - mvarViewportY)
        ElseIf mvarViewportY > mvarViewportYTarget Then
            l_sngYV = Engine.ClipNumber(mvarViewportYVelocity, 0, mvarViewportY - mvarViewportYTarget)
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
            Engine.Break
        End If
    End If
End Sub

Friend Property Set ViewingMap(ByVal NewMap As Fury2Map)
    Set mvarViewingMap = NewMap
End Property

Public Property Get ViewingMap() As Fury2Map
    Set ViewingMap = mvarViewingMap
End Property

Public Property Get ViewportRectangle() As Fury2Rect
On Error Resume Next
    Set ViewportRectangle = F2Rect(ViewportX, ViewportY, Width, Height, False)
End Property

Public Property Let ViewportX(ByVal vData As Single)
    If mvarConstrainToMap Then
        If mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth <= mvarWidth Then
            mvarViewportX = -(mvarWidth - (mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth)) \ 2
        Else
            mvarViewportX = Engine.ClipNumber(vData, 0, Abs((mvarViewingMap.Width * mvarViewingMap.Layers(1).Tileset.TileWidth) - mvarWidth))
        End If
    Else
        mvarViewportX = vData
    End If
End Property

Public Property Get ViewportX() As Single
    ViewportX = mvarViewportX
End Property

Public Property Let ViewportY(ByVal vData As Single)
    If mvarConstrainToMap Then
        If mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight <= mvarHeight Then
            mvarViewportY = -(mvarHeight - (mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight)) \ 2
        Else
            mvarViewportY = Engine.ClipNumber(vData, 0, Abs((mvarViewingMap.Height * mvarViewingMap.Layers(1).Tileset.TileHeight) - mvarHeight))
        End If
    Else
        mvarViewportY = vData
    End If
End Property

Public Property Get ViewportY() As Single
    ViewportY = mvarViewportY
End Property

Public Property Let Width(ByVal vData As Long)
On Error Resume Next
    mvarWidth = vData
    mvarDirty = True
End Property

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Let X(ByVal vData As Long)
    mvarX = ClipValue(vData, 0, Engine.ScreenWidth - mvarWidth)
End Property

Public Property Get X() As Long
    X = mvarX
End Property

Public Function XPad() As Long
On Error Resume Next
    XPad = 0
End Function

Public Property Let Y(ByVal vData As Long)
    mvarY = ClipValue(vData, 0, Engine.ScreenHeight - mvarHeight)
End Property

Public Property Get Y() As Long
    Y = mvarY
End Property

Public Function YPad() As Long
On Error Resume Next
    YPad = 0
End Function

