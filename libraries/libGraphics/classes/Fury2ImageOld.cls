VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ImageOld"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Implements IVirtualFileSaveable

Private m_booIaeron As Boolean, m_Iaeron As Long
Private m_Image As Long
Private m_MaskColor As Long
Private m_ForeColor As Long
Private m_Width As Long, m_Height As Long
Private m_Font As StdFont
Private m_FontPR As FontParam
Private m_FontEx As Fury2Image
Private Const m_Pitch As Long = 4 '32 bit
Private m_TileMode As Boolean
Private m_NewImage As Long
Private m_Initialized As Boolean
Private m_ClipRect As Fury2Rect
Private m_MyRect As Fury2Rect
Private FontData As RFN_CharFile
Private m_lngCurrentX As Long, m_lngCurrentY As Long
Public LoadedAlpha As Boolean
Public CompatiblityDC As Long
Public Name As String
Public RefCount As Long
Public TileCount As Long
Public TileSize As Long
Private m_booDirty As Boolean
Private m_booHeapManaged As Boolean

'Public Function Quantize(Optional Quality As Single = 0.5) As Fury2PalettedImage
'Dim l_imgNew As Fury2PalettedImage
'    Set l_imgNew = New Fury2PalettedImage
'    l_imgNew.Resize m_Width, m_Height
'    Call SoftFX.ReduceImage_Neuqant(m_Image, l_imgNew.Pointer(0, 0), l_imgNew.PalettePointer(0), 256, m_Width * m_Height, ClipValue(Quality * 30, 1, 30))
'    Set Quantize = l_imgNew
'End Function
'
'Public Function ReduceToPalette(Palette() As Long) As Fury2PalettedImage
'Dim l_imgNew As Fury2PalettedImage
'    Set l_imgNew = New Fury2PalettedImage
'    l_imgNew.SetPalette Palette
'    l_imgNew.Resize m_Width, m_Height
'    Call SoftFX.ReduceImage_Direct(m_Image, l_imgNew.PalettePointer(0), l_imgNew.Pointer(0, 0), 256, m_Width * m_Height)
'    Set ReduceToPalette = l_imgNew
'End Function

Public Sub Grid(x As Long, y As Long, Color As Long)
On Error Resume Next
Dim m_bpArea As BlitParam
    m_bpArea = GetParam
    SoftFX.Grid m_bpArea, -1, -1, x, y, Color
End Sub

Friend Function GetParam() As BlitParam
On Error Resume Next
    FillParam GetParam, 0, 0, m_Width, m_Height, m_Width, m_Image
End Function

Private Sub FreeBuffer()
On Error Resume Next
    If m_booIaeron Then
        iaFreeImage m_Iaeron
        m_Iaeron = 0
        m_booIaeron = False
        m_Image = 0
    Else
        If m_Image <> 0 Then Call GlobalFree(m_Image)
        m_Image = 0
    End If
End Sub

Public Property Let HeapManaged(NewValue As Boolean)
    m_booHeapManaged = NewValue
    If NewValue = True Then HeapAdd Me Else HeapRemove Me
End Property

Public Property Get HeapManaged() As Boolean
    HeapManaged = m_booHeapManaged
End Property

Public Property Let Dirty(NewDirty As Boolean)
    m_booDirty = NewDirty
End Property

Public Property Get Dirty() As Boolean
    Dirty = m_booDirty
End Property

Friend Function FontSurf() As Fury2Image
    Set FontSurf = m_FontEx
End Function

Friend Function FontPR() As FontParam
    FontPR = m_FontPR
End Function

Public Sub CopyFontEx(OtherSurf As Fury2Image)
    Set m_FontEx = OtherSurf.FontSurf
    m_FontPR = OtherSurf.FontPR
End Sub

Public Property Get CurrentX() As Long
    CurrentX = m_lngCurrentX
End Property

Public Property Let CurrentX(NewX As Long)
    m_lngCurrentX = NewX
End Property

Public Property Get CurrentY() As Long
    CurrentY = m_lngCurrentY
End Property

Public Property Let CurrentY(NewY As Long)
    m_lngCurrentY = NewY
End Property

Public Sub TextOut(Text As String, Optional Rct = Nothing)
On Error Resume Next
Dim m_bpDest As BlitParam
Dim m_bpSource As BlitParam
Dim m_rctRect As Fury2Rect
    m_booDirty = True
    If Rct Is Nothing Then
        FillParam m_bpDest, m_lngCurrentX, m_lngCurrentY, m_Width, m_Height, m_Width, m_Image
    Else
        Set m_rctRect = Rct
        FillParam m_bpDest, m_rctRect.left, m_rctRect.top, m_rctRect.Right, m_rctRect.Bottom, m_Width, m_Image
    End If
    FillParam m_bpSource, 0, 0, m_FontEx.Width, m_FontEx.Height, m_FontEx.Width, m_FontEx.Pointer(0, 0)
    If Rct Is Nothing Then
        m_lngCurrentY = m_lngCurrentY + RenderFontFixed(ByVal StrPtr(Text), m_bpDest, m_bpSource, m_FontPR, 0)
    Else
        Call RenderFontFixed(ByVal StrPtr(Text), m_bpDest, m_bpSource, m_FontPR, 0)
    End If
End Sub

Public Sub TextOutScroll(Text As String, Optional Rect = Nothing, Optional Offset = 0)
On Error Resume Next
Dim m_bpDest As BlitParam
Dim m_bpSource As BlitParam
Dim m_rctRect As Fury2Rect
Dim m_bytText() As Byte
    m_booDirty = True
    If Rect Is Nothing Then
        FillParam m_bpDest, m_lngCurrentX, m_lngCurrentY, m_Width, m_Height, m_Width, m_Image
    Else
        Set m_rctRect = Rect
        FillParam m_bpDest, m_rctRect.left, m_rctRect.top, m_rctRect.Right, m_rctRect.Bottom, m_Width, m_Image
    End If
    FillParam m_bpSource, 0, 0, m_FontEx.Width, m_FontEx.Height, m_FontEx.Width, m_FontEx.Pointer(0, 0)
    If Rect Is Nothing Then
        m_lngCurrentY = m_lngCurrentY + RenderFontFixed(ByVal StrPtr(Text), m_bpDest, m_bpSource, m_FontPR, CLng(Offset))
    Else
        Call RenderFontFixed(ByVal StrPtr(Text), m_bpDest, m_bpSource, m_FontPR, CLng(Offset))
    End If
End Sub

Public Function TextOutScrollEx(Text As String, Optional Rect = Nothing, Optional Offset = 0) As Long
On Error Resume Next
Dim m_bpDest As BlitParam
Dim m_bpSource As BlitParam
Dim m_rctRect As Fury2Rect
Dim m_bytText() As Byte
    m_booDirty = True
    If Rect Is Nothing Then
        FillParam m_bpDest, m_lngCurrentX, m_lngCurrentY, m_Width, m_Height, m_Width, m_Image
    Else
        Set m_rctRect = Rect
        FillParam m_bpDest, m_rctRect.left, m_rctRect.top, m_rctRect.Right, m_rctRect.Bottom, m_Width, m_Image
    End If
    FillParam m_bpSource, 0, 0, m_FontEx.Width, m_FontEx.Height, m_FontEx.Width, m_FontEx.Pointer(0, 0)
    If Rect Is Nothing Then
        m_lngCurrentY = m_lngCurrentY + RenderFontFixed(ByVal StrPtr(Text), m_bpDest, m_bpSource, m_FontPR, CLng(Offset))
    Else
        TextOutScrollEx = RenderFontFixed(ByVal StrPtr(Text), m_bpDest, m_bpSource, m_FontPR, CLng(Offset))
    End If
End Function

Public Sub SetFontEx(FontSurf, CharWidth, CharHeight)
On Error Resume Next
    Set m_FontEx = FontSurf
    With m_FontPR
        .Width = CLng(CharWidth)
        .Height = CLng(CharHeight)
        .MaskColor = FontSurf.MaskColor
        .FillColor = F2White
        .Alpha = 255
        .EffectMode = 4
    End With
End Sub

Public Property Get TextWrapMode() As Long
On Error Resume Next
    TextWrapMode = m_FontPR.WrapMode
End Property

Public Property Let TextWrapMode(NewMode As Long)
On Error Resume Next
    m_FontPR.WrapMode = NewMode Mod 3
End Property

Public Property Get ForeColorEx() As Long
On Error Resume Next
    ForeColorEx = m_FontPR.FillColor
End Property

Public Property Let ForeColorEx(NewColor As Long)
On Error Resume Next
    m_FontPR.FillColor = NewColor
    m_FontPR.Alpha = GetAlpha(NewColor)
End Property

Public Property Get FontAlpha() As Single
On Error Resume Next
    FontAlpha = CSng(CLng((CSng(m_FontPR.Alpha / 255) * 250))) / 250
End Property

Public Property Let FontAlpha(NewAlpha As Single)
On Error Resume Next
    m_FontPR.Alpha = (CSng(CLng(NewAlpha * 250)) / 250) * 255
End Property

Public Property Get FData() As RFN_CharFile
    If Not Initialized Then Exit Sub
    FData = FontData
End Property

Public Sub RenderHeightmap(ByRef Heightmap() As Integer, Texture, Alpha)
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
Dim m_imgTexture As Fury2Image
Dim m_bpDest As BlitParam, m_bpTexture As BlitParam
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_lngWidth = (UBound(Heightmap, 1) - LBound(Heightmap, 1)) + 1
    m_lngHeight = (UBound(Heightmap, 2) - LBound(Heightmap, 2)) + 1
    m_bpDest = GetParam
    Set m_imgTexture = Texture
    m_bpTexture = m_imgTexture.GetParam
    Display_Water m_bpDest, m_bpTexture, VarPtr(Heightmap(LBound(Heightmap, 1), LBound(Heightmap, 2))), m_lngWidth, m_lngHeight, CLng(Alpha)
End Sub

Public Sub RenderHeightmapHighQuality(ByRef Heightmap() As Integer, Texture, Alpha)
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
Dim m_imgTexture As Fury2Image
Dim m_bpDest As BlitParam, m_bpTexture As BlitParam
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_lngWidth = (UBound(Heightmap, 1) - LBound(Heightmap, 1)) + 1
    m_lngHeight = (UBound(Heightmap, 2) - LBound(Heightmap, 2)) + 1
    m_bpDest = GetParam
    Set m_imgTexture = Texture
    m_bpTexture = m_imgTexture.GetParam
    Display_Water_HQ m_bpDest, m_bpTexture, VarPtr(Heightmap(LBound(Heightmap, 1), LBound(Heightmap, 2))), m_lngWidth, m_lngHeight, CLng(Alpha)
End Sub

Public Sub Dynawarp(Strength As Single)
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
Dim m_sngSrcX As Single, m_sngSrcY As Single
Dim m_sngWarpX As Single, m_sngWarpY As Single
Dim DestParam As BlitParam, m_lngRet As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    DestParam = GetParam
    SoftFX.Filter_Dynawarp DestParam, Strength + 1, Rnd * 32000
    Exit Sub
'    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
'    For m_lngY = 0 To m_Height - 1
'        For m_lngX = 0 To m_Width - 1
'            m_sngWarpX = (Rnd - Rnd) * Strength
'            m_sngWarpY = (Rnd - Rnd) * Strength
'            m_sngSrcX = CSng(m_lngX) + m_sngWarpX
'            m_sngSrcY = CSng(m_lngY) + m_sngWarpY
'            FastSet m_lngX, m_lngY, SoftFX.GetPixel_Bilinear(DestParam, m_sngSrcX, m_sngSrcY)
'        Next m_lngX
'    Next m_lngY
'    FreeBuffer
'    m_Image = m_NewImage
End Sub

Public Sub Mosaic(BlockSize As Long, Optional Average As Boolean = False)
On Error Resume Next
Dim m_lngBlockX As Long, m_lngBlockY As Long
Dim m_lngColor As Long
Dim m_lngReds As Long, m_lngGreens As Long, m_lngBlues As Long
Dim m_lngSampleX As Long, m_lngSampleY As Long, m_lngSampleCount As Long
Dim m_lngX As Long, m_lngY As Long
Dim RectParam As BlitParam
    If Initialized = False Then Exit Sub
    m_booDirty = True
    m_lngSampleCount = BlockSize * BlockSize
    If Average Then
        For m_lngBlockX = 0 To m_Width - 1 Step BlockSize
            For m_lngBlockY = 0 To m_Height - 1 Step BlockSize
                m_lngReds = 0
                m_lngGreens = 0
                m_lngBlues = 0
                For m_lngSampleY = 0 To BlockSize - 1
                    For m_lngSampleX = 0 To BlockSize - 1
                        m_lngX = ClipValue(m_lngBlockX + m_lngSampleX, 0, m_Width - 1) * 4
                        m_lngY = ClipValue(m_lngBlockY + m_lngSampleY, 0, m_Height - 1)
                        m_lngReds = m_lngReds + m_Image + ((m_lngX + 2) * 4) + (m_lngY * m_Width * 4)
                        m_lngGreens = m_lngGreens + m_Image + ((m_lngX + 1) * 4) + (m_lngY * m_Width * 4)
                        m_lngBlues = m_lngBlues + m_Image + ((m_lngX + 0) * 4) + (m_lngY * m_Width * 4)
                    Next m_lngSampleX
                Next m_lngSampleY
                m_lngColor = BGRA((m_lngReds \ m_lngSampleCount), (m_lngGreens \ m_lngSampleCount), (m_lngBlues \ m_lngSampleCount), 255)
                FillParam RectParam, m_lngBlockX, m_lngBlockY, ClipValue(m_lngBlockX + BlockSize, 0, m_Width), ClipValue(m_lngBlockY + BlockSize, 0, m_Height), m_Width, m_Image
                SoftFX.Fill RectParam, m_lngColor
            Next m_lngBlockY
        Next m_lngBlockX
    Else
        For m_lngBlockX = 0 To m_Width - 1 Step BlockSize
            For m_lngBlockY = 0 To m_Height - 1 Step BlockSize
                m_lngColor = FastGet(m_lngBlockX, m_lngBlockY)
                FillParam RectParam, m_lngBlockX, m_lngBlockY, ClipValue(m_lngBlockX + BlockSize, 0, m_Width), ClipValue(m_lngBlockY + BlockSize, 0, m_Height), m_Width, m_Image
                SoftFX.Fill RectParam, m_lngColor
            Next m_lngBlockY
        Next m_lngBlockX
    End If
End Sub

Public Sub DrawParticlesEx(ByVal ViewportX As Long, ByVal ViewportY As Long, ByRef Particles() As Fury2Particle, ByVal DecaySpeed As Long, Optional ByVal NumToRender As Long = -1)
On Error Resume Next
Dim ClipParam As BlitParam
    If Initialized = False Then Exit Sub
    m_booDirty = True
    If NumToRender = -1 Then
        NumToRender = (UBound(Particles) - LBound(Particles)) + 1
    End If
    FillParam ClipParam, m_ClipRect.left, m_ClipRect.top, m_ClipRect.Right, m_ClipRect.Bottom, m_Width, m_Image
    SFXRenderParticlesEx ViewportX, ViewportY, Particles(LBound(Particles)), NumToRender, ClipParam
End Sub

Public Sub DrawParticlesAdditiveEx(ByVal ViewportX As Long, ByVal ViewportY As Long, ByRef Particles() As Fury2Particle, ByVal DecaySpeed As Long, Optional ByVal NumToRender As Long = -1)
On Error Resume Next
Dim ClipParam As BlitParam
    If Initialized = False Then Exit Sub
    m_booDirty = True
    If NumToRender = -1 Then
        NumToRender = (UBound(Particles) - LBound(Particles)) + 1
    End If
    FillParam ClipParam, m_ClipRect.left, m_ClipRect.top, m_ClipRect.Right, m_ClipRect.Bottom, m_Width, m_Image
    SFXRenderParticlesAddEx ViewportX, ViewportY, Particles(LBound(Particles)), NumToRender, ClipParam
End Sub

Public Sub UpdateParticles(ByRef Particles() As Fury2Particle, ByVal DecaySpeed As Long, Optional ByVal NumToRender As Long = -1)
On Error Resume Next
    If NumToRender = -1 Then
        NumToRender = (UBound(Particles) - LBound(Particles)) + 1
    End If
    SFXUpdateParticles Particles(LBound(Particles)), NumToRender, DecaySpeed
End Sub

Public Sub DrawParticles(ByRef Particles() As Fury2Particle, ByVal DecaySpeed As Long, Optional ByVal NumToRender As Long = -1)
On Error Resume Next
Dim ClipParam As BlitParam
    If Initialized = False Then Exit Sub
    m_booDirty = True
    If NumToRender = -1 Then
        NumToRender = (UBound(Particles) - LBound(Particles)) + 1
    End If
    FillParam ClipParam, m_ClipRect.left, m_ClipRect.top, m_ClipRect.Right, m_ClipRect.Bottom, m_Width, m_Image
    SFXRenderParticles Particles(LBound(Particles)), NumToRender, ClipParam
End Sub

Public Sub DrawParticlesAdditive(ByRef Particles() As Fury2Particle, ByVal DecaySpeed As Long, Optional ByVal NumToRender As Long = -1)
On Error Resume Next
Dim ClipParam As BlitParam
    If Initialized = False Then Exit Sub
    m_booDirty = True
    If NumToRender = -1 Then
        NumToRender = (UBound(Particles) - LBound(Particles)) + 1
    End If
    FillParam ClipParam, m_ClipRect.left, m_ClipRect.top, m_ClipRect.Right, m_ClipRect.Bottom, m_Width, m_Image
    SFXRenderParticlesAdd Particles(LBound(Particles)), NumToRender, ClipParam
End Sub

Public Function CompareEx(DestRect, SourceRect, OtherImage) As Boolean
Dim m_rctDest As Fury2Rect, m_rctSource As Fury2Rect
Dim m_bpDest As BlitParam, m_bpSource As BlitParam
    If Initialized = False Then Exit Function
Dim m_imgSource As Fury2Image
    Set m_imgSource = OtherImage
    If m_imgSource Is Nothing Then Exit Function
    If m_imgSource.Initialized = False Then Exit Function
    Set m_rctDest = DestRect
    Set m_rctSource = SourceRect
    m_bpDest = m_rctDest.GetParam(m_Width, m_Image)
    m_bpSource = m_rctSource.GetParam(m_imgSource.Width, m_imgSource.Pointer(0, 0))
    CompareEx = CBool(SoftFX.Compare(m_bpDest, m_bpSource))
End Function

Public Function CompareIfEx(DestRect, SourceRect, OtherImage) As Boolean
Dim m_rctDest As Fury2Rect, m_rctSource As Fury2Rect
Dim m_bpDest As BlitParam, m_bpSource As BlitParam
    If Initialized = False Then Exit Function
Dim m_imgSource As Fury2Image
    Set m_imgSource = OtherImage
    If m_imgSource Is Nothing Then Exit Function
    If m_imgSource.Initialized = False Then Exit Function
    Set m_rctDest = DestRect
    Set m_rctSource = SourceRect
    m_bpDest = m_rctDest.GetParam(m_Width, m_Image)
    m_bpSource = m_rctSource.GetParam(m_imgSource.Width, m_imgSource.Pointer(0, 0))
    CompareIfEx = CBool(SoftFX.Compare_If(m_bpDest, m_bpSource))
End Function

Public Function Compare(OtherImage) As Boolean
Dim m_bpDest As BlitParam, m_bpSource As BlitParam
    If Initialized = False Then Exit Function
Dim m_imgSource As Fury2Image
    Set m_imgSource = OtherImage
    If m_imgSource Is Nothing Then Exit Function
    If m_imgSource.Initialized = False Then Exit Function
    m_bpDest = GetParam
    m_bpSource = m_imgSource.GetParam
    Compare = CBool(SoftFX.Compare(m_bpDest, m_bpSource))
End Function

Public Function CompareIf(OtherImage) As Boolean
Dim m_rctDest As Fury2Rect, m_rctSource As Fury2Rect
Dim m_bpDest As BlitParam, m_bpSource As BlitParam
    If Initialized = False Then Exit Function
Dim m_imgSource As Fury2Image
    Set m_imgSource = OtherImage
    If m_imgSource Is Nothing Then Exit Function
    If m_imgSource.Initialized = False Then Exit Function
    m_bpDest = GetParam
    m_bpSource = m_imgSource.GetParam
    CompareIf = CBool(SoftFX.Compare_If(m_bpDest, m_bpSource))
End Function

Public Sub GenerateAlpha(Optional Antialias As Boolean = False)
On Error Resume Next
Dim m_lngCurrentX As Long, m_lngCurrentY As Long
Dim m_lngValue As Long, m_lngAlpha As Long
    If Initialized = False Then Exit Sub
    m_booDirty = True
    If Antialias Then
        FillChannel f2aAlpha, 255
        ReplaceColor F2RGB(GetRed(m_MaskColor), GetGreen(m_MaskColor), GetBlue(m_MaskColor), 255), 0
        m_MaskColor = 0
        For m_lngCurrentY = 0 To m_Height - 1
            For m_lngCurrentX = 0 To m_Width - 1
                m_lngAlpha = 255
                m_lngValue = FastGet(m_lngCurrentX, m_lngCurrentY)
                If (m_lngValue = 0) Or (GetAlpha(m_lngValue) = 0) Then
                    m_lngAlpha = 0
                    m_lngValue = 0
                    FastSet m_lngCurrentX, m_lngCurrentY, 0
                Else
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX - 1, m_lngCurrentY - 1)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX, m_lngCurrentY - 1)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX + 1, m_lngCurrentY - 1)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX - 1, m_lngCurrentY)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX + 1, m_lngCurrentY)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX - 1, m_lngCurrentY + 1)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX, m_lngCurrentY + 1)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = GetPixel(m_lngCurrentX + 1, m_lngCurrentY + 1)
                    If ((m_lngValue = m_MaskColor) Or (GetAlpha(m_lngValue) = 0)) And (Err.Number = 0) Then
                        m_lngAlpha = m_lngAlpha - 28
                    End If
                    Err.Clear
                    m_lngValue = FastGet(m_lngCurrentX, m_lngCurrentY)
                    FastSet m_lngCurrentX, m_lngCurrentY, F2RGB(GetRed(m_lngValue), GetGreen(m_lngValue), GetBlue(m_lngValue), m_lngAlpha)
                End If
            Next m_lngCurrentX
        Next m_lngCurrentY
    Else
        FillChannel f2aAlpha, 255
        ReplaceColor BGRA(GetRed(m_MaskColor), GetGreen(m_MaskColor), GetBlue(m_MaskColor), 255), 0
'        For m_lngCurrentY = 0 To m_Height - 1
'            For m_lngCurrentX = 0 To m_Width - 1
'                m_lngValue = FastGet(m_lngCurrentX, m_lngCurrentY)
'                If m_lngValue = m_MaskColor Then
'                    m_lngValue = F2RGB(0, 0, 0, 0)
'                Else
'                    m_lngValue = F2RGB(GetRed(m_lngValue), GetGreen(m_lngValue), GetBlue(m_lngValue), 255)
'                End If
'                FastSet m_lngCurrentX, m_lngCurrentY, m_lngValue
'            Next m_lngCurrentX
'        Next m_lngCurrentY
    End If
End Sub

Public Sub UpdateClipRect()
On Error Resume Next
    Set m_ClipRect = F2Rect(0, 0, m_Width, m_Height)
    Set m_MyRect = F2Rect(0, 0, m_Width, m_Height)
End Sub

Public Sub FilterEx(ByRef Area, ByRef Filter As F2Filter)
On Error Resume Next
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    ClipRectEx Area
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelFilter m_Image, m_Width, F2Rect(Area.left, Area.top, Area.Right, Area.Bottom), Filter
End Sub

Public Function GetRect() As Fury2Rect
On Error Resume Next
    If m_MyRect Is Nothing Then
        Set m_MyRect = F2Rect(0, 0, m_Width, m_Height)
        Set GetRect = m_MyRect.Copy
    Else
        Set GetRect = m_MyRect.Copy
    End If
End Function

Public Property Get TextWidthEx(Text) As Long
On Error Resume Next
    TextWidthEx = Len(Text) * m_FontPR.Width
End Property

Public Property Get TextHeightEx(Text) As Long
On Error Resume Next
    TextHeightEx = m_FontPR.Height
End Property

Public Property Get TextWidth(Text) As Long
On Error Resume Next
Dim deskWnd As Long, deskDC As Long, hdcMem
Dim textExtent As Size, hFont As IFont
    If Initialized = False Then Exit Function
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    hdcMem = CreateCompatibleDC(deskDC)
    ReleaseDC deskWnd, deskDC
    If hdcMem = 0 Then
        Exit Sub
    End If
    Set hFont = m_Font
    SelectObject hdcMem, hFont.hFont
    GetTextExtentPoint32 hdcMem, Text, Len(Text), textExtent
    SelectObject hdcMem, 0
    Set hFont = Nothing
    DeleteDC hdcMem
    TextWidth = textExtent.cX
End Property

Public Property Get TextHeight(Text) As Long
On Error Resume Next
Dim deskWnd As Long, deskDC As Long, hdcMem
Dim textExtent As Size, hFont As IFont
    If Initialized = False Then Exit Function
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    hdcMem = CreateCompatibleDC(deskDC)
    ReleaseDC deskWnd, deskDC
    If hdcMem = 0 Then
        Exit Sub
    End If
    Set hFont = m_Font
    SelectObject hdcMem, hFont.hFont
    GetTextExtentPoint32 hdcMem, Text, Len(Text), textExtent
    SelectObject hdcMem, 0
    Set hFont = Nothing
    DeleteDC hdcMem
    TextHeight = textExtent.cY
End Property

Public Property Get Initialized()
On Error Resume Next
    Err.Clear
    Initialized = False
    If m_Width > -1 Then
        If m_Height > -1 Then
            If m_Image <> 0 Then
                If F2Initialized Then
                    If Err = 0 Then
                        Initialized = m_Initialized
                    Else
                    End If
                End If
            End If
        End If
    End If
End Property

Sub Mirror()
Attribute Mirror.VB_Description = "Mirrors the image."
On Error Resume Next
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
    AccelMirror m_NewImage, m_Image, m_Width, m_Height
    FreeBuffer
    m_Image = m_NewImage
End Sub

Sub Flip()
On Error Resume Next
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
    AccelFlip m_NewImage, m_Image, m_Width, m_Height
    FreeBuffer
    m_Image = m_NewImage
End Sub

Sub Rotate90()
On Error Resume Next
Dim m_bpDest As BlitParam
Dim m_bpSource As BlitParam
Dim m_lngTemp As Long
Dim m_lngX As Long, m_lngY As Long
Dim m_lngSub As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
    FillParam m_bpDest, 0, 0, m_Height, m_Width, m_Height, m_NewImage
    FillParam m_bpSource, 0, 0, m_Width, m_Height, m_Width, m_Image
    SoftFX.Blit_Rotate90 m_bpDest, m_bpSource
'    m_lngSub = m_Width - 1
'    For m_lngY = 0 To m_Height - 1
'        For m_lngX = 0 To m_Width - 1
'            CopyMemory ByVal m_NewImage + (((m_lngY) + ((m_lngSub - m_lngX) * m_Height)) * 4), ByVal m_Image + (((m_lngY * m_Width) + (m_lngX)) * 4), 4
'        Next m_lngX
'    Next m_lngY
    m_lngTemp = m_Width
    m_Width = m_Height
    m_Height = m_lngTemp
    FreeBuffer
    m_Image = m_NewImage
End Sub

Function SmartSample(ByVal x As Long, ByVal y As Long, ByRef ColorValue As Long, ByRef Counter As Long) As Boolean
Attribute SmartSample.VB_Description = "Like GetPixel, but smart - useful for Blur filters."
On Error Resume Next
    If Not Initialized Then Exit Function
    If m_TileMode Then
        Counter = Counter + 1
        If x < 0 Then x = x + m_Width
        If y < 0 Then y = y + m_Height
        x = x Mod m_Width
        y = y Mod m_Height
        ColorValue = FastGet(x, y)
        SmartSample = True
    Else
        If (x >= 0) And (y >= 0) And (x < m_Width) And (y < m_Height) Then
            Counter = Counter + 1
            ColorValue = FastGet(x, y)
            SmartSample = True
        Else
            SmartSample = False
        End If
    End If
End Function

Public Sub Blur(Mode As F2BlurModes, Optional Wrap As Boolean = False)
Attribute Blur.VB_Description = "Applies an extremely slow blur filter to the image."
On Error Resume Next
Dim cX As Long, cY As Long, cR As Long, cG As Long, cB As Long, cA As Long, CC As Long, cP As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
    Select Case Mode
    Case F2BlurModes.f2bmTrilinear5x5
        If Wrap Then
            AccelTriBlur5x5W m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        Else
            AccelTriBlur5x5 m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        End If
    Case F2BlurModes.f2bmTrilinear3x3
        If Wrap Then
            AccelTriBlur3x3W m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        Else
            AccelTriBlur3x3 m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        End If
    Case F2BlurModes.f2bmBilinear5x5
        If Wrap Then
            AccelBiBlur5x5W m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        Else
            AccelBiBlur5x5 m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        End If
    Case F2BlurModes.f2bmBilinear3x3
        If Wrap Then
            AccelBiBlur3x3W m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        Else
            AccelBiBlur3x3 m_NewImage, m_Image, m_Width, m_Width, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, m_Width, m_Height)
        End If
    Case Else
    End Select
    FreeBuffer
    m_Image = m_NewImage
End Sub

Public Sub BlurEx(Area, Mode As F2BlurModes, Optional Wrap As Boolean = False)
On Error Resume Next
Dim cX As Long, cY As Long, cR As Long, cG As Long, cB As Long, cA As Long, CC As Long, cP As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
    Select Case Mode
    Case F2BlurModes.f2bmTrilinear5x5
        If Wrap Then
            AccelTriBlur5x5W m_NewImage, m_Image, m_Width, m_Width, Area, Area
        Else
            AccelTriBlur5x5 m_NewImage, m_Image, m_Width, m_Width, Area, Area
        End If
    Case F2BlurModes.f2bmTrilinear3x3
        If Wrap Then
            AccelTriBlur3x3W m_NewImage, m_Image, m_Width, m_Width, Area, Area
        Else
            AccelTriBlur3x3 m_NewImage, m_Image, m_Width, m_Width, Area, Area
        End If
    Case F2BlurModes.f2bmBilinear5x5
        If Wrap Then
            AccelBiBlur5x5W m_NewImage, m_Image, m_Width, m_Width, Area, Area
        Else
            AccelBiBlur5x5 m_NewImage, m_Image, m_Width, m_Width, Area, Area
        End If
    Case F2BlurModes.f2bmBilinear3x3
        If Wrap Then
            AccelBiBlur3x3W m_NewImage, m_Image, m_Width, m_Width, Area, Area
        Else
            AccelBiBlur3x3 m_NewImage, m_Image, m_Width, m_Width, Area, Area
        End If
    Case Else
    End Select
    FreeBuffer
    m_Image = m_NewImage
End Sub

Public Property Get Font() As StdFont
Attribute Font.VB_Description = "The font used for drawing GDI text."
    Set Font = m_Font
End Property

Public Property Set Font(NewFont As StdFont)
    Set m_Font = NewFont
End Property

Public Sub FloodFill(x As Long, y As Long, ByRef Color)
Attribute FloodFill.VB_Description = "Flood fills an image (Alpha blending doesn't always work right.)"
On Error Resume Next
Dim isSeen() As Byte, isDrawn() As Byte
Dim xPosition As New Collection, yPosition As New Collection, sColor As Long
Dim NextX As Long, NextY As Long, deltaX As Long, deltaY As Long, CurrentX As Long, CurrentY As Long, m_Color As Long, m_Alpha As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    If (x < 0) Or (y < 0) Or (x >= m_Width) Or (y >= m_Height) Then Err.Raise 10241, "FloodFill", "Out of bounds": Exit Sub
    ReDim isSeen(0 To m_Width - 1, 0 To m_Height - 1)
    isDrawn = isSeen
    sColor = FastGet(x, y)
    m_Color = CLng(Color)
    m_Alpha = GetAlpha(m_Color)
    xPosition.Add x
    yPosition.Add y
    isSeen(x, y) = 1
    If m_Alpha = 0 Then Exit Sub
    If m_Alpha = 255 Then
        Do Until xPosition.Count <= 0
            CurrentX = CLng(xPosition.Item(xPosition.Count))
            CurrentY = CLng(yPosition.Item(yPosition.Count))
            xPosition.Remove xPosition.Count
            yPosition.Remove yPosition.Count
            If isDrawn(CurrentX, CurrentY) = 1 Then Else FastSet CurrentX, CurrentY, m_Color
            isDrawn(CurrentX, CurrentY) = 1
            For deltaX = -1 To 1
                NextX = CurrentX + deltaX
                If (NextX >= 0) And (NextX < m_Width) Then
                    For deltaY = -1 To 1
                        NextY = CurrentY + deltaY
                        If (NextY >= 0) And (NextY < m_Height) Then
                            If FastGet(NextX, NextY) <> sColor Then
                            Else
                                If isSeen(NextX, NextY) = 0 Then
                                    xPosition.Add NextX
                                    yPosition.Add NextY
                                    isSeen(NextX, NextY) = 1
                                End If
                            End If
                        End If
                    Next deltaY
                End If
            Next deltaX
        Loop
    Else
        Do Until xPosition.Count <= 0
            CurrentX = CLng(xPosition.Item(xPosition.Count))
            CurrentY = CLng(yPosition.Item(yPosition.Count))
            xPosition.Remove xPosition.Count
            yPosition.Remove yPosition.Count
            If isDrawn(CurrentX, CurrentY) = 1 Then Else AlphaSet CurrentX, CurrentY, m_Color, m_Alpha
            isDrawn(CurrentX, CurrentY) = 1
            For deltaX = -1 To 1
                NextX = CurrentX + deltaX
                If (NextX >= 0) And (NextX < m_Width) Then
                    For deltaY = -1 To 1
                        NextY = CurrentY + deltaY
                        If (NextY >= 0) And (NextY < m_Height) Then
                            If FastGet(NextX, NextY) <> sColor Then
                            Else
                                If isSeen(NextX, NextY) = 0 Then
                                    xPosition.Add NextX
                                    yPosition.Add NextY
                                    isSeen(NextX, NextY) = 1
                                End If
                            End If
                        End If
                    Next deltaY
                End If
            Next deltaX
        Loop
    End If
End Sub

Public Sub FloodFillFast(x As Long, y As Long, ByRef Color)
On Error Resume Next
Dim isSeen() As Byte, isDrawn() As Byte
Dim xPosition As New Collection, yPosition As New Collection, sColor As Long
Dim NextX As Long, NextY As Long, deltaX As Long, deltaY As Long, CurrentX As Long, CurrentY As Long, m_Color As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    If (x < 0) Or (y < 0) Or (x >= m_Width) Or (y >= m_Height) Then Err.Raise 10241, "FloodFill", "Out of bounds": Exit Sub
    ReDim isSeen(0 To m_Width - 1, 0 To m_Height - 1)
    isDrawn = isSeen
    sColor = FastGet(x, y)
    m_Color = CLng(Color)
    xPosition.Add x
    yPosition.Add y
    isSeen(x, y) = 1
    Do Until xPosition.Count <= 0
        CurrentX = CLng(xPosition.Item(xPosition.Count))
        CurrentY = CLng(yPosition.Item(yPosition.Count))
        xPosition.Remove xPosition.Count
        yPosition.Remove yPosition.Count
        If isDrawn(CurrentX, CurrentY) = 1 Then Else FastSet CurrentX, CurrentY, m_Color
        isDrawn(CurrentX, CurrentY) = 1
        For deltaX = -1 To 1
            NextX = CurrentX + deltaX
            If (NextX >= 0) And (NextX < m_Width) Then
                For deltaY = -1 To 1
                    NextY = CurrentY + deltaY
                    If (NextY >= 0) And (NextY < m_Height) Then
                        If FastGet(NextX, NextY) <> sColor Then
                        Else
                            If isSeen(NextX, NextY) = 0 Then
                                xPosition.Add NextX
                                yPosition.Add NextY
                                isSeen(NextX, NextY) = 1
                            End If
                        End If
                    End If
                Next deltaY
            End If
        Next deltaX
    Loop
End Sub

Public Property Set Picture(NewPicture As IPictureDisp)
Attribute Picture.VB_Description = "You can use this like a picturebox's picture property."
On Error Resume Next
Dim m_NewPic As IPictureDisp, CopyScans As Long, imgWidth As Long, imgHeight As Long
Dim MaskArray() As Byte, m_iciIcon As IconInfo
    Set m_NewPic = NewPicture
    m_booDirty = True
    If m_NewPic Is Nothing Then
    Else
        If NewPicture.Type = picType.PicType_Bitmap Then
            LoadedAlpha = False
            imgWidth = GetPictureWidth(m_NewPic)
            imgHeight = GetPictureHeight(m_NewPic)
            Resize imgWidth, imgHeight
            Call GetPictureArrayPtr(m_NewPic, m_Image)
            m_Width = imgWidth
            m_Height = imgHeight
            UpdateClipRect
            m_Initialized = True
        ElseIf NewPicture.Type = picType.PicType_Icon Then
            Call GetIconInfo(m_NewPic.Handle, m_iciIcon)
            Set m_NewPic = BitmapFromHandle(m_iciIcon.hbmColor)
            imgWidth = GetPictureWidth(m_NewPic)
            imgHeight = GetPictureHeight(m_NewPic)
            Resize imgWidth, imgHeight
            ReDim MaskArray(0 To (imgWidth * 4) - 1, 0 To imgHeight - 1)
            Call GetPictureArrayPtr(m_NewPic, m_Image)
            Set m_NewPic = BitmapFromHandle(m_iciIcon.hbmMask)
            Call GetPictureArrayByte(m_NewPic, MaskArray)
            mdlSoftFX.AccelInvert VarPtr(MaskArray(0, 0)), imgWidth, F2Rect(0, 0, imgWidth, imgHeight, False), False
            mdlSoftFX.AccelCopyChannel m_Image, VarPtr(MaskArray(0, 0)), imgWidth, imgWidth, F2Rect(0, 0, imgWidth, imgHeight, False), F2Rect(0, 0, imgWidth, imgHeight, False), 3, 0
            Erase MaskArray
            m_Width = imgWidth
            m_Height = imgHeight
            UpdateClipRect
            m_Initialized = True
        End If
    End If
    Set m_NewPic = Nothing
End Property

Public Sub CopyToPicture(ByRef Pic As IPictureDisp)
On Error Resume Next
Dim memDC As Long
    memDC = CreateMemoryDC
    BytePtrToPicture Pic, m_Image, memDC
    DeleteMemoryDC memDC
    Err.Clear
End Sub

Public Sub CopyToPictureAlphaComposite(ByRef Pic As IPictureDisp, Optional BKColor As Long = -12558208)
On Error Resume Next
Dim m_imgNew As Fury2Image
    Set m_imgNew = F2Image(m_Width, m_Height)
    m_imgNew.Clear BKColor
    m_imgNew.BlitEx m_imgNew.GetRect, Me.GetRect, Me, f2bSourceAlpha, 1
    m_imgNew.CopyToPicture Pic
    Set m_imgNew = Nothing
End Sub

Public Property Get Picture() As IPictureDisp
On Error Resume Next
Dim m_NewPic As IPictureDisp, memDC As Long
    If Not Initialized Then Exit Property
    Set m_NewPic = CreatePicture(m_Width, m_Height)
    memDC = CreateMemoryDC
    BytePtrToPicture m_NewPic, m_Image, memDC
    DeleteMemoryDC memDC
    Set Picture = m_NewPic
    Set m_NewPic = Nothing
    Err.Clear
End Property

Public Property Get AlphaCompositePicture(Optional BKColor As Long = -12558208) As IPictureDisp
On Error Resume Next
Dim m_imgNew As Fury2Image
    Set m_imgNew = F2Image(m_Width, m_Height)
    m_imgNew.Clear BKColor
    m_imgNew.BlitEx m_imgNew.GetRect, Me.GetRect, Me, f2bSourceAlpha, 1
    Set AlphaCompositePicture = m_imgNew.Picture
    Set m_imgNew = Nothing
End Property

Public Function DrawTextMultiline(ByRef Area, ByVal Text As String, ByRef Color, Optional ByVal GammaBlend As Boolean = True, Optional ByVal BackColor As Long = 0, Optional ByVal ItemForCoord As Long = 1, Optional ByRef ReturnLineCount As Long, Optional ByRef ReturnLineHeights As Variant) As Long
On Error Resume Next
Dim picGuid As IId
Dim picDesc As PICTDESC
Dim hdcMem As Long
Dim hBmp As Long
Dim hOldBmp As Long
Dim Pic As StdPicture
Dim rcBitmap As GeRECT
Dim textExtent As Size
Dim hFont As IFont, hFontNum As Long
Dim textArray() As Byte
Dim pWidth As Long, pHeight As Long
Dim deskWnd As Long, deskDC As Long
Dim m_Alpha As Long, m_Color As Long, m_Mask As Long, m_TrueColor As Long
Dim tWidth As Long, tHeight As Long
Dim TSize As Long, TSizeEx As Int32
Dim SourceRect, OldFont As Long
Dim m_varLines As Variant, m_lngTotalHeight As Long, m_lngTotalWidth As Long, m_lngLineCount As Long, m_lngCurrentLine As Long
Dim m_lngCurrentY As Long, m_lngHeight() As Long, m_strCurrentText As String
Dim m_varHeights() As Long
Dim m_lngSrcLine As Long
Dim m_lngDepth As Long, m_strNextText As String
Dim m_strLines() As String
Dim m_lngClipPos As Long, m_lngClipPos2 As Long
Dim m_strClipText1 As String, m_strClipText2 As String
Dim m_szClip1 As Size, m_szClip2 As Size
    
    If Not Initialized Then Exit Function
    m_booDirty = True
    ClipRectEx Area
    GdiFlush
    
    If Area.Width <= 0 Or Area.Height <= 0 Then Exit Function
    
    If InStr(Text, vbCrLf) Then Else Text = Text + vbCrLf
    m_varLines = Split(Text, vbCrLf)
    m_lngLineCount = UBound(m_varLines) + 1
    ReDim m_lngHeight(LBound(m_varLines) To UBound(m_varLines))
    
    m_Color = CLng(Color)
    m_TrueColor = RGB(GetRed(m_Color), GetGreen(m_Color), GetBlue(m_Color))
    m_Alpha = GetAlpha(m_Color)
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
    picGuid.S1 = &HBF32
    picGuid.S2 = &H101A
    picGuid.C(0) = &H8B
    picGuid.C(1) = &HBB
    picGuid.C(2) = &H0
    picGuid.C(3) = &HAA
    picGuid.C(4) = &H0
    picGuid.C(5) = &H30
    picGuid.C(6) = &HC
    picGuid.C(7) = &HAB
    Set frmTextRender.Font = m_Font
    DrawTextMultiline = -32767
    ReDim m_strLines(0 To UBound(m_varLines))
    ReDim m_varHeights(0 To UBound(m_varLines))
    m_lngCurrentLine = 0
    m_lngSrcLine = 0
    Do
        If m_strNextText = "" And m_lngSrcLine > UBound(m_varLines) Then m_lngLineCount = m_lngCurrentLine + 1: Exit Do
        If m_strNextText = "" Then
            m_strCurrentText = m_varLines(m_lngSrcLine)
        Else
            m_strCurrentText = m_strNextText
            m_strNextText = ""
        End If
        If m_lngCurrentLine + 1 = ItemForCoord Then
            DrawTextMultiline = m_lngTotalHeight
        End If
        GetTextExtentPoint32 frmTextRender.hdc, m_strCurrentText, Len(m_strCurrentText), textExtent
        If textExtent.cX >= Area.Width Then
            m_lngDepth = 0
            Do Until m_lngDepth >= 99 Or ((textExtent.cX < Area.Width) And ((left(m_strNextText, 1) = " ") Or (left(m_strNextText, 1) = ",") Or (left(m_strNextText, 1) = ".")))
                m_lngDepth = m_lngDepth + 1
                m_strNextText = Right(m_strCurrentText, 1) + m_strNextText
                m_strCurrentText = left(m_strCurrentText, Len(m_strCurrentText) - 1)
                GetTextExtentPoint32 frmTextRender.hdc, m_strCurrentText, Len(m_strCurrentText), textExtent
            Loop
            m_strNextText = LTrim(m_strNextText)
        End If
        If m_lngCurrentLine > UBound(m_lngHeight) Then ReDim Preserve m_lngHeight(0 To m_lngCurrentLine)
        If m_lngHeight(m_lngCurrentLine) = 0 Then m_lngHeight(m_lngCurrentLine) = textExtent.cY
        m_lngTotalHeight = m_lngTotalHeight + textExtent.cY + 2
        If (m_lngSrcLine > 0) Then
            m_varHeights(m_lngSrcLine) = m_varHeights(m_lngSrcLine) + textExtent.cY
        Else
            m_varHeights(0) = m_varHeights(0) + textExtent.cY
        End If
        If textExtent.cX > m_lngTotalWidth Then m_lngTotalWidth = textExtent.cX
        If m_lngTotalHeight > Area.Height Then m_lngLineCount = m_lngCurrentLine + 1: Exit Do
        If m_lngCurrentLine > UBound(m_strLines) Then ReDim Preserve m_strLines(0 To m_lngCurrentLine)
        m_strLines(m_lngCurrentLine) = m_strCurrentText
        If m_strNextText = "" Then m_lngSrcLine = m_lngSrcLine + 1
        m_lngCurrentLine = m_lngCurrentLine + 1
    Loop
    textExtent.cX = m_lngTotalWidth
    textExtent.cY = m_lngTotalHeight
    hBmp = CreateCompatibleBitmap(frmTextRender.hdc, textExtent.cX, textExtent.cY)
    pWidth = textExtent.cX
    pHeight = textExtent.cY
    If hBmp = 0 Then
        Exit Function
    End If
    hOldBmp = SelectObject(frmTextRender.hdc, hBmp)
    rcBitmap.Bottom = textExtent.cY
    rcBitmap.Right = textExtent.cX
    rcBitmap.top = 0
    rcBitmap.left = 0
    If GammaBlend Then
        SetBkColor frmTextRender.hdc, &H0
        SetTextColor frmTextRender.hdc, &HFFFFFF
    Else
        SetBkColor frmTextRender.hdc, BackColor
        SetTextColor frmTextRender.hdc, m_TrueColor
    End If
    GDIFill frmTextRender.hdc, rcBitmap, 0
    GdiFlush
    m_strNextText = ""
    m_lngCurrentY = 0
    For m_lngCurrentLine = 0 To UBound(m_strLines)
        m_strCurrentText = m_strLines(m_lngCurrentLine)
        With rcBitmap
            .top = m_lngCurrentY
            .Bottom = m_lngCurrentY + m_lngHeight(m_lngCurrentLine)
        End With
        GDIDrawText frmTextRender.hdc, m_strCurrentText, Len(m_strCurrentText), rcBitmap, DT_LEFT Or DT_TOP
        m_lngCurrentY = m_lngCurrentY + m_lngHeight(m_lngCurrentLine) + 2
    Next m_lngCurrentLine
    If IsMissing(ReturnLineHeights) Then Else ReturnLineHeights = m_varHeights()
    GdiFlush
    ReturnLineCount = m_lngLineCount
    SelectObject frmTextRender.hdc, hOldBmp
    ReDim textArray(0 To (pWidth * 4) - 1, 0 To pHeight - 1)
    Call GetHBmpArrayByte(hBmp, textArray, CompatiblityDC)
    If Area.Width > pWidth Then Area.Width = pWidth
    If Area.Height > pHeight Then Area.Height = pHeight
    tWidth = pWidth
    If tWidth > Area.Width Then tWidth = Area.Width
    tHeight = pHeight
    If tHeight > Area.Height Then tHeight = Area.Height
    frmTextRender.Move 0, 0, Screen.TwipsPerPixelX * 2, Screen.TwipsPerPixelY * 2
    DeleteObject hBmp
    Set SourceRect = F2Rect(0, 0, CLng(tWidth), CLng(tHeight))
    ClipRectsA Area, pWidth, pHeight, SourceRect
    If GammaBlend Then
        AccelBlitGEx m_Image, VarPtr(textArray(0, 0)), m_Width, pWidth, Area, SourceRect, m_Alpha, m_Color
    Else
        AccelBlitFast m_Image, VarPtr(textArray(0, 0)), m_Width, pWidth, Area, SourceRect
    End If
End Function

Public Sub DrawText(ByRef Area, ByVal Text As String, ByRef Color)
Attribute DrawText.VB_Description = "Draws GDI text using the surface's current Font."
On Error Resume Next
Dim picGuid As IId
Dim picDesc As PICTDESC
Dim hdcMem As Long
Dim hBmp As Long
Dim hOldBmp As Long
Dim Pic As StdPicture
Dim rcBitmap As GeRECT
Dim textExtent As Size
Dim hFont As IFont, hFontNum As Long
Dim textArray() As Byte
Dim pWidth As Long, pHeight As Long
Dim deskWnd As Long, deskDC As Long
Dim m_Alpha As Long, m_Color As Long
Dim tWidth As Long, tHeight As Long
Dim TSize As Long, TSizeEx As Int32
Dim SourceRect, OldFont As Long
Dim m_brsBlack As Long
    
    If Not Initialized Then Exit Sub
    m_booDirty = True
    GdiFlush
    ClipRectEx Area
    
    If Area.Width <= 0 Or Area.Height <= 0 Then Exit Sub
    
    m_Color = CLng(Color)
    m_Alpha = GetAlpha(m_Color)
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
    picGuid.S1 = &HBF32
    picGuid.S2 = &H101A
    picGuid.C(0) = &H8B
    picGuid.C(1) = &HBB
    picGuid.C(2) = &H0
    picGuid.C(3) = &HAA
    picGuid.C(4) = &H0
    picGuid.C(5) = &H30
    picGuid.C(6) = &HC
    picGuid.C(7) = &HAB
    Set frmTextRender.Font = m_Font
    textArray() = StrConv(Text, vbFromUnicode)
    ReDim Preserve textArray(LBound(textArray) To UBound(textArray) + 2)
    GetTextExtentPoint32B frmTextRender.hdc, ByVal VarPtr(textArray(0)), Len(Text), textExtent
    hBmp = CreateCompatibleBitmap(frmTextRender.hdc, textExtent.cX, textExtent.cY)
    pWidth = textExtent.cX
    pHeight = textExtent.cY
    If hBmp = 0 Then
        Exit Sub
    End If
    hOldBmp = SelectObject(frmTextRender.hdc, hBmp)
    rcBitmap.Bottom = textExtent.cY
    rcBitmap.Right = textExtent.cX
    rcBitmap.top = 0
    rcBitmap.left = 0
    GdiFlush
    SetBkColor frmTextRender.hdc, &H0
    SetTextColor frmTextRender.hdc, &HFFFFFF
    GDIFill frmTextRender.hdc, rcBitmap, 0
    GDIDrawTextB frmTextRender.hdc, ByVal VarPtr(textArray(0)), Len(Text), rcBitmap, DT_LEFT Or DT_TOP
    GdiFlush
    SelectObject frmTextRender.hdc, hOldBmp
    ReDim textArray(0 To (pWidth * 4) - 1, 0 To pHeight - 1)
    Call GetHBmpArrayByte(hBmp, textArray, CompatiblityDC)
    If Area.Width > pWidth Then Area.Width = pWidth
    If Area.Height > pHeight Then Area.Height = pHeight
    tWidth = pWidth
    If tWidth > Area.Width Then tWidth = Area.Width
    tHeight = pHeight
    If tHeight > Area.Height Then tHeight = Area.Height
    DeleteObject hBmp
    frmTextRender.Move 0, 0, Screen.TwipsPerPixelX * 2, Screen.TwipsPerPixelY * 2
    Set SourceRect = F2Rect(0, 0, CLng(tWidth), CLng(tHeight))
    ClipRectsA Area, pWidth, pHeight, SourceRect
    'AccelFlip VarPtr(textArray(0)), VarPtr(textArray2(0)), pWidth, pHeight
    AccelBlitGEx m_Image, VarPtr(textArray(0, 0)), m_Width, pWidth, Area, SourceRect, m_Alpha, m_Color
End Sub

Public Sub DrawTextSoft(ByRef Area, ByVal Text As String, ByRef Color)
On Error Resume Next
Dim picGuid As IId
Dim picDesc As PICTDESC
Dim hdcMem As Long
Dim hBmp As Long
Dim hOldBmp As Long
Dim Pic As StdPicture
Dim rcBitmap As GeRECT
Dim textExtent As Size
Dim hFont As IFont
Dim textArray() As Byte
Dim pWidth As Long, pHeight As Long
Dim deskWnd As Long, deskDC As Long
Dim m_Alpha As Long, m_Color As Long
Dim tWidth As Long, tHeight As Long
Dim SourceRect, hFontNum As Long
    
    If Not Initialized Then Exit Sub
    m_booDirty = True
    ClipRectEx Area
    GdiFlush
    
    If Area.Width <= 0 Or Area.Height <= 0 Then Exit Sub
    
    m_Color = CLng(Color)
    m_Alpha = GetAlpha(m_Color)
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
    picGuid.S1 = &HBF32
    picGuid.S2 = &H101A
    picGuid.C(0) = &H8B
    picGuid.C(1) = &HBB
    picGuid.C(2) = &H0
    picGuid.C(3) = &HAA
    picGuid.C(4) = &H0
    picGuid.C(5) = &H30
    picGuid.C(6) = &HC
    picGuid.C(7) = &HAB
    Set frmTextRender.Font = m_Font
    GetTextExtentPoint32 frmTextRender.hdc, Text, Len(Text), textExtent
    hBmp = CreateCompatibleBitmap(frmTextRender.hdc, textExtent.cX, textExtent.cY)
    pWidth = textExtent.cX
    pHeight = textExtent.cY
    If hBmp = 0 Then
        Exit Sub
    End If
    hOldBmp = SelectObject(frmTextRender.hdc, hBmp)
    rcBitmap.Bottom = textExtent.cY
    rcBitmap.Right = textExtent.cX
    rcBitmap.top = 0
    rcBitmap.left = 0
    SetBkColor frmTextRender.hdc, &H0
    SetTextColor frmTextRender.hdc, &HFFFFFF
    GDIFill frmTextRender.hdc, rcBitmap, 0
    GDIDrawText frmTextRender.hdc, Text, Len(Text), rcBitmap, DT_LEFT Or DT_TOP
    GdiFlush
    SelectObject frmTextRender.hdc, hOldBmp
    ReDim textArray(0 To (pWidth * 4) - 1, 0 To pHeight - 1)
    Call GetHBmpArrayByte(hBmp, textArray, CompatiblityDC)
    If Area.Width > pWidth Then Area.Width = pWidth
    If Area.Height > pHeight Then Area.Height = pHeight
    tWidth = pWidth
    If tWidth > Area.Width Then tWidth = Area.Width
    tHeight = pHeight
    If tHeight > Area.Height Then tHeight = Area.Height
    DeleteObject hBmp
    frmTextRender.Move 0, 0, Screen.TwipsPerPixelX * 2, Screen.TwipsPerPixelY * 2
    'SelectObject hdcMem, OldFont
    DeleteDC hdcMem
    hFont.ReleaseHfont hFontNum
    Set SourceRect = F2Rect(0, 0, CLng(tWidth), CLng(tHeight))
    ClipRectsA Area, pWidth, pHeight, SourceRect
    AccelBiBlur3x3 VarPtr(textArray(0, 0)), VarPtr(textArray(0, 0)), pWidth, pWidth, F2Rect(0, 0, pWidth, pHeight), F2Rect(0, 0, pWidth, pHeight)
    AccelBlitGEx m_Image, VarPtr(textArray(0, 0)), m_Width, pWidth, Area, SourceRect, m_Alpha, m_Color
End Sub

Public Sub Slide(ByVal x As Long, ByVal y As Long)
Attribute Slide.VB_Description = "Slides the graphics inside the image by a certain number of pixels."
On Error Resume Next
Dim m_bpDest As BlitParam, m_bpSource As BlitParam, m_bpClip As BlitParam
    m_booDirty = True
    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
    FillParam m_bpDest, x, y, m_Width + x, m_Height + y, m_Width, m_NewImage
    FillParam m_bpSource, 0, 0, m_Width, m_Height, m_Width, m_Image
    m_bpClip = m_bpSource
    If SoftFX.ClipRectangles(m_bpDest, m_bpSource, m_bpClip) Then
        Call SoftFX.Blit(m_bpDest, m_bpSource)
        FreeBuffer
        m_Image = m_NewImage
    Else
        FreeBuffer
    End If
    If Not Initialized Then Exit Sub
End Sub

Public Sub Wrap(ByVal x As Long, ByVal y As Long)
On Error Resume Next
Dim m_bpDest As BlitParam, m_bpSource As BlitParam, m_bpClip As BlitParam
    x = x Mod m_Width
    y = y Mod m_Height
    If (x <> 0) And (y <> 0) Then
        Wrap x, 0
        Wrap 0, y
        Exit Sub
    End If
    m_booDirty = True
    m_NewImage = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, m_Width * m_Height * 4)
    FillParam m_bpDest, x, y, m_Width + x, m_Height + y, m_Width, m_NewImage
    FillParam m_bpSource, 0, 0, m_Width, m_Height, m_Width, m_Image
    m_bpClip = m_bpSource
    Call SoftFX.ClipRectangles(m_bpDest, m_bpSource, m_bpClip)
    Call SoftFX.Blit(m_bpDest, m_bpSource)
    If x < 0 Then
        If y < 0 Then
        ElseIf y > 0 Then
        Else
            FillParam m_bpDest, m_Width + x, 0, m_Width, m_Height, m_Width, m_NewImage
            FillParam m_bpSource, 0, 0, (-x), m_Height, m_Width, m_Image
            Call SoftFX.ClipRectangles(m_bpDest, m_bpSource, m_bpClip)
            Call SoftFX.Blit(m_bpDest, m_bpSource)
        End If
    ElseIf x > 0 Then
        If y < 0 Then
        ElseIf y > 0 Then
        Else
            FillParam m_bpDest, 0, 0, x, m_Height, m_Width, m_NewImage
            FillParam m_bpSource, m_Width - x, 0, m_Width, m_Height, m_Width, m_Image
            Call SoftFX.ClipRectangles(m_bpDest, m_bpSource, m_bpClip)
            Call SoftFX.Blit(m_bpDest, m_bpSource)
        End If
    Else
        If y < 0 Then
            FillParam m_bpDest, 0, m_Height + y, m_Width, m_Height, m_Width, m_NewImage
            FillParam m_bpSource, 0, 0, m_Width, (-y), m_Width, m_Image
            Call SoftFX.ClipRectangles(m_bpDest, m_bpSource, m_bpClip)
            Call SoftFX.Blit(m_bpDest, m_bpSource)
        ElseIf y > 0 Then
            FillParam m_bpDest, 0, 0, m_Width, y, m_Width, m_NewImage
            FillParam m_bpSource, 0, m_Height - y, m_Width, m_Height, m_Width, m_Image
            Call SoftFX.ClipRectangles(m_bpDest, m_bpSource, m_bpClip)
            Call SoftFX.Blit(m_bpDest, m_bpSource)
        Else
        End If
    End If
    FreeBuffer
    m_Image = m_NewImage
    If Not Initialized Then Exit Sub
End Sub

Public Sub SaveF2G(filename As String)
Attribute SaveF2G.VB_Description = "Saves the image as a Fury2 Graphics Engine 32-bit image."
On Error Resume Next
    If Not Initialized Then Exit Sub
    SaveF2GEx filename, f2cZLib
End Sub

Public Sub SaveJPG(filename As String, Optional Quality = 75)
On Error Resume Next
Dim tJ As JPEG_CORE_PROPERTIES_VB
Dim lPtr As Long
Dim lR As Long
Dim lFileSize As Long
Dim vfFile As New VirtualFile
Dim bFile() As Byte
    If Not Initialized Then Exit Sub
    lR = ijlInit(tJ)
    If lR = IJL_OK Then
        ' Set up the DIB information:
        tJ.DIBWidth = m_Width
        tJ.DIBHeight = m_Height
        vfFile.RawSave m_Image, m_Width * m_Height * 4
        vfFile.MoveCursor 1
        vfFile.UnPad 3, 1
        tJ.DIBBytes = vfFile.Pointer
        
        ' Set up the JPEG information:
        bFile = StrConv(filename, vbFromUnicode)
        ReDim Preserve bFile(0 To UBound(bFile) + 1) As Byte
        bFile(UBound(bFile)) = 0
        lPtr = VarPtr(bFile(0))
        CopyMemory tJ.JPGFile, lPtr, 4
        ' Store JPGWidth:
        tJ.JPGWidth = m_Width
        ' .. & JPGHeight member values:
        tJ.JPGHeight = m_Height
        ' Set the quality/compression to save:
        tJ.jquality = CLng(Quality)
              
        ' Write the image:
        lR = ijlWrite(tJ, IJL_JFILE_WRITEWHOLEIMAGE)
        
        ' Check for success:
        If lR = IJL_OK Then
           ' Success
        Else
           ' Throw error
           Err.Raise 26001, "Fury2GE", "IJL11.dll JPG save failed: " & lR
        End If
        
        ' Ensure we have freed memory
        ijlFree tJ
    Else
        ' Throw error:
        Err.Raise 26001, "Fury2GE", "Failed to initialize IJL11.dll: " & lR
    End If
End Sub

Public Sub SavePNG(filename As String)
On Error Resume Next
    If Not Initialized Then Exit Sub
    Call mdlIaeron.iaSaveImage(filename, m_Image, m_Width, m_Height)
End Sub

Public Sub SaveTST(filename As String, Optional TileSize As Long = 32)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_intVersion As Integer
Dim m_intTileCount As Integer
Dim m_intDetail As Integer
Dim m_lngX As Long, m_lngY As Long
Dim m_bytTile() As Byte
Dim m_bytTile2() As Byte
Dim m_bytConverted() As Byte
Dim m_bpDest As BlitParam, m_bpDest2 As BlitParam, m_bpSource As BlitParam
Dim m_pfDest As PixelFormat, m_pfSource As PixelFormat
Dim m_fpfFormat As Fury2PixelFormat
    If Not Initialized Then Exit Sub
    m_FileHandle = FreeFile
    Kill filename
    m_intVersion = 20
    m_intTileCount = (m_Width \ TileSize) * (m_Height \ TileSize)
    If TileSize = 32 Then
        m_intDetail = 1
    ElseIf TileSize = 16 Then
        m_intDetail = 2
    Else
        Err.Raise 10243, "SaveTST", "TST files only support 32x32 and 16x16 tiles"
        Exit Sub
    End If
    ReDim m_bytTile(0 To TileSize - 1, 0 To (TileSize * 4) - 1)
    ReDim m_bytTile2(0 To TileSize - 1, 0 To (TileSize * 4) - 1)
    ReDim m_bytConverted(0 To ((TileSize * TileSize) * 3) - 1)
    FillParam m_bpDest, 0, 0, TileSize, TileSize, TileSize, VarPtr(m_bytTile(0, 0))
    FillParam m_bpDest2, 0, 0, TileSize, TileSize, TileSize, VarPtr(m_bytTile2(0, 0))
    Set m_fpfFormat = New Fury2PixelFormat
    m_fpfFormat.SetPixelFormatPreset BGRA32
    m_pfSource = m_fpfFormat.GetPF
    m_fpfFormat.SetPixelFormatPreset RGB24
    m_pfDest = m_fpfFormat.GetPF
    Set m_fpfFormat = Nothing
    Open filename For Binary Access Write As #m_FileHandle
        Put #m_FileHandle, , m_intVersion
        Put #m_FileHandle, , m_intTileCount
        Put #m_FileHandle, , m_intDetail
        For m_lngY = 0 To (m_Height \ TileSize) - 1
            For m_lngX = 0 To (m_Width \ TileSize) - 1
                FillParam m_bpSource, m_lngX * TileSize, m_lngY * TileSize, (m_lngX + 1) * TileSize, (m_lngY + 1) * TileSize, m_Width, m_Image
                SoftFX.Fill m_bpDest, F2RGB(0, 1, 2, 0)
                SoftFX.Blit_Rotate90 m_bpDest2, m_bpSource
                SoftFX.Blit_Alpha_Masked m_bpDest, m_bpDest2, 255, 0
                SoftFX.ConvertColorSpace m_pfDest, m_pfSource, VarPtr(m_bytConverted(0)), VarPtr(m_bytTile(0, 0)), TileSize * TileSize
                Put #m_FileHandle, , m_bytConverted
            Next m_lngX
        Next m_lngY
    Close #m_FileHandle
End Sub

Public Sub SaveF2GEx(filename As String, Optional CompressionMethod As F2CompressionModes = f2cZLib)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_Header As F2GraphicHeaderv1dot01
    If Not Initialized Then Exit Sub
    With m_Header
        .MaskColor = m_MaskColor
        .Width = m_Width
        .Height = m_Height
        .Version = 101
        .CompressionMode = CLng(CompressionMethod)
    End With
    m_FileHandle = FreeFile
    Kill filename
    Open filename For Binary Access Write As #m_FileHandle
        Put #m_FileHandle, 1, m_Header
        If CompressionMethod = f2cNoCompression Then
            Put #m_FileHandle, 11, m_Image
        ElseIf CompressionMethod = f2cZLib Then
            SaveZLib32B m_FileHandle, 11, GetByteArray
        Else
        End If
    Close #m_FileHandle
End Sub

Public Sub LoadXYZ(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_hdrXYZ As XYZHeader
Dim m_bytData() As Byte
Dim m_bytNewData() As Byte
Dim m_lngPalette() As Long, m_lngColors As Long
Dim m_lngX As Long, m_lngY As Long
Dim m_lngYOffset As Long, m_lngStartOffset As Long
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    m_FileHandle = FreeFile
    Open filename For Binary Access Read As #m_FileHandle
        Get #m_FileHandle, 1, m_hdrXYZ
        ReDim m_bytData(0 To (LOF(m_FileHandle) - 9))
        Resize m_hdrXYZ.Width, m_hdrXYZ.Height
        Get #m_FileHandle, 9, m_bytData
        ReDim m_bytNewData(0 To ((CLng(m_hdrXYZ.Width) * CLng(m_hdrXYZ.Height)) - 1) + 2048)
        mdlCompression.zlibUnCompress m_bytNewData(0), (CLng(m_hdrXYZ.Width) * CLng(m_hdrXYZ.Height)) + (CLng(256) * 3), m_bytData(0), UBound(m_bytData) + 1
        ReDim m_lngPalette(0 To 255)
        SoftFX.PadData VarPtr(m_lngPalette(0)), VarPtr(m_bytNewData(0)), 3, 1, 256
        SoftFX.IntersperseData VarPtr(m_lngPalette(0)) + 3, 255, 3, 256
        SoftFX.SwapBytesData VarPtr(m_lngPalette(0)), 0, 2, 4, 256
        m_lngStartOffset = 256& * 3&
        For m_lngY = 0 To m_hdrXYZ.Height - 1
            m_lngYOffset = m_lngY * m_hdrXYZ.Width
            SoftFX.Depalettize m_Image + (m_lngY * m_Width * 4), VarPtr(m_bytNewData(m_lngStartOffset + m_lngYOffset)), VarPtr(m_lngPalette(0)), m_hdrXYZ.Width
        Next m_lngY
    Close #m_FileHandle
End Sub

Public Sub LoadGPH(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_lngX As Long, m_lngY As Long
Dim m_lngNumber As Long, m_lngColor As Long
Dim m_strText As String, m_lngSet As Long, m_lngSize As Long
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    m_FileHandle = FreeFile
    Open filename For Input As #m_FileHandle
        Line Input #m_FileHandle, m_strText
        Line Input #m_FileHandle, m_strText
        Line Input #m_FileHandle, m_strText
        Line Input #m_FileHandle, m_strText
        m_lngSize = CLng(m_strText)
        Line Input #m_FileHandle, m_strText
        If m_lngSize = 2 Then
            Resize 16, 16
            Do Until EOF(m_FileHandle) Or (m_lngY > 15)
                Line Input #m_FileHandle, m_strText
                m_lngNumber = CLng(m_strText)
                Line Input #m_FileHandle, m_strText
                m_lngColor = CLng(m_strText)
                m_lngColor = IIf(m_lngColor = -1, 0, BGRA(GetBlue(m_lngColor), GetGreen(m_lngColor), GetRed(m_lngColor), 255))
                For m_lngSet = 0 To m_lngNumber - 1
                    FastSet (m_lngX + m_lngSet) Mod 16, (m_lngY) + ((m_lngX + m_lngSet) \ 16), m_lngColor
                Next m_lngSet
                m_lngX = m_lngX + m_lngNumber
                If m_lngX > 15 Then m_lngX = m_lngX Mod 16: m_lngY = m_lngY + 1
            Loop
        ElseIf m_lngSize = 1 Then
            Resize 32, 32
            Do Until EOF(m_FileHandle) Or (m_lngY > 31)
                Line Input #m_FileHandle, m_strText
                m_lngNumber = CLng(m_strText)
                Line Input #m_FileHandle, m_strText
                m_lngColor = CLng(m_strText)
                m_lngColor = IIf(m_lngColor = -1, 0, BGRA(GetBlue(m_lngColor), GetGreen(m_lngColor), GetRed(m_lngColor), 255))
                For m_lngSet = 0 To m_lngNumber - 1
                    FastSet m_lngX + m_lngSet, m_lngY, m_lngColor
                Next m_lngSet
                m_lngX = m_lngX + m_lngNumber
                If m_lngX > 31 Then m_lngX = m_lngX Mod 32: m_lngY = m_lngY + 1
            Loop
        End If
    Close #m_FileHandle
    TileCount = 1
End Sub

Public Sub LoadTST(filename As String, Optional m_lngRowWidth As Long = 9)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_ctData() As ColorTriplet, m_lngValue As Long, m_lngNewData() As Long
Dim m_bytData() As Byte
Dim m_bytPalette() As Byte
Dim m_ConvertX As Long, m_ConvertY As Long, m_ctValue As ColorTriplet
Dim m_CurrentX As Long, m_CurrentY As Long, m_lngLoadFrames As Long
Dim m_intVersion As Integer, m_intNumTiles As Integer, m_intDetail As Integer, m_lngTileSize As Long
Dim m_lngLoadTiles As Long, m_ctMask As ColorTriplet, m_bpDest As BlitParam, m_bpSource As BlitParam
Dim m_lngRTile As Long, m_lngBPP As Long
    m_booDirty = True
    m_FileHandle = FreeFile
    If Len(Mid(filename, InStrRev(filename, ".") + 1)) > 3 Then
        m_lngRTile = CLng(Mid(filename, InStrRev(filename, ".") + 4)) - 1
        Open left(filename, InStrRev(filename, ".") - 1) + ".tst" For Binary Access Read As #m_FileHandle
            Get #m_FileHandle, , m_intVersion
            Get #m_FileHandle, , m_intNumTiles
            Get #m_FileHandle, , m_intDetail
            If m_intDetail = 1 Then
                m_lngTileSize = 32
                m_lngBPP = 24
            ElseIf m_intDetail = 2 Then
                m_lngTileSize = 16
                m_lngBPP = 24
            ElseIf m_intDetail = 3 Then
                m_lngTileSize = 32
                m_lngBPP = 8
            ElseIf m_intDetail = 4 Then
                m_lngTileSize = 16
                m_lngBPP = 8
            End If
            Resize m_lngTileSize, m_lngTileSize
            If m_lngBPP = 24 Then
                ReDim m_ctData(0 To m_lngTileSize - 1, 0 To m_lngTileSize - 1)
                For m_lngLoadTiles = 0 To m_intNumTiles - 1
                    Get #m_FileHandle, , m_ctData
                    If m_lngLoadTiles = m_lngRTile Then
                        ConvertTK m_Image, VarPtr(m_ctData(0, 0)), m_lngTileSize, m_lngTileSize
                    End If
                Next
            ElseIf m_lngBPP = 8 Then
                m_bytPalette = LoadResData("RPGTOOLKIT", "PALETTE")
                SoftFX.IntersperseData VarPtr(m_bytPalette(3)), 255, 3, 256
                ReDim m_bytData(0 To m_lngTileSize - 1, 0 To m_lngTileSize - 1)
                For m_lngLoadTiles = 0 To m_intNumTiles - 1
                    Get #m_FileHandle, , m_bytData
                    If m_lngLoadTiles = m_lngRTile Then
                        Depalettize m_Image, VarPtr(m_bytData(0, 0)), VarPtr(m_bytPalette(0)), m_lngTileSize * m_lngTileSize
                    End If
                Next m_lngLoadTiles
            End If
        Close #m_FileHandle
    Else
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
        Open filename For Binary Access Read As #m_FileHandle
            Get #m_FileHandle, , m_intVersion
            Get #m_FileHandle, , m_intNumTiles
            Get #m_FileHandle, , m_intDetail
            If m_intDetail = 1 Then
                m_lngTileSize = 32
                m_lngBPP = 24
            ElseIf m_intDetail = 2 Then
                m_lngTileSize = 16
                m_lngBPP = 24
            ElseIf m_intDetail = 3 Then
                m_lngTileSize = 32
                m_lngBPP = 8
            ElseIf m_intDetail = 4 Then
                m_lngTileSize = 16
                m_lngBPP = 8
            End If
            TileCount = m_intNumTiles
            If m_lngBPP = 24 Then
                Resize (m_lngRowWidth) * m_lngTileSize, (((m_intNumTiles - 1) \ m_lngRowWidth) + 1) * m_lngTileSize
                ReDim m_ctData(0 To m_lngTileSize - 1, 0 To m_lngTileSize - 1)
                ReDim m_lngData(0 To m_lngTileSize - 1, 0 To m_lngTileSize - 1)
                FillParam m_bpSource, 0, 0, m_lngTileSize, m_lngTileSize, m_lngTileSize, VarPtr(m_lngData(0, 0))
                For m_lngLoadTiles = 0 To m_intNumTiles - 1
                    If (m_lngLoadTiles Mod m_lngRowWidth = 0) And (m_lngLoadTiles > 0) Then
                        m_CurrentX = 0
                        m_CurrentY = m_CurrentY + m_lngTileSize
                    End If
                    Get #m_FileHandle, , m_ctData
                    FillParam m_bpDest, m_CurrentX, m_CurrentY, m_CurrentX + m_lngTileSize, m_CurrentY + m_lngTileSize, m_Width, m_Image
                    ConvertTK VarPtr(m_lngData(0, 0)), VarPtr(m_ctData(0, 0)), m_lngTileSize, m_lngTileSize
                    SoftFX.Blit m_bpDest, m_bpSource
                    m_CurrentX = m_CurrentX + m_lngTileSize
                Next
            ElseIf m_lngBPP = 8 Then
                m_bytPalette = LoadResData("RPGTOOLKIT", "PALETTE")
                SoftFX.IntersperseData VarPtr(m_bytPalette(3)), 255, 3, 256
                Resize (m_lngRowWidth) * m_lngTileSize, (((m_intNumTiles - 1) \ m_lngRowWidth) + 1) * m_lngTileSize
                ReDim m_bytData(0 To m_lngTileSize - 1, 0 To m_lngTileSize - 1)
                ReDim m_lngData(0 To m_lngTileSize - 1, 0 To m_lngTileSize - 1)
                FillParam m_bpSource, 0, 0, m_lngTileSize, m_lngTileSize, m_lngTileSize, VarPtr(m_lngData(0, 0))
                For m_lngLoadTiles = 0 To m_intNumTiles - 1
                    If (m_lngLoadTiles Mod m_lngRowWidth = 0) And (m_lngLoadTiles > 0) Then
                        m_CurrentX = 0
                        m_CurrentY = m_CurrentY + m_lngTileSize
                    End If
                    Get #m_FileHandle, , m_bytData
                    FillParam m_bpDest, m_CurrentX, m_CurrentY, m_CurrentX + m_lngTileSize, m_CurrentY + m_lngTileSize, m_Width, m_Image
                    Depalettize VarPtr(m_lngData(0, 0)), VarPtr(m_bytData(0, 0)), VarPtr(m_bytPalette(0)), m_lngTileSize * m_lngTileSize
                    SoftFX.Blit m_bpDest, m_bpSource
                    m_CurrentX = m_CurrentX + m_lngTileSize
                Next
            End If
        Close #m_FileHandle
    End If
    LoadedAlpha = True
    TileSize = m_lngTileSize
    m_MaskColor = F2RGB(0, 0, 0, 0)
    m_Initialized = True
End Sub

Public Sub LoadVSP(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_bytData() As Byte, m_lngValue As Long
Dim m_bytPalette() As Byte
Dim m_bytLoadPal() As Byte
Dim m_ConvertX As Long, m_ConvertY As Long, m_intTileCount As Integer
Dim m_CurrentX As Long, m_CurrentY As Long, m_lngLoadTiles As Long
Dim m_lngPalette As Long
Dim m_lngRTile As Long
    m_booDirty = True
    ReDim m_bytLoadPal(0 To (256 * 3) - 1)
    ReDim m_bytPalette(0 To (256 * 4) - 1)
    m_FileHandle = FreeFile
    If Len(Mid(filename, InStrRev(filename, ".") + 1)) > 3 Then
        m_lngRTile = CLng(Mid(filename, InStrRev(filename, ".") + 4)) - 1
        m_Initialized = True
        Resize 16, 16
        Open left(filename, InStrRev(filename, ".") + 3) For Binary Access Read As #m_FileHandle
            Get #m_FileHandle, 3, m_bytLoadPal
            Get #m_FileHandle, 771, m_intTileCount
            TileCount = 1
            SoftFX.TranslateData VarPtr(m_bytLoadPal(0)), 1, 768
            SoftFX.PadData VarPtr(m_bytPalette(0)), VarPtr(m_bytLoadPal(0)), 3, 1, 256
            SoftFX.IntersperseData VarPtr(m_bytPalette(3)), 255, 3, 256
            SoftFX.SwapBytesData VarPtr(m_bytPalette(0)), 0, 2, 4, 256
            m_bytPalette(3) = 0
            Erase m_bytLoadPal
            For m_lngLoadTiles = 0 To m_intTileCount - 1
                ReDim m_bytData(0 To 15, 0 To 15)
                Get #m_FileHandle, , m_bytData()
                If m_lngLoadTiles = m_lngRTile Then
                    For m_ConvertY = 0 To 15
                        Call SoftFX.Depalettize(m_Image + (m_ConvertY * m_Width * 4), VarPtr(m_bytData(0, m_ConvertY)), VarPtr(m_bytPalette(0)), 16)
                    Next m_ConvertY
                    Exit For
                End If
            Next m_lngLoadTiles
        Close #m_FileHandle
    Else
        If FileLen(filename) <= 0 Then Exit Sub
        If Trim(Dir(filename)) = "" Then Exit Sub
        m_Initialized = True
        Open filename For Binary Access Read As #m_FileHandle
            Get #m_FileHandle, 3, m_bytLoadPal
            Get #m_FileHandle, 771, m_intTileCount
            TileCount = m_intTileCount
            SoftFX.TranslateData VarPtr(m_bytLoadPal(0)), 1, 768
            SoftFX.PadData VarPtr(m_bytPalette(0)), VarPtr(m_bytLoadPal(0)), 3, 1, 256
            SoftFX.IntersperseData VarPtr(m_bytPalette(3)), 255, 3, 256
            SoftFX.SwapBytesData VarPtr(m_bytPalette(0)), 0, 2, 4, 256
            m_bytPalette(3) = 0
            Erase m_bytLoadPal
            Resize 288, ((m_intTileCount \ 18) + 1) * 16
            For m_lngLoadTiles = 0 To m_intTileCount - 1
                If (m_CurrentX >= 287) Then
                    m_CurrentX = 0
                    m_CurrentY = m_CurrentY + 16
                End If
                ReDim m_bytData(0 To 15, 0 To 15)
                Get #m_FileHandle, , m_bytData()
                For m_ConvertY = 0 To 15
                    Call SoftFX.Depalettize(m_Image + (m_CurrentX * 4) + ((m_CurrentY + m_ConvertY) * m_Width * 4), VarPtr(m_bytData(0, m_ConvertY)), VarPtr(m_bytPalette(0)), 16)
                Next m_ConvertY
                m_CurrentX = m_CurrentX + 16
            Next m_lngLoadTiles
        Close #m_FileHandle
    End If
    TileSize = 16
    m_MaskColor = F2RGB(0, 0, 0, 0)
End Sub

Public Sub LoadCHR(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_bytData() As Byte, m_lngValue As Long
Dim m_bytPalette() As Byte
Dim m_ConvertX As Long, m_ConvertY As Long
Dim m_CurrentX As Long, m_CurrentY As Long, m_lngLoadFrames As Long
Dim m_lngPalette As Long
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    Resize 80, 256
    m_FileHandle = FreeFile
    m_bytPalette = LoadResData("VERGE", "PALETTE")
    m_Initialized = True
    LoadedAlpha = True
    Open filename For Binary Access Read As #m_FileHandle
        Do Until Err <> 0 Or EOF(m_FileHandle)
            If (m_lngLoadFrames Mod 5 = 0) And (m_lngLoadFrames > 0) Then
                m_CurrentX = 0
                m_CurrentY = m_CurrentY + 32
            End If
            ReDim m_bytData(0 To 15, 0 To 31)
            Get #m_FileHandle, , m_bytData()
            For m_ConvertY = 0 To 31
                Call SoftFX.Depalettize(m_Image + (m_CurrentX * 4) + ((m_CurrentY + m_ConvertY) * m_Width * 4), VarPtr(m_bytData(0, m_ConvertY)), VarPtr(m_bytPalette(0)), 16)
            Next m_ConvertY
            m_CurrentX = m_CurrentX + 16
            m_lngLoadFrames = m_lngLoadFrames + 1
        Loop
    Close #m_FileHandle
    m_MaskColor = F2RGB(0, 0, 0, 0)
    ResizePreserve 80, m_CurrentY
    TileCount = (m_Width \ 16) * (m_Height \ 32)
End Sub

Public Sub LoadPPM(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_ConvertX As Long, m_ConvertY As Long
Dim m_CurrentX As Long, m_CurrentY As Long, m_lngLoadFrames As Long
Dim m_varLines As Variant, m_lngLines As Long, m_strLine As String
Dim m_bytData() As Byte, m_lngPixels As Long, m_lngRed As Long, m_lngGreen As Long, m_lngBlue As Long
Dim m_lngWidth As Long, m_lngHeight As Long
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    m_FileHandle = FreeFile
    m_Initialized = True
    LoadedAlpha = True
    Open filename For Binary As #m_FileHandle
        ReDim m_bytData(0 To LOF(m_FileHandle) - 1)
        Get #m_FileHandle, 1, m_bytData
        m_varLines = Split(StrConv(m_bytData, vbUnicode), Chr(10))
        Resize CLng(left(m_varLines(2), InStr(m_varLines(2), " "))), CLng(Mid(m_varLines(2), InStr(m_varLines(2), " ") + 1))
        m_lngWidth = CLng(left(m_varLines(2), InStr(m_varLines(2), " ")))
        For m_lngLines = LBound(m_varLines) + 3 To UBound(m_varLines)
            m_strLine = CStr(m_varLines(m_lngLines))
            Do While InStr(m_strLine, " ")
                m_lngRed = CLng(left(m_strLine, InStr(m_strLine, " ")))
                m_strLine = Mid(m_strLine, InStr(m_strLine, " ") + 1)
                m_lngGreen = CLng(left(m_strLine, InStr(m_strLine, " ")))
                m_strLine = Mid(m_strLine, InStr(m_strLine, " ") + 1)
                m_lngBlue = CLng(left(m_strLine, InStr(m_strLine, " ")))
                m_strLine = Mid(m_strLine, InStr(m_strLine, " ") + 1)
                FastSet m_CurrentX, m_CurrentY, BGRA(m_lngRed, m_lngGreen, m_lngBlue, 255)
                m_CurrentX = m_CurrentX + 1
                If m_CurrentX >= m_lngWidth Then m_CurrentX = 0: m_CurrentY = m_CurrentY + 1
            Loop
        Next m_lngLines
    Close #m_FileHandle
End Sub

Public Sub LoadFNT(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_bytData() As Byte, m_lngValue As Long
Dim m_ConvertX As Long, m_ConvertY As Long
Dim m_CurrentX As Long, m_CurrentY As Long, m_lngLoadFrames As Long
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    m_FileHandle = FreeFile
    m_Initialized = True
    LoadedAlpha = True
    If LCase(Trim(Right(filename, InStrRev(filename, "\") + 1))) = "\small.fnt" Then
        Resize 7 * (256 - 32), 9
        Open filename For Binary Access Read As #m_FileHandle
            Do Until Err <> 0 Or EOF(m_FileHandle)
                ReDim m_bytData(0 To 6, 0 To 8)
                Get #m_FileHandle, , m_bytData()
                For m_ConvertY = 0 To 8
                    For m_ConvertX = 0 To 6
                        m_lngValue = m_bytData(m_ConvertX, m_ConvertY)
                        FastSet m_ConvertX + m_CurrentX, m_ConvertY + m_CurrentY, IIf(m_lngValue <> 0, &HFFFFFFFF, 0)
                    Next m_ConvertX
                Next m_ConvertY
                m_CurrentX = m_CurrentX + 7
                m_lngLoadFrames = m_lngLoadFrames + 1
            Loop
        Close #m_FileHandle
        m_MaskColor = F2RGB(0, 0, 0, 0)
        ResizePreserve 7 * (m_lngLoadFrames + 1), 9
    Else
        Resize 9 * (256 - 32), 16
        Open filename For Binary Access Read As #m_FileHandle
            Do Until Err <> 0 Or EOF(m_FileHandle)
                ReDim m_bytData(0 To 8, 0 To 15)
                Get #m_FileHandle, , m_bytData()
                For m_ConvertY = 0 To 15
                    For m_ConvertX = 0 To 8
                        m_lngValue = m_bytData(m_ConvertX, m_ConvertY)
                        FastSet m_ConvertX + m_CurrentX, m_ConvertY + m_CurrentY, IIf(m_lngValue <> 0, &HFFFFFFFF, 0)
                    Next m_ConvertX
                Next m_ConvertY
                m_CurrentX = m_CurrentX + 9
                m_lngLoadFrames = m_lngLoadFrames + 1
            Loop
        Close #m_FileHandle
        m_MaskColor = F2RGB(0, 0, 0, 0)
        ResizePreserve 9 * (m_lngLoadFrames + 1), 16
    End If
End Sub

Public Sub LoadRFN(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_bytData() As Byte, m_lngValue As Long, m_lngData() As Long
Dim m_ConvertX As Long, m_ConvertY As Long
Dim m_CurrentX As Long, m_CurrentY As Long, m_lngLoadChars As Long
Dim m_rfhHeader As RFN_Header, m_rchHeader As RFN_CharHeader
Dim m_rfdData As RFN_CharFile
Dim m_bytFluff(0 To 27) As Byte
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    m_FileHandle = FreeFile
    m_Initialized = True
    LoadedAlpha = True
    Open filename For Binary Access Read As #m_FileHandle
        Get #m_FileHandle, , m_rfhHeader
        m_rfdData.Header = m_rfhHeader
        ReDim m_rfdData.Chars(0 To m_rfhHeader.NumberOfChars - 1)
        If m_rfhHeader.Version = 1 Then
            For m_lngLoadChars = 0 To m_rfhHeader.NumberOfChars - 1
                Get #m_FileHandle, , m_rchHeader
                Get #m_FileHandle, , m_bytFluff
                ReDim m_bytData(0 To m_rchHeader.Width - 1, 0 To m_rchHeader.Height - 1)
                m_rfdData.Chars(m_lngLoadChars) = m_rchHeader
                Get #m_FileHandle, , m_bytData
                If m_lngLoadChars = 0 Then
                    Resize m_rchHeader.Width, m_rchHeader.Height
                Else
                    ResizePreserve m_Width + m_rchHeader.Width, IIf(m_rchHeader.Height > m_Height, m_rchHeader.Height, m_Height)
                End If
                For m_ConvertY = 0 To m_rchHeader.Height - 1
                    For m_ConvertX = 0 To m_rchHeader.Width - 1
                        FastSet m_ConvertX + m_CurrentX, m_ConvertY, BGRA(255, 255, 255, m_bytData(m_ConvertX, m_ConvertY))
                    Next m_ConvertX
                Next m_ConvertY
                m_CurrentX = m_CurrentX + m_rchHeader.Width
            Next m_lngLoadChars
        ElseIf m_rfhHeader.Version = 2 Then
            For m_lngLoadChars = 0 To m_rfhHeader.NumberOfChars - 1
                Get #m_FileHandle, , m_rchHeader
                Get #m_FileHandle, , m_bytFluff
                ReDim m_lngData(0 To m_rchHeader.Width - 1, 0 To m_rchHeader.Height - 1)
                m_rfdData.Chars(m_lngLoadChars) = m_rchHeader
                Get #m_FileHandle, , m_lngData
                If m_lngLoadChars = 0 Then
                    Resize m_rchHeader.Width, m_rchHeader.Height
                Else
                    ResizePreserve m_Width + m_rchHeader.Width, IIf(m_rchHeader.Height > m_Height, m_rchHeader.Height, m_Height)
                End If
                For m_ConvertY = 0 To m_rchHeader.Height - 1
                    For m_ConvertX = 0 To m_rchHeader.Width - 1
                        FastSet m_ConvertX + m_CurrentX, m_ConvertY, SwapRedBlue(m_lngData(m_ConvertX, m_ConvertY))
                    Next m_ConvertX
                Next m_ConvertY
                m_CurrentX = m_CurrentX + m_rchHeader.Width
            Next m_lngLoadChars
        End If
    Close #m_FileHandle
    FontData = m_rfdData
    m_MaskColor = F2RGB(0, 0, 0, 0)
End Sub

Public Sub LoadRTS(filename As String)
On Error Resume Next
Dim m_vfFile As VirtualFile
Dim m_strHeader As String
Dim m_intVersion As Integer, m_intTileWidth As Integer, m_intTileHeight As Integer, m_intTileBPP As Integer, m_intCount As Integer
Dim m_lngTiles As Long, m_lngX As Long, m_lngY As Long
Dim m_lngCX As Long, m_lngCY As Long
Dim m_lngPixels() As Long
Dim m_bpSource As BlitParam, m_bpDest As BlitParam
    Set m_vfFile = New VirtualFile
    m_vfFile.LoadFile filename
    With m_vfFile
        m_strHeader = Space(2)
        .RawLoad StrPtr(m_strHeader), 4
        m_strHeader = StrConv(m_strHeader, vbUnicode)
        If m_strHeader <> ".rts" Then Exit Sub
        .Load m_intVersion
        If m_intVersion <> 1 Then Exit Sub
        .Load m_intCount
        If m_intCount < 1 Then Exit Sub
        .Load m_intTileWidth
        .Load m_intTileHeight
        .Load m_intTileBPP
        .Skip 242
        Resize m_intTileWidth * 16, m_intTileHeight * ((m_intCount \ 16) + 1)
        ReDim m_lngPixels(0 To m_intTileWidth - 1, 0 To m_intTileHeight - 1)
        FillParam m_bpSource, 0, 0, m_intTileWidth, m_intTileHeight, m_intTileWidth, VarPtr(m_lngPixels(0, 0))
        FillParam m_bpDest, 0, 0, m_intTileWidth, m_intTileHeight, m_Width, m_Image
        For m_lngTiles = 1 To m_intCount
            If m_intTileBPP = 32 Then
                .RawLoad VarPtr(m_lngPixels(0, 0)), m_intTileWidth * m_intTileHeight * (m_intTileBPP \ 8)
                FillParam m_bpDest, m_lngCX * m_intTileWidth, m_lngCY * m_intTileHeight, (m_lngCX + 1) * m_intTileWidth, (m_lngCY + 1) * m_intTileHeight, m_Width, m_Image
                SoftFX.Blit m_bpDest, m_bpSource
            ElseIf m_intTileBPP = 24 Then
                For m_lngY = 0 To m_intTileHeight - 1
                    For m_lngX = 0 To m_intTileWidth - 1
                        .RawLoad VarPtr(m_lngPixels(m_lngX, m_lngY)), 3
                    Next m_lngX
                Next m_lngY
                FillParam m_bpDest, m_lngCX * m_intTileWidth, m_lngCY * m_intTileHeight, (m_lngCX + 1) * m_intTileWidth, (m_lngCY + 1) * m_intTileHeight, m_Width, m_Image
                SoftFX.Blit m_bpDest, m_bpSource
            End If
            m_lngCX = m_lngCX + 1
            If m_lngCX > 15 Then m_lngCX = 0: m_lngCY = m_lngCY + 1
            If m_lngCY > (m_Height \ m_intTileHeight) Then Exit For
        Next m_lngTiles
    End With
    SwapChannels f2aRed, f2aBlue
    LoadedAlpha = (m_intTileBPP = 32)
End Sub

Public Sub LoadRWS(filename As String)
On Error Resume Next
Dim m_FileHandle As Long
Dim m_lngValue As Long, m_lngData() As Long
Dim m_ConvertX As Long, m_ConvertY As Long
Dim m_CurrentX As Long, m_CurrentY As Long, m_lngLoadBitmaps As Long
Dim m_intWidth As Integer, m_intHeight As Integer
Dim m_rwsHeader As RWS_Header
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    m_FileHandle = FreeFile
    m_Initialized = True
    LoadedAlpha = True
    Open filename For Binary Access Read As #m_FileHandle
        Get #m_FileHandle, , m_rwsHeader
        If m_rwsHeader.Version = 1 Then
            ReDim m_lngData(0 To m_rwsHeader.EdgeWidth - 1, 0 To m_rwsHeader.EdgeWidth - 1)
            Resize m_rwsHeader.EdgeWidth * 3, m_rwsHeader.EdgeWidth * 3
            For m_lngLoadBitmaps = 0 To 8
                Get #m_FileHandle, , m_lngData
                For m_ConvertY = 0 To m_rwsHeader.EdgeWidth - 1
                    For m_ConvertX = 0 To m_rwsHeader.EdgeWidth - 1
                        FastSet m_ConvertX, m_ConvertY, SwapRedBlue(m_lngData(m_ConvertX, m_ConvertY))
                    Next m_ConvertX
                Next m_ConvertY
            Next m_lngLoadBitmaps
        ElseIf m_rwsHeader.Version = 2 Then
            For m_lngLoadBitmaps = 0 To 8
                Get #m_FileHandle, , m_intWidth
                Get #m_FileHandle, , m_intHeight
                ReDim m_lngData(0 To m_intWidth - 1, m_intHeight - 1)
                Get #m_FileHandle, , m_lngData
                If m_lngLoadBitmaps = 0 Then
                    Resize m_intWidth, m_intHeight
                Else
                    ResizePreserve m_Width + m_intWidth, IIf(m_intHeight > m_Height, m_intHeight, m_Height)
                End If
                For m_ConvertY = 0 To m_intHeight - 1
                    For m_ConvertX = 0 To m_intWidth - 1
                        FastSet m_ConvertX + m_CurrentX, m_ConvertY, SwapRedBlue(m_lngData(m_ConvertX, m_ConvertY))
                    Next m_ConvertX
                Next m_ConvertY
                m_CurrentX = m_CurrentX + m_intWidth
            Next m_lngLoadBitmaps
        End If
    Close #m_FileHandle
    m_MaskColor = F2RGB(0, 0, 0, 0)
End Sub

Public Sub LoadF2G(filename As String)
Attribute LoadF2G.VB_Description = "Loads a Fury2 Graphics Engine 32-bit image."
On Error Resume Next
Dim m_FileHandle As Long
Dim m_Header As F2GraphicHeaderv1dot01
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    m_booDirty = True
    m_Initialized = True
    m_FileHandle = FreeFile
    Open filename For Binary Access Read As #m_FileHandle
        If LOF(m_FileHandle) = 0 Then
            Err.Raise 10241, "LoadF2G", "File Not Found."
            Close #m_FileHandle
            Kill filename
            Exit Sub
        ElseIf LOF(m_FileHandle) < 10 Then
            Err.Raise 10242, "LoadF2G", "Invalid F2G File."
            Close #m_FileHandle
            Exit Sub
        End If
        Get #m_FileHandle, 1, m_Header
        If (m_Header.Version = 100) Or (m_Header.Version = 101) Then
            If (m_Header.Version = 101) Then m_MaskColor = m_Header.MaskColor
            Resize m_Header.Width, m_Header.Height
            If m_Header.CompressionMode = F2CompressionModes.f2cNoCompression Then
                Get #m_FileHandle, 11, m_Image
            ElseIf m_Header.CompressionMode = F2CompressionModes.f2cZLib Then
                LoadZLib32BP m_FileHandle, 11, m_Image, CLng(m_Header.Width), CLng(m_Header.Height)
            End If
            LoadedAlpha = True
        Else
        End If
    Close #m_FileHandle
    m_Width = CLng(m_Header.Width)
    m_Height = CLng(m_Header.Height)
    UpdateClipRect
End Sub

Public Sub LoadPNG(filename As String)
On Error Resume Next
Dim m_byteImage() As Byte
    If FileLen(filename) <= 0 Then Exit Sub
    If Trim(Dir(filename)) = "" Then Exit Sub
    LoadBitmap filename
    Exit Sub
'    m_booDirty = True
'    m_Initialized = True
'    If mdlPNG.LoadPNGFile32(Filename, m_byteImage()) Then
'        SetByteArray m_byteImage
'        LoadedAlpha = True
'    Else
'        LoadBitmap Filename
'    End If
'    UpdateClipRect
End Sub

Public Sub SwapChannels(ByVal Channel1 As Long, ByVal Channel2 As Long)
    If Not Initialized Then Exit Sub
    m_booDirty = True
    Err.Clear
    SoftFX.SwapBytesData m_Image, 0, 2, 4, m_Width * m_Height
End Sub

Public Sub LoadChannel(Source, ByVal DestChannel As Long, ByVal SourceChannel As Long)
Attribute LoadChannel.VB_Description = "Copies a channel (R,G,B,A) from one image to another."
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Source Is Nothing Then Exit Sub
    If Not Initialized Then Exit Sub
    m_booDirty = True
    Err.Clear
    TotalHeight = Source.Height
    If TotalHeight > m_Height Then TotalHeight = m_Height
    TotalWidth = Source.Width
    If TotalWidth > m_Width Then TotalWidth = m_Width
    AccelCopyChannel m_Image, Source.Pointer(0, 0), m_Width, Source.Width, F2Rect(0, 0, TotalWidth, TotalHeight), F2Rect(0, 0, TotalWidth, TotalHeight), DestChannel, SourceChannel
End Sub

Public Sub AdjustChannel(ByVal Channel As Long, ByVal Amount As Long)
Attribute AdjustChannel.VB_Description = "Adds or subtracts a value from one of the image's channels (R,G,B,A)."
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    If Amount < -255 Then Amount = -255
    If Amount > 255 Then Amount = 255
    m_booDirty = True
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelFadeChannel m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), CLng(Channel), Amount
End Sub

Public Sub AdjustRGB(ByVal Red As Long, ByVal Green As Long, ByVal Blue As Long)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    If Red < -255 Then Red = -255
    If Red > 255 Then Red = 255
    If Green < -255 Then Green = -255
    If Green > 255 Then Green = 255
    If Blue < -255 Then Blue = -255
    If Blue > 255 Then Blue = 255
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelFadeRGB m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Red, Green, Blue
End Sub

Public Sub AdjustGamma(ByVal Red As Long, ByVal Green As Long, ByVal Blue As Long)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    If Red < -255 Then Red = -255
    If Red > 255 Then Red = 255
    If Green < -255 Then Green = -255
    If Green > 255 Then Green = 255
    If Blue < -255 Then Blue = -255
    If Blue > 255 Then Blue = 255
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelGammaRGB m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Red + 255, Green + 255, Blue + 255
End Sub

Public Sub AdjustGammaEx(ByRef Area, ByVal Red As Long, ByVal Green As Long, ByVal Blue As Long)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    If Red < -255 Then Red = -255
    If Red > 255 Then Red = 255
    If Green < -255 Then Green = -255
    If Green > 255 Then Green = 255
    If Blue < -255 Then Blue = -255
    If Blue > 255 Then Blue = 255
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelGammaRGB m_Image, m_Width, F2Rect(Area.left, Area.top, Area.Right, Area.Bottom), Red + 255, Green + 255, Blue + 255
End Sub

Public Sub Adjust(ByVal Amount As Long)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    If Amount < -255 Then Amount = -255
    If Amount > 255 Then Amount = 255
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelFade m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Amount
End Sub

Public Sub Solarize(ByVal Level As Long)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    If Level < 1 Then Level = 1
    If Level > 7 Then Level = 7
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelSolarize m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Level
End Sub

Public Sub HueShift(ByVal Amount As Single)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    TotalHeight = m_Height
    TotalWidth = m_Width
    Amount = Amount * 4096!
    AccelHueShift m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), CLng(Amount)
End Sub

Public Sub Grayscale()
On Error Resume Next
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelGrayscale m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height)
End Sub

Public Sub Decay()
On Error Resume Next
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelDecay m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height)
End Sub

Public Sub SolarizeEx(ByRef Area, ByVal Level As Long)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    ClipRectEx Area
    If Level < 1 Then Level = 1
    If Level > 7 Then Level = 7
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelSolarize m_Image, m_Width, F2Rect(Area.left, Area.top, Area.Right, Area.Bottom), Level
End Sub

Public Sub AdjustEx(ByRef Area, ByVal Amount As Long)
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    ClipRectEx Area
    If Amount < -255 Then Amount = -255
    If Amount > 255 Then Amount = 255
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelFade m_Image, m_Width, F2Rect(Area.left, Area.top, Area.Right, Area.Bottom), Amount
End Sub

Public Sub GrayscaleEx(ByRef Area)
On Error Resume Next
Dim CopyScans As Long, TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    TotalHeight = m_Height
    TotalWidth = m_Width
    AccelGrayscale m_Image, m_Width, Area
End Sub

Public Sub InvertChannel(ByVal Channel As Long)
    If Not Initialized Then Exit Sub
    m_booDirty = True
    AccelInvertChannel m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Channel
End Sub

Public Sub FillChannel(ByVal Channel As Long, ByVal Value As Long)
Attribute FillChannel.VB_Description = "Fills one of the image's channels (R,G,B,A) with a value."
    If Not Initialized Then Exit Sub
    m_booDirty = True
    AccelFillChannel m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Channel, Value
End Sub

Public Sub SolarizeChannel(ByVal Channel As Long, ByVal Level As Long)
    If Not Initialized Then Exit Sub
    m_booDirty = True
    AccelSolarizeChannel m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Channel, Level
End Sub

Public Sub StaticFillChannel(ByVal Channel As Long)
    If Not Initialized Then Exit Sub
    m_booDirty = True
    Randomize Timer
    AccelStaticFillChannel m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), Channel, Rnd * 240000000
End Sub

Public Property Get MaskColor()
Attribute MaskColor.VB_Description = "The color used for masking. For no masking, set it to -32767 (don't ask)."
    Set MaskColor = F2ColorBGRA(m_MaskColor)
End Property

Public Property Let MaskColor(NewColor)
    m_MaskColor = CLng(NewColor)
End Property

Public Property Set MaskColor(NewColor)
    m_MaskColor = CLng(NewColor)
End Property

Public Sub Invert(Optional WithAlpha As Boolean = True)
Attribute Invert.VB_Description = "Inverts the colors of the image."
On Error Resume Next
    If Not Initialized Then Exit Sub
    m_booDirty = True
    AccelInvert m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), WithAlpha
End Sub

Public Sub ClipAlpha()
On Error Resume Next
    If Not Initialized Then Exit Sub
    m_booDirty = True
    AccelClipAlpha m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height)
End Sub

Public Function FindColor(Area, Color) As Boolean
On Error Resume Next
Dim m_bpFind As BlitParam
    If Not Initialized Then Exit Function
Dim m_araArea As Fury2Rect
    Set m_araArea = Area
    If m_araArea Is Nothing Then Exit Function
    ClipRect m_araArea
    If m_araArea.Width <= 0 Or m_araArea.Height <= 0 Then Exit Function
    FillParam m_bpFind, m_araArea.left, m_araArea.top, m_araArea.Right, m_araArea.Bottom, m_Width, m_Image
    FindColor = CBool(SoftFX.Filter_FindColor(m_bpFind, CLng(Color)))
End Function

Public Sub ReplaceColor(OldColor, NewColor)
Attribute ReplaceColor.VB_Description = "Replaces one color with another."
Dim TotalWidth As Long, TotalHeight As Long
    If Not Initialized Then Exit Sub
    TotalHeight = m_Height
    TotalWidth = m_Width
    m_booDirty = True
    AccelReplaceColor m_Image, m_Width, F2Rect(0, 0, m_Width, m_Height), CLng(OldColor), CLng(NewColor)
End Sub

Public Sub LoadBitmap(PictureOrFilename)
Attribute LoadBitmap.VB_Description = "Loads a Bitmap or a Picture object into the image."
On Error Resume Next
Dim m_NewPic As IPictureDisp, imgHeight As Long, imgWidth As Long
Dim m_lngHandle As Long
Dim m_strExt As String
Dim m_lngAlpha As Long
    If Not F2Initialized Then Exit Sub
    m_booDirty = True
    m_Initialized = True
    LoadedAlpha = False
    m_lngAlpha = 0
    If VarType(PictureOrFilename) = vbString Then
        Err.Clear
        m_lngHandle = iaLoadImageA(CStr(PictureOrFilename), m_lngAlpha)
        If m_lngHandle = 0 Or Err <> 0 Then Exit Sub
    Else
        Set m_NewPic = PictureOrFilename
    End If
    If m_lngHandle <> 0 Then
        m_strExt = Mid(PictureOrFilename, InStrRev(PictureOrFilename, ".") + 1)
        Free
        'iaSwapRedBlue m_lngHandle
        m_Width = iaGetWidth(m_lngHandle)
        m_Height = iaGetHeight(m_lngHandle)
        If m_Width <= 0 Or m_Height <= 0 Then Exit Sub
        m_Image = iaGetPointer(m_lngHandle)
        m_Iaeron = m_lngHandle
        m_booIaeron = True
        Select Case LCase(m_strExt)
        Case "tga", "png"
            LoadedAlpha = CBool(m_lngAlpha)
        Case Else
            LoadedAlpha = False
        End Select
        m_booDirty = True
        m_lngCurrentX = 0
        m_lngCurrentY = 0
        UpdateClipRect
        m_Initialized = True
    ElseIf m_NewPic Is Nothing Then
    Else
        imgWidth = GetPictureWidth(m_NewPic)
        imgHeight = GetPictureHeight(m_NewPic)
        Resize imgWidth, imgHeight
        Call GetPictureArrayPtr(m_NewPic, m_Image)
        m_Width = imgWidth
        m_Height = imgHeight
        UpdateClipRect
    End If
    Set m_NewPic = Nothing
End Sub

Public Property Get ForeColor()
Attribute ForeColor.VB_Description = "The color used for drawing GDI text... or something. :)"
    ForeColor = m_ForeColor
End Property

Public Property Let ForeColor(NewForeColor)
    m_ForeColor = CLng(NewForeColor)
    m_FontPR.FillColor = CLng(NewForeColor)
End Property

Public Property Set ForeColor(NewForeColor)
    m_ForeColor = CLng(NewForeColor)
    m_FontPR.FillColor = CLng(NewForeColor)
End Property

Public Sub SetClipRect(ByRef ClipRect)
    If Not Initialized Then Exit Sub
    Set m_ClipRect = ClipRect
End Sub

Public Function GetClipRect() As Fury2Rect
    If Not Initialized Then Exit Function
    Set GetClipRect = m_ClipRect
End Function

Sub ClipRects(ByRef DestRect, ByRef SourceImage, ByRef SourceRect)
Attribute ClipRects.VB_Description = "Clips two rects to two images' boundaries, adjusting coordinates for proper blits."
On Error Resume Next
Dim m_bpDest As BlitParam, m_bpSource As BlitParam, m_bpClip As BlitParam
Dim m_araDest As Fury2Rect, m_araSource As Fury2Rect, m_imgSource As Fury2Image
    If Not Initialized Then Exit Sub
    If DestRect Is Nothing Then Exit Sub
    If SourceRect Is Nothing Then Exit Sub
    Err.Clear
    Set m_araDest = DestRect
    Set m_araSource = SourceRect
    Set m_imgSource = SourceImage
    If Err <> 0 Then Exit Sub
    FillParam m_bpDest, m_araDest.left, m_araDest.top, m_araDest.Right, m_araDest.Bottom, m_Width, 0
    FillParam m_bpSource, m_araSource.left, m_araSource.top, m_araSource.Right, m_araSource.Bottom, m_imgSource.Width, 0
    FillParam m_bpClip, m_ClipRect.left, m_ClipRect.top, m_ClipRect.Right, m_ClipRect.Bottom, m_Width, 0
    SoftFX.ClipRectangles m_bpDest, m_bpSource, m_bpClip
    If m_bpDest.y2 > m_bpClip.y2 Then
        m_bpDest.y2 = m_bpClip.y2
        m_bpSource.y2 = m_bpSource.y1 + (m_bpDest.y2 - m_bpDest.y1)
    End If
    If ((m_bpDest.y2 - m_bpDest.y1) + (m_bpSource.y1)) > m_imgSource.Height Then
        m_bpDest.y2 = m_bpDest.y1 + (m_imgSource.Height - m_bpSource.y1)
        m_bpSource.y2 = m_bpSource.y1 + (m_imgSource.Height - m_bpSource.y1)
    End If
    If m_bpSource.y1 > m_imgSource.Height Then
        m_bpSource.y2 = m_imgSource.Height
        m_bpSource.y1 = m_imgSource.Height
        m_bpDest.y2 = m_bpDest.y1
    End If
    If m_bpSource.y2 > m_imgSource.Height Then
        m_bpSource.y2 = m_imgSource.Height
        m_bpDest.y2 = m_bpDest.y1 + (m_bpSource.y2 - m_bpSource.y1)
    End If
    If (m_bpSource.y2 - m_bpSource.y1) <> (m_bpDest.y2 - m_bpDest.y1) Then
        m_bpSource.y2 = m_bpSource.y1 + (m_bpDest.y2 - m_bpDest.y1)
    End If
    With m_bpDest
        m_araDest.SetValues CLng(.x1), CLng(.y1), CLng(.x2), CLng(.y2)
    End With
    With m_bpSource
        m_araSource.SetValues CLng(.x1), CLng(.y1), CLng(.x2), CLng(.y2)
    End With
End Sub

Sub ClipRectsScale(ByRef DestRect, ByRef SourceImage, ByRef SourceRect)
On Error Resume Next
Dim m_bpDest As BlitParam, m_bpSource As BlitParam, m_bpClip As BlitParam
Dim m_araDest As Fury2Rect, m_araSource As Fury2Rect, m_imgSource As Fury2Image
    If Not Initialized Then Exit Sub
    If DestRect Is Nothing Then Exit Sub
    If SourceRect Is Nothing Then Exit Sub
    If TypeOf DestRect Is Fury2Rect Then Else Exit Sub
    If TypeOf SourceRect Is Fury2Rect Then Else Exit Sub
    Set m_araDest = DestRect
    Set m_araSource = SourceRect
    Set m_imgSource = SourceImage
    FillParam m_bpDest, m_araDest.left, m_araDest.top, m_araDest.Right, m_araDest.Bottom, m_Width, 0
    FillParam m_bpSource, m_araSource.left, m_araSource.top, m_araSource.Right, m_araSource.Bottom, m_imgSource.Width, 0
    FillParam m_bpClip, m_ClipRect.left, m_ClipRect.top, m_ClipRect.Right, m_ClipRect.Bottom, m_Width, 0
    SoftFX.ClipRectangles_Scale m_bpDest, m_bpSource, m_bpClip
    With m_bpDest
        m_araDest.SetValues CLng(.x1), CLng(.y1), CLng(.x2), CLng(.y2)
    End With
    With m_bpSource
        m_araSource.SetValues CLng(.x1), CLng(.y1), CLng(.x2), CLng(.y2)
    End With
End Sub

Sub ClipRectsEx(ByRef DestRect, ByRef SourceRect, ByVal DestX1, ByVal DestY1, ByVal DestX2, ByVal DestY2, ByVal SourceX2, ByVal SourceY2)
On Error Resume Next
Dim m_araDest As Fury2Rect, m_araSource As Fury2Rect, m_imgSource As Fury2Image
    If Not Initialized Then Exit Sub
    If DestRect Is Nothing Then Exit Sub
    If SourceRect Is Nothing Then Exit Sub
    Err.Clear
    Set m_araDest = DestRect
    Set m_araSource = SourceRect
    If Err <> 0 Then Exit Sub
    If m_araDest.left < DestX1 Then m_araSource.left = m_araSource.left + -(m_araDest.left - DestX1): m_araDest.left = DestX1
    If m_araDest.top < DestY1 Then m_araSource.top = m_araSource.top + -(m_araDest.top - DestY1): m_araDest.top = DestY1
    If m_araDest.Right > DestX2 Then m_araDest.Right = DestX2
    If m_araDest.Bottom > DestY2 Then m_araDest.Bottom = DestY2
    If m_araSource.left < 0 Then m_araSource.left = 0
    If m_araSource.top < 0 Then m_araSource.top = 0
    If m_araSource.Right > SourceX2 Then m_araDest.Right = m_araDest.Right - (m_araSource.Right - SourceX2): m_araSource.Right = SourceX2
    If m_araSource.Bottom > SourceY2 Then m_araDest.Bottom = m_araDest.Bottom - (m_araSource.Bottom - SourceY2): m_araSource.Bottom = SourceY2
    If m_araSource.Width + m_araDest.left > DestX2 Then m_araSource.Width = DestX2 - m_araDest.left
    If m_araSource.Height + m_araDest.top > DestY2 Then m_araSource.Height = DestY2 - m_araDest.top
End Sub

Sub ClipRectsA(ByRef DestRect, ByVal SourceWidth As Long, ByVal sourceHeight As Long, ByRef SourceRect)
Attribute ClipRectsA.VB_Description = "Clips a pair of rects."
On Error Resume Next
Dim m_araDest As Fury2Rect, m_araSource As Fury2Rect, m_imgSource As Fury2Image
    If Not Initialized Then Exit Sub
    If DestRect Is Nothing Then Exit Sub
    If SourceRect Is Nothing Then Exit Sub
    Err.Clear
    Set m_araDest = DestRect
    Set m_araSource = SourceRect
    If Err <> 0 Then Exit Sub
    If m_araDest.left < 0 Then m_araDest.left = 0
    If m_araDest.top < 0 Then m_araDest.top = 0
    If m_araDest.Right > m_Width Then m_araDest.Right = m_Width
    If m_araDest.Bottom > m_Height Then m_araDest.Bottom = m_Height
    If m_araSource.left < 0 Then m_araSource.left = 0
    If m_araSource.top < 0 Then m_araSource.top = 0
    If m_araSource.Right > SourceWidth Then m_araSource.Right = SourceWidth
    If m_araSource.Bottom > sourceHeight Then m_araSource.Bottom = sourceHeight
    If m_araSource.Width + m_araDest.left > m_Width Then m_araSource.Width = m_Width - m_araDest.left
    If m_araSource.Height + m_araDest.top > m_Height Then m_araSource.Height = m_Height - m_araDest.top
End Sub

Sub ClipRect(TheRect)
Attribute ClipRect.VB_Description = "Clips a Fury2Rect to the boundaries of the image."
On Error Resume Next
Dim m_rctRect As Fury2Rect
    If Not Initialized Then Exit Sub
    If TheRect Is Nothing Then Exit Sub
    Set m_rctRect = TheRect
    If m_rctRect.left < m_ClipRect.left Then m_rctRect.left = m_ClipRect.left
    If m_rctRect.top < m_ClipRect.top Then m_rctRect.top = m_ClipRect.top
    If m_rctRect.Right > m_ClipRect.Right Then m_rctRect.Right = m_ClipRect.Right
    If m_rctRect.Bottom > m_ClipRect.Bottom Then m_rctRect.Bottom = m_ClipRect.Bottom
End Sub

Sub ClipRectEx(ByRef TheRect)
Attribute ClipRectEx.VB_Description = "Clips a rect to the image's boundaries, using a different clipping method."
On Error Resume Next
Dim x1 As Long, y1 As Long, x2 As Long, y2 As Long, temp As Long
    If Not Initialized Then Exit Sub
    If TheRect Is Nothing Then Exit Sub
    If TypeOf TheRect Is Fury2Rect Then Else Exit Sub
    x1 = TheRect.left
    y1 = TheRect.top
    x2 = TheRect.Right
    y2 = TheRect.Bottom
    If x1 < m_ClipRect.left Then x1 = m_ClipRect.left
    If y1 < m_ClipRect.top Then y1 = m_ClipRect.top
    If x2 > m_ClipRect.Right Then x2 = m_ClipRect.Right
    If y2 > m_ClipRect.Bottom Then y2 = m_ClipRect.Bottom
    If x1 > x2 Then
        temp = x2
        x2 = x1
        x1 = temp
    End If
    If y1 > y2 Then
        temp = y2
        y2 = y1
        y1 = temp
    End If
    If x1 < m_ClipRect.left Then x1 = m_ClipRect.left
    If y1 < m_ClipRect.top Then y1 = m_ClipRect.top
    If x2 > m_ClipRect.Right Then x2 = m_ClipRect.Right
    If y2 > m_ClipRect.Bottom Then y2 = m_ClipRect.Bottom
    TheRect.left = x1
    TheRect.top = y1
    TheRect.Right = x2
    TheRect.Bottom = y2
End Sub

Sub ClipRectExL(ByRef TheRect)
On Error Resume Next
Dim x1 As Long, y1 As Long, x2 As Long, y2 As Long, temp As Long
    If Not Initialized Then Exit Sub
    If TheRect Is Nothing Then Exit Sub
    If TypeOf TheRect Is Fury2Rect Then Else Exit Sub
    TheRect.GetValues x1, y1, x2, y2
    x1 = IIf(x1 <= 0, 1, x1)
    y1 = IIf(y1 <= 0, 1, y1)
    x2 = IIf(x2 >= m_Width, m_Width - 1, x2)
    y2 = IIf(y2 >= m_Height, m_Height - 1, y2)
    If x1 > x2 Then
        temp = x2
        x2 = x1
        x1 = temp
    End If
    If y1 > y2 Then
        temp = y2
        y2 = y1
        y1 = temp
    End If
    TheRect.SetValues x1, y1, x2, y2
End Sub

Sub Free()
Attribute Free.VB_Description = "Frees all memory used by the image buffer."
On Error Resume Next
    If Not Initialized Then Exit Sub
    LoadedAlpha = False
    FreeBuffer
    m_Width = -1
    m_Height = -1
    m_Image = 0
    m_Initialized = False
End Sub

Sub Resize(ByVal x As Long, ByVal y As Long)
Attribute Resize.VB_Description = "Resizes the buffer used by the image to a new size."
On Error Resume Next
    LoadedAlpha = False
    If x <= 0 Or y <= 0 Then Free: Exit Sub
    If m_Image <> 0 Then Free
    m_Image = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, x * y * 4)
    m_booDirty = True
    m_Width = x
    m_Height = y
    m_lngCurrentX = 0
    m_lngCurrentY = 0
    UpdateClipRect
    m_Initialized = True
End Sub

Sub ResizePreserve(ByVal x As Long, ByVal y As Long)
On Error Resume Next
Dim m_OldImage As Long
    If x <= 0 Or y <= 0 Then Free: Exit Sub
    If Not Initialized Then Exit Sub
    m_OldImage = m_Image
    m_Image = GlobalAlloc(GMEM_ZEROINIT Or GMEM_FIXED, x * y * 4)
    m_booDirty = True
    If m_Width <= 0 Or m_OldImage = 0 Then
        m_Width = x
        m_Height = y
        UpdateClipRect
        Exit Sub
    End If
    AccelBlitFast m_Image, m_OldImage, x, m_Width, F2Rect(0, 0, ClipValue(m_Width, 0, x), ClipValue(m_Height, 0, y)), F2Rect(0, 0, ClipValue(m_Width, 0, x), ClipValue(m_Height, 0, y))
    m_Width = x
    m_Height = y
    UpdateClipRect
    If m_booIaeron Then
        iaFreeImage m_Iaeron
        m_Iaeron = 0
        m_booIaeron = False
    Else
        If m_OldImage <> 0 Then Call GlobalFree(m_OldImage)
    End If
End Sub

Function TileUp(ByVal MinWidth As Long, ByVal MinHeight As Long) As Fury2Image
On Error Resume Next
Dim m_imgNew As Fury2Image
Dim m_lngXRatio As Long, m_lngYRatio As Long
    If MinWidth < m_Width Then
        m_lngXRatio = 1
    Else
        m_lngXRatio = Ceil(MinWidth / m_Width)
    End If
    If MinHeight < m_Height Then
        m_lngYRatio = 1
    Else
        m_lngYRatio = Ceil(MinHeight / m_Height)
    End If
    If (m_lngXRatio = 1) And (m_lngYRatio = 1) Then
        Set TileUp = Me
        Exit Function
    End If
    Set m_imgNew = F2Image(m_Width * m_lngXRatio, m_Height * m_lngYRatio)
    With m_imgNew
        .TileBlitEx .GetRect, Me.GetRect, Me, False, 1
    End With
    Set TileUp = m_imgNew
End Function

Function Rotate(Angle As Single, Optional Antialias As Boolean = True) As Fury2Image
On Error Resume Next
Dim NewAngle As Single
Dim SS As Single, CC As Single
Dim m_bpSource As BlitParam, m_bpDest As BlitParam
Dim Pi As Single
Dim m_dblX2 As Double, m_dblX1 As Double, m_dblY2 As Double, m_dblY1 As Double
    Pi = 3.14159265358979
    NewAngle = -3.1415927 * Angle / 180 ' To radian
    If (NewAngle > 0) Then
        NewAngle = NewAngle - 2 * Pi * Fix(NewAngle / 2 / Pi)
    Else
        NewAngle = -NewAngle + 2 * Pi * Fix(Abs(NewAngle) / 2 / Pi)
    End If
    SS = Sin(NewAngle)  'Sine of the rotation angle
    CC = Cos(NewAngle)  'Cosine of the rotation angle
    
    m_dblX2 = m_Width
    m_dblY2 = m_Height
    
    RotateRect m_dblX2, m_dblY2, m_dblX1, m_dblY1, NewAngle
    
    Set Rotate = F2Image(CLng(Round(m_dblX2)), CLng(Round(m_dblY2)))
    
    FillParam m_bpDest, 0, 0, Rotate.Width, Rotate.Height, Rotate.Width, Rotate.Pointer(0, 0)
    FillParam m_bpSource, 0, 0, m_Width, m_Height, m_Width, m_Image
    
    If Antialias Then
        Rotate_Bilinear m_bpDest, m_bpSource, Angle
    Else
        Rotate_Integer m_bpDest, m_bpSource, Angle
    End If
End Function

Function Resample(ByVal NewWidth As Long, ByVal NewHeight As Long, Optional ByVal Method As ResamplingMethods) As Fury2Image
On Error Resume Next
Dim newImage As Fury2Image
    If Not Initialized Then Exit Function
    If m_Width = NewWidth And m_Height = NewHeight Then Set newImage = Me.Duplicate
    Set newImage = F2Image(NewWidth, NewHeight)
    Select Case Method
    Case [Linear]
        ResamplePoint Me, newImage, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, NewWidth, NewHeight)
    Case [BiLinear]
        ResampleBiLinear Me, newImage, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, NewWidth, NewHeight)
    Case [SastraxiSmooth]
        ResampleSS Me, newImage, F2Rect(0, 0, m_Width, m_Height), F2Rect(0, 0, NewWidth, NewHeight)
    Case Else
    End Select
    newImage.LoadedAlpha = Me.LoadedAlpha
    newImage.MaskColor = Me.MaskColor
    Set Resample = newImage
    Set newImage = Nothing
End Function

Function ResampleEx(ByRef NewRectangle, ByRef OldRectangle, Optional ByVal Method As ResamplingMethods) As Fury2Image
On Error Resume Next
Dim newImage As Fury2Image
    If Not Initialized Then Exit Function
    Set newImage = F2Image(NewRectangle.Right, NewRectangle.Bottom)
    Select Case Method
    Case [Linear]
        ResamplePoint Me, newImage, ConvertRect(OldRectangle), ConvertRect(NewRectangle)
    Case [BiLinear]
        ResampleBiLinear Me, newImage, ConvertRect(OldRectangle), ConvertRect(NewRectangle)
    Case [SastraxiSmooth]
        ResampleSS Me, newImage, ConvertRect(OldRectangle), ConvertRect(NewRectangle)
    Case Else
    End Select
    newImage.LoadedAlpha = Me.LoadedAlpha
    newImage.MaskColor = Me.MaskColor
    Set ResampleEx = newImage
    Set newImage = Nothing
End Function

Public Property Get Width() As Long
Attribute Width.VB_Description = "Yeah, how wide is this thing?"
On Error Resume Next
    Width = m_Width
End Property

Public Property Get Height() As Long
Attribute Height.VB_Description = "Guess. :)"
On Error Resume Next
    Height = m_Height
End Property

Public Sub SetArray(NewArray() As Long)
Attribute SetArray.VB_Description = "Lets you change the buffer used by the image."
    If Not F2Initialized Then Exit Sub
    m_Width = (UBound(NewArray, 1) + 1)
    m_Height = UBound(NewArray, 2) + 1
    m_Initialized = True
    m_booDirty = True
    Resize UBound(NewArray, 1) + 1, UBound(NewArray, 2) + 1
    CopyMemory ByVal m_Image, NewArray(0, 0), m_Width * m_Height * 4
    UpdateClipRect
End Sub

Public Sub SetByteArray(NewArray() As Byte)
    If Not F2Initialized Then Exit Sub
    m_Width = (UBound(NewArray, 1) + 1) \ 4
    m_Height = UBound(NewArray, 2) + 1
    m_Initialized = True
    m_booDirty = True
    Resize (UBound(NewArray, 1) + 1) \ 4, UBound(NewArray, 2) + 1
    CopyMemory ByVal m_Image, NewArray(0, 0), m_Width * m_Height * 4
    UpdateClipRect
End Sub

Public Function GetByteArray() As Byte()
On Error Resume Next
Dim m_bytArray() As Byte
    If Not Initialized Then Exit Function
    ReDim m_bytArray(0 To (m_Width * 4) - 1, 0 To m_Height - 1)
    CopyMemory m_bytArray(0, 0), ByVal m_Image, m_Width * m_Height * 4
    GetByteArray = m_bytArray
End Function

Public Function GetConvertedArray(NewFormat As Fury2PixelFormat) As Byte()
On Error Resume Next
Dim m_bytNew() As Byte
Dim m_pfDest As PixelFormat, m_pfSource As PixelFormat
Dim m_fpfSource As Fury2PixelFormat
    If Not Initialized Then Exit Function
    Set m_fpfSource = New Fury2PixelFormat
    m_fpfSource.SetPixelFormatPreset BGRA32
    m_pfSource = m_fpfSource.GetPF
    Set m_fpfSource = Nothing
    m_pfDest = NewFormat.GetPF
    ReDim m_bytNew(0 To (m_Width * m_pfDest.Pitch) - 1, 0 To m_Height - 1)
    SoftFX.ConvertColorSpace m_pfDest, m_pfSource, ByVal VarPtr(m_bytNew(0, 0)), ByVal m_Image, m_Width * m_Height
End Function

Public Function GetArray() As Long()
Attribute GetArray.VB_Description = "A handy little array of the image's pixels."
Dim m_lngImage() As Long
    If Not Initialized Then Exit Function
    ReDim m_lngImage(0 To m_Width - 1, 0 To m_Height - 1)
    CopyMemory m_lngImage(0, 0), ByVal m_Image, (m_Width * m_Height) * 4
    GetArray = m_lngImage
End Function

Public Sub SetPixelWu(ByVal x As Single, ByVal y As Single, ByRef Color)
On Error Resume Next
Dim DestParam As BlitParam
    If Not Initialized Then Exit Sub
    m_booDirty = True
    FillParam DestParam, 0, 0, m_Width, m_Height, m_Width, m_Image
    SetPixel_Wu DestParam, x, y, CLng(Color)
End Sub

Public Sub SetPixel(ByVal x As Long, ByVal y As Long, ByRef Color)
Attribute SetPixel.VB_Description = "Sets a pixel, gee pretty complicated eh?"
On Error Resume Next
Dim m_lngColor As Long, m_lngAlpha As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    m_lngColor = CLng(Color)
    m_lngAlpha = GetAlpha(m_lngColor)
    If (x < 0) Or (y < 0) Or (x >= m_Width) Or (y >= m_Height) Then Exit Sub
    AlphaSet x, y, m_lngColor, m_lngAlpha
End Sub

Public Sub SetPixelFast(ByVal x As Long, ByVal y As Long, ByRef Color)
Attribute SetPixelFast.VB_Description = "Sets a pixel, but without alpha blending it."
On Error Resume Next
    If Not Initialized Then Exit Sub
    If (x < 0) Or (y < 0) Or (x >= m_Width) Or (y >= m_Height) Then Exit Sub
    FastSet x, y, CLng(Color)
End Sub

Public Function GetPixelAA(ByVal x As Single, ByVal y As Single) As Long
On Error Resume Next
Dim DestParam As BlitParam, m_lngRet As Long
    If Not Initialized Then Exit Function
    FillParam DestParam, 0, 0, m_Width, m_Height, m_Width, m_Image
    m_lngRet = SoftFX.GetPixel_Bilinear(DestParam, x, y)
    GetPixelAA = m_lngRet
End Function

Public Function GetPixelAARolloff(ByVal x As Single, ByVal y As Single) As Long
On Error Resume Next
Dim DestParam As BlitParam, m_lngRet As Long
    If Not Initialized Then Exit Function
    FillParam DestParam, 0, 0, m_Width, m_Height, m_Width, m_Image
    m_lngRet = SoftFX.GetPixel_Bilinear_Rolloff(DestParam, x, y)
    GetPixelAARolloff = m_lngRet
End Function

Public Function GetPixel(ByVal x As Long, ByVal y As Long) As Long
Attribute GetPixel.VB_Description = "Hey, you can get a pixel! And it's not slow! :D"
On Error Resume Next
Dim m_lngValue As Long
    If Not Initialized Then Exit Function
    If (x < 0) Or (y < 0) Or (x >= m_Width) Or (y >= m_Height) Then Err.Raise 10241, "GetPixel", "Out of bounds": Exit Function
    CopyMemory m_lngValue, ByVal m_Image + (x * 4) + (y * m_Width * 4), 4
    GetPixel = m_lngValue
End Function

Public Function GetPixelFast(ByVal x As Long, ByVal y As Long) As Long
On Error Resume Next
Dim m_lngValue As Long
    If Not Initialized Then Exit Function
    CopyMemory m_lngValue, ByVal m_Image + (x * 4) + (y * m_Width * 4), 4
    GetPixelFast = m_lngValue
End Function

Public Function GetPixelClip(ByVal x As Long, ByVal y As Long) As Long
On Error Resume Next
Dim m_lngValue As Long
    If Not Initialized Then Exit Function
    CopyMemory m_lngValue, ByVal m_Image + (ClipValue(x, 0, m_Width - 1) * 4) + (ClipValue(y, 0, m_Height - 1) * m_Width * 4), 4
    GetPixelClip = m_lngValue
End Function

Public Function GetPixelWrap(ByVal x As Long, ByVal y As Long) As Long
On Error Resume Next
Dim m_lngValue As Long
    If Not Initialized Then Exit Function
    CopyMemory m_lngValue, ByVal m_Image + (WrapValue(x, 0, m_Width - 1) * 4) + (WrapValue(y, 0, m_Height - 1) * m_Width * 4), 4
    GetPixelWrap = m_lngValue
End Function

Private Sub AlphaSet(ByRef x As Long, ByRef y As Long, ByRef Color As Long, ByRef Alpha As Long)
On Error Resume Next
Dim m_lngValue As Long
    If Not Initialized Then Exit Sub
    CopyMemory m_lngValue, ByVal m_Image + (x * 4) + (y * m_Width * 4), 4
    m_lngValue = Blend_Alpha(m_lngValue, Color, Alpha)
    CopyMemory ByVal m_Image + (x * 4) + (y * m_Width * 4), m_lngValue, 4
End Sub

Private Sub FastSet(ByRef x As Long, ByRef y As Long, ByVal Color As Long)
On Error Resume Next
    If Not Initialized Then Exit Sub
    CopyMemory ByVal m_Image + (x * 4) + (y * m_Width * 4), Color, 4
End Sub

Private Function FastGet(ByRef x As Long, ByRef y As Long) As Long
On Error Resume Next
Dim m_lngValue As Long
    If Not Initialized Then Exit Function
    CopyMemory m_lngValue, ByVal m_Image + (x * 4) + (y * m_Width * 4), 4
    FastGet = m_lngValue
End Function

Public Sub DrawCircle(ByVal x As Long, ByVal y As Long, ByVal Radius As Long, ByRef Color)
Attribute DrawCircle.VB_Description = "Draws a circle. (Not an ellipse!)"
On Error Resume Next
Dim cX As Long, cY As Long, U As Long, V As Long, E As Long
Dim dx As Long, dy As Long
Dim m_Color As Long, m_Alpha As Long
    If Not Initialized Then Exit Sub
    'Bresenham Circle Algorithm
    'Based on implementation from http://www.ericgiguere.com/microjava/cldc_circle.html
    m_booDirty = True
    
    m_Color = CLng(Color)
    m_Alpha = GetAlpha(m_Color)
    
    cX = 0
    cY = Radius
    E = 0
    U = 1
    V = 2 * Radius - 1
    
    Do While (cX < cY)

        dx = x + cX: dy = y + cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha
        dx = x + cY: dy = y - cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha
        dx = x - cX: dy = y - cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha
        dx = x - cY: dy = y + cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha
        
        cX = cX + 1
        E = E + U
        U = U + 2
        If V < 2 * E Then cY = cY - 1: E = E - V: V = V - 2
        If (cX > cY) Then Exit Do
        
        dx = x + cY: dy = y + cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha
        dx = x + cX: dy = y - cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha
        dx = x - cY: dy = y - cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha
        dx = x - cX: dy = y + cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then AlphaSet dx, dy, m_Color, m_Alpha

    Loop
    
End Sub

Public Sub DrawCircleFilled(ByVal x As Long, ByVal y As Long, ByVal Radius As Long, ByRef Color)
On Error Resume Next
Dim cX As Long, cY As Long, U As Long, V As Long, E As Long
Dim dx As Long, dy As Long
Dim rowStart() As Long, rowEnd() As Long
Dim m_Color As Long, m_Alpha As Long
Dim FillRows As Long
    If Not Initialized Then Exit Sub
    'Bresenham Circle Algorithm
    'Based on implementation from http://www.ericgiguere.com/microjava/cldc_circle.html
    m_booDirty = True
    
    m_Color = CLng(Color)
    m_Alpha = GetAlpha(m_Color)
    
    ReDim rowStart(0 To (Radius * 2))
    ReDim rowEnd(0 To (Radius * 2))
    For FillRows = 0 To (Radius * 2)
        rowStart(FillRows) = x
        rowEnd(FillRows) = x
    Next FillRows
    
    cX = 0
    cY = Radius
    E = 0
    U = 1
    V = 2 * Radius - 1
    
    Do While (cX < cY)

        dx = x + cX: dy = y + cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x + cY: dy = y - cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cX: dy = y - cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cY: dy = y + cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        
        cX = cX + 1
        E = E + U
        U = U + 2
        If V < 2 * E Then cY = cY - 1: E = E - V: V = V - 2
        If (cX > cY) Then Exit Do
        
        dx = x + cY: dy = y + cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x + cX: dy = y - cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cY: dy = y - cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cX: dy = y + cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx

    Loop
    
    For FillRows = 0 To (Radius * 2)
        Fill F2Rect(rowStart(FillRows), (FillRows - Radius) + y, rowEnd(FillRows), (FillRows - Radius) + (y + 1), True), Color
    Next FillRows
    
End Sub

Public Sub DrawCircleFilledFast(ByVal x As Long, ByVal y As Long, ByVal Radius As Long, ByRef Color)
On Error Resume Next
Dim cX As Long, cY As Long, U As Long, V As Long, E As Long
Dim dx As Long, dy As Long
Dim rowStart() As Long, rowEnd() As Long
Dim m_Color As Long
Dim FillRows As Long
    If Not Initialized Then Exit Sub
    'Bresenham Circle Algorithm
    'Based on implementation from http://www.ericgiguere.com/microjava/cldc_circle.html
    m_booDirty = True
    
    m_Color = CLng(Color)
    
    ReDim rowStart(0 To (Radius * 2))
    ReDim rowEnd(0 To (Radius * 2))
    For FillRows = 0 To (Radius * 2)
        rowStart(FillRows) = x
        rowEnd(FillRows) = x
    Next FillRows
    
    cX = 0
    cY = Radius
    E = 0
    U = 1
    V = 2 * Radius - 1
    
    Do While (cX < cY)

        dx = x + cX: dy = y + cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x + cY: dy = y - cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cX: dy = y - cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cY: dy = y + cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        
        cX = cX + 1
        E = E + U
        U = U + 2
        If V < 2 * E Then cY = cY - 1: E = E - V: V = V - 2
        If (cX > cY) Then Exit Do
        
        dx = x + cY: dy = y + cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x + cX: dy = y - cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cY: dy = y - cX
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx
        dx = x - cX: dy = y + cY
        If dx < rowStart(dy - (y - Radius)) Then rowStart(dy - (y - Radius)) = dx
        If dx > rowEnd(dy - (y - Radius)) Then rowEnd(dy - (y - Radius)) = dx

    Loop
    
    For FillRows = 0 To (Radius * 2)
        FastFill F2Rect(rowStart(FillRows), (FillRows - Radius) + y, rowEnd(FillRows), (FillRows - Radius) + (y + 1), True), Color
    Next FillRows
    
End Sub

Public Sub DrawCircleFast(ByVal x As Long, ByVal y As Long, ByVal Radius As Long, ByRef Color)
Attribute DrawCircleFast.VB_Description = "Draws a circle without alpha blending."
On Error Resume Next
Dim cX As Long, cY As Long, U As Long, V As Long, E As Long
Dim dx As Long, dy As Long
Dim m_Color As Long, m_Alpha As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    'Bresenham Circle Algorithm
    'Based on implementation from http://www.ericgiguere.com/microjava/cldc_circle.html
    
    m_Color = CLng(Color)
    m_Alpha = GetAlpha(m_Color)
    
    cX = 0
    cY = Radius
    E = 0
    U = 1
    V = 2 * Radius - 1
    
    Do While (cX < cY)

        dx = x + cX: dy = y + cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color
        dx = x + cY: dy = y - cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color
        dx = x - cX: dy = y - cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color
        dx = x - cY: dy = y + cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color
        
        cX = cX + 1
        E = E + U
        U = U + 2
        If V < 2 * E Then cY = cY - 1: E = E - V: V = V - 2
        If (cX > cY) Then Exit Do
        
        dx = x + cY: dy = y + cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color
        dx = x + cX: dy = y - cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color
        dx = x - cY: dy = y - cX
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color
        dx = x - cX: dy = y + cY
        If Not ((dx < 0) Or (dy < 0) Or (dx >= m_Width) Or (dy >= m_Height)) Then FastSet dx, dy, m_Color

    Loop
    
End Sub

Public Sub DrawBoxWu(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single, ByRef Color)
On Error Resume Next
    DrawLineWu x1, y1, x2, y1, Color
    DrawLineWu x2, y1, x2, y2, Color
    DrawLineWu x2, y2, x1, y2, Color
    DrawLineWu x1, y2, x1, y1, Color
End Sub

Public Sub DrawBox(ByRef Coords, ByRef Color)
Attribute DrawBox.VB_Description = "Draws a box. (Outlined)."
On Error Resume Next
    If Not Initialized Then Exit Sub
    ClipRectEx Coords
    m_booDirty = True
    If Coords.Width = 0 Or Coords.Height = 0 Then Exit Sub
    AccelBoxT m_Image, m_Width, Coords, CLng(Color), GetAlpha(CLng(Color))
End Sub

Public Sub DrawBoxFast(ByRef Coords, ByRef Color)
On Error Resume Next
    If Not Initialized Then Exit Sub
    ClipRectEx Coords
    m_booDirty = True
    If Coords.Width = 0 Or Coords.Height = 0 Then Exit Sub
    AccelBox m_Image, m_Width, Coords, CLng(Color)
End Sub

Public Sub DrawCircleWu(ByVal x As Single, ByVal y As Single, ByVal XRadius As Single, ByVal YRadius As Single, ByRef Color, Optional Detail As Long = -1)
On Error Resume Next
Dim Angle As Single, cX As Single, cY As Single, lX As Single, lY As Single
Dim XRad As Single, YRad As Single, Ang As Single, Pi As Single, Radian As Single
Dim ACount As Long, AInc As Single
    
    XRad = Abs(XRadius)
    YRad = Abs(YRadius)
    m_booDirty = True
    
    If Not Initialized Then Exit Sub
    
    If Detail < 1 Then Detail = Int((XRad + YRad))
    
    lX = -32767
    lY = -32767
    
    Pi = (3.141593)
    Radian = Pi / 180
    
    Angle = 180
    AInc = 360 / Detail
    Do Until ACount > Detail
        Angle = Angle + AInc
        If Angle > 360 Then Angle = Angle - 360
        Ang = CSng(Angle) * Radian
        cX = x + (Cos(Ang) * XRad)
        cY = y + (Sin(Ang) * YRad)
        If lX = -32767 Then lX = cX: lY = cY
        DrawLineWu lX, lY, cX, cY, Color
        lX = cX
        lY = cY
        ACount = ACount + 1
    Loop
    
End Sub

Public Sub DrawLineWu(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single, ByRef Color)
On Error Resume Next
Dim m_DestX As Single, m_DestY As Single
Dim m_XIncrement As Single, m_YIncrement As Single, m_PixelCount As Long, m_Pixels As Long
Dim m_CurrentX As Single, m_CurrentY As Single, m_Color As Long
Dim DestParam As BlitParam
    
    If Not Initialized Then Exit Sub
    m_booDirty = True
    FillParam DestParam, 0, 0, m_Width, m_Height, m_Width, m_Image
    If ((x1 < 0) And (y1 < 0)) And ((x2 < 0) And (y2 < 0)) Then Exit Sub
    If ((x1 > m_Width) And (y1 < 0)) And ((x2 > m_Width) And (y2 > m_Width)) Then Exit Sub
    If ((x1 < 0) And (y1 > m_Height)) And ((x2 < 0) And (y2 > m_Height)) Then Exit Sub
    If ((x1 > m_Width) And (y1 > m_Height)) And ((x2 < 0) And (y2 > m_Height)) Then Exit Sub
    If (y1 < 0) And (y2 < 0) Then Exit Sub
    If (x1 < 0) And (x2 < 0) Then Exit Sub
    If (y1 > m_Height) And (y2 > m_Height) Then Exit Sub
    If (x1 > m_Width) And (x2 > m_Width) Then Exit Sub
    SoftFX.DrawLineWu DestParam, x1, y1, x2, y2, CLng(Color)
    
    Exit Sub
    
    m_Color = CLng(Color)
    
    m_CurrentX = x1
    m_CurrentY = y1
    
    If Abs(x2 - x1) > Abs(y2 - y1) Then
        m_PixelCount = Abs(x2 - x1)
    Else
        m_PixelCount = Abs(y2 - y1)
    End If
    
    If m_PixelCount < 1 Then m_PixelCount = 1
    
    m_XIncrement = (x2 - x1) / m_PixelCount
    m_YIncrement = (y2 - y1) / m_PixelCount
    
    For m_Pixels = 1 To m_PixelCount
        SetPixel_Wu DestParam, m_CurrentX, m_CurrentY, m_Color
        m_CurrentX = m_CurrentX + m_XIncrement
        m_CurrentY = m_CurrentY + m_YIncrement
    Next m_Pixels

End Sub

Public Sub DrawLine(ByRef Coords, ByRef Color)
Attribute DrawLine.VB_Description = "Draws a line."
On Error Resume Next
'Dim m_DestX As Long, m_DestY As Long
'Dim m_XIncrement As Long, m_YIncrement As Long, m_ErrorLevel As Long, m_PixelCount As Long
'Dim m_CurrentX As Long, m_CurrentY As Long, m_Color As Long, m_Alpha As Long
Dim m_bpDest As BlitParam
    
    If Not Initialized Then Exit Sub
    m_booDirty = True
    
    ClipRect Coords
    FillParam m_bpDest, Coords.left, Coords.top, Coords.Right, Coords.Bottom, m_Width, m_Image
    
    If GetAlpha(CLng(Color)) = 255 Then
        SoftFX.DrawLine m_bpDest, CLng(Color), 255
    Else
        SoftFX.DrawLine m_bpDest, CLng(Color), GetAlpha(CLng(Color))
    End If
'
'    'Bresenham Line Algorithm
'    'Based on implementation from "Teach yourself game programming in 21 days" by André LaMothe
'
'    m_DestX = Coords.Width
'    m_DestY = Coords.Height
'
'    m_CurrentX = Coords.Left
'    m_CurrentY = Coords.Top
'
'    If m_DestX >= 0 Then
'        m_XIncrement = 1
'    Else
'        m_XIncrement = -1
'        m_DestX = Abs(m_DestX)
'    End If
'
'    If m_DestY >= 0 Then
'        m_YIncrement = 1
'    Else
'        m_YIncrement = -1
'        m_DestY = Abs(m_DestY)
'    End If
'
'    m_Color = CLng(Color)
'    m_Alpha = GetAlpha(m_Color)
'
'    If m_DestX > m_DestY Then
'        ' m_DestX is greater
'        m_PixelCount = 0
'        Do Until m_PixelCount >= Abs(m_DestX)
'            m_PixelCount = m_PixelCount + 1
'            If Not ((m_CurrentX < 0) Or (m_CurrentY < 0) Or (m_CurrentX >= m_Width) Or (m_CurrentY >= m_Height)) Then AlphaSet m_CurrentX, m_CurrentY, m_Color, m_Alpha
'            m_ErrorLevel = m_ErrorLevel + m_DestY
'            If m_ErrorLevel > m_DestX Then 'm_ErrorLevel value overflow
'                m_ErrorLevel = m_ErrorLevel - m_DestX 'lower value
'                m_CurrentY = m_CurrentY + m_YIncrement
'            End If
'            m_CurrentX = m_CurrentX + m_XIncrement
'        Loop 'm_PixelCount <= m_DestY
'    Else
'        ' m_DestY is greater or equal
'        m_PixelCount = 0
'        Do Until m_PixelCount >= Abs(m_DestY)
'            m_PixelCount = m_PixelCount + 1
'            If Not ((m_CurrentX < 0) Or (m_CurrentY < 0) Or (m_CurrentX >= m_Width) Or (m_CurrentY >= m_Height)) Then AlphaSet m_CurrentX, m_CurrentY, m_Color, m_Alpha
'            m_ErrorLevel = m_ErrorLevel + m_DestX
'            If m_ErrorLevel > 0 Then 'm_ErrorLevel value overflow
'                m_ErrorLevel = m_ErrorLevel - m_DestY 'lower value
'                m_CurrentX = m_CurrentX + m_XIncrement
'            End If
'            m_CurrentY = m_CurrentY + m_YIncrement
'        Loop 'm_PixelCount <= m_DestY
'    End If
End Sub

Public Sub DrawLineFast(ByRef Coords, ByRef Color)
Attribute DrawLineFast.VB_Description = "Draws a line, without alpha blending."
On Error Resume Next
Dim m_bpDest As BlitParam
    
    If Not Initialized Then Exit Sub
    
    m_booDirty = True
    ClipRect Coords
    FillParam m_bpDest, Coords.left, Coords.top, Coords.Right, Coords.Bottom, m_Width, m_Image
    SoftFX.DrawLine m_bpDest, CLng(Color), 255
    
'    'Bresenham Line Algorithm
'    'Based on implementation from "Teach yourself game programming in 21 days" by André LaMothe
'
'    m_DestX = Coords.Width
'    m_DestY = Coords.Height
'
'    m_CurrentX = Coords.Left
'    m_CurrentY = Coords.Top
'
'    If m_DestX >= 0 Then
'        m_XIncrement = 1
'    Else
'        m_XIncrement = -1
'        m_DestX = Abs(m_DestX)
'    End If
'
'    If m_DestY >= 0 Then
'        m_YIncrement = 1
'    Else
'        m_YIncrement = -1
'        m_DestY = Abs(m_DestY)
'    End If
'
'    m_Color = CLng(Color)
'    m_Alpha = GetAlpha(m_Color)
'
'    If m_DestX > m_DestY Then
'        ' m_DestX is greater
'        m_PixelCount = 0
'        Do Until m_PixelCount >= Abs(m_DestX)
'            m_PixelCount = m_PixelCount + 1
'            If Not ((m_CurrentX < 0) Or (m_CurrentY < 0) Or (m_CurrentX >= m_Width) Or (m_CurrentY >= m_Height)) Then FastSet m_CurrentX, m_CurrentY, m_Color
'            m_ErrorLevel = m_ErrorLevel + m_DestY
'            If m_ErrorLevel > m_DestX Then 'm_ErrorLevel value overflow
'                m_ErrorLevel = m_ErrorLevel - m_DestX 'lower value
'                m_CurrentY = m_CurrentY + m_YIncrement
'            End If
'            m_CurrentX = m_CurrentX + m_XIncrement
'        Loop 'm_PixelCount <= m_DestY
'    Else
'        ' m_DestY is greater or equal
'        m_PixelCount = 0
'        Do Until m_PixelCount >= Abs(m_DestY)
'            m_PixelCount = m_PixelCount + 1
'            If Not ((m_CurrentX < 0) Or (m_CurrentY < 0) Or (m_CurrentX >= m_Width) Or (m_CurrentY >= m_Height)) Then FastSet m_CurrentX, m_CurrentY, m_Color
'            m_ErrorLevel = m_ErrorLevel + m_DestX
'            If m_ErrorLevel > 0 Then 'm_ErrorLevel value overflow
'                m_ErrorLevel = m_ErrorLevel - m_DestY 'lower value
'                m_CurrentX = m_CurrentX + m_XIncrement
'            End If
'            m_CurrentY = m_CurrentY + m_YIncrement
'        Loop 'm_PixelCount <= m_DestY
'    End If
End Sub

Public Sub Clear(ByRef Color)
Attribute Clear.VB_Description = "Fills the entire image with a color."
On Error Resume Next
    If Not Initialized Then Exit Sub
    m_booDirty = True
    Fill_Strip m_Image, m_Width * m_Height, CLng(Color)
    m_lngCurrentX = 0
    m_lngCurrentY = 0
End Sub

Public Sub FastFill(ByRef Area, ByRef Color)
Attribute FastFill.VB_Description = "Fills a rectangle with a color, without alpha blending."
On Error Resume Next
Dim CopyScans As Long
Dim m_bpArea As BlitParam, sColor As Long
Dim m_rctArea As Fury2Rect
    If Not Initialized Then Exit Sub
    Set m_rctArea = Area
    m_booDirty = True
    ClipRectEx m_rctArea
    If m_rctArea.Width <= 0 Or m_rctArea.Height <= 0 Then Exit Sub
    sColor = CLng(Color)
    m_bpArea = m_rctArea.GetParam(m_Width, m_Image)
    SoftFX.Fill m_bpArea, sColor
End Sub

Public Sub Fill(ByRef Area, ByRef Color)
Attribute Fill.VB_Description = "Fills a rectangle with a color."
On Error Resume Next
Dim CopyScans As Long
Dim m_bpArea As BlitParam, sColor As Long, sAlpha As Long
Dim m_rctArea As Fury2Rect
    If Not Initialized Then Exit Sub
    Set m_rctArea = Area
    m_booDirty = True
    ClipRectEx m_rctArea
    If m_rctArea.Width <= 0 Or m_rctArea.Height <= 0 Then Exit Sub
    sColor = CLng(Color)
    sAlpha = GetAlpha(sColor)
    m_bpArea = m_rctArea.GetParam(m_Width, m_Image)
'    If sAlpha = 255 Then
'        SoftFX.Fill m_bpArea, sColor
'    Else
        SoftFX.Fill_Transparent m_bpArea, sColor, sAlpha
'    End If
End Sub

Public Sub DitherFill(ByRef Area, ByRef Color1, ByRef Color2)
On Error Resume Next
Dim m_bpArea As BlitParam
Dim m_rctArea As Fury2Rect
    If Not Initialized Then Exit Sub
    Set m_rctArea = Area
    m_booDirty = True
    ClipRectEx m_rctArea
    If m_rctArea.Width <= 0 Or m_rctArea.Height <= 0 Then Exit Sub
    m_bpArea = m_rctArea.GetParam(m_Width, m_Image)
    SoftFX.Fill_Dither m_bpArea, CLng(Color1), CLng(Color2)
End Sub

Public Sub SeededStatic(ByVal Seed)
On Error Resume Next
Dim m_bpDest As BlitParam
    m_booDirty = True
    FillParam m_bpDest, 0, 0, m_Width, m_Height, m_Width, m_Image
    SoftFX.Fill_StaticGen m_bpDest, CLng(Seed)
End Sub

Public Sub StaticFill(ByRef Area)
On Error Resume Next
Dim CopyScans As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    ClipRectEx Area
    If Area.Width <= 0 Or Area.Height <= 0 Then Exit Sub
    Randomize Timer
    AccelStaticFill m_Image, m_Width, Area, CLng(Rnd * 240000000)
End Sub

Public Sub Blit(ByRef Dest, ByRef Source, ByRef SourceImage)
Attribute Blit.VB_Description = "Copies a section of one image to this image, masking if a MaskColor is set."
On Error Resume Next
Dim CopyScans As Long, sMaskColor As Long
Dim sParam As BlitParam, dParam As BlitParam
Dim m_imgSource As Fury2Image
Dim m_rctDest As Fury2Rect, m_rctSource As Fury2Rect
    If Not Initialized Then Exit Sub
    Set m_imgSource = SourceImage
    Set m_rctDest = Dest
    Set m_rctSource = Source
    If m_imgSource Is Nothing Then Exit Sub
    If (m_imgSource.Width <= 0) Or (m_imgSource.Height <= 0) Then Exit Sub
    If m_rctDest Is Nothing Then Exit Sub
    If m_rctSource Is Nothing Then Exit Sub
    If m_rctSource.Width <= 0 Or m_rctDest.Width <= 0 Or m_rctSource.Height <= 0 Or m_rctDest.Height <= 0 Then Exit Sub
    m_booDirty = True
    sMaskColor = m_imgSource.MaskColor
    ClipRects m_rctDest, m_imgSource, m_rctSource
    sParam = m_rctSource.GetParam(SourceImage.Width, SourceImage.Pointer(0, 0))
    dParam = m_rctDest.GetParam(m_Width, m_Image)
    If sMaskColor = -32767 Then
        SoftFX.Blit dParam, sParam
    Else
        SoftFX.Blit_Masked dParam, sParam, sMaskColor
    End If
End Sub

Public Function Subimage(ByRef Rectangle) As Fury2Image
On Error Resume Next
    Set Subimage = New Fury2Image
    ClipRect Rectangle
    If Rectangle.Width < 0 Or Rectangle.Height < 0 Then Exit Function
    With Subimage
        .Resize Rectangle.Width, Rectangle.Height
        .BlitFast .GetRect, Rectangle, Me
        .LoadedAlpha = Me.LoadedAlpha
        .MaskColor = Me.MaskColor
        Set .Font = Me.Font
        .ForeColor = Me.ForeColor
    End With
End Function

Public Function Duplicate() As Fury2Image
On Error Resume Next
    Set Duplicate = New Fury2Image
    Duplicate.Copy Me
    Duplicate.LoadedAlpha = Me.LoadedAlpha
    Duplicate.MaskColor = Me.MaskColor
    Set Duplicate.Font = Me.Font
    Duplicate.ForeColor = Me.ForeColor
End Function

Public Sub Copy(ByRef SourceImage)
On Error Resume Next
    If SourceImage Is Nothing Then Exit Sub
    If SourceImage.Width <> m_Width Or SourceImage.Height <> m_Height Then Resize SourceImage.Width, SourceImage.Height
    BlitFast F2Rect(0, 0, SourceImage.Width, SourceImage.Height, True), SourceImage.GetRect, SourceImage
    m_booDirty = True
    m_lngCurrentX = SourceImage.CurrentX
    m_lngCurrentY = SourceImage.CurrentY
End Sub

Public Sub HSBGradient(ByRef Area)
On Error Resume Next
Dim bp As BlitParam
    If TypeOf Area Is Fury2Rect Then Else Exit Sub
    m_booDirty = True
    ClipRectEx Area
    FillParam bp, Area.left, Area.top, Area.Right, Area.Bottom, m_Width, m_Image
    SoftFX.Fill_Gradient_HSB bp
End Sub

Public Sub BlitFast(ByRef Dest, ByRef Source, ByRef SourceImage)
Attribute BlitFast.VB_Description = "A quicker, brute-force version of Blit. (No masking)."
On Error Resume Next
Dim CopyScans As Long
Dim sParam As BlitParam, dParam As BlitParam
Dim m_imgSource As Fury2Image
Dim m_rctDest As Fury2Rect, m_rctSource As Fury2Rect
    If Not Initialized Then Exit Sub
    Set m_imgSource = SourceImage
    Set m_rctDest = Dest
    Set m_rctSource = Source
    If m_imgSource Is Nothing Then Exit Sub
    If (m_imgSource.Width <= 0) Or (m_imgSource.Height <= 0) Then Exit Sub
    If m_rctDest Is Nothing Then Exit Sub
    If m_rctSource Is Nothing Then Exit Sub
    If m_rctSource.Width <= 0 Or m_rctDest.Width <= 0 Or m_rctSource.Height <= 0 Or m_rctDest.Height <= 0 Then Exit Sub
    m_booDirty = True
    ClipRects m_rctDest, m_imgSource, m_rctSource
    sParam = m_rctSource.GetParam(SourceImage.Width, SourceImage.Pointer(0, 0))
    dParam = m_rctDest.GetParam(m_Width, m_Image)
    SoftFX.Blit dParam, sParam
End Sub

Public Sub TileBlitEx(ByRef Dest, ByRef Source, ByRef SourceImage, Optional SourceAlpha As Boolean = False, Optional ByVal Alpha As Single = 1)
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
Dim m_bpDest As BlitParam, m_bpSource As BlitParam, m_bpClip As BlitParam
Dim m_rctDest As Fury2Rect, m_imgSource As Fury2Image
Dim m_bpSourceCache As BlitParam
Dim m_lngMask As Long, m_lngAlpha As Long
Dim m_lngWidth As Long, m_lngHeight As Long
    If Not Initialized Then Exit Sub
    m_booDirty = True
    Set m_rctDest = Dest
    Set m_imgSource = SourceImage
    If m_imgSource.Initialized = False Then Exit Sub
    If m_imgSource.Width <= 0 Or m_imgSource.Height <= 0 Then Exit Sub
    m_bpClip = m_rctDest.GetParam(m_Width, 0)
    With m_bpClip
        .x1 = ClipValue(.x1, m_ClipRect.left, m_ClipRect.Right)
        .x2 = ClipValue(.x2, .x1, m_ClipRect.Right)
        .y1 = ClipValue(.y1, m_ClipRect.top, m_ClipRect.Bottom)
        .y2 = ClipValue(.y2, .y1, m_ClipRect.Bottom)
        .StartPointer = m_Image
        .Width = m_Width
    End With
    m_bpSource = m_imgSource.GetParam
'    m_lngMask = SourceImage.MaskColor
    m_lngAlpha = ClipValue(Alpha * 255, 0, 255)
'    m_lngWidth = Source.Width
'    m_lngHeight = Source.Height
'    m_bpSource = m_bpSourceCache
    If SourceAlpha Then
        Call SoftFX.TileBlit_Alpha(m_bpClip, m_bpSource, m_imgSource.Height, m_lngAlpha, m_imgSource.MaskColor)
'        For m_lngY = m_bpClip.y1 To m_bpClip.y2 Step m_lngHeight
'            For m_lngX = m_bpClip.x1 To m_bpClip.x2 Step m_lngWidth
'                FillParam m_bpDest, m_lngX, m_lngY, m_lngX + m_lngWidth, m_lngY + m_lngHeight, m_Width, m_Image
'                If ClipRectangles(m_bpDest, m_bpSource, m_bpClip) Then
'                    Call SoftFX.Blit_Alpha_Masked(m_bpDest, m_bpSource, m_lngAlpha, m_lngMask)
'                    m_bpSource = m_bpSourceCache
'                End If
'            Next m_lngX
'            m_bpSource = m_bpSourceCache
'        Next m_lngY
    Else
        SoftFX.TileBlit_Transparent m_bpClip, m_bpSource, m_imgSource.Height, m_lngAlpha, m_imgSource.MaskColor
'        For m_lngY = m_bpClip.y1 To m_bpClip.y2 Step m_lngHeight
'            For m_lngX = m_bpClip.x1 To m_bpClip.x2 Step m_lngWidth
'                FillParam m_bpDest, m_lngX, m_lngY, m_lngX + m_lngWidth, m_lngY + m_lngHeight, m_Width, m_Image
'                If ClipRectangles(m_bpDest, m_bpSource, m_bpClip) Then
'                    Call SoftFX.Blit_Transparent_Masked(m_bpDest, m_bpSource, m_lngAlpha, m_lngMask)
'                    m_bpSource = m_bpSourceCache
'                End If
'            Next m_lngX
'            m_bpSource = m_bpSourceCache
'        Next m_lngY
    End If
End Sub

Public Sub BlitEx(ByRef Dest, ByRef Source, ByRef SourceImage, Optional ByVal BlitMode As F2BlitModes = f2bSourceAlpha, Optional ByVal Alpha As Single = 1, Optional FillColor = &H0)
Attribute BlitEx.VB_Description = "Copies an area of one image to this image, applying special effects."
On Error Resume Next
Dim CopyScans As Long, sMaskColor As Long, sFillColor As Long
Dim m_imgSource As Fury2Image, m_rctSource As Fury2Rect, m_rctDest As Fury2Rect
Dim sParam As BlitParam, dParam As BlitParam
Dim rAlpha As Long
    If Not Initialized Then Exit Sub
    If SourceImage Is Nothing Then Exit Sub
    If Dest Is Nothing Then Exit Sub
    If Source Is Nothing Then Exit Sub
    Set m_imgSource = SourceImage
    Set m_rctSource = Source
    Set m_rctDest = Dest
    m_booDirty = True
    If BlitMode = f2bAlphaBlend And Alpha = 1 Then Blit Dest, Source, SourceImage: Exit Sub
    If BlitMode = f2bAlphaBlend And Alpha = 0 Then Exit Sub
    If BlitMode = f2bSourceAlphaBiScaled Or BlitMode = f2bSourceAlphaIntScaled Or BlitMode = f2bBiScaled Or BlitMode = f2bIntScaled Then
        ClipRectsScale m_rctDest, m_imgSource, m_rctSource
    Else
        ClipRects m_rctDest, m_imgSource, m_rctSource
    End If
    If m_rctSource.Width <= 0 Or m_rctDest.Width <= 0 Or m_rctSource.Height <= 0 Or m_rctDest.Height <= 0 Then Exit Sub
    sParam = m_rctSource.GetParam(m_imgSource.Width, m_imgSource.Pointer(0, 0))
    dParam = m_rctDest.GetParam(m_Width, m_Image)
    sMaskColor = SourceImage.MaskColor
    sFillColor = FillColor
    rAlpha = Abs(Alpha) * 255
    Select Case BlitMode
    Case f2bSourceAlpha
        SoftFX.Blit_Alpha_Masked dParam, sParam, rAlpha, sMaskColor
    Case f2bSourceAlphaIntScaled
        SoftFX.Blit_Alpha_IntResampled dParam, sParam, rAlpha
    Case f2bSourceAlphaBiScaled
        SoftFX.Blit_Alpha_BiResampled dParam, sParam, rAlpha
    Case f2bIntScaled
        SoftFX.Blit_Transparent_IntResampled dParam, sParam, rAlpha
    Case f2bBiScaled
        SoftFX.Blit_Transparent_BiResampled dParam, sParam, rAlpha
    Case f2bAlphaBlend
        If sMaskColor = -32767 Then
            SoftFX.Blit_Transparent dParam, sParam, rAlpha
        Else
            SoftFX.Blit_Transparent_Masked dParam, sParam, rAlpha, sMaskColor
        End If
    Case f2bDarkened
        rAlpha = Alpha * 255
        SoftFX.Blit_Adjust_Masked dParam, sParam, rAlpha, sMaskColor
    Case f2bColorFill
        rAlpha = Abs(Alpha) * GetAlpha(sFillColor)
        SoftFX.Blit_ColorFill dParam, sParam, rAlpha, sFillColor, sMaskColor
    Case f2bGammaBlend
        rAlpha = Abs(Alpha) * GetAlpha(sFillColor)
        SoftFX.Blit_Gamma dParam, sParam, sFillColor, rAlpha
    Case f2bAdditiveBlend
        If sMaskColor = -32767 Then
            SoftFX.Blit_Additive dParam, sParam, rAlpha
        Else
            SoftFX.Blit_Additive_Masked dParam, sParam, rAlpha, sMaskColor
        End If
    Case f2bSubtractiveBlend
        If sMaskColor = -32767 Then
            SoftFX.Blit_Subtractive dParam, sParam, rAlpha
        Else
            SoftFX.Blit_Subtractive_Masked dParam, sParam, rAlpha, sMaskColor
        End If
    Case f2bAND
        AccelBlitAND m_Image, SourceImage.Pointer(0, 0), m_Width, SourceImage.Width, Dest, Source
    Case f2bOR
        AccelBlitOR m_Image, SourceImage.Pointer(0, 0), m_Width, SourceImage.Width, Dest, Source
    Case f2bXOR
        AccelBlitXOR m_Image, SourceImage.Pointer(0, 0), m_Width, SourceImage.Width, Dest, Source
    Case f2bNAND
        AccelBlitNAND m_Image, SourceImage.Pointer(0, 0), m_Width, SourceImage.Width, Dest, Source
    Case f2bNOR
        AccelBlitNOR m_Image, SourceImage.Pointer(0, 0), m_Width, SourceImage.Width, Dest, Source
    Case f2bANDN
        AccelBlitANDN m_Image, SourceImage.Pointer(0, 0), m_Width, SourceImage.Width, Dest, Source
    Case f2bORN
        AccelBlitORN m_Image, SourceImage.Pointer(0, 0), m_Width, SourceImage.Width, Dest, Source
    Case f2bLightmap
        SoftFX.Blit_Lightmap dParam, sParam, rAlpha
    Case f2bGammaAdjusted
        SoftFX.Blit_AdjustGammaFast dParam, sParam, rAlpha, sMaskColor
    Case Else
    End Select
End Sub

Public Function Pointer(ByVal x As Long, ByVal y As Long) As Long
Attribute Pointer.VB_Description = "Returns a pointer to the specified pixel. If you don't know what this means, don't use it ^_^"
On Error Resume Next
    Pointer = 0
    If Not Initialized Then Exit Function
    Pointer = m_Image + (x * 4) + (y * m_Width * 4)
End Function

Public Sub GradientFill(ByRef Area, ByRef StartColor, ByRef EndColor, ByVal Direction As F2GradientDirections)
Attribute GradientFill.VB_Description = "Fills a rectangle with a vertical/horizontal gradient, with alpha blending."
On Error Resume Next
Dim sR As Long, sG As Long, sB As Long, sA As Long
Dim eR As Long, eG As Long, eB As Long, eA As Long
Dim cR As Single, cG As Single, cB As Single, cA As Single
Dim iR As Single, iG As Single, iB As Single, Ia As Single
Dim sC As Fury2Color, eC As Fury2Color, CC As Fury2Color
Dim Steps As Long, cStep As Long, NoAlpha As Boolean, cArea
Dim sWidth As Single, sHeight As Single
Dim cX As Single, cY As Single, nX As Single, nY As Single
Dim CopyScans As Long, NewArea As Fury2Rect
    If Not Initialized Then Exit Sub
    m_booDirty = True
    ClipRect Area
    If Area.Width <= 0 Or Area.Height <= 0 Then Exit Sub
    If Direction = f2gVertical Then
        AccelVerticalGradient m_Image, m_Width, Area, CLng(StartColor), CLng(EndColor)
    ElseIf Direction = f2gHorizontal Then
        Set NewArea = F2Rect(Area.left, Area.top, Area.Width, 1, False)
        If (GetAlpha(StartColor) = 255) And (GetAlpha(EndColor) = 255) Then NoAlpha = True Else NoAlpha = False
        If NoAlpha Then
            AccelHorizontalGradient m_Image, m_Width, NewArea, CLng(StartColor), CLng(EndColor)
        Else
            AccelHorizontalGradient m_Image, m_Width, Area, CLng(StartColor), CLng(EndColor)
        End If
    End If
End Sub

Public Sub GradientFillEx(ByRef Area, Color1 As Long, Color2 As Long, Color3 As Long, Color4 As Long)
Attribute GradientFillEx.VB_Description = "Fills a rectangle with an N-point gradient. In development."
On Error Resume Next
Dim Colors(0 To 3) As Long
Dim m_bpDest As BlitParam
    If Not Initialized Then Exit Sub
    m_booDirty = True
    Colors(0) = Color1
    Colors(1) = Color2
    Colors(2) = Color3
    Colors(3) = Color4
    ClipRectEx Area
    FillParam m_bpDest, Area.left, Area.top, Area.Right, Area.Bottom, m_Width, m_Image
    SoftFX.Fill_Gradient_4Corners m_bpDest, Colors(0)
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set m_Font = New StdFont
    m_Width = -1
    m_Height = -1
    m_MaskColor = -32767
End Sub

Private Sub Class_Terminate()
    Free
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As VirtualFile)
On Error Resume Next
    File.ReadSegment_Begin
    File.Load m_Width
    File.Load m_Height
    File.Load m_MaskColor
    File.Load LoadedAlpha
    If m_Width > 0 And m_Height > 0 Then
        Resize m_Width, m_Height
        File.RawLoad m_Image, CLng(m_Width) * CLng(m_Height) * 4&
    Else
        Free
    End If
    m_booDirty = True
    File.ReadSegment_End
    UpdateClipRect
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As VirtualFile)
On Error Resume Next
    File.WriteSegment_Begin
    If m_Initialized = True Then
        File.Save m_Width
        File.Save m_Height
        File.Save m_MaskColor
        File.Save LoadedAlpha
        File.RawSave m_Image, CLng(m_Width) * CLng(m_Height) * 4&
    End If
    File.WriteSegment_End
End Sub
