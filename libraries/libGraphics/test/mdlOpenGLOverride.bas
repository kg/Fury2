Attribute VB_Name = "mdlOpenGLOverride"
Option Explicit
Public Const RTIdentifierTag As Long = &H4F474C ' OGL
Public Const TexIdentifierTag As Long = &H4F474D ' OGL + 1
Public Const IdentifierTagIndex As Long = 7
Public Const ContextTagIndex As Long = 6
Public Const TextureTagIndex As Long = 5
Public Const DCTagIndex As Long = 4

Private m_lngCurrent As Long
Private m_lngDC As Long

Public Enum ImageTypes
    ImageType_Normal
    ImageType_Texture
    ImageType_RenderTarget
End Enum

Private Sub CopyFramebufferToImage(ByVal Image As Long)
On Error Resume Next
Dim l_lngY As Long
Dim l_lngPointer As Long
Dim l_lngWidth As Long, l_lngHeight As Long, l_lngTexHeight As Long
Dim l_rctImage As Rectangle
    l_lngPointer = GetImagePointer(Image, 0, 0)
    If l_lngPointer = 0 Then Exit Sub
    l_lngWidth = GetImageWidth(Image)
    l_lngHeight = GetImageHeight(Image)
    l_lngTexHeight = GetPowerOfTwoSize(l_lngHeight)
    For l_lngY = 0 To l_lngHeight - 1
        glReadPixels 0, l_lngHeight - l_lngY - 1, l_lngWidth, 1, GL_BGRA_EXT, GL_UNSIGNED_BYTE, ByVal GetImagePointer(Image, 0, l_lngY)
    Next l_lngY
    l_rctImage.Width = l_lngWidth
    l_rctImage.Height = l_lngHeight
End Sub

Private Sub CopyFramebufferToTexture(ByVal Image As Long)
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long, l_lngTexHeight As Long, l_lngTexWidth As Long
Dim l_lngY As Long
Dim l_rctFramebuffer As Rectangle
    ' This function is fun because the OpenGL Framebuffer is flipped vertically and there is no way to tell OpenGL to do a non-flipped copy
    ' This is one of the (many) areas where Direct3D blows OpenGL out of the water
    l_lngWidth = GetImageWidth(Image)
    l_lngHeight = GetImageHeight(Image)
    l_lngTexWidth = GetPowerOfTwoSize(l_lngWidth)
    l_lngTexHeight = GetPowerOfTwoSize(l_lngHeight)
    ' Method #0: This doesn't work, even though it should
'    glCopyTexImage2D GL_TEXTURE_2D, 0, GL_RGB8, 0, 0, l_lngWidth, l_lngHeight, 0
    ' Method #1: Copy the stupid framebuffer one stupid scanline at a time
'    For l_lngY = 0 To l_lngHeight - 1
'        glCopyTexSubImage2D GL_TEXTURE_2D, 0, 0, l_lngHeight - l_lngY - 1, 0, l_lngY, l_lngWidth, 1
'    Next l_lngY
'    Exit Sub
    ' Method #2: Copy the framebuffer, render it to itself, copy it again
    glCopyTexSubImage2D GL_TEXTURE_2D, 0, 0, 0, 0, 0, l_lngTexWidth, l_lngTexHeight
    l_rctFramebuffer.Width = l_lngWidth
    l_rctFramebuffer.Height = l_lngHeight
    GLDrawTexQuad1 l_rctFramebuffer, 0, 0, l_lngWidth / l_lngTexWidth, l_lngHeight / l_lngTexHeight
    glCopyTexSubImage2D GL_TEXTURE_2D, 0, 0, 0, 0, 0, l_lngTexWidth, l_lngTexHeight
    GLDrawTexQuad1 l_rctFramebuffer, 0, 0, l_lngWidth / l_lngTexWidth, l_lngHeight / l_lngTexHeight
End Sub

Private Sub CopyImageToFramebuffer(ByVal Image As Long)
On Error Resume Next
Dim l_lngY As Long
Dim l_lngPointer As Long
Dim l_lngWidth As Long, l_lngHeight As Long, l_lngTexHeight As Long
Dim l_rctImage As Rectangle
    l_lngPointer = GetImagePointer(Image, 0, 0)
    If l_lngPointer = 0 Then Exit Sub
    l_lngWidth = GetImageWidth(Image)
    l_lngHeight = GetImageHeight(Image)
    l_lngTexHeight = GetPowerOfTwoSize(l_lngHeight)
    SetGLBlendMode BlitMode_Default
    l_rctImage.Width = l_lngWidth
    l_rctImage.Height = l_lngHeight
'    Filter_Flip Image, l_rctImage
'    Filter_Swap_Channels Image, l_rctImage, Red, Green
'    Filter_Swap_Channels Image, l_rctImage, Blue, Alpha
    'glDrawPixels l_lngWidth, l_lngHeight, GL_BGRA_EXT, GL_UNSIGNED_BYTE, ByVal GetImagePointer(Image, 0, 0)
    For l_lngY = 0 To l_lngHeight - 1
        glRasterPos2i 0, l_lngY + 1
        glDrawPixels l_lngWidth, 1, GL_BGRA_EXT, GL_UNSIGNED_BYTE, ByVal GetImagePointer(Image, 0, l_lngY)
    Next l_lngY
End Sub

Private Sub CopyImageToTexture(ByVal Image As Long)
On Error Resume Next
Dim l_lngHandle As Long
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngTempImage As Long, l_rctBlit As Rectangle
    l_lngWidth = GetPowerOfTwoSize(GetImageWidth(Image))
    l_lngHeight = GetPowerOfTwoSize(GetImageHeight(Image))
    l_lngTempImage = AllocateImage(l_lngWidth, l_lngHeight)
    ClearImage l_lngTempImage
    l_rctBlit.Width = GetImageWidth(Image)
    l_rctBlit.Height = GetImageHeight(Image)
    SetImageClipRectangle l_lngTempImage, l_rctBlit
    Blit_Normal l_lngTempImage, Image, l_rctBlit, 0, 0
    ' ARGH I HATE YOU OPENGL
    Filter_Swap_Channels l_lngTempImage, l_rctBlit, Red, Green
    Filter_Swap_Channels l_lngTempImage, l_rctBlit, Blue, Alpha
    glTexImage2D GL_TEXTURE_2D, 0, GL_RGBA8, l_lngWidth, l_lngHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, ByVal GetImagePointer(l_lngTempImage, 0, 0) + 1
    DeallocateImage l_lngTempImage
End Sub

Public Function CreateContext(ByVal hDC As Long, ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
Dim l_imgImage As Fury2Image
    m_lngDC = hDC
    AddOverride "Allocate", AddressOf o_Allocate
    Set l_imgImage = F2Image(Width, Height)
    RemoveOverride "Allocate", AddressOf o_Allocate
    SetImageLocked l_imgImage.Handle, 1
    Set CreateContext = l_imgImage
End Function

Private Function CreateTextureFromImageH(ByVal Image As Long) As Long
On Error Resume Next
Dim l_lngHandle As Long
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngTempImage As Long, l_rctBlit As Rectangle
    Debug.Print "CreateTextureFromImage(" & Image & ")"
    l_lngWidth = GetPowerOfTwoSize(GetImageWidth(Image))
    l_lngHeight = GetPowerOfTwoSize(GetImageHeight(Image))
    glGenTextures 1, l_lngHandle
    glBindTexture GL_TEXTURE_2D, l_lngHandle
    glTexParameterf GL_TEXTURE_2D, tpnTextureWrapS, GL_CLAMP
    glTexParameterf GL_TEXTURE_2D, tpnTextureWrapT, GL_CLAMP
    glTexParameterf GL_TEXTURE_2D, tpnTextureMagFilter, GL_NEAREST
    glTexParameterf GL_TEXTURE_2D, tpnTextureMinFilter, GL_NEAREST
        
        l_lngTempImage = AllocateImage(l_lngWidth, l_lngHeight)
        ClearImage l_lngTempImage
        l_rctBlit.Width = GetImageWidth(Image)
        l_rctBlit.Height = GetImageHeight(Image)
        SetImageClipRectangle l_lngTempImage, l_rctBlit
        Blit_Normal l_lngTempImage, Image, l_rctBlit, 0, 0
        ' ARGH I HATE YOU OPENGL
        Filter_Swap_Channels l_lngTempImage, l_rctBlit, Red, Green
        Filter_Swap_Channels l_lngTempImage, l_rctBlit, Blue, Alpha
        glTexImage2D GL_TEXTURE_2D, 0, GL_RGBA8, l_lngWidth, l_lngHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, ByVal GetImagePointer(l_lngTempImage, 0, 0) + 1
        DeallocateImage l_lngTempImage
    
    SetImageDirty Image, 0
    SetImageTag Image, IdentifierTagIndex, TexIdentifierTag
    SetImageTag Image, TextureTagIndex, l_lngHandle
    CreateTextureFromImageH = l_lngHandle
End Function

Private Function CreateTextureFromRenderTargetH(ByVal Image As Long) As Long
On Error Resume Next
Dim l_lngHandle As Long
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngTempImage As Long, l_rctBlit As Rectangle
    Debug.Print "CreateTextureFromRenderTarget(" & Image & ")"
    l_lngWidth = GetPowerOfTwoSize(GetImageWidth(Image))
    l_lngHeight = GetPowerOfTwoSize(GetImageHeight(Image))
    glGenTextures 1, l_lngHandle
    glBindTexture GL_TEXTURE_2D, l_lngHandle
    glTexParameterf GL_TEXTURE_2D, tpnTextureWrapS, GL_CLAMP
    glTexParameterf GL_TEXTURE_2D, tpnTextureWrapT, GL_CLAMP
    glTexParameterf GL_TEXTURE_2D, tpnTextureMagFilter, GL_NEAREST
    glTexParameterf GL_TEXTURE_2D, tpnTextureMinFilter, GL_NEAREST
        
        glTexImage2D GL_TEXTURE_2D, 0, GL_RGB8, l_lngWidth, l_lngHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, ByVal 0
        CopyFramebufferToTexture Image
    
    SetImageDirty Image, 0
    SetImageTag Image, TextureTagIndex, l_lngHandle
    CreateTextureFromRenderTargetH = l_lngHandle
End Function

Private Sub g_Blit(ByRef Parameters As BlitSimple_Parameters)
On Error Resume Next
Dim l_rctParameter As Rectangle, l_rctCopy As Rectangle
Dim l_lngSW As Long, l_lngSH As Long
Dim l_sngU2 As Single, l_sngV2 As Single
Dim l_sngXR As Single, l_sngYR As Single
Dim l_lngSX As Long, l_lngSY As Long
Dim l_sngSX As Single, l_sngSY As Single
Dim l_sngU(0 To 3) As Single, l_sngV(0 To 3) As Single
Dim l_sngW As Single, l_sngH As Single
    DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Dest
    l_rctCopy = l_rctParameter
    l_lngSX = Parameters.SX
    l_lngSY = Parameters.SY
    SoftFX.Clip2D_SimpleRect l_rctParameter, Parameters.Dest, Parameters.Source, l_rctCopy, l_lngSX, l_lngSY
    UseImageAsTextureH Parameters.Source
    l_lngSW = GetPowerOfTwoSize(GetImageWidth(Parameters.Source))
    l_lngSH = GetPowerOfTwoSize(GetImageHeight(Parameters.Source))
    l_sngXR = 1 / l_lngSW
    l_sngYR = 1 / l_lngSH
    l_sngSX = (l_lngSX + 0.5) * l_sngXR
    l_sngSY = (l_lngSY + 0.5) * l_sngYR
    l_sngW = l_rctParameter.Width - 1
    l_sngH = l_rctParameter.Height - 1
    l_sngU2 = (l_sngSX) + (l_sngW * l_sngXR)
    l_sngV2 = (l_sngSY) + (l_sngH * l_sngYR)
    GLDrawTexQuad1 l_rctParameter, l_sngSX, l_sngSY, l_sngU2, l_sngV2
    SetImageDirty Parameters.Dest, 1
End Sub

Public Function GetImageType(ByVal Image As Long) As ImageTypes
On Error Resume Next
Dim l_lngTag As Long
    l_lngTag = GetImageTag(Image, IdentifierTagIndex)
    If l_lngTag = TexIdentifierTag Then
        GetImageType = ImageType_Texture
    ElseIf l_lngTag = RTIdentifierTag Then
        GetImageType = ImageType_RenderTarget
    Else
        GetImageType = ImageType_Normal
    End If
End Function

Public Function GetPowerOfTwoSize(ByVal Size As Long) As Long
On Error Resume Next
Dim Log10 As Double, Log102 As Double
Dim Log2Value As Double
    Log10 = Log(10)
    Log102 = Log(2) / Log10
    Log2Value = (Log(Size) / Log10) / Log102
    GetPowerOfTwoSize = Size
    If Floor(Log2Value) <> Ceil(Log2Value) Then
        GetPowerOfTwoSize = 2 ^ Ceil(Log2Value)
    End If
End Function

Public Sub InitOpenGLOverride()
On Error Resume Next
    AddOverride "Deallocate", AddressOf o_Deallocate
    AddOverride "Clear", AddressOf o_Clear
    AddOverride "FilterSimple_Fill", AddressOf o_Fill
    AddOverride "FilterSimple_Fill_SourceAlpha", AddressOf o_Fill
    AddOverride "FilterSimple_Fill_Additive", AddressOf o_Fill
    AddOverride "FilterSimple_Box", AddressOf o_Box
    AddOverride "FilterSimple_Box_SourceAlpha", AddressOf o_Box
    AddOverride "Lock", AddressOf o_Lock
    AddOverride "Unlock", AddressOf o_Unlock
    AddOverride "BlitSimple_Normal", AddressOf o_Blit_Normal
    AddOverride "BlitSimple_Normal_Opacity", AddressOf o_Blit_Normal
    AddOverride "BlitSimple_SourceAlpha", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitSimple_SourceAlpha_Opacity", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitSimple_SourceAlphaMatte", AddressOf o_Blit_SourceAlphaMatte
    AddOverride "BlitSimple_SourceAlphaMatte_Opacity", AddressOf o_Blit_SourceAlphaMatte
    AddOverride "BlitSimple_Additive", AddressOf o_Blit_Additive
    AddOverride "BlitSimple_Additive_Opacity", AddressOf o_Blit_Additive
'    AddOverride "FilterSimple_Line", AddressOf o_Line
'    AddOverride "FilterSimple_Line_SourceAlpha", AddressOf o_Line
'    AddOverride "Fill", AddressOf o_Clear
'    AddOverride "BlitResample_Normal", AddressOf o_Blit_Normal
'    AddOverride "BlitResample_Normal_Opacity", AddressOf o_Blit_Normal
'    AddOverride "BlitSimple_Normal_Tint", AddressOf o_Blit_Normal_Tint
'    AddOverride "BlitSimple_Normal_Tint_Opacity", AddressOf o_Blit_Normal_Tint
'    AddOverride "BlitResample_SourceAlpha", AddressOf o_Blit_SourceAlpha
'    AddOverride "BlitResample_SourceAlpha_Opacity", AddressOf o_Blit_SourceAlpha
'    AddOverride "BlitSimple_SourceAlpha_Tint", AddressOf o_Blit_SourceAlpha_Tint
'    AddOverride "BlitSimple_SourceAlpha_Solid_Tint", AddressOf o_Blit_SourceAlpha_Tint
'    AddOverride "BlitSimple_SourceAlpha_Tint_Opacity", AddressOf o_Blit_SourceAlpha_Tint
'    AddOverride "BlitSimple_SourceAlphaMatte", AddressOf o_Blit_SourceAlpha
'    AddOverride "BlitSimple_SourceAlphaMatte_Opacity", AddressOf o_Blit_SourceAlpha
'    AddOverride "BlitSimple_Additive", AddressOf o_Blit_Additive
'    AddOverride "BlitSimple_Additive_Opacity", AddressOf o_Blit_Additive
'    AddOverride "BlitSimple_Subtractive", AddressOf o_Blit_Subtractive
'    AddOverride "BlitSimple_Subtractive_Opacity", AddressOf o_Blit_Subtractive
'    AddOverride "BlitSimple_Unerase", AddressOf o_Blit_Unerase
'    AddOverride "BlitSimple_Unerase_Opacity", AddressOf o_Blit_Unerase
'    AddOverride "BlitSimple_Erase", AddressOf o_Blit_Erase
'    AddOverride "BlitSimple_Erase_Opacity", AddressOf o_Blit_Erase
'    AddOverride "BlitSimple_Lightmap", AddressOf o_Blit_Lightmap
'    AddOverride "BlitSimple_Lightmap_Opacity", AddressOf o_Blit_Lightmap
'    AddOverride "BlitSimple_Lightmap_RGB", AddressOf o_Blit_Lightmap
'    AddOverride "BlitSimple_Lightmap_RGB_Opacity", AddressOf o_Blit_Lightmap
'    AddOverride "BlitSimple_Multiply", AddressOf o_Blit_Multiply
'    AddOverride "BlitSimple_Multiply_Opacity", AddressOf o_Blit_Multiply
''    AddOverride "BlitSimple_Font", AddressOf o_Blit_Font
''    AddOverride "BlitSimple_Font_Opacity", AddressOf o_Blit_Font
'    AddOverride "BlitSimple_Font_SourceAlpha", AddressOf o_Blit_Font_SourceAlpha
'    AddOverride "BlitSimple_Font_SourceAlpha_Opacity", AddressOf o_Blit_Font_SourceAlpha
'    AddOverride "BlitSimple_Font_SourceAlpha_RGB", AddressOf o_Blit_Font_SourceAlpha
'    AddOverride "BlitSimple_Font_SourceAlpha_RGB_Opacity", AddressOf o_Blit_Font_SourceAlpha
''    AddOverride "BlitSimple_Merge", AddressOf o_Blit
''    AddOverride "BlitSimple_Merge_Opacity", AddressOf o_Blit
''    AddOverride "BlitResample_Normal", AddressOf o_ResampleBlit
''    AddOverride "BlitResample_Normal_Opacity", AddressOf o_ResampleBlit_Opacity
'    AddOverride "FilterSimple_Adjust", AddressOf o_Adjust
''    AddOverride "FilterSimple_Invert_Color", AddressOf o_Invert
''    AddOverride "FilterSimple_Invert_Channel", AddressOf o_Invert
''    AddOverride "FilterSimple_Fill_Channel", AddressOf o_Fill_Channel
''    AddOverride "SetPixel(image, x, y, value)", AddressOf o_SetPixel
'    AddOverride "FilterSimple_ConvexPolygon", AddressOf o_ConvexPolygon
'    AddOverride "FilterSimple_ConvexPolygon_Textured", AddressOf o_ConvexPolygon_Textured
'    AddOverride "FilterSimple_Gradient_Radial", AddressOf o_Gradient_Radial
End Sub

Private Function o_Allocate(ByRef Parameters As Image_Allocate_Parameters) As Long
On Error Resume Next
Dim l_lngDC As Long, l_lngContext As Long
Dim l_pfdFormat As PIXELFORMATDESCRIPTOR
Dim l_lngFormat As Long
    ' Allocate overrides always consume the operation
    o_Allocate = 0
    If (Parameters.Width < 1) Then Exit Function
    If (Parameters.Height < 1) Then Exit Function
    If (Parameters.Image = 0) Then Exit Function
    l_lngDC = m_lngDC
    
    l_pfdFormat.nSize = Len(l_pfdFormat)
    l_pfdFormat.nVersion = 1
    l_pfdFormat.dwFlags = PFD_DRAW_TO_WINDOW Or PFD_SUPPORT_OPENGL Or PFD_DOUBLEBUFFER
    l_pfdFormat.cColorBits = 32
    l_pfdFormat.iPixelType = PFD_TYPE_RGBA
    l_pfdFormat.iLayerType = PFD_MAIN_PLANE
    
    l_lngFormat = ChoosePixelFormat(l_lngDC, l_pfdFormat)
    SetPixelFormat l_lngDC, l_lngFormat, l_pfdFormat
    l_lngContext = wglCreateContext(l_lngDC)
    wglMakeCurrent l_lngDC, l_lngContext
    SetImageTag Parameters.Image, DCTagIndex, l_lngDC
    SetImageTag Parameters.Image, IdentifierTagIndex, RTIdentifierTag
    SetImageTag Parameters.Image, ContextTagIndex, l_lngContext
    SetImageWidth Parameters.Image, Parameters.Width
    SetImageHeight Parameters.Image, Parameters.Height
    SetImagePitch Parameters.Image, 0
    SetImagePointer Parameters.Image, 0
    SetImageLocked Parameters.Image, True
    Debug.Print "CreateContext(" & Parameters.Image & ")"
End Function

Public Function o_Blit_Additive(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
    If GetImageLocked(Parameters.Dest) = 0 Then Exit Function
    If GetImageType(Parameters.Dest) = ImageType_RenderTarget Then
        UseImageAsContextH Parameters.Dest
        o_Blit_Additive = 1
        SetGLBlendMode BlitMode_Additive
        With Parameters.Header
            Static l_lngOpacityIndex As Long, l_lngResampleIndex As Long, l_lngResampleOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Additive_Opacity")
            End If
            If l_lngResampleIndex = 0 Then
                l_lngResampleIndex = GetOverrideIndex("BlitResample_Additive")
            End If
            If l_lngResampleOpacityIndex = 0 Then
                l_lngResampleOpacityIndex = GetOverrideIndex("BlitResample_Additive_Opacity")
            End If
            If (.Index = l_lngOpacityIndex) Or (.Index = l_lngResampleOpacityIndex) Then
                glColor4ub 0, 0, 0, Parameters.P6
            Else
                glColor4ub 0, 0, 0, 255
            End If
        End With
        If (Parameters.Header.Index = l_lngResampleIndex) Or (Parameters.Header.Index = l_lngResampleOpacityIndex) Then
'            g_Blit_Resample l_devDevice, Parameters, l_lngTintColor, False
        Else
            g_Blit Parameters
        End If
    End If
End Function

Public Function o_Blit_Normal(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
    If GetImageLocked(Parameters.Dest) = 0 Then Exit Function
    If GetImageType(Parameters.Dest) = ImageType_RenderTarget Then
        UseImageAsContextH Parameters.Dest
        o_Blit_Normal = 1
        SetGLBlendMode BlitMode_Normal
        With Parameters.Header
            Static l_lngOpacityIndex As Long, l_lngResampleIndex As Long, l_lngResampleOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Normal_Opacity")
            End If
            If l_lngResampleIndex = 0 Then
                l_lngResampleIndex = GetOverrideIndex("BlitResample_Normal")
            End If
            If l_lngResampleOpacityIndex = 0 Then
                l_lngResampleOpacityIndex = GetOverrideIndex("BlitResample_Normal_Opacity")
            End If
            If (.Index = l_lngOpacityIndex) Or (.Index = l_lngResampleOpacityIndex) Then
                glColor4ub 0, 0, 0, Parameters.P6
            Else
                glColor4ub 0, 0, 0, 255
            End If
        End With
        If (Parameters.Header.Index = l_lngResampleIndex) Or (Parameters.Header.Index = l_lngResampleOpacityIndex) Then
'            g_Blit_Resample l_devDevice, Parameters, l_lngTintColor, False
        Else
            g_Blit Parameters
        End If
    End If
End Function

Public Function o_Blit_SourceAlpha(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
    If GetImageLocked(Parameters.Dest) = 0 Then Exit Function
    If GetImageType(Parameters.Dest) = ImageType_RenderTarget Then
        UseImageAsContextH Parameters.Dest
        o_Blit_SourceAlpha = 1
        SetGLBlendMode BlitMode_SourceAlpha
        With Parameters.Header
            Static l_lngOpacityIndex As Long, l_lngResampleIndex As Long, l_lngResampleOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_SourceAlpha_Opacity")
            End If
            If l_lngResampleIndex = 0 Then
                l_lngResampleIndex = GetOverrideIndex("BlitResample_SourceAlpha")
            End If
            If l_lngResampleOpacityIndex = 0 Then
                l_lngResampleOpacityIndex = GetOverrideIndex("BlitResample_SourceAlpha_Opacity")
            End If
            If (.Index = l_lngOpacityIndex) Or (.Index = l_lngResampleOpacityIndex) Then
                glColor4ub 255, 255, 255, Parameters.P6
            Else
                glColor4ub 255, 255, 255, 255
            End If
        End With
        If (Parameters.Header.Index = l_lngResampleIndex) Or (Parameters.Header.Index = l_lngResampleOpacityIndex) Then
'            g_Blit_Resample l_devDevice, Parameters, l_lngTintColor, False
        Else
            g_Blit Parameters
        End If
    End If
End Function

Public Function o_Blit_SourceAlphaMatte(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
    If GetImageLocked(Parameters.Dest) = 0 Then Exit Function
    If GetImageType(Parameters.Dest) = ImageType_RenderTarget Then
        UseImageAsContextH Parameters.Dest
        o_Blit_SourceAlphaMatte = 1
        SetGLBlendMode BlitMode_SourceAlpha
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_SourceAlphaMatte_Opacity")
            End If
            If (.Index = l_lngOpacityIndex) Then
                glColor4ub 255, 255, 255, Parameters.P6
            Else
                glColor4ub 255, 255, 255, 255
            End If
        End With
        g_Blit Parameters
    End If
End Function

Private Function o_Box(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_rctArea As Rectangle
    If GetImageLocked(Parameters.Image) = 0 Then Exit Function
    If GetImageType(Parameters.Image) = ImageType_RenderTarget Then
        o_Box = 1
        UseImageAsContextH Parameters.Image
        DerefRectangle Parameters.Rectangle, l_rctArea, Parameters.Image
        With Parameters.Header
            Static l_lngSourceAlphaIndex As Long, l_lngAdditiveIndex As Long
            If l_lngSourceAlphaIndex = 0 Then
                l_lngSourceAlphaIndex = GetOverrideIndex("FilterSimple_Box_SourceAlpha")
            End If
            If l_lngAdditiveIndex = 0 Then
                l_lngAdditiveIndex = GetOverrideIndex("FilterSimple_Box_Additive")
            End If
            If (.Index = l_lngSourceAlphaIndex) Then
                SetGLBlendMode BlitMode_SourceAlpha
            ElseIf (.Index = l_lngAdditiveIndex) Then
                ' Alpha hack
                Parameters.P3 = SetAlpha(Parameters.P3, 255)
                SetGLBlendMode BlitMode_Additive
            Else
                SetGLBlendMode BlitMode_Default
            End If
        End With
        SetGLColor Parameters.P3
        GLDrawBox l_rctArea
        SetImageDirty Parameters.Image, 1
    End If
End Function

Private Function o_Clear(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
    If GetImageLocked(Parameters.Image) = 0 Then Exit Function
    If GetImageType(Parameters.Image) = ImageType_RenderTarget Then
        o_Clear = 1
        UseImageAsContextH Parameters.Image
        glClearColor 0, 0, 0, 0
        glClear clrColorBufferBit
        SetImageDirty Parameters.Image, 1
    End If
End Function

Private Function o_Deallocate(ByRef Parameters As Image_Allocate_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngContext As Long
    o_Deallocate = 0
    If (Parameters.Image = 0) Then Exit Function
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = RTIdentifierTag Then
        l_lngContext = GetImageTag(Parameters.Image, ContextTagIndex)
        wglDeleteContext l_lngContext
        o_Deallocate = 1
        Debug.Print "DestroyContext(" & Parameters.Image & ")"
    End If
End Function

Private Function o_Fill(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_rctArea As Rectangle
    If GetImageLocked(Parameters.Image) = 0 Then Exit Function
    If GetImageType(Parameters.Image) = ImageType_RenderTarget Then
        o_Fill = 1
        UseImageAsContextH Parameters.Image
        DerefRectangle Parameters.Rectangle, l_rctArea, Parameters.Image
        With Parameters.Header
            Static l_lngSourceAlphaIndex As Long, l_lngAdditiveIndex As Long
            If l_lngSourceAlphaIndex = 0 Then
                l_lngSourceAlphaIndex = GetOverrideIndex("FilterSimple_Fill_SourceAlpha")
            End If
            If l_lngAdditiveIndex = 0 Then
                l_lngAdditiveIndex = GetOverrideIndex("FilterSimple_Fill_Additive")
            End If
            If (.Index = l_lngSourceAlphaIndex) Then
                SetGLBlendMode BlitMode_SourceAlpha
            ElseIf (.Index = l_lngAdditiveIndex) Then
                ' Alpha hack
                Parameters.P3 = SetAlpha(Parameters.P3, 255)
                SetGLBlendMode BlitMode_Additive
            Else
                SetGLBlendMode BlitMode_Default
            End If
        End With
        SetGLColor Parameters.P3
        GLDrawQuad l_rctArea
        SetImageDirty Parameters.Image, 1
    End If
End Function

Private Function o_Lock(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If (l_lngTag = RTIdentifierTag) Then
        o_Lock = 1
        If GetImageDirty(Parameters.Image) <> 0 Then
            CopyImageToFramebuffer Parameters.Image
        End If
        SetImageLocked Parameters.Image, 1
        SetImageDirty Parameters.Image, 0
    End If
End Function

Private Function o_Unlock(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If (l_lngTag = RTIdentifierTag) Then
        o_Unlock = 1
        SetImageDirty Parameters.Image, 0
        SetImageLocked Parameters.Image, 0
        CopyFramebufferToImage Parameters.Image
    End If
End Function

Public Sub UseImageAsContext(ByRef Image As Fury2Image)
On Error Resume Next
    UseImageAsContextH Image.Handle
End Sub

Private Sub UseImageAsContextH(ByVal Image As Long)
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngContext As Long
Dim l_lngDC As Long
    l_lngTag = GetImageTag(Image, IdentifierTagIndex)
    If (l_lngTag = RTIdentifierTag) Then
        l_lngDC = GetImageTag(Image, DCTagIndex)
        l_lngContext = GetImageTag(Image, ContextTagIndex)
        If m_lngCurrent <> l_lngContext Then
            wglMakeCurrent l_lngDC, l_lngContext
        End If
        m_lngCurrent = l_lngContext
    End If
End Sub

Public Sub UseImageAsTexture(ByRef Image As Fury2Image)
On Error Resume Next
    UseImageAsTextureH Image.Handle
End Sub

Private Sub UseImageAsTextureH(ByVal Image As Long)
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngTex As Long
    l_lngTag = GetImageTag(Image, IdentifierTagIndex)
    If (l_lngTag = TexIdentifierTag) Or (l_lngTag = RTIdentifierTag) Then
        l_lngTex = GetImageTag(Image, TextureTagIndex)
        If (l_lngTag = RTIdentifierTag) Then
            If l_lngTex = 0 Then
                l_lngTex = CreateTextureFromRenderTargetH(Image)
            ElseIf GetImageDirty(Image) <> 0 Then
                glBindTexture GL_TEXTURE_2D, l_lngTex
                CopyFramebufferToTexture Image
                SetImageDirty Image, 0
            End If
        ElseIf GetImageDirty(Image) <> 0 Then
            glBindTexture GL_TEXTURE_2D, l_lngTex
            CopyImageToTexture Image
            SetImageDirty Image, 0
        Else
            glBindTexture GL_TEXTURE_2D, l_lngTex
        End If
    Else
        ' No texture, so allocate one
        l_lngTex = CreateTextureFromImageH(Image)
        glBindTexture GL_TEXTURE_2D, l_lngTex
    End If
End Sub

