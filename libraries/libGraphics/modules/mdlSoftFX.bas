Attribute VB_Name = "mdlSoftFX"
'
'   ::fury2 software fx library::
'

Public Type Pixel32
    Blue As Byte
    Green As Byte
    Red As Byte
    Alpha As Byte
End Type

Public Declare Function SFXRenderParticlesAddEx Lib "f2softfx" Alias "_f2geRenderParticlesAddEx@20" (ByVal ViewportX As Long, ByVal ViewportY As Long, ByRef FirstParticle As Fury2Particle, ByVal ParticleCount As Long, ByRef ClipArea As BlitParam) As Long
Public Declare Function SFXRenderParticlesEx Lib "f2softfx" Alias "_f2geRenderParticlesEx@20" (ByVal ViewportX As Long, ByVal ViewportY As Long, ByRef FirstParticle As Fury2Particle, ByVal ParticleCount As Long, ByRef ClipArea As BlitParam) As Long
Public Declare Function SFXRenderParticlesAdd Lib "f2softfx" Alias "_f2geRenderParticlesAdd@12" (ByRef FirstParticle As Fury2Particle, ByVal ParticleCount As Long, ByRef ClipArea As BlitParam) As Long
Public Declare Function SFXRenderParticles Lib "f2softfx" Alias "_f2geRenderParticles@12" (ByRef FirstParticle As Fury2Particle, ByVal ParticleCount As Long, ByRef ClipArea As BlitParam) As Long
Public Declare Function SFXUpdateParticles Lib "f2softfx" Alias "_f2geUpdateParticles@12" (ByRef FirstParticle As Fury2Particle, ByVal ParticleCount As Long, ByVal FadeSpeed As Long) As Long

#If noaccel = 1 Then
#Else

Sub AccelVerticalGradient(DestArrayPtr As Long, DestWidth As Long, DestRect, TopColor As Long, BottomColor As Long)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Fill_Gradient_Vertical DestParam, TopColor, BottomColor
End Sub

Sub AccelHorizontalGradient(DestArrayPtr As Long, DestWidth As Long, DestRect, LeftColor As Long, RightColor As Long)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Fill_Gradient_Horizontal DestParam, LeftColor, RightColor
End Sub

Sub AccelFlip(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, DestHeight As Long)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, 0, 0, DestWidth, DestHeight, DestWidth, DestArrayPtr
    FillParam SourceParam, 0, 0, DestWidth, DestHeight, DestWidth, SourceArrayPtr
    Blit_Flip DestParam, SourceParam
End Sub

Sub AccelRotate90(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, DestHeight As Long)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, 0, 0, DestWidth, DestHeight, DestWidth, DestArrayPtr
    FillParam SourceParam, 0, 0, DestWidth, DestHeight, DestWidth, SourceArrayPtr
    Blit_Rotate90 DestParam, SourceParam
End Sub

Sub AccelMirror(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, DestHeight As Long)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, 0, 0, DestWidth, DestHeight, DestWidth, DestArrayPtr
    FillParam SourceParam, 0, 0, DestWidth, DestHeight, DestWidth, SourceArrayPtr
    Blit_Mirror DestParam, SourceParam
End Sub

Sub AccelBiBlur3x3(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Bilinear_3x3 DestParam, SourceParam
End Sub

Sub AccelBiBlur5x5(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Bilinear_5x5 DestParam, SourceParam
End Sub

Sub AccelTriBlur3x3(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Trilinear_3x3 DestParam, SourceParam
End Sub

Sub AccelTriBlur5x5(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Trilinear_5x5 DestParam, SourceParam
End Sub

Sub AccelBiBlur3x3W(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Bilinear_Wrap3x3 DestParam, SourceParam
End Sub

Sub AccelBiBlur5x5W(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Bilinear_Wrap5x5 DestParam, SourceParam
End Sub

Sub AccelTriBlur3x3W(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Trilinear_Wrap3x3 DestParam, SourceParam
End Sub

Sub AccelTriBlur5x5W(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
'On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Filter_Blur_Trilinear_Wrap5x5 DestParam, SourceParam
End Sub

Sub AccelBlit(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Masked DestParam, SourceParam, MaskColor
End Sub

Sub AccelBlitCF(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional FillColor As Long = &HFFFFFFFF, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_ColorFill DestParam, SourceParam, Alpha, FillColor, MaskColor
End Sub

Sub AccelBlitFast(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit DestParam, SourceParam
End Sub

Sub AccelBlitOR(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_OR DestParam, SourceParam
End Sub

Sub AccelBlitNOR(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_NOT_OR DestParam, SourceParam
End Sub

Sub AccelBlitORN(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_OR_NOT DestParam, SourceParam
End Sub

Sub AccelBlitAND(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_AND DestParam, SourceParam
End Sub

Sub AccelBlitNAND(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_NOT_AND DestParam, SourceParam
End Sub

Sub AccelBlitANDN(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_AND_NOT DestParam, SourceParam
End Sub

Sub AccelBlitXOR(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_XOR DestParam, SourceParam
End Sub

Sub AccelInvert(DestArrayPtr As Long, DestWidth As Long, DestRect, Optional WithAlpha As Boolean = True)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    If WithAlpha Then
        Filter_Invert DestParam
    Else
        Filter_Invert_RGBOnly DestParam
    End If
End Sub

Sub AccelClipAlpha(DestArrayPtr As Long, DestWidth As Long, DestRect)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_ClipAlpha DestParam
End Sub

Sub AccelCopyChannel(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, DestChannel As Long, SourceChannel As Long)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    BlitChannel DestParam, SourceParam, DestChannel, SourceChannel
End Sub

Sub AccelFadeChannel(DestArrayPtr As Long, DestWidth As Long, DestRect, DestChannel As Long, Amount As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_AdjustChannel DestParam, DestChannel, Amount
End Sub

Sub AccelFillChannel(DestArrayPtr As Long, DestWidth As Long, DestRect, DestChannel As Long, Value As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillChannel DestParam, DestChannel, Value
End Sub

Sub AccelSolarizeChannel(DestArrayPtr As Long, DestWidth As Long, DestRect, DestChannel As Long, Level As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillChannel DestParam, DestChannel, Level
End Sub

Sub AccelInvertChannel(DestArrayPtr As Long, DestWidth As Long, DestRect, DestChannel As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_InvertChannel DestParam, DestChannel
End Sub

Sub AccelStaticFillChannel(DestArrayPtr As Long, DestWidth As Long, DestRect, DestChannel As Long, Seed As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillChannel_Static DestParam, DestChannel, Seed
End Sub

Sub AccelFade(DestArrayPtr As Long, DestWidth As Long, DestRect, Amount As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_Adjust DestParam, Amount
End Sub

Sub AccelReplaceColor(DestArrayPtr As Long, DestWidth As Long, DestRect, OldColor As Long, NewColor As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_ReplaceColor DestParam, OldColor, NewColor
End Sub

Sub AccelSolarize(DestArrayPtr As Long, DestWidth As Long, DestRect, Level As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_Solarize DestParam, Level
End Sub

Sub AccelHueShift(DestArrayPtr As Long, DestWidth As Long, DestRect, Amount As Single)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_HueShift DestParam, CLng(Amount)
End Sub

Sub AccelFadeRGB(DestArrayPtr As Long, DestWidth As Long, DestRect, Red As Long, Green As Long, Blue As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_AdjustRGB DestParam, Red, Green, Blue
End Sub

Sub AccelGammaRGB(DestArrayPtr As Long, DestWidth As Long, DestRect, Red As Long, Green As Long, Blue As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Filter_GammaRGB DestParam, Red, Green, Blue
End Sub

Sub AccelFilter(DestArrayPtr As Long, DestWidth As Long, DestRect, Filter As F2Filter)
On Error Resume Next
Dim DestParam As BlitParam
Dim DestFilter As FilterParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    CopyMemory DestFilter, Filter, Len(Filter)
    SoftFX.FilterEx DestParam, DestFilter
End Sub

Sub AccelGrayscale(DestArrayPtr As Long, DestWidth As Long, DestRect)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Filter_Grayscale DestParam
End Sub

Sub AccelDecay(DestArrayPtr As Long, DestWidth As Long, DestRect)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Filter_Decay DestParam
End Sub

Sub AccelFill(DestArrayPtr As Long, DestWidth As Long, DestRect, Color As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    SoftFX.Fill DestParam, Color
End Sub

Sub AccelBox(DestArrayPtr As Long, DestWidth As Long, DestRect, Color As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Box DestParam, Color
End Sub

Sub AccelBoxT(DestArrayPtr As Long, DestWidth As Long, DestRect, Color As Long, Alpha As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Box_Transparent DestParam, Color, Alpha
End Sub

Sub AccelStaticFill(DestArrayPtr As Long, DestWidth As Long, DestRect, Seed As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Fill_Static DestParam, Seed
End Sub

Sub AccelFillT(DestArrayPtr As Long, DestWidth As Long, DestRect, Color As Long, Alpha As Long)
On Error Resume Next
Dim DestParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    Fill_Transparent DestParam, Color, Alpha
End Sub

Sub AccelBlitT(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    If MaskColor = -32767 Then
        Blit_Transparent DestParam, SourceParam, Alpha
    Else
        Blit_Transparent_Masked DestParam, SourceParam, Alpha, MaskColor
    End If
End Sub

Sub AccelBlitBRSA(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Alpha_BiResampled DestParam, SourceParam, Alpha
End Sub

Sub AccelBlitIRSA(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Alpha_IntResampled DestParam, SourceParam, Alpha
End Sub

Sub AccelBlitBR(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Transparent_BiResampled DestParam, SourceParam, Alpha
End Sub

Sub AccelBlitIR(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Transparent_IntResampled DestParam, SourceParam, Alpha
End Sub

Sub AccelBlitSA(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Alpha_Masked DestParam, SourceParam, Alpha, MaskColor
End Sub

Sub AccelBlitA(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    If MaskColor = -32767 Then
        SoftFX.Blit_Additive DestParam, SourceParam, Alpha
    Else
        SoftFX.Blit_Additive_Masked DestParam, SourceParam, Alpha, MaskColor
    End If
End Sub

Sub AccelBlitLightmap(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    SoftFX.Blit_Lightmap DestParam, SourceParam, Alpha
End Sub

Sub AccelBlitGamma(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional FillColor As Long = &HFFFFFF)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Gamma DestParam, SourceParam, FillColor, Alpha
End Sub

Sub AccelBlitGammaAdjust(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_AdjustGammaFast DestParam, SourceParam, Alpha, MaskColor
End Sub

Sub AccelBlitG(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional FillColor As Long = &HFFFFFF)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Gamma DestParam, SourceParam, FillColor, Alpha
End Sub

Sub AccelBlitGEx(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional FillColor As Long = &HFFFFFF)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Gamma_RGB DestParam, SourceParam, FillColor, Alpha
End Sub

Sub AccelBlitS(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    If MaskColor = -32767 Then
        SoftFX.Blit_Subtractive DestParam, SourceParam, Alpha
    Else
        SoftFX.Blit_Subtractive_Masked DestParam, SourceParam, Alpha, MaskColor
    End If
End Sub

Sub AccelBlitF(DestArrayPtr As Long, SourceArrayPtr As Long, DestWidth As Long, SourceWidth As Long, DestRect, SourceRect, Optional Alpha As Long = 255, Optional MaskColor As Long = -32767)
On Error Resume Next
Dim DestParam As BlitParam, SourceParam As BlitParam
    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestWidth, DestArrayPtr
    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceWidth, SourceArrayPtr
    Blit_Adjust_Masked DestParam, SourceParam, Alpha, MaskColor
End Sub
#End If
