Attribute VB_Name = "mdlOpenGL"
Option Explicit

Public Sub SetGLBlendMode(ByVal BlendMode As SFXBlitModes)
On Error Resume Next
    Select Case BlendMode
    Case BlitMode_Matte
    Case BlitMode_SourceAlpha
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfSrcAlpha, dfOneMinusSrcAlpha
    Case BlitMode_Additive
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepDecal
        glBlendFunc sfSrcAlpha, dfOne
    Case BlitMode_Subtractive
    Case BlitMode_AND
    Case BlitMode_OR
    Case BlitMode_XOR
    Case BlitMode_Lightmap
    Case BlitMode_Lightmap_RGB
    Case BlitMode_Matte_Tint
    Case BlitMode_SourceAlpha_Tint
    Case BlitMode_Font
    Case BlitMode_Font_SourceAlpha
    Case BlitMode_Dither
    Case BlitMode_Screen
    Case BlitMode_Multiply
    Case BlitMode_Merge
    Case BlitMode_Unerase
    Case BlitMode_Erase
    Case BlitMode_Font_Merge
    Case BlitMode_Behind
    Case BlitMode_Dodge
    Case BlitMode_Burn
    Case BlitMode_Normal_Tint
    Case BlitMode_Additive_SourceAlpha
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfSrcAlpha, dfOne
    Case BlitMode_Subtractive_SourceAlpha
    Case BlitMode_SourceAlpha_ColorMask
    Case BlitMode_Default
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfOne, dfZero
    Case Else
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepDecal
        glBlendFunc sfSrcAlpha, dfOneMinusSrcAlpha
    End Select
End Sub

Public Sub SetGLTexColor(ByVal Color As Long)
On Error Resume Next
'Dim l_sngColors(0 To 3) As Single
'    l_sngColors(0) = R / 255
'    l_sngColors(1) = G / 255
'    l_sngColors(2) = B / 255
'    l_sngColors(3) = A / 255
    glTexEnviv tetTextureEnv, tenTextureEnvColor, Color
End Sub

Public Sub SetGLColor(ByVal Color As Long)
On Error Resume Next
    glColor4ub GetRed(Color), GetGreen(Color), GetBlue(Color), GetAlpha(Color)
End Sub

Public Sub GLDrawQuad(ByRef Rect As Rectangle)
On Error Resume Next
    glBegin bmQuads
        glVertex2i Rect.Left, Rect.Top
        glVertex2i Rect.Left + Rect.Width, Rect.Top
        glVertex2i Rect.Left + Rect.Width, Rect.Top + Rect.Height
        glVertex2i Rect.Left, Rect.Top + Rect.Height
    glEnd
End Sub

Public Sub GLDrawBox(ByRef Rect As Rectangle)
On Error Resume Next
    glBegin bmLines
        glVertex2i Rect.Left, Rect.Top + 1
        glVertex2i Rect.Left + Rect.Width, Rect.Top + 1

        glVertex2i Rect.Left + Rect.Width - 1, Rect.Top + 1
        glVertex2i Rect.Left + Rect.Width - 1, Rect.Top + Rect.Height
        
        glVertex2i Rect.Left, Rect.Top + Rect.Height - 1
        glVertex2i Rect.Left + Rect.Width - 1, Rect.Top + Rect.Height

        glVertex2i Rect.Left, Rect.Top + Rect.Height - 1
        glVertex2i Rect.Left, Rect.Top + 1
    glEnd
End Sub

Public Sub GLDrawTexQuad1(ByRef Rect As Rectangle, ByVal U1 As Single, ByVal V1 As Single, ByVal U2 As Single, ByVal V2 As Single)
On Error Resume Next
    glEnable glcTexture2D
    glBegin bmQuads
        glTexCoord2f U1, V1
        glVertex2i Rect.Left, Rect.Top
        glTexCoord2f U2, V1
        glVertex2i Rect.Left + Rect.Width, Rect.Top
        glTexCoord2f U2, V2
        glVertex2i Rect.Left + Rect.Width, Rect.Top + Rect.Height
        glTexCoord2f U1, V2
        glVertex2i Rect.Left, Rect.Top + Rect.Height
    glEnd
    glDisable glcTexture2D
End Sub

