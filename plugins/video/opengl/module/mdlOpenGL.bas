Attribute VB_Name = "mdlOpenGL"
Option Explicit
#If 0 Then
Private m_lngBlendMode As Long
Private m_lngDrawMode As Long
Private m_lngTexture As Long
Private m_booTexturesEnabled As Boolean

Public Sub DisableGLTextures()
On Error Resume Next
    If Not m_booTexturesEnabled Then Exit Sub
    EndGLDraw
    glDisable GL_TEXTURE_2D
    m_booTexturesEnabled = False
End Sub

Public Sub EnableGLTextures()
On Error Resume Next
    If m_booTexturesEnabled Then Exit Sub
    EndGLDraw
    glEnable GL_TEXTURE_2D
    m_booTexturesEnabled = True
End Sub

Public Sub BeginGLDraw(ByVal Mode As Long)
On Error Resume Next
    If m_lngDrawMode = Mode Then Exit Sub
    EndGLDraw
    glBegin Mode
    m_lngDrawMode = Mode
End Sub

Public Sub EndGLDraw()
On Error Resume Next
    If m_lngDrawMode <> 0 Then
        glEnd
        m_lngDrawMode = 0
    End If
End Sub

Public Sub SetGLTexture(ByVal Handle As Long)
On Error Resume Next
    If m_lngTexture = Handle Then Exit Sub
    EndGLDraw
    m_lngTexture = Handle
    glBindTexture GL_TEXTURE_2D, Handle
End Sub

Public Sub SetGLFilter(ByVal Filter As Long)
On Error Resume Next
    glTexParameterf GL_TEXTURE_2D, tpnTextureMagFilter, Filter
    glTexParameterf GL_TEXTURE_2D, tpnTextureMinFilter, Filter
End Sub

Public Sub SetGLBlendMode(ByVal BlendMode As SFXBlitModes)
On Error Resume Next
    If m_lngBlendMode = BlendMode Then Exit Sub
    EndGLDraw
    Select Case BlendMode
    Case BlitMode_SourceAlpha, BlitMode_Font_SourceAlpha, BlitMode_SourceAlpha_Tint
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfSrcAlpha, dfOneMinusSrcAlpha
    Case BlitMode_Additive
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfOne, dfOne
    Case BlitMode_Additive_SourceAlpha
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfSrcAlpha, dfOne
    Case BlitMode_Font
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfOne, dfOneMinusSrcColor
    Case BlitMode_Default
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfOne, dfZero
    Case BlitMode_Normal, BlitMode_Normal_Tint, BlitMode_Matte, BlitMode_Matte_Tint
        glTexEnvi tetTextureEnv, tenTextureEnvMode, GL_BLEND
        glBlendFunc sfSrcAlpha, dfOneMinusSrcAlpha
    Case BlitMode_Multiply
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepModulate
        glBlendFunc sfZero, dfSrcColor
    Case Else
        glTexEnvi tetTextureEnv, tenTextureEnvMode, tepDecal
        glBlendFunc sfSrcAlpha, dfOneMinusSrcAlpha
    End Select
    m_lngBlendMode = BlendMode
End Sub

Public Sub SetGLFogColor(ByVal Color As Long)
On Error Resume Next
Dim l_sngColor(0 To 3) As Single
    l_sngColor(0) = GetRed(Color) / 255
    l_sngColor(1) = GetGreen(Color) / 255
    l_sngColor(2) = GetBlue(Color) / 255
    l_sngColor(3) = GetAlpha(Color) / 255
    glFogfv GL_FOG_COLOR, l_sngColor(0)
End Sub

Public Sub SetGLTexColor(ByVal Color As Long)
On Error Resume Next
Dim l_sngColor(0 To 3) As Single
    l_sngColor(0) = GetRed(Color) / 255
    l_sngColor(1) = GetGreen(Color) / 255
    l_sngColor(2) = GetBlue(Color) / 255
    l_sngColor(3) = GetAlpha(Color) / 255
    glTexEnvfv tetTextureEnv, tenTextureEnvColor, l_sngColor(0)
End Sub

Public Sub SetGLColor(ByVal Color As Long)
On Error Resume Next
    glColor4ub GetRed(Color), GetGreen(Color), GetBlue(Color), GetAlpha(Color)
End Sub

Public Sub GLDrawQuad(ByRef Rect As Rectangle)
On Error Resume Next
    DisableGLTextures
    BeginGLDraw bmQuads
        glVertex2i Rect.Left, Rect.Top
        glVertex2i Rect.Left + Rect.Width, Rect.Top
        glVertex2i Rect.Left + Rect.Width, Rect.Top + Rect.Height
        glVertex2i Rect.Left, Rect.Top + Rect.Height
End Sub

Public Sub GLDrawGradientQuad(ByRef Rect As Rectangle, ByVal Color1 As Long, ByVal Color2 As Long, ByVal Color3 As Long, ByVal Color4 As Long)
On Error Resume Next
    DisableGLTextures
    BeginGLDraw bmQuads
        SetGLColor Color1
        glVertex2i Rect.Left, Rect.Top
        SetGLColor Color2
        glVertex2i Rect.Left + Rect.Width, Rect.Top
        SetGLColor Color3
        glVertex2i Rect.Left + Rect.Width, Rect.Top + Rect.Height
        SetGLColor Color4
        glVertex2i Rect.Left, Rect.Top + Rect.Height
End Sub

Public Sub GLDrawGradientLine(ByRef Rect As Rectangle, ByVal Color1 As Long, ByVal Color2 As Long)
On Error Resume Next
    DisableGLTextures
    BeginGLDraw bmLines
        SetGLColor Color1
        glVertex2i Rect.Left, Rect.Top
        SetGLColor Color2
        glVertex2i Rect.Left + Rect.Width + IIf(Rect.Width > 0, 1, -1), Rect.Top + Rect.Height + IIf(Rect.Height > 0, 1, -1)
End Sub

Public Sub GLDrawLine(ByRef Rect As Rectangle)
On Error Resume Next
    DisableGLTextures
    BeginGLDraw bmLines
        glVertex2i Rect.Left, Rect.Top
        glVertex2i Rect.Left + Rect.Width + IIf(Rect.Width > 0, 1, -1), Rect.Top + Rect.Height + IIf(Rect.Height > 0, 1, -1)
End Sub

Public Sub GLDrawGradientLineAA(ByRef Rect As Rectangle, ByVal Color1 As Long, ByVal Color2 As Long)
On Error Resume Next
    DisableGLTextures
    glEnable GL_LINE_SMOOTH
    glHint htLineSmoothHint, hmNicest
    BeginGLDraw bmLines
        SetGLColor Color1
        glVertex2i Rect.Left, Rect.Top
        SetGLColor Color2
        glVertex2i Rect.Left + Rect.Width + IIf(Rect.Width > 0, 1, -1), Rect.Top + Rect.Height + IIf(Rect.Height > 0, 1, -1)
    EndGLDraw
    glDisable GL_LINE_SMOOTH
End Sub

Public Sub GLDrawLineAA(ByRef Rect As Rectangle)
On Error Resume Next
    DisableGLTextures
    glEnable GL_LINE_SMOOTH
    glHint htLineSmoothHint, hmNicest
    BeginGLDraw bmLines
        glVertex2i Rect.Left, Rect.Top
        glVertex2i Rect.Left + Rect.Width + IIf(Rect.Width > 0, 1, -1), Rect.Top + Rect.Height + IIf(Rect.Height > 0, 1, -1)
    EndGLDraw
    glDisable GL_LINE_SMOOTH
End Sub

Public Sub GLDrawBox(ByRef Rect As Rectangle)
On Error Resume Next
    DisableGLTextures
    BeginGLDraw bmLines
        glVertex2i Rect.Left, Rect.Top + 1
        glVertex2i Rect.Left + Rect.Width, Rect.Top + 1

        glVertex2i Rect.Left + Rect.Width - 1, Rect.Top + 1
        glVertex2i Rect.Left + Rect.Width - 1, Rect.Top + Rect.Height
        
        glVertex2i Rect.Left, Rect.Top + Rect.Height - 1
        glVertex2i Rect.Left + Rect.Width - 1, Rect.Top + Rect.Height

        glVertex2i Rect.Left, Rect.Top + Rect.Height - 1
        glVertex2i Rect.Left, Rect.Top + 1
End Sub

Public Sub GLDrawTexQuad1(ByRef Rect As Rectangle, ByVal U1 As Single, ByVal V1 As Single, ByVal U2 As Single, ByVal V2 As Single)
On Error Resume Next
    EnableGLTextures
    BeginGLDraw bmQuads
        glTexCoord2f U1, V1
        glVertex2i Rect.Left, Rect.Top
        glTexCoord2f U2, V1
        glVertex2i Rect.Left + Rect.Width, Rect.Top
        glTexCoord2f U2, V2
        glVertex2i Rect.Left + Rect.Width, Rect.Top + Rect.Height
        glTexCoord2f U1, V2
        glVertex2i Rect.Left, Rect.Top + Rect.Height
End Sub

Public Sub GLFlip(ByVal HDC As Long)
On Error Resume Next
    EndGLDraw
    glFlush
    glFinish
    SwapBuffers HDC
End Sub
#End If
