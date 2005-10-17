VERSION 5.00
Begin VB.Form frmTest 
   Caption         =   "OpenGL"
   ClientHeight    =   4500
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   6000
   Icon            =   "frmTest.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   300
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   400
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdToggleDeform 
      Caption         =   "Toggle Deform"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   3570
      TabIndex        =   1
      Top             =   4005
      Width           =   1215
   End
   Begin VB.CommandButton cmdToggleHW 
      Caption         =   "Toggle GL"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   4785
      TabIndex        =   0
      Top             =   4005
      Width           =   1215
   End
   Begin VB.Timer tmrRedraw 
      Interval        =   25
      Left            =   75
      Top             =   90
   End
End
Attribute VB_Name = "frmTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private m_lngOldBitmap As Long
Private m_filFilter As Fury2ConvolutionFilter
Dim m_imgFramebuffer As Fury2Image
Dim m_imgMask As Fury2Image
Dim m_imgPattern As Fury2Image
Dim m_imgTexture() As Fury2Image
Dim m_imgTextureBlend As Fury2Image
Dim m_imgBuffer As Fury2Image
Dim m_imgCopy As Fury2Image
Dim m_lngTexture As Long
Dim m_lngX As Long, m_lngY As Long
Dim m_varPoly() As Variant
Public UseHardware As Boolean
Public EnableDeform As Boolean
Const c_dblPi As Double = 3.14159265358979
Const c_dblRadian As Double = 1.74532925199433E-02

Private Function RotatePoint(X As Single, Y As Single, Angle As Single) As Variant
On Error Resume Next
Dim l_sngX As Single, l_sngY As Single
Dim l_sngTheta As Single, l_sngR As Single
    l_sngX = CSng(X)
    l_sngY = CSng(Y)
    l_sngR = Sqr((l_sngX * l_sngX) + (l_sngY * l_sngY))
    l_sngTheta = Atn(l_sngY / l_sngX)
    If l_sngX < 0 Then
        l_sngTheta = l_sngTheta + c_dblPi
    End If
    l_sngTheta = l_sngTheta + CSng(Angle * c_dblRadian)
    RotatePoint = Array(l_sngR * Cos(l_sngTheta), l_sngR * Sin(l_sngTheta))
End Function

Public Sub Redraw()
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngX As Long, l_lngY As Long
Dim l_sngX As Single, l_sngY As Single
Static l_sngS As Single, l_sngR As Single
Static l_lngFrame As Long
Dim l_mshMesh As Fury2DeformationMesh
    'F2LockingMode = LockingMode_AutoUnlock_AutoLock
''    Set l_devDevice = GetImageDevice(m_imgBuffer)
'    With m_imgBuffer
'        Set .ClipRectangle = .Rectangle
'        .Clear F2RGB(63, 63, 63, 255)
'        .ClippedSetClipRectangle F2Rect(50, 50, 200, 150, False)
'        .Fill .Rectangle, F2Black
'        .ClippedSetClipRectangle F2Rect(0, 0, 240, 150, False)
'        .Fill .Rectangle, F2RGB(255, 0, 0, 255)
'        .ClippedSetClipRectangle F2Rect(-50, 40, 100, 100, False)
'        .Fill .Rectangle, F2RGB(0, 255, 0, 255)
'        If PointInPolygon(m_varPoly, Array(m_lngX, m_lngY)) Then
'            .ConvexPolygon m_varPoly, F2RGB(0, 255, 0, 255)
'            .Fill F2Rect(m_lngX - 2, m_lngY - 2, 5, 5, False), F2RGB(0, 0, 255, 255)
'        Else
'            .ConvexPolygon m_varPoly, F2RGB(255, 0, 0, 255)
'            .Fill F2Rect(m_lngX - 2, m_lngY - 2, 5, 5, False), F2RGB(0, 0, 255, 255)
'        End If
'        .Locked = True
''        mdlDX8.DrawDeviceToWindow l_devDevice, Me.hWnd
'    End With
'    Set l_devDevice = Nothing
'    m_imgBuffer.Blit , , m_imgBuffer
'    m_imgBuffer.Blit F2Rect(0, 0, m_imgBuffer.Width, m_imgBuffer.Height, False), , m_imgBuffer, , BlitMode_Normal
'    m_imgBuffer.GradientFill F2Rect(100, 100, 100, 100, False), Array(F2Transparent, F2Black, F2White, F2White), RenderMode_SourceAlpha
'    m_imgCopy.Draw m_imgBuffer, m_imgBuffer.Width / 2, m_imgBuffer.Height / 2, , Rnd, 0, , , ResampleMode_Bilinear
'    m_imgBuffer.Blit , , m_imgTexture, , BlitMode_SourceAlpha_Tint, F2RGB(127, 127, 127, Rnd * 255) 'F2RGB(Rnd * 255, Rnd * 255, Rnd * 255, Rnd * 255)
'    m_imgBuffer.Fill m_imgBuffer.Rectangle, F2RGB(0, 0, 127, 255)
'    m_imgTexture.FillChannel m_imgTexture.Rectangle, Alpha, 31
'    m_imgTexture.Draw m_imgBuffer, Me.ScaleWidth / 2, Me.ScaleHeight / 2, 1, l_sngS, l_sngR, BlitMode_Additive, , ResampleMode_Bilinear
'    glClearColor 0.33, 0.33, 0.33, 0
'    glClear clrColorBufferBit
''    m_imgBuffer.ConvexPolygon Array(Array(10, 10), Array(50, 10), Array(50, 50), Array(25, 75), Array(10, 50)), F2White
'    m_imgBuffer.Adjust -16
'    m_imgBuffer.Clear F2RGB(32, 48, 64, 0)
'    m_imgTexture.Draw m_imgBuffer, m_imgBuffer.Width / 2, m_imgBuffer.Height / 2, 1, 1, l_sngR, BlitMode_Additive, , ResampleMode_Bilinear
'    m_imgBuffer.Blit f2rect(50, 50, 100, 100, False), , m_imgBuffer
'    m_imgBuffer.Blit , , m_imgTexture, 1, BlitMode_SourceAlpha_Tint, F2RGB(0, 255, 0, 255)
'    m_imgBuffer.Fill F2Rect(10, 10, 50, 50, False), F2RGB(0, 31, 0, 127)
'    m_imgBuffer.Fill F2Rect(60, 10, 50, 50, False), F2RGB(0, 31, 0, 127), RenderMode_SourceAlpha
'    m_imgBuffer.Fill F2Rect(110, 10, 50, 50, False), F2RGB(0, 31, 0, 127), RenderMode_Additive
'    m_imgBuffer.Fill F2Rect(160, 10, 50, 50, False), F2RGB(0, 31, 0, 127), RenderMode_Subtractive
'    m_imgBuffer.Locked = False
'        m_imgBuffer.RadialGradientFill F2Rect(100, 100, 200, 200, False), Array(F2Black, F2White), RenderMode_SourceAlpha
'    m_imgBuffer.Locked = True
'    m_imgBuffer.Box m_imgBuffer.Rectangle, F2White
'    m_imgBuffer.Box m_imgBuffer.Rectangle.Adjust(-1, -1), F2Black
'    m_imgBuffer.Locked = False
'    m_imgBuffer.Adjust -32
'    m_imgBuffer.Locked = True
'    m_imgBuffer.GradientFill F2Rect(10, 60, 50, 50, False), Array(F2White, F2White, F2Black, F2Black)
'    m_imgBuffer.GradientFill F2Rect(60, 60, 50, 50, False), Array(F2White, F2Black, F2White, F2Black)
'    m_imgBuffer.GradientFill F2Rect(110, 60, 50, 50, False), Array(F2White, F2Black, F2Black, F2White)
'    m_imgBuffer.[Line] Array(10, 110, 20, 160), F2White
'    m_imgBuffer.GradientLine Array(60, 110, 70, 160), F2White, F2Black
'    m_imgBuffer.Blit F2Rect(50, 50, m_imgBuffer.Width, m_imgBuffer.Height, False), , m_imgBuffer, 0.5, BlitMode_Normal
'    m_imgBuffer.Draw m_imgBuffer, m_imgBuffer.Width / 2, m_imgBuffer.Height / 2, 1, 0.5, , , , ResampleMode_Bilinear
    'm_imgBuffer.Blit F2Rect(50, 50, m_imgBuffer.Width, m_imgBuffer.Height, False), , m_imgBuffer, , BlitMode_Normal
    'm_imgBuffer.AdjustRGB 0, 64, -64
'    Set m_filFilter = F2EmbossFilter(1)
    Set m_filFilter = F2BoxFilter(3)
'    m_filFilter.Offset = 0.5
'    m_filFilter.SetAll 0
'    m_filFilter.Weight(1, 1) = 1
'    m_filFilter.Divisor = 1
    m_imgTextureBlend.Blit , , m_imgTexture(l_lngFrame), 1
    m_imgTextureBlend.Blit , , m_imgTexture(WrapValue(l_lngFrame + 1, 0, UBound(m_imgTexture))), l_sngS
    m_imgMask.Clear F2RGB(127, 127, 127, 255)
'    m_imgMask.GradientFill m_imgMask.Rectangle, Array(F2RGB(0, 255, 0, 255), F2RGB(0, 0, 255, 255), "V"), RenderMode_Normal
'    m_imgMask.GradientFill m_imgMask.Rectangle, Array(F2Black, F2Black, F2White, F2White), RenderMode_Normal
'    m_imgMask.GradientFill m_imgMask.Rectangle, Array(F2Black, F2RGB(127, 127, 127, 255), F2RGB(127, 127, 127, 255), F2White), RenderMode_Normal
'    m_imgMask.AntiAliasEllipse Array(100, 100), F2RGB(0, 0, 0, 255), l_sngR * 45, l_sngR * 45, RenderMode_SourceAlpha, 0.25, 6, 1
'    m_imgMask.[Line] Array(5, 5, 100, 5), F2White
'    m_imgMask.AntiAliasFilledEllipse Array(100, 100), F2RGB(0, 0, 0, 255), l_sngR * 55, l_sngR * 55, RenderMode_SourceAlpha, 0.25
'    m_imgMask.AntiAliasEllipse Array(100, 40), F2RGB(255, 255, 255, 255), 65, 65, RenderMode_Merge, 0.25, 6, 1
'    m_imgMask.AntiAliasEllipse Array(40, 100), F2RGB(255, 255, 255, 255), 65, 65, RenderMode_Merge, 0.25, 6, 1
    m_imgMask.GradientFilledArc Array(100, 100), F2RGB(0, 0, 127, 255), F2RGB(63, 63, 255, 255), 40, 40 + (87 - 73), l_sngR, l_sngR + 93, , 7
'    m_imgMask.Composite
    'SoftFX.Primitive_Arc_Filled_Gradient m_imgMask.Handle, 100, 100, 40, 70, l_sngR, l_sngR - 90, F2White, F2Black, 0, 10
'    m_imgMask.CopyChannel m_imgMask, 3, 0
'    m_imgMask.AntiAliasFilledEllipse Array(100, 100), F2RGB(255, 255, 255, 255), 65, 65, RenderMode_SourceAlpha, 0.25
'    m_imgMask.AntiAliasFilledEllipse Array(100, 100), F2RGB(0, 0, 0, 255), 75, 75, RenderMode_SourceAlpha, 0.25
'    m_imgMask.Box m_imgMask.Rectangle, F2White
'    m_imgMask.RadialGradientFill F2Rect(100 - (l_sngR * 50), 100 - (l_sngR * 50), 100 + (l_sngR * 50), 100 + (l_sngR * 50), True), Array(F2RGB(255, 0, 0, 255), F2RGB(255, 0, 0, 0)), RenderMode_SourceAlpha
'    m_imgMask.RadialGradientFill F2Rect(100 - (l_sngR * 33), 100 - (l_sngR * 33), 100 + (l_sngR * 33), 100 + (l_sngR * 33), True), Array(F2RGB(0, 0, 0, 255), F2RGB(0, 0, 0, 0)), RenderMode_SourceAlpha
    Set l_mshMesh = New Fury2DeformationMesh
    l_mshMesh.Resize ClipValue(CLng(Rnd * 32), 6, 32), ClipValue(CLng(Rnd * 32), 6, 32)
'    l_mshMesh.SetAll 16, 0
    l_mshMesh.ApplyHeightmap m_imgMask, , 32
'    l_mshMesh.XValue(0, 0) = 100
'    l_mshMesh.YValue(0, 0) = 100
'    l_mshMesh.XValue(99, 9) = 100
'    l_mshMesh.YValue(99, 9) = 100
'    MeshFromHeightmap l_mshMesh.GetParam, m_imgMask.Handle, 3, m_imgMask.Rectangle.GetRectangle
'    For l_lngY = 0 To l_mshMesh.Height - 1
'        For l_lngX = 0 To l_mshMesh.Width - 1
'            If EnableDeform Then
'                l_mshMesh.Value(l_lngX, l_lngY) = Array(Sin((l_lngY / 2) + l_sngR) * 2.5, Sin((l_lngX / 2) - l_sngR) * 7.5)
'            End If
'        Next l_lngX
'    Next l_lngY
    'm_imgPattern.DeformBlit m_imgPattern.Rectangle, m_imgPattern.Rectangle, m_imgTextureBlend, l_mshMesh, RenderMode_Normal, ResampleMode_Bilinear_Wrap
'    m_imgPattern.Box m_imgPattern.Rectangle, F2White
'    m_imgBuffer.DeformBlit m_imgPattern.Rectangle, m_imgPattern.Rectangle, m_imgTextureBlend, l_mshMesh, RenderMode_Normal, ResampleMode_Bilinear_Wrap
'    m_imgBuffer.MaskDeformBlit m_imgPattern.Rectangle, m_imgPattern.Rectangle, m_imgMask.Rectangle, m_imgTextureBlend, m_imgMask, l_mshMesh, 0.66, RenderMode_SourceAlpha, ResampleMode_Bilinear_Wrap
'    m_imgFramebuffer.Clear F2RGB(64, 32, 32, 255)
'    m_imgFramebuffer.Blit , , m_imgMask
    m_imgBuffer.Locked = UseHardware
    If EnableDeform Then
'        m_imgBuffer.DeformBlit m_imgPattern.Rectangle, m_imgPattern.Rectangle, m_imgPattern, l_mshMesh, RenderMode_Normal, ResampleMode_Bilinear
'        m_imgBuffer.MaskDeformBlit m_imgPattern.Rectangle, m_imgPattern.Rectangle, m_imgMask.Rectangle, m_imgPattern, m_imgMask, l_mshMesh, 1#, RenderMode_SourceAlpha, ResampleMode_Bilinear
        'm_imgBuffer.Stroke Array(Array(25, 25), Array(75, 75)), F2White, 3, 1, False, RenderMode_Normal
'        m_filFilter.MaskFilter m_imgBuffer, m_imgPattern, m_imgMask, F2Rect(0, 0, 200, 200, False), m_imgMask.Rectangle, , RenderMode_SourceAlpha
    End If
'    m_imgFramebuffer.Clear F2RGB(127, 0, 0, 255)
'    m_imgFramebuffer.Blit , , m_imgMask, 0.5, BlitMode_Normal
'    m_imgBuffer.Blit m_imgMask.Rectangle.Translate(0, 0), m_imgMask.Rectangle, m_imgFramebuffer, , BlitMode_Normal
'    m_imgBuffer.Blit m_imgMask.Rectangle.Translate(200, 0), m_imgMask.Rectangle, m_imgMask, , BlitMode_SourceAlpha
    'm_imgBuffer.FillChannel m_imgBuffer.Rectangle, 0, 127
'    m_imgBuffer.FillChannel m_imgBuffer.Rectangle, 0, 0
'    m_imgBuffer.FillChannel m_imgBuffer.Rectangle, 1, 0
'    m_imgBuffer.FillChannel m_imgBuffer.Rectangle, 2, 0
'    m_imgBuffer.FillChannel m_imgBuffer.Rectangle, 3, 0
'    m_imgBuffer.CopyChannel m_imgBuffer, 0, 2
'    m_imgBuffer.CopyChannel m_imgBuffer, 1, 2
'    m_imgBuffer.InvertChannel 0
'    m_imgTexture(0).Draw m_imgBuffer, 75, 75, , 2, l_sngR, BlitMode_SourceAlpha, , ResampleMode_Bilinear, True
'    m_imgBuffer.Stroke Array(Array(25, 25), Array(75, 75)), F2White, 3, , , RenderMode_SourceAlpha
'    m_imgBuffer.AntiAliasFilledEllipse Array(150, 150), F2White, 50, 50, RenderMode_SourceAlpha
    'm_imgBuffer.GradientConvexPolygon Array(Array(75, 75, F2White), Array(75 + (Sin(l_sngR) * l_sngS), 75 + (-Cos(l_sngR) * l_sngS), F2Black), Array(75 + (Sin(l_sngR + (c_dblPi / 2)) * l_sngS), 75 + (-Cos(l_sngR + (c_dblPi / 2)) * l_sngS), F2Black)), RenderMode_Additive
'    m_imgBuffer.AdjustChannelGamma 1, l_sngS
'    m_imgBuffer.SwapChannels 0, 2
    m_imgBuffer.Adjust -32
    m_imgMask.Draw m_imgBuffer, 200, 150, 1, 1, l_sngR, BlitMode_Additive, , ResampleMode_Linear
    m_imgBuffer.Locked = True
    GLFlip ' Me.HDC
    l_sngS = l_sngS + 0.025
    If (l_sngS >= 2) Then
        l_sngS = 0
        l_lngFrame = WrapValue(l_lngFrame + 1, 0, UBound(m_imgTexture))
    End If
    l_sngR = l_sngR + 2
    If l_sngR > 360 Then l_sngR = 0
End Sub

Private Sub cmdToggleDeform_Click()
On Error Resume Next
    EnableDeform = Not EnableDeform
End Sub

Private Sub cmdToggleHW_Click()
On Error Resume Next
    UseHardware = Not UseHardware
End Sub

Private Sub Form_Load()
On Error Resume Next
    UseHardware = True
    Randomize Timer
    F2Init
    GLInit Me.HWND, Me.HDC
    GLSetShaderLoadCallback AddressOf ShaderLoadCallback
    GLSetOutputSize Me.ScaleWidth, Me.ScaleHeight
    GLInstallAllocateHook
    Set m_imgBuffer = F2Image(400, 300)
    GLUninstallAllocateHook
    SetImageLocked m_imgBuffer.Handle, 1
    GLInstallFBAllocateHook
    Set m_imgFramebuffer = F2Image(256, 256)
    GLUninstallFBAllocateHook
    SetImageLocked m_imgFramebuffer.Handle, 1
'    InitOpenGLOverride
'    Set m_imgBuffer = CreateContext(Me.HDC, Me.ScaleWidth, Me.ScaleHeight)
'    UseImageAsContext m_imgBuffer
'    glEnable glcScissorTest
'    glScissor 0, 0, Me.ScaleWidth, Me.ScaleHeight
'    glShadeModel smSmooth
'    glEnable glcBlend
'    glBlendFunc sfSrcAlpha, dfOneMinusSrcAlpha
''    glBlendFunc sfOne, dfZero
'    glEnable glcPointSmooth
'    glMatrixMode mmProjection
'    glLoadIdentity
'    gluOrtho2D 0, Me.ScaleWidth, Me.ScaleHeight, 0
'    glMatrixMode mmModelView
'    glLoadIdentity
'    Set m_imgRenderTarget = mdlDX8Override.CreateDX8RenderTarget(128, 128, GetImageDevice(m_imgBuffer))
'    Set m_imgRenderTarget = F2Image(150, 150)
'    Set m_imgTexture = F2LoadImage("J:\chia\set00.png")
    m_imgTexture() = F2LoadImage("J:\water.png").Split(32, 32)
    Set m_imgTextureBlend = F2Image(32, 32)
    Set m_imgPattern = F2Image(200, 200)
    m_imgPattern.Blit , , F2LoadImage("J:\test.jpg")
    Set m_imgMask = F2Image(200, 200)
'    m_imgMask.RadialGradientFill m_imgMask.Rectangle, Array(F2RGB(255, 0, 0, 255), F2RGB(127, 0, 0, 255)), RenderMode_Normal
'    m_imgMask.AntiAliasFilledEllipse Array(100, 100), F2RGB(255, 0, 0, 255), 50, 50, RenderMode_SourceAlpha
'    m_imgMask.AntiAliasFilledEllipse Array(100, 100), F2RGB(0, 0, 0, 255), 25, 25, RenderMode_SourceAlpha
'    m_imgBuffer.Clear F2Black
'    m_lngTexture = CreateTextureFromImage(m_imgTexture.Handle)
'    m_imgInFrontTexture.Box m_imgInFrontTexture.Rectangle, F2RGB(255, 255, 255, 0)
'    m_imgInFrontTexture.[Line] Array(0, 0, 63, 0), F2RGB(255, 0, 0, 0)
'    m_imgInFrontTexture.[Line] Array(0, 0, 0, 63), F2RGB(0, 0, 255, 0)
'    InitGDIOverride
'    Set m_imgGDISurface = CreateGDIImageFromFile("J:\test.bmp")
'    Debug.Print "IsGDI: " & IsImageGDI(m_imgGDISurface)
'    Debug.Print "Size: " & m_imgGDISurface.Width & "x" & m_imgGDISurface.Height
'    m_imgBuffer.Clear F2RGB(255, 255, 255, 255)
'    m_imgBuffer.Fill F2Rect(1, 1, m_imgBuffer.Width - 2, m_imgBuffer.Height - 2, False), F2Black
''    m_imgBuffer.Fill F2Rect(50, 50, 150, 100, False), F2RGB(0, 255, 0, 255)
''    m_imgBuffer.Blit , , m_imgTexture, Rnd, BlitMode_Additive
''    m_imgBuffer.Fill F2Rect(25, 25, 100, 100, False), F2RGB(255, 0, 0, 127), RenderMode_Additive
'    m_imgBuffer.Locked = False
'    m_imgBuffer.Blit F2Rect(10, 10, 100, 50, False), , m_imgTexture, 1, BlitMode_SourceAlpha
'    m_imgBuffer.Locked = True
'    m_imgBuffer.GradientFill m_imgBuffer.Rectangle, Array(F2Black, F2Black, F2White, F2White)
'    m_imgBuffer.Box F2Rect(10, 10, 100, 50, False), F2RGB(255, 255, 255, 127), RenderMode_SourceAlpha
'    m_imgBuffer.Fill F2Rect(10, 10, 100, 50, False), F2RGB(0, 220, 0, 127), RenderMode_SourceAlpha
'    Set m_imgCopy = m_imgBuffer.Duplicate
''    m_imgCopy.SavePNG "C:\test.png"
'    m_imgCopy.Locked = False
    m_imgBuffer.Clear F2White
    Redraw
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_lngX = X
    m_lngY = Y
End Sub

Private Sub Form_Resize()
On Error Resume Next
    GLSetOutputSize Me.ScaleWidth, Me.ScaleHeight
    GLInstallAllocateHook
    m_imgBuffer.Unsize
    m_imgBuffer.Resize 400, 300
    GLUninstallAllocateHook
    SetImageLocked m_imgBuffer.Handle, 1
'    If m_imgBuffer.Width <> (Me.ScaleWidth \ 2) Or m_imgBuffer.Height <> (Me.ScaleHeight \ 2) Then
'        Set m_imgBuffer = Nothing
'        GLInstallAllocateHook
'        Set m_imgBuffer = F2Image(Me.ScaleWidth \ 2, Me.ScaleHeight \ 2)
'        GLUninstallAllocateHook
'        SetImageLocked m_imgBuffer.Handle, 1
'    End If
    Redraw
End Sub

Private Sub Form_Unload(Cancel As Integer)
    GLShutdown
    F2Shutdown
End Sub

Private Sub tmrRedraw_Timer()
    Redraw
End Sub
