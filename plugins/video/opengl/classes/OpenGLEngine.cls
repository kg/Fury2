VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "OpenGLEngine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Default Output Engine (GDI)"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Attribute VB_Ext_KEY = "Member0" ,"GFXSurface"
Option Explicit
Implements Fury2VideoDriver
Private Declare Function SetWindowPos Lib "user32" (ByVal HWND As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Private Declare Function ShowCursor Lib "user32" (ByVal bShow As Long) As Long
Private Const SWP_NOMOVE As Long = 2
Private Const SWP_NOSIZE As Long = 1
Private Const HWND_TOPMOST As Long = -1
Private Const HWND_NOTOPMOST As Long = -2
Private WithEvents m_OutForm As Form
Attribute m_OutForm.VB_VarHelpID = -1
Private m_booClosed As Boolean
Private m_DX7 As DirectX7
Private m_DD7 As DirectDraw7
Private m_booError As Boolean
Private m_booEnabled As Boolean
Private m_booFullscreen As Boolean
Private m_objHooked As Fury2VideoEventSink
Private m_lngWidth As Long, m_lngHeight As Long, m_lngBPP As Long
Private m_sngScaleRatio As Single
Private m_imgBackbuffer As Fury2Image
Private m_lngScaleMode As Long

Public Property Get ShaderPath() As String
    ShaderPath = mdlGLFX.ShaderPath
End Property

Public Property Let ShaderPath(ByVal NewValue As String)
    mdlGLFX.ShaderPath = NewValue
End Property

Private Property Let Fury2VideoDriver_ScaleMode(ByVal RHS As Long)
    m_lngScaleMode = RHS
    GLSetScaleMode RHS
End Property

Private Property Get Fury2VideoDriver_ScaleMode() As Long
    Fury2VideoDriver_ScaleMode = m_lngScaleMode
End Property

Public Property Get Feature(ByRef Name As String) As Boolean
On Error Resume Next
    Feature = GLGetFeatureSupport(Name) = 1
End Property

Private Sub SetFormSize(ByRef Frm As Form, x As Long, y As Long, Optional ByVal Center As Boolean = False)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    If Center Then
        Frm.Move (Screen.Width - (CLng(x * Screen.TwipsPerPixelX) + (BW * 2))) \ 2, (Screen.Height - (CLng(y * Screen.TwipsPerPixelY) + TH + (BW * 2))) \ 2, CLng(x * Screen.TwipsPerPixelX) + (BW * 2), CLng(y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    Else
        Frm.Move Frm.Left, Frm.Top, CLng(x * Screen.TwipsPerPixelX) + (BW * 2), CLng(y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    End If
    Frm.ScaleMode = OldMode
End Sub

Private Property Get Fury2VideoDriver_Backbuffer() As libGraphics.Fury2Image
    Set Fury2VideoDriver_Backbuffer = m_imgBackbuffer
End Property

Public Function Fury2VideoDriver_CreateImage(ByVal Width As Long, ByVal Height As Long) As libGraphics.Fury2Image
On Error Resume Next
    GLInstallFBAllocateHook
    Set Fury2VideoDriver_CreateImage = F2Image(Width, Height)
    GLUninstallFBAllocateHook
    If Fury2VideoDriver_CreateImage.NumericTags(7) <> 0 Then
        SetImageLocked Fury2VideoDriver_CreateImage.Handle, 1
    End If
End Function

Friend Sub InitBackbuffer()
On Error Resume Next
    If m_booError Then Exit Sub
    If Not m_booEnabled Then Exit Sub
    GLSetOutputSize m_OutForm.ScaleWidth, m_OutForm.ScaleHeight
    GLInstallAllocateHook
    If m_imgBackbuffer Is Nothing Then
        Set m_imgBackbuffer = F2Image(m_lngWidth, m_lngHeight)
    Else
        m_imgBackbuffer.Unsize
        m_imgBackbuffer.Resize m_lngWidth, m_lngHeight
    End If
    GLUninstallAllocateHook
    SetImageLocked m_imgBackbuffer.Handle, 1
    m_imgBackbuffer.Clear
'    Set m_imgBackbuffer = CreateContext(m_OutForm.HDC, m_lngWidth, m_lngHeight)
End Sub

Friend Sub DestroyBackbuffer()
On Error Resume Next
    Set m_imgBackbuffer = Nothing
End Sub

Private Sub Fury2VideoDriver_HookEvents(ByVal Obj As libVideo.Fury2VideoEventSink)
    Set m_objHooked = Obj
End Sub

Private Property Let Fury2VideoDriver_ScaleRatio(ByVal RHS As Single)
    m_sngScaleRatio = RHS
End Property

Private Property Get Fury2VideoDriver_ScaleRatio() As Single
    Fury2VideoDriver_ScaleRatio = m_sngScaleRatio
End Property

Public Sub Fury2VideoDriver_SetCaption(ByRef Text As String)
On Error Resume Next
    If m_booFullscreen Then
        If m_OutForm.Caption <> "" Then
            m_OutForm.Caption = ""
            SetWindowText m_OutForm.HWND, Text
        End If
    Else
        If m_OutForm.Caption <> Text Then m_OutForm.Caption = Text
    End If
End Sub

Friend Sub SetFullscreen(ByVal RHS As Boolean)
On Error Resume Next
    If RHS = m_booFullscreen Then
    Else
        m_booFullscreen = RHS
        If RHS Then
            m_OutForm.Caption = ""
            m_DD7.SetCooperativeLevel m_OutForm.HWND, DDSCL_FULLSCREEN Or DDSCL_EXCLUSIVE
            Err.Clear
            m_DD7.SetDisplayMode m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, 32, 0, DDSDM_DEFAULT
            If Err.Number = DDERR_INVALIDMODE Or Err.Number = DDERR_UNSUPPORTEDMODE Then
                Err.Clear
                m_DD7.SetDisplayMode m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, 24, 0, DDSDM_DEFAULT
                If Err.Number = DDERR_INVALIDMODE Or Err.Number = DDERR_UNSUPPORTEDMODE Then
                    Err.Clear
                    m_DD7.SetDisplayMode m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, 16, 0, DDSDM_DEFAULT
                    If Err.Number = DDERR_INVALIDMODE Or Err.Number = DDERR_UNSUPPORTEDMODE Then
                        Err.Clear
                        m_booFullscreen = False
                        m_DD7.RestoreDisplayMode
                        m_DD7.SetCooperativeLevel m_OutForm.HWND, DDSCL_NORMAL
                        Exit Sub
                    End If
                End If
            End If
            SetWindowPos m_OutForm.HWND, HWND_TOPMOST, -1, -1, m_lngWidth * m_sngScaleRatio + 2, m_lngHeight * m_sngScaleRatio + 2, 0
            Reallocate
        Else
            SetWindowPos m_OutForm.HWND, HWND_NOTOPMOST, 0, 0, m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, 0
            m_DD7.RestoreDisplayMode
            m_DD7.SetCooperativeLevel m_OutForm.HWND, DDSCL_NORMAL
            SetFormSize m_OutForm, m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, True
            Reallocate
        End If
    End If
End Sub

Public Property Get Fury2VideoDriver_Fullscreen() As Boolean
    Fury2VideoDriver_Fullscreen = m_booFullscreen
End Property

Public Property Get Fury2VideoDriver_Window() As Object
    Set Fury2VideoDriver_Window = m_OutForm
End Property

Public Property Get Fury2VideoDriver_Closed() As Boolean
    Fury2VideoDriver_Closed = m_booClosed
End Property

Public Sub Fury2VideoDriver_Shutdown()
On Error Resume Next
    m_booClosed = True
    Unload m_OutForm
End Sub

Public Sub Fury2VideoDriver_ResizeWindow(ByVal XSize As Long, ByVal YSize As Long)
On Error Resume Next
    SetFormSize m_OutForm, XSize * m_sngScaleRatio, YSize * m_sngScaleRatio, True
    Reallocate
End Sub

Public Sub Fury2VideoDriver_Init(ByVal XRes As Long, ByVal YRes As Long, ByRef Caption As String, Optional ByVal Windowed As Boolean = False)
On Error Resume Next
    If F2Initialized = False Then
        Exit Sub
    End If
    Set m_OutForm = New frmOpenGL
    Load m_OutForm
    m_OutForm.Caption = Caption
    m_lngWidth = XRes
    m_lngHeight = YRes
    SetFormSize m_OutForm, XRes * m_sngScaleRatio, YRes * m_sngScaleRatio
    m_OutForm.Show
    Err.Clear
    GLInit m_OutForm.HWND, m_OutForm.HDC
    GLSetShaderLoadCallback AddressOf ShaderLoadCallback
    If Err <> 0 Then
        MsgBox "Unable to load OpenGL!", vbExclamation, "Error"
        m_booError = True
        Exit Sub
    End If
    m_booEnabled = True
    m_booClosed = False
    Reallocate
    SetFullscreen Not Windowed
End Sub

Private Sub Fury2VideoDriver_Flip()
On Error Resume Next
    If m_booClosed Then Exit Sub
    If Not m_booEnabled Then Exit Sub
    GLFlip ' m_OutForm.HDC
End Sub

Public Sub Fury2VideoDriver_GoFullscreen()
On Error Resume Next
    SetFullscreen True
End Sub

Public Sub Fury2VideoDriver_GoWindowed()
On Error Resume Next
    SetFullscreen False
End Sub

Public Sub Free()
    DestroyBackbuffer
End Sub

Public Sub Reallocate()
    If m_OutForm.WindowState = 1 Then Exit Sub
    InitBackbuffer
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    ShaderPath = App.Path & "\shaders\"
    SetImageTag 0, 0, 0
    Set m_DX7 = New DirectX7
    Set m_DD7 = m_DX7.DirectDrawCreate("")
    F2Init
    m_sngScaleRatio = 1
    'InitOpenGLOverride
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    GLShutdown
End Sub

Private Sub m_OutForm_DblClick()
On Error Resume Next
    m_objHooked.DblClick
End Sub

Private Sub m_OutForm_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyF4 And Shift = vbAltMask Then
        m_objHooked.Closed
        m_booClosed = True
    ElseIf KeyCode = vbKeyReturn And Shift = vbAltMask Then
        SetFullscreen Not m_booFullscreen
    Else
        m_objHooked.KeyDown KeyCode, Shift
    End If
End Sub

Private Sub m_OutForm_KeyPress(KeyAscii As Integer)
On Error Resume Next
    m_objHooked.KeyPress KeyAscii
End Sub

Private Sub m_OutForm_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    m_objHooked.KeyUp KeyCode, Shift
End Sub

Private Sub m_OutForm_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_objHooked.MouseDown Button, Shift, x, y
End Sub

Private Sub m_OutForm_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_objHooked.MouseMove Button, Shift, x, y
End Sub

Private Sub m_OutForm_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_objHooked.MouseUp Button, Shift, x, y
End Sub

Private Sub m_OutForm_Paint()
On Error Resume Next
    m_objHooked.Paint
End Sub

Private Sub m_OutForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    If UnloadMode = 1 Or UnloadMode = 5 Then
    Else
        m_objHooked.Closed
        m_booClosed = True
        Cancel = True
        GLShutdown
        Fury2VideoDriver_Shutdown
    End If
End Sub

Private Sub m_OutForm_Resize()
On Error Resume Next
Static l_lngState As Long
    If (l_lngState <> m_OutForm.WindowState) Or (m_OutForm.ScaleWidth <> (m_lngWidth * m_sngScaleRatio)) Or (m_OutForm.ScaleHeight <> (m_lngHeight * m_sngScaleRatio)) Then
        l_lngState = m_OutForm.WindowState
        Reallocate
        GLFlip ' m_OutForm.HDC
    End If
    m_objHooked.Resize
End Sub

Public Property Get Fury2VideoDriver_MouseVisible() As Boolean
On Error Resume Next
    Fury2VideoDriver_MouseVisible = m_OutForm.MousePointer = 0
End Property

Public Property Let Fury2VideoDriver_MouseVisible(ByVal NewValue As Boolean)
On Error Resume Next
    If m_booFullscreen Then
        If m_OutForm.MousePointer <> IIf(NewValue, 0, 99) Then
            ShowCursor Abs(NewValue)
        End If
    End If
    m_OutForm.MousePointer = IIf(NewValue, 0, 99)
End Property

Public Sub CopySurface(ByVal FromSurface As Fury2Image, ByVal ToSurface As Fury2Image)
On Error Resume Next
    GLCopySurface FromSurface.Handle, ToSurface.Handle
End Sub

Public Function LoadShader(ByVal Shader As String) As Long
On Error Resume Next
    LoadShader = GLGetShader(Shader)
End Function

Public Sub ShaderBlit(ByVal Dest As Fury2Image, ByVal Source As Fury2Image, ByVal DestRect As Fury2Rect, ByVal SourceRect As Fury2Rect, ByVal Shader As Long)
On Error Resume Next
    GLShaderBlit Dest.Handle, Source.Handle, DestRect.GetRectangle, SourceRect.GetRectangle, 0, GetBilinearScaler(), Shader
End Sub
