VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "DirectDrawEngine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Default Output Engine (GDI)"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Attribute VB_Ext_KEY = "Member0" ,"GFXSurface"
Option Explicit
Dim WithEvents m_OutForm As Form
Attribute m_OutForm.VB_VarHelpID = -1
Dim WithEvents m_OutBuffer As PictureBox
Attribute m_OutBuffer.VB_VarHelpID = -1
Dim m_Closed As Boolean
Private m_Hooked As Object
Private m_lngWidth As Long, m_lngHeight As Long
Public Fullscreen As Boolean
Public ScaleRatio As Single
Public Backbuffer As Fury2Image
Public HWFlip As Boolean
Private m_ddcClip As DirectDrawClipper
Private m_ddsHWBuffer As DirectDrawSurface7
Private m_ddsBackbuffer As DirectDrawSurface7
Private m_bytBackbuffer() As Byte
Public VSync As Boolean

Public Function CreateImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    Set CreateImage = F2Image(Width, Height)
End Function

Public Sub SetCaption(Text As String)
    If Fullscreen Then
        If m_OutForm.Caption <> "" Then
            m_OutForm.Caption = ""
            SetWindowText m_OutForm.hWnd, Text
        End If
    Else
        If m_OutForm.Caption <> Text Then m_OutForm.Caption = Text
    End If
End Sub

Public Property Get SupportsGamma() As Boolean
    SupportsGamma = False
End Property

Public Sub HookEvents(Obj As Object)
    Set m_Hooked = Obj
End Sub

Public Property Get Window() As Object
    Set Window = m_OutForm
End Property

Public Property Let Closed(NewClosed As Boolean)
    m_Closed = NewClosed
End Property

Public Property Get Closed() As Boolean
    Closed = m_Closed
End Property

Public Sub Shutdown()
On Error Resume Next
    Free
    Backbuffer.SetHandle 0
    m_Closed = True
    Unload m_OutForm
End Sub

Public Sub ResizeWindow(ByVal XSize As Long, ByVal YSize As Long)
On Error Resume Next
    DDUnInitScreen
    SetFormSize m_OutForm, XSize * ScaleRatio, YSize * ScaleRatio, True
    DDInitScreen Fullscreen, XSize * ScaleRatio, YSize * ScaleRatio, m_Hooked.BitDepth, HWFlip
End Sub

Public Sub Init(ByVal XRes As Long, ByVal YRes As Long, ByVal BPP As Long, ByVal Caption As String, Optional ByVal Windowed As Boolean = False, Optional ByVal ScreenX = 0, Optional ByVal ScreenY = 0, Optional ByVal Window As Long = 0)
On Error Resume Next
    If F2Initialized = False Then
        Exit Sub
    End If
    Fullscreen = Not Windowed
    HWFlip = Fullscreen
    m_lngWidth = XRes
    m_lngHeight = YRes
    LoadForm
    DDInit m_OutForm.hWnd
    DDInitScreen Not Windowed, XRes * ScaleRatio, YRes * ScaleRatio, BPP, HWFlip
    Reallocate
End Sub

Sub LoadForm()
On Error Resume Next
    If m_OutForm Is Nothing Then
    Else
        m_OutForm.Hide
        Unload m_OutForm
        Set m_OutForm = Nothing
        Set m_OutBuffer = Nothing
    End If
    If Fullscreen Then
        Set m_OutForm = New frmDirectDrawFullscreen
    Else
        Set m_OutForm = New frmDirectDraw
    End If
    Load m_OutForm
    Set m_OutBuffer = m_OutForm.picBuffer
    SetFormSize m_OutForm, m_lngWidth * ScaleRatio, m_lngHeight * ScaleRatio
    m_OutForm.Show
End Sub

Public Sub Flip()
On Error Resume Next
Dim l_rctDest As DxVBLib.Rect
Dim l_rctSource As DxVBLib.Rect
    If m_Closed Then Exit Sub
    LockBackbuffer
    If m_ddsBackbuffer.isLost Then
        Reallocate
    End If
    If m_ddsHWBuffer.isLost Then
        Reallocate
    End If
    If m_ddsScreen.isLost Then
        DDUnInitScreen
        DDInitScreen Fullscreen, m_lngWidth * ScaleRatio, m_lngHeight * ScaleRatio, m_Hooked.BitDepth, HWFlip
        InitClipper
    End If
    m_DX7.GetWindowRect m_OutForm.picBuffer.hWnd, l_rctDest
    l_rctSource.Right = Backbuffer.Width
    l_rctSource.Bottom = Backbuffer.Height
    m_ddsHWBuffer.BltFast 0, 0, m_ddsBackbuffer, l_rctSource, DDBLTFAST_WAIT
    If HWFlip Then
        m_ddsFlip.Blt l_rctDest, m_ddsHWBuffer, l_rctSource, DDBLT_WAIT
        If VSync Then
            m_ddsScreen.Flip Nothing, DDFLIP_WAIT
        Else
            m_ddsScreen.Flip Nothing, DDFLIP_WAIT Or DDFLIP_NOVSYNC
        End If
    Else
        If VSync Then
            m_DD7.WaitForVerticalBlank DDWAITVB_BLOCKEND, 0
        End If
        m_ddsScreen.Blt l_rctDest, m_ddsHWBuffer, l_rctSource, DDBLT_WAIT
    End If
    If Fullscreen And m_Hooked.ShowFPS And (Not HWFlip) Then
        m_OutForm.picBuffer.ForeColor = 0
        m_OutForm.picBuffer.CurrentX = 1
        m_OutForm.picBuffer.CurrentY = 1
        m_OutForm.picBuffer.Print m_Hooked.FPS
        m_OutForm.picBuffer.ForeColor = RGB(255, 255, 255)
        m_OutForm.picBuffer.CurrentX = 0
        m_OutForm.picBuffer.CurrentY = 0
        m_OutForm.picBuffer.Print m_Hooked.FPS
    End If
    UnlockBackbuffer
End Sub

Public Sub GoFullscreen()
On Error Resume Next
    Fullscreen = True
    HWFlip = Fullscreen
    Free
    DDUnInitScreen
    DDShutdown
    LoadForm
    DDInit m_OutForm.hWnd
    DDInitScreen Fullscreen, m_lngWidth * ScaleRatio, m_lngHeight * ScaleRatio, m_Hooked.BitDepth, HWFlip
    Allocate
End Sub

Public Sub GoWindowed()
On Error Resume Next
    Fullscreen = False
    HWFlip = Fullscreen
    Free
    DDUnInitScreen
    DDShutdown
    LoadForm
    DDInit m_OutForm.hWnd
    DDInitScreen Fullscreen, m_lngWidth * ScaleRatio, m_lngHeight * ScaleRatio, m_Hooked.BitDepth, HWFlip
    Allocate
End Sub

Friend Sub LockBackbuffer()
On Error Resume Next
Dim l_rcLock As DxVBLib.Rect
    SoftFX.SetImagePitch Backbuffer.Handle, 0
    SoftFX.SetImagePointer Backbuffer.Handle, 0
    SoftFX.SetImageLocked Backbuffer.Handle, 1
    l_rcLock.Right = Backbuffer.Width
    l_rcLock.Bottom = Backbuffer.Height
    Err.Clear
    m_ddsBackbuffer.Unlock l_rcLock
    Erase m_bytBackbuffer
End Sub

Friend Sub UnlockBackbuffer()
On Error Resume Next
Dim l_sdDesc As DDSURFACEDESC2
Dim l_rcLock As DxVBLib.Rect
    m_ddsBackbuffer.GetSurfaceDesc l_sdDesc
    SoftFX.SetImagePointer Backbuffer.Handle, 0
    SoftFX.SetImageLocked Backbuffer.Handle, 1
    l_rcLock.Right = l_sdDesc.lWidth
    l_rcLock.Bottom = l_sdDesc.lHeight
    Err.Clear
    m_ddsBackbuffer.Lock l_rcLock, l_sdDesc, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT Or DDLOCK_SURFACEMEMORYPTR, 0
    If Err = 0 Then
        m_ddsBackbuffer.GetLockedArray m_bytBackbuffer
        SoftFX.SetImageWidth Backbuffer.Handle, l_sdDesc.lWidth
        SoftFX.SetImageHeight Backbuffer.Handle, l_sdDesc.lHeight
        SoftFX.SetImagePitch Backbuffer.Handle, l_sdDesc.lPitch - (l_sdDesc.lWidth * 4)
        SoftFX.SetImagePointer Backbuffer.Handle, ByVal VarPtr(m_bytBackbuffer(0, 0))
        SoftFX.SetImageLocked Backbuffer.Handle, 0
    End If
End Sub

Public Sub Free()
On Error Resume Next
    LockBackbuffer
    SetImageWidth Backbuffer.Handle, 0
    SetImageHeight Backbuffer.Handle, 0
    SetImagePointer Backbuffer.Handle, 0
    m_ddsScreen.SetClipper Nothing
    Set m_ddcClip = Nothing
    Set m_ddsBackbuffer = Nothing
    Set m_ddsHWBuffer = Nothing
End Sub

Sub InitClipper()
On Error Resume Next
    Set m_ddcClip = m_DD7.CreateClipper(0)
    m_ddcClip.SetHWnd m_OutForm.picBuffer.hWnd
    m_ddsScreen.SetClipper m_ddcClip
End Sub

Public Sub Allocate()
On Error Resume Next
Dim l_rctSurface As DxVBLib.Rect
    SoftFX.SetImageWidth Backbuffer.Handle, m_lngWidth
    SoftFX.SetImageHeight Backbuffer.Handle, m_lngHeight
    SoftFX.SetImagePitch Backbuffer.Handle, 0
    SoftFX.SetImagePointer Backbuffer.Handle, 0
    SoftFX.SetImageLocked Backbuffer.Handle, 1
    Set m_ddsBackbuffer = DDCreateSurface(m_lngWidth, m_lngHeight)
    Set m_ddsHWBuffer = DDCreateSurface(m_lngWidth, m_lngHeight, True)
    l_rctSurface.Right = m_lngWidth
    l_rctSurface.Bottom = m_lngHeight
    m_ddsBackbuffer.BltColorFill l_rctSurface, 0
    InitClipper
    UnlockBackbuffer
End Sub

Public Sub Reallocate()
On Error Resume Next
    Free
    Allocate
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set Backbuffer = New Fury2Image
    ScaleRatio = 1
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    DDUnInitScreen
    DDShutdown
End Sub

Private Sub m_OutBuffer_DblClick()
On Error Resume Next
    m_Hooked.GFXEngine_DblClick
End Sub

Private Sub m_OutForm_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyF4 And Shift = vbAltMask Then
        m_OutForm.Hide
        m_Closed = True
    ElseIf KeyCode = vbKeyReturn And Shift = vbAltMask Then
        If Fullscreen Then
            GoWindowed
        Else
            GoFullscreen
        End If
    Else
        m_Hooked.GFXEngine_KeyDown KeyCode, Shift
    End If
End Sub

Private Sub m_OutForm_KeyPress(KeyAscii As Integer)
On Error Resume Next
    m_Hooked.GFXEngine_KeyPress KeyAscii
End Sub

Private Sub m_OutForm_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    m_Hooked.GFXEngine_KeyUp KeyCode, Shift
End Sub

Private Sub m_OutBuffer_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseDown Button, Shift, X, Y
End Sub

Private Sub m_OutBuffer_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseMove Button, Shift, X, Y
End Sub

Private Sub m_OutBuffer_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseUp Button, Shift, X, Y
End Sub

Private Sub m_OutBuffer_Paint()
On Error Resume Next
    m_Hooked.GFXEngine_Paint
End Sub

Private Sub m_OutForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    If UnloadMode = 1 Or UnloadMode = 5 Then
    Else
        Shutdown
        m_Hooked.GFXEngine_Closed
        m_Closed = True
    End If
End Sub

Private Sub m_OutForm_Resize()
On Error Resume Next
    m_Hooked.GFXEngine_Resize
    If m_OutForm.WindowState = 2 Then
        GoFullscreen
    End If
End Sub
