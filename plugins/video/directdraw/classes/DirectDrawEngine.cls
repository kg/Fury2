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
Implements Fury2VideoDriver
Dim WithEvents m_OutForm As Form
Attribute m_OutForm.VB_VarHelpID = -1
Dim WithEvents m_OutBuffer As PictureBox
Attribute m_OutBuffer.VB_VarHelpID = -1
Dim m_Closed As Boolean
Private m_objHooked As Fury2VideoEventSink
Private m_lngWidth As Long, m_lngHeight As Long
Private m_booFullscreen As Boolean
Private m_sngScaleRatio As Single
Private m_imgBackbuffer As Fury2Image
Public HWFlip As Boolean
Private m_ddcClip As DirectDrawClipper
Private m_ddsHWBuffer As DirectDrawSurface7
Private m_ddsBackbuffer As DirectDrawSurface7
Private m_bytBackbuffer() As Byte
Public VSync As Boolean

Private Property Get Fury2VideoDriver_Backbuffer() As libGraphics.Fury2Image
    Set Fury2VideoDriver_Backbuffer = m_imgBackbuffer
End Property

Public Function Fury2VideoDriver_CreateImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    Set Fury2VideoDriver_CreateImage = F2Image(Width, Height)
End Function

Private Property Get Fury2VideoDriver_Fullscreen() As Boolean
    Fury2VideoDriver_Fullscreen = m_booFullscreen
End Property

Private Sub Fury2VideoDriver_HookEvents(ByVal Obj As libVideo.Fury2VideoEventSink)
On Error Resume Next
    Set m_objHooked = Obj
End Sub

Private Property Let Fury2VideoDriver_ScaleRatio(ByVal RHS As Single)
On Error Resume Next
    m_sngScaleRatio = RHS
End Property

Public Sub Fury2VideoDriver_SetCaption(Text As String)
    If m_booFullscreen Then
        If m_OutForm.Caption <> "" Then
            m_OutForm.Caption = ""
            SetWindowText m_OutForm.hwnd, Text
        End If
    Else
        If m_OutForm.Caption <> Text Then m_OutForm.Caption = Text
    End If
End Sub

Public Property Get Fury2VideoDriver_Window() As Object
    Set Fury2VideoDriver_Window = m_OutForm
End Property

Public Property Get Fury2VideoDriver_Closed() As Boolean
    Fury2VideoDriver_Closed = m_Closed
End Property

Public Sub Fury2VideoDriver_Shutdown()
On Error Resume Next
    Free
    m_imgBackbuffer.SetHandle 0
    m_Closed = True
    Unload m_OutForm
End Sub

Public Sub Fury2VideoDriver_ResizeWindow(ByVal XSize As Long, ByVal YSize As Long)
On Error Resume Next
    DDUnInitScreen
    SetFormSize m_OutForm, XSize * m_sngScaleRatio, YSize * m_sngScaleRatio, True
    DDInitScreen m_booFullscreen, XSize * m_sngScaleRatio, YSize * m_sngScaleRatio, HWFlip
    InitClipper
End Sub

Public Sub Fury2VideoDriver_Init(ByVal XRes As Long, ByVal YRes As Long, ByRef Caption As String, Optional ByVal Windowed As Boolean = False)
On Error Resume Next
    If Not F2Initialized Then Exit Sub
    m_booFullscreen = Not Windowed
    HWFlip = m_booFullscreen
    m_lngWidth = XRes
    m_lngHeight = YRes
    LoadForm
    DDInit m_OutForm.hwnd
    DDInitScreen Not Windowed, XRes * m_sngScaleRatio, YRes * m_sngScaleRatio, HWFlip
    Reallocate
End Sub

Friend Sub LoadForm()
On Error Resume Next
    If m_OutForm Is Nothing Then
    Else
        m_OutForm.Hide
        Unload m_OutForm
        Set m_OutForm = Nothing
        Set m_OutBuffer = Nothing
    End If
    If m_booFullscreen Then
        Set m_OutForm = New frmDirectDrawFullscreen
    Else
        Set m_OutForm = New frmDirectDraw
    End If
    Load m_OutForm
    Set m_OutBuffer = m_OutForm.picBuffer
    SetFormSize m_OutForm, m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio
    m_OutForm.Show
End Sub

Public Sub Fury2VideoDriver_Flip()
On Error Resume Next
Dim l_rctDest As DxVBLib.RECT
Dim l_rctSource As DxVBLib.RECT
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
        DDInitScreen m_booFullscreen, m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, HWFlip
        InitClipper
    End If
    m_DX7.GetWindowRect m_OutForm.picBuffer.hwnd, l_rctDest
    l_rctSource.Right = m_imgBackbuffer.Width
    l_rctSource.Bottom = m_imgBackbuffer.Height
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
    UnlockBackbuffer
End Sub

Public Sub Fury2VideoDriver_GoFullscreen()
On Error Resume Next
    m_booFullscreen = True
    HWFlip = m_booFullscreen
    Free
    DDUnInitScreen
    DDShutdown
    LoadForm
    DDInit m_OutForm.hwnd
    DDInitScreen m_booFullscreen, m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, HWFlip
    Allocate
End Sub

Public Sub Fury2VideoDriver_GoWindowed()
On Error Resume Next
    m_booFullscreen = False
    HWFlip = m_booFullscreen
    Free
    DDUnInitScreen
    DDShutdown
    LoadForm
    DDInit m_OutForm.hwnd
    DDInitScreen m_booFullscreen, m_lngWidth * m_sngScaleRatio, m_lngHeight * m_sngScaleRatio, HWFlip
    Allocate
End Sub

Friend Sub LockBackbuffer()
On Error Resume Next
Dim l_rcLock As DxVBLib.RECT
    SoftFX.SetImagePitch m_imgBackbuffer.Handle, 0
    SoftFX.SetImagePointer m_imgBackbuffer.Handle, 0
    SoftFX.SetImageLocked m_imgBackbuffer.Handle, 1
    l_rcLock.Right = m_imgBackbuffer.Width
    l_rcLock.Bottom = m_imgBackbuffer.Height
    Err.Clear
    m_ddsBackbuffer.Unlock l_rcLock
    Erase m_bytBackbuffer
End Sub

Friend Sub UnlockBackbuffer()
On Error Resume Next
Dim l_sdDesc As DDSURFACEDESC2
Dim l_rcLock As DxVBLib.RECT
    m_ddsBackbuffer.GetSurfaceDesc l_sdDesc
    SoftFX.SetImagePointer m_imgBackbuffer.Handle, 0
    SoftFX.SetImageLocked m_imgBackbuffer.Handle, 1
    l_rcLock.Right = l_sdDesc.lWidth
    l_rcLock.Bottom = l_sdDesc.lHeight
    Err.Clear
    m_ddsBackbuffer.Lock l_rcLock, l_sdDesc, DDLOCK_NOSYSLOCK Or DDLOCK_WAIT Or DDLOCK_SURFACEMEMORYPTR, 0
    If Err = 0 Then
        m_ddsBackbuffer.GetLockedArray m_bytBackbuffer
        SoftFX.SetImageWidth m_imgBackbuffer.Handle, l_sdDesc.lWidth
        SoftFX.SetImageHeight m_imgBackbuffer.Handle, l_sdDesc.lHeight
        SoftFX.SetImagePitch m_imgBackbuffer.Handle, l_sdDesc.lPitch - (l_sdDesc.lWidth * 4)
        SoftFX.SetImagePointer m_imgBackbuffer.Handle, ByVal VarPtr(m_bytBackbuffer(0, 0))
        SoftFX.SetImageLocked m_imgBackbuffer.Handle, 0
    End If
End Sub

Public Sub Free()
On Error Resume Next
    LockBackbuffer
    SetImageWidth m_imgBackbuffer.Handle, 0
    SetImageHeight m_imgBackbuffer.Handle, 0
    SetImagePointer m_imgBackbuffer.Handle, 0
    m_ddsScreen.SetClipper Nothing
    Set m_ddcClip = Nothing
    Set m_ddsBackbuffer = Nothing
    Set m_ddsHWBuffer = Nothing
End Sub

Friend Sub InitClipper()
On Error Resume Next
    Set m_ddcClip = m_DD7.CreateClipper(0)
    m_ddcClip.SetHWnd m_OutForm.picBuffer.hwnd
    m_ddsScreen.SetClipper m_ddcClip
End Sub

Friend Sub Allocate()
On Error Resume Next
Dim l_rctSurface As DxVBLib.RECT
    SoftFX.SetImageWidth m_imgBackbuffer.Handle, m_lngWidth
    SoftFX.SetImageHeight m_imgBackbuffer.Handle, m_lngHeight
    SoftFX.SetImagePitch m_imgBackbuffer.Handle, 0
    SoftFX.SetImagePointer m_imgBackbuffer.Handle, 0
    SoftFX.SetImageLocked m_imgBackbuffer.Handle, 1
    Set m_ddsBackbuffer = DDCreateSurface(m_lngWidth, m_lngHeight)
    Set m_ddsHWBuffer = DDCreateSurface(m_lngWidth, m_lngHeight, True)
    l_rctSurface.Right = m_lngWidth
    l_rctSurface.Bottom = m_lngHeight
    m_ddsBackbuffer.BltColorFill l_rctSurface, 0
    InitClipper
    UnlockBackbuffer
End Sub

Friend Sub Reallocate()
On Error Resume Next
    Free
    Allocate
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set m_imgBackbuffer = New Fury2Image
    m_sngScaleRatio = 1
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    DDUnInitScreen
    DDShutdown
End Sub

Private Sub m_OutBuffer_DblClick()
On Error Resume Next
    m_objHooked.DblClick
End Sub

Private Sub m_OutForm_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyF4 And Shift = vbAltMask Then
        m_OutForm.Hide
        Fury2VideoDriver_Shutdown
        m_objHooked.Closed
        m_Closed = True
    ElseIf KeyCode = vbKeyReturn And Shift = vbAltMask Then
        If m_booFullscreen Then
            Fury2VideoDriver_GoWindowed
        Else
            Fury2VideoDriver_GoFullscreen
        End If
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

Private Sub m_OutBuffer_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_objHooked.MouseDown Button, Shift, X, Y
End Sub

Private Sub m_OutBuffer_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_objHooked.MouseMove Button, Shift, X, Y
End Sub

Private Sub m_OutBuffer_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_objHooked.MouseUp Button, Shift, X, Y
End Sub

Private Sub m_OutBuffer_Paint()
On Error Resume Next
    m_objHooked.Paint
End Sub

Private Sub m_OutForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    If UnloadMode = 1 Or UnloadMode = 5 Then
    Else
        Fury2VideoDriver_Shutdown
        m_objHooked.Closed
        m_Closed = True
    End If
End Sub

Private Sub m_OutForm_Resize()
On Error Resume Next
    m_objHooked.Resize
    If m_OutForm.WindowState = 2 Then
        Fury2VideoDriver_GoFullscreen
    End If
    InitClipper
End Sub

Public Property Get Fury2VideoDriver_MouseVisible() As Boolean
On Error Resume Next
    Fury2VideoDriver_MouseVisible = m_OutForm.MousePointer = 0
End Property

Public Property Let Fury2VideoDriver_MouseVisible(ByVal NewValue As Boolean)
On Error Resume Next
    m_OutForm.MousePointer = IIf(NewValue, 0, 99)
End Property

Public Property Get Fury2VideoDriver_ScaleRatio() As Single
    Fury2VideoDriver_ScaleRatio = m_sngScaleRatio
End Property
