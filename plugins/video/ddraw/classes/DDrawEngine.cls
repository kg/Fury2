VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "DDrawEngine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Attribute VB_Ext_KEY = "Member0" ,"GFXSurface"
Option Explicit

Dim WithEvents m_OutForm As Form
Attribute m_OutForm.VB_VarHelpID = -1
Dim WithEvents m_OutPic As PictureBox
Attribute m_OutPic.VB_VarHelpID = -1
Dim m_DX7 As DirectX7
Dim m_DD7 As DirectDraw7
Dim m_ddsBackbuffer As DirectDrawSurface7
Dim m_ddsFrontbuffer As DirectDrawSurface7
Dim m_dclClipper As DirectDrawClipper
Dim m_dsdBackbuffer As DDSURFACEDESC2
Dim m_dsdFrontbuffer As DDSURFACEDESC2
Dim m_Closed As Boolean
Dim m_lngBorderSize As Long
Dim m_booRenderToFrontbuffer As Boolean
Public Event MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
Public Event MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
Public Event MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
Public Event KeyDown(KeyCode As Integer, Shift As Integer)
Public Event KeyUp(KeyCode As Integer, Shift As Integer)
Public Event Closed()
Public Event Resize()
Public Event Paint()
Private m_Hooked As Object
Public VSync As Boolean
Private m_lngWidth As Long, m_lngHeight As Long, m_lngBPP As Long, m_booFullscreen As Boolean
Private m_lngOWidth As Long, m_lngOHeight As Long

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

Friend Sub ClosedEvent()
On Error Resume Next
    m_Hooked.GFXEngine_Closed
End Sub

Public Sub ClearScreen(Optional Color As Long = 0)
Dim m_rctFill As DxVBLib.RECT
    With m_rctFill
        .Right = m_lngWidth
        .Bottom = m_lngHeight
    End With
    m_ddsBackbuffer.BltColorFill m_rctFill, Color
End Sub

Public Property Get SupportsGamma() As Boolean
    SupportsGamma = True
End Property

Private Sub SetTopmost(Window As Form, Topmost As Boolean)
On Error Resume Next
Dim lResult As Long
    If Topmost Then
        Window.ZOrder
        lResult = SetWindowPos(Window.hWnd, Topmost, 0, 0, 0, 0, NoMove Or NoSize)
        Window.ZOrder
    Else
        lResult = SetWindowPos(Window.hWnd, NotTopMost, 0, 0, 0, 0, NoMove Or NoSize)
    End If
End Sub

Private Sub SetMousePos(ByRef Frm As Form, x As Long, y As Long)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
Dim FrmWidth As Long, FrmHeight As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    Frm.ScaleMode = OldMode
    SetCursorPos (x * (Frm.ScaleWidth / m_Hooked.BufferWidth)) + (BW / Screen.TwipsPerPixelX) + (Frm.Left / Screen.TwipsPerPixelX), (y * (Frm.ScaleHeight / m_Hooked.BufferHeight)) + ((BW + TH) / Screen.TwipsPerPixelY) + (Frm.Top / Screen.TwipsPerPixelY)
End Sub

Private Sub SetFormSize(ByRef Frm As Form, x As Long, y As Long, Optional Center As Boolean = False)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
Dim FrmWidth As Long, FrmHeight As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    FrmWidth = CLng(x * Screen.TwipsPerPixelX) + (BW * 2)
    FrmHeight = CLng(y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    If Center = True Then
        Frm.Move (Screen.Width - FrmWidth) \ 2, (Screen.Height - FrmHeight) \ 2, FrmWidth, FrmHeight
    Else
        Frm.Move Frm.Left, Frm.Top, FrmWidth, FrmHeight
    End If
    Frm.ScaleMode = OldMode
End Sub

Public Property Get Fullscreen() As Boolean
    Fullscreen = m_booFullscreen
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
    m_Closed = True
    F2Shutdown
    m_ddsFrontbuffer.SetClipper Nothing
    Set m_ddsFrontbuffer = Nothing
    Set m_ddsBackbuffer = Nothing
    Set m_dclClipper = Nothing
    Set m_DD7 = Nothing
    Set m_DX7 = Nothing
    Set m_OutPic = Nothing
    Unload m_OutForm
End Sub

Public Sub ResizeWindow(ByVal XSize As Long, ByVal YSize As Long)
On Error Resume Next
    If F2Initialized = False Then Exit Sub
    If m_Closed Then Exit Sub
    If m_booFullscreen = True Then Exit Sub
    SetFormSize m_OutForm, XSize, YSize, True
    m_OutForm.picBackbuffer.Move 0, 0, XSize, YSize
End Sub

Public Sub Init(ByVal XRes As Long, ByVal YRes As Long, ByVal BPP As Long, ByVal Caption As String, Icon As IPictureDisp, Optional ByVal Windowed As Boolean = False, Optional ByVal ScreenX = 0, Optional ByVal ScreenY = 0, Optional ByVal OutputWindow As Long = 0)
On Error Resume Next
    If F2Initialized = False Then Exit Sub
    If XRes <= 0 Or YRes <= 0 Then Exit Sub
    If BPP < 16 Then BPP = 16
    If XRes < 320 Then XRes = 320
    If YRes < 200 Then YRes = 200
    If ScreenX < 320 Then ScreenX = 320
    If ScreenY < 200 Then ScreenY = 200
    Set m_OutForm = New frmDirectDraw
    Load m_OutForm
    Set m_OutPic = m_OutForm.picBackbuffer
    Set m_OutForm.Icon = Icon
    Set m_OutForm.Parent = Me
    SetFormSize m_OutForm, XRes, YRes, True
    m_OutForm.picBackbuffer.Move 0, 0, XRes, YRes
    m_OutForm.Show
    m_OutForm.Caption = "Initializing DirectX7"
    Set m_DX7 = Nothing
    Err.Clear
    Set m_DX7 = New DirectX7
    If Err <> 0 Then MsgBox "DirectX 7 Init Failed. Try GDI." + vbCrLf + Err.Description: m_Hooked.CriticalError "DirectDrawEngine.Init", "Set m_DX7 = New DirectX7": Exit Sub
    Err.Clear
    Set m_DD7 = m_DX7.DirectDrawCreate("")
    If Err <> 0 Then MsgBox "DirectDraw 7 Init Failed. Try GDI." + vbCrLf + Err.Description: m_Hooked.CriticalError "DirectDrawEngine.Init", "Set m_DD7 = m_DX7.DirectDrawCreate("""")": Exit Sub
    m_lngWidth = XRes
    m_lngHeight = YRes
    m_lngOWidth = ScreenX
    m_lngOHeight = ScreenY
    m_lngBPP = BPP
    If (Windowed) Then
        GoWindowed
    Else
        GoFullscreen
    End If
    m_OutForm.Caption = Caption
End Sub

Public Sub Flip()
On Error Resume Next
Dim m_rctWindow As DxVBLib.RECT
Dim m_rctSource As DxVBLib.RECT
Dim m_dsdScreen As DDSURFACEDESC2, m_rctEmpty As DxVBLib.RECT
Dim m_dfxFX As DDBLTFX
    If F2Initialized = False Then Exit Sub
    If m_lngWidth <= 0 Or m_lngHeight <= 0 Then Exit Sub
    If m_Closed Then Exit Sub
    If m_OutForm.WindowState = 1 Then Exit Sub
    If m_booRenderToFrontbuffer Then Exit Sub
    If m_ddsBackbuffer Is Nothing Or m_ddsFrontbuffer Is Nothing Then Reallocate: Exit Sub
    If m_ddsBackbuffer.isLost Or m_ddsFrontbuffer.isLost Then Reallocate: Exit Sub
    Err.Clear
    If m_booFullscreen = True Then
        m_OutForm.Move -(m_lngBorderSize * Screen.TwipsPerPixelX), -(m_lngBorderSize * Screen.TwipsPerPixelY), (m_lngOWidth + (m_lngBorderSize * 2)) * Screen.TwipsPerPixelX, (m_lngOHeight + (m_lngBorderSize * 2)) * Screen.TwipsPerPixelY
        With m_rctSource
            .Left = 0
            .Top = 0
            .Right = m_dsdBackbuffer.lWidth
            .Bottom = m_dsdBackbuffer.lHeight
        End With
        With m_rctWindow
            .Left = 0
            .Top = 0
            .Right = m_lngOWidth
            .Bottom = m_lngOHeight
        End With
        If m_Hooked.VSync Then
            m_DD7.WaitForVerticalBlank DDWAITVB_BLOCKBEGIN, 0
        End If
        Err.Clear
        m_ddsFrontbuffer.Blt m_rctWindow, m_ddsBackbuffer, m_rctSource, DDBLT_WAIT
    Else
        m_DX7.GetWindowRect m_OutForm.picBackbuffer.hWnd, m_rctWindow
        With m_rctSource
            .Left = 0
            .Top = 0
            .Right = m_dsdBackbuffer.lWidth
            .Bottom = m_dsdBackbuffer.lHeight
        End With
        If m_Hooked.VSync Then
            m_DD7.WaitForVerticalBlank DDWAITVB_BLOCKBEGIN, 0
        End If
        Err.Clear
        m_ddsFrontbuffer.Blt m_rctWindow, m_ddsBackbuffer, m_rctSource, DDBLT_WAIT
    End If
End Sub

Public Sub GoFullscreen()
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
Dim FrmWidth As Long, FrmHeight As Long
    If F2Initialized = False Then Exit Sub
    If m_Closed Then Exit Sub
    Free
    If m_lngOWidth = 0 Then m_lngOWidth = m_lngWidth
    If m_lngOHeight = 0 Then m_lngOHeight = m_lngHeight
    SetTopmost m_OutForm, True
    m_DD7.SetCooperativeLevel m_OutForm.hWnd, DDSCL_FULLSCREEN Or DDSCL_ALLOWMODEX Or DDSCL_ALLOWREBOOT Or DDSCL_EXCLUSIVE Or DDSCL_NOWINDOWCHANGES
    If m_lngBPP <= 0 Then
        Err.Clear
        m_DD7.SetDisplayMode m_lngOWidth, m_lngOHeight, 32, 0, DDSDM_DEFAULT
        If Err <> 0 Then
            Err.Clear
            m_DD7.SetDisplayMode m_lngOWidth, m_lngOHeight, 24, 0, DDSDM_DEFAULT
            If Err <> 0 Then
                Err.Clear
                m_DD7.SetDisplayMode m_lngOWidth, m_lngOHeight, 16, 0, DDSDM_DEFAULT
            End If
        End If
    Else
        Err.Clear
        m_DD7.SetDisplayMode m_lngOWidth, m_lngOHeight, m_lngBPP, 0, DDSDM_DEFAULT
        If Err <> 0 Then
            Err.Clear
            m_DD7.SetDisplayMode m_lngOWidth, m_lngOHeight, 32, 0, DDSDM_DEFAULT
            If Err <> 0 Then
                Err.Clear
                m_DD7.SetDisplayMode m_lngOWidth, m_lngOHeight, 24, 0, DDSDM_DEFAULT
                If Err <> 0 Then
                    Err.Clear
                    m_DD7.SetDisplayMode m_lngOWidth, m_lngOHeight, 16, 0, DDSDM_DEFAULT
                End If
            End If
        End If
    End If
    m_ddsBackbuffer.GetPixelFormat m_dsdBackbuffer.ddpfPixelFormat
    m_booFullscreen = True
    m_OutForm.ScaleMode = 1
    m_lngBorderSize = ((m_OutForm.Width - m_OutForm.ScaleWidth) \ 2) \ Screen.TwipsPerPixelX
    m_OutForm.ScaleMode = 3
    m_OutForm.Move -(m_lngBorderSize * Screen.TwipsPerPixelX), -(m_lngBorderSize * Screen.TwipsPerPixelY), (m_lngOWidth + (m_lngBorderSize * 2)) * Screen.TwipsPerPixelX, (m_lngOHeight + (m_lngBorderSize * 2)) * Screen.TwipsPerPixelY
    Reallocate
End Sub

Public Sub GoWindowed()
On Error Resume Next
    If F2Initialized = False Then Exit Sub
    If m_Closed Then Exit Sub
    Free
    m_OutForm.Visible = False
    m_OutForm.BorderStyle = 2
    m_OutForm.Visible = True
    SetTopmost m_OutForm, False
    m_booFullscreen = False
    m_DD7.RestoreDisplayMode
    m_DD7.SetCooperativeLevel frmDirectDraw.hWnd, DDSCL_NORMAL Or DDSCL_NOWINDOWCHANGES
    m_OutForm.Caption = m_Hooked.WindowCaption
    SetFormSize m_OutForm, m_Hooked.ScreenWidth, m_Hooked.ScreenHeight, True
    Reallocate
End Sub

Public Sub Free()
On Error Resume Next
    m_ddsFrontbuffer.SetClipper Nothing
    Set m_ddsFrontbuffer = Nothing
    Set m_ddsBackbuffer = Nothing
    Set m_dclClipper = Nothing
End Sub

Public Property Get RenderToFrontBuffer() As Boolean
    RenderToFrontBuffer = m_booRenderToFrontbuffer
End Property

Public Property Let RenderToFrontBuffer(NewValue As Boolean)
    If m_booRenderToFrontbuffer <> NewValue Then
        m_booRenderToFrontbuffer = NewValue
        Reallocate
    End If
End Property

Public Sub Reallocate()
On Error Resume Next
    If F2Initialized = False Then Exit Sub
    If m_Closed Then Exit Sub
    Free
    If m_lngWidth <= 0 Or m_lngHeight <= 0 Then Exit Sub
    With m_dsdBackbuffer
        .lFlags = DDSD_CAPS Or DDSD_WIDTH Or DDSD_HEIGHT Or DDSD_PIXELFORMAT
        .ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_VIDEOMEMORY
        .lWidth = m_lngWidth
        .lHeight = m_lngHeight
        With .ddpfPixelFormat
            .lRGBBitCount = 32
            .lBBitMask = F2RGB(0, 0, 255, 0)
            .lGBitMask = F2RGB(0, 255, 0, 0)
            .lRBitMask = F2RGB(255, 0, 0, 0)
            .lRGBAlphaBitMask = &HFFFFFFFF
            .lFlags = DDPF_RGB
        End With
    End With
    With m_dsdFrontbuffer
        .lFlags = DDSD_CAPS
        .ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_VIDEOMEMORY
    End With
    If m_booRenderToFrontbuffer Then
    Else
        Err.Clear
        Set m_ddsBackbuffer = m_DD7.CreateSurface(m_dsdBackbuffer)
        Select Case Err
        Case 0
        Case DxVBLib.CONST_DDRAWERR.DDERR_OUTOFMEMORY
            MsgBox "DirectX was unable to allocate memory for the backbuffer. Fury² will now close."
            m_Hooked.CriticalError "DirectDrawEngine.Reallocate", "Set m_ddsBackbuffer = m_DD7.CreateSurface(m_dsdBackbuffer)", "DXVBLib.CONST_DDRAWERR.DDERR_OUTOFMEMORY": Exit Sub
            Exit Sub
        Case DxVBLib.CONST_DDRAWERR.DDERR_OUTOFVIDEOMEMORY
            MsgBox "DirectX was unable to allocate video memory for the backbuffer."
            m_booRenderToFrontbuffer = True
            Reallocate
            Exit Sub
        Case DxVBLib.CONST_DDRAWERR.DDERR_INVALIDPIXELFORMAT
            m_Hooked.TextOut "Hardware doesn't support 32-bit video surfaces."
            m_booRenderToFrontbuffer = True
            Reallocate
            Exit Sub
        Case DxVBLib.CONST_DDRAWERR.DDERR_NODIRECTDRAWHW
            If m_booRenderToFrontbuffer Then
                m_Hooked.TextOut "No Hardware DirectDraw acceleration support. Fury² will now close."
                m_Hooked.CriticalError "DirectDrawEngine.Reallocate""Set m_ddsBackbuffer = m_DD7.CreateSurface(m_dsdBackbuffer)", "DXVBLib.CONST_DDRAWERR.DDERR_NODIRECTDRAWHW"
                Exit Sub
            Else
                m_Hooked.TextOut "No Hardware DirectDraw acceleration support. Switching to direct render."
                m_booRenderToFrontbuffer = True
                Reallocate
                Exit Sub
            End If
        Case Else
            m_Hooked.CriticalError "DirectDrawEngine.Reallocate", "Set m_ddsBackbuffer = m_DD7.CreateSurface(m_dsdBackbuffer)"
            Exit Sub
        End Select
        m_ddsBackbuffer.GetPixelFormat m_dsdBackbuffer.ddpfPixelFormat
    End If
    Set m_ddsFrontbuffer = m_DD7.CreateSurface(m_dsdFrontbuffer)
    Set m_dclClipper = m_DD7.CreateClipper(0)
    m_dclClipper.SetHWnd m_OutForm.picBackbuffer.hWnd
    m_ddsFrontbuffer.SetClipper m_dclClipper
    DoEvents
End Sub

Private Sub m_OutForm_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyF4 And Shift = vbAltMask Then
        m_OutForm.Hide
        m_Closed = True
    ElseIf KeyCode = vbKeyF5 And Shift = vbAltMask Then
        If m_booFullscreen Then
            If m_Hooked.ScreenWidth = m_Hooked.BufferWidth Then
                m_Hooked.ScreenWidth = m_Hooked.BufferWidth * 2
                m_Hooked.ScreenHeight = m_Hooked.BufferHeight * 2
            Else
                m_Hooked.ScreenWidth = m_Hooked.BufferWidth
                m_Hooked.ScreenHeight = m_Hooked.BufferHeight
            End If
            GoWindowed
            GoFullscreen
        End If
    ElseIf KeyCode = vbKeyReturn And Shift = vbAltMask Then
        If m_booFullscreen Then
            GoWindowed
        Else
            GoFullscreen
        End If
    Else
        m_Hooked.GFXEngine_KeyDown KeyCode, Shift
    End If
End Sub

Private Sub m_OutForm_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    m_Hooked.GFXEngine_KeyUp KeyCode, Shift
End Sub

Private Sub m_OutForm_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseDown Button, Shift, x, y
End Sub

Private Sub m_OutForm_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseMove Button, Shift, x, y
End Sub

Private Sub m_OutForm_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseUp Button, Shift, x, y
End Sub

Private Sub m_OutForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    If m_Hooked Is Nothing Then Exit Sub
    If UnloadMode = 0 Then Cancel = True: m_Hooked.GFXEngine_Closed
End Sub

Private Sub m_OutForm_Resize()
On Error Resume Next
    m_OutForm.picBackbuffer.Move 0, 0, m_OutForm.ScaleWidth, m_OutForm.ScaleHeight
    If m_OutForm.WindowState = 2 Then m_OutForm.WindowState = 0: GoFullscreen: Exit Sub
    Flip
    m_Hooked.GFXEngine_Resize
End Sub

Private Sub m_OutPic_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseDown Button, Shift, x, y
End Sub

Private Sub m_OutPic_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseMove Button, Shift, x, y
End Sub

Private Sub m_OutPic_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseUp Button, Shift, x, y
End Sub

Private Sub m_OutPic_Paint()
On Error Resume Next
    Flip
    m_Hooked.GFXEngine_Paint
End Sub
