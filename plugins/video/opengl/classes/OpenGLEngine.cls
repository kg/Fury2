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
Dim WithEvents m_OutForm As Form
Attribute m_OutForm.VB_VarHelpID = -1
Dim m_Closed As Boolean
Public Event DblClick()
Public Event MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event KeyDown(KeyCode As Integer, Shift As Integer)
Public Event KeyUp(KeyCode As Integer, Shift As Integer)
Public Event Closed()
Public Event Resize()
Public Event Paint()
Private m_Hooked As Object
Private m_lngWidth As Long, m_lngHeight As Long
Public ScaleRatio As Single
Public Backbuffer As Fury2Image

Private Sub SetFormSize(ByRef Frm As Form, X As Long, Y As Long, Optional ByVal Center As Boolean = False)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    If Center Then
        Frm.Move (Screen.Width - (CLng(X * Screen.TwipsPerPixelX) + (BW * 2))) \ 2, (Screen.Height - (CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2))) \ 2, CLng(X * Screen.TwipsPerPixelX) + (BW * 2), CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    Else
        Frm.Move Frm.Left, Frm.Top, CLng(X * Screen.TwipsPerPixelX) + (BW * 2), CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    End If
    Frm.ScaleMode = OldMode
End Sub

Public Function CreateImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    Set CreateImage = F2Image(Width, Height)
'    Set CreateImage = CreateContext(m_OutForm.hDC, Width, Height)
End Function

Private Sub InitBackbuffer()
On Error Resume Next
    GLSetOutputSize m_OutForm.ScaleWidth, m_OutForm.ScaleHeight
    GLInstallAllocateHook
    If Backbuffer Is Nothing Then
        Set Backbuffer = F2Image(m_lngWidth, m_lngHeight)
    Else
        Backbuffer.Unsize
        Backbuffer.Resize m_lngWidth, m_lngHeight
    End If
    GLUninstallAllocateHook
    SetImageLocked Backbuffer.Handle, 1
    Backbuffer.Clear
'    Set Backbuffer = CreateContext(m_OutForm.HDC, m_lngWidth, m_lngHeight)
End Sub

Private Sub DestroyBackbuffer()
On Error Resume Next
    Set Backbuffer = Nothing
End Sub

Public Sub SetCaption(Text As String)
    If Fullscreen Then
        If m_OutForm.Caption <> "" Then
            m_OutForm.Caption = ""
            SetWindowText m_OutForm.HWND, Text
        End If
    Else
        If m_OutForm.Caption <> Text Then m_OutForm.Caption = Text
    End If
End Sub

Public Property Get SupportsGamma() As Boolean
    SupportsGamma = False
End Property

Public Property Let Fullscreen(NewValue As Boolean)
End Property

Public Property Get Fullscreen() As Boolean
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
    Unload m_OutForm
End Sub

Public Sub ResizeWindow(ByVal XSize As Long, ByVal YSize As Long)
On Error Resume Next
    SetFormSize m_OutForm, XSize * ScaleRatio, YSize * ScaleRatio, True
    Reallocate
End Sub

Public Sub Init(ByVal XRes As Long, ByVal YRes As Long, ByVal BPP As Long, ByVal Caption As String, Optional ByVal Windowed As Boolean = False, Optional ByVal ScreenX = 0, Optional ByVal ScreenY = 0, Optional ByVal Window As Long = 0)
On Error Resume Next
'    If F2Initialized = False Then
'        Exit Sub
'    End If
    Set m_OutForm = New frmOpenGL
    Load m_OutForm
    GLInit m_OutForm.HWND, m_OutForm.HDC
    m_OutForm.Caption = Caption
    #If UseWindowBuffer Then
        m_OutForm.AutoRedraw = True
    #Else
        m_OutForm.AutoRedraw = False
    #End If
    m_lngWidth = XRes
    m_lngHeight = YRes
    SetFormSize m_OutForm, XRes * ScaleRatio, YRes * ScaleRatio
    m_OutForm.Show
    Reallocate
End Sub

Public Sub Flip()
On Error Resume Next
    If m_Closed Then Exit Sub
    GLFlip ' m_OutForm.HDC
End Sub

Public Sub GoFullscreen()
On Error Resume Next
End Sub

Public Sub GoWindowed()
On Error Resume Next
End Sub

Public Sub Free()
    DestroyBackbuffer
End Sub

Public Sub Reallocate()
    InitBackbuffer
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    SetImageTag 0, 0, 0
    F2Init
    ScaleRatio = 1
    'InitOpenGLOverride
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    GLShutdown
End Sub

Private Sub m_OutForm_DblClick()
On Error Resume Next
    m_Hooked.GFXEngine_DblClick
End Sub

Private Sub m_OutForm_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyF4 And Shift = vbAltMask Then
        m_OutForm.Hide
        m_Closed = True
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

Private Sub m_OutForm_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseDown Button, Shift, X, Y
End Sub

Private Sub m_OutForm_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseMove Button, Shift, X, Y
End Sub

Private Sub m_OutForm_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_Hooked.GFXEngine_MouseUp Button, Shift, X, Y
End Sub

Private Sub m_OutForm_Paint()
On Error Resume Next
    m_Hooked.GFXEngine_Paint
End Sub

Private Sub m_OutForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    If UnloadMode = 1 Or UnloadMode = 5 Then
    Else
        GLShutdown
        Shutdown
        m_Hooked.GFXEngine_Closed
        m_Closed = True
    End If
End Sub

Private Sub m_OutForm_Resize()
On Error Resume Next
Static l_lngState As Long
    If (l_lngState <> m_OutForm.WindowState) Or (m_OutForm.ScaleWidth <> (m_lngWidth * ScaleRatio)) Or (m_OutForm.ScaleHeight <> (m_lngHeight * ScaleRatio)) Then
        l_lngState = m_OutForm.WindowState
        Reallocate
        GLFlip ' m_OutForm.HDC
    End If
    m_Hooked.GFXEngine_Resize
End Sub

Public Property Get MouseVisible() As Boolean
On Error Resume Next
    MouseVisible = m_OutForm.MousePointer = 0
End Property

Public Property Let MouseVisible(ByVal NewValue As Boolean)
On Error Resume Next
    m_OutForm.MousePointer = IIf(NewValue, 0, 99)
End Property

