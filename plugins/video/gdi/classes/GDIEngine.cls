VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "GDIEngine"
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
Private m_lngDC As Long, m_lngBitmap As Long, m_lngOldBitmap As Long
Private m_lngWidth As Long, m_lngHeight As Long
Public ScaleRatio As Single
Public Backbuffer As Fury2Image
Public VSync As Boolean

Public Function CreateImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    Set CreateImage = F2Image(Width, Height)
End Function

Private Sub InitDIBSection()
On Error Resume Next
    DestroyDIBSection
#If UseWindowBuffer Then
    m_lngDC = m_OutForm.hDC
#Else
    m_lngDC = CreateCompatibleDC(0)
#End If
    Set Backbuffer = F2DIBSection(m_lngWidth, m_lngHeight, m_lngDC)
    m_lngBitmap = Backbuffer.DIBHandle
    m_lngOldBitmap = SelectObject(m_lngDC, m_lngBitmap)
End Sub

Private Sub DestroyDIBSection()
On Error Resume Next
    SelectObject m_lngDC, m_lngOldBitmap
    Set Backbuffer = Nothing
    If m_lngDC <> 0 Then
#If UseWindowBuffer Then
#Else
        DeleteDC m_lngDC
#End If
        m_lngDC = 0
    End If
End Sub

Public Sub SetCaption(Text As String)
    If Fullscreen Then
        If m_OutForm.Caption <> "" Then
            m_OutForm.Caption = ""
            SetWindowText m_OutForm.hwnd, Text
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
End Sub

Public Sub Init(ByVal XRes As Long, ByVal YRes As Long, ByVal BPP As Long, ByVal Caption As String, Optional ByVal Windowed As Boolean = False, Optional ByVal ScreenX = 0, Optional ByVal ScreenY = 0, Optional ByVal Window As Long = 0)
On Error Resume Next
    If Not F2Initialized Then Exit Sub
    Set m_OutForm = New frmGDI
    Load m_OutForm
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
    GDI32.Flush
#If UseWindowBuffer Then
#Else
    If CLng(ScaleRatio * 100) = 100 Then
        BitBlt m_OutForm.hDC, 0, 0, m_OutForm.ScaleWidth, m_OutForm.ScaleHeight, m_lngDC, 0, 0, vbSrcCopy
    Else
        StretchBlt m_OutForm.hDC, 0, 0, m_OutForm.ScaleWidth, m_OutForm.ScaleHeight, m_lngDC, 0, 0, m_lngWidth, m_lngHeight, vbSrcCopy
    End If
#End If
    If m_OutForm.AutoRedraw Then m_OutForm.Refresh
End Sub

Public Sub GoFullscreen()
On Error Resume Next
End Sub

Public Sub GoWindowed()
On Error Resume Next
End Sub

Public Sub Free()
    DestroyDIBSection
End Sub

Public Sub Reallocate()
    InitDIBSection
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    ScaleRatio = 1
End Sub

Private Sub m_OutForm_DblClick()
On Error Resume Next
    m_Hooked.GFXEngine_DblClick
End Sub

Private Sub m_OutForm_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyF4 And Shift = vbAltMask Then
        Shutdown
        m_Hooked.GFXEngine_Closed
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
        Shutdown
        m_Hooked.GFXEngine_Closed
        m_Closed = True
    End If
End Sub

Private Sub m_OutForm_Resize()
On Error Resume Next
    m_Hooked.GFXEngine_Resize
End Sub
