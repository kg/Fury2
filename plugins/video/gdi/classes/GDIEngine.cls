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
Implements Fury2VideoDriver
Private WithEvents m_OutForm As Form
Attribute m_OutForm.VB_VarHelpID = -1
Private m_booClosed As Boolean
Private m_objHooked As Fury2VideoEventSink
Private m_lngDC As Long, m_lngBitmap As Long, m_lngOldBitmap As Long
Private m_lngWidth As Long, m_lngHeight As Long
Private m_sngScaleRatio As Single
Private m_imgBackbuffer As Fury2Image
Private m_lngScaleMode As Long

Private Property Let Fury2VideoDriver_ScaleMode(ByVal RHS As Long)
    m_lngScaleMode = RHS
End Property

Private Property Get Fury2VideoDriver_ScaleMode() As Long
    Fury2VideoDriver_ScaleMode = m_lngScaleMode
End Property

Public Property Get Feature(ByRef Name As String) As Boolean
On Error Resume Next
    Select Case LCase(Trim(Name))
    Case "glsl"
        Feature = False
    Case "multitexturing"
        Feature = False
    Case Else
        Feature = True
    End Select
End Property

Private Property Get Fury2VideoDriver_Backbuffer() As libGraphics.Fury2Image
    Set Fury2VideoDriver_Backbuffer = m_imgBackbuffer
End Property

Private Property Get Fury2VideoDriver_Closed() As Boolean
    Fury2VideoDriver_Closed = m_booClosed
End Property

Public Function Fury2VideoDriver_CreateImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    Set Fury2VideoDriver_CreateImage = F2Image(Width, Height)
End Function

Friend Sub InitDIBSection()
On Error Resume Next
    DestroyDIBSection
#If UseWindowBuffer Then
    m_lngDC = m_OutForm.hDC
#Else
    m_lngDC = CreateCompatibleDC(0)
#End If
    Set m_imgBackbuffer = F2DIBSection(m_lngWidth, m_lngHeight, m_lngDC)
    m_lngBitmap = m_imgBackbuffer.DIBHandle
    m_lngOldBitmap = SelectObject(m_lngDC, m_lngBitmap)
End Sub

Private Sub DestroyDIBSection()
On Error Resume Next
    SelectObject m_lngDC, m_lngOldBitmap
    Set m_imgBackbuffer = Nothing
    If m_lngDC <> 0 Then
#If UseWindowBuffer Then
#Else
        DeleteDC m_lngDC
#End If
        m_lngDC = 0
    End If
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

Public Sub Fury2VideoDriver_SetCaption(Text As String)
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

Public Property Get Fury2VideoDriver_Window() As Object
    Set Fury2VideoDriver_Window = m_OutForm
End Property

Private Property Get Fury2VideoDriver_Fullscreen() As Boolean
End Property

Public Sub Fury2VideoDriver_Shutdown()
On Error Resume Next
    m_booClosed = True
    Unload m_OutForm
End Sub

Public Sub Fury2VideoDriver_ResizeWindow(ByVal XSize As Long, ByVal YSize As Long)
On Error Resume Next
    SetFormSize m_OutForm, XSize * m_sngScaleRatio, YSize * m_sngScaleRatio, True
End Sub

Public Sub Fury2VideoDriver_Init(ByVal XRes As Long, ByVal YRes As Long, ByRef Caption As String, Optional ByVal Windowed As Boolean = False)
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
    m_booClosed = False
    SetFormSize m_OutForm, XRes * m_sngScaleRatio, YRes * m_sngScaleRatio
    m_OutForm.Show
    Reallocate
End Sub

Public Sub Fury2VideoDriver_Flip()
On Error Resume Next
    If m_booClosed Then Exit Sub
    GDI32.Flush
#If UseWindowBuffer Then
#Else
    If CLng(m_sngScaleRatio * 100) = 100 Then
        BitBlt m_OutForm.hDC, 0, 0, m_OutForm.ScaleWidth, m_OutForm.ScaleHeight, m_lngDC, 0, 0, vbSrcCopy
    Else
        StretchBlt m_OutForm.hDC, 0, 0, m_OutForm.ScaleWidth, m_OutForm.ScaleHeight, m_lngDC, 0, 0, m_lngWidth, m_lngHeight, vbSrcCopy
    End If
#End If
    If m_OutForm.AutoRedraw Then m_OutForm.Refresh
End Sub

Public Sub Fury2VideoDriver_GoFullscreen()
On Error Resume Next
End Sub

Public Sub Fury2VideoDriver_GoWindowed()
On Error Resume Next
End Sub

Friend Sub Free()
    DestroyDIBSection
End Sub

Friend Sub Reallocate()
    InitDIBSection
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    m_sngScaleRatio = 1
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

Private Sub m_OutForm_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_objHooked.MouseDown Button, Shift, X, Y
End Sub

Private Sub m_OutForm_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_objHooked.MouseMove Button, Shift, X, Y
End Sub

Private Sub m_OutForm_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_objHooked.MouseUp Button, Shift, X, Y
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
        Fury2VideoDriver_Shutdown
    End If
End Sub

Private Sub m_OutForm_Resize()
On Error Resume Next
    m_objHooked.Resize
End Sub

Public Property Get Fury2VideoDriver_MouseVisible() As Boolean
On Error Resume Next
    Fury2VideoDriver_MouseVisible = m_OutForm.MousePointer = 0
End Property

Public Property Let Fury2VideoDriver_MouseVisible(ByVal NewValue As Boolean)
On Error Resume Next
    m_OutForm.MousePointer = IIf(NewValue, 0, 99)
End Property
