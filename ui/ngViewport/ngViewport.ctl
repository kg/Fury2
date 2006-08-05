VERSION 5.00
Begin VB.UserControl ngViewport 
   Alignable       =   -1  'True
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   Begin VB.VScrollBar vsScroll 
      Enabled         =   0   'False
      Height          =   1140
      LargeChange     =   128
      Left            =   2280
      Max             =   100
      SmallChange     =   16
      TabIndex        =   1
      TabStop         =   0   'False
      Top             =   1230
      Width           =   240
   End
   Begin VB.HScrollBar hsScroll 
      Enabled         =   0   'False
      Height          =   240
      LargeChange     =   128
      Left            =   1830
      Max             =   100
      SmallChange     =   16
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   1680
      Width           =   1140
   End
End
Attribute VB_Name = "ngViewport"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Declare Function InvalidateRect Lib "user32" (ByVal hwnd As Long, lpRect As Rect, ByVal bErase As Long) As Long
Private Declare Function UpdateWindow Lib "user32" (ByVal hwnd As Long) As Long

Event Scroll()
Event MouseDown(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event Resize()
Event Redraw(ByVal Surface As Fury2Image, ByVal XOffset As Single, ByVal YOffset As Single)

Private m_imgSurface As Fury2Image

Public ZoomLevel As Single
Public VirtualWidth As Long, VirtualHeight As Long
Public MaxWidth As Long, MaxHeight As Long
Public MinWidth As Long, MinHeight As Long

Public BackgroundColor As Long
Public ScrollBackground As Boolean
Public BackgroundImage As Fury2Image

Public Handler As IViewportHandler

Public Property Get Buffer() As Fury2Image
On Error Resume Next
    Set Buffer = m_imgSurface
End Property

Private Sub UserControl_Hide()
On Error Resume Next
    Deallocate
End Sub

Public Sub SetVirtualSize(ByVal Width As Long, ByVal Height As Long)
On Error Resume Next
    If Width = VirtualWidth And Height = VirtualHeight Then Exit Sub
    VirtualWidth = Width
    VirtualHeight = Height
    Refresh
End Sub

Private Sub UserControl_Initialize()
On Error Resume Next
    F2Init
    BackgroundColor = F2FromGDIColor(GetSystemColor(SystemColor_Button_Face))
    ZoomLevel = 1
    MinWidth = 0
    MinHeight = 0
    MaxWidth = 4096
    MaxHeight = 4096
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Not (Handler Is Nothing) Then
        If Handler.MouseDown(Button, Shift, X, Y) Then Exit Sub
    End If
    RaiseEvent MouseDown(Button, Shift, (X * ZoomLevel) + ScrollX, (Y * ZoomLevel) + ScrollY)
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Not (Handler Is Nothing) Then
        If Handler.MouseMove(Button, Shift, X, Y) Then Exit Sub
    End If
    RaiseEvent MouseMove(Button, Shift, (X * ZoomLevel) + ScrollX, (Y * ZoomLevel) + ScrollY)
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Not (Handler Is Nothing) Then
        If Handler.MouseUp(Button, Shift, X, Y) Then Exit Sub
    End If
    RaiseEvent MouseUp(Button, Shift, (X * ZoomLevel) + ScrollX, (Y * ZoomLevel) + ScrollY)
End Sub

Private Sub UserControl_Paint()
On Error Resume Next
Dim l_rctSource As Fury2Rect, l_rctDest As Fury2Rect
Dim l_lngWidth As Long, l_lngHeight As Long
    l_lngWidth = ViewportWidth
    l_lngHeight = ViewportHeight
    Set l_rctSource = F2Rect(0, 0, l_lngWidth, l_lngHeight, False)
    Set l_rctDest = F2Rect(0, 0, l_lngWidth * ZoomLevel, l_lngHeight * ZoomLevel, False)
    DrawImageToDC UserControl.hDC, l_rctDest, l_rctSource, m_imgSurface
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
    hsScroll.Move 0, UserControl.ScaleHeight - hsScroll.Height, UserControl.ScaleWidth - vsScroll.Width, hsScroll.Height
    vsScroll.Move UserControl.ScaleWidth - vsScroll.Width, 0, vsScroll.Width, UserControl.ScaleHeight - hsScroll.Height
    Refresh
End Sub

Private Sub UserControl_Show()
On Error Resume Next
    Allocate
    Redraw
    Repaint
End Sub

Public Sub Repaint()
On Error Resume Next
'Dim l_rctWindow As Win32.Rect
'    l_rctWindow.Right = UserControl.ScaleWidth
'    l_rctWindow.Bottom = UserControl.ScaleHeight
'    InvalidateRect UserControl.hwnd, l_rctWindow, 0
    UserControl_Paint
End Sub

Public Sub Redraw()
On Error Resume Next
    m_imgSurface.Clear BackgroundColor
    If Not (BackgroundImage Is Nothing) Then
        If ScrollBackground Then
            m_imgSurface.Blit , F2Rect(ScrollX, ScrollY, m_imgSurface.Width, m_imgSurface.Height, False), BackgroundImage, , IIf(BackgroundImage.AlphaChannel, BlitMode_SourceAlpha, BlitMode_Normal)
        Else
            m_imgSurface.Blit , , BackgroundImage, , IIf(BackgroundImage.AlphaChannel, BlitMode_SourceAlpha, BlitMode_Normal)
        End If
    End If
    RaiseEvent Redraw(m_imgSurface, ScrollX, ScrollY)
End Sub

Public Sub Refresh()
On Error Resume Next
    Reallocate
    RefreshScrollbars
    Redraw
    Repaint
End Sub

Public Sub Reallocate()
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_booRealloc As Boolean
    l_lngWidth = ViewportWidth
    l_lngHeight = ViewportHeight
    If Not (m_imgSurface Is Nothing) Then
        If ((m_imgSurface.Width < l_lngWidth) Or (m_imgSurface.Height < l_lngHeight)) Then
            l_booRealloc = True
        ElseIf ((m_imgSurface.Width > (l_lngWidth + 16)) Or (m_imgSurface.Height > (l_lngHeight + 16))) Then
            l_booRealloc = True
        End If
    End If
    If (m_imgSurface Is Nothing) Then
        Set m_imgSurface = F2Image(l_lngWidth, l_lngHeight)
    ElseIf l_booRealloc Then
        m_imgSurface.Resize l_lngWidth, l_lngHeight, False
    End If
End Sub

Public Property Get ViewportWidth() As Long
On Error Resume Next
    ViewportWidth = ClipValue(Ceil((ScaleWidth - vsScroll.Width) / ZoomLevel), MinWidth, MaxWidth)
End Property

Public Property Get ViewportHeight() As Long
On Error Resume Next
    ViewportHeight = ClipValue(Ceil((ScaleHeight - hsScroll.Height) / ZoomLevel), MinHeight, MaxHeight)
End Property

Public Property Get ScrollX() As Long
On Error Resume Next
    ScrollX = hsScroll.Value
End Property

Public Property Get ScrollY() As Long
On Error Resume Next
    ScrollY = vsScroll.Value
End Property

Public Sub RefreshScrollbars()
On Error Resume Next
Dim l_lngMax As Long
    l_lngMax = VirtualWidth - ViewportWidth
    If (l_lngMax > 0) Then
        If hsScroll.Enabled = False Then hsScroll.Enabled = True
        If hsScroll.Max <> l_lngMax Then hsScroll.Max = l_lngMax
    Else
        If hsScroll.Value <> 0 Then hsScroll.Value = 0
        If hsScroll.Enabled = True Then hsScroll.Enabled = False
    End If
    l_lngMax = VirtualHeight - ViewportHeight
    If (l_lngMax > 0) Then
        If vsScroll.Enabled = False Then vsScroll.Enabled = True
        If vsScroll.Max <> l_lngMax Then vsScroll.Max = l_lngMax
    Else
        If vsScroll.Value <> 0 Then vsScroll.Value = 0
        If vsScroll.Enabled = True Then vsScroll.Enabled = False
    End If
End Sub

Public Sub Allocate()
On Error Resume Next
    Reallocate
End Sub

Public Sub Deallocate()
On Error Resume Next
    Set m_imgSurface = Nothing
End Sub

Public Sub Scrolled()
On Error Resume Next
    RaiseEvent Scroll
    Redraw
End Sub

Private Sub vsScroll_Change()
On Error Resume Next
    If vsScroll.Enabled Then Scrolled
End Sub

Private Sub vsScroll_Scroll()
On Error Resume Next
    If vsScroll.Enabled Then Scrolled
End Sub

Private Sub hsScroll_Change()
On Error Resume Next
    If hsScroll.Enabled Then Scrolled
End Sub

Private Sub hsScroll_Scroll()
On Error Resume Next
    If hsScroll.Enabled Then Scrolled
End Sub
