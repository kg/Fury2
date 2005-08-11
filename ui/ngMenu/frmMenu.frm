VERSION 5.00
Begin VB.Form frmMenu 
   BorderStyle     =   3  'Fixed Dialog
   ClientHeight    =   915
   ClientLeft      =   -4050
   ClientTop       =   -4050
   ClientWidth     =   915
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   9
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmMenu.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   61
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   61
   ShowInTaskbar   =   0   'False
   Visible         =   0   'False
   Begin VB.Timer tmrFocusTracker 
      Interval        =   100
      Left            =   420
      Top             =   0
   End
   Begin VB.Timer tmrMouseTracker 
      Interval        =   100
      Left            =   0
      Top             =   0
   End
End
Attribute VB_Name = "frmMenu"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements ngMenuHost
Private Declare Function GetFocus Lib "user32" () As Long
Private Declare Function GetForegroundWindow Lib "user32" () As Long
Private Declare Function InvalidateRect Lib "user32" (ByVal hwnd As Long, lpRect As Rect, ByVal bErase As Long) As Long
Private Declare Function UpdateWindow Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal wNewLong As Long) As Long
Private Const GWL_HWNDPARENT As Long = (-8)

Public HasFocus As Boolean
Public ChildMenu As ngMenu
Public Menu As ngMenu
Public BorderWidth As Long
Private m_lngDC As Long
Private m_lngOldParent As Long
Private m_lngNullBitmap As Long
Private m_booMouseOver As Boolean
Private m_imgSurface As Fury2Image

Public Sub Release()
On Error Resume Next
    Debug.Print "frmMenu.Release"
    FreeSurface
    Set Menu = Nothing
    Set ChildMenu = Nothing
    Set m_imgSurface = Nothing
End Sub

Public Sub SetParent(ByVal Parent As Long)
On Error Resume Next
    m_lngOldParent = SetWindowLong(Me.hwnd, GWL_HWNDPARENT, Parent)
End Sub

Private Sub MouseEntered()
On Error Resume Next
    If m_booMouseOver Then Exit Sub
    tmrMouseTracker.Interval = 1
End Sub

Private Sub InitSurface()
On Error Resume Next
    m_lngDC = CreateCompatibleDC(Me.hDC)
    Set m_imgSurface = F2DIBSection(Me.ScaleWidth, Me.ScaleHeight, m_lngDC)
    m_lngNullBitmap = SelectObject(m_lngDC, m_imgSurface.DIBHandle)
End Sub

Private Sub ResizeSurface()
On Error Resume Next
    If m_imgSurface Is Nothing Then Exit Sub
    FreeSurface
    InitSurface
End Sub

Private Sub FreeSurface()
On Error Resume Next
    If m_lngDC = 0 Then Exit Sub
    SelectObject m_lngDC, m_lngNullBitmap
    Set m_imgSurface = Nothing
    DeleteDC m_lngDC
    m_lngDC = 0
End Sub

Public Sub Reflow()
On Error Resume Next
    Menu.Reflow
End Sub

Private Property Get ColorOffset(ByVal State As ngMenuItemStates) As Long
On Error Resume Next
    ColorOffset = (State - ngMenuItemStates.mstNormal) * (ngMenuColors.mncGlow - ngMenuColors.mncBackground + 1)
End Property

Friend Sub RenderItem(ByVal Item As ngMenuItem)
On Error Resume Next
Dim l_rctClip As Fury2Rect
Dim l_rctItem As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_lngOrientation As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect, l_rctAccelerator As Rect
    Set l_rctClip = m_imgSurface.ClipRectangle
    With Item
        Select Case .Style
        Case msyNormal, msyCheck, msyGroup
            Set l_rctItem = .Rectangle
            l_rctItem.Width = Me.ScaleWidth - l_rctItem.Left
            l_lngOffset = ColorOffset(.State)
            l_rctItem.Adjust -Menu.Metrics(mnmItemMargin), -Menu.Metrics(mnmItemMargin)
            If Not l_rctClip.Intersect(l_rctItem) Then Exit Sub
            SetTextColor m_lngDC, SwapChannels(SetAlpha(Menu.Colors(mncText + l_lngOffset), 0), Red, Blue)
            SetBackgroundMode m_lngDC, BackgroundMode_Transparent
            If .Image Is Nothing Then
                l_lngImageWidth = .ImageColumnWidth
                l_lngImageHeight = 0
            Else
                l_lngImageWidth = ClipValue(.Image.Width, .ImageColumnWidth, 999999)
                l_lngImageHeight = .Image.Height
            End If
            Set l_rctImage = l_rctItem.Copy
            l_rctImage.Width = l_lngImageWidth
            l_rctImage.Height = l_lngImageHeight
            l_rctImage.RelLeft = (l_rctItem.Left) + (.ImageColumnWidth - l_lngImageWidth) \ 2
            l_rctImage.RelTop = (l_rctItem.Top) + (l_rctItem.Height - l_lngImageHeight) \ 2
            l_rctText.Left = l_rctItem.Left + Menu.Metrics(mnmTextMargin)
            l_rctText.Top = l_rctItem.Top + Menu.Metrics(mnmTextMargin)
            l_rctText.Right = l_rctItem.Left + l_rctItem.Width - Menu.Metrics(mnmTextMargin)
            l_rctText.Bottom = l_rctItem.Top + l_rctItem.Height - Menu.Metrics(mnmTextMargin)
            Select Case .TextAlignment
            Case mtaLeft
                l_dtfFlags = DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                l_rctText.Left = l_rctItem.Left + l_lngImageWidth + Menu.Metrics(mnmTextMargin)
                l_rctImage.RelLeft = l_rctItem.Left - Menu.Metrics(mnmImageMargin)
            Case mtaRight
                l_dtfFlags = DrawText_Align_Right Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                l_rctText.Right = l_rctItem.Left + l_rctItem.Width - l_lngImageWidth - Menu.Metrics(mnmTextMargin)
                l_rctImage.RelLeft = l_rctItem.Right - l_lngImageWidth + Menu.Metrics(mnmImageMargin)
            End Select
            If (.Font Is Nothing) Then
            Else
                Set Font = .Font
            End If
            SelectObject m_lngDC, GetCurrentObject(Me.hDC, Object_Font)
            If .Enabled Then
                DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
                l_rctAccelerator = l_rctText
                l_rctAccelerator.Left = l_rctAccelerator.Left + Me.TextWidth(.DisplayText) + 8
                l_rctAccelerator.Right = l_rctAccelerator.Right - (GetTextWidth("4", g_fntMarlett) + 2)
                DrawText m_lngDC, .Accelerator, Len(.Accelerator), l_rctAccelerator, (l_dtfFlags And (Not DrawText_Align_Left)) Or DrawText_Align_Right
                If .ChildMenu Is Nothing Then
                Else
                    Set Font = g_fntMarlett
                    SelectObject m_lngDC, GetCurrentObject(Me.hDC, Object_Font)
                    l_rctText.Left = l_rctText.Right - GetTextWidth("4", g_fntMarlett)
                    DrawText m_lngDC, "4", 1, l_rctText, DrawText_Align_Center_Horizontal Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                End If
            Else
                With l_rctText
                    .Left = .Left + 1
                    .Top = .Top + 1
                    .Right = .Right + 1
                    .Bottom = .Bottom + 1
                End With
                l_rctAccelerator = l_rctText
                l_rctAccelerator.Left = l_rctAccelerator.Left + Me.TextWidth(.DisplayText) + 8
                l_rctAccelerator.Right = l_rctAccelerator.Right - (GetTextWidth("4", g_fntMarlett) + 2)
                SetTextColor m_lngDC, GetSystemColor(SystemColor_Button_Highlight)
                DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
                DrawText m_lngDC, .Accelerator, Len(.Accelerator), l_rctAccelerator, (l_dtfFlags And (Not DrawText_Align_Left)) Or DrawText_Align_Right
                With l_rctText
                    .Left = .Left - 1
                    .Top = .Top - 1
                    .Right = .Right - 1
                    .Bottom = .Bottom - 1
                End With
                l_rctAccelerator = l_rctText
                l_rctAccelerator.Left = l_rctAccelerator.Left + Me.TextWidth(.DisplayText) + 8
                l_rctAccelerator.Right = l_rctAccelerator.Right - (GetTextWidth("4", g_fntMarlett) + 2)
                SetTextColor m_lngDC, GetSystemColor(SystemColor_Button_Shadow)
                DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
                DrawText m_lngDC, .Accelerator, Len(.Accelerator), l_rctAccelerator, (l_dtfFlags And (Not DrawText_Align_Left)) Or DrawText_Align_Right
                If .ChildMenu Is Nothing Then
                Else
                    Set Font = g_fntMarlett
                    SelectObject m_lngDC, GetCurrentObject(Me.hDC, Object_Font)
                    l_rctText.Left = l_rctText.Right - GetTextWidth("4", g_fntMarlett)
                    With l_rctText
                        .Left = .Left + 1
                        .Top = .Top + 1
                        .Right = .Right + 1
                        .Bottom = .Bottom + 1
                    End With
                    SetTextColor m_lngDC, GetSystemColor(SystemColor_Button_Highlight)
                    DrawText m_lngDC, "4", 1, l_rctText, DrawText_Align_Center_Horizontal Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                    With l_rctText
                        .Left = .Left - 1
                        .Top = .Top - 1
                        .Right = .Right - 1
                        .Bottom = .Bottom - 1
                    End With
                    SetTextColor m_lngDC, GetSystemColor(SystemColor_Button_Shadow)
                    DrawText m_lngDC, "4", 1, l_rctText, DrawText_Align_Center_Horizontal Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                End If
            End If
            If GetAlpha(Menu.Colors(mncGlow + l_lngOffset)) > 0 Then
                m_imgSurface.Blit l_rctImage.Copy.Adjust(2, 2), , .GlowImage, IIf(.State = bstChecked, 1, 0.66), BlitMode_Font_SourceAlpha, Menu.Colors(mncGlow + l_lngOffset)
            End If
            m_imgSurface.Blit l_rctImage, , .Image, , BlitMode_SourceAlpha_ColorMask, Menu.Colors(mncTint + l_lngOffset)
        Case msySeparator
            Set l_rctItem = F2Rect(.Left, .Top, m_imgSurface.Width, .Height, False)
            If Not l_rctClip.Intersect(l_rctItem) Then Exit Sub
'            m_imgSurface.Fill l_rctItem, Colors(mncBackground + l_lngOffset), RenderMode_Normal
            l_rctItem.Adjust -Menu.Metrics(mnmSeparatorMargin), -Menu.Metrics(mnmSeparatorMargin)
            m_imgSurface.Fill l_rctItem, Menu.Colors(mncSeparator), RenderMode_SourceAlpha
        End Select
    End With
    Set m_imgSurface.ClipRectangle = l_rctClip
End Sub

Friend Sub RenderItemBackground(ByVal Item As ngMenuItem)
On Error Resume Next
Dim l_rctClip As Fury2Rect
Dim l_rctItem As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
    Set l_rctClip = m_imgSurface.ClipRectangle
    With Item
        Select Case .Style
        Case bsyNormal, bsyCheck, bsyGroup
            Set l_rctItem = .Rectangle
            l_rctItem.Width = Me.ScaleWidth
            If Not l_rctClip.Intersect(l_rctItem) Then Exit Sub
            l_lngOffset = ColorOffset(.State)
            If (.State = bstNormal) Then
            Else
                m_imgSurface.Fill l_rctItem, Menu.Colors(mncBackground + l_lngOffset), RenderMode_SourceAlpha
                m_imgSurface.Box l_rctItem, Menu.Colors(mncBorder + l_lngOffset), RenderMode_SourceAlpha
            End If
        Case bsySeparator
        End Select
    End With
    Set m_imgSurface.ClipRectangle = l_rctClip
End Sub

Friend Sub DrawSegment(ByVal x As Long, ByVal y As Long, ByRef Image As Fury2Image, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal BlitMode As SFXBlitModes = BlitMode_SourceAlpha, Optional ByVal Color As Long = -1, Optional ByVal Alpha As Single = 1, Optional ByVal Vertical As Boolean = False)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_rctStrip As Fury2Rect, l_rctDest As Fury2Rect
Dim l_rctClip As Fury2Rect
    If Width = -1 Then
        Width = m_imgSurface.Width
    End If
    If Height = -1 Then
        Height = m_imgSurface.Height
    End If
    With m_imgSurface
        If Vertical Then
            l_lngY = y
            Do While (l_lngY < (y + Height))
                Set l_rctClip = .ClipRectangle
                .Blit F2Rect(0, l_lngY, 11, Image.Height, False), , Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(x + 11, l_lngY, .Width, l_lngY + Image.Height)
                .Blit F2Rect(.Width - 12, l_lngY, 12, Image.Height, False), F2Rect(12, 0, 10, Image.Height, False), Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(x, l_lngY, .Width - 12, l_lngY + Image.Height)
                Set l_rctDest = F2Rect(0, l_lngY, 1, Image.Height, False)
                Set l_rctStrip = F2Rect(11, 0, 1, Image.Height, False)
                For l_lngX = 0 To .Width
                    l_rctDest.RelLeft = l_lngX
                    .Blit l_rctDest, l_rctStrip, Image, Alpha, BlitMode, Color
                Next l_lngX
                Set .ClipRectangle = l_rctClip
                l_lngY = l_lngY + Image.Height
            Loop
        Else
            l_lngX = x
            Do While (l_lngX < (x + Width))
                Set l_rctClip = .ClipRectangle
                .Blit F2Rect(l_lngX, y, Image.Width, 13, False), , Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(l_lngX, y + 13, l_lngX + Image.Width, y + Height)
                .Blit F2Rect(l_lngX, y + Height - 10, Image.Width, 10, False), F2Rect(0, 14, Image.Width, 10, False), Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(l_lngX, y, l_lngX + Image.Width, y + Height - 10)
                Set l_rctDest = F2Rect(l_lngX, y, Image.Width, 1, False)
                Set l_rctStrip = F2Rect(0, 13, Image.Width, 1, False)
                For l_lngY = y To .Height
                    l_rctDest.RelTop = l_lngY
                    .Blit l_rctDest, l_rctStrip, Image, Alpha, BlitMode, Color
                Next l_lngY
                Set .ClipRectangle = l_rctClip
                l_lngX = l_lngX + Image.Width
            Loop
        End If
    End With
End Sub

Public Sub Redraw(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
Dim l_miItem As ngMenuItem
Dim l_fntOld As StdFont
Dim l_lngColor As Long
Dim l_rctArea As Rect
    If Menu.DisableUpdates Then Exit Sub
    If Area Is Nothing Then
        Set Area = m_imgSurface.Rectangle
    End If
    If m_imgSurface Is Nothing Then Exit Sub
    If Menu Is Nothing Then Exit Sub
    Set m_imgSurface.ClipRectangle = Area
    Set l_fntOld = Me.Font
    m_imgSurface.Clear Menu.Colors(mncBackground)
    For Each l_miItem In Menu.Items
        RenderItemBackground l_miItem
    Next l_miItem
    For Each l_miItem In Menu.Items
        Set Me.Font = l_fntOld
        RenderItem l_miItem
    Next l_miItem
    Set Me.Font = l_fntOld
    Repaint Area
End Sub

Public Sub Repaint(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If m_imgSurface Is Nothing Then Exit Sub
    If Area Is Nothing Then
        Set Area = m_imgSurface.Rectangle
    End If
    BitBlt Me.hDC, Area.Left, Area.Top, Area.Width, Area.Height, m_lngDC, Area.Left, Area.Top, vbSrcCopy
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    Menu.Event_KeyDown KeyCode, Shift
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
On Error Resume Next
    Menu.Event_KeyPress KeyAscii
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    Menu.Event_KeyUp KeyCode, Shift
End Sub

Private Sub Form_Load()
On Error Resume Next
    InitSurface
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    If m_lngOldParent <> 0 Then Call SetWindowLong(Me.hwnd, GWL_HWNDPARENT, m_lngOldParent)
End Sub

Private Property Get ngMenuHost_Left() As Long
On Error Resume Next
    ngMenuHost_Left = Me.Left / Screen.TwipsPerPixelX
End Property

Private Property Get ngMenuHost_Top() As Long
On Error Resume Next
    ngMenuHost_Top = Me.Top / Screen.TwipsPerPixelY
End Property

Private Sub tmrFocusTracker_Timer()
On Error Resume Next
Dim l_lngWindow As Long
    l_lngWindow = GetFocus
    HasFocus = l_lngWindow = Me.hwnd
End Sub

Private Sub tmrMouseTracker_Timer()
On Error Resume Next
Dim l_ptMouse As PointAPI
    GetCursorPos l_ptMouse
    ScreenToClient Me.hwnd, l_ptMouse
    Menu.SetMousePosition l_ptMouse.x, l_ptMouse.y
End Sub

Private Sub Form_Hide()
On Error Resume Next
    tmrMouseTracker.Enabled = False
    tmrFocusTracker.Enabled = False
    FreeSurface
End Sub

Private Sub Form_Initialize()
On Error Resume Next
    g_lngMenuHosts = g_lngMenuHosts + 1
    Me.ScaleMode = 1
    BorderWidth = ScaleX((Me.Width - Me.ScaleWidth) / 2, vbTwips, vbPixels)
    Me.ScaleMode = 3
    F2Init
    Debug.Print "frmMenu_Initiaize"
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    Menu.Event_MouseDown Button, Shift, x, y
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    Menu.Event_MouseMove Button, Shift, x, y
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    Menu.Event_MouseUp Button, Shift, x, y
End Sub

Private Sub Form_Paint()
On Error Resume Next
    Repaint
End Sub

Private Sub Form_Resize()
On Error Resume Next
    Reflow
    ResizeSurface
    Redraw
End Sub

Private Sub Form_Show()
On Error Resume Next
    InitSurface
    Reflow
End Sub

Private Sub Form_Terminate()
On Error Resume Next
    g_lngMenuHosts = g_lngMenuHosts - 1
    Set Menu = Nothing
    FreeSurface
    Debug.Print "frmMenu_Terminate"
End Sub

Private Property Get ngMenuHost_Height() As Long
    ngMenuHost_Height = Me.ScaleHeight
End Property

Private Property Get ngMenuHost_hwnd() As Long
    ngMenuHost_hwnd = Me.hwnd
End Property

Private Sub ngMenuHost_Redraw(Optional ByVal Area As libGraphics.Fury2Rect = Nothing)
    Redraw Area
End Sub

Private Sub ngMenuHost_SetSize(ByVal Width As Long, ByVal Height As Long)
    Me.Move Me.Left, Me.Top, Width * Screen.TwipsPerPixelX, Height * Screen.TwipsPerPixelY
End Sub

Private Property Get ngMenuHost_This() As Object
    Set ngMenuHost_This = Me
End Property

Private Property Get ngMenuHost_Width() As Long
    ngMenuHost_Width = Me.ScaleWidth
End Property
