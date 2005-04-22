VERSION 5.00
Begin VB.UserControl ngTabStrip 
   Alignable       =   -1  'True
   ClientHeight    =   465
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   2430
   ControlContainer=   -1  'True
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ScaleHeight     =   31
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   162
   Begin VB.Timer tmrEdgeScroll 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   1050
      Top             =   0
   End
   Begin VB.Timer tmrMouseTracker 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   615
      Top             =   0
   End
   Begin VB.Image imgCurrentTab 
      Height          =   240
      Left            =   30
      ToolTipText     =   "Tooltip"
      Top             =   30
      Visible         =   0   'False
      Width           =   240
   End
End
Attribute VB_Name = "ngTabStrip"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Declare Function InvalidateRect Lib "user32" (ByVal hwnd As Long, lpRect As Rect, ByVal bErase As Long) As Long
Private Declare Function UpdateWindow Lib "user32" (ByVal hwnd As Long) As Long

Event MouseDown(ByRef Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event TabClick(TheTab As ngTab)
Event TabClose(TheTab As ngTab)
Event TabSelected(TheTab As ngTab)
Event Reflow()
Event Resize()
Event Redraw()
Private m_imgShadow(0 To 2) As Fury2Image
Private m_imgOutline(0 To 3) As Fury2Image
Private m_imgMask(0 To 3) As Fury2Image
Private m_imgHighlight As Fury2Image
Private m_imgClose As Fury2Image
Private m_lngMetrics(0 To tsm_max) As Long
Private m_lngColors(0 To tsc_max) As Long
Private m_tscTabs As ngTabs
Private m_imgSurface As Fury2Image, m_lngDC As Long, m_lngNullBitmap As Long
Private m_tabHover As ngTab, m_tabPressed As ngTab
Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngIdealWidth As Long, m_lngIdealHeight As Long
Private m_lngRowHeight As Long
Private m_booInitialized As Boolean
Private m_booMouseOver As Boolean
Private m_tabSelected As ngTab
Private m_lngScrollDirection As Long
Private m_lngScrollOffset As Long
Public ThemeBackground As Variant
Public EnableTheme As Boolean
Public ResourceFile As ngResourceFile
Public ResourcePattern As String
Public DisableUpdates As Boolean
Public ShowCloseButtons As Boolean

Friend Property Get CloseButtonWidth() As Long
    CloseButtonWidth = m_imgClose.Width + 4
End Property

Public Property Get ScaleWidth() As Long
    ScaleWidth = UserControl.ScaleWidth
End Property

Public Property Get ScaleHeight() As Long
    ScaleHeight = UserControl.ScaleHeight
End Property

Private Function LoadThemeImage(ByRef Path As String, ByRef Filename As String, Optional ByRef Failed As Boolean) As Fury2Image
On Error Resume Next
    Set LoadThemeImage = ResourceFile.ItemData(Path & Filename & ".png")
    If LoadThemeImage Is Nothing Then Failed = True
    Err.Clear
End Function

Public Sub LoadTheme(ByRef Path As String)
On Error Resume Next
Dim l_lngIndex As Long, l_lngOrientation As Long, l_strOrientation As String
Dim l_booFailed As Boolean
    EnableTheme = False
    For l_lngIndex = 0 To 2
        Set m_imgShadow(l_lngIndex) = LoadThemeImage(Path, "shadow_" & (l_lngIndex + 1), l_booFailed)
        Set m_imgMask(l_lngIndex) = LoadThemeImage(Path, "mask_" & (l_lngIndex + 1), l_booFailed)
        Set m_imgOutline(l_lngIndex) = LoadThemeImage(Path, "outline_" & (l_lngIndex + 1), l_booFailed)
    Next l_lngIndex
    Set m_imgHighlight = LoadThemeImage(Path, "highlight", l_booFailed)
    Set m_imgClose = LoadThemeImage(Path, "close", l_booFailed)
    EnableTheme = Not l_booFailed
End Sub

Public Property Get Font() As StdFont
    Set Font = UserControl.Font
End Property

Public Property Set Font(ByRef NewFont As StdFont)
On Error Resume Next
    Set UserControl.Font = NewFont
End Property

Private Sub MouseEntered()
On Error Resume Next
    If m_booMouseOver Then Exit Sub
    tmrMouseTracker.Interval = 1
End Sub

Public Property Get hwnd() As Long
On Error Resume Next
    hwnd = UserControl.hwnd
End Property

Public Property Get hDC() As Long
On Error Resume Next
    hDC = UserControl.hDC
End Property

Friend Property Get TextHeight(ByVal TheFont As StdFont, ByRef Text As String) As Long
On Error Resume Next
Dim l_fntOld As StdFont
    Set l_fntOld = UserControl.Font
    Set UserControl.Font = TheFont
    TextHeight = UserControl.TextHeight(Replace(Text, "&", ""))
    Set UserControl.Font = l_fntOld
End Property

Friend Property Get TextWidth(ByVal TheFont As StdFont, ByRef Text As String) As Long
On Error Resume Next
Dim l_fntOld As StdFont
    Set l_fntOld = UserControl.Font
    Set UserControl.Font = TheFont
    TextWidth = UserControl.TextWidth(Replace(Text, "&", ""))
    Set UserControl.Font = l_fntOld
End Property

Public Property Get Tabs() As ngTabs
    Set Tabs = m_tscTabs
End Property

Public Property Get Metrics(ByVal Index As ngTabStripMetrics) As Long
    Metrics = m_lngMetrics(Index)
End Property

Public Property Let Metrics(ByVal Index As ngTabStripMetrics, ByVal NewValue As Long)
    m_lngMetrics(Index) = NewValue
End Property

Public Property Get Colors(ByVal Index As ngTabStripColors) As Long
    Colors = m_lngColors(Index)
End Property

Public Property Let Colors(ByVal Index As ngTabStripColors, ByVal NewColor As Long)
    m_lngColors(Index) = NewColor
End Property

Private Property Get ColorOffset(ByVal State As ngTabStates) As Long
On Error Resume Next
    ColorOffset = (State - ngTabStates.tstNormal) * (ngTabStripColors.tscBorder - ngTabStripColors.tscBackground + 1)
End Property

Private Sub InitMetrics()
On Error Resume Next
    Metrics(tsmTabHorizontalMargin) = 2
    Metrics(tsmTabVerticalMargin) = 2
    Metrics(tsmImageMargin) = 2
    Metrics(tsmTextMargin) = 3
End Sub

Private Sub InitColors()
On Error Resume Next
Dim l_lngParent As Long, l_lngParentDC As Long, l_lngColor As Long
    l_lngParent = GetParent(UserControl.hwnd)
    l_lngParentDC = GetDC(l_lngParent)
    l_lngColor = GetBackgroundColor(l_lngParentDC)
    ReleaseDC l_lngParent, l_lngParentDC
    UserControl.BackColor = l_lngColor
    Colors(tscBackground) = SetAlpha(SwapChannels(l_lngColor, Red, Blue), 255)
    Colors(tscBorder) = ConvertSystemColor(SystemColor_Button_Shadow)
    Colors(tscText) = ConvertSystemColor(SystemColor_Button_Text)
    Colors(tscFill) = ConvertSystemColor(SystemColor_Button_Face)
    Colors(tscHighlight) = Colors(tscBackground)
    Colors(tscHighlightBorder) = ConvertSystemColor(SystemColor_Button_Shadow)
    Colors(tscHighlightText) = ConvertSystemColor(SystemColor_Button_Text)
    Colors(tscHighlightFill) = BlendColors(ConvertSystemColor(SystemColor_Button_Highlight), ConvertSystemColor(SystemColor_Button_Face), 127)
    Colors(tscSelected) = Colors(tscBackground)
    Colors(tscSelectedBorder) = ConvertSystemColor(SystemColor_Button_Shadow)
    Colors(tscSelectedText) = ConvertSystemColor(SystemColor_Button_Text)
    Colors(tscSelectedFill) = ConvertSystemColor(SystemColor_Button_Highlight)
    ' TODO: Initialize other colors
End Sub

Private Sub InitTabs()
On Error Resume Next
    Set m_tscTabs = New ngTabs
    m_tscTabs.SetParent Me
End Sub

Private Sub FreeTabs()
On Error Resume Next
    m_tscTabs.Free
    Set m_tscTabs = Nothing
End Sub

Private Sub InitSurface()
On Error Resume Next
    If UserControl.Ambient.UserMode Then
        m_lngDC = CreateCompatibleDC(UserControl.hDC)
        Set m_imgSurface = F2DIBSection(UserControl.ScaleWidth, ClipValue(UserControl.ScaleHeight, 0, IdealHeight), m_lngDC)
        m_lngNullBitmap = SelectObject(m_lngDC, m_imgSurface.DIBHandle)
    End If
End Sub

Private Sub ResizeSurface()
On Error Resume Next
    If m_imgSurface Is Nothing Then Exit Sub
    FreeSurface
    InitSurface
'    SelectObject m_lngDC, m_lngNullBitmap
'    m_imgSurface.Resize UserControl.ScaleWidth, UserControl.ScaleHeight
'    m_lngNullBitmap = SelectObject(m_lngDC, m_imgSurface.DIBHandle)
End Sub

Private Sub FreeSurface()
On Error Resume Next
    SelectObject m_lngDC, m_lngNullBitmap
    Set m_imgSurface = Nothing
    DeleteDC m_lngDC
    m_lngDC = 0
End Sub

Public Property Get IdealWidth() As Long
On Error Resume Next
    IdealWidth = m_lngIdealWidth
End Property

Public Property Get IdealHeight(Optional ByVal Width As Long = -1) As Long
On Error Resume Next
    IdealHeight = m_lngIdealHeight
    If IdealHeight < 4 Then IdealHeight = 4
End Property

Public Sub Reflow()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_tabTab As ngTab
Dim l_lngIndex As Long, l_lngRowStart As Long, l_lngTab As Long
    If Not UserControl.Ambient.UserMode Then Exit Sub
    m_lngIdealWidth = 0
    m_lngIdealHeight = 0
    For Each l_tabTab In m_tscTabs
        With l_tabTab
            m_lngIdealWidth = m_lngIdealWidth + .Width + IIf(EnableTheme, 1, 0)
            If .Height > m_lngIdealHeight Then m_lngIdealHeight = .Height
        End With
    Next l_tabTab
    l_lngIndex = 0
    For Each l_tabTab In m_tscTabs
        With l_tabTab
            l_lngIndex = l_lngIndex + 1
            
            .Left = l_lngX
            .Top = l_lngY
            
            l_lngX = l_lngX + .Width + IIf(EnableTheme, 1, 0)
        End With
    Next l_tabTab
    RaiseEvent Reflow
    m_lngScrollOffset = ClipValue(m_lngScrollOffset + (m_lngScrollDirection * 6), 0, ClipValue(m_lngIdealWidth - UserControl.ScaleWidth, 0, 999999))
    If DisableUpdates Then
    Else
        UpdateMouse
        Redraw
    End If
End Sub

Friend Sub RenderTab(ByVal TheTab As ngTab)
On Error Resume Next
Dim l_rctClip As Fury2Rect
Dim l_rctTab As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_rctClose As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_lngOrientation As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
    Set l_rctClip = m_imgSurface.ClipRectangle
    With TheTab
        Set l_rctTab = .Rectangle
        l_rctTab.Translate -m_lngScrollOffset, 0
        l_lngOffset = ColorOffset(.State)
        If Not l_rctClip.Intersect(l_rctTab) Then Exit Sub
        If EnableTheme Then
            m_imgSurface.ClippedSetClipRectangle l_rctTab
            DrawSegment l_rctTab.Left, l_rctTab.Top, m_imgMask(0), m_imgMask(0).Width, l_rctTab.Height, BlitMode_Font, Colors(tscFill + l_lngOffset)
            m_imgSurface.ClippedSetClipRectangle F2Rect(l_rctTab.Left + m_imgMask(0).Width, l_rctTab.Top, m_imgSurface.Width, m_imgSurface.Height)
            DrawSegment l_rctTab.Right - m_imgMask(2).Width, l_rctTab.Top, m_imgMask(2), m_imgMask(2).Width, l_rctTab.Height, BlitMode_Font, Colors(tscFill + l_lngOffset)
            m_imgSurface.ClippedSetClipRectangle F2Rect(0, 0, l_rctTab.Right - m_imgMask(2).Width, m_imgSurface.Height)
            DrawSegment l_rctTab.Left + m_imgMask(0).Width, l_rctTab.Top, m_imgMask(1), l_rctTab.Width - (m_imgMask(0).Width + m_imgMask(2).Width), l_rctTab.Height, BlitMode_Font, Colors(tscFill + l_lngOffset)
            Set m_imgSurface.ClipRectangle = l_rctClip
            m_imgSurface.ClippedSetClipRectangle l_rctTab
            DrawSegment l_rctTab.Left, l_rctTab.Top, m_imgOutline(0), m_imgOutline(0).Width, l_rctTab.Height, BlitMode_Font, Colors(tscBorder + l_lngOffset)
            m_imgSurface.ClippedSetClipRectangle F2Rect(l_rctTab.Left + m_imgOutline(0).Width, l_rctTab.Top, m_imgSurface.Width, m_imgSurface.Height)
            DrawSegment l_rctTab.Right - m_imgOutline(2).Width, l_rctTab.Top, m_imgOutline(2), m_imgOutline(2).Width, l_rctTab.Height, BlitMode_Font, Colors(tscBorder + l_lngOffset)
            m_imgSurface.ClippedSetClipRectangle F2Rect(0, 0, l_rctTab.Right - m_imgOutline(2).Width, m_imgSurface.Height)
            DrawSegment l_rctTab.Left + m_imgOutline(0).Width, l_rctTab.Top, m_imgOutline(1), l_rctTab.Width - (m_imgOutline(0).Width + m_imgOutline(2).Width), l_rctTab.Height, BlitMode_Font, Colors(tscBorder + l_lngOffset)
            Set m_imgSurface.ClipRectangle = l_rctClip
            m_imgSurface.ClippedSetClipRectangle l_rctTab
            DrawSegment l_rctTab.Left, l_rctTab.Top, m_imgShadow(0), m_imgShadow(0).Width, , BlitMode_Font, Colors(tscBorder + l_lngOffset)
            m_imgSurface.ClippedSetClipRectangle F2Rect(l_rctTab.Left + m_imgShadow(0).Width, l_rctTab.Top, m_imgSurface.Width, m_imgSurface.Height)
            DrawSegment l_rctTab.Right - m_imgShadow(2).Width, l_rctTab.Top, m_imgShadow(2), m_imgShadow(2).Width, , BlitMode_Font, Colors(tscBorder + l_lngOffset)
            m_imgSurface.ClippedSetClipRectangle F2Rect(0, 0, l_rctTab.Right - m_imgShadow(2).Width, m_imgSurface.Height)
            DrawSegment l_rctTab.Left + m_imgShadow(0).Width, l_rctTab.Top, m_imgShadow(1), l_rctTab.Width - (m_imgShadow(0).Width + m_imgShadow(2).Width), , BlitMode_Font, Colors(tscBorder + l_lngOffset)
            l_rctTab.Adjust -Metrics(tsmTabHorizontalMargin), 0
            l_rctTab.Height = l_rctTab.Height - Metrics(tsmTabVerticalMargin)
            l_rctTab.Translate 0, Metrics(tsmTabVerticalMargin)
        Else
            l_rctTab.Adjust -Metrics(tsmTabHorizontalMargin), 0
            m_imgSurface.ClippedSetClipRectangle l_rctTab
            l_rctTab.Height = l_rctTab.Height - Metrics(tsmTabVerticalMargin)
            l_rctTab.Translate 0, Metrics(tsmTabVerticalMargin)
            m_imgSurface.Fill l_rctTab, Colors(tscFill + l_lngOffset), RenderMode_SourceAlpha
            l_rctTab.Height = l_rctTab.Height + 1
            m_imgSurface.Box l_rctTab, Colors(tscBorder + l_lngOffset), RenderMode_SourceAlpha
            l_rctTab.Height = l_rctTab.Height - 1
        End If
        SetTextColor m_lngDC, SwapChannels(SetAlpha(Colors(tscText + l_lngOffset), 0), Red, Blue)
        SetBackgroundMode m_lngDC, BackgroundMode_Transparent
        Set m_imgSurface.ClipRectangle = l_rctClip
        If .Image Is Nothing Then
            l_lngImageWidth = 0
            l_lngImageHeight = 0
        Else
            l_lngImageWidth = .Image.Width
            l_lngImageHeight = .Image.Height
        End If
        Set l_rctImage = l_rctTab.Copy
        l_rctImage.Width = l_lngImageWidth
        l_rctImage.Height = l_lngImageHeight
        l_rctImage.RelLeft = (l_rctTab.Left) + (l_rctTab.Width - l_lngImageWidth) \ 2
        l_rctImage.RelTop = (l_rctTab.Top) + (l_rctTab.Height - l_lngImageHeight) \ 2
        l_rctText.Left = l_rctTab.Left + Metrics(tsmTextMargin)
        l_rctText.Top = l_rctTab.Top + Metrics(tsmTextMargin)
        l_rctText.Right = l_rctTab.Left + l_rctTab.Width - Metrics(tsmTextMargin)
        l_rctText.Bottom = l_rctTab.Top + l_rctTab.Height - Metrics(tsmTextMargin)
        Select Case .TextAlignment
        Case tsaLeft
            l_dtfFlags = DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
            l_rctText.Right = l_rctTab.Left + l_rctTab.Width - l_lngImageWidth - Metrics(tsmTextMargin)
            l_rctImage.RelLeft = l_rctTab.Right - l_lngImageWidth - Metrics(tsmImageMargin) - IIf(ShowCloseButtons, CloseButtonWidth, 0)
        Case tsaRight
            l_dtfFlags = DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
            l_rctText.Left = l_rctTab.Left + l_lngImageWidth + Metrics(tsmTextMargin)
            l_rctImage.RelLeft = l_rctTab.Left + Metrics(tsmImageMargin)
        End Select
        If (.Font Is Nothing) Then
        Else
            Set UserControl.Font = .Font
        End If
        SelectObject m_lngDC, GetCurrentObject(UserControl.hDC, Object_Font)
        If .Enabled Then
            DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
        Else
            With l_rctText
                .Left = .Left + 1
                .Top = .Top + 1
                .Right = .Right + 1
                .Bottom = .Bottom + 1
            End With
            SetTextColor m_lngDC, GetSystemColor(SystemColor_Button_Highlight)
            DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
            With l_rctText
                .Left = .Left - 1
                .Top = .Top - 1
                .Right = .Right - 1
                .Bottom = .Bottom - 1
            End With
            SetTextColor m_lngDC, GetSystemColor(SystemColor_Button_Shadow)
            DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
        End If
        m_imgSurface.Blit l_rctImage, , .Image, , BlitMode_SourceAlpha
        If ShowCloseButtons Then
            Set l_rctClose = F2Rect(l_rctTab.Right - (m_imgClose.Width) - 4, l_rctTab.Top + (l_rctTab.Height - m_imgClose.Height) / 2, m_imgClose.Width, m_imgClose.Height, False)
            m_imgSurface.Blit l_rctClose, , m_imgClose, , BlitMode_SourceAlpha
        End If
        If EnableTheme Then
            Set l_rctTab = .Rectangle
            l_rctTab.Translate -m_lngScrollOffset, 0
            Set m_imgSurface.ClipRectangle = l_rctClip
            m_imgSurface.ClippedSetClipRectangle l_rctTab
            m_imgSurface.Blit l_rctTab, , m_imgHighlight, 0.8, BlitMode_Screen
        End If
    End With
    Set m_imgSurface.ClipRectangle = l_rctClip
    If TheTab.Selected Then
    Else
        If UserControl.ScaleHeight > IdealHeight Then
            Set l_rctTab = TheTab.Rectangle
            l_rctTab.Translate -m_lngScrollOffset, 0
            m_imgSurface.[Line] F2Rect(l_rctTab.Left, l_rctTab.Bottom - 1, l_rctTab.Right, l_rctTab.Bottom - 1), Colors(tscBorder)
        End If
    End If
End Sub

Friend Sub RenderTabBackground(ByVal TheTab As ngTab)
On Error Resume Next
Dim l_rctClip As Fury2Rect
Dim l_rctTab As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
    Set l_rctClip = m_imgSurface.ClipRectangle
    With TheTab
        Set l_rctTab = .Rectangle
        l_rctTab.Translate -m_lngScrollOffset, 0
        If Not l_rctClip.Intersect(l_rctTab) Then Exit Sub
        m_imgSurface.ClippedSetClipRectangle l_rctTab
        l_lngOffset = ColorOffset(.State)
        If (.State = bstNormal) Then
        Else
            m_imgSurface.Fill l_rctTab, Colors(tscBackground + l_lngOffset), RenderMode_SourceAlpha
        End If
    End With
    Set m_imgSurface.ClipRectangle = l_rctClip
End Sub

Friend Sub DrawSegment(ByVal X As Long, ByVal Y As Long, ByRef Image As Fury2Image, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal BlitMode As SFXBlitModes = BlitMode_SourceAlpha, Optional ByVal Color As Long = -1, Optional ByVal Alpha As Single = 1, Optional ByVal Vertical As Boolean = False)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_rctStrip As Fury2Rect, l_rctDest As Fury2Rect
Dim l_rctClip As Fury2Rect
    If Width = -1 Then
        Width = Image.Width
    End If
    If Height = -1 Then
        Height = Image.Height
    End If
    With m_imgSurface
        l_lngX = X
        Do While (l_lngX < (X + Width))
            Set l_rctClip = .ClipRectangle
            .Blit F2Rect(l_lngX, Y, Image.Width, Height, False), , Image, Alpha, BlitMode, Color
            Set l_rctDest = F2Rect(l_lngX, Y + Image.Height, Image.Width, 1, False)
            Set l_rctStrip = F2Rect(0, Image.Height - 1, Image.Width, 1, False)
            For l_lngY = Y + Image.Height To Height
                l_rctDest.RelTop = l_lngY
                .Blit l_rctDest, l_rctStrip, Image, Alpha, BlitMode, Color
            Next l_lngY
            l_lngX = l_lngX + Image.Width
        Loop
    End With
End Sub

Public Sub Redraw(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
Dim l_tabTab As ngTab
Dim l_fntOld As StdFont
Dim l_lngColor As Long
Dim l_rctArea As Rect
    If DisableUpdates Then Exit Sub
    If Area Is Nothing Then
        Set Area = m_imgSurface.Rectangle
    End If
    If UserControl.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If m_tscTabs Is Nothing Then Exit Sub
        Set m_imgSurface.ClipRectangle = Area
        Set l_fntOld = UserControl.Font
        m_imgSurface.Clear Colors(tscBackground)
        If UserControl.ScaleHeight > IdealHeight Then
            m_imgSurface.[Line] F2Rect(0, IdealHeight - 1, m_imgSurface.Width, 0, False), Colors(tscBorder)
        End If
        For Each l_tabTab In m_tscTabs
            RenderTabBackground l_tabTab
        Next l_tabTab
        For Each l_tabTab In m_tscTabs
            Set UserControl.Font = l_fntOld
            RenderTab l_tabTab
        Next l_tabTab
        Set UserControl.Font = l_fntOld
        RaiseEvent Redraw
        Refresh Area
    End If
End Sub

Private Sub UpdateMouse()
On Error Resume Next
Dim l_tabNewHover As ngTab
    Set l_tabNewHover = TabFromPoint(m_lngMouseX, m_lngMouseY)
    If l_tabNewHover Is m_tabHover Then
    Else
        If Not (m_tabHover Is Nothing) Then
            m_tabHover.MouseLeave
        End If
        If Not (l_tabNewHover Is Nothing) Then
            l_tabNewHover.MouseEnter
            With l_tabNewHover
                imgCurrentTab.Move .Left - m_lngScrollOffset, .Top, .Width, .Height
                imgCurrentTab.ToolTipText = .Tooltip
                imgCurrentTab.Visible = True
            End With
        Else
            imgCurrentTab.Visible = False
        End If
'        Redraw l_tabNewHover.Rectangle.Translate(-m_lngScrollOffset, 0)
'        Redraw m_tabHover.Rectangle.Translate(-m_lngScrollOffset, 0)
        Set m_tabHover = l_tabNewHover
        Redraw
    End If
End Sub

Public Function TabFromPoint(ByVal X As Long, ByVal Y As Long) As ngTab
On Error Resume Next
Dim l_tabTab As ngTab
    If m_imgSurface Is Nothing Then Exit Function
    If m_tscTabs Is Nothing Then Exit Function
    For Each l_tabTab In m_tscTabs
        With l_tabTab
            If .Rectangle.PointInside(X + m_lngScrollOffset, Y) Then
                Set TabFromPoint = l_tabTab
                Exit For
            End If
        End With
    Next l_tabTab
End Function

Public Sub AutoSize()
On Error Resume Next
    Reflow
    UserControl.Size IdealWidth * Screen.TwipsPerPixelX, IdealHeight * Screen.TwipsPerPixelY
End Sub

Public Sub Refresh(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If UserControl.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If Area Is Nothing Then
            Set Area = m_imgSurface.Rectangle
            If UserControl.ScaleHeight > IdealHeight Then
                UserControl.Line (0, IdealHeight - 1)-(UserControl.ScaleWidth - 1, UserControl.ScaleHeight - 1), SetAlpha(SwapChannels(Colors(tscBorder), Red, Blue), 0), B
                UserControl.Line (1, IdealHeight)-(UserControl.ScaleWidth - 2, UserControl.ScaleHeight - 2), SetAlpha(SwapChannels(Colors(tscSelectedFill), Red, Blue), 0), BF
            End If
        End If
        BitBlt UserControl.hDC, Area.Left, Area.Top, Area.Width, Area.Height, m_lngDC, Area.Left, Area.Top, vbSrcCopy
    Else
        UserControl.Cls
        UserControl.Print "ngTabStrip: " & UserControl.Extender.Name
    End If
End Sub

Private Sub imgCurrentTab_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    UserControl_MouseDown Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentTab.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentTab.Top
End Sub

Private Sub imgCurrentTab_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    UserControl_MouseMove Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentTab.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentTab.Top
End Sub

Private Sub imgCurrentTab_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    UserControl_MouseUp Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentTab.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentTab.Top
End Sub

Private Sub tmrEdgeScroll_Timer()
On Error Resume Next
    DisableUpdates = True
    m_lngScrollOffset = ClipValue(m_lngScrollOffset + (m_lngScrollDirection * 7), 0, ClipValue(m_lngIdealWidth - UserControl.ScaleWidth, 0, 999999))
    If m_tabHover Is Nothing Then
    Else
        With m_tabHover
            imgCurrentTab.Move .Left - m_lngScrollOffset, .Top, .Width, .Height
        End With
    End If
    UpdateMouse
    DisableUpdates = False
    Redraw
End Sub

Private Sub tmrMouseTracker_Timer()
On Error Resume Next
Dim l_ptMouse As PointAPI
    GetCursorPos l_ptMouse
    ScreenToClient UserControl.hwnd, l_ptMouse
    m_lngMouseX = l_ptMouse.X
    m_lngMouseY = l_ptMouse.Y
    If (m_lngMouseX < 0) Or (m_lngMouseX >= UserControl.ScaleWidth) Or (m_lngMouseY < 0) Or (m_lngMouseY >= UserControl.ScaleHeight) Then
        tmrEdgeScroll.Enabled = False
        m_booMouseOver = False
        tmrMouseTracker.Interval = 100
        UpdateMouse
    End If
End Sub

Private Sub UserControl_Hide()
On Error Resume Next
    FreeSurface
End Sub

Private Sub UserControl_Initialize()
On Error Resume Next
    F2Init
    InitMetrics
    InitColors
    InitTabs
    ResourcePattern = "*"
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_booCancel As Boolean
    RaiseEvent MouseDown(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_tabPressed Is Nothing Then UpdateMouse
    If Button = 1 Then
        Set m_tabPressed = TabFromPoint(X, Y)
        If m_tabPressed Is Nothing Then Exit Sub
        If m_tabPressed.Enabled Then
            If ShowCloseButtons Then
                If ((X - m_tabPressed.Left) > (m_tabPressed.Width - m_imgClose.Width - 5)) And ((X - m_tabPressed.Left) < (m_tabPressed.Width - 5)) Then
                    RaiseEvent TabClose(m_tabPressed)
                    Exit Sub
                End If
            End If
            RaiseEvent TabClick(m_tabPressed)
            m_tabPressed.MouseDown
            SelectTab m_tabPressed
            Redraw
        Else
            Set m_tabPressed = Nothing
        End If
    End If
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If (X < 8) Then
        m_lngScrollDirection = -1
        tmrEdgeScroll.Enabled = True
    ElseIf (X > UserControl.ScaleWidth - 8) Then
        m_lngScrollDirection = 1
        tmrEdgeScroll.Enabled = True
    Else
        m_lngScrollDirection = 0
        tmrEdgeScroll.Enabled = False
    End If
    MouseEntered
    RaiseEvent MouseMove(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_tabPressed Is Nothing Then UpdateMouse
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim m_tabHover As ngTab
    RaiseEvent MouseUp(Button, Shift, X, Y)
    If Not (m_tabPressed Is Nothing) Then
        m_tabPressed.MouseUp
        Set m_tabPressed = Nothing
        Set m_tabHover = TabFromPoint(X, Y)
    End If
    tmrMouseTracker.Interval = IIf(m_booMouseOver, 1, 100)
    UpdateMouse
    Redraw
End Sub

Private Sub UserControl_Paint()
On Error Resume Next
    Refresh
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
Dim l_lngIdealWidth As Long
Dim l_lngIdealHeight As Long
    Reflow
    Select Case UserControl.Extender.Align
    Case vbAlignTop, vbAlignBottom
        If Ambient.UserMode Then
            l_lngIdealHeight = IdealHeight(UserControl.ScaleWidth)
        Else
            l_lngIdealHeight = UserControl.TextHeight("AaBbYyZz")
        End If
        If UserControl.ScaleHeight <> l_lngIdealHeight Then
            UserControl.Height = l_lngIdealHeight * Screen.TwipsPerPixelY
        End If
    Case Else
    End Select
    RaiseEvent Resize
    ResizeSurface
    Reflow
End Sub

Private Sub UserControl_Show()
On Error Resume Next
Dim l_rfOld As ngResourceFile
Dim l_strOldPattern As String
    If Ambient.UserMode Then
        tmrMouseTracker.Enabled = True
        If g_strTabTheme <> "" Then
            Set l_rfOld = Me.ResourceFile
            l_strOldPattern = Me.ResourcePattern
            Set Me.ResourceFile = g_rfThemeFile
            Me.ResourcePattern = g_strThemePattern
            LoadTheme g_strTabTheme
            Me.ResourcePattern = l_strOldPattern
            Set Me.ResourceFile = l_rfOld
        End If
    End If
    InitSurface
    InitColors
    Reflow
    SelectTab Tabs(1)
End Sub

Private Sub UserControl_Terminate()
On Error Resume Next
    FreeTabs
End Sub

Public Sub SelectTab(ByVal TheTab, Optional ByVal DispatchEvent As Boolean = True)
On Error Resume Next
Dim l_tabTab As ngTab
    If VarType(TheTab) = vbObject Then
    Else
        Set TheTab = Tabs(TheTab)
    End If
    For Each l_tabTab In Tabs
        l_tabTab.Selected = False
    Next l_tabTab
    TheTab.Selected = True
    Set m_tabSelected = TheTab
    If DispatchEvent Then
        Set l_tabTab = TheTab
        RaiseEvent TabSelected(l_tabTab)
    End If
    If (l_tabTab.Left > (m_lngScrollOffset + UserControl.ScaleWidth)) Or ((l_tabTab.Left + l_tabTab.Width) < m_lngScrollOffset) Then
        m_lngScrollOffset = ClipValue(l_tabTab.Left, 0, ClipValue(m_lngIdealWidth - UserControl.ScaleWidth, 0, 999999))
    End If
    Redraw
End Sub

Public Property Get SelectedTab() As ngTab
    Set SelectedTab = m_tabSelected
End Property
