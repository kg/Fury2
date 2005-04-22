VERSION 5.00
Begin VB.UserControl ngToolbar 
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
   Begin VB.Timer tmrMouseTracker 
      Enabled         =   0   'False
      Interval        =   100
      Left            =   615
      Top             =   0
   End
   Begin VB.Image imgCurrentButton 
      Height          =   240
      Left            =   30
      ToolTipText     =   "Tooltip"
      Top             =   30
      Visible         =   0   'False
      Width           =   240
   End
End
Attribute VB_Name = "ngToolbar"
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
Event ButtonPress(Button As ngToolButton, ByRef Cancel As Boolean)
Event ButtonClick(Button As ngToolButton)
Event Reflow()
Event Resize()
Event Redraw()
Private m_imgShadow(0 To 1, 0 To 2) As Fury2Image
Private m_imgHighlight(0 To 1, 0 To 2) As Fury2Image
Private m_imgDivider(0 To 1, 0 To 1) As Fury2Image
Private m_imgTexture(0 To 1) As Fury2Image
Private m_imgAccent(0 To 1) As Fury2Image
Private m_lngMetrics(0 To tbm_max) As Long
Private m_lngColors(0 To tbc_max) As Long
Private m_tbcButtons As ngToolButtons
Private m_tboOrientation As ngToolbarOrientations
Private m_imgSurface As Fury2Image, m_lngDC As Long, m_lngNullBitmap As Long
Private m_btnHover As ngToolButton, m_btnPressed As ngToolButton
Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngIdealWidth As Long, m_lngIdealHeight As Long
Private m_lngRowHeight As Long
Private m_booInitialized As Boolean
Private m_booMouseOver As Boolean
Public ThemeBackground As Variant
Public EnableTheme As Boolean
Public ResourceFile As ngResourceFile
Public ResourcePattern As String
Public DisableUpdates As Boolean

Public Property Get ScaleWidth() As Long
    ScaleWidth = UserControl.ScaleWidth
End Property

Public Property Get ScaleHeight() As Long
    ScaleHeight = UserControl.ScaleHeight
End Property

Public Property Get Orientation() As ngToolbarOrientations
On Error Resume Next
    Orientation = m_tboOrientation
End Property

Public Property Let Orientation(ByVal NewValue As ngToolbarOrientations)
On Error Resume Next
    m_tboOrientation = NewValue
    Reflow
End Property

Private Function LoadThemeImage(ByRef Path As String, ByRef Filename As String, Optional ByRef Failed As Boolean) As Fury2Image
On Error Resume Next
    Set LoadThemeImage = ResourceFile.ItemData(Path & Filename & ".png")
    If LoadThemeImage Is Nothing Then Failed = True
    Err.Clear
End Function

Public Sub LoadTheme(ByRef Path As String)
On Error Resume Next
Dim l_lngColorLeft As Long
Dim l_lngColorRight As Long
Dim l_lngIndex As Long, l_lngOrientation As Long, l_strOrientation As String
Dim l_booFailed As Boolean
    EnableTheme = False
    For l_lngOrientation = 0 To 1
        l_strOrientation = Choose(l_lngOrientation + 1, "horizontal\", "vertical\")
        For l_lngIndex = 0 To 2
            Set m_imgShadow(l_lngOrientation, l_lngIndex) = LoadThemeImage(Path, l_strOrientation & "shadow_" & (l_lngIndex + 1), l_booFailed)
            Set m_imgHighlight(l_lngOrientation, l_lngIndex) = LoadThemeImage(Path, l_strOrientation & "highlight_" & (l_lngIndex + 1), l_booFailed)
        Next l_lngIndex
        Set m_imgDivider(l_lngOrientation, 0) = LoadThemeImage(Path, l_strOrientation & "divider_s", l_booFailed)
        Set m_imgDivider(l_lngOrientation, 1) = LoadThemeImage(Path, l_strOrientation & "divider_h", l_booFailed)
        Set m_imgAccent(l_lngOrientation) = LoadThemeImage(Path, l_strOrientation & "accent", l_booFailed)
        Set m_imgTexture(l_lngOrientation) = LoadThemeImage(Path, l_strOrientation & "pattern", l_booFailed)
    Next l_lngOrientation
    l_lngColorLeft = BlendColors(m_lngColors(tbcBackground), F2White, 24)
    l_lngColorRight = BlendColors(m_lngColors(tbcBackground), F2Black, 24)
    ThemeBackground = Array(l_lngColorLeft, l_lngColorRight, l_lngColorLeft, l_lngColorRight)
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

Public Property Get Buttons() As ngToolButtons
    Set Buttons = m_tbcButtons
End Property

Public Property Get Metrics(ByVal Index As ngToolbarMetrics) As Long
    Metrics = m_lngMetrics(Index)
End Property

Public Property Let Metrics(ByVal Index As ngToolbarMetrics, ByVal NewValue As Long)
    m_lngMetrics(Index) = NewValue
End Property

Public Property Get Colors(ByVal Index As ngToolbarColors) As Long
    Colors = m_lngColors(Index)
End Property

Public Property Let Colors(ByVal Index As ngToolbarColors, ByVal NewColor As Long)
    m_lngColors(Index) = NewColor
End Property

Private Property Get ColorOffset(ByVal State As ngToolButtonStates) As Long
On Error Resume Next
    ColorOffset = (State - ngToolButtonStates.bstNormal) * (ngToolbarColors.tbcGlow - ngToolbarColors.tbcBackground + 1)
End Property

Private Sub InitMetrics()
On Error Resume Next
    Metrics(tbmButtonMargin) = 2
    Metrics(tbmImageMargin) = 1
    Metrics(tbmTextMargin) = 1
    Metrics(tbmSeparatorMargin) = 2
    Metrics(tbmSeparatorSize) = 1
End Sub

Private Sub InitColors()
On Error Resume Next
    Colors(tbcBackground) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Button_Shadow), 0)
    Colors(tbcBorder) = F2Transparent
    Colors(tbcText) = ConvertSystemColor(SystemColor_Button_Text)
    Colors(tbcTint) = F2RGB(255, 255, 255, 192)
    Colors(tbcGlow) = F2RGB(0, 0, 0, 0)
    Colors(tbcHighlight) = SetAlpha(ConvertSystemColor(SystemColor_Highlight), 140)
    Colors(tbcHighlightBorder) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 190)
    Colors(tbcHighlightText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(tbcHighlightTint) = F2White
    Colors(tbcHighlightGlow) = F2RGB(255, 255, 255, 140)
    Colors(tbcPressed) = SetAlpha(ConvertSystemColor(SystemColor_Highlight), 192)
    Colors(tbcPressedBorder) = ConvertSystemColor(SystemColor_Highlight)
    Colors(tbcPressedText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(tbcPressedTint) = F2White
    Colors(tbcPressedGlow) = F2RGB(255, 255, 255, 190)
    Colors(tbcDisabled) = F2Transparent
    Colors(tbcDisabledBorder) = F2Transparent
    Colors(tbcDisabledText) = ConvertSystemColor(SystemColor_Button_Text_Disabled)
    Colors(tbcDisabledTint) = F2RGB(127, 127, 127, 63)
    Colors(tbcDisabledGlow) = F2RGB(127, 127, 127, 0)
    Colors(tbcChecked) = SetAlpha(ConvertSystemColor(SystemColor_Highlight), 150)
    Colors(tbcCheckedBorder) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 220)
    Colors(tbcCheckedText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(tbcCheckedTint) = F2White
    Colors(tbcCheckedGlow) = F2RGB(255, 255, 255, 255)
    Colors(tbcSeparator) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 140)
    ' TODO: Initialize other colors
End Sub

Private Sub InitButtons()
On Error Resume Next
    Set m_tbcButtons = New ngToolButtons
    m_tbcButtons.SetParent Me
End Sub

Private Sub FreeButtons()
On Error Resume Next
    m_tbcButtons.Free
    Set m_tbcButtons = Nothing
End Sub

Private Sub InitSurface()
On Error Resume Next
    If UserControl.Ambient.UserMode Then
        m_lngDC = CreateCompatibleDC(UserControl.hDC)
        Set m_imgSurface = F2DIBSection(UserControl.ScaleWidth, UserControl.ScaleHeight, m_lngDC)
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

Public Property Get IdealVerticalWidth() As Long
On Error Resume Next
Dim l_lngWidth As Long
Dim l_btnButton As ngToolButton
    For Each l_btnButton In m_tbcButtons
        With l_btnButton
            If (.Width) > l_lngWidth Then
                l_lngWidth = .Width
            End If
        End With
    Next l_btnButton
    IdealVerticalWidth = l_lngWidth
End Property

Public Property Get IdealVerticalHeight() As Long
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_btnButton As ngToolButton
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
Dim l_lngWidth As Long
    l_lngWidth = IdealVerticalWidth
    For Each l_btnButton In m_tbcButtons
        l_lngIndex = l_lngIndex + 1
        With l_btnButton
            If (l_lngX + .Width) > l_lngWidth Then
                l_lngX = 0
                l_lngY = l_lngY + .RowHeight
                l_booNewRow = True
            End If
            If (l_lngY + .Height) > IdealVerticalHeight Then IdealVerticalHeight = l_lngY + .Height
            l_lngX = l_lngX + .Width
            If (l_booNewRow) And (.Style = bsySeparator) Then
                l_lngY = l_lngY + .Height
                l_lngX = 0
                l_booNewRow = True
            Else
                l_booNewRow = False
            End If
        End With
    Next l_btnButton
    If IdealVerticalHeight < 4 Then IdealVerticalHeight = 4
End Property

Public Property Get IdealWidth() As Long
On Error Resume Next
    IdealWidth = m_lngIdealWidth
End Property

Public Property Get IdealHeight(Optional ByVal Width As Long = -1) As Long
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_btnButton As ngToolButton
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
Dim l_lngRowHeight
    If Width = -1 Then
        IdealHeight = m_lngIdealHeight
        If IdealHeight < 4 Then IdealHeight = 4
    Else
        For Each l_btnButton In m_tbcButtons
            l_lngIndex = l_lngIndex + 1
            With l_btnButton
                If (l_lngX + .Width) > Width Then
                    l_lngX = 0
                    l_lngY = l_lngY + l_lngRowHeight
                    l_booNewRow = True
                End If
                If (l_lngY + .Height) > IdealHeight Then IdealHeight = l_lngY + .Height
                l_lngX = l_lngX + .Width
                If (l_booNewRow) And (.Style = bsySeparator) Then
                    l_lngY = l_lngY + .Height
                    l_lngX = 0
                    l_booNewRow = True
                Else
                    l_booNewRow = False
                End If
                l_lngRowHeight = .RowHeight
            End With
        Next l_btnButton
        If IdealHeight < 4 Then IdealHeight = 4
    End If
End Property

Public Sub Reflow()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_btnButton As ngToolButton
Dim l_lngRowHeight As Long
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long, l_lngRowStart As Long, l_lngButton As Long
    If Not UserControl.Ambient.UserMode Then Exit Sub
    m_lngIdealWidth = 0
    m_lngIdealHeight = 0
    For Each l_btnButton In m_tbcButtons
        With l_btnButton
            m_lngIdealWidth = m_lngIdealWidth + .Width
            If .Height > m_lngIdealHeight Then m_lngIdealHeight = .Height
        End With
    Next l_btnButton
    l_lngIndex = 0
    l_lngRowStart = 1
    For Each l_btnButton In m_tbcButtons
        With l_btnButton
            l_lngIndex = l_lngIndex + 1
            If (l_lngX + .Width) > UserControl.ScaleWidth Then
                For l_lngButton = l_lngRowStart To l_lngIndex - 1
                    m_tbcButtons(l_lngButton).RowHeight = l_lngRowHeight
                Next l_lngButton
                l_lngX = 0
                l_lngY = l_lngY + l_lngRowHeight
                l_lngRowHeight = .Height
                l_booNewRow = True
                l_lngRowStart = l_lngIndex
            End If
            
            If (.Style = bsySeparator) Then
                .Orientation = IIf(l_booNewRow, borVertical, borHorizontal)
            End If
            
            .Left = l_lngX
            .Top = l_lngY
            If .Height > l_lngRowHeight Then l_lngRowHeight = .Height
            
            l_lngX = l_lngX + .Width
            l_booNewRow = False
            If (.Orientation = borVertical) Then
                l_lngY = l_lngY + .Height
                l_lngX = 0
                l_booNewRow = True
            End If
        End With
    Next l_btnButton
    If l_lngRowStart <> m_tbcButtons.Count Then
        For l_lngButton = l_lngRowStart To m_tbcButtons.Count
            m_tbcButtons(l_lngButton).RowHeight = l_lngRowHeight
        Next l_lngButton
    End If
    RaiseEvent Reflow
    UpdateMouse
    Redraw
End Sub

Friend Sub RenderButton(ByVal Button As ngToolButton)
On Error Resume Next
Dim l_rctClip As Fury2Rect
Dim l_rctButton As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_lngOrientation As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
    Set l_rctClip = m_imgSurface.ClipRectangle
    With Button
        Select Case .Style
        Case bsyNormal, bsyCheck, bsyGroup
            Set l_rctButton = .Rectangle
            l_lngOffset = ColorOffset(.State)
            l_rctButton.Adjust -Metrics(tbmButtonMargin), -Metrics(tbmButtonMargin)
            If Not l_rctClip.Intersect(l_rctButton) Then Exit Sub
            SetTextColor m_lngDC, SwapChannels(SetAlpha(Colors(tbcText + l_lngOffset), 0), Red, Blue)
            SetBackgroundMode m_lngDC, BackgroundMode_Transparent
            If .Image Is Nothing Then
                l_lngImageWidth = 0
                l_lngImageHeight = 0
            Else
                l_lngImageWidth = .Image.Width
                l_lngImageHeight = .Image.Height
            End If
            Set l_rctImage = l_rctButton.Copy
            l_rctImage.Width = l_lngImageWidth
            l_rctImage.Height = l_lngImageHeight
            l_rctImage.RelLeft = (l_rctButton.Left) + (l_rctButton.Width - l_lngImageWidth) \ 2
            l_rctImage.RelTop = (l_rctButton.Top) + (l_rctButton.Height - l_lngImageHeight) \ 2
            l_rctText.Left = l_rctButton.Left + Metrics(tbmTextMargin)
            l_rctText.Top = l_rctButton.Top + Metrics(tbmTextMargin)
            l_rctText.Right = l_rctButton.Left + l_rctButton.Width - Metrics(tbmTextMargin)
            l_rctText.Bottom = l_rctButton.Top + l_rctButton.Height - Metrics(tbmTextMargin)
            Select Case .TextAlignment
            Case btaLeft
                l_dtfFlags = DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                l_rctText.Right = l_rctButton.Left + l_rctButton.Width - l_lngImageWidth - Metrics(tbmTextMargin)
                l_rctImage.RelLeft = l_rctButton.Right - l_lngImageWidth - Metrics(tbmImageMargin)
            Case btaRight
                l_dtfFlags = DrawText_Align_Right Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                l_rctText.Left = l_rctButton.Left + l_lngImageWidth + Metrics(tbmTextMargin)
                l_rctImage.RelLeft = l_rctButton.Left + Metrics(tbmImageMargin)
            Case btaTop
                l_dtfFlags = DrawText_Align_Top Or DrawText_Align_Center_Horizontal Or DrawText_Wrap_None
                l_rctText.Bottom = l_rctButton.Top + l_rctButton.Height - l_lngImageHeight - Metrics(tbmTextMargin)
                l_rctImage.RelTop = l_rctButton.Bottom - l_lngImageHeight - Metrics(tbmImageMargin)
            Case btaBottom
                l_dtfFlags = DrawText_Align_Bottom Or DrawText_Align_Center_Horizontal Or DrawText_Wrap_None
                l_rctText.Top = l_rctButton.Top + l_lngImageHeight + Metrics(tbmTextMargin)
                l_rctImage.RelTop = l_rctButton.Top + Metrics(tbmImageMargin)
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
            If GetAlpha(Colors(tbcGlow + l_lngOffset)) > 0 Then
                m_imgSurface.Blit l_rctImage.Copy.Adjust(2, 2), , .GlowImage, IIf(.State = bstChecked, 1, 0.66), BlitMode_Font_SourceAlpha, Colors(tbcGlow + l_lngOffset)
            End If
            m_imgSurface.Blit l_rctImage, , .Image, , BlitMode_SourceAlpha_ColorMask, Colors(tbcTint + l_lngOffset)
        Case bsySeparator
            If .Orientation = borHorizontal Then
                Set l_rctButton = F2Rect(.Left, .Top, .Width, .RowHeight, False)
            Else
                Set l_rctButton = F2Rect(.Left, .Top, m_imgSurface.Width, .Height, False)
            End If
            If Not l_rctClip.Intersect(l_rctButton) Then Exit Sub
            If EnableTheme Then
                m_imgSurface.ClippedSetClipRectangle l_rctButton
                Select Case .Orientation
                Case tboHorizontal
                    DrawSegment .Left, .Top, m_imgDivider(.Orientation, 0), .Width, .RowHeight, BlitMode_Subtractive, , 0.15
                    DrawSegment .Left, .Top, m_imgDivider(.Orientation, 1), .Width, .RowHeight, BlitMode_Screen, , 0.35
                Case tboVertical
                    DrawSegment .Left, .Top, m_imgDivider(.Orientation, 0), , .Height, BlitMode_Subtractive, , 0.15, True
                    DrawSegment .Left, .Top, m_imgDivider(.Orientation, 1), , .Height, BlitMode_Screen, , 0.35, True
                End Select
            Else
    '            m_imgSurface.Fill l_rctButton, Colors(tbcBackground + l_lngOffset), RenderMode_Normal
                l_rctButton.Adjust -Metrics(tbmSeparatorMargin), -Metrics(tbmSeparatorMargin)
                m_imgSurface.Fill l_rctButton, Colors(tbcSeparator), RenderMode_SourceAlpha
            End If
        End Select
    End With
    Set m_imgSurface.ClipRectangle = l_rctClip
End Sub

Friend Sub RenderButtonBackground(ByVal Button As ngToolButton)
On Error Resume Next
Dim l_rctClip As Fury2Rect
Dim l_rctButton As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
    Set l_rctClip = m_imgSurface.ClipRectangle
    With Button
        Select Case .Style
        Case bsyNormal, bsyCheck, bsyGroup
            Set l_rctButton = .Rectangle
            If Not l_rctClip.Intersect(l_rctButton) Then Exit Sub
            l_lngOffset = ColorOffset(.State)
            If (.State = bstNormal) Then
            Else
                m_imgSurface.Fill l_rctButton, Colors(tbcBackground + l_lngOffset), RenderMode_SourceAlpha
                m_imgSurface.Box l_rctButton, Colors(tbcBorder + l_lngOffset), RenderMode_SourceAlpha
            End If
        Case bsySeparator
        End Select
    End With
    Set m_imgSurface.ClipRectangle = l_rctClip
End Sub

Friend Sub DrawSegment(ByVal X As Long, ByVal Y As Long, ByRef Image As Fury2Image, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal BlitMode As SFXBlitModes = BlitMode_SourceAlpha, Optional ByVal Color As Long = -1, Optional ByVal Alpha As Single = 1, Optional ByVal Vertical As Boolean = False)
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
            l_lngY = Y
            Do While (l_lngY < (Y + Height))
                Set l_rctClip = .ClipRectangle
                .Blit F2Rect(0, l_lngY, 11, Image.Height, False), , Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(X + 11, l_lngY, .Width, l_lngY + Image.Height)
                .Blit F2Rect(.Width - 12, l_lngY, 12, Image.Height, False), F2Rect(12, 0, 10, Image.Height, False), Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(X, l_lngY, .Width - 12, l_lngY + Image.Height)
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
            l_lngX = X
            Do While (l_lngX < (X + Width))
                Set l_rctClip = .ClipRectangle
                .Blit F2Rect(l_lngX, Y, Image.Width, 13, False), , Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(l_lngX, Y + 13, l_lngX + Image.Width, Y + Height)
                .Blit F2Rect(l_lngX, Y + Height - 10, Image.Width, 10, False), F2Rect(0, 14, Image.Width, 10, False), Image, Alpha, BlitMode, Color
                .ClippedSetClipRectangle F2Rect(l_lngX, Y, l_lngX + Image.Width, Y + Height - 10)
                Set l_rctDest = F2Rect(l_lngX, Y, Image.Width, 1, False)
                Set l_rctStrip = F2Rect(0, 13, Image.Width, 1, False)
                For l_lngY = Y To .Height
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
Dim l_btnButton As ngToolButton
Dim l_fntOld As StdFont
Dim l_lngColor As Long
Dim l_rctArea As Rect
    If DisableUpdates Then Exit Sub
    If Area Is Nothing Then
        Set Area = m_imgSurface.Rectangle
    End If
    If UserControl.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If m_tbcButtons Is Nothing Then Exit Sub
        Set m_imgSurface.ClipRectangle = Area
        Set l_fntOld = UserControl.Font
        If EnableTheme Then
'            m_imgSurface.Clear Colors(tbcBackground)
            m_imgSurface.GradientFill m_imgSurface.Rectangle, ThemeBackground
            m_imgSurface.TileBlit m_imgSurface.Rectangle, m_imgTexture(m_tboOrientation), 0.015, BlitMode_Subtractive
        Else
            m_imgSurface.Clear Colors(tbcBackground)
        End If
        For Each l_btnButton In m_tbcButtons
            RenderButtonBackground l_btnButton
        Next l_btnButton
        If EnableTheme Then
            If m_tboOrientation = tboHorizontal Then
                m_imgSurface.TileBlit F2Rect(0, m_imgSurface.Height - m_imgAccent(m_tboOrientation).Height, m_imgSurface.Width, m_imgAccent(m_tboOrientation).Height, False), m_imgAccent(m_tboOrientation), 0.125, BlitMode_Font, F2RGB(63, 63, 220, 255)
                Set m_imgSurface.ClipRectangle = Area
                DrawSegment 0, 0, m_imgShadow(m_tboOrientation, 0), m_imgShadow(m_tboOrientation, 0).Width, , BlitMode_Subtractive, , 0.2
                m_imgSurface.ClippedSetClipRectangle F2Rect(m_imgShadow(m_tboOrientation, 0).Width, 0, m_imgSurface.Width, m_imgSurface.Height)
                DrawSegment m_imgSurface.Width - m_imgShadow(m_tboOrientation, 2).Width, 0, m_imgShadow(m_tboOrientation, 2), m_imgShadow(m_tboOrientation, 2).Width, , BlitMode_Subtractive, , 0.2
                m_imgSurface.ClippedSetClipRectangle F2Rect(0, 0, m_imgSurface.Width - m_imgShadow(m_tboOrientation, 2).Width, m_imgSurface.Height)
                DrawSegment m_imgShadow(m_tboOrientation, 0).Width, 0, m_imgShadow(m_tboOrientation, 1), m_imgSurface.Width - (m_imgShadow(m_tboOrientation, 0).Width + m_imgShadow(m_tboOrientation, 2).Width), , BlitMode_Subtractive, , 0.2
                Set m_imgSurface.ClipRectangle = Area
                DrawSegment 0, 0, m_imgHighlight(m_tboOrientation, 0), m_imgHighlight(m_tboOrientation, 0).Width, , BlitMode_Screen, , 0.3
                m_imgSurface.ClippedSetClipRectangle F2Rect(m_imgHighlight(m_tboOrientation, 0).Width, 0, m_imgSurface.Width, m_imgSurface.Height)
                DrawSegment m_imgSurface.Width - m_imgHighlight(m_tboOrientation, 2).Width, 0, m_imgHighlight(m_tboOrientation, 2), m_imgHighlight(m_tboOrientation, 2).Width, , BlitMode_Screen, , 0.3
                m_imgSurface.ClippedSetClipRectangle F2Rect(0, 0, m_imgSurface.Width - m_imgHighlight(m_tboOrientation, 2).Width, m_imgSurface.Height)
                DrawSegment m_imgHighlight(m_tboOrientation, 0).Width, 0, m_imgHighlight(m_tboOrientation, 1), m_imgSurface.Width - (m_imgHighlight(m_tboOrientation, 0).Width + m_imgHighlight(m_tboOrientation, 2).Width), , BlitMode_Screen, , 0.3
                Set m_imgSurface.ClipRectangle = Area
            Else
                m_imgSurface.TileBlit F2Rect(0, 0, m_imgAccent(m_tboOrientation).Width, m_imgSurface.Height, False), m_imgAccent(m_tboOrientation), 0.125, BlitMode_Font, F2RGB(63, 63, 220, 255)
                Set m_imgSurface.ClipRectangle = Area
                DrawSegment 0, 0, m_imgShadow(m_tboOrientation, 0), , m_imgShadow(m_tboOrientation, 0).Height, BlitMode_Subtractive, , 0.2, True
                m_imgSurface.ClippedSetClipRectangle F2Rect(0, m_imgShadow(m_tboOrientation, 0).Height, m_imgSurface.Width, m_imgSurface.Height)
                DrawSegment 0, m_imgSurface.Height - m_imgShadow(m_tboOrientation, 2).Height, m_imgShadow(m_tboOrientation, 2), , m_imgShadow(m_tboOrientation, 2).Height, BlitMode_Subtractive, , 0.2, True
                m_imgSurface.ClippedSetClipRectangle F2Rect(0, 0, m_imgSurface.Width, m_imgSurface.Height - m_imgShadow(m_tboOrientation, 2).Height)
                DrawSegment 0, m_imgShadow(m_tboOrientation, 0).Height, m_imgShadow(m_tboOrientation, 1), , m_imgSurface.Height - (m_imgShadow(m_tboOrientation, 0).Height + m_imgShadow(m_tboOrientation, 2).Height), BlitMode_Subtractive, , 0.2, True
                Set m_imgSurface.ClipRectangle = Area
                DrawSegment 0, 0, m_imgHighlight(m_tboOrientation, 0), , m_imgHighlight(m_tboOrientation, 0).Height, BlitMode_Screen, , 0.3, True
                m_imgSurface.ClippedSetClipRectangle F2Rect(0, m_imgHighlight(m_tboOrientation, 0).Height, m_imgSurface.Width, m_imgSurface.Height)
                DrawSegment 0, m_imgSurface.Height - m_imgHighlight(m_tboOrientation, 2).Height, m_imgHighlight(m_tboOrientation, 2), , m_imgHighlight(m_tboOrientation, 2).Height, BlitMode_Screen, , 0.3, True
                m_imgSurface.ClippedSetClipRectangle F2Rect(0, 0, m_imgSurface.Width, m_imgSurface.Height - m_imgHighlight(m_tboOrientation, 2).Height)
                DrawSegment 0, m_imgHighlight(m_tboOrientation, 0).Height, m_imgHighlight(m_tboOrientation, 1), , m_imgSurface.Height - (m_imgHighlight(m_tboOrientation, 0).Height + m_imgHighlight(m_tboOrientation, 2).Height), BlitMode_Screen, , 0.3, True
                Set m_imgSurface.ClipRectangle = Area
            End If
        End If
        For Each l_btnButton In m_tbcButtons
            Set UserControl.Font = l_fntOld
            RenderButton l_btnButton
        Next l_btnButton
        Set UserControl.Font = l_fntOld
        RaiseEvent Redraw
        Refresh Area
    End If
End Sub

Private Sub UpdateMouse()
On Error Resume Next
Dim l_btnNewHover As ngToolButton
    Set l_btnNewHover = ButtonFromPoint(m_lngMouseX, m_lngMouseY)
    If l_btnNewHover Is m_btnHover Then
    Else
        If Not (m_btnHover Is Nothing) Then
            m_btnHover.MouseLeave
        End If
        If Not (l_btnNewHover Is Nothing) Then
            l_btnNewHover.MouseEnter
            With l_btnNewHover
                imgCurrentButton.Move .Left, .Top, .Width, .Height
                imgCurrentButton.ToolTipText = .Tooltip
                imgCurrentButton.Visible = True
            End With
        Else
            imgCurrentButton.Visible = False
        End If
        Redraw m_btnHover.Rectangle
        Set m_btnHover = l_btnNewHover
        Redraw m_btnHover.Rectangle
    End If
End Sub

Public Function ButtonFromPoint(ByVal X As Long, ByVal Y As Long) As ngToolButton
On Error Resume Next
Dim l_btnButton As ngToolButton
    If m_imgSurface Is Nothing Then Exit Function
    If m_tbcButtons Is Nothing Then Exit Function
    For Each l_btnButton In m_tbcButtons
        With l_btnButton
            If .Rectangle.PointInside(X, Y) Then
                Set ButtonFromPoint = l_btnButton
                Exit For
            End If
        End With
    Next l_btnButton
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
        End If
        BitBlt UserControl.hDC, Area.Left, Area.Top, Area.Width, Area.Height, m_lngDC, Area.Left, Area.Top, vbSrcCopy
    Else
        UserControl.Cls
        UserControl.Print "ngToolbar: " & UserControl.Extender.Name
    End If
End Sub

Private Sub imgCurrentButton_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    UserControl_MouseDown Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentButton.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentButton.Top
End Sub

Private Sub imgCurrentButton_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    UserControl_MouseMove Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentButton.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentButton.Top
End Sub

Private Sub imgCurrentButton_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    UserControl_MouseUp Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentButton.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentButton.Top
End Sub

Private Sub tmrMouseTracker_Timer()
On Error Resume Next
Dim l_ptMouse As PointAPI
    GetCursorPos l_ptMouse
    ScreenToClient UserControl.hwnd, l_ptMouse
    m_lngMouseX = l_ptMouse.X
    m_lngMouseY = l_ptMouse.Y
    If (m_lngMouseX < 0) Or (m_lngMouseX >= UserControl.ScaleWidth) Or (m_lngMouseY < 0) Or (m_lngMouseY >= UserControl.ScaleHeight) Then
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
    InitButtons
    ResourcePattern = "*"
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_booCancel As Boolean
    RaiseEvent MouseDown(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_btnPressed Is Nothing Then UpdateMouse
    If Button = 1 Then
        Set m_btnPressed = ButtonFromPoint(X, Y)
        If m_btnPressed Is Nothing Then Exit Sub
        If m_btnPressed.Enabled Then
            RaiseEvent ButtonPress(m_btnPressed, l_booCancel)
            If Not l_booCancel Then
                m_btnPressed.MouseDown
            Else
                m_btnHover.MouseLeave
                Set m_btnHover = Nothing
                m_btnPressed.MouseLeave
                Set m_btnPressed = Nothing
                UpdateMouse
                Redraw
            End If
            Redraw m_btnPressed.Rectangle
        Else
            Set m_btnPressed = Nothing
        End If
    End If
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    MouseEntered
    RaiseEvent MouseMove(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_btnPressed Is Nothing Then UpdateMouse
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim m_btnHover As ngToolButton
    RaiseEvent MouseUp(Button, Shift, X, Y)
    If Not (m_btnPressed Is Nothing) Then
        Set m_btnHover = ButtonFromPoint(X, Y)
        If m_btnHover Is m_btnPressed Then RaiseEvent ButtonClick(m_btnPressed)
        m_btnPressed.MouseUp
        Set m_btnPressed = Nothing
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
    Case vbAlignLeft, vbAlignRight
        If Ambient.UserMode Then
            l_lngIdealWidth = IdealVerticalWidth
        Else
            l_lngIdealWidth = UserControl.TextWidth("AaBbYyZz")
        End If
        If UserControl.ScaleWidth <> l_lngIdealWidth Then
            UserControl.Width = l_lngIdealWidth * Screen.TwipsPerPixelX
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
        If g_strToolbarTheme <> "" Then
            Set l_rfOld = Me.ResourceFile
            l_strOldPattern = Me.ResourcePattern
            Set Me.ResourceFile = g_rfThemeFile
            Me.ResourcePattern = g_strThemePattern
            LoadTheme g_strToolbarTheme
            Me.ResourcePattern = l_strOldPattern
            Set Me.ResourceFile = l_rfOld
        End If
    End If
    InitSurface
    Reflow
End Sub

Private Sub UserControl_Terminate()
On Error Resume Next
    FreeButtons
End Sub
