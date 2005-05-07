VERSION 5.00
Begin VB.Form frmMenu 
   BorderStyle     =   0  'None
   ClientHeight    =   3150
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4680
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   9
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   ScaleHeight     =   3150
   ScaleWidth      =   4680
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "frmMenu"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
#If 0 Then
Option Explicit
Implements ngMenuParent
Private Declare Function InvalidateRect Lib "user32" (ByVal hwnd As Long, lpRect As Rect, ByVal bErase As Long) As Long
Private Declare Function UpdateWindow Lib "user32" (ByVal hwnd As Long) As Long

Private m_lngMetrics(0 To mnm_max) As Long
Private m_lngColors(0 To mnc_max) As Long
Private m_miItems As ngMenuItems
Private m_imgSurface As Fury2Image, m_lngDC As Long, m_lngNullBitmap As Long
Private m_miHover As ngMenuItem, m_miPressed As ngMenuItem
Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngIdealWidth As Long, m_lngIdealHeight As Long
Private m_booInitialized As Boolean
Private m_booMouseOver As Boolean
Public ResourceFile As ngResourceFile
Public ResourcePattern As String
Public DisableUpdates As Boolean

Private Sub MouseEntered()
On Error Resume Next
    If m_booMouseOver Then Exit Sub
    tmrMouseTracker.Interval = 1
End Sub

Public Property Get Buttons() As ngMenuItems
    Set Buttons = m_miItems
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

Private Property Get ColorOffset(ByVal State As ngMenuItemStates) As Long
On Error Resume Next
    ColorOffset = (State - ngMenuItemStates.bstNormal) * (ngToolbarColors.tbcGlow - ngToolbarColors.tbcBackground + 1)
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
    Set m_miItems = New ngMenuItems
    m_miItems.SetParent Me
End Sub

Private Sub FreeButtons()
On Error Resume Next
    m_miItems.Free
    Set m_miItems = Nothing
End Sub

Private Sub InitSurface()
On Error Resume Next
    If Form.Ambient.UserMode Then
        m_lngDC = CreateCompatibleDC(Form.hDC)
        Set m_imgSurface = F2DIBSection(Form.ScaleWidth, Form.ScaleHeight, m_lngDC)
        m_lngNullBitmap = SelectObject(m_lngDC, m_imgSurface.DIBHandle)
    End If
End Sub

Private Sub ResizeSurface()
On Error Resume Next
    If m_imgSurface Is Nothing Then Exit Sub
    FreeSurface
    InitSurface
'    SelectObject m_lngDC, m_lngNullBitmap
'    m_imgSurface.Resize Form.ScaleWidth, Form.ScaleHeight
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
Dim l_miButton As ngMenuItem
    For Each l_miButton In m_miItems
        With l_miButton
            If (.Width) > l_lngWidth Then
                l_lngWidth = .Width
            End If
        End With
    Next l_miButton
    IdealVerticalWidth = l_lngWidth
End Property

Public Property Get IdealVerticalHeight() As Long
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_miButton As ngMenuItem
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
Dim l_lngWidth As Long
    l_lngWidth = IdealVerticalWidth
    For Each l_miButton In m_miItems
        l_lngIndex = l_lngIndex + 1
        With l_miButton
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
    Next l_miButton
    If IdealVerticalHeight < 4 Then IdealVerticalHeight = 4
End Property

Public Property Get IdealWidth() As Long
On Error Resume Next
    IdealWidth = m_lngIdealWidth
End Property

Public Property Get IdealHeight(Optional ByVal Width As Long = -1) As Long
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_miButton As ngMenuItem
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
Dim l_lngRowHeight
    If Width = -1 Then
        IdealHeight = m_lngIdealHeight
        If IdealHeight < 4 Then IdealHeight = 4
    Else
        For Each l_miButton In m_miItems
            l_lngIndex = l_lngIndex + 1
            With l_miButton
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
        Next l_miButton
        If IdealHeight < 4 Then IdealHeight = 4
    End If
End Property

Public Sub Reflow()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_miButton As ngMenuItem
Dim l_lngRowHeight As Long
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long, l_lngRowStart As Long, l_lngButton As Long
    If Not Form.Ambient.UserMode Then Exit Sub
    m_lngIdealWidth = 0
    m_lngIdealHeight = 0
    For Each l_miButton In m_miItems
        With l_miButton
            m_lngIdealWidth = m_lngIdealWidth + .Width
            If .Height > m_lngIdealHeight Then m_lngIdealHeight = .Height
        End With
    Next l_miButton
    l_lngIndex = 0
    l_lngRowStart = 1
    For Each l_miButton In m_miItems
        With l_miButton
            l_lngIndex = l_lngIndex + 1
            If (l_lngX + .Width) > Form.ScaleWidth Then
                For l_lngButton = l_lngRowStart To l_lngIndex - 1
                    m_miItems(l_lngButton).RowHeight = l_lngRowHeight
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
    Next l_miButton
    If l_lngRowStart <> m_miItems.Count Then
        For l_lngButton = l_lngRowStart To m_miItems.Count
            m_miItems(l_lngButton).RowHeight = l_lngRowHeight
        Next l_lngButton
    End If
    RaiseEvent Reflow
    UpdateMouse
    Redraw
End Sub

Friend Sub RenderButton(ByVal Button As ngMenuItem)
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
                Set Form.Font = .Font
            End If
            SelectObject m_lngDC, GetCurrentObject(Form.hDC, Object_Font)
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

Friend Sub RenderButtonBackground(ByVal Button As ngMenuItem)
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
Dim l_miButton As ngMenuItem
Dim l_fntOld As StdFont
Dim l_lngColor As Long
Dim l_rctArea As Rect
    If DisableUpdates Then Exit Sub
    If Area Is Nothing Then
        Set Area = m_imgSurface.Rectangle
    End If
    If Form.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If m_miItems Is Nothing Then Exit Sub
        Set m_imgSurface.ClipRectangle = Area
        Set l_fntOld = Form.Font
        If EnableTheme Then
'            m_imgSurface.Clear Colors(tbcBackground)
            m_imgSurface.GradientFill m_imgSurface.Rectangle, ThemeBackground
            m_imgSurface.TileBlit m_imgSurface.Rectangle, m_imgTexture(m_tboOrientation), 0.015, BlitMode_Subtractive
        Else
            m_imgSurface.Clear Colors(tbcBackground)
        End If
        For Each l_miButton In m_miItems
            RenderButtonBackground l_miButton
        Next l_miButton
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
        For Each l_miButton In m_miItems
            Set Form.Font = l_fntOld
            RenderButton l_miButton
        Next l_miButton
        Set Form.Font = l_fntOld
        RaiseEvent Redraw
        Repaint Area
    End If
End Sub

Private Sub UpdateMouse()
On Error Resume Next
Dim l_miNewHover As ngMenuItem
    Set l_miNewHover = ButtonFromPoint(m_lngMouseX, m_lngMouseY)
    If l_miNewHover Is m_miHover Then
    Else
        If Not (m_miHover Is Nothing) Then
            m_miHover.MouseLeave
        End If
        If Not (l_miNewHover Is Nothing) Then
            l_miNewHover.MouseEnter
            With l_miNewHover
                imgCurrentButton.Move .Left, .Top, .Width, .Height
                imgCurrentButton.ToolTipText = .Tooltip
                imgCurrentButton.Visible = True
            End With
        Else
            imgCurrentButton.Visible = False
        End If
        Redraw m_miHover.Rectangle
        Set m_miHover = l_miNewHover
        Redraw m_miHover.Rectangle
    End If
End Sub

Public Function ButtonFromPoint(ByVal X As Long, ByVal Y As Long) As ngMenuItem
On Error Resume Next
Dim l_miButton As ngMenuItem
    If m_imgSurface Is Nothing Then Exit Function
    If m_miItems Is Nothing Then Exit Function
    For Each l_miButton In m_miItems
        With l_miButton
            If .Rectangle.PointInside(X, Y) Then
                Set ButtonFromPoint = l_miButton
                Exit For
            End If
        End With
    Next l_miButton
End Function

Public Sub AutoSize()
On Error Resume Next
    Reflow
    Form.Size IdealWidth * Screen.TwipsPerPixelX, IdealHeight * Screen.TwipsPerPixelY
End Sub

Public Sub Repaint(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Form.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If Area Is Nothing Then
            Set Area = m_imgSurface.Rectangle
        End If
        BitBlt Form.hDC, Area.Left, Area.Top, Area.Width, Area.Height, m_lngDC, Area.Left, Area.Top, vbSrcCopy
    Else
        Form.Cls
        Form.Print "ngToolbar: " & Form.Extender.Name
    End If
End Sub

Private Sub imgCurrentButton_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Form_MouseDown Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentButton.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentButton.Top
End Sub

Private Sub imgCurrentButton_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Form_MouseMove Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentButton.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentButton.Top
End Sub

Private Sub imgCurrentButton_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Form_MouseUp Button, Shift, (X / Screen.TwipsPerPixelX) + imgCurrentButton.Left, (Y / Screen.TwipsPerPixelY) + imgCurrentButton.Top
End Sub

Private Sub tmrMouseTracker_Timer()
On Error Resume Next
Dim l_ptMouse As PointAPI
    GetCursorPos l_ptMouse
    ScreenToClient Form.hwnd, l_ptMouse
    m_lngMouseX = l_ptMouse.X
    m_lngMouseY = l_ptMouse.Y
    If (m_lngMouseX < 0) Or (m_lngMouseX >= Form.ScaleWidth) Or (m_lngMouseY < 0) Or (m_lngMouseY >= Form.ScaleHeight) Then
        m_booMouseOver = False
        tmrMouseTracker.Interval = 100
        UpdateMouse
    End If
End Sub

Private Sub Form_Hide()
On Error Resume Next
    FreeSurface
End Sub

Private Sub Form_Initialize()
On Error Resume Next
    F2Init
    InitMetrics
    InitColors
    InitButtons
    ResourcePattern = "*"
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_booCancel As Boolean
    RaiseEvent MouseDown(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_miPressed Is Nothing Then UpdateMouse
    If Button = 1 Then
        Set m_miPressed = ButtonFromPoint(X, Y)
        If m_miPressed Is Nothing Then Exit Sub
        If m_miPressed.Enabled Then
            RaiseEvent ButtonPress(m_miPressed, l_booCancel)
            If Not l_booCancel Then
                m_miPressed.MouseDown
            Else
                m_miHover.MouseLeave
                Set m_miHover = Nothing
                m_miPressed.MouseLeave
                Set m_miPressed = Nothing
                UpdateMouse
                Redraw
            End If
            Redraw m_miPressed.Rectangle
        Else
            Set m_miPressed = Nothing
        End If
    End If
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    MouseEntered
    RaiseEvent MouseMove(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_miPressed Is Nothing Then UpdateMouse
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim m_miHover As ngMenuItem
    RaiseEvent MouseUp(Button, Shift, X, Y)
    If Not (m_miPressed Is Nothing) Then
        Set m_miHover = ButtonFromPoint(X, Y)
        If m_miHover Is m_miPressed Then RaiseEvent ButtonClick(m_miPressed)
        m_miPressed.MouseUp
        Set m_miPressed = Nothing
    End If
    tmrMouseTracker.Interval = IIf(m_booMouseOver, 1, 100)
    UpdateMouse
    Redraw
End Sub

Private Sub Form_Paint()
On Error Resume Next
    Repaint
End Sub

Private Sub Form_Resize()
On Error Resume Next
Dim l_lngIdealWidth As Long
Dim l_lngIdealHeight As Long
    Reflow
    Select Case Form.Extender.Align
    Case vbAlignTop, vbAlignBottom
        If Ambient.UserMode Then
            l_lngIdealHeight = IdealHeight(Form.ScaleWidth)
        Else
            l_lngIdealHeight = Form.TextHeight("AaBbYyZz")
        End If
        If Form.ScaleHeight <> l_lngIdealHeight Then
            Form.Height = l_lngIdealHeight * Screen.TwipsPerPixelY
        End If
    Case vbAlignLeft, vbAlignRight
        If Ambient.UserMode Then
            l_lngIdealWidth = IdealVerticalWidth
        Else
            l_lngIdealWidth = Form.TextWidth("AaBbYyZz")
        End If
        If Form.ScaleWidth <> l_lngIdealWidth Then
            Form.Width = l_lngIdealWidth * Screen.TwipsPerPixelX
        End If
    Case Else
    End Select
    RaiseEvent Resize
    ResizeSurface
    Reflow
End Sub

Private Sub Form_Show()
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

Private Sub Form_Terminate()
On Error Resume Next
    FreeButtons
End Sub

Private Property Get ngMenuParent_Children() As ngMenuItems
    Set ngMenuParent_Children = m_miItems
End Property

Private Property Get ngMenuParent_Form() As Object
    Set ngMenuParent_Form = Me
End Property

Private Property Get ngMenuParent_MenuItem() As ngMenuItem
End Property

Private Sub ngMenuParent_Redraw()
    Redraw
End Sub

Private Sub ngMenuParent_Reflow()
    Reflow
End Sub

Private Property Get ngMenuParent_TextHeight() As Long

End Property

Private Property Get ngMenuParent_TextWidth() As Long

End Property
#End If
