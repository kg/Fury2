VERSION 5.00
Begin VB.UserControl ngToolbar 
   Alignable       =   -1  'True
   ClientHeight    =   465
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   2430
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
      Interval        =   1
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
Event MouseDown(ByRef Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event ButtonClick(Button As ngToolButton)
Event ButtonDropDown(Button As ngToolButton)
Event Resize()
Event Redraw()
Private m_lngMetrics(0 To tbm_max) As Long
Private m_lngColors(0 To tbc_max) As Long
Private m_tbcButtons As ngToolButtons
Private m_imgSurface As Fury2Image, m_lngDC As Long, m_lngNullBitmap As Long
Private m_btnHover As ngToolButton, m_btnPressed As ngToolButton
Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngIdealWidth As Long, m_lngIdealHeight As Long
Private m_lngRowHeight As Long
Private m_booInitialized As Boolean
Private m_booMouseOver As Boolean

Private Sub MouseEntered()
On Error Resume Next
    If m_booMouseOver Then Exit Sub
    tmrMouseTracker.Enabled = True
End Sub

Public Property Get hWnd() As Long
On Error Resume Next
    hWnd = UserControl.hWnd
End Property

Public Property Get hDC() As Long
On Error Resume Next
    hDC = UserControl.hDC
End Property

Friend Property Get TextHeight(ByRef Text As String) As Long
    TextHeight = UserControl.TextHeight(Text)
End Property

Friend Property Get TextWidth(ByRef Text As String) As Long
    TextWidth = UserControl.TextWidth(Text)
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
    ColorOffset = (State - ngToolButtonStates.bstNormal) * (ngToolbarColors.tbcBorder - ngToolbarColors.tbcBackground + 1)
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
    Colors(tbcHighlight) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 140)
    Colors(tbcHighlightBorder) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 210)
    Colors(tbcHighlightText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(tbcHighlightTint) = F2White
    Colors(tbcPressed) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 192)
    Colors(tbcPressedBorder) = ConvertSystemColor(SystemColor_Highlight)
    Colors(tbcPressedText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(tbcPressedTint) = F2White
    Colors(tbcDisabled) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Button_Shadow), 0)
    Colors(tbcDisabledBorder) = F2Transparent
    Colors(tbcDisabledText) = ConvertSystemColor(SystemColor_Button_Text_Disabled)
    Colors(tbcDisabledTint) = F2RGB(127, 127, 127, 127)
    Colors(tbcChecked) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 140)
    Colors(tbcCheckedBorder) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 210)
    Colors(tbcCheckedText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(tbcCheckedTint) = F2White
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
    m_lngDC = CreateCompatibleDC(UserControl.hDC)
    Set m_imgSurface = F2DIBSection(UserControl.ScaleWidth, UserControl.ScaleHeight, m_lngDC)
    m_lngNullBitmap = SelectObject(m_lngDC, m_imgSurface.DIBHandle)
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
Dim l_lngX As Long, l_lngY As Long
Dim l_btnButton As ngToolButton
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
    If Width = -1 Then
        IdealHeight = m_lngIdealHeight
        If IdealHeight < 4 Then IdealHeight = 4
    Else
        For Each l_btnButton In m_tbcButtons
            l_lngIndex = l_lngIndex + 1
            With l_btnButton
                If (l_lngX + .Width) > Width Then
                    l_lngX = 0
                    l_lngY = l_lngY + m_lngRowHeight
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
            End With
        Next l_btnButton
        If IdealHeight < 4 Then IdealHeight = 4
    End If
End Property

Public Sub Reflow()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_btnButton As ngToolButton
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
    m_lngIdealWidth = 0
    m_lngIdealHeight = 0
    m_lngRowHeight = 0
    For Each l_btnButton In m_tbcButtons
        With l_btnButton
            m_lngIdealWidth = m_lngIdealWidth + .Width
            If .Height > m_lngIdealHeight Then m_lngIdealHeight = .Height
            If .Height > m_lngRowHeight Then m_lngRowHeight = .Height
        End With
    Next l_btnButton
    l_lngIndex = 0
    For Each l_btnButton In m_tbcButtons
        With l_btnButton
            l_lngIndex = l_lngIndex + 1
            If (l_lngX + .Width) > UserControl.ScaleWidth Then
                l_lngX = 0
                l_lngY = l_lngY + m_lngRowHeight
                l_booNewRow = True
            End If
            
            If (.Style = bsySeparator) Then
                .Orientation = IIf(l_booNewRow, borVertical, borHorizontal)
            End If
            
            .Left = l_lngX
            .Top = l_lngY
            
            l_lngX = l_lngX + .Width
            l_booNewRow = False
            If (.Orientation = borVertical) Then
                l_lngY = l_lngY + .Height
                l_lngX = 0
                l_booNewRow = True
            End If
        End With
    Next l_btnButton
    Redraw
End Sub

Friend Sub RenderButton(ByVal Button As ngToolButton)
On Error Resume Next
Dim l_rctButton As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
    With Button
        Select Case .Style
        Case bsyNormal, bsyCheck, bsyGroup, bsyDropdown
            Set l_rctButton = .Rectangle
            l_lngOffset = ColorOffset(.State)
            m_imgSurface.Fill l_rctButton, Colors(tbcBackground + l_lngOffset), RenderMode_Normal
            m_imgSurface.Box l_rctButton, Colors(tbcBorder + l_lngOffset), RenderMode_SourceAlpha
            l_rctButton.Adjust -Metrics(tbmButtonMargin), -Metrics(tbmButtonMargin)
            SetTextColor m_lngDC, SwapChannels(SetAlpha(Colors(tbcText + l_lngOffset), 0), Red, Blue)
            SetBackgroundMode m_lngDC, BackgroundMode_Transparent
            l_lngImageWidth = IIf(.Image Is Nothing, 0, .Image.Width)
            l_lngImageHeight = IIf(.Image Is Nothing, 0, .Image.Height)
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
            DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
            m_imgSurface.Blit l_rctImage, , .Image, , BlitMode_SourceAlpha_ColorMask, Colors(tbcTint + l_lngOffset)
        Case bsySeparator
            If .Orientation = borHorizontal Then
                Set l_rctButton = F2Rect(.Left, .Top, .Width, m_lngRowHeight, False)
            Else
                Set l_rctButton = F2Rect(.Left, .Top, UserControl.ScaleWidth, .Width, False)
            End If
            m_imgSurface.Fill l_rctButton, Colors(tbcBackground + l_lngOffset), RenderMode_Normal
            l_rctButton.Adjust -Metrics(tbmSeparatorMargin), -Metrics(tbmSeparatorMargin)
            m_imgSurface.Fill l_rctButton, Colors(tbcSeparator), RenderMode_SourceAlpha
        End Select
    End With
End Sub

Public Sub Redraw()
On Error Resume Next
Dim l_btnButton As ngToolButton
    If m_imgSurface Is Nothing Then Exit Sub
    If m_tbcButtons Is Nothing Then Exit Sub
    m_imgSurface.Clear Colors(tbcBackground)
    SelectObject m_lngDC, GetCurrentObject(UserControl.hDC, Object_Font)
    For Each l_btnButton In m_tbcButtons
        With l_btnButton
            RenderButton l_btnButton
        End With
    Next l_btnButton
    Refresh
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
        Set m_btnHover = l_btnNewHover
        Redraw
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

Public Sub Refresh()
On Error Resume Next
    If m_imgSurface Is Nothing Then Exit Sub
    BitBlt UserControl.hDC, 0, 0, UserControl.ScaleWidth, UserControl.ScaleHeight, m_lngDC, 0, 0, vbSrcCopy
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
    ScreenToClient UserControl.hWnd, l_ptMouse
    m_lngMouseX = l_ptMouse.X
    m_lngMouseY = l_ptMouse.Y
    If (m_lngMouseX < 0) Or (m_lngMouseX >= UserControl.ScaleWidth) Or (m_lngMouseY < 0) Or (m_lngMouseY >= UserControl.ScaleHeight) Then
        m_booMouseOver = False
        tmrMouseTracker.Enabled = False
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
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    RaiseEvent MouseDown(Button, Shift, X, Y)
    If Button = 1 Then
        tmrMouseTracker.Enabled = False
        Set m_btnPressed = ButtonFromPoint(X, Y)
        If m_btnPressed.Enabled Then
            m_btnPressed.MouseDown
            Redraw
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
    RaiseEvent MouseUp(Button, Shift, X, Y)
    If Not (m_btnPressed Is Nothing) Then
        RaiseEvent ButtonClick(m_btnPressed)
        m_btnPressed.MouseUp
        Set m_btnPressed = Nothing
    End If
    tmrMouseTracker.Enabled = m_booMouseOver
    UpdateMouse
    Redraw
End Sub

Private Sub UserControl_Paint()
On Error Resume Next
    Refresh
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
Dim l_lngIdealHeight As Long
    If UserControl.Extender.Align <> 0 Then
        l_lngIdealHeight = IdealHeight(UserControl.ScaleWidth)
        If UserControl.ScaleHeight <> l_lngIdealHeight Then
            UserControl.Height = l_lngIdealHeight * Screen.TwipsPerPixelY
        End If
    End If
    ResizeSurface
    Reflow
End Sub

Private Sub UserControl_Show()
On Error Resume Next
    InitSurface
    Reflow
End Sub

Private Sub UserControl_Terminate()
On Error Resume Next
    FreeButtons
End Sub
