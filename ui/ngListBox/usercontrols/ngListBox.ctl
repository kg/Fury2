VERSION 5.00
Begin VB.UserControl ngListBox 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
End
Attribute VB_Name = "ngListBox"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Event MouseDown(ByRef Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event ButtonPress(Button As ngListItem, ByRef Cancel As Boolean)
Event ButtonClick(Button As ngListItem)
Event Reflow()
Event Resize()
Event Redraw()
Private m_lngMetrics(0 To tbm_max) As Long
Private m_lngColors(0 To tbc_max) As Long
Private m_licListItems As ngListItems
Private m_imgSurface As Fury2Image, m_lngDC As Long, m_lngNullBitmap As Long
Private m_liHover As ngListItem, m_liPressed As ngListItem
Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngIdealWidth As Long, m_lngIdealHeight As Long
Private m_lngRowHeight As Long
Private m_booInitialized As Boolean
Private m_booMouseOver As Boolean
Public ResourceFile As ngResourceFile
Public ResourcePattern As String
Public DisableUpdates As Boolean

Public Property Get Font() As StdFont
    Set Font = UserControl.Font
End Property

Public Property Set Font(ByRef NewFont As StdFont)
On Error Resume Next
    Set UserControl.Font = NewFont
End Property

Public Property Get hWnd() As Long
On Error Resume Next
    hWnd = UserControl.hWnd
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
    TextHeight = UserControl.TextHeight(Text)
    Set UserControl.Font = l_fntOld
End Property

Friend Property Get TextWidth(ByVal TheFont As StdFont, ByRef Text As String) As Long
On Error Resume Next
Dim l_fntOld As StdFont
    Set l_fntOld = UserControl.Font
    Set UserControl.Font = TheFont
    TextWidth = UserControl.TextWidth(Text)
    Set UserControl.Font = l_fntOld
End Property

Public Property Get ListItems() As ngListItems
    Set ListItems = m_licListItems
End Property

Public Property Get Metrics(ByVal Index As ngListBoxMetrics) As Long
    Metrics = m_lngMetrics(Index)
End Property

Public Property Let Metrics(ByVal Index As ngListBoxMetrics, ByVal NewValue As Long)
    m_lngMetrics(Index) = NewValue
End Property

Public Property Get Colors(ByVal Index As ngListBoxColors) As Long
    Colors = m_lngColors(Index)
End Property

Public Property Let Colors(ByVal Index As ngListBoxColors, ByVal NewColor As Long)
    m_lngColors(Index) = NewColor
End Property

Private Property Get ColorOffset(ByVal State As ngListItemstates) As Long
On Error Resume Next
    ColorOffset = (State - ngListItemstates.bstNormal) * (ngListBoxColors.tbcGlow - ngListBoxColors.tbcBackground + 1)
End Property

Private Sub InitMetrics()
On Error Resume Next
    Metrics(lbmButtonMargin) = 2
    Metrics(lbmImageMargin) = 1
    Metrics(lbmTextMargin) = 1
    Metrics(lbmSeparatorMargin) = 2
    Metrics(lbmSeparatorSize) = 1
End Sub

Private Sub InitColors()
On Error Resume Next
    Colors(lbcBackground) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Button_Shadow), 0)
    Colors(lbcBorder) = F2Transparent
    Colors(lbcText) = ConvertSystemColor(SystemColor_Button_Text)
    Colors(lbcTint) = F2RGB(255, 255, 255, 192)
    Colors(lbcGlow) = F2RGB(0, 0, 0, 0)
    Colors(lbcHighlight) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 140)
    Colors(lbcHighlightBorder) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 210)
    Colors(lbcHighlightText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(lbcHighlightTint) = F2White
    Colors(lbcHighlightGlow) = F2RGB(255, 255, 255, 140)
    Colors(lbcPressed) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 192)
    Colors(lbcPressedBorder) = ConvertSystemColor(SystemColor_Highlight)
    Colors(lbcPressedText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(lbcPressedTint) = F2White
    Colors(lbcPressedGlow) = F2RGB(255, 255, 255, 190)
    Colors(lbcDisabled) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Button_Shadow), 0)
    Colors(lbcDisabledBorder) = F2Transparent
    Colors(lbcDisabledText) = ConvertSystemColor(SystemColor_Button_Text_Disabled)
    Colors(lbcDisabledTint) = F2RGB(127, 127, 127, 63)
    Colors(lbcDisabledGlow) = F2RGB(127, 127, 127, 0)
    Colors(lbcChecked) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 140)
    Colors(lbcCheckedBorder) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 210)
    Colors(lbcCheckedText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(lbcCheckedTint) = F2White
    Colors(lbcCheckedGlow) = F2RGB(255, 255, 255, 255)
    Colors(lbcSeparator) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Highlight), 140)
    ' TODO: Initialize other colors
End Sub

Private Sub InitListItems()
On Error Resume Next
    Set m_licListItems = New ngListItems
    m_licListItems.SetParent Me
End Sub

Private Sub FreeListItems()
On Error Resume Next
    m_licListItems.Free
    Set m_licListItems = Nothing
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

Public Sub Reflow()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_liItem As ngListItem
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
    m_lngIdealWidth = 0
    m_lngIdealHeight = 0
    m_lngRowHeight = 0
    For Each l_liItem In m_licListItems
        With l_liItem
            m_lngIdealWidth = m_lngIdealWidth + .Width
            If .Height > m_lngIdealHeight Then m_lngIdealHeight = .Height
            If .Height > m_lngRowHeight Then m_lngRowHeight = .Height
        End With
    Next l_liItem
    l_lngIndex = 0
    For Each l_liItem In m_licListItems
        With l_liItem
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
    Next l_liItem
    RaiseEvent Reflow
    Redraw
End Sub

Friend Sub RenderButton(ByVal Button As ngListItem)
On Error Resume Next
Dim l_rctItem As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
    With Button
        Select Case .Style
        Case bsyNormal, bsyCheck, bsyGroup
            Set l_rctItem = .Rectangle
            l_lngOffset = ColorOffset(.State)
            m_imgSurface.Fill l_rctItem, Colors(lbcBackground + l_lngOffset), RenderMode_Normal
            m_imgSurface.Box l_rctItem, Colors(lbcBorder + l_lngOffset), RenderMode_SourceAlpha
            l_rctItem.Adjust -Metrics(lbmButtonMargin), -Metrics(lbmButtonMargin)
            SetTextColor m_lngDC, SwapChannels(SetAlpha(Colors(lbcText + l_lngOffset), 0), Red, Blue)
            SetBackgroundMode m_lngDC, BackgroundMode_Transparent
            l_lngImageWidth = IIf(.Image Is Nothing, 0, .Image.Width)
            l_lngImageHeight = IIf(.Image Is Nothing, 0, .Image.Height)
            Set l_rctImage = l_rctItem.Copy
            l_rctImage.Width = l_lngImageWidth
            l_rctImage.Height = l_lngImageHeight
            l_rctImage.RelLeft = (l_rctItem.Left) + (l_rctItem.Width - l_lngImageWidth) \ 2
            l_rctImage.RelTop = (l_rctItem.Top) + (l_rctItem.Height - l_lngImageHeight) \ 2
            l_rctText.Left = l_rctItem.Left + Metrics(lbmTextMargin)
            l_rctText.Top = l_rctItem.Top + Metrics(lbmTextMargin)
            l_rctText.Right = l_rctItem.Left + l_rctItem.Width - Metrics(lbmTextMargin)
            l_rctText.Bottom = l_rctItem.Top + l_rctItem.Height - Metrics(lbmTextMargin)
            Select Case .TextAlignment
            Case btaLeft
                l_dtfFlags = DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                l_rctText.Right = l_rctItem.Left + l_rctItem.Width - l_lngImageWidth - Metrics(lbmTextMargin)
                l_rctImage.RelLeft = l_rctItem.Right - l_lngImageWidth - Metrics(lbmImageMargin)
            Case btaRight
                l_dtfFlags = DrawText_Align_Right Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None
                l_rctText.Left = l_rctItem.Left + l_lngImageWidth + Metrics(lbmTextMargin)
                l_rctImage.RelLeft = l_rctItem.Left + Metrics(lbmImageMargin)
            Case btaTop
                l_dtfFlags = DrawText_Align_Top Or DrawText_Align_Center_Horizontal Or DrawText_Wrap_None
                l_rctText.Bottom = l_rctItem.Top + l_rctItem.Height - l_lngImageHeight - Metrics(lbmTextMargin)
                l_rctImage.RelTop = l_rctItem.Bottom - l_lngImageHeight - Metrics(lbmImageMargin)
            Case btaBottom
                l_dtfFlags = DrawText_Align_Bottom Or DrawText_Align_Center_Horizontal Or DrawText_Wrap_None
                l_rctText.Top = l_rctItem.Top + l_lngImageHeight + Metrics(lbmTextMargin)
                l_rctImage.RelTop = l_rctItem.Top + Metrics(lbmImageMargin)
            End Select
            If (.Font Is Nothing) Then
            Else
                Set UserControl.Font = .Font
            End If
            SelectObject m_lngDC, GetCurrentObject(UserControl.hDC, Object_Font)
            DrawText m_lngDC, .Text, Len(.Text), l_rctText, l_dtfFlags
            If GetAlpha(Colors(lbcGlow + l_lngOffset)) > 0 Then
                m_imgSurface.Blit l_rctImage.Copy.Adjust(2, 2), , .GlowImage, IIf(.State = bstChecked, 1, 0.66), BlitMode_Font_SourceAlpha, Colors(lbcGlow + l_lngOffset)
            End If
            m_imgSurface.Blit l_rctImage, , .Image, , BlitMode_SourceAlpha_ColorMask, Colors(lbcTint + l_lngOffset)
        Case bsySeparator
            If .Orientation = borHorizontal Then
                Set l_rctItem = F2Rect(.Left, .Top, .Width, m_lngRowHeight, False)
            Else
                Set l_rctItem = F2Rect(.Left, .Top, UserControl.ScaleWidth, .Width, False)
            End If
            m_imgSurface.Fill l_rctItem, Colors(lbcBackground + l_lngOffset), RenderMode_Normal
            l_rctItem.Adjust -Metrics(lbmSeparatorMargin), -Metrics(lbmSeparatorMargin)
            m_imgSurface.Fill l_rctItem, Colors(lbcSeparator), RenderMode_SourceAlpha
        End Select
    End With
End Sub

Public Sub Redraw()
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_fntOld As StdFont
    If DisableUpdates Then Exit Sub
    If UserControl.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If m_licListItems Is Nothing Then Exit Sub
        Set l_fntOld = UserControl.Font
        m_imgSurface.Clear Colors(lbcBackground)
        For Each l_liItem In m_licListItems
            With l_liItem
                Set UserControl.Font = l_fntOld
                RenderButton l_liItem
            End With
        Next l_liItem
        Set UserControl.Font = l_fntOld
        RaiseEvent Redraw
        Refresh
    End If
End Sub

Private Sub UpdateMouse()
On Error Resume Next
    Set m_liHover = ItemFromPoint(m_lngMouseX, m_lngMouseY)
End Sub

Public Function ItemFromPoint(ByVal X As Long, ByVal Y As Long) As ngListItem
On Error Resume Next
Dim l_liItem As ngListItem
    If m_imgSurface Is Nothing Then Exit Function
    If m_licListItems Is Nothing Then Exit Function
    For Each l_liItem In m_licListItems
        With l_liItem
            If .Rectangle.PointInside(X, Y) Then
                Set ItemFromPoint = l_liItem
                Exit For
            End If
        End With
    Next l_liItem
End Function

Public Sub Refresh()
On Error Resume Next
    If UserControl.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        BitBlt UserControl.hDC, 0, 0, UserControl.ScaleWidth, UserControl.ScaleHeight, m_lngDC, 0, 0, vbSrcCopy
    Else
        UserControl.Cls
        UserControl.Print "ngListBox: " & UserControl.Extender.Name
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
    InitListItems
    ResourcePattern = "*"
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_booCancel As Boolean
    RaiseEvent MouseDown(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_liPressed Is Nothing Then UpdateMouse
    If Button = 1 Then
        tmrMouseTracker.Enabled = False
        Set m_liPressed = ItemFromPoint(X, Y)
        If m_liPressed Is Nothing Then Exit Sub
        If m_liPressed.Enabled Then
            RaiseEvent ButtonPress(m_liPressed, l_booCancel)
            If Not l_booCancel Then
                m_liPressed.MouseDown
            Else
                m_liPressed.MouseLeave
                Set m_liPressed = Nothing
                tmrMouseTracker.Enabled = m_booMouseOver
                UpdateMouse
            End If
            Redraw
        Else
            Set m_liPressed = Nothing
        End If
    End If
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    MouseEntered
    RaiseEvent MouseMove(Button, Shift, X, Y)
    m_lngMouseX = X
    m_lngMouseY = Y
    If m_liPressed Is Nothing Then UpdateMouse
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim m_liHover As ngListItem
    RaiseEvent MouseUp(Button, Shift, X, Y)
    If Not (m_liPressed Is Nothing) Then
        Set m_liHover = ItemFromPoint(X, Y)
        If m_liHover Is m_liPressed Then RaiseEvent ButtonClick(m_liPressed)
        m_liPressed.MouseUp
        Set m_liPressed = Nothing
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
    RaiseEvent Resize
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
    FreeListItems
End Sub

