VERSION 5.00
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
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
   OLEDropMode     =   1  'Manual
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   Begin cFScroll.FlatScrollBar vsScroll 
      Height          =   3600
      Left            =   4605
      TabIndex        =   0
      Top             =   0
      Width           =   195
      _ExtentX        =   344
      _ExtentY        =   6350
      Orientation     =   1
      Max             =   100
      Style           =   -1
   End
End
Attribute VB_Name = "ngListBox"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Declare Function UpdateWindow Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function InvalidateRect Lib "user32" (ByVal hwnd As Long, lpRect As Rect, ByVal bErase As Long) As Long

Event OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single)
Event OLEDragOver(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single, State As Integer)
Event SelectionChange()
Event ItemContextMenu(ByRef Item As ngListItem)
Event ItemSelect(ByRef Item As ngListItem)
Event DragBegin(ByRef Cancel As Boolean)
Event DragMoveItem(ByRef Item As ngListItem, ByVal OldIndex As Long, ByVal NewIndex As Long)
Event DragComplete()
Event MouseDown(ByRef Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
Event Reflow()
Event Resize()
Event Redraw()
Private m_fntFont As StdFont
Private m_licDragItems As ngListItems
Private m_lngMetrics(0 To lbm_max) As Long
Private m_lngColors(0 To lbc_max) As Long
Private m_licListItems As ngListItems
Private m_imgSurface As Fury2Image, m_lngDC As Long, m_lngNullBitmap As Long
Private m_lngHeight As Long
Private m_liFocus As ngListItem, m_liDrag As ngListItem
Private m_lngStartX As Long, m_lngStartY As Long
Private m_booDragged As Boolean
Private m_booInitialized As Boolean
Private m_booMouseOver As Boolean
Private m_booHaveFocus As Boolean
Private m_booVisible As Boolean
Public ResourceFile As ngResourceFile
Public ResourcePattern As String
Public DisableUpdates As Boolean
Public AllowReorder As Boolean
Public AllowMultiSelect As Boolean
Public AllowNullSelection As Boolean

Public Property Get FirstSelectedItem() As ngListItem
On Error Resume Next
Dim l_liItem As ngListItem
    For Each l_liItem In m_licListItems
        If l_liItem.Selected Then
            Set FirstSelectedItem = l_liItem
            Exit Property
        End If
    Next l_liItem
End Property

Public Property Get SelectedItems() As ngListItem()
On Error Resume Next
Dim l_lngCount As Long, l_lngIndex As Long
Dim l_liItem As ngListItem
Dim l_liItems() As ngListItem
    l_lngCount = SelectedItemCount
    If l_lngCount = 0 Then Exit Property
    ReDim l_liItems(0 To l_lngCount - 1)
    For Each l_liItem In m_licListItems
        If l_liItem.Selected Then
            Set l_liItems(l_lngIndex) = l_liItem
            l_lngIndex = l_lngIndex + 1
        End If
    Next l_liItem
    SelectedItems = l_liItems
End Property

Public Property Get SelectedItemCollection() As Collection
On Error Resume Next
Dim l_liItem As ngListItem
    Set SelectedItemCollection = New Collection
    For Each l_liItem In m_licListItems
        If l_liItem.Selected Then
            SelectedItemCollection.Add l_liItem
        End If
    Next l_liItem
End Property

Public Property Get SelectedItemCount() As Long
On Error Resume Next
Dim l_liItem As ngListItem
    For Each l_liItem In m_licListItems
        If l_liItem.Selected Then
            SelectedItemCount = SelectedItemCount + 1
        End If
    Next l_liItem
End Property

Public Property Get DragItems() As ngListItems
On Error Resume Next
    Set DragItems = m_licDragItems
End Property

Public Property Get ScrollY() As Long
On Error Resume Next
    ScrollY = vsScroll.Value
End Property

Public Sub SelectNone(Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_booOldState As Boolean
    l_booOldState = DisableUpdates
    DisableUpdates = True
    For Each l_liItem In m_licListItems
        l_liItem.Selected = False
    Next l_liItem
    If AllowNullSelection Or Force Then
    Else
        m_licListItems(1).Selected = True
        RaiseEvent ItemSelect(m_licListItems(1))
    End If
    RaiseEvent SelectionChange
    DisableUpdates = l_booOldState
    Redraw
End Sub

Public Sub SelectItems(ByVal FirstItem As Long, Optional ByVal LastItem As Long = -1, Optional ByVal Toggle As Boolean = False, Optional ByVal Add As Boolean = False)
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_booOldState As Boolean
Dim l_lngIndex As Long
Dim l_lngTemp As Long
    l_booOldState = DisableUpdates
    DisableUpdates = True
    If ((Not Toggle) And (Not Add)) Or (Not AllowMultiSelect) Then SelectNone True
    If LastItem = -1 Then LastItem = FirstItem
    If Not AllowMultiSelect Then LastItem = FirstItem
    If LastItem < FirstItem Then
        l_lngTemp = FirstItem
        FirstItem = LastItem
        LastItem = l_lngTemp
    End If
    For l_lngIndex = FirstItem To LastItem
        Set l_liItem = m_licListItems(l_lngIndex)
        If Toggle Then
            If l_liItem.Selected Then
                If (SelectedItemCount > 1) Or (AllowNullSelection) Then
                    l_liItem.Selected = False
                End If
            Else
                l_liItem.Selected = True
                If l_liItem.Selected Then RaiseEvent ItemSelect(l_liItem)
            End If
        Else
            l_liItem.Selected = True
            RaiseEvent ItemSelect(l_liItem)
        End If
    Next l_lngIndex
    RaiseEvent SelectionChange
    DisableUpdates = l_booOldState
    Redraw
End Sub

Public Sub SelectAll()
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_booOldState As Boolean
    l_booOldState = DisableUpdates
    DisableUpdates = True
    For Each l_liItem In m_licListItems
        l_liItem.Selected = True
        RaiseEvent ItemSelect(l_liItem)
    Next l_liItem
    RaiseEvent SelectionChange
    DisableUpdates = l_booOldState
    Redraw
End Sub

Public Property Get Font() As StdFont
    Set Font = UserControl.Font
End Property

Public Property Set Font(ByRef NewFont As StdFont)
On Error Resume Next
    Set UserControl.Font = NewFont
End Property

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

Private Property Get ColorOffset(ByVal Selected As Boolean) As Long
On Error Resume Next
    ColorOffset = IIf(Selected, lbcHighlight, lbcBackground)
End Property

Private Sub InitMetrics()
On Error Resume Next
    Metrics(lbmItemMargin) = 0
    Metrics(lbmImageMargin) = 1
    Metrics(lbmTextMargin) = 1
End Sub

Private Sub InitColors()
On Error Resume Next
    Colors(lbcBackground) = ConvertSystemColor(SystemColor_Window)
    Colors(lbcBorder) = F2Transparent
    Colors(lbcText) = ConvertSystemColor(SystemColor_Window_Text)
    Colors(lbcTint) = F2RGB(255, 255, 255, 255)
    Colors(lbcGlow) = F2Transparent
    Colors(lbcHighlight) = ConvertSystemColor(SystemColor_Highlight)
    Colors(lbcHighlightBorder) = F2Transparent
    Colors(lbcHighlightText) = ConvertSystemColor(SystemColor_Highlight_Text)
    Colors(lbcHighlightTint) = F2RGB(255, 255, 255, 127)
    Colors(lbcHighlightGlow) = F2Transparent
    ' TODO: Initialize other colors
End Sub

Private Sub InitListItems()
On Error Resume Next
    Set m_licListItems = New ngListItems
    m_licListItems.SetParent Me
    Set m_licDragItems = New ngListItems
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
Dim l_lngIndex As Long
    If Not m_booVisible Then Exit Sub
    If DisableUpdates Then Exit Sub
    l_lngIndex = 0
    m_lngHeight = 0
    For Each l_liItem In m_licListItems
        With l_liItem
            l_lngIndex = l_lngIndex + 1
            
            .Top = l_lngY
            
            l_lngY = l_lngY + .Height
            m_lngHeight = m_lngHeight + .Height
        End With
    Next l_liItem
    RaiseEvent Reflow
    If m_lngHeight < UserControl.ScaleHeight Then
        vsScroll.Visible = False
        vsScroll.Value = 0
    Else
        vsScroll.Visible = True
        vsScroll.Max = m_lngHeight - UserControl.ScaleHeight
        vsScroll.SmallChange = m_licListItems(1).Height
        vsScroll.LargeChange = m_licListItems(1).Height * 4
    End If
    Redraw
End Sub

Friend Sub RenderItem(ByVal Item As ngListItem)
On Error Resume Next
Dim l_rctItem As Fury2Rect
Dim l_rctImage As Fury2Rect
Dim l_lngOffset As Long
Dim l_lngImageWidth As Long, l_lngImageHeight As Long
Dim l_dtfFlags As DrawTextFlags
Dim l_rctText As Rect
Dim l_rctFocus As Rect
    With Item
        Set l_rctItem = .Rectangle
        If l_rctItem.Intersect(m_imgSurface.ClipRectangle) Then
            l_lngOffset = ColorOffset(.Selected)
            m_imgSurface.Fill l_rctItem, Colors(lbcBackground + l_lngOffset), RenderMode_Normal
            m_imgSurface.Box l_rctItem, Colors(lbcBorder + l_lngOffset), RenderMode_SourceAlpha
            Set l_rctItem = .Rectangle
            l_rctItem.Adjust -Metrics(lbmItemMargin), -Metrics(lbmItemMargin)
            SetTextColor m_lngDC, SwapChannels(SetAlpha(Colors(lbcText + l_lngOffset), 0), Red, Blue)
            SetBackgroundMode m_lngDC, BackgroundMode_Transparent
            If .Image Is Nothing Then
                l_lngImageWidth = 0
                l_lngImageHeight = 0
            Else
                l_lngImageWidth = .Image.Width
                l_lngImageHeight = .Image.Height
            End If
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
                l_dtfFlags = DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None Or DrawText_NoPrefix
                l_rctText.Left = l_rctItem.Left + l_lngImageWidth + Metrics(lbmTextMargin) + Metrics(lbmImageMargin)
                l_rctImage.RelLeft = l_rctItem.Left + Metrics(lbmImageMargin)
            Case btaRight
                l_dtfFlags = DrawText_Align_Right Or DrawText_Align_Center_Vertical Or DrawText_Wrap_None Or DrawText_NoPrefix
                l_rctText.Right = l_rctItem.Left + l_rctItem.Width - l_lngImageWidth - Metrics(lbmTextMargin) - Metrics(lbmImageMargin)
                l_rctImage.RelLeft = l_rctItem.Right - l_lngImageWidth - Metrics(lbmImageMargin)
            Case btaTop
                l_dtfFlags = DrawText_Align_Top Or DrawText_Align_Center_Horizontal Or DrawText_Wrap_None Or DrawText_NoPrefix
                l_rctText.Bottom = l_rctItem.Top + l_rctItem.Height - l_lngImageHeight - Metrics(lbmTextMargin)
                l_rctImage.RelTop = l_rctItem.Bottom - l_lngImageHeight - Metrics(lbmImageMargin)
            Case btaBottom
                l_dtfFlags = DrawText_Align_Bottom Or DrawText_Align_Center_Horizontal Or DrawText_Wrap_None Or DrawText_NoPrefix
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
    '            m_imgSurface.Blit l_rctImage.Copy.Adjust(2, 2), , .GlowImage, IIf(.State = bstChecked, 1, 0.66), BlitMode_Font_SourceAlpha, Colors(lbcGlow + l_lngOffset)
            End If
            m_imgSurface.Blit l_rctImage, , .Image, , BlitMode_SourceAlpha_ColorMask, Colors(lbcTint + l_lngOffset)
            If (m_liFocus Is Item) And (m_booHaveFocus) Then
                With l_rctFocus
                    .Left = l_rctItem.Left
                    .Top = l_rctItem.Top
                    .Right = l_rctItem.Right
                    .Bottom = l_rctItem.Bottom
                End With
                SetBackgroundColor m_lngDC, SetAlpha(SwapChannels(Colors(lbcBackground + l_lngOffset), Red, Blue), 0)
                DrawFocusRect m_lngDC, l_rctFocus
            End If
        End If
    End With
End Sub

Public Sub Redraw(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
Dim l_rctArea As Rect
Dim l_liItem As ngListItem
Dim l_fntOld As StdFont
    If Not m_booVisible Then Exit Sub
    If DisableUpdates Then Exit Sub
    If Area Is Nothing Then
        Set Area = m_imgSurface.Rectangle
    End If
    If UserControl.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If m_licListItems Is Nothing Then Exit Sub
        Set m_imgSurface.ClipRectangle = Area
        m_imgSurface.Clear Colors(lbcBackground)
        For Each l_liItem In m_licListItems
            With l_liItem
                Set UserControl.Font = m_fntFont
                RenderItem l_liItem
            End With
        Next l_liItem
        Set UserControl.Font = m_fntFont
        RaiseEvent Redraw
        Refresh Area
    End If
End Sub

Public Function ItemFromPoint(ByVal X As Long, ByVal Y As Long) As ngListItem
On Error Resume Next
Dim l_liItem As ngListItem
    If m_imgSurface Is Nothing Then Exit Function
    If m_licListItems Is Nothing Then Exit Function
    Y = Y + vsScroll.Value
    For Each l_liItem In m_licListItems
        With l_liItem
            If Y >= .Top And Y < (.Top + .Height) Then
                Set ItemFromPoint = l_liItem
                Exit For
            End If
        End With
    Next l_liItem
End Function

Public Sub Refresh(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not m_booVisible Then Exit Sub
    If UserControl.Ambient.UserMode Then
        If m_imgSurface Is Nothing Then Exit Sub
        If Area Is Nothing Then
            Set Area = m_imgSurface.Rectangle
        End If
        BitBlt UserControl.hDC, Area.Left, Area.Top, Area.Width, Area.Height, m_lngDC, Area.Left, Area.Top, vbSrcCopy
    Else
        UserControl.Cls
        UserControl.Print "ngListBox: " & UserControl.Extender.Name
    End If
End Sub

Private Sub UserControl_EnterFocus()
On Error Resume Next
    If Not m_booVisible Then Exit Sub
    m_booHaveFocus = True
    Reflow
End Sub

Private Sub UserControl_ExitFocus()
On Error Resume Next
    If Not m_booVisible Then Exit Sub
    m_booHaveFocus = False
    Redraw
End Sub

Private Sub UserControl_Hide()
On Error Resume Next
    m_booVisible = False
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

Public Sub BeginDrag(ByRef Item As ngListItem)
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_lngFirstItem As Long
Dim l_booCancel As Boolean
    m_booDragged = False
    If m_licDragItems.Count > 0 Then Exit Sub
    RaiseEvent DragBegin(l_booCancel)
    If l_booCancel Then Exit Sub
    m_licDragItems.Clear
    l_lngFirstItem = Item.Index
    For Each l_liItem In m_licListItems
        With l_liItem
            If .Selected Then
                m_licDragItems.Add l_liItem
                l_liItem.DragPosition = l_liItem.Index - l_lngFirstItem
            End If
        End With
    Next l_liItem
End Sub

Public Sub EndDrag()
On Error Resume Next
Dim l_liItem As ngListItem
    If m_licDragItems.Count < 1 Then Exit Sub
    m_licListItems.Reindex
    RaiseEvent DragComplete
    For Each l_liItem In m_licDragItems
        With l_liItem
            .DragPosition = 0
        End With
    Next l_liItem
    m_licDragItems.Clear
End Sub

Private Sub UserControl_InitProperties()
On Error Resume Next
    Set m_fntFont = UserControl.Ambient.Font
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_booCancel As Boolean
Dim l_booOldState As Boolean
Dim l_booToggle As Boolean
Dim l_liNew As ngListItem
    RaiseEvent MouseDown(Button, Shift, X, Y)
    m_lngStartX = X
    m_lngStartY = Y
'    If m_liFocus Is Nothing Then UpdateMouse
    Set l_liNew = ItemFromPoint(X, Y)
    If Button = 1 Then
        If l_liNew Is Nothing Then Exit Sub
        l_booToggle = (Shift And vbCtrlMask) = vbCtrlMask
        If (l_liNew.Selected) And (AllowReorder) And (Shift = 0) Then
            Set m_liFocus = l_liNew
            BeginDrag l_liNew
        ElseIf (Shift And vbShiftMask) = vbShiftMask Then
            SelectItems m_liFocus.Index, l_liNew.Index, , l_booToggle
        Else
            Set m_liFocus = l_liNew
            SelectItems l_liNew.Index, , l_booToggle
            Redraw
        End If
    ElseIf Button = 2 Then
        RaiseEvent ItemContextMenu(l_liNew)
    End If
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngY As Long
Dim l_liTarget As ngListItem, l_liItem As ngListItem
Dim l_lngItems As Long
Dim l_lngOldIndex As Long, l_lngNewIndex As Long
Dim l_lngDragTarget As Long
    RaiseEvent MouseMove(Button, Shift, X, Y)
    If (Abs(X - m_lngStartX) > 2) Or (Abs(Y - m_lngStartY) > 2) Then
        m_booDragged = True
    End If
    If Button = 1 Then
        If m_licDragItems.Count < 1 Then
        Else
            Set l_liTarget = ItemFromPoint(X, Y)
            l_lngY = Y + vsScroll.Value
            If l_liTarget Is Nothing Then
                If l_lngY <= 0 Then
                    l_lngDragTarget = 1
                Else
                    l_lngDragTarget = m_licListItems.Count
                End If
            Else
                l_lngDragTarget = m_licListItems.Find(l_liTarget)
            End If
            DisableUpdates = True
            For l_lngItems = m_licDragItems.Count To 1 Step -1
                Set l_liItem = m_licDragItems(l_lngItems)
                l_lngOldIndex = m_licListItems.Find(l_liItem)
                l_lngNewIndex = l_lngDragTarget + l_liItem.DragPosition
                If l_lngOldIndex <> l_lngNewIndex Then
                    RaiseEvent DragMoveItem(l_liItem, l_lngOldIndex, l_lngNewIndex)
                    m_licListItems.Move l_lngOldIndex, l_lngNewIndex
                End If
            Next l_lngItems
            For l_lngItems = 1 To m_licDragItems.Count
                Set l_liItem = m_licDragItems(l_lngItems)
                l_lngOldIndex = m_licListItems.Find(l_liItem)
                l_lngNewIndex = l_lngDragTarget + l_liItem.DragPosition
                If l_lngOldIndex <> l_lngNewIndex Then
                    RaiseEvent DragMoveItem(l_liItem, l_lngOldIndex, l_lngNewIndex)
                    m_licListItems.Move l_lngOldIndex, l_lngNewIndex
                End If
            Next l_lngItems
            DisableUpdates = False
            Reflow
        End If
    End If
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_liHover As ngListItem
Dim l_booToggle As Boolean
    RaiseEvent MouseUp(Button, Shift, X, Y)
    EndDrag
    If Not m_booDragged Then
        Set l_liHover = ItemFromPoint(X, Y)
        If l_liHover.Selected Then
            If Shift = 0 Then
                SelectItems l_liHover.Index
            End If
        End If
    End If
    Redraw
End Sub

Private Sub UserControl_OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    RaiseEvent OLEDragDrop(Data, Effect, Button, Shift, X, Y)
End Sub

Private Sub UserControl_OLEDragOver(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single, State As Integer)
On Error Resume Next
    RaiseEvent OLEDragOver(Data, Effect, Button, Shift, X, Y, State)
End Sub

Private Sub UserControl_Paint()
On Error Resume Next
    If Not m_booVisible Then Exit Sub
    Refresh
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
On Error Resume Next
    Set m_fntFont = PropBag.ReadProperty("Font", UserControl.Ambient.Font)
    AllowReorder = PropBag.ReadProperty("AllowReorder", False)
    AllowMultiSelect = PropBag.ReadProperty("AllowMultiSelect", False)
    AllowNullSelection = PropBag.ReadProperty("AllowNullSelection", False)
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
Dim l_lngWidth As Long
    If Not m_booVisible Then Exit Sub
    l_lngWidth = GetScrollbarSize(vsScroll) + 1
    vsScroll.Move UserControl.ScaleWidth - l_lngWidth, 0, l_lngWidth, UserControl.ScaleHeight
    RaiseEvent Resize
    ResizeSurface
    Reflow
End Sub

Private Sub UserControl_Show()
On Error Resume Next
    m_booVisible = True
    InitSurface
    UserControl_Resize
    Reflow
End Sub

Private Sub UserControl_Terminate()
On Error Resume Next
    m_booVisible = False
    FreeListItems
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
On Error Resume Next
    PropBag.WriteProperty "Font", m_fntFont
    PropBag.WriteProperty "AllowReorder", AllowReorder
    PropBag.WriteProperty "AllowMultiSelect", AllowMultiSelect
    PropBag.WriteProperty "AllowNullSelection", AllowNullSelection
End Sub

Private Sub vsScroll_Change()
On Error Resume Next
    Redraw
End Sub

Public Property Get ScaleWidth() As Long
    ScaleWidth = UserControl.ScaleWidth - IIf(vsScroll.Visible, vsScroll.Width, 0)
End Property

Public Property Get ScaleHeight() As Long
    ScaleHeight = UserControl.ScaleHeight
End Property

