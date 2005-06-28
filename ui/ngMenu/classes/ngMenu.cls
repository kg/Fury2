VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngMenu"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Declare Function SetFocus Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function SetForegroundWindow Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function ShowWindow Lib "user32" (ByVal hwnd As Long, ByVal nCmdShow As Long) As Long
Private Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
Private Declare Function GetKeyState Lib "user32" (ByVal nVirtKey As Long) As Integer
Private Const SW_SHOWNOACTIVATE As Long = 4
Private Const KEY_PRESSED As Long = &H1000
Private Const KEY_TOGGLED As Long = &H1
Private Const VK_LBUTTON As Long = &H1
Private Const VK_RBUTTON As Long = &H2
Private Const GWL_STYLE As Long = (-16)
Private Const WS_VISIBLE As Long = &H10000000
Private Const HWND_TOP = 0
Private Const SWP_NOACTIVATE = &H10
Private Const SWP_SHOWWINDOW = &H40
Private Const SWP_NOZORDER = &H4

Event MouseDown(ByRef Button As Integer, ByVal Shift As Integer, ByVal x As Single, ByVal y As Single)
Event MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal x As Single, ByVal y As Single)
Event MouseUp(ByVal Button As Integer, ByVal Shift As Integer, ByVal x As Single, ByVal y As Single)
Event ItemClick(Item As ngMenuItem)
Event ItemPress(Item As ngMenuItem, ByRef Cancel As Boolean)
Event ItemHover(Item As ngMenuItem)
Event Reflow()
Event Show()
Event Hide()
Private m_booClosed As Boolean
Private m_lngMetrics(0 To mnm_max) As Long
Private m_lngColors(0 To mnc_max) As Long
Private m_miItems As ngMenuItems
Private m_miHover As ngMenuItem
Private m_miSelected As ngMenuItem
Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngIdealWidth As Long, m_lngIdealHeight As Long
Private m_booInitialized As Boolean
Private m_booMouseOver As Boolean, m_booMouseTouched As Boolean
Private m_booVisible As Boolean, m_lngParentWindow As Long
Private m_evtShow As ngEvent
Private m_evtSelect As ngEvent
Private m_evtHide As ngEvent
Public Parent As ngMenu
Public ParentItem As ngMenuItem
Public Font As StdFont
Public Host As ngMenuHost
Public ResourceFile As ngResourceFile
Public ResourcePattern As String
Public DisableUpdates As Boolean
Public Tag As Variant

Public Property Get SelectEvent() As ngEvent
    Set SelectEvent = m_evtSelect
End Property

Public Property Set SelectEvent(ByVal NewValue As ngEvent)
    Set m_evtSelect = NewValue
End Property

Public Property Get ShowEvent() As ngEvent
    Set ShowEvent = m_evtShow
End Property

Public Property Set ShowEvent(ByVal NewValue As ngEvent)
    Set m_evtShow = NewValue
End Property

Public Property Get HideEvent() As ngEvent
    Set HideEvent = m_evtHide
End Property

Public Property Set HideEvent(ByVal NewValue As ngEvent)
    Set m_evtHide = NewValue
End Property

Public Property Get ParentWindow() As Long
    ParentWindow = m_lngParentWindow
End Property

Public Property Get Items() As ngMenuItems
    Set Items = m_miItems
End Property

Public Property Get Metrics(ByVal Index As ngMenuMetrics) As Long
    Metrics = m_lngMetrics(Index)
End Property

Public Property Let Metrics(ByVal Index As ngMenuMetrics, ByVal NewValue As Long)
    m_lngMetrics(Index) = NewValue
End Property

Public Property Get Colors(ByVal Index As ngMenuColors) As Long
    Colors = m_lngColors(Index)
End Property

Public Property Let Colors(ByVal Index As ngMenuColors, ByVal NewColor As Long)
    m_lngColors(Index) = NewColor
End Property

Private Sub InitMetrics()
On Error Resume Next
    Metrics(mnmItemMargin) = 2
    Metrics(mnmImageMargin) = 1
    Metrics(mnmTextMargin) = 1
    Metrics(mnmSeparatorMargin) = 2
    Metrics(mnmSeparatorSize) = 1
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
    Colors(tbcSeparator) = BlendColors(ConvertSystemColor(SystemColor_Button_Face), ConvertSystemColor(SystemColor_Button_Shadow), 192)
    ' TODO: Initialize other colors
End Sub

Private Sub InitItems()
On Error Resume Next
    Set m_miItems = New ngMenuItems
    m_miItems.SetParent Me
End Sub

Private Sub FreeItems()
On Error Resume Next
    m_miItems.Free
    Set m_miItems = Nothing
End Sub

Public Property Get IdealWidth() As Long
On Error Resume Next
    IdealWidth = m_lngIdealWidth
End Property

Public Property Get IdealHeight(Optional ByVal Width As Long = -1) As Long
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_miItem As ngMenuItem
Dim l_booNewRow As Boolean
Dim l_lngIndex As Long
    If Width = -1 Then
        IdealHeight = m_lngIdealHeight
        If IdealHeight < 4 Then IdealHeight = 4
    Else
        For Each l_miItem In m_miItems
            l_lngIndex = l_lngIndex + 1
            With l_miItem
                IdealHeight = IdealHeight + .Height
            End With
        Next l_miItem
        If IdealHeight < 4 Then IdealHeight = 4
    End If
End Property

Public Sub Reflow()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_miItem As ngMenuItem
Dim l_lngIndex As Long, l_lngItem As Long
Dim l_lngImageWidth As Long
    m_lngIdealWidth = 0
    m_lngIdealHeight = 0
    For Each l_miItem In m_miItems
        With l_miItem
            If .Image Is Nothing Then
            Else
                If .Image.Width > l_lngImageWidth Then
                    l_lngImageWidth = .Image.Width
                End If
            End If
        End With
    Next l_miItem
    For Each l_miItem In m_miItems
        With l_miItem
            .ImageColumnWidth = l_lngImageWidth
            If .Width > m_lngIdealWidth Then m_lngIdealWidth = .Width
            m_lngIdealHeight = m_lngIdealHeight + .Height
        End With
    Next l_miItem
    l_lngIndex = 0
    For Each l_miItem In m_miItems
        With l_miItem
            l_lngIndex = l_lngIndex + 1
            
            .Left = l_lngX
            .Top = l_lngY
            
            l_lngY = l_lngY + .Height
        End With
    Next l_miItem
    RaiseEvent Reflow
    UpdateMouse
    Host.Redraw
End Sub

Friend Sub UpdateMouse()
On Error Resume Next
Dim l_miNewHover As ngMenuItem
    Set l_miNewHover = ItemFromPoint(m_lngMouseX, m_lngMouseY)
    If l_miNewHover Is m_miHover Then
    Else
        If Not (m_miHover Is Nothing) Then
            m_miHover.MouseLeave
        End If
        If Not (l_miNewHover Is Nothing) Then
            l_miNewHover.MouseEnter
        Else
        End If
        Host.Redraw m_miHover.Rectangle
        Set m_miHover = l_miNewHover
        RaiseEvent ItemHover(m_miHover)
        Host.Redraw m_miHover.Rectangle
        DoShowChildMenu
    End If
End Sub

Public Function ItemFromPoint(ByVal x As Long, ByVal y As Long) As ngMenuItem
On Error Resume Next
Dim l_miItem As ngMenuItem
    If m_miItems Is Nothing Then Exit Function
    For Each l_miItem In m_miItems
        With l_miItem
            If .Rectangle.PointInside(x, y) Then
                Set ItemFromPoint = l_miItem
                Exit For
            End If
        End With
    Next l_miItem
End Function

Public Sub AutoSize()
On Error Resume Next
    Reflow
    Host.SetSize IdealWidth, IdealHeight
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    F2Init
    InitMetrics
    InitColors
    InitItems
    Set Font = New StdFont
    Font.Name = "Tahoma"
    Font.Size = 9
    ResourcePattern = "*"
End Sub

Friend Sub Event_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
Dim l_lngIndex As Long
Dim l_miNewHover As ngMenuItem
Dim l_miItem As ngMenuItem
Dim l_lngCount As Long
Dim l_booSelect As Boolean
Dim l_colItems As Collection
    Select Case KeyCode
    Case vbKeyUp
        l_lngIndex = 2
        l_lngIndex = m_miHover.Index
        Do
            l_lngIndex = WrapValue(l_lngIndex - 1, 1, Items.Count)
            Set l_miNewHover = Items(l_lngIndex)
            If l_miNewHover Is m_miHover Then Exit Do
        Loop Until l_miNewHover.Enabled
    Case vbKeyDown
        l_lngIndex = 0
        l_lngIndex = m_miHover.Index
        Do
            l_lngIndex = WrapValue(l_lngIndex + 1, 1, Items.Count)
            Set l_miNewHover = Items(l_lngIndex)
            If l_miNewHover Is m_miHover Then Exit Do
        Loop Until l_miNewHover.Enabled
    Case vbKeyLeft
        If Parent Is Nothing Then
        Else
            Hide
        End If
    Case vbKeyRight
        DoShowChildMenu
    Case vbKeyReturn, vbKeySpace
        l_booSelect = True
    Case Else
        Set l_colItems = New Collection
        For Each l_miItem In Items
            If l_miItem.Enabled Then
                If l_miItem.Hotkey = KeyCode Then
                    l_lngCount = l_lngCount + 1
                    l_colItems.Add l_miItem
                End If
            End If
        Next l_miItem
        If l_lngCount = 1 Then
            l_booSelect = True
            Set l_miNewHover = l_colItems(1)
        ElseIf l_lngCount > 1 Then
            For l_lngIndex = 1 To l_colItems.Count
                If m_miHover Is l_colItems(l_lngIndex) Then
                    Set l_miNewHover = l_colItems(WrapValue(l_lngIndex + 1, 1, l_colItems.Count))
                    Exit For
                End If
            Next l_lngIndex
        End If
    End Select
    If l_miNewHover Is m_miHover Then
    ElseIf l_miNewHover Is Nothing Then
    Else
        If Not (m_miHover Is Nothing) Then
            m_miHover.MouseLeave
        End If
        l_miNewHover.MouseEnter
        SetCursorPos Host.Left + Host.Width - 6, Host.Top + l_miNewHover.Top + (l_miNewHover.Height \ 2)
        Host.Redraw m_miHover.Rectangle
        Set m_miHover = l_miNewHover
        Host.Redraw m_miHover.Rectangle
    End If
    If l_booSelect Then
        Event_ItemPress
        Event_ItemRelease
    End If
End Sub

Friend Sub Event_ItemPress()
On Error Resume Next
Dim l_booCancel As Boolean
    If m_miHover.Enabled Then
        RaiseEvent ItemPress(m_miHover, l_booCancel)
        If Not l_booCancel Then
            m_miHover.MouseDown
        Else
            m_miHover.MouseLeave
            Set m_miHover = Nothing
            UpdateMouse
            Host.Redraw
        End If
        Host.Redraw m_miHover.Rectangle
    Else
        Set m_miHover = Nothing
    End If
End Sub

Friend Sub Event_ItemRelease()
On Error Resume Next
    If Not m_miHover Is Nothing Then
        If m_miHover.Enabled = False Then
        Else
            If m_miHover.ChildMenu Is Nothing Then
                ItemSelected m_miHover
                RaiseEvent ItemClick(m_miHover)
                m_miHover.MouseUp
                DoClose
            Else
                ItemSelected m_miHover
                m_miHover.MouseUp
                DoShowChildMenu
            End If
        End If
    End If
End Sub

Public Sub Hide()
On Error Resume Next
    If GetCapture = Host.hwnd Then
        ReleaseCapture
    End If
    Host.This.Hide
    SetFocus Parent.Host.hwnd
    m_booVisible = False
End Sub

Friend Sub DoClose()
On Error Resume Next
    m_booClosed = True
    ReleaseCapture
    Hide
    Parent.DoClose
    ResetItems
End Sub

Friend Sub DoShowChildMenu()
On Error Resume Next
    If Host.This.ChildMenu Is m_miHover.ChildMenu Then
    Else
        If GetCapture = Host.This.ChildMenu.Host.hwnd Then
            ReleaseCapture
        End If
        Host.This.ChildMenu.Hide
        Set Host.This.ChildMenu = m_miHover.ChildMenu
        If Host.This.ChildMenu Is Nothing Then
        Else
            m_miHover.ChildMenu.Show Host.Left + Host.Width, Host.Top + m_miHover.Top, Host.hwnd, False, , False
        End If
    End If
End Sub

Friend Sub Event_KeyPress(KeyAscii As Integer)
On Error Resume Next
End Sub

Friend Sub Event_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
End Sub

Friend Sub Event_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
    RaiseEvent MouseDown(Button, Shift, x, y)
    m_lngMouseX = x
    m_lngMouseY = y
    UpdateMouse
    If (x >= 0) And (y >= 0) And (x < Host.Width) And (y < Host.Height) Then
        m_booMouseTouched = True
    End If
    If Button = 1 Then
        Event_ItemPress
    End If
End Sub

Friend Sub SetMousePosition(ByVal x As Long, ByVal y As Long)
On Error Resume Next
    m_lngMouseX = x
    m_lngMouseY = y
End Sub

Friend Sub Event_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
Dim m_miHover As ngMenuItem
Dim l_booCancel As Boolean
    m_lngMouseX = x
    m_lngMouseY = y
    RaiseEvent MouseMove(Button, Shift, x, y)
    If (x >= 0) And (y >= 0) And (x < Host.Width) And (y < Host.Height) Then
        m_booMouseTouched = True
        If Host.This.ChildMenu Is Nothing Then
        Else
            'Host.This.ChildMenu.Hide
        End If
    Else
        If GetCapture = Host.hwnd Then
            Host.This.ChildMenu.DoStealCapture
            Parent.DoStealCapture
        Else
            DoStealCapture
        End If
    End If
    UpdateMouse
    Host.Redraw
End Sub

Friend Sub Event_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
On Error Resume Next
Dim m_miHover As ngMenuItem
    RaiseEvent MouseUp(Button, Shift, x, y)
    Set m_miHover = ItemFromPoint(x, y)
    If m_miHover Is Nothing Then
        'If m_booMouseTouched Then Hide
    Else
        Event_ItemRelease
    End If
    UpdateMouse
    Host.Redraw
    If (x >= 0) And (y >= 0) And (x < Host.Width) And (y < Host.Height) Then
        If Host.This.ChildMenu Is Nothing Then
            'DoClose
        End If
    End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    FreeItems
End Sub

Friend Sub DoStealCapture()
On Error Resume Next
Dim l_ptMouse As PointAPI
    GetCursorPos l_ptMouse
    ScreenToClient Host.hwnd, l_ptMouse
    If (l_ptMouse.x >= 0) And (l_ptMouse.y >= 0) And (l_ptMouse.x < Host.Width) And (l_ptMouse.y < Host.Height) Then
        If GetCapture <> Host.hwnd Then
            SetCapture Host.hwnd
        End If
    End If
End Sub

Friend Sub ResetItems()
On Error Resume Next
Dim l_miItem As ngMenuItem
    Set m_miHover = Nothing
    For Each l_miItem In Items
        l_miItem.MouseUp
        l_miItem.MouseLeave
    Next
End Sub

Public Property Get Visible() As Boolean
    Visible = m_booVisible
End Property

Public Function Show(Optional ByVal x As Long = -32767, Optional ByVal y As Long = -32767, Optional ByVal Parent As Long = 0, Optional ByVal AutoCapture As Boolean = True, Optional ByVal Absolute As Boolean = True, Optional ByVal CloseOtherMenus As Boolean = True) As ngMenuItem
On Error Resume Next
Dim l_ptMouse As PointAPI
Dim l_lngWindow As Long
Dim l_lngOldParent As Long
Dim l_mnuMenu As ngMenu
    If m_booVisible Then Exit Function
    If CloseOtherMenus Then
        For Each l_mnuMenu In g_colOpenMenus
            l_mnuMenu.DoClose
        Next l_mnuMenu
        Set g_colOpenMenus = New Collection
    End If
    If Me.Parent Is Nothing Then
        l_lngWindow = GetActiveWindow()
    Else
        l_lngWindow = Me.Parent.ParentWindow
        If l_lngWindow = 0 Then
            l_lngWindow = GetActiveWindow()
        End If
    End If
    If Parent = 0 Then
        Parent = l_lngWindow
    End If
    m_lngParentWindow = Parent
    Set m_miSelected = Nothing
    GetCursorPos l_ptMouse
    If x = -32767 Then x = l_ptMouse.x
    If y = -32767 Then y = l_ptMouse.y
    If Absolute Then
    Else
        l_ptMouse.x = x
        l_ptMouse.y = y
        ClientToScreen Parent, l_ptMouse
        x = l_ptMouse.x
        y = l_ptMouse.y
    End If
    m_booClosed = False
    m_booVisible = True
    m_evtShow.Invoke
    RaiseEvent Show
    ResetItems
    Set Host = New frmMenu
    Set Host.This.Menu = Me
    Host.This.Visible = False
    Load Host.This
    Host.This.Visible = False
    Reflow
    Host.This.Move x * Screen.TwipsPerPixelX, y * Screen.TwipsPerPixelY, (IdealWidth * Screen.TwipsPerPixelX) + Host.This.BorderWidth * 2, (IdealHeight * Screen.TwipsPerPixelY) + Host.This.BorderWidth * 2
    Host.This.SetParent Parent
    SetFocus Host.hwnd
'    ShowWindow Host.hwnd, SW_SHOWNOACTIVATE
    Err.Clear
    g_colOpenMenus.Add Me
    SetWindowPos Host.hwnd, HWND_TOP, x, y, IdealWidth + (Host.This.BorderWidth * 2), IdealHeight + (Host.This.BorderWidth * 2), SWP_NOACTIVATE Or SWP_SHOWWINDOW
    Host.This.HasFocus = True
    m_booMouseTouched = False
    If (GetKeyState(VK_LBUTTON) And KEY_PRESSED) Or (GetKeyState(VK_RBUTTON) And KEY_PRESSED) Then
        If AutoCapture Then
            ReleaseCapture
            SetCapture Host.hwnd
            GetCursorPos l_ptMouse
            Event_MouseDown 1, 0, l_ptMouse.x - x, l_ptMouse.y - y
        End If
    End If
    Do While m_booVisible
        If Host.This.Visible = False Then
            m_booVisible = False
        End If
        Tick
        DoEvents
        Sleep 1
    Loop
    Set Show = m_miSelected
    Set m_miSelected = Nothing
    If Show.Parent Is Me Then
        m_evtSelect.Invoke Show
    End If
    m_evtHide.Invoke
    RaiseEvent Hide
    Set Me.Parent.Host.This.ChildMenu = Nothing
    Host.This.ChildMenu.Hide
    Hide
    m_lngParentWindow = 0
    Set Host.This.Menu = Nothing
    Set Host = Nothing
    If Me.Parent Is Nothing Then
    ElseIf Not m_booClosed Then
        Me.Parent.Host.This.HasFocus = True
        SetFocus Me.Parent.Host.hwnd
        SetCapture Me.Parent.Host.hwnd
    End If
    m_booVisible = False
    ResetItems
End Function

Friend Sub Tick()
On Error Resume Next
Dim l_lngFlags As Long
    l_lngFlags = GetWindowLong(m_lngParentWindow, GWL_STYLE)
    If (l_lngFlags And WS_VISIBLE) = WS_VISIBLE Then
    Else
        m_booVisible = False
    End If
    If Host.This.ChildMenu Is Nothing Then
        If Host.This.HasFocus Then
        Else
            DoClose
            m_booVisible = False
        End If
    Else
        Host.This.ChildMenu.Tick
    End If
End Sub

Friend Sub ItemSelected(ByRef Item As ngMenuItem)
On Error Resume Next
    Set m_miSelected = Item
    Parent.ItemSelected Item
End Sub
