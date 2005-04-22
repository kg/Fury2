VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ListBox"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements iWidget
Implements iItemParent
Public Visible As Boolean
Public Parent As Object
Public EventHandler As Object
Public Name As String
Public AutoSizeHeight As Boolean
Public SelectedItem As Long
Private m_colItems As ItemCollection
Private m_staState As SkinState
Private m_sbrScrollbar As Scrollbar
Private m_lngTotalHeight As Long
Public Skin As Skin
Public Rectangle As Fury2Rect
Public HaveFocus As Boolean
Public ScrollOffset As Long
Private m_wdgMe As iWidget
Private m_engEngine As Fury2Engine

Public Sub ScrollToItem(ByVal Item As Long)
On Error Resume Next
    iWidget_Redraw Nothing, Nothing, 0, 0, 1
    ScrollOffset = GetItemY(Item)
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Friend Sub SetEngine(NewEngine As Fury2Engine)
    Set m_engEngine = NewEngine
End Sub

Public Property Get Scrollbar() As Scrollbar
    Set Scrollbar = m_sbrScrollbar
End Property

Public Function GetItemY(ByVal Item As Long) As Long
Dim l_itmItem As Item
Dim l_lngItem As Long
Dim l_lngItemHeight As Long
Dim l_lngY As Long
Dim l_lngWidth As Long
    If (m_sbrScrollbar.Visible) Then
        l_lngWidth = Rectangle.Width - m_sbrScrollbar.Rectangle.Width - m_sbrScrollbar.EdgeOffset
    Else
        l_lngWidth = Rectangle.Width
    End If
    l_lngItem = 1
    For Each l_itmItem In m_colItems
        If (Item = l_lngItem) Then Exit For
        l_lngItemHeight = 0
        Err.Clear
        l_lngItemHeight = l_itmItem.Height(l_lngWidth)
        If Err <> 0 Then
            l_lngItemHeight = l_itmItem.Height
            Err.Clear
        End If
        l_lngY = l_lngY + l_lngItemHeight
        l_lngItem = l_lngItem + 1
    Next l_itmItem
    GetItemY = l_lngY
End Function

Public Property Get iWidget_CanGetFocus() As Boolean
    iWidget_CanGetFocus = True
End Property

Public Sub Scrollbar_Scroll(Value)
On Error Resume Next
    ScrollOffset = CLng(Value)
End Sub

Public Function Class_ToString() As String
    Class_ToString = "ListBox " + Name
End Function

Public Property Get Items() As ItemCollection
    Set Items = m_colItems
End Property

Private Sub RefreshState()
On Error Resume Next
Dim l_strState As String
    If HaveFocus Then
        l_strState = "active"
    Else
        l_strState = "inactive"
    End If
    Set m_staState = Skin.States(l_strState)
End Sub

Private Sub DispatchEvent(EventName As String, ParamArray Parameters() As Variant)
On Error Resume Next
    mdlGlobal.Engine.ContextLevelAdd Name & "_" & EventName
    If EventHandler Is Nothing Then
    Else
        Select Case UBound(Parameters)
        Case 0
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0)
        Case 1
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0), Parameters(1)
        Case 2
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0), Parameters(1), Parameters(2)
        Case 3
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3)
        Case 4
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4)
        Case 5
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5)
        Case 6
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6)
        Case 7
            CallByName EventHandler, Name & "_" & EventName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6), Parameters(7)
        Case Else
            CallByName EventHandler, Name & "_" & EventName, VbMethod
        End Select
    End If
    mdlGlobal.Engine.ContextLevelRemove
End Sub

Public Property Get Widget() As iWidget
On Error Resume Next
    Set Widget = m_wdgMe
End Property

Private Sub Class_Initialize()
On Error Resume Next
    ListBoxCount = ListBoxCount + 1
    Name = "ListBox" & ListBoxCount
    Set Rectangle = New Fury2Rect
    Set m_colItems = New ItemCollection
    Set m_colItems.Parent = Me
    Visible = True
    Set m_sbrScrollbar = Globals.CreateWidget("Scrollbar")
    m_sbrScrollbar.Name = "Scrollbar"
    Set m_sbrScrollbar.EventHandler = Me
    Set m_wdgMe = Me
End Sub

Private Property Get iItemParent_Font() As Engine.Fury2Font
    Set iItemParent_Font = Skin.States("active").Font
End Property

Private Property Get iItemParent_TextColor() As Long
    iItemParent_TextColor = Skin.States("active").TextColor
End Property

Private Sub iWidget_LostCapture(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_GotFocus(ByVal Parent As iContainer, ByVal FromWidget As iWidget)
On Error Resume Next
    HaveFocus = True
    DispatchEvent "GotFocus", FromWidget
    RefreshState
    m_sbrScrollbar.Widget.GotFocus Nothing, Nothing
End Sub

Private Function iWidget_HitTest(ByVal X As Long, ByVal Y As Long) As Boolean
On Error Resume Next
    iWidget_HitTest = Me.Rectangle.PointInside(X, Y)
End Function

Private Sub iWidget_KeyDown(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    DispatchEvent "KeyDown", KeyCode, Shift
    Select Case KeyCode
    Case vbKeyUp
        SelectedItem = mdlGlobal.Engine.ClipInteger(SelectedItem - 1, 1, Items.Count)
        DispatchEvent "Select", SelectedItem
    Case vbKeyDown
        SelectedItem = mdlGlobal.Engine.ClipInteger(SelectedItem + 1, 1, Items.Count)
        DispatchEvent "Select", SelectedItem
    Case Else
    End Select
End Sub

Private Sub iWidget_KeyPress(ByVal Parent As iContainer, Character As Long)
On Error Resume Next
    DispatchEvent "KeyPress", Character
End Sub

Private Sub iWidget_KeyUp(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    DispatchEvent "KeyUp", KeyCode, Shift
End Sub

Private Sub iWidget_LostFocus(ByVal Parent As iContainer, ByVal ToWidget As iWidget)
On Error Resume Next
    HaveFocus = False
    DispatchEvent "LostFocus", ToWidget
    RefreshState
    m_sbrScrollbar.Widget.LostFocus Nothing, Nothing
End Sub

Private Sub iWidget_MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_rctNew As Fury2Rect, l_rctItem As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_staState As SkinState
Dim l_lngHeight As Long, l_itmItem As Object, l_lngItem As Long, l_lngY As Long, l_lngItemHeight As Long
Dim l_lngWidth As Long
Dim l_lngMouseY As Long
    If Visible = False Then Exit Sub
    Parent.SetFocus Me
    Parent.SetCapture Me
    If (m_sbrScrollbar.Visible) Then
        l_lngWidth = Rectangle.Width - m_sbrScrollbar.Rectangle.Width - m_sbrScrollbar.EdgeOffset
    Else
        l_lngWidth = Rectangle.Width
    End If
    If (X < (Me.Rectangle.Left + l_lngWidth)) Then
        If m_staState Is Nothing Then RefreshState
        Set l_rctNew = Rectangle.Copy
        l_lngMouseY = Y - Me.Rectangle.Top + ScrollOffset
        l_lngItem = 1
        For Each l_itmItem In m_colItems
            l_lngItemHeight = 0
            Err.Clear
            l_lngItemHeight = l_itmItem.Height(l_lngWidth)
            If Err <> 0 Then
                l_lngItemHeight = l_itmItem.Height
                Err.Clear
            End If
            If (l_lngMouseY >= l_lngY) And (l_lngMouseY < (l_lngY + l_lngItemHeight)) Then
                If Button = 1 Then
                    Me.SelectedItem = l_lngItem
                    DispatchEvent "Select", l_lngItem
                ElseIf Button = 2 Then
                    Me.SelectedItem = l_lngItem
                    DispatchEvent "Select", l_lngItem
                    DispatchEvent "ContextMenu", l_lngItem
                End If
                Exit For
            End If
            l_lngY = l_lngY + l_lngItemHeight
            l_rctItem.Top = l_lngY + l_rctNew.Top
            l_rctItem.Bottom = mdlGlobal.Engine.ClipInteger(l_rctNew.Bottom, l_rctItem.Top, l_rctNew.Bottom)
            l_lngHeight = l_lngHeight + l_lngItemHeight
            l_lngItem = l_lngItem + 1
        Next l_itmItem
    Else
        m_sbrScrollbar.Widget.MouseDown Nothing, Button, Shift, X - Me.Rectangle.Left, Y - Me.Rectangle.Top
    End If
    With Rectangle
        DispatchEvent "MouseDown", Button, Shift, X - .Left, Y - .Top
    End With
End Sub

Private Sub iWidget_MouseEnter(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_MouseLeave(ByVal Parent As iContainer)
On Error Resume Next
    m_sbrScrollbar.Widget.MouseLeave Nothing
End Sub

Private Sub iWidget_MouseMove(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_lngWidth As Long
    If Visible = False Then Exit Sub
    If (m_sbrScrollbar.Visible) Then
        l_lngWidth = Rectangle.Width - m_sbrScrollbar.Rectangle.Width - m_sbrScrollbar.EdgeOffset
    Else
        l_lngWidth = Rectangle.Width
    End If
    If (X < (Me.Rectangle.Left + l_lngWidth)) Then
    Else
        If m_sbrScrollbar.MouseHovering Then
        Else
            m_sbrScrollbar.Widget.MouseEnter Nothing
        End If
        m_sbrScrollbar.Widget.MouseMove Nothing, Button, Shift, X - Rectangle.Left, Y - Rectangle.Top
    End If
    With Rectangle
        DispatchEvent "MouseMove", Button, Shift, X - .Left, Y - .Top
    End With
End Sub

Private Sub iWidget_MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_lngWidth As Long
    If Visible = False Then Exit Sub
    If (m_sbrScrollbar.Visible) Then
        l_lngWidth = Rectangle.Width - m_sbrScrollbar.Rectangle.Width - m_sbrScrollbar.EdgeOffset
    Else
        l_lngWidth = Rectangle.Width
    End If
    If (X < (Me.Rectangle.Left + l_lngWidth)) Then
    Else
        m_sbrScrollbar.Widget.MouseUp Nothing, Button, Shift, X - Rectangle.Left, Y - Rectangle.Top
    End If
    Parent.ReleaseCapture Me
    With Rectangle
        DispatchEvent "MouseUp", Button, Shift, X - .Left, Y - .Top
    End With
End Sub

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
On Error Resume Next
Dim l_rctNew As Fury2Rect, l_rctItem As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_staState As SkinState
Dim l_lngHeight As Long, l_itmItem As Object, l_lngItem As Long, l_lngY As Long, l_lngItemHeight As Long
Dim l_lngWidth As Long
    If m_staState Is Nothing Then RefreshState
    If Visible = False Then Exit Sub
    If (m_lngTotalHeight <= Rectangle.Height) Then
        ScrollOffset = 0
        m_sbrScrollbar.Value = ScrollOffset
    Else
        ScrollOffset = ClipValue(ScrollOffset, 0, m_lngTotalHeight - Rectangle.Height)
        m_sbrScrollbar.Value = ScrollOffset
    End If
    l_lngY = -ScrollOffset
    With Surface
        Set l_rctOld = .ClipRectangle
        Set l_rctNew = Rectangle.Copy.Translate(XOffset, YOffset)
        Set l_rctItem = l_rctNew.Copy
        m_sbrScrollbar.Visible = (m_lngTotalHeight > Rectangle.Height)
        If (m_sbrScrollbar.Visible) Then
            Set m_sbrScrollbar.Rectangle = F2Rect(Rectangle.Width - m_sbrScrollbar.OptimalWidth, 0, m_sbrScrollbar.OptimalWidth, Rectangle.Height, False)
            m_sbrScrollbar.Refresh
            m_sbrScrollbar.Minimum = 0
            m_sbrScrollbar.Maximum = (m_lngTotalHeight - Rectangle.Height)
            m_sbrScrollbar.Increment = m_staState.Font.FullHeight
            m_sbrScrollbar.ThumbSize = (Rectangle.Height / m_lngTotalHeight)
            l_lngWidth = Rectangle.Width - m_sbrScrollbar.Rectangle.Width - m_sbrScrollbar.EdgeOffset
        Else
            l_lngWidth = Rectangle.Width
        End If
        l_rctItem.Width = l_lngWidth
        m_staState.WindowSkin.Draw Surface, l_rctNew, Opacity
        .ClippedSetClipRectangle l_rctNew
        l_lngItem = 1
        For Each l_itmItem In m_colItems
            l_rctItem.Top = l_lngY + l_rctNew.Top
            l_rctItem.Bottom = mdlGlobal.Engine.ClipInteger(l_rctNew.Bottom, l_rctItem.Top, l_rctNew.Bottom)
            l_lngItemHeight = 0
            Err.Clear
            l_lngItemHeight = l_itmItem.Height(l_rctItem.Width)
            If Err <> 0 Then
                l_lngItemHeight = l_itmItem.Height
            End If
            Err.Clear
            If ((l_rctItem.Top <= l_rctNew.Bottom) And ((l_rctItem.Top + l_lngItemHeight) > l_rctNew.Top)) Then
                If l_lngItem = SelectedItem Then
                    Surface.Fill F2Rect(l_rctItem.Left, l_rctItem.Top, l_rctItem.Width, l_lngItemHeight, False), SetAlpha(m_staState.HighlightColor, GetAlpha(m_staState.HighlightColor) * Opacity), RenderMode_SourceAlpha
                    l_itmItem.Draw Surface, l_rctItem, True, Opacity
                Else
                    l_itmItem.Draw Surface, l_rctItem, False, Opacity
                End If
            End If
            l_lngY = l_lngY + l_lngItemHeight
            l_lngHeight = l_lngHeight + l_lngItemHeight
            l_lngItem = l_lngItem + 1
        Next l_itmItem
        If AutoSizeHeight Then
            Rectangle.Height = l_lngHeight
        End If
        m_lngTotalHeight = l_lngHeight
        Set .ClipRectangle = l_rctOld
        If (m_sbrScrollbar.Visible) Then
            ' Dirty hack
            m_sbrScrollbar.Widget.Redraw Nothing, Surface, XOffset + Rectangle.Left, YOffset + Rectangle.Top, Opacity
        End If
    End With
End Sub
