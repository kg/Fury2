VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Menu"
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
Public AutoSizeWidth As Boolean
Public AutoSizeHeight As Boolean
Public SelectedItem As Long
Private m_colItems As ItemCollection
Private m_staState As SkinState
Private m_wdgOldFocus As iWidget
Public Skin As Skin
Public Rectangle As Fury2Rect
Public HaveFocus As Boolean
Public Popup As Boolean
Private m_wdgMe As iWidget
Private m_engEngine As Fury2Engine

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Friend Sub SetEngine(NewEngine As Fury2Engine)
    Set m_engEngine = NewEngine
End Sub

Public Function GetItemY(ByVal Item As Long) As Long
Dim l_itmItem As Item
Dim l_lngItem As Long
Dim l_lngItemHeight As Long
Dim l_lngY As Long
    l_lngItem = 1
    For Each l_itmItem In m_colItems
        If (Item = l_lngItem) Then Exit For
        l_lngItemHeight = 0
        Err.Clear
        l_lngItemHeight = l_itmItem.Height
        l_lngY = l_lngY + l_lngItemHeight
        l_lngItem = l_lngItem + 1
    Next l_itmItem
    GetItemY = l_lngY
End Function

Public Property Get iWidget_CanGetFocus() As Boolean
    iWidget_CanGetFocus = True
End Property

Public Function Class_ToString() As String
    Class_ToString = "Menu " + Name
End Function

Public Sub Show(ByVal X As Long, ByVal Y As Long)
On Error Resume Next
Dim l_cntParent As iContainer
    Set Rectangle = F2Rect(X, Y, Rectangle.Width, Rectangle.Height, False)
    Visible = True
    SelectedItem = 0
    ' Trigger the autosize mechanism
    Call Widget.Redraw(Nothing, Nothing, 0, 0, 0)
    Set l_cntParent = Parent
    Set m_wdgOldFocus = l_cntParent.ActiveWidget
    l_cntParent.SetCapture Me
    l_cntParent.SetFocus Me
End Sub

Public Sub Hide()
On Error Resume Next
Dim l_cntParent As iContainer
    HaveFocus = False
    Visible = False
    Set l_cntParent = Parent
    l_cntParent.ReleaseCapture Me
    mdlGlobal.Globals.Desktop.Container.SetFocus m_wdgOldFocus
    Set m_wdgOldFocus = Nothing
    DispatchEvent "Close"
    If Popup Then mdlGlobal.Globals.Desktop.Children.Remove Me
    Popup = False
End Sub

Private Property Get iItemParent_Font() As Engine.Fury2Font
    Set iItemParent_Font = Skin.States("active").Font
End Property

Private Property Get iItemParent_TextColor() As Long
    iItemParent_TextColor = Skin.States("active").TextColor
End Property

Public Property Get Items() As ItemCollection
    Set Items = m_colItems
End Property

Private Sub RefreshState()
    Set m_staState = Skin.States("active")
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
    MenuCount = MenuCount + 1
    Name = "Menu" & MenuCount
    Set Rectangle = New Fury2Rect
    Set m_colItems = New ItemCollection
    Set m_colItems.Parent = Me
    Visible = False
    Set m_wdgMe = Me
End Sub

Private Sub iWidget_LostCapture(ByVal Parent As iContainer)
On Error Resume Next
    Visible = False
    DispatchEvent "Close"
End Sub

Private Sub iWidget_GotFocus(ByVal Parent As iContainer, ByVal FromWidget As iWidget)
On Error Resume Next
    HaveFocus = True
    DispatchEvent "GotFocus"
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
    Case vbKeyDown
        SelectedItem = mdlGlobal.Engine.ClipInteger(SelectedItem + 1, 1, Items.Count)
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
    Select Case KeyCode
    Case vbKeySpace
        If (Me.SelectedItem > 0) Then
            DispatchEvent "Select", Me.SelectedItem
        End If
        Hide
    Case Else
    End Select
End Sub

Private Sub iWidget_LostFocus(ByVal Parent As iContainer, ByVal ToWidget As iWidget)
On Error Resume Next
    HaveFocus = False
    DispatchEvent "LostFocus"
End Sub

Private Sub iWidget_MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
    If Visible = False Then Exit Sub
    Parent.SetCapture Me
    DispatchEvent "MouseDown", Button, Shift, X, Y
End Sub

Private Sub iWidget_MouseEnter(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_MouseLeave(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_MouseMove(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_rctNew As Fury2Rect, l_rctItem As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_staState As SkinState
Dim l_lngHeight As Long, l_itmItem As Item, l_lngItem As Long, l_lngY As Long, l_lngItemHeight As Long
    If m_staState Is Nothing Then RefreshState
    If Visible = False Then Exit Sub
    Set l_rctNew = Rectangle.Copy
    Me.SelectedItem = 0
    If ((X < Me.Rectangle.Left) Or (X > Me.Rectangle.Right)) Or ((Y < Me.Rectangle.Top) Or (Y > Me.Rectangle.Bottom)) Then
        DispatchEvent "MouseMove", Button, Shift, X, Y
        Exit Sub
    End If
    Y = Y - Me.Rectangle.Top
    l_lngItem = 1
    For Each l_itmItem In m_colItems
        l_lngItemHeight = 0
        Err.Clear
        l_lngItemHeight = l_itmItem.Height
        If (Y >= l_lngY) And (Y < (l_lngY + l_lngItemHeight)) Then
            Me.SelectedItem = l_lngItem
            Exit For
        End If
        l_lngY = l_lngY + l_lngItemHeight
        l_rctItem.Top = l_lngY + l_rctNew.Top
        l_rctItem.Bottom = mdlGlobal.Engine.ClipInteger(l_rctNew.Bottom, l_rctItem.Top, l_rctNew.Bottom)
        l_lngHeight = l_lngHeight + l_lngItemHeight
        l_lngItem = l_lngItem + 1
    Next l_itmItem
    DispatchEvent "MouseMove", Button, Shift, X, Y + Me.Rectangle.Top
End Sub

Private Sub iWidget_MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
    If Visible = False Then Exit Sub
    DispatchEvent "MouseUp", Button, Shift, X, Y
    If (Me.SelectedItem > 0) Then
        DispatchEvent "Select", Me.SelectedItem
    End If
    Hide
End Sub

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
On Error Resume Next
Dim l_rctNew As Fury2Rect, l_rctItem As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_staState As SkinState
Dim l_lngWidth As Long, l_lngItemWidth As Long
Dim l_lngHeight As Long, l_itmItem As Item, l_lngItem As Long, l_lngY As Long, l_lngItemHeight As Long
Dim l_rctSize As Fury2Rect
    If m_staState Is Nothing Then RefreshState
    If Visible = False Then Exit Sub
    With Surface
        Set l_rctOld = .ClipRectangle
        Set l_rctNew = Rectangle.Copy.Translate(XOffset, YOffset)
        Set l_rctItem = l_rctNew.Copy
        m_staState.WindowSkin.Draw Surface, l_rctNew, Opacity
        .ClippedSetClipRectangle l_rctNew
        l_lngItem = 1
        For Each l_itmItem In m_colItems
            l_rctItem.Top = l_lngY + l_rctNew.Top
            l_rctItem.Bottom = mdlGlobal.Engine.ClipInteger(l_rctNew.Bottom, l_rctItem.Top, l_rctNew.Bottom)
            l_lngItemHeight = 0
            Err.Clear
            l_lngItemWidth = l_itmItem.Width
            l_lngItemHeight = l_itmItem.Height
            If ((l_rctItem.Top <= l_rctNew.Bottom) And ((l_rctItem.Top + l_lngItemHeight) > l_rctNew.Top)) Then
                If l_lngItem = SelectedItem Then
                    Surface.Fill F2Rect(l_rctItem.Left, l_rctItem.Top, l_rctItem.Width, l_lngItemHeight, False), SetAlpha(m_staState.HighlightColor, GetAlpha(m_staState.HighlightColor) * Opacity), RenderMode_SourceAlpha
                    l_itmItem.Draw Surface, l_rctItem, True, Opacity
                Else
                    l_itmItem.Draw Surface, l_rctItem, False, Opacity
                End If
            End If
            If l_lngItemWidth > l_lngWidth Then l_lngWidth = l_lngItemWidth
            l_lngY = l_lngY + l_lngItemHeight
            l_rctItem.Top = l_lngY + l_rctNew.Top
            l_rctItem.Bottom = mdlGlobal.Engine.ClipInteger(l_rctNew.Bottom, l_rctItem.Top, l_rctNew.Bottom)
            l_lngHeight = l_lngHeight + l_lngItemHeight
            l_lngItem = l_lngItem + 1
        Next l_itmItem
        If AutoSizeWidth Then
            Rectangle.Width = l_lngWidth
        End If
        If AutoSizeHeight Then
            Rectangle.Height = l_lngHeight
        End If
        Set .ClipRectangle = l_rctOld
    End With
End Sub
