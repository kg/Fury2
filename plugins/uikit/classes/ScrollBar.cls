VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ScrollBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements iContainer
Implements iWidget
Public Visible As Boolean
Public Parent As Object
Public EventHandler As Object
Public Name As String
Private Children As WidgetCollection
Private LastMouseWidget As iWidget
Public Skin As Skin
Public Rectangle As Fury2Rect
Private CapturedWidget As iWidget
Private FocusedWidget As iWidget
Public HaveFocus As Boolean
Public MouseHovering As Boolean
Public ThumbSize As Single
Public Minimum As Single
Public Maximum As Single
Public Increment As Single
Public Value As Single
Public IntegerValue As Boolean
Private m_booHorizontal As Boolean
Private m_btnMinus As Button
Private m_btnPlus As Button
Private m_btnThumb As Button
Private m_booFocusing As Boolean
Private m_wdgMe As iWidget
Private m_cntMe As iContainer
Private m_engEngine As Fury2Engine

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Friend Sub SetEngine(NewEngine As Fury2Engine)
    Set m_engEngine = NewEngine
End Sub

Public Property Get Horizontal() As Boolean
    Horizontal = m_booHorizontal
End Property

Public Property Let Horizontal(ByVal NewValue As Boolean)
    m_booHorizontal = NewValue
    Refresh
End Property

Public Property Let BlitMode(ByVal NewMode As SFXBlitModes)
    m_btnMinus.BlitMode = NewMode
    m_btnPlus.BlitMode = NewMode
    m_btnThumb.BlitMode = NewMode
End Property

Public Sub ChildRemoved(Obj As Object)
    If FocusedWidget Is Obj Then
        Container.SetFocus Nothing
    End If
    If CapturedWidget Is Obj Then
        Container.ReleaseCapture Obj
    End If
End Sub

Public Function GetFocusCount() As Long
On Error Resume Next
Dim l_wdgWidget As iWidget
Dim l_lngIndex As Long
    For Each l_wdgWidget In Children
        If l_wdgWidget.CanGetFocus Then
            l_lngIndex = l_lngIndex + 1
        End If
    Next l_wdgWidget
    GetFocusCount = l_lngIndex
End Function

Public Function GetFocusIndex(ByRef Widget As iWidget) As Long
On Error Resume Next
Dim l_wdgWidget As iWidget
Dim l_lngIndex As Long
    For Each l_wdgWidget In Children
        If l_wdgWidget.CanGetFocus Then
            l_lngIndex = l_lngIndex + 1
            If l_wdgWidget Is Widget Then
                GetFocusIndex = l_lngIndex
            End If
        Else
            If l_wdgWidget Is Widget Then
                GetFocusIndex = 0
            End If
        End If
    Next l_wdgWidget
End Function

Public Function GetFocusableChild(ByVal Index As Long) As Object
On Error Resume Next
Dim l_objWidget As Object
Dim l_wdgWidget As iWidget
Dim l_lngIndex As Long
    If Index = 0 Then Exit Function
    For Each l_objWidget In Children
        Set l_wdgWidget = Nothing
        Set l_wdgWidget = l_objWidget
        If l_wdgWidget.CanGetFocus Then
            l_lngIndex = l_lngIndex + 1
            If l_lngIndex = Index Then
                Set GetFocusableChild = l_objWidget
            End If
        End If
    Next l_objWidget
End Function

Public Property Get iWidget_CanGetFocus() As Boolean
    iWidget_CanGetFocus = True
End Property

Public Property Get iContainer_HasChild(ByRef Widget As iWidget) As Boolean
On Error Resume Next
Dim l_cntWidget As iContainer
    If Children.Find(Widget) > 0 Then
        iContainer_HasChild = True
    Else
        For Each l_cntWidget In Children
            If l_cntWidget.HasChild(Widget) Then
                iContainer_HasChild = True
                Exit For
            End If
        Next l_cntWidget
    End If
End Property

Public Property Get iContainer_ActiveWidget() As iWidget
On Error Resume Next
Dim l_objWidget As Object
Dim l_cntWidget As iContainer
Dim l_booFocus As Boolean
    If HaveFocus Then
        For Each l_objWidget In Children
            l_booFocus = False
            l_booFocus = l_objWidget.HaveFocus
            If l_booFocus Then
                If (TypeOf l_objWidget Is iContainer) Then
                    Set l_cntWidget = l_objWidget
                    Set iContainer_ActiveWidget = l_cntWidget.ActiveWidget
                    Exit Property
                Else
                    Set iContainer_ActiveWidget = l_objWidget
                    Exit Property
                End If
            End If
        Next l_objWidget
        Set iContainer_ActiveWidget = Me
    End If
End Property

Public Sub Thumb_MouseMove(Button, Shift, X, Y)
On Error Resume Next
Dim l_staState As SkinState
Dim l_lngBarSize As Long
Dim l_lngThumbSize As Long
Dim l_lngBarPos As Long
Dim l_sngValue As Single
Dim l_lngPadding As Long
    If Button <> 1 Then Exit Sub
    Set l_staState = Skin.States("border")
    l_lngPadding = l_staState.EdgeOffset
    l_sngValue = (Value - Minimum) / (Maximum - Minimum)
    If Horizontal Then
        l_lngBarSize = Rectangle.Width - (m_btnMinus.Rectangle.Width + m_btnPlus.Rectangle.Width) - (l_lngPadding * 2)
        l_lngThumbSize = l_lngBarSize * ThumbSize
        l_lngBarSize = l_lngBarSize - (l_lngThumbSize)
        l_lngBarPos = l_sngValue * l_lngBarSize
        Value = Minimum + (((X - m_btnMinus.Rectangle.Right - l_lngPadding - (l_lngThumbSize / 2)) / l_lngBarSize) * (Maximum - Minimum))
    Else
        l_lngBarSize = Rectangle.Height - (m_btnMinus.Rectangle.Height + m_btnPlus.Rectangle.Height) - (l_lngPadding * 2)
        l_lngThumbSize = l_lngBarSize * ThumbSize
        l_lngBarSize = l_lngBarSize - (l_lngThumbSize)
        l_lngBarPos = l_sngValue * l_lngBarSize
        Value = Minimum + (((Y - m_btnMinus.Rectangle.Bottom - l_lngPadding - (l_lngThumbSize / 2)) / l_lngBarSize) * (Maximum - Minimum))
    End If
    If Value < Minimum Then Value = Minimum
    If Value > Maximum Then Value = Maximum
    If IntegerValue Then Value = Round(Value)
    DispatchEvent "Scroll", Value
End Sub

Public Sub Plus_Click()
On Error Resume Next
    Value = Value + Increment
    If Value < Minimum Then Value = Minimum
    If Value > Maximum Then Value = Maximum
    DispatchEvent "Scroll", Value
End Sub

Public Sub Minus_Click()
On Error Resume Next
    Value = Value - Increment
    If Value < Minimum Then Value = Minimum
    If Value > Maximum Then Value = Maximum
    DispatchEvent "Scroll", Value
End Sub

Public Function Class_ToString() As String
    Class_ToString = "ScrollBar " + Name
End Function

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

Public Property Get Container() As iContainer
On Error Resume Next
    Set Container = m_cntMe
End Property

Public Property Get Widget() As iWidget
On Error Resume Next
    Set Widget = m_wdgMe
End Property

Private Sub Class_Initialize()
On Error Resume Next
    ScrollBarCount = ScrollBarCount + 1
    Name = "ScrollBar" & ScrollBarCount
    Set Children = New WidgetCollection
    Set Children.Parent = Me
    Set m_btnMinus = Globals.CreateWidget("Button")
    Set m_btnPlus = Globals.CreateWidget("Button")
    Set m_btnThumb = Globals.CreateWidget("Button")
    Children.Add m_btnMinus
    Children.Add m_btnPlus
    Children.Add m_btnThumb
    Set Rectangle = New Fury2Rect
    Visible = True
    Minimum = 0
    Maximum = 9
    Increment = 1
    Value = 0
    ThumbSize = 0.2
    Set m_wdgMe = Me
    Set m_cntMe = Me
End Sub

Public Sub Refresh()
On Error Resume Next
    Set m_btnThumb.EventHandler = Me
    m_btnThumb.Name = "Thumb"
    m_btnThumb.Text = ""
    Set m_btnThumb.Image = Skin.Glyphs("thumb")
    If Horizontal Then
        Set m_btnThumb.Rectangle = F2Rect(0, 0, 0, Rectangle.Height, False)
    Else
        Set m_btnThumb.Rectangle = F2Rect(0, 0, Rectangle.Width, 0, False)
    End If
    Set m_btnMinus.EventHandler = Me
    m_btnMinus.Name = "Minus"
    m_btnMinus.Text = ""
    If Horizontal Then
        Set m_btnMinus.Image = Skin.Glyphs("left")
        Set m_btnMinus.Rectangle = F2Rect(0, 0, Rectangle.Height, Rectangle.Height, False)
    Else
        Set m_btnMinus.Image = Skin.Glyphs("up")
        Set m_btnMinus.Rectangle = F2Rect(0, 0, Rectangle.Width, Rectangle.Width, False)
    End If
    Set m_btnPlus.EventHandler = Me
    m_btnPlus.Name = "Plus"
    m_btnPlus.Text = ""
    If Horizontal Then
        Set m_btnPlus.Image = Skin.Glyphs("right")
        Set m_btnPlus.Rectangle = F2Rect(Rectangle.Width - Rectangle.Height, 0, Rectangle.Height, Rectangle.Height, False)
    Else
        Set m_btnPlus.Image = Skin.Glyphs("down")
        Set m_btnPlus.Rectangle = F2Rect(0, Rectangle.Height - Rectangle.Width, Rectangle.Width, Rectangle.Width, False)
    End If
End Sub

Private Property Get iContainer_Captured(ByRef Widget As iWidget) As Boolean
On Error Resume Next
    If CapturedWidget Is Widget Then
        iContainer_Captured = False
    ElseIf CapturedWidget Is Nothing Then
        iContainer_Captured = False
    Else
        iContainer_Captured = True
    End If
End Property

Private Property Get iContainer_Children() As WidgetCollection
On Error Resume Next
    Set iContainer_Children = Children
End Property

Private Sub iWidget_LostCapture(ByVal Parent As iContainer)
On Error Resume Next
Dim l_cntChild As iContainer
    For Each l_cntChild In Children
        l_cntChild.SetCapture Nothing
    Next l_cntChild
End Sub

Private Sub iContainer_ReleaseCapture(ByRef Widget As iWidget)
On Error Resume Next
Dim l_cntParent As iContainer
Dim l_cntChild As iContainer
    Set l_cntParent = Parent
    If (CapturedWidget Is Widget) Or (CapturedWidget Is NullCapture) Then
        Set CapturedWidget = Nothing
    End If
    If Widget Is NullCapture Then Exit Sub
    For Each l_cntChild In Children
        l_cntChild.ReleaseCapture NullCapture
    Next l_cntChild
    l_cntParent.ReleaseCapture Me
End Sub

Private Sub iContainer_SetCapture(ByRef Widget As iWidget)
On Error Resume Next
Dim l_cntParent As iContainer
Dim l_cntChild As iContainer
    If Not (CapturedWidget Is Nothing) Then
        CapturedWidget.LostCapture Me
    End If
    Set l_cntParent = Parent
    Set CapturedWidget = Widget
    If Widget Is NullCapture Then Exit Sub
    For Each l_cntChild In Children
        If l_cntChild Is Widget Then
        Else
            l_cntChild.SetCapture NullCapture
        End If
    Next l_cntChild
    l_cntParent.SetCapture Me
End Sub

Private Sub iContainer_SetFocus(ByRef Widget As iWidget)
On Error Resume Next
Dim l_cntParent As iContainer
Dim l_cntChild As iContainer
Dim l_wdgOldFocus As iWidget
Dim l_booChild As Boolean
    If m_booFocusing Then Exit Sub
    m_booFocusing = True
    HaveFocus = False
    l_booChild = Children.Find(Widget) > 0
    If Not l_booChild Then
        l_booChild = iContainer_HasChild(Widget)
    End If
    If l_booChild Then
        Set l_cntParent = Parent
        l_cntParent.SetFocus Me
        HaveFocus = True
    End If
    If (Widget Is Nothing) Or (Not l_booChild) Then
        Set l_wdgOldFocus = FocusedWidget
        Set FocusedWidget = Nothing
        If l_wdgOldFocus Is Nothing Then Else l_wdgOldFocus.LostFocus Me, Widget
    Else
        If Children.Find(Widget) > 0 Then
            If FocusedWidget Is Widget Then
            Else
                Set l_wdgOldFocus = FocusedWidget
                Set FocusedWidget = Widget
                l_wdgOldFocus.LostFocus Me, FocusedWidget
                FocusedWidget.GotFocus Me, l_wdgOldFocus
            End If
        Else
            For Each l_cntChild In Children
                If l_booChild = l_cntChild.HasChild(Widget) Then
                    Set l_wdgOldFocus = FocusedWidget
                    Set FocusedWidget = l_cntChild
                    l_wdgOldFocus.LostFocus Me, FocusedWidget
                    FocusedWidget.GotFocus Me, l_wdgOldFocus
                    l_cntChild.SetFocus Widget
                End If
            Next l_cntChild
        End If
    End If
    m_booFocusing = False
End Sub

Private Function iContainer_WidgetFromPoint(ByVal X As Long, ByVal Y As Long) As iWidget
On Error Resume Next
Dim l_wdgWidget As iWidget
    With Rectangle
        If .PointInside(X, Y) Then
            For Each l_wdgWidget In Children
                If l_wdgWidget.HitTest(X - .Left, Y - .Top) Then
                    Set iContainer_WidgetFromPoint = l_wdgWidget
                    Exit Function
                End If
            Next l_wdgWidget
        Else
        End If
    End With
End Function

Private Sub iWidget_GotFocus(ByVal Parent As iContainer, ByVal FromWidget As iWidget)
On Error Resume Next
    Me.HaveFocus = True
    If Children.Count > 0 Then
        iContainer_SetFocus GetFocusableChild(1)
    Else
        iContainer_SetFocus Nothing
    End If
End Sub

Private Function iWidget_HitTest(ByVal X As Long, ByVal Y As Long) As Boolean
On Error Resume Next
    iWidget_HitTest = Me.Rectangle.PointInside(X, Y)
End Function

Private Sub iWidget_KeyDown(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
Dim l_lngFocusIndex As Long
Dim l_objChild As Object
    FocusedWidget.KeyDown Me, KeyCode, Shift
    If KeyCode = vbKeyTab Then
        l_lngFocusIndex = GetFocusIndex(FocusedWidget)
        If (GetFocusCount = 1) Then
            l_lngFocusIndex = 1
        ElseIf (l_lngFocusIndex = 0) Then
            If Shift = vbShiftMask Then
                l_lngFocusIndex = 1
            Else
                l_lngFocusIndex = GetFocusCount
            End If
        Else
            If Shift = vbShiftMask Then
                l_lngFocusIndex = l_lngFocusIndex + 1
                If (l_lngFocusIndex > GetFocusCount) Then
                    If (Parent Is Nothing) Then
                        l_lngFocusIndex = 1
                    Else
                        Exit Sub
                    End If
                End If
            Else
                l_lngFocusIndex = l_lngFocusIndex - 1
                If (l_lngFocusIndex < 1) Then
                    If (Parent Is Nothing) Then
                        l_lngFocusIndex = GetFocusCount
                    Else
                        Exit Sub
                    End If
                End If
            End If
        End If
        Set l_objChild = GetFocusableChild(l_lngFocusIndex)
        If TypeOf l_objChild Is iContainer Then
            If Shift = vbShiftMask Then
                  iContainer_SetFocus l_objChild.GetFocusableChild(l_objChild.GetFocusCount)
            Else
                  iContainer_SetFocus l_objChild.GetFocusableChild(1)
            End If
        Else
            iContainer_SetFocus l_objChild
        End If
        KeyCode = 0
    End If
End Sub

Private Sub iWidget_KeyPress(ByVal Parent As iContainer, Character As Long)
On Error Resume Next
    FocusedWidget.KeyPress Me, Character
End Sub

Private Sub iWidget_KeyUp(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    FocusedWidget.KeyUp Me, KeyCode, Shift
End Sub

Private Sub iWidget_LostFocus(ByVal Parent As iContainer, ByVal ToWidget As iWidget)
On Error Resume Next
    Me.HaveFocus = False
    iContainer_SetFocus Nothing
End Sub

Private Sub iWidget_MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_wdgWidget As iWidget
    If Visible = False Then Exit Sub
    With Rectangle
        If .PointInside(X, Y) Then
            Parent.SetFocus Me
            If CapturedWidget Is Nothing Then
                For Each l_wdgWidget In Children
                    If l_wdgWidget.HitTest(X - .Left, Y - .Top) Then
                        l_wdgWidget.MouseDown Me, Button, Shift, X - .Left, Y - .Top
                    End If
                Next l_wdgWidget
            Else
                CapturedWidget.MouseDown Me, Button, Shift, X - .Left, Y - .Top
            End If
        End If
    End With
End Sub

Private Sub iWidget_MouseEnter(ByVal Parent As iContainer)
On Error Resume Next
    MouseHovering = True
End Sub

Private Sub iWidget_MouseLeave(ByVal Parent As iContainer)
On Error Resume Next
    MouseHovering = False
    If Not (LastMouseWidget Is Nothing) Then
        LastMouseWidget.MouseLeave Me
        Set LastMouseWidget = Nothing
    End If
End Sub

Private Sub iWidget_MouseMove(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_wdgMouseWidget As iWidget
Dim l_wdgWidget As iWidget
Dim l_cntChild As iContainer
    If Visible = False Then Exit Sub
    With Rectangle
        If (.PointInside(X, Y)) Or (Not (CapturedWidget Is Nothing)) Then
            Set l_wdgMouseWidget = iContainer_WidgetFromPoint(X, Y)
            If l_wdgMouseWidget Is LastMouseWidget Then
            Else
                LastMouseWidget.MouseLeave Me
                l_wdgMouseWidget.MouseEnter Me
            End If
            Set LastMouseWidget = l_wdgMouseWidget
            If CapturedWidget Is Nothing Then
                For Each l_wdgWidget In Children
                    If l_wdgWidget.HitTest(X - .Left, Y - .Top) Then
                        l_wdgWidget.MouseMove Me, Button, Shift, X - .Left, Y - .Top
                    End If
                Next l_wdgWidget
            Else
                CapturedWidget.MouseMove Me, Button, Shift, X - .Left, Y - .Top
                For Each l_cntChild In Children
                    Set l_wdgWidget = l_cntChild
                    If l_wdgWidget.HitTest(X - .Left, Y - .Top) Then
                        l_wdgWidget.MouseMove Me, Button, Shift, X - .Left, Y - .Top
                    End If
                Next l_cntChild
            End If
        End If
    End With
End Sub

Private Sub iWidget_MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_wdgMouseWidget As iWidget
Dim l_wdgWidget As iWidget
    If Visible = False Then Exit Sub
    With Rectangle
        If CapturedWidget Is Nothing Then
            If iWidget_HitTest(X, Y) Then
                For Each l_wdgWidget In Children
                    If l_wdgWidget.HitTest(X - .Left, Y - .Top) Then
                        l_wdgWidget.MouseUp Me, Button, Shift, X - .Left, Y - .Top
                    End If
                Next l_wdgWidget
            End If
        Else
            CapturedWidget.MouseUp Me, Button, Shift, X - .Left, Y - .Top
        End If
    End With
End Sub

Public Property Get OptimalWidth() As Long
On Error Resume Next
Dim l_staState As SkinState
    Set l_staState = Skin.States("border")
    OptimalWidth = l_staState.Width
End Property

Public Property Get EdgeOffset() As Long
On Error Resume Next
Dim l_staState As SkinState
    Set l_staState = Skin.States("border")
    EdgeOffset = l_staState.EdgeOffset
End Property

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
On Error Resume Next
Dim l_wdgWidget As iWidget
Dim l_rctNew As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_staState As SkinState
Dim l_lngBarSize As Long
Dim l_lngThumbSize As Long
Dim l_lngBarPos As Long
Dim l_sngValue As Single
Dim l_lngPadding As Long
    If Visible = False Then Exit Sub
    With Surface
'        Set l_rctOld = .ClipRectangle
        Set l_rctNew = Rectangle.Copy.Translate(XOffset, YOffset)
'        .ClippedSetClipRectangle l_rctNew
        Set l_staState = Skin.States("border")
        l_lngPadding = l_staState.EdgeOffset
        l_staState.WindowSkin.Draw Surface, l_rctNew, Opacity
        If IntegerValue Then Value = Round(Value)
        l_sngValue = (Value - Minimum) / (Maximum - Minimum)
        If Horizontal Then
            l_lngBarSize = Rectangle.Width - (m_btnMinus.Rectangle.Width + m_btnPlus.Rectangle.Width) - (l_lngPadding * 2)
            l_lngThumbSize = l_lngBarSize * ThumbSize
            l_lngBarSize = l_lngBarSize - (l_lngThumbSize)
            l_lngBarPos = l_sngValue * l_lngBarSize
            With m_btnThumb.Rectangle
                .Left = (l_lngBarPos) + (m_btnMinus.Rectangle.Right) + l_lngPadding
                .Width = l_lngThumbSize
            End With
        Else
            l_lngBarSize = Rectangle.Height - (m_btnMinus.Rectangle.Height + m_btnPlus.Rectangle.Height) - (l_lngPadding * 2)
            l_lngThumbSize = l_lngBarSize * ThumbSize
            l_lngBarSize = l_lngBarSize - (l_lngThumbSize)
            l_lngBarPos = l_sngValue * l_lngBarSize
            With m_btnThumb.Rectangle
                .Top = (l_lngBarPos) + (m_btnMinus.Rectangle.Bottom) + l_lngPadding
                .Height = l_lngThumbSize
            End With
        End If
        With Rectangle
            For Each l_wdgWidget In Children
                l_wdgWidget.Redraw Me, Surface, XOffset + .Left, YOffset + .Top, Opacity
            Next l_wdgWidget
        End With
'        Set .ClipRectangle = l_rctOld
    End With
End Sub

Private Sub iContainer_Sync()
On Error Resume Next
Dim l_objWidget As Object
Dim l_cntContainer As iContainer
    For Each l_objWidget In Children
        Set l_objWidget.Parent = Me
        CallByName l_objWidget.EventHandler, l_objWidget.Name, VbLet, l_objWidget
        CallByName l_objWidget.EventHandler, l_objWidget.Name, VbSet, l_objWidget
        Set l_cntContainer = Nothing
        Set l_cntContainer = l_objWidget
        If l_cntContainer Is Nothing Then Else l_cntContainer.Sync
    Next l_objWidget
    Err.Clear
End Sub


