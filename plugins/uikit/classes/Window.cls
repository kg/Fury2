VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Window"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements iContainer
Implements iWidget
Implements Fury2UserdataUI
Public Visible As Boolean
Public Parent As Object
Public EventHandler As Object
Public Name As String
Public Children As WidgetCollection
Private LastMouseWidget As iWidget
Public Text As String
Public Skin As Skin
Public CapturedWidget As iWidget
Public FocusedWidget As iWidget
Public HaveFocus As Boolean
Public Background As Fury2Image
Private m_booFocusing As Boolean
Private m_cntMe As iContainer
Private m_wdgMe As iWidget
Private m_engEngine As Fury2Engine
Private m_rctRectangle As Fury2Rect

Public Property Get Rectangle() As Fury2Rect
    Set Rectangle = m_rctRectangle
End Property

Public Property Set Rectangle(ByVal NewRect As Fury2Rect)
On Error Resume Next
    Set m_rctRectangle = NewRect
    DispatchEvent "Resize"
End Property

Private Sub Fury2UserdataUI_KeyDown(ByVal Key As Long, ByVal Shift As Long)
On Error Resume Next
    iWidget_KeyDown Nothing, Key, Shift
End Sub

Private Sub Fury2UserdataUI_KeyPress(ByVal Character As Long)
On Error Resume Next
    iWidget_KeyPress Nothing, Character
End Sub

Private Sub Fury2UserdataUI_KeyUp(ByVal Key As Long, ByVal Shift As Long)
On Error Resume Next
    iWidget_KeyUp Nothing, Key, Shift
End Sub

Private Sub Fury2UserdataUI_MouseDown(ByVal Button As Long, ByVal Shift As Long, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    iWidget_MouseDown Nothing, Button, Shift, X, Y
End Sub

Private Sub Fury2UserdataUI_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    iWidget_MouseMove Nothing, Button, Shift, X, Y
End Sub

Private Sub Fury2UserdataUI_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    iWidget_MouseUp Nothing, Button, Shift, X, Y
End Sub

Private Sub Fury2UserdataUI_Redraw(ByVal Surface As libGraphics.Fury2Image)
On Error Resume Next
    iWidget_Redraw Nothing, Surface, 0, 0, 1
End Sub

Private Property Get Fury2UserdataUI_This() As Object
On Error Resume Next
    Set Fury2UserdataUI_This = Me
End Property

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Friend Sub SetEngine(NewEngine As Fury2Engine)
    Set m_engEngine = NewEngine
End Sub

Public Sub ChildRemoved(Obj As Object)
Dim l_lngIndex As Long, l_lngCount As Long
    If FocusedWidget Is Obj Then
        l_lngIndex = GetFocusIndex(FocusedWidget)
        l_lngCount = GetFocusCount
        If (l_lngCount = 1) Then
            Container.SetFocus Nothing
        Else
            If (GetFocusCount > l_lngIndex) Then
                Container.SetFocus GetFocusableChild(l_lngIndex + 1)
            Else
                Container.SetFocus GetFocusableChild(l_lngIndex - 1)
            End If
        End If
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
                Exit Function
            End If
        Else
            If l_wdgWidget Is Widget Then
                GetFocusIndex = 0
                Exit Function
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
                Exit Function
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

Public Function Class_ToString() As String
    Class_ToString = "Window " + Name
End Function

Public Property Get Caption() As String
On Error Resume Next
    Caption = Text
End Property
Public Property Let Caption(ByRef NewCaption As String)
On Error Resume Next
    Text = NewCaption
End Property

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
    WindowCount = WindowCount + 1
    Name = "Window" & WindowCount
    Text = Name
    Set Children = New WidgetCollection
    Set Children.Parent = Me
    Set Rectangle = New Fury2Rect
    Visible = True
    Set m_cntMe = Me
    Set m_wdgMe = Me
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
    l_booChild = Children.Find(Widget) > 0
    If Not l_booChild Then
        l_booChild = iContainer_HasChild(Widget)
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
            If l_booChild Then
                For Each l_cntChild In Children
                    If l_cntChild.HasChild(Widget) Then
                        Set l_wdgOldFocus = FocusedWidget
                        Set FocusedWidget = l_cntChild
                        l_wdgOldFocus.LostFocus Me, FocusedWidget
                        FocusedWidget.GotFocus Me, l_wdgOldFocus
                        l_cntChild.SetFocus Widget
                        Exit For
                    End If
                Next l_cntChild
            End If
        End If
    End If
    m_booFocusing = False
    If l_booChild Then
        Set l_cntParent = Parent
        l_cntParent.SetFocus Me
        HaveFocus = True
    End If
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
    If Children.Count > 0 Then
        iContainer_SetFocus GetFocusableChild(GetFocusCount)
    Else
        iContainer_SetFocus Nothing
    End If
    Me.HaveFocus = True
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
        iContainer_SetFocus l_objChild
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
    Me.FocusedWidget.LostFocus Me, Nothing
    Set Me.FocusedWidget = Nothing
    Me.HaveFocus = False
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
                        Exit Sub
                    End If
                Next l_wdgWidget
            Else
                CapturedWidget.MouseDown Me, Button, Shift, X - .Left, Y - .Top
            End If
        End If
    End With
End Sub

Private Sub iWidget_MouseEnter(ByVal Parent As iContainer)
End Sub

Private Sub iWidget_MouseLeave(ByVal Parent As iContainer)
On Error Resume Next
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
        If (.PointInside(X, Y)) Or ((Button <> 0) And (Not (CapturedWidget Is Nothing))) Then
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
                        Exit For
                    End If
                Next l_wdgWidget
            Else
                CapturedWidget.MouseMove Me, Button, Shift, X - .Left, Y - .Top
                For Each l_cntChild In Children
                    Set l_wdgWidget = l_cntChild
                    If l_wdgWidget.HitTest(X - .Left, Y - .Top) Then
                        l_wdgWidget.MouseMove Me, Button, Shift, X - .Left, Y - .Top
                        Exit For
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
                        Exit Sub
                    End If
                Next l_wdgWidget
            End If
        Else
            CapturedWidget.MouseUp Me, Button, Shift, X - .Left, Y - .Top
        End If
    End With
End Sub

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
On Error Resume Next
Dim l_wdgWidget As iWidget, l_lngWidget As Long
Dim l_rctNew As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_staState As SkinState, l_strState As String
    If Visible = False Then Exit Sub
    With Surface
        Set l_rctOld = .ClipRectangle
        Set l_rctNew = Rectangle.Copy.Translate(XOffset, YOffset)
        If HaveFocus Then
            l_strState = "active"
        Else
            l_strState = "inactive"
        End If
        Set l_staState = Skin.States(l_strState)
        l_staState.WindowSkin.Draw Surface, l_rctNew, Opacity
        .ClippedSetClipRectangle l_rctNew
        Surface.Blit l_rctNew, , Background, , IIf(Background.AlphaChannel, BlitMode_SourceAlpha, BlitMode_Normal)
        l_staState.Font.DrawCentered Surface, Engine.ParseString(Text), Fury2GEGlobal.F2Rect(l_rctNew.Left, l_rctNew.Top, l_rctNew.Width, l_staState.Font.FullHeight, False), SetAlpha(l_staState.TextColor, GetAlpha(l_staState.TextColor) * Opacity)
        With Rectangle
            For l_lngWidget = Children.Count To 1 Step -1
                Set l_wdgWidget = Children(l_lngWidget)
                l_wdgWidget.Redraw Me, Surface, XOffset + .Left, YOffset + .Top, Opacity
            Next l_lngWidget
        End With
        Set .ClipRectangle = l_rctOld
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

