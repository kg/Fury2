VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Button"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements iWidget
Public Visible As Boolean
Public Parent As Object
Public EventHandler As Object
Public Name As String
Public Text As String
Public Skin As Skin
Public Rectangle As Fury2Rect
Public HaveFocus As Boolean
Public MousePressed As Boolean, KeyboardPressed As Boolean, KeyboardDisabled As Boolean
Public MouseHovering As Boolean
Public Image As Object
Private m_staState As SkinState

Public Property Get iWidget_CanGetFocus() As Boolean
    iWidget_CanGetFocus = True
End Property

Public Function Class_ToString() As String
    Class_ToString = "Button " + Name
End Function

Private Sub RefreshState()
On Error Resume Next
Dim l_strState As String
Dim l_booCaptured As Boolean
    l_booCaptured = Parent.Captured(Me)
    If l_booCaptured Then
        l_strState = "inactive"
    ElseIf MousePressed Then
        If MouseHovering Then
            l_strState = "pressed"
        Else
            l_strState = "hot"
        End If
    ElseIf KeyboardPressed Then
        l_strState = "pressed"
    ElseIf MouseHovering Or HaveFocus Then
        l_strState = "hot"
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
    Set Widget = Me
End Property

Public Property Get Caption() As String
On Error Resume Next
    Caption = Text
End Property
Public Property Let Caption(ByRef NewCaption As String)
On Error Resume Next
    Text = NewCaption
End Property

Private Sub Class_Initialize()
On Error Resume Next
    ButtonCount = ButtonCount + 1
    Name = "Button" & ButtonCount
    Text = Name
    Set Rectangle = New Fury2Rect
    Visible = True
End Sub

Private Sub iWidget_GotFocus(ByVal Parent As iContainer, ByVal FromWidget As iWidget)
On Error Resume Next
    HaveFocus = True
    DispatchEvent "GotFocus", FromWidget
    RefreshState
End Sub

Private Function iWidget_HitTest(ByVal X As Long, ByVal Y As Long) As Boolean
On Error Resume Next
    iWidget_HitTest = Me.Rectangle.PointInside(X, Y)
End Function

Private Sub iWidget_KeyDown(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    If KeyCode = vbKeySpace Or KeyCode = vbKeyReturn Then
        If Not KeyboardDisabled Then
            KeyboardDisabled = True
            KeyboardPressed = True
            DispatchEvent "Press"
        End If
    End If
    DispatchEvent "KeyDown", KeyCode, Shift
    RefreshState
End Sub

Private Sub iWidget_KeyPress(ByVal Parent As iContainer, Character As Long)
On Error Resume Next
    DispatchEvent "KeyPress", Character
    RefreshState
End Sub

Private Sub iWidget_KeyUp(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    DispatchEvent "KeyUp", KeyCode, Shift
    If KeyboardPressed Then
        DispatchEvent "Release"
        DispatchEvent "Click"
    End If
    KeyboardPressed = False
    KeyboardDisabled = False
    RefreshState
End Sub

Private Sub iWidget_LostCapture(ByVal Parent As iContainer)
On Error Resume Next
    KeyboardDisabled = False
    MousePressed = False
    KeyboardPressed = False
    MouseHovering = False
    RefreshState
End Sub

Private Sub iWidget_LostFocus(ByVal Parent As iContainer, ByVal ToWidget As iWidget)
On Error Resume Next
    KeyboardDisabled = False
    MousePressed = False
    KeyboardPressed = False
    MouseHovering = False
    HaveFocus = False
    DispatchEvent "LostFocus", ToWidget
    RefreshState
End Sub

Private Sub iWidget_MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
    Parent.SetFocus Me
    Parent.SetCapture Me
    If Button = 1 Then
        MousePressed = True
        DispatchEvent "Press"
    End If
    DispatchEvent "MouseDown", Button, Shift, X, Y
    RefreshState
End Sub

Private Sub iWidget_MouseEnter(ByVal Parent As iContainer)
On Error Resume Next
    MouseHovering = True
    If Not Parent.Captured(Me) Then RefreshState
End Sub

Private Sub iWidget_MouseLeave(ByVal Parent As iContainer)
On Error Resume Next
    MouseHovering = False
    If Not Parent.Captured(Me) Then RefreshState
End Sub

Private Sub iWidget_MouseMove(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
    If Visible = False Then Exit Sub
    DispatchEvent "MouseMove", Button, Shift, X, Y
End Sub

Private Sub iWidget_MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
    If MousePressed Then
        DispatchEvent "Release"
        If MouseHovering Then
            DispatchEvent "Click"
        End If
    End If
    Parent.ReleaseCapture Me
    MousePressed = False
    RefreshState
    DispatchEvent "MouseUp", Button, Shift, X, Y
End Sub

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long)
On Error Resume Next
Dim l_rctNew As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_imgImage As Fury2Image
Dim l_gphImage As Fury2Graphic
    If m_staState Is Nothing Then RefreshState
    If Visible = False Then Exit Sub
    With Surface
        Set l_rctOld = .ClipRectangle
        Set l_rctNew = Rectangle.Copy.Translate(XOffset, YOffset)
        .ClippedSetClipRectangle l_rctNew
        m_staState.WindowSkin.Draw Surface, l_rctNew, 1
        m_staState.Font.DrawCentered Surface, Text, l_rctNew, m_staState.TextColor
        If TypeOf Image Is Fury2Image Then
            Set l_imgImage = Image
            l_rctNew.Translate (Rectangle.Width - l_imgImage.Width) / 2, (Rectangle.Height - l_imgImage.Height) / 2
            If Image.AlphaChannel Then
                .Blit l_rctNew, , l_imgImage, , BlitMode_SourceAlpha
            Else
                .Blit l_rctNew, , l_imgImage, , BlitMode_Matte
            End If
        ElseIf TypeOf Image Is Fury2Graphic Then
            Set l_gphImage = Image
            l_gphImage.Draw Surface, l_rctNew.Left + (l_rctNew.Width / 2), l_rctNew.Top + (l_rctNew.Height / 2)
        End If
        Set .ClipRectangle = l_rctOld
    End With
End Sub