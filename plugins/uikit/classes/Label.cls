VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Label"
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
Public AutoSize As Boolean
Public Center As Boolean
Public Text As String
Public Skin As Skin
Public Rectangle As Fury2Rect
Private m_staState As SkinState

Public Property Get iWidget_CanGetFocus() As Boolean
    iWidget_CanGetFocus = False
End Property

Public Function Class_ToString() As String
    Class_ToString = "Label " + Name
End Function

Private Sub RefreshState()
On Error Resume Next
Dim l_strState As String
    l_strState = "normal"
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
    LabelCount = LabelCount + 1
    Name = "Label" & LabelCount
    Text = Name
    Set Rectangle = New Fury2Rect
    Visible = True
End Sub

Private Sub iWidget_LostCapture(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_GotFocus(ByVal Parent As iContainer, ByVal FromWidget As iWidget)
On Error Resume Next
End Sub

Private Function iWidget_HitTest(ByVal X As Long, ByVal Y As Long) As Boolean
On Error Resume Next
    iWidget_HitTest = Me.Rectangle.PointInside(X, Y)
End Function

Private Sub iWidget_KeyDown(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    DispatchEvent "KeyDown", KeyCode, Shift
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
End Sub

Private Sub iWidget_MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
    If Visible = False Then Exit Sub
    Parent.SetFocus Me
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
Dim l_lngPos As Long
    If Visible = False Then Exit Sub
End Sub

Private Sub iWidget_MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_lngPos As Long
    If Visible = False Then Exit Sub
    Parent.ReleaseCapture Me
End Sub

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
On Error Resume Next
Dim l_rctNew As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_strState As String
Dim l_rctText As Fury2Rect
    If m_staState Is Nothing Then RefreshState
    If Visible = False Then Exit Sub
    With Surface
        Set l_rctOld = .ClipRectangle
        Set l_rctNew = Rectangle.Copy.Translate(XOffset, YOffset)
        Set l_rctText = m_staState.Font.GetTextRectangle(Text, IIf(AutoSize, -1, Rectangle.Width))
        l_rctText.Translate l_rctNew.Left, l_rctNew.Top
        If Center Then
            l_rctText.Translate (l_rctNew.Width - l_rctText.Width) / 2, (l_rctNew.Height - l_rctText.Height) / 2
        End If
        m_staState.WindowSkin.Draw Surface, l_rctNew, 1
        .ClippedSetClipRectangle l_rctNew
        m_staState.Font.Draw Surface, Engine.Engine.ParseString(Text), l_rctText, SetAlpha(m_staState.TextColor, GetAlpha(m_staState.TextColor) * Opacity)
        If AutoSize Then
            Rectangle.Width = l_rctText.Width
            Rectangle.Height = l_rctText.Height
        End If
        Set .ClipRectangle = l_rctOld
    End With
End Sub

