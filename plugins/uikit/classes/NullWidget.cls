VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "NullWidget"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements iWidget
Private m_wdgMe As iWidget
Private m_engEngine As Fury2Engine

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Friend Sub SetEngine(NewEngine As Fury2Engine)
    Set m_engEngine = NewEngine
End Sub

Public Property Get Name() As String
    Name = "Null"
End Property

Public Property Get iWidget_CanGetFocus() As Boolean
    iWidget_CanGetFocus = False
End Property

Public Function Class_ToString() As String
    Class_ToString = "Null Widget"
End Function

Public Property Get Widget() As iWidget
On Error Resume Next
    Set Widget = m_wdgMe
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_wdgMe = Me
End Sub

Private Sub iWidget_LostCapture(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_GotFocus(ByVal Parent As iContainer, ByVal FromWidget As iWidget)
On Error Resume Next
End Sub

Private Function iWidget_HitTest(ByVal X As Long, ByVal Y As Long) As Boolean
On Error Resume Next
End Function

Private Sub iWidget_KeyDown(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
End Sub

Private Sub iWidget_KeyPress(ByVal Parent As iContainer, Character As Long)
On Error Resume Next
End Sub

Private Sub iWidget_KeyUp(ByVal Parent As iContainer, KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
End Sub

Private Sub iWidget_LostFocus(ByVal Parent As iContainer, ByVal ToWidget As iWidget)
On Error Resume Next
End Sub

Private Sub iWidget_MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
End Sub

Private Sub iWidget_MouseEnter(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_MouseLeave(ByVal Parent As iContainer)
On Error Resume Next
End Sub

Private Sub iWidget_MouseMove(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
End Sub

Private Sub iWidget_MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
End Sub

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
On Error Resume Next
End Sub

