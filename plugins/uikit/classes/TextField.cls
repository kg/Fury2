VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TextField"
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
Public AutoSizeHeight As Boolean
Public MultiLine As Boolean
Private m_lngSelectionDir As Long
Private m_lngSelectionStart As Long
Private m_lngSelectionLength As Long
Private m_lngDragStartPos As Long
Private m_booDragging As Boolean
Private m_strText As String
Private m_staState As SkinState
Public PasswordChar As String
Public Locked As Boolean
Public Tip As String
Public Skin As Skin
Public Rectangle As Fury2Rect
Public HaveFocus As Boolean

Public Property Get iWidget_CanGetFocus() As Boolean
    iWidget_CanGetFocus = True
End Property

Public Function Class_ToString() As String
    Class_ToString = "TextField " + Name
End Function

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
    
Public Sub MoveCaret(Position As Long)
On Error Resume Next
    m_lngSelectionStart = mdlGlobal.Engine.ClipInteger(Position, 1, Len(Me.Text) + 1)
    m_lngSelectionLength = 0
    m_lngSelectionDir = 1
End Sub

Public Sub ResizeSelection(Amount As Long)
On Error Resume Next
    Dim SelMax As Long
    If m_lngSelectionDir = -1 Then
        If ((m_lngSelectionStart > 1) Or (Amount > 0)) Then
            m_lngSelectionStart = mdlGlobal.Engine.ClipInteger(m_lngSelectionStart + Amount, 1, Len(Me.Text) + 1)
            m_lngSelectionLength = m_lngSelectionLength - Amount
            SelMax = Len(Me.Text) - m_lngSelectionStart + 1
            If m_lngSelectionLength <= 0 Then
                m_lngSelectionDir = 1
            End If
            If m_lngSelectionLength > SelMax Then
                m_lngSelectionLength = SelMax
            End If
        End If
    Else
        m_lngSelectionLength = m_lngSelectionLength + Amount
        If m_lngSelectionLength < 0 Then
            m_lngSelectionDir = -1
            m_lngSelectionStart = mdlGlobal.Engine.ClipInteger(m_lngSelectionStart + m_lngSelectionLength, 1, Len(Me.Text) + 1)
            m_lngSelectionLength = 1
        End If
        SelMax = Len(Me.Text) - m_lngSelectionStart + 1
        If m_lngSelectionLength > SelMax Then
            m_lngSelectionLength = SelMax
        End If
    End If
End Sub

Public Sub SetSelection(StartPos As Long, EndPos As Long)
On Error Resume Next
    m_lngSelectionStart = mdlGlobal.Engine.ClipInteger(StartPos, 1, Len(Me.Text) + 1)
    m_lngSelectionLength = mdlGlobal.Engine.ClipInteger(EndPos, 1, Len(Me.Text) + 1) - m_lngSelectionStart
    m_lngSelectionDir = 1
End Sub

Public Property Get SelectionStart() As Long
    SelectionStart = m_lngSelectionStart
End Property
Public Property Let SelectionStart(NewStart As Long)
    m_lngSelectionStart = NewStart
    m_lngSelectionDir = 1
End Property
Public Property Get SelectionLength() As Long
    SelectionLength = m_lngSelectionLength
End Property
Public Property Let SelectionLength(NewLength As Long)
    m_lngSelectionLength = NewLength
    m_lngSelectionDir = 1
End Property
Public Property Get SelectionEnd() As Long
    SelectionEnd = m_lngSelectionStart + m_lngSelectionLength
End Property
Public Property Let SelectionEnd(NewEnd As Long)
Dim l_lngTemp As Long
    If NewEnd < m_lngSelectionStart Then
        l_lngTemp = m_lngSelectionStart
        m_lngSelectionStart = mdlGlobal.Engine.ClipInteger(NewEnd, 1, Len(Me.Text) + 1)
        m_lngSelectionLength = l_lngTemp - m_lngSelectionStart
        m_lngSelectionDir = -1
    Else
        m_lngSelectionLength = mdlGlobal.Engine.ClipInteger(NewEnd, 1, Len(Me.Text) + 1) - m_lngSelectionStart
        m_lngSelectionDir = 1
    End If
End Property

Public Property Get SelectionText() As String
On Error Resume Next
    SelectionText = Mid(m_strText, m_lngSelectionStart, m_lngSelectionLength)
End Property
Public Property Let SelectionText(NewText As String)
On Error Resume Next
Dim sText As String, eText As String
    sText = ""
    sText = Left(m_strText, Me.SelectionStart - 1)
    eText = ""
    eText = Mid(m_strText, Me.SelectionEnd)
    m_strText = sText + NewText + eText
    MoveCaret Me.SelectionStart + Len(NewText)
    DispatchEvent "Change"
End Property

Public Property Get DisplayedText() As String
    If Len(Trim(PasswordChar)) = 0 Then
        DisplayedText = m_strText
    Else
        DisplayedText = String(Len(m_strText), PasswordChar)
    End If
End Property

Public Property Get Text() As String
On Error Resume Next
    Text = m_strText
End Property

Public Property Let Text(ByRef NewText As String)
On Error Resume Next
    If m_strText <> NewText Then
        m_strText = NewText
        If MultiLine Then
        Else
            m_strText = Replace(m_strText, vbCrLf, "")
        End If
        MoveCaret Len(m_strText) + 1
        DispatchEvent "Change"
    End If
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
    TextFieldCount = TextFieldCount + 1
    Name = "TextField" & TextFieldCount
    Tip = ""
    Text = ""
    PasswordChar = ""
    Set Rectangle = New Fury2Rect
    Visible = True
    m_lngSelectionStart = 1
    m_lngSelectionLength = 0
    m_lngSelectionDir = 1
    MultiLine = False
End Sub

Private Sub iWidget_LostCapture(ByVal Parent As iContainer)
On Error Resume Next
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
Dim sText As String, eText As String
    DispatchEvent "KeyDown", KeyCode, Shift
    If Locked Then Exit Sub
    Select Case KeyCode
    Case vbKeyHome
        If Shift = vbShiftMask Then
            If m_lngSelectionDir = -1 Then
                SetSelection 1, SelectionEnd
            Else
                SetSelection 1, SelectionStart
            End If
        Else
            SetSelection 1, 1
        End If
        KeyCode = 0
    Case vbKeyEnd
        If Shift = vbShiftMask Then
            If m_lngSelectionDir = -1 Then
                SetSelection SelectionEnd, Len(Text)
            Else
                SetSelection SelectionStart, Len(Text)
            End If
        Else
            SetSelection Len(Text), Len(Text)
        End If
        KeyCode = 0
    Case vbKeyLeft
        If Shift = vbShiftMask Then
            Me.ResizeSelection -1
        Else
            Me.MoveCaret Me.SelectionStart - 1
        End If
        KeyCode = 0
    Case vbKeyRight
        If Shift = vbShiftMask Then
            Me.ResizeSelection 1
        Else
            Me.MoveCaret Me.SelectionEnd + 1
        End If
        KeyCode = 0
    Case vbKeyBack
        If Me.SelectionLength = 0 Then
            sText = ""
            sText = Left(m_strText, Me.SelectionStart - 2)
            eText = ""
            eText = Mid(m_strText, Me.SelectionStart)
            m_strText = sText + eText
            Me.MoveCaret Me.SelectionStart - 1
        Else
            Me.SelectionText = ""
        End If
        KeyCode = 0
    Case vbKeyDelete
        If Me.SelectionLength = 0 Then
            sText = ""
            sText = Left(m_strText, Me.SelectionStart - 1)
            eText = ""
            eText = Mid(m_strText, Me.SelectionStart + 1)
            m_strText = sText + eText
        Else
            Me.SelectionText = ""
        End If
        KeyCode = 0
    Case vbKeyA
        If Shift = vbCtrlMask Then
            Me.SelectionStart = 1
            Me.SelectionLength = Len(m_strText)
        End If
        KeyCode = 0
    Case vbKeyD
        If Shift = vbCtrlMask Then
            MoveCaret Len(m_strText) + 1
        End If
        KeyCode = 0
    Case vbKeyX
        If Shift = vbCtrlMask Then
            mdlGlobal.Engine.ClipboardText = Me.SelectionText
            Me.SelectionText = ""
        End If
        KeyCode = 0
    Case vbKeyC
        If Shift = vbCtrlMask Then
            mdlGlobal.Engine.ClipboardText = Me.SelectionText
        End If
        KeyCode = 0
    Case vbKeyV
        If Shift = 2 Then
            Me.SelectionText = mdlGlobal.Engine.ClipboardText
        End If
        KeyCode = 0
    Case Else
    End Select
End Sub

Private Sub iWidget_KeyPress(ByVal Parent As iContainer, Character As Long)
On Error Resume Next
    DispatchEvent "KeyPress", Character
    If Locked Then Exit Sub
    If mdlGlobal.Engine.CharIsPrintable(Character) Then
        Me.SelectionText = Chr(Character)
    End If
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
End Sub

Private Sub iWidget_MouseDown(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
    If Visible = False Then Exit Sub
    Parent.SetFocus Me
    Parent.SetCapture Me
    If Button = 1 Then
        m_booDragging = True
        m_lngDragStartPos = m_staState.Font.IndexFromPoint(DisplayedText, Me.Rectangle, X, Y)
        If (m_lngDragStartPos < SelectionStart) Or (m_lngDragStartPos > SelectionEnd) Then
            MoveCaret m_lngDragStartPos
        End If
    End If
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
    If (m_booDragging) And (Button = 1) Then
        l_lngPos = m_staState.Font.IndexFromPoint(DisplayedText, Me.Rectangle, X, Y)
        Me.SelectionStart = m_lngDragStartPos
        Me.SelectionEnd = l_lngPos
    End If
End Sub

Private Sub iWidget_MouseUp(ByVal Parent As iContainer, ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
On Error Resume Next
Dim l_lngPos As Long
    If Visible = False Then Exit Sub
    If (m_booDragging) And (Button = 1) Then
        l_lngPos = m_staState.Font.IndexFromPoint(DisplayedText, Me.Rectangle, X, Y)
        If l_lngPos <> m_lngDragStartPos Then
            Me.SelectionStart = m_lngDragStartPos
            Me.SelectionEnd = l_lngPos
        End If
    End If
    Parent.ReleaseCapture Me
    m_booDragging = False
End Sub

Private Sub iWidget_Redraw(ByVal Parent As iContainer, ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long, ByVal Opacity As Single)
On Error Resume Next
Dim l_rctNew As Fury2Rect
Dim l_rctOld As Fury2Rect
Dim l_staState As SkinState, l_strState As String
Dim l_lngHeight As Long
    If m_staState Is Nothing Then RefreshState
    If Visible = False Then Exit Sub
    With Surface
        Set l_rctOld = .ClipRectangle
        Set l_rctNew = Rectangle.Copy.Translate(XOffset, YOffset)
        m_staState.WindowSkin.Draw Surface, l_rctNew, 1
        .ClippedSetClipRectangle l_rctNew
        Set l_staState = Skin.States("tip")
        l_staState.Font.Draw Surface, Tip, l_rctNew, l_staState.TextColor, Opacity
        If Locked Then
            m_staState.Font.Draw Surface, DisplayedText, l_rctNew, m_staState.TextColor, Opacity, , l_lngHeight
        Else
            m_staState.Font.Draw Surface, DisplayedText, l_rctNew, m_staState.TextColor, Opacity, m_staState.Font.Options(0, 0, SelectionStart, SelectionEnd, m_staState.HighlightColor, SelectionStart, IIf((SelectionLength > 0) Or (Not HaveFocus), 0, m_staState.HighlightColor)), l_lngHeight
        End If
        If AutoSizeHeight Then
            Rectangle.Height = l_lngHeight
        End If
        Set .ClipRectangle = l_rctOld
    End With
End Sub
