VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngToolButton"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_tbrParent As ngToolbar
Private m_strTooltip As String
Private m_strText As String
Private m_strKey As String
Private m_imgImage As Fury2Image
Private m_evtClick As ngEvent
Private m_booPressed As Boolean
Private m_booHovering As Boolean
Private m_booChecked As Boolean
Private m_booEnabled As Boolean
Private m_booVisible As Boolean
Private m_oriOrientation As ngToolButtonOrientations
Private m_tbsStyle As ngToolButtonStyles
Private m_txaTextAlignment As ngToolButtonTextAlignment
Private m_lngLeft As Long
Private m_lngTop As Long

Public Property Get Orientation() As ngToolButtonOrientations
    Orientation = m_oriOrientation
End Property

Friend Property Let Orientation(ByVal NewValue As ngToolButtonOrientations)
On Error Resume Next
    m_oriOrientation = NewValue
End Property

Friend Sub MouseDown()
On Error Resume Next
    m_booPressed = True
    If m_tbsStyle = bsyCheck Then
        m_booChecked = Not m_booChecked
    End If
End Sub

Friend Sub MouseUp()
On Error Resume Next
    m_booPressed = False
    If Not (m_evtClick Is Nothing) Then m_evtClick.Invoke
End Sub

Friend Sub MouseEnter()
On Error Resume Next
    m_booHovering = True
End Sub

Friend Sub MouseLeave()
On Error Resume Next
    m_booHovering = False
End Sub

Friend Sub Free()
On Error Resume Next
    Set m_tbrParent = Nothing
    Set m_imgImage = Nothing
    Set m_evtClick = Nothing
End Sub

Public Property Get Parent() As ngToolbar
    Set Parent = m_tbrParent
End Property

Friend Sub SetParent(ByRef NewParent As ngToolbar)
    Set m_tbrParent = NewParent
End Sub

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
    Set Rectangle = F2Rect(m_lngLeft, m_lngTop, Width, Height, False)
End Property

Public Property Get State() As ngToolButtonStates
On Error Resume Next
    If Enabled Then
        If Checked Then
            If Pressed Then
                State = bstPressed
            Else
                State = bstChecked
            End If
        ElseIf Hovering Then
            If Pressed Then
                State = bstPressed
            Else
                State = bstHighlighted
            End If
        ElseIf Pressed Then
            State = bstHighlighted
        Else
            State = bstNormal
        End If
    Else
        State = bstDisabled
    End If
End Property

Public Property Get Left() As Long
    Left = m_lngLeft
End Property

Friend Property Let Left(ByVal NewValue As Long)
    m_lngLeft = NewValue
End Property

Public Property Get Top() As Long
    Top = m_lngTop
End Property

Friend Property Let Top(ByVal NewValue As Long)
    m_lngTop = NewValue
End Property

Public Property Get Width() As Long
On Error Resume Next
Dim l_lngImageWidth As Long
Dim l_lngTextWidth As Long
    If m_tbrParent Is Nothing Then Exit Property
    l_lngTextWidth = IIf(Len(m_strText) = 0, 0, m_tbrParent.TextWidth(m_strText) + (m_tbrParent.Metrics(tbmTextMargin) * 2))
    l_lngImageWidth = IIf(m_imgImage Is Nothing, 0, m_imgImage.Width + (m_tbrParent.Metrics(tbmImageMargin) * 2))
    Select Case m_tbsStyle
    Case bsyNormal, bsyCheck, bsyGroup
        Select Case m_txaTextAlignment
        Case btaLeft, btaRight
            Width = l_lngTextWidth + l_lngImageWidth + (m_tbrParent.Metrics(tbmButtonMargin) * 2)
        Case btaTop, btaBottom
            Width = IIf(l_lngTextWidth > l_lngImageWidth, l_lngTextWidth, l_lngImageWidth) + (m_tbrParent.Metrics(tbmButtonMargin) * 2)
        End Select
    Case bsyDropdown
        Select Case m_txaTextAlignment
        Case btaLeft, btaRight
            Width = l_lngTextWidth + l_lngImageWidth + (m_tbrParent.Metrics(tbmButtonMargin) * 2)
        Case btaTop, btaBottom
            Width = IIf(l_lngTextWidth > l_lngImageWidth, l_lngTextWidth, l_lngImageWidth) + (m_tbrParent.Metrics(tbmButtonMargin) * 2)
        End Select
    Case bsySeparator
        Width = m_tbrParent.Metrics(tbmSeparatorSize) + (m_tbrParent.Metrics(tbmSeparatorMargin) * 2)
    End Select
End Property

Public Property Get Height() As Long
On Error Resume Next
Dim l_lngImageHeight As Long
Dim l_lngTextHeight As Long
    If m_tbrParent Is Nothing Then Exit Property
    l_lngTextHeight = IIf(Len(m_strText) = 0, 0, m_tbrParent.TextHeight(m_strText) + (m_tbrParent.Metrics(tbmTextMargin) * 2))
    l_lngImageHeight = IIf(m_imgImage Is Nothing, 0, m_imgImage.Height + (m_tbrParent.Metrics(tbmImageMargin) * 2))
    Select Case m_tbsStyle
    Case bsyNormal, bsyCheck, bsyGroup, bsyDropdown
        Select Case m_txaTextAlignment
        Case btaLeft, btaRight
            Height = IIf(l_lngTextHeight > l_lngImageHeight, l_lngTextHeight, l_lngImageHeight) + (m_tbrParent.Metrics(tbmButtonMargin) * 2)
        Case btaTop, btaBottom
            Height = l_lngTextHeight + l_lngImageHeight + (m_tbrParent.Metrics(tbmButtonMargin) * 2)
        End Select
    Case bsySeparator
        Height = m_tbrParent.Metrics(tbmSeparatorSize) + (m_tbrParent.Metrics(tbmSeparatorMargin) * 2)
    End Select
End Property

Public Property Get Tooltip() As String
    Tooltip = m_strTooltip
End Property

Public Property Let Tooltip(ByRef NewValue As String)
On Error Resume Next
    m_strTooltip = NewValue
End Property

Public Property Get Text() As String
    Text = m_strText
End Property

Public Property Let Text(ByRef NewValue As String)
On Error Resume Next
    m_strText = NewValue
    If Not (m_tbrParent Is Nothing) Then m_tbrParent.Reflow
End Property

Public Property Get Key() As String
    Key = m_strKey
End Property

Friend Property Let Key(ByRef NewValue As String)
    m_strKey = NewValue
End Property

Public Property Get Image() As Fury2Image
    Set Image = m_imgImage
End Property

Public Property Set Image(ByVal NewValue As Fury2Image)
On Error Resume Next
    Set m_imgImage = NewValue
    If Not (m_tbrParent Is Nothing) Then m_tbrParent.Reflow
End Property

Public Property Get ClickEvent() As ngEvent
    Set ClickEvent = m_evtClick
End Property

Public Property Set ClickEvent(ByVal NewValue As ngEvent)
    Set m_evtClick = NewValue
End Property

Public Property Get Checked() As Boolean
    Checked = m_booChecked
End Property

Public Property Let Checked(ByVal NewValue As Boolean)
On Error Resume Next
    m_booChecked = NewValue
    If Not (m_tbrParent Is Nothing) Then m_tbrParent.Redraw
End Property

Public Property Get Enabled() As Boolean
    If m_tbsStyle = bsySeparator Then
        Enabled = False
    Else
        Enabled = m_booEnabled
    End If
End Property

Public Property Let Enabled(ByVal NewValue As Boolean)
On Error Resume Next
    m_booEnabled = NewValue
    If Not (m_tbrParent Is Nothing) Then m_tbrParent.Redraw
End Property

Public Property Get Visible() As Boolean
    Visible = m_booVisible
End Property

Public Property Let Visible(ByVal NewValue As Boolean)
On Error Resume Next
    m_booVisible = NewValue
    If Not (m_tbrParent Is Nothing) Then m_tbrParent.Reflow
End Property

Public Property Get Pressed() As Boolean
    Pressed = m_booPressed
End Property

Public Property Get Hovering() As Boolean
    Hovering = m_booHovering
End Property

Public Property Get Style() As ngToolButtonStyles
    Style = m_tbsStyle
End Property

Public Property Let Style(ByVal NewValue As ngToolButtonStyles)
On Error Resume Next
    m_tbsStyle = NewValue
    If Not (m_tbrParent Is Nothing) Then m_tbrParent.Reflow
End Property

Public Property Get TextAlignment() As ngToolButtonTextAlignment
    TextAlignment = m_txaTextAlignment
End Property

Public Property Let TextAlignment(ByVal NewValue As ngToolButtonTextAlignment)
On Error Resume Next
    m_txaTextAlignment = NewValue
    If Not (m_tbrParent Is Nothing) Then m_tbrParent.Reflow
End Property

Private Sub Class_Initialize()
On Error Resume Next
    m_txaTextAlignment = btaRight
    m_booVisible = True
    m_booEnabled = True
End Sub
