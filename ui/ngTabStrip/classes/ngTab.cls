VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngTab"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Tag As Variant
Private m_lngRowHeight As Long
Private m_tbsParent As ngTabStrip
Private m_strTooltip As String
Private m_strText As String
Private m_strKey As String
Private m_lngIndex As Long
Private m_imgImage As Fury2Image
Private m_evtClick As ngEvent
Private m_booPressed As Boolean
Private m_booHovering As Boolean
Private m_booSelected As Boolean
Private m_booEnabled As Boolean
Private m_booVisible As Boolean
Private m_txaTextAlignment As ngTabTextAlignment
Private m_lngLeft As Long
Private m_lngTop As Long
Private m_fntFont As StdFont
Private m_lngWidth As Long, m_lngHeight As Long

Friend Sub Invalidate()
On Error Resume Next
    If Not (m_tbsParent Is Nothing) Then m_tbsParent.Reflow
    m_lngWidth = 0
    m_lngHeight = 0
End Sub

Friend Property Get RowHeight() As Long
    RowHeight = m_lngRowHeight
End Property

Friend Property Let RowHeight(ByVal NewValue As Long)
    m_lngRowHeight = NewValue
End Property

Public Property Get Font() As StdFont
On Error Resume Next
    Set Font = m_fntFont
End Property

Public Property Set Font(ByVal NewFont As StdFont)
On Error Resume Next
    Set m_fntFont = NewFont
    Invalidate
End Property

Public Property Get Index() As Long
    Index = m_lngIndex
End Property

Friend Property Let Index(ByVal NewValue As Long)
    m_lngIndex = NewValue
End Property

Friend Sub MouseDown()
On Error Resume Next
    m_booPressed = True
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
    Set m_tbsParent = Nothing
    Set m_imgImage = Nothing
    Set m_evtClick = Nothing
    Set m_fntFont = Nothing
End Sub

Public Property Get Parent() As ngTabStrip
    Set Parent = m_tbsParent
End Property

Friend Sub SetParent(ByRef NewParent As ngTabStrip)
    Set m_tbsParent = NewParent
End Sub

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
    Set Rectangle = F2Rect(m_lngLeft, m_lngTop, Width, Height, False)
End Property

Public Property Get State() As ngTabStates
On Error Resume Next
    If Enabled Then
        If Selected Then
            State = tstSelected
        ElseIf Hovering Then
            State = tstHighlighted
        Else
            State = tstNormal
        End If
    Else
'        State = bstDisabled
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
    If m_tbsParent Is Nothing Then Exit Property
    If m_lngWidth <> 0 Then
        Width = m_lngWidth
        Exit Function
    End If
    If Len(m_strText) > 0 Then
        l_lngTextWidth = m_tbsParent.TextWidth(Font, m_strText) + (m_tbsParent.Metrics(tsmTextMargin) * 2)
    End If
    If Not (m_imgImage Is Nothing) Then
        l_lngImageWidth = m_imgImage.Width + (m_tbsParent.Metrics(tsmImageMargin) * 2)
    End If
    Width = l_lngTextWidth + l_lngImageWidth + (m_tbsParent.Metrics(tsmTabHorizontalMargin) * 2)
    If m_tbsParent.ShowCloseButtons Then
        Width = Width + m_tbsParent.CloseButtonWidth
    End If
    m_lngWidth = Width
End Property

Public Property Get Height() As Long
On Error Resume Next
Dim l_lngImageHeight As Long
Dim l_lngTextHeight As Long
    If m_tbsParent Is Nothing Then Exit Property
    If m_lngHeight <> 0 Then
        Height = m_lngHeight
        Exit Function
    End If
    If Len(m_strText) > 0 Then
        l_lngTextHeight = m_tbsParent.TextHeight(Font, m_strText) + (m_tbsParent.Metrics(tsmTextMargin) * 2)
    End If
    If Not (m_imgImage Is Nothing) Then
        l_lngImageHeight = m_imgImage.Height + (m_tbsParent.Metrics(tsmImageMargin) * 2)
    End If
    Height = IIf(l_lngTextHeight > l_lngImageHeight, l_lngTextHeight, l_lngImageHeight) + (m_tbsParent.Metrics(tsmTabVerticalMargin))
    If m_tbsParent.IdealHeight > Height Then
        Height = m_tbsParent.IdealHeight
    End If
    m_lngHeight = Height
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
    Invalidate
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
    Invalidate
End Property

Public Property Get ClickEvent() As ngEvent
    Set ClickEvent = m_evtClick
End Property

Public Property Set ClickEvent(ByVal NewValue As ngEvent)
    Set m_evtClick = NewValue
End Property

Public Property Get Selected() As Boolean
    Selected = m_booSelected
End Property

Public Property Let Selected(ByVal NewValue As Boolean)
On Error Resume Next
    If NewValue = m_booSelected Then Exit Property
    m_booSelected = NewValue
    If Not (m_tbsParent Is Nothing) Then m_tbsParent.Redraw Me.Rectangle
End Property

Public Property Get Enabled() As Boolean
    Enabled = m_booEnabled
End Property

Public Property Let Enabled(ByVal NewValue As Boolean)
On Error Resume Next
    If NewValue = m_booEnabled Then Exit Property
    m_booEnabled = NewValue
    If Not (m_tbsParent Is Nothing) Then m_tbsParent.Redraw Me.Rectangle
End Property

Public Property Get Visible() As Boolean
    Visible = m_booVisible
End Property

Public Property Let Visible(ByVal NewValue As Boolean)
On Error Resume Next
    If NewValue = m_booVisible Then Exit Property
    m_booVisible = NewValue
    Invalidate
End Property

Public Property Get Hovering() As Boolean
    Hovering = m_booHovering
End Property

Public Property Get TextAlignment() As ngTabTextAlignment
    TextAlignment = m_txaTextAlignment
End Property

Public Property Let TextAlignment(ByVal NewValue As ngTabTextAlignment)
On Error Resume Next
    m_txaTextAlignment = NewValue
    Invalidate
End Property

Private Sub Class_Initialize()
On Error Resume Next
    m_txaTextAlignment = tsaRight
    m_booVisible = True
    m_booEnabled = True
End Sub
