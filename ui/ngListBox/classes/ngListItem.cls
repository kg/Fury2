VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngListItem"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Tag As Variant
Private m_lstParent As ngListBox
Private m_strText As String
Private m_strKey As String
Private m_lngIndex As Long
Private m_imgImage As Fury2Image
Private m_booSelected As Boolean
Private m_booVisible As Boolean
Private m_txaTextAlignment As ngListBoxTextAlignment
Private m_lngLeft As Long
Private m_lngTop As Long
Private m_fntFont As StdFont

Public Property Get Font() As StdFont
On Error Resume Next
    Set Font = m_fntFont
End Property

Public Property Set Font(ByVal NewFont As StdFont)
On Error Resume Next
    Set m_fntFont = NewFont
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
End Property

Public Property Get Index() As Long
    Index = m_lngIndex
End Property

Friend Property Let Index(ByVal NewValue As Long)
    m_lngIndex = NewValue
End Property

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
    Set m_lstParent = Nothing
    Set m_imgImage = Nothing
    Set m_fntFont = Nothing
End Sub

Public Property Get Parent() As ngListBox
    Set Parent = m_lstParent
End Property

Friend Sub SetParent(ByRef NewParent As ngListBox)
    Set m_lstParent = NewParent
End Sub

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
    Set Rectangle = F2Rect(m_lngLeft, m_lngTop, Width, Height, False)
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
    If m_lstParent Is Nothing Then Exit Property
    l_lngTextWidth = IIf(Len(m_strText) = 0, 0, m_lstParent.TextWidth(Font, m_strText) + (m_lstParent.Metrics(tbmTextMargin) * 2))
    l_lngImageWidth = IIf(m_imgImage Is Nothing, 0, m_imgImage.Width + (m_lstParent.Metrics(tbmImageMargin) * 2))
    Select Case m_txaTextAlignment
    Case ltaLeft, ltaRight
        Width = l_lngTextWidth + l_lngImageWidth + (m_lstParent.Metrics(tbmButtonMargin) * 2)
    Case ltaTop, ltaBottom
        Width = IIf(l_lngTextWidth > l_lngImageWidth, l_lngTextWidth, l_lngImageWidth) + (m_lstParent.Metrics(tbmButtonMargin) * 2)
    End Select
End Property

Public Property Get Height() As Long
On Error Resume Next
Dim l_lngImageHeight As Long
Dim l_lngTextHeight As Long
    If m_lstParent Is Nothing Then Exit Property
    l_lngTextHeight = IIf(Len(m_strText) = 0, 0, m_lstParent.TextHeight(Font, m_strText) + (m_lstParent.Metrics(tbmTextMargin) * 2))
    l_lngImageHeight = IIf(m_imgImage Is Nothing, 0, m_imgImage.Height + (m_lstParent.Metrics(tbmImageMargin) * 2))
    Select Case m_txaTextAlignment
    Case ltaLeft, ltaRight
        Height = IIf(l_lngTextHeight > l_lngImageHeight, l_lngTextHeight, l_lngImageHeight) + (m_lstParent.Metrics(tbmButtonMargin) * 2)
    Case ltaTop, ltaBottom
        Height = l_lngTextHeight + l_lngImageHeight + (m_lstParent.Metrics(tbmButtonMargin) * 2)
    End Select
End Property

Public Property Get Text() As String
    Text = m_strText
End Property

Public Property Let Text(ByRef NewValue As String)
On Error Resume Next
    m_strText = NewValue
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
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
    Set m_imgGlow = Nothing
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
End Property

Public Property Get Selected() As Boolean
    Selected = m_booSelected
End Property

Public Property Let Selected(ByVal NewValue As Boolean)
On Error Resume Next
    m_booSelected = NewValue
    If Not (m_lstParent Is Nothing) Then m_lstParent.Redraw
End Property

Public Property Get Visible() As Boolean
    Visible = m_booVisible
End Property

Public Property Let Visible(ByVal NewValue As Boolean)
On Error Resume Next
    m_booVisible = NewValue
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
End Property

Public Property Get TextAlignment() As ngListBoxTextAlignment
    TextAlignment = m_txaTextAlignment
End Property

Public Property Let TextAlignment(ByVal NewValue As ngListBoxTextAlignment)
On Error Resume Next
    m_txaTextAlignment = NewValue
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
End Property

Private Sub Class_Initialize()
On Error Resume Next
    m_txaTextAlignment = ltaRight
    m_booVisible = True
    m_booEnabled = True
End Sub

