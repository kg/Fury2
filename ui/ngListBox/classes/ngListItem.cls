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
Private m_lngDragPosition As Long
Private m_lstParent As ngListBox
Private m_strText As String
Private m_strKey As String
Private m_lngIndex As Long
Private m_imgImage As Fury2Image
Private m_booSelected As Boolean
Private m_booVisible As Boolean
Private m_txaTextAlignment As ngListItemTextAlignment
Private m_lngTop As Long
Private m_fntFont As StdFont

Friend Property Let DragPosition(ByVal NewValue As Long)
On Error Resume Next
    m_lngDragPosition = NewValue
End Property

Friend Property Get DragPosition() As Long
    DragPosition = m_lngDragPosition
End Property

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
    Set Rectangle = F2Rect(0, m_lngTop - m_lstParent.ScrollY, m_lstParent.ScaleWidth, Height, False)
End Property

Public Property Get Top() As Long
    Top = m_lngTop
End Property

Friend Property Let Top(ByVal NewValue As Long)
    m_lngTop = NewValue
End Property

Public Property Get Height() As Long
On Error Resume Next
Dim l_lngImageHeight As Long
Dim l_lngTextHeight As Long
    If m_lstParent Is Nothing Then Exit Property
    If Len(m_strText) > 0 Then
        l_lngTextHeight = m_lstParent.TextHeight(Font, m_strText) + (m_lstParent.Metrics(tbmTextMargin) * 2)
    End If
    If Not (m_imgImage Is Nothing) Then
        l_lngImageHeight = m_imgImage.Height + (m_lstParent.Metrics(tbmImageMargin) * 2)
    End If
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
    If NewValue = m_strText Then Exit Property
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
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
End Property

Public Property Get Selected() As Boolean
    Selected = m_booSelected
End Property

Public Property Let Selected(ByVal NewValue As Boolean)
On Error Resume Next
    If NewValue = m_booSelected Then Exit Property
    m_booSelected = NewValue
    If Not (m_lstParent Is Nothing) Then m_lstParent.Redraw
End Property

Public Property Get Visible() As Boolean
    Visible = m_booVisible
End Property

Public Property Let Visible(ByVal NewValue As Boolean)
On Error Resume Next
    If NewValue = m_booVisible Then Exit Property
    m_booVisible = NewValue
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
End Property

Public Property Get TextAlignment() As ngListItemTextAlignment
    TextAlignment = m_txaTextAlignment
End Property

Public Property Let TextAlignment(ByVal NewValue As ngListItemTextAlignment)
On Error Resume Next
    m_txaTextAlignment = NewValue
    If Not (m_lstParent Is Nothing) Then m_lstParent.Reflow
End Property

Private Sub Class_Initialize()
On Error Resume Next
    m_txaTextAlignment = ltaRight
    m_booVisible = True
End Sub

