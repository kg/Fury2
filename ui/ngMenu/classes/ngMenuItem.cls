VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngMenuItem"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Tag As Variant
Private m_mnuParent As ngMenu
Private m_mnuChild As ngMenu
Private m_strText As String
Private m_strDisplayText As String
Private m_strAccelerator As String
Private m_lngHotkey As Long
Private m_strKey As String
Private m_lngIndex As Long
Private m_lngImageColumnWidth As Long
Private m_imgImage As Fury2Image
Private m_imgGlow As Fury2Image
Private m_evtHover As ngEvent
Private m_evtClick As ngEvent
Private m_booPressed As Boolean
Private m_booHovering As Boolean
Private m_booChecked As Boolean
Private m_booEnabled As Boolean
Private m_booVisible As Boolean
Private m_tbsStyle As ngMenuItemStyles
Private m_txaTextAlignment As ngMenuItemTextAlignment
Private m_lngLeft As Long
Private m_lngTop As Long
Private m_fntFont As StdFont

Public Property Get ImageColumnWidth() As Long
On Error Resume Next
    ImageColumnWidth = m_lngImageColumnWidth
End Property

Public Property Let ImageColumnWidth(ByVal Value As Long)
On Error Resume Next
    m_lngImageColumnWidth = Value
End Property

Public Property Get Font() As StdFont
On Error Resume Next
    Set Font = m_fntFont
End Property

Public Property Set Font(ByVal NewFont As StdFont)
On Error Resume Next
    Set m_fntFont = NewFont
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Reflow
End Property

Public Property Get Index() As Long
    Index = m_lngIndex
End Property

Friend Property Let Index(ByVal NewValue As Long)
    m_lngIndex = NewValue
End Property

Private Sub GenerateGlow()
On Error Resume Next
Dim l_filFilter As Fury2ConvolutionFilter
Dim l_imgTemp As Fury2Image
    If m_imgImage Is Nothing Then Exit Sub
    Set m_imgGlow = F2Image(m_imgImage.Width + 4, m_imgImage.Height + 4)
    Set l_imgTemp = m_imgGlow.Duplicate
    l_imgTemp.Blit F2Rect(2, 2, m_imgImage.Width, m_imgImage.Height, False), , m_imgImage
    l_imgTemp.AdjustRGB 255, 255, 255
    Set l_filFilter = F2GaussianBlurFilter(3)
    l_filFilter.Filter m_imgGlow, l_imgTemp, , , RenderMode_Normal
    Set l_filFilter = F2GaussianBlurFilter(2)
    l_filFilter.Filter m_imgGlow, l_imgTemp, , , RenderMode_Additive
    m_imgGlow.AdjustChannelGamma Alpha, 400
End Sub

Friend Property Get GlowImage() As Fury2Image
On Error Resume Next
    If m_imgGlow Is Nothing Then
        GenerateGlow
    End If
    Set GlowImage = m_imgGlow
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
    If m_booHovering Then Exit Sub
    m_booHovering = True
    If Not (m_evtHover Is Nothing) Then m_evtHover.Invoke
End Sub

Friend Sub MouseLeave()
On Error Resume Next
    If (m_booHovering = False) And (m_booPressed = False) Then Exit Sub
    m_booHovering = False
    m_booPressed = False
End Sub

Friend Sub Free()
On Error Resume Next
    Set m_mnuParent = Nothing
    Set m_imgImage = Nothing
    Set m_imgGlow = Nothing
    Set m_evtClick = Nothing
    Set m_fntFont = Nothing
End Sub

Public Property Get ChildMenu() As ngMenu
    Set ChildMenu = m_mnuChild
End Property

Public Property Set ChildMenu(ByRef NewMenu As ngMenu)
On Error Resume Next
    Set m_mnuChild = NewMenu
    Set NewMenu.Parent = Me.Parent
    Set NewMenu.ParentItem = Me
End Property

Public Property Get Parent() As ngMenu
    Set Parent = m_mnuParent
End Property

Friend Sub SetParent(ByRef NewParent As ngMenu)
    Set m_mnuParent = NewParent
End Sub

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
    If Parent.Host Is Nothing Then
        Set Rectangle = F2Rect(m_lngLeft, m_lngTop, Width, Height, False)
    Else
        Set Rectangle = F2Rect(m_lngLeft, m_lngTop, Parent.Host.Width, Height, False)
    End If
End Property

Public Property Get State() As ngMenuItemStates
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
    If m_mnuParent Is Nothing Then Exit Property
    If Len(m_strText) > 0 Then
        l_lngTextWidth = GetTextWidth(m_strDisplayText, IIf(Font Is Nothing, m_mnuParent.Font, Font)) + IIf(Len(m_strAccelerator) = 0, 0, GetTextWidth(m_strAccelerator, IIf(Font Is Nothing, m_mnuParent.Font, Font)) + 8) + (GetTextWidth("4", g_fntMarlett) + 2) + (m_mnuParent.Metrics(mnmTextMargin) * 2)
    End If
    If Not (m_imgImage Is Nothing) Then
        l_lngImageWidth = ClipValue(m_imgImage.Width + (m_mnuParent.Metrics(mnmImageMargin) * 2), m_lngImageColumnWidth, 999999)
    Else
        l_lngImageWidth = m_lngImageColumnWidth
    End If
    Select Case m_tbsStyle
    Case bsyNormal, bsyCheck, bsyGroup
        Width = l_lngTextWidth + l_lngImageWidth + (m_mnuParent.Metrics(mnmItemMargin) * 2)
    Case bsySeparator
        Width = m_mnuParent.Metrics(mnmSeparatorSize) + (m_mnuParent.Metrics(mnmSeparatorMargin) * 2)
    End Select
End Property

Public Property Get Height() As Long
On Error Resume Next
Dim l_lngImageHeight As Long
Dim l_lngTextHeight As Long
    If m_mnuParent Is Nothing Then Exit Property
    If Len(m_strText) > 0 Then
        l_lngTextHeight = GetTextHeight(m_strDisplayText, IIf(Font Is Nothing, m_mnuParent.Font, Font)) + (m_mnuParent.Metrics(mnmTextMargin) * 2)
    End If
    If Not (m_imgImage Is Nothing) Then
        l_lngImageHeight = m_imgImage.Height + (m_mnuParent.Metrics(mnmImageMargin) * 2)
    End If
    Select Case m_tbsStyle
    Case bsyNormal, bsyCheck, bsyGroup
        Height = IIf(l_lngTextHeight > l_lngImageHeight, l_lngTextHeight, l_lngImageHeight) + (m_mnuParent.Metrics(mnmItemMargin) * 2)
    Case bsySeparator
        Height = m_mnuParent.Metrics(mnmSeparatorSize) + (m_mnuParent.Metrics(mnmSeparatorMargin) * 2)
    End Select
End Property

Public Property Get Text() As String
    Text = m_strText
End Property

Public Property Let Text(ByRef NewValue As String)
On Error Resume Next
Dim l_strTemp As String
    m_strText = NewValue
    m_strDisplayText = Replace(Replace(Replace(m_strText, "&&", "*^^*"), "&", ""), "*^^*", "&")
    l_strTemp = Replace(m_strText, "&&", "")
    If InStr(l_strTemp, "&") Then
        m_lngHotkey = Asc(UCase(Mid(l_strTemp, InStr(l_strTemp, "&") + 1, 1)))
    End If
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Reflow
End Property

Public Property Get DisplayText() As String
    DisplayText = m_strDisplayText
End Property

Public Property Get Accelerator() As String
    Accelerator = m_strAccelerator
End Property

Public Property Let Accelerator(ByRef NewValue As String)
On Error Resume Next
    m_strAccelerator = NewValue
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Reflow
End Property

Public Property Get FullKey() As String
On Error Resume Next
    If m_mnuParent.ParentItem Is Nothing Then
        FullKey = m_strKey
    Else
        FullKey = m_mnuParent.ParentItem.FullKey & "\" & m_strKey
    End If
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
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Reflow
End Property

Public Property Get ClickEvent() As ngEvent
    Set ClickEvent = m_evtClick
End Property

Public Property Set ClickEvent(ByVal NewValue As ngEvent)
    Set m_evtClick = NewValue
End Property

Public Property Get HoverEvent() As ngEvent
    Set HoverEvent = m_evtHover
End Property

Public Property Set HoverEvent(ByVal NewValue As ngEvent)
    Set m_evtHover = NewValue
End Property

Public Property Get Checked() As Boolean
    Checked = m_booChecked
End Property

Public Property Let Checked(ByVal NewValue As Boolean)
On Error Resume Next
    If NewValue = m_booChecked Then Exit Property
    m_booChecked = NewValue
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Host.Redraw Me.Rectangle
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
    If NewValue = m_booEnabled Then Exit Property
    m_booEnabled = NewValue
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Host.Redraw Me.Rectangle
End Property

Public Property Get Visible() As Boolean
    Visible = m_booVisible
End Property

Public Property Let Visible(ByVal NewValue As Boolean)
On Error Resume Next
    If NewValue = m_booVisible Then Exit Property
    m_booVisible = NewValue
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Reflow
End Property

Public Property Get Pressed() As Boolean
On Error Resume Next
    If ChildMenu Is Nothing Then
        Pressed = m_booPressed
    Else
        If ChildMenu.Visible Then Exit Property
        Pressed = m_booPressed
    End If
End Property

Public Property Get Hovering() As Boolean
    Hovering = m_booHovering
End Property

Public Property Get Hotkey() As Long
    Hotkey = m_lngHotkey
End Property

Public Property Get Style() As ngMenuItemStyles
    Style = m_tbsStyle
End Property

Public Property Let Style(ByVal NewValue As ngMenuItemStyles)
On Error Resume Next
    m_tbsStyle = NewValue
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Reflow
End Property

Public Property Get TextAlignment() As ngMenuItemTextAlignment
    TextAlignment = m_txaTextAlignment
End Property

Public Property Let TextAlignment(ByVal NewValue As ngMenuItemTextAlignment)
On Error Resume Next
    m_txaTextAlignment = NewValue
    If Not (m_mnuParent Is Nothing) Then m_mnuParent.Reflow
End Property

Private Sub Class_Initialize()
On Error Resume Next
    m_txaTextAlignment = mtaRight
    m_booVisible = True
    m_booEnabled = True
End Sub
