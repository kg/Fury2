VERSION 5.00
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#8.10#0"; "ngUI.ocx"
Begin VB.UserControl EntityList 
   AutoRedraw      =   -1  'True
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   11.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   KeyPreview      =   -1  'True
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   Begin ngUI.ngToolbar tbrOptions 
      Height          =   390
      Left            =   1185
      TabIndex        =   1
      Top             =   1560
      Width           =   1965
      _ExtentX        =   3466
      _ExtentY        =   688
   End
   Begin cFScroll.FlatScrollBar vsScrollbar 
      Height          =   1590
      Left            =   675
      TabIndex        =   0
      Top             =   0
      Width           =   240
      _ExtentX        =   423
      _ExtentY        =   2805
      Orientation     =   1
      Max             =   100
      Style           =   -1
   End
End
Attribute VB_Name = "EntityList"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Private Declare Function ClientToScreen Lib "user32" (ByVal hwnd As Long, lpPoint As POINTAPI) As Long
Private m_objBoundObject As Object
Private m_booEnableDragging As Boolean
Private m_booEnableMultiSelect As Boolean
Private m_booActive As Boolean
Private m_booFocus As Boolean
Private m_booDragging As Boolean, m_objDragItem As Object, m_booDragged As Boolean
Private m_lngSelectedItem As Long
Private m_booSelection() As Boolean
Private m_lngItemHeight As Long, m_lngItemTextHeight As Long
Public Event ItemDragStart(ByVal Item As Long, ByRef Cancel As Boolean)
Public Event ItemDrag(ByVal Item As Long, ByVal NewIndex As Long)
Public Event ItemDragComplete(ByVal Item As Long)
Public Event ItemSelected(ByVal Item As Long)
Public Event ItemVisibilityChanged(ByVal Item As Long)
Public Event ContextMenu(ByVal X As Long, ByVal Y As Long)
Public Event ItemContextMenu(ByVal Item As Long, ByVal X As Long, ByVal Y As Long)
Public Event ToolbarClick(ByVal Button As ngUI.ngToolButton)
Public VisibleIcon As Fury2Image
Public InvisibleIcon As Fury2Image
Public ShowVisibilityToggles As Boolean
Private m_imgIcon As Fury2Image

Public Property Get Toolbar() As ngToolbar
On Error Resume Next
    Set Toolbar = tbrOptions
End Property

Public Sub ShowToolbar()
On Error Resume Next
    tbrOptions.Visible = True
    UserControl_Resize
End Sub

Public Sub HideToolbar()
On Error Resume Next
    tbrOptions.Visible = False
End Sub

Public Property Get BoundObject() As Object
On Error Resume Next
    Set BoundObject = m_objBoundObject
End Property

Public Property Set BoundObject(NewObject As Object)
On Error Resume Next
    If NewObject Is m_objBoundObject Then
    Else
        Set m_objBoundObject = NewObject
        m_lngSelectedItem = ClipValue(m_lngSelectedItem, 0, BoundObject.Count)
        Refresh
    End If
End Property

Public Property Get EnableDragging() As Boolean
On Error Resume Next
    EnableDragging = m_booEnableDragging
End Property

Public Property Let EnableDragging(ByVal NewValue As Boolean)
On Error Resume Next
    m_booEnableDragging = NewValue
    PropertyChanged "EnableDragging"
End Property

Public Property Get SelectedItem() As Long
On Error Resume Next
    SelectedItem = m_lngSelectedItem
End Property

Public Property Let SelectedItem(ByVal NewItem As Long)
On Error Resume Next
Dim l_lngY As Long, l_lngViewMinY As Long, l_lngViewMaxY As Long
Dim l_lngHeight As Long, l_lngMaxHeight As Long
    If NewItem = m_lngSelectedItem Then Exit Property
    m_lngSelectedItem = ClipValue(NewItem, 0, BoundObject.Count)
    If m_booActive = False Then Exit Property
    RaiseEvent ItemSelected(m_lngSelectedItem)
    UserControl_Paint
End Property

Public Sub Refresh()
On Error Resume Next
    UserControl_Paint
End Sub

Private Sub tbrOptions_GotFocus()
On Error Resume Next
    If Not m_booFocus Then
        m_booFocus = True
        If m_booActive Then UserControl_Paint
    End If
End Sub

Private Sub tbrOptions_LostFocus()
On Error Resume Next
    If m_booFocus Then
        m_booFocus = False
        If m_booActive Then UserControl_Paint
    End If
End Sub

Private Sub tbrOptions_ButtonClick(Button As ngUI.ngToolButton)
    RaiseEvent ToolbarClick(Button)
End Sub

Private Sub UserControl_GotFocus()
On Error Resume Next
    If Not m_booFocus Then
        m_booFocus = True
        UserControl_Paint
    End If
End Sub

Private Sub UserControl_Hide()
On Error Resume Next
    tbrOptions.Visible = False
    m_booFocus = False
    m_booActive = False
    m_booDragging = False
    UserControl.AutoRedraw = False
End Sub

Private Sub UserControl_Initialize()
On Error Resume Next
    Set VisibleIcon = Nothing
    Set InvisibleIcon = Nothing
    m_lngSelectedItem = -1
    vsScrollbar.Width = GetScrollbarSize(vsScrollbar)
End Sub

Private Sub UserControl_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    Select Case KeyCode
    Case vbKeyUp
        SelectedItem = ClipValue(m_lngSelectedItem - 1, 1, BoundObject.Count)
    Case vbKeyDown
        SelectedItem = ClipValue(m_lngSelectedItem + 1, 1, BoundObject.Count)
    Case Else
    End Select
End Sub

Private Sub UserControl_LostFocus()
On Error Resume Next
    m_booDragging = False
    If m_booFocus Then
        m_booFocus = False
        If m_booActive Then UserControl_Paint
    End If
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngMenuX As Long, l_lngMenuY As Long
Dim l_lngY As Long, l_lngIndex As Long
Dim l_objObject As Object
Dim l_ptPoint As POINTAPI
    If BoundObject Is Nothing Then Exit Sub
    l_lngY = Y + vsScrollbar.Value
    l_lngIndex = (l_lngY \ m_lngItemHeight) + 1
    If (l_lngIndex < 1) Or (l_lngIndex > BoundObject.Count) Then
        l_lngIndex = 0
    End If
    m_booDragging = False
    If Button = 1 Then
        If (X >= 26) Or (Not ShowVisibilityToggles) Then
            SelectedItem = l_lngIndex
            m_booDragging = False
            RaiseEvent ItemDragStart(l_lngIndex, m_booDragging)
            If m_booDragging Then
                m_booDragging = False
            Else
                m_booDragging = True
                Set m_objDragItem = BoundObject.Item(l_lngIndex)
            End If
        Else
            Set l_objObject = BoundObject.Item(l_lngIndex)
            l_objObject.Visible = Not l_objObject.Visible
            RaiseEvent ItemVisibilityChanged(l_lngIndex)
            UserControl_Paint
        End If
    ElseIf Button = 2 Then
        ClientToScreen UserControl.hwnd, l_ptPoint
        l_lngMenuX = l_ptPoint.X + X
        If (l_lngIndex >= 1) And (l_lngIndex <= BoundObject.Count) Then
            l_lngMenuY = l_ptPoint.Y + (((l_lngIndex - 1) * m_lngItemHeight) - vsScrollbar.Value) + m_lngItemHeight
        Else
            l_lngMenuY = l_ptPoint.Y + Y
        End If
        l_ptPoint.X = 0
        l_ptPoint.Y = 0
        ClientToScreen UserControl.Parent.hwnd, l_ptPoint
        l_lngMenuX = l_lngMenuX - l_ptPoint.X
        l_lngMenuY = l_lngMenuY - l_ptPoint.Y
        If (l_lngIndex >= 1) And (l_lngIndex <= BoundObject.Count) Then
            RaiseEvent ItemContextMenu(l_lngIndex, l_lngMenuX, l_lngMenuY)
        Else
            RaiseEvent ContextMenu(l_lngMenuX, l_lngMenuY)
        End If
    End If
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngY As Long, l_lngIndex As Long, l_lngOldIndex As Long
    l_lngY = Y + vsScrollbar.Value
    l_lngIndex = (l_lngY \ m_lngItemHeight) + 1
    If (Button = 1) And (m_booDragging) And (m_booEnableDragging) Then
        If (l_lngIndex <= BoundObject.Count) And (l_lngIndex >= 1) Then
            l_lngOldIndex = BoundObject.Find(m_objDragItem)
            If (l_lngOldIndex) And (l_lngOldIndex <> l_lngIndex) Then
                RaiseEvent ItemDrag(l_lngOldIndex, l_lngIndex)
                BoundObject.Move l_lngOldIndex, l_lngIndex
                SelectedItem = l_lngIndex
                m_booDragged = True
            End If
        End If
    End If
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If m_booDragging Then
        m_booDragging = False
        Set m_objDragItem = Nothing
        If m_booDragged Then RaiseEvent ItemDragComplete(m_lngSelectedItem)
    End If
    m_booDragging = False
    m_booDragged = False
End Sub

Private Sub UserControl_Paint()
On Error Resume Next
Static m_booHere As Boolean
Dim l_lngY As Long, l_lngHeight As Long, l_lngItems As Long, l_lngMaxHeight As Long
Dim l_objObject As Object, l_rctItem As RECT
    If m_booActive = False Then Exit Sub
    If m_booHere Then Exit Sub
    vsScrollbar.Move UserControl.ScaleWidth - vsScrollbar.Width, 0, vsScrollbar.Width, UserControl.ScaleHeight - IIf(tbrOptions.Visible, tbrOptions.Height, 0)
    m_booHere = True
    UserControl.Line (0, 0)-(vsScrollbar.Left - 1, UserControl.ScaleHeight - 1), UserControl.BackColor, BF
    If BoundObject Is Nothing Then
        m_booHere = False
        Exit Sub
    End If
    l_lngHeight = (BoundObject.Count + 1) * m_lngItemHeight
    l_lngMaxHeight = UserControl.ScaleHeight - IIf(tbrOptions.Visible, tbrOptions.Height, 0)
    If l_lngHeight > l_lngMaxHeight Then
        If vsScrollbar.Enabled Then Else vsScrollbar.Enabled = True
        If vsScrollbar.LargeChange <> ClipValue(l_lngMaxHeight, 0, (l_lngHeight - l_lngMaxHeight)) Then vsScrollbar.LargeChange = ClipValue(l_lngMaxHeight, 0, (l_lngHeight - l_lngMaxHeight))
        If vsScrollbar.SmallChange <> m_lngItemHeight Then vsScrollbar.SmallChange = m_lngItemHeight
        If vsScrollbar.Max <> (l_lngHeight - l_lngMaxHeight) Then vsScrollbar.Max = l_lngHeight - l_lngMaxHeight
        l_lngY = -vsScrollbar.Value
    Else
        If vsScrollbar.Enabled Then vsScrollbar.Enabled = False
        If vsScrollbar.Value <> 0 Then vsScrollbar.Value = 0
        l_lngY = 0
    End If
    SelectedItem = ClipValue(m_lngSelectedItem, 0, BoundObject.Count)
    If BoundObject.Count > 0 Then
        For l_lngItems = 1 To BoundObject.Count
            With UserControl
                If ((l_lngY + m_lngItemHeight) >= 0) And (l_lngY < l_lngMaxHeight) Then
                    Set l_objObject = BoundObject.Item(l_lngItems)
                    If m_lngSelectedItem = l_lngItems Then
                        UserControl.Line (0, l_lngY)-(vsScrollbar.Left - 1, l_lngY + m_lngItemHeight - 1), vbHighlight, BF
                        .ForeColor = vbHighlightText
                        .Font.Bold = True
                        If m_booFocus Then
                            With l_rctItem
                                .Left = 1
                                .Top = l_lngY + 1
                                .Right = vsScrollbar.Left - 1
                                .Bottom = l_lngY + (m_lngItemHeight - 1)
                            End With
                            DrawFocusRect UserControl.hdc, l_rctItem
                        End If
                    Else
                        .ForeColor = vbButtonText
                        .Font.Bold = False
                    End If
                    If ShowVisibilityToggles Then
                        .CurrentX = 4 + (VisibleIcon.Width)
                    Else
                        .CurrentX = 4
                    End If
                    .CurrentY = l_lngY + ((m_lngItemHeight - m_lngItemTextHeight) \ 2)
                    UserControl.Print l_objObject.Name
                    If ShowVisibilityToggles Then
                        If m_imgIcon Is Nothing Then
                            Set m_imgIcon = VisibleIcon.Duplicate
                        Else
                            m_imgIcon.Resize VisibleIcon.Width, VisibleIcon.Height
                        End If
                        If m_lngSelectedItem = l_lngItems Then
                            m_imgIcon.Clear SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue)
                        Else
                            m_imgIcon.Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
                        End If
                        m_imgIcon.Blit , , IIf(l_objObject.Visible, VisibleIcon, InvisibleIcon), , BlitMode_SourceAlpha
                        If m_lngSelectedItem = l_lngItems Then
                            m_imgIcon.Fill m_imgIcon.Rectangle, SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
                        End If
                        CopyImageToDC UserControl.hdc, F2Rect(2, l_lngY + 2, m_imgIcon.Width, m_imgIcon.Height, False), m_imgIcon
                    End If
                End If
            End With
            l_lngY = l_lngY + m_lngItemHeight
        Next l_lngItems
    End If
    UserControl.Refresh
    m_booHere = False
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
On Error Resume Next
    m_booEnableDragging = PropBag.ReadProperty("EnableDragging", True)
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
    vsScrollbar.Move UserControl.ScaleWidth - vsScrollbar.Width, 0, vsScrollbar.Width, UserControl.ScaleHeight - IIf(tbrOptions.Visible, tbrOptions.Height, 0)
    If tbrOptions.Visible Then
        If tbrOptions.Width <> ClipValue(UserControl.ScaleWidth, tbrOptions.IdealWidth, 999999) Then tbrOptions.Width = ClipValue(UserControl.ScaleWidth, tbrOptions.IdealWidth, 999999)
        If tbrOptions.Height <> tbrOptions.IdealHeight(tbrOptions.Width) Then tbrOptions.Height = tbrOptions.IdealHeight(tbrOptions.Width)
        tbrOptions.Move 0, UserControl.ScaleHeight - tbrOptions.Height
    End If
    UserControl_Paint
End Sub

Private Sub UserControl_Show()
On Error Resume Next
    UserControl.AutoRedraw = True
    m_booActive = True
    m_lngItemTextHeight = UserControl.TextHeight("AaBbYyZz")
    m_lngItemHeight = ClipValue(m_lngItemTextHeight, 24, 32)
    UserControl_Paint
End Sub

Private Sub UserControl_Terminate()
On Error Resume Next
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
On Error Resume Next
    PropBag.WriteProperty "EnableDragging", m_booEnableDragging
End Sub

Private Sub vsScrollbar_Change()
On Error Resume Next
    UserControl_Paint
End Sub

Private Sub vsScrollbar_GotFocus()
On Error Resume Next
    If Not m_booFocus Then
        m_booFocus = True
        UserControl_Paint
    End If
End Sub

Private Sub vsScrollbar_LostFocus()
On Error Resume Next
    If m_booFocus Then
        m_booFocus = False
        UserControl_Paint
    End If
End Sub
