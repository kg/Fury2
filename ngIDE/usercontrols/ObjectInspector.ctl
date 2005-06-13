VERSION 5.00
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Begin VB.UserControl ObjectInspector 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
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
   Begin VB.PictureBox picSplit 
      BorderStyle     =   0  'None
      Height          =   3570
      Left            =   15
      ScaleHeight     =   238
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   310
      TabIndex        =   0
      Top             =   -15
      Width           =   4650
      Begin VB.PictureBox picItems 
         AutoRedraw      =   -1  'True
         BorderStyle     =   0  'None
         Height          =   2820
         Left            =   210
         ScaleHeight     =   188
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   245
         TabIndex        =   2
         Top             =   435
         Width           =   3675
         Begin VB.PictureBox picHierarchy 
            BorderStyle     =   0  'None
            Height          =   270
            Left            =   15
            ScaleHeight     =   18
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   307
            TabIndex        =   7
            Top             =   0
            Width           =   4605
            Begin VB.TextBox txtFilter 
               BackColor       =   &H8000000F&
               BorderStyle     =   0  'None
               Height          =   255
               Left            =   0
               TabIndex        =   8
               ToolTipText     =   "Filter"
               Top             =   0
               Width           =   1125
            End
         End
         Begin VB.CommandButton cmdDropDown 
            Caption         =   "7"
            BeginProperty Font 
               Name            =   "Marlett"
               Size            =   8.25
               Charset         =   2
               Weight          =   500
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   270
            Left            =   2565
            TabIndex        =   6
            Top             =   0
            Width           =   270
         End
         Begin VB.CommandButton cmdElipsis 
            Caption         =   "…"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   6.75
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   270
            Left            =   2280
            TabIndex        =   5
            Top             =   0
            Visible         =   0   'False
            Width           =   270
         End
         Begin VB.TextBox txtEdit 
            BorderStyle     =   0  'None
            Height          =   300
            Left            =   0
            TabIndex        =   4
            Top             =   0
            Visible         =   0   'False
            Width           =   2265
         End
         Begin cFScroll.FlatScrollBar vsScroll 
            Height          =   3600
            Left            =   0
            TabIndex        =   3
            Top             =   0
            Width           =   240
            _ExtentX        =   423
            _ExtentY        =   6350
            Orientation     =   1
            Max             =   100
            Style           =   -1
         End
      End
      Begin VB.PictureBox picInfo 
         BorderStyle     =   0  'None
         Height          =   735
         Left            =   210
         ScaleHeight     =   49
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   240
         TabIndex        =   1
         Top             =   2760
         Width           =   3600
      End
   End
End
Attribute VB_Name = "ObjectInspector"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False

'    ngIDE (Fury² Game Creation System Next-Generation Editor)
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
Private Declare Function DrawText Lib "user32" Alias "DrawTextA" (ByVal hdc As Long, ByVal lpStr As String, ByVal nCount As Long, lpRect As Win32.Rect, ByVal wFormat As Long) As Long
Private Const DT_WORDBREAK = &H10
Private Const DT_NOPREFIX = &H800
Private Const MinimumEditWidth As Long = 60

Private m_colObjectStack As New Collection
Private m_colNameStack As New Collection

Private Enum ObjectItemTypes
    OIT_Normal
    OIT_Filename
    OIT_MapFilename
    OIT_ImageFilename
    OIT_SpriteFilename
    OIT_WindowsFilename
    OIT_Hex
    OIT_Color
    OIT_Enum
    OIT_CollectionItem
End Enum

Private Type ObjectItem
    Name As String
    Value As Variant
    ValueText As String
    CallTypes As Long
    Description As String
    CanInspect As Boolean
    ShowElipsis As Boolean
    ShowDropdown As Boolean
    Filtered As Boolean
    DataType As TliVarType
    SpecialType As ObjectItemTypes
    DropdownValues() As Variant
    DropdownText() As String
End Type

Public InspectAny As Boolean

Private m_booGroupEdit As Boolean
Private m_objGroupItems() As Object

Private m_booVisible As Boolean
Private m_booNoItems As Boolean

Private m_booHaveFocus As Boolean
Private m_booEditBoxActive As Boolean

Private m_splSplitter As New cSplitter

Private m_lngSelectedItem As Long

Private m_strFilter As String
Private m_booFilterActive As Boolean

Private m_lngNameWidth As Long
Private m_lngTotalHeight As Long
Private m_lngItemHeight As Long
Private m_lngEditY As Long

Private m_objObject As Object
Private m_objObjects() As Object
Private m_oiItems() As ObjectItem

Private m_booMultiple As Boolean

Private m_imgColorBuffer As Fury2Image
Private m_imgColorBufferBG As Fury2Image

Private m_booShowInfobox As Boolean
Private m_booShowHierarchy As Boolean

Public Editor As Object

Public Event EllipsisPressed(ByVal Index As Long)
Public Event ItemSelected(ByVal Index As Long)
Public Event ItemTitleDoubleClick(ByVal Index As Long)
Public Event BeforeItemChange(ByRef Cancel As Boolean)
Public Event AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)

Public Property Get ShowInfobox() As Boolean
On Error Resume Next
    ShowInfobox = m_booShowInfobox
End Property

Public Property Let ShowInfobox(ByVal NewValue As Boolean)
On Error Resume Next
    m_booShowInfobox = NewValue
    With m_splSplitter
        If m_booShowInfobox Then
            .MinimumSize(cSPLTRightOrBottomPanel) = 25
            .MaximumSize(cSPLTRightOrBottomPanel) = 80
        Else
            .MinimumSize(cSPLTRightOrBottomPanel) = 1
            .MaximumSize(cSPLTRightOrBottomPanel) = 1
        End If
    End With
    UserControl_Resize
    picSplit_Resize
End Property

Public Property Get ShowHierarchy() As Boolean
On Error Resume Next
    ShowHierarchy = m_booShowHierarchy
End Property

Public Property Let ShowHierarchy(ByVal NewValue As Boolean)
On Error Resume Next
    m_booShowHierarchy = NewValue
    UserControl_Resize
    picItems_Resize
End Property

Public Function SelectItemByName(ByVal Name As String, Optional ByVal Redraw As Boolean = True) As Long
On Error Resume Next
Dim l_lngItems As Long
    SelectItemByName = -1
    m_lngSelectedItem = 0
    Name = LCase(Trim(Name))
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        If LCase(Trim(m_oiItems(l_lngItems).Name)) = Name Then
            m_lngSelectedItem = l_lngItems
            SelectItemByName = l_lngItems
            Exit For
        End If
    Next l_lngItems
    If Redraw Then Me.Redraw
    RefreshEditBox
    RaiseEvent ItemSelected(m_lngSelectedItem)
End Function

Public Sub ClearStack()
On Error Resume Next
    Set m_colObjectStack = New Collection
    Set m_colNameStack = New Collection
End Sub

Public Sub AddToStack(Obj As Object, Optional ByVal Title As String = "Object")
On Error Resume Next
    m_colObjectStack.Add Obj
    m_colNameStack.Add Title
End Sub

Public Property Get hwnd() As Long
On Error Resume Next
    hwnd = UserControl.hwnd
End Property

Public Sub RefreshFilter()
On Error Resume Next
Dim l_lngItems As Long
Dim l_lngFirstIndex As Long
    l_lngFirstIndex = -1
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        With m_oiItems(l_lngItems)
            If Len(m_strFilter) > 0 Then
                .Filtered = Not (InStr(1, .Name, m_strFilter, vbTextCompare) > 0)
            Else
                .Filtered = False
            End If
            If Not (.Filtered) Then
                If l_lngFirstIndex = -1 Then l_lngFirstIndex = l_lngItems
            End If
        End With
    Next l_lngItems
    m_lngSelectedItem = l_lngFirstIndex
    CalculateSize
    picItems_Paint
    picInfo_Paint
    RefreshEditBox
End Sub

Public Sub RefreshEditBox(Optional SelectText As Boolean = True)
On Error Resume Next
Static l_lngOldIndex As Long
Dim l_lngNameWidth As Long
Dim l_vtType As VbVarType
Dim l_lngButtonWidth As Long
Dim l_lngLeftSpace As Long
Dim l_lngItems As Long, l_lngY As Long
    If (m_objObject Is Nothing) Or (m_booNoItems) Then
        txtEdit.Visible = False
        cmdDropDown.Visible = False
        cmdElipsis.Visible = False
        Exit Sub
    End If
    Err.Clear
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        If Not (m_oiItems(l_lngItems).Filtered) Then
            If (m_oiItems(l_lngItems).CallTypes And VbGet) = VbGet Then
                If (l_lngItems = m_lngSelectedItem) Then
                    m_lngEditY = l_lngY
                    Exit For
                End If
                l_lngY = l_lngY + m_lngItemHeight
            End If
        End If
    Next l_lngItems
    With m_oiItems(m_lngSelectedItem)
        If Err <> 0 Then Exit Sub
        l_lngNameWidth = m_lngNameWidth
        If (l_lngNameWidth > ((picItems.ScaleWidth - vsScroll.Width - MinimumEditWidth))) Then
            l_lngNameWidth = ((picItems.ScaleWidth - vsScroll.Width - MinimumEditWidth))
        End If
        If (VarType(.Value) = vbBoolean) And ((.CallTypes And VbLet) = VbLet) Then
            .ShowDropdown = True
        ElseIf .SpecialType = OIT_Enum Then
            .ShowDropdown = True
        ElseIf VarType(.Value) = vbObject Then
            If .Value Is Nothing Then
                .CanInspect = False
            ElseIf TypeOf .Value Is IInspectable Then
                .CanInspect = True
            ElseIf TypeOf .Value Is IInspectableCollection Then
                .CanInspect = True
            Else
                .CanInspect = InspectAny
            End If
        End If
        If .SpecialType = OIT_Color Then
            l_lngLeftSpace = m_imgColorBuffer.Width + 2
            .ShowElipsis = True
        ElseIf .SpecialType = OIT_Filename Then
            .ShowElipsis = True
        ElseIf .SpecialType = OIT_ImageFilename Then
            .ShowElipsis = True
        ElseIf .SpecialType = OIT_WindowsFilename Then
            .ShowElipsis = True
        ElseIf .CanInspect Then
            .ShowElipsis = True
        End If
        txtEdit.Visible = False
        If (Not m_booEditBoxActive) Or (txtEdit.Locked) Or (l_lngOldIndex <> m_lngSelectedItem) Then txtEdit.Text = .ValueText
        If (.ShowDropdown) Then
            cmdDropDown.Move vsScroll.Left - cmdDropDown.Width - 1, m_lngEditY - vsScroll.Value + 1 + picHierarchy.Height, cmdDropDown.Width, m_lngItemHeight - 1
            cmdDropDown.Visible = True
            l_lngButtonWidth = l_lngButtonWidth + cmdDropDown.Width
            cmdElipsis.Visible = False
        ElseIf (.ShowElipsis) Then
            cmdElipsis.Move vsScroll.Left - cmdElipsis.Width - 1, m_lngEditY - vsScroll.Value + 1 + picHierarchy.Height, cmdElipsis.Width, m_lngItemHeight - 1
            cmdElipsis.Visible = True
            l_lngButtonWidth = l_lngButtonWidth + cmdElipsis.Width
            cmdDropDown.Visible = False
        Else
            cmdDropDown.Visible = False
            cmdElipsis.Visible = False
        End If
        txtEdit.Move l_lngNameWidth + 6 + l_lngLeftSpace, m_lngEditY - vsScroll.Value + 2 + picHierarchy.Height, vsScroll.Left - (l_lngNameWidth + 8) - l_lngButtonWidth - l_lngLeftSpace, m_lngItemHeight - 2
        l_vtType = VarType(m_oiItems(m_lngSelectedItem).Value)
        txtEdit.Locked = ((m_oiItems(m_lngSelectedItem).CallTypes And VbLet) <> VbLet) And (Not (TypeOf m_oiItems(m_lngSelectedItem).Value Is IInspectorType))
        txtEdit.ForeColor = IIf(txtEdit.Locked, vbButtonShadow, vbWindowText)
        Select Case l_vtType
        Case vbObject
            If TypeOf m_oiItems(m_lngSelectedItem).Value Is IInspectorType Then
                If Not m_oiItems(m_lngSelectedItem).Filtered Then txtEdit.Visible = True
            End If
        Case Else
            If (l_vtType And vbArray) = vbArray Then
            Else
                If Not m_oiItems(m_lngSelectedItem).Filtered Then txtEdit.Visible = True
            End If
        End Select
        If m_booHaveFocus Then
            If m_booFilterActive Then
            Else
                If txtEdit.Visible Then
                    If UserControl.ActiveControl Is cmdElipsis Then
                    ElseIf UserControl.ActiveControl Is cmdDropDown Then
                    Else
                        txtEdit.SetFocus
                    End If
                    If SelectText Then
                        txtEdit.SelStart = 0
                        txtEdit.SelLength = Len(txtEdit.Text)
                    End If
                Else
                    If UserControl.ActiveControl Is cmdElipsis Then
                    ElseIf UserControl.ActiveControl Is cmdDropDown Then
                    Else
                        picItems.SetFocus
                    End If
                End If
            End If
        End If
    End With
    l_lngOldIndex = m_lngSelectedItem
End Sub

Public Property Get CurrentObject() As Object
On Error Resume Next
    Set CurrentObject = m_objObject
End Property

Public Property Get SelectedItem() As Long
On Error Resume Next
    SelectedItem = m_lngSelectedItem
End Property

Public Property Get ItemName(ByVal Index As Long) As String
On Error Resume Next
    ItemName = m_oiItems(Index).Name
End Property

Public Property Get ItemValue(ByVal Index As Long)
On Error Resume Next
    Err.Clear
    Set ItemValue = m_oiItems(Index).Value
    If Err <> 0 Then
        ItemValue = m_oiItems(Index).Value
    End If
End Property

Public Sub Redraw()
On Error Resume Next
    picItems_Paint
    picInfo_Paint
    picHierarchy_Paint
End Sub

Public Sub RefreshValues()
On Error Resume Next
Dim l_lngItems As Long, l_lngIndex As Long
Dim m_itValue As IInspectorType
Dim l_varValue As Variant
Dim l_varFirstValue As Variant
Dim l_objObject As Object, l_lngObject As Long
Dim l_strValue As String, l_strIndex As String
Dim l_colObject As IInspectableCollection
Dim l_booInspectorType As Boolean
    Set l_colObject = m_objObject
    Err.Clear
    If m_objObject Is Nothing Then Exit Sub
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        With m_oiItems(l_lngItems)
            If ((.CallTypes And VbGet) = VbGet) Then
                If .SpecialType = OIT_CollectionItem Then
                    l_varValue = Empty
                    l_strIndex = Replace(.Name, "Item ", "")
                    l_lngIndex = CLng(l_strIndex)
                    Err.Clear
                    Set .Value = l_colObject.ItemValue(l_lngIndex)
                    If Err <> 0 Then
                        .Value = l_colObject.ItemValue(l_lngIndex)
                    End If
                    Err.Clear
                ElseIf m_booMultiple Then
                    l_varFirstValue = Empty
                    l_varValue = Empty
                    For l_lngObject = LBound(m_objObjects) To UBound(m_objObjects)
                        Set l_objObject = m_objObjects(l_lngObject)
                        Err.Clear
                        Set l_varValue = CallByName(l_objObject, .Name, VbGet)
                        If Err <> 0 Then
                            Err.Clear
                            l_varValue = CallByName(l_objObject, .Name, VbGet)
                            If Err <> 0 Then
                                l_varValue = "{Error}"
                                Exit For
                            Else
                                If IsEmpty(l_varFirstValue) Then l_varFirstValue = l_varValue
                                If Not (l_varFirstValue = l_varValue) Then
                                    l_varValue = "{Multiple Values}"
                                    Exit For
                                End If
                            End If
                        Else
                            If TypeOf l_varValue Is IInspectorType Then
                                Set m_itValue = l_varValue
                                l_strValue = m_itValue.ToString
                                If IsEmpty(l_varFirstValue) Then l_varFirstValue = l_strValue
                                If Not (l_varFirstValue = l_strValue) Then
                                    l_varValue = "{Multiple Values}"
                                    Exit For
                                End If
                            Else
                                If IsEmpty(l_varFirstValue) Then Set l_varFirstValue = l_varValue
                                If Not (l_varFirstValue Is l_varValue) Then
                                    l_varValue = "{Multiple Values}"
                                    Exit For
                                End If
                            End If
                        End If
                    Next l_lngObject
                    Err.Clear
                    Set .Value = l_varValue
                    If Err <> 0 Then
                        .Value = l_varValue
                    End If
                Else
                    Err.Clear
                    Set .Value = CallByName(m_objObject, .Name, VbGet)
                    If Err <> 0 Then
                        Err.Clear
                        .Value = CallByName(m_objObject, .Name, VbGet)
                        If Err <> 0 Then
                            .Value = "{Error}"
                        End If
                    End If
                End If
                If (.Value Is Nothing) Or (VarType(.Value) <> vbObject) Then
                    l_booInspectorType = False
                Else
                    l_booInspectorType = (TypeOf .Value Is IInspectorType)
                End If
                If l_booInspectorType Then
                    Set m_itValue = .Value
                    .ValueText = m_itValue.ToString
                Else
                    Select Case .SpecialType
                    Case OIT_Hex
                        .ValueText = Hex(.Value)
                    Case OIT_Color
                        .ValueText = Hex(.Value)
                        If Len(.ValueText) < 8 Then
                            .ValueText = String(8 - Len(.ValueText), "0") + .ValueText
                        End If
                    Case OIT_Enum
                        Dim l_lngEnumItems As Long
                        For l_lngEnumItems = 0 To UBound(.DropdownValues)
                            If .Value = .DropdownValues(l_lngEnumItems) Then
                                .ValueText = .DropdownText(l_lngEnumItems)
                            End If
                        Next l_lngEnumItems
                    Case Else
                        If VarType(.Value) = vbString Then
                            .ValueText = .Value
                        Else
                            .ValueText = Fury2Globals.ToString(.Value, True)
                        End If
                    End Select
                End If
                If VarType(.Value) = vbBoolean Then
                    ReDim .DropdownValues(0 To 1)
                    .DropdownValues(0) = False
                    .DropdownValues(1) = True
                    ReDim .DropdownText(0 To 1)
                    .DropdownText(0) = CStr(False)
                    .DropdownText(1) = CStr(True)
                End If
            End If
        End With
    Next l_lngItems
    RefreshEditBox
End Sub

Public Sub CalculateSize()
On Error Resume Next
Dim l_lngItems As Long
Dim l_lngY As Long, l_lngWidth As Long
    If m_objObject Is Nothing Then Exit Sub
    m_lngItemHeight = TextHeight("AaBbYyZz") + 4
    m_lngNameWidth = 0
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        With m_oiItems(l_lngItems)
            If Not (.Filtered) Then
                If (.CallTypes And VbGet) = VbGet Then
                    l_lngWidth = TextWidth(.Name)
                    If l_lngWidth > m_lngNameWidth Then m_lngNameWidth = l_lngWidth
                    l_lngY = l_lngY + m_lngItemHeight
                End If
            End If
        End With
    Next l_lngItems
    m_lngTotalHeight = l_lngY
    m_booNoItems = (m_lngTotalHeight <= 0)
    UserControl_Resize
    picSplit_Resize
    picItems_Resize
    m_splSplitter.Resize
End Sub

Private Sub SortItems(Optional Descending As Boolean = False)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim p As Long, K As Long, H As Long, i As Long, J As Long
Dim Temp As ObjectItem
    Err.Clear
    m_lngLB = LBound(m_oiItems)
    m_lngUB = UBound(m_oiItems)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If m_lngCount < 2 Then Exit Sub
    For p = m_lngLB To ClipValue(m_lngUB - 1, m_lngLB, m_lngUB)
        H = p
        If Descending Then
            For K = p + 1 To m_lngUB
                If m_oiItems(K).Name > m_oiItems(H).Name Then H = K
            Next K
        Else
            For K = p + 1 To m_lngUB
                If m_oiItems(K).Name < m_oiItems(H).Name Then H = K
            Next K
        End If
        If p <> H Then
            i = H
            J = p
            Temp = m_oiItems(i)
            m_oiItems(i) = m_oiItems(J)
            m_oiItems(J) = Temp
        End If
    Next p
    Err.Clear
End Sub

Public Sub InspectMultiple(Objects() As Object)
On Error Resume Next
Dim l_iifObject As InterfaceInfo
Dim l_lngItems As Long
Dim l_lngItemCount As Long
Dim l_booAddNew As Boolean
Dim l_lngStack As Long, l_booFound As Boolean
Dim l_strInfo As String
Dim l_lngOldIndex As Long, l_lngIndex As Long, l_objFind As Object
Dim l_strSelectedItem As String
    Set m_colObjectStack = New Collection
    Set m_colNameStack = New Collection
    Set m_objObject = Objects(LBound(Objects))
    m_objObjects = Objects
    m_colObjectStack.Add Nothing
    m_colNameStack.Add "(" & (UBound(Objects) - LBound(Objects) + 1) & " objects)"
    m_booMultiple = True
    Reinspect
End Sub

Public Sub Inspect(Obj As Object, Optional ByVal Title As String = "Object", Optional ByVal TopLevel As Boolean = True, Optional ByVal IgnoreStack As Boolean = False, Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim l_iifObject As InterfaceInfo
Dim l_lngItems As Long
Dim l_lngItemCount As Long
Dim l_booAddNew As Boolean
Dim l_lngStack As Long, l_booFound As Boolean
Dim l_strInfo As String
Dim l_lngOldIndex As Long, l_lngIndex As Long, l_objFind As Object
Dim l_strSelectedItem As String
    m_booMultiple = False
    If Obj Is Nothing Then
        ClearStack
        Erase m_oiItems
        ReDim m_oiItems(0 To 0)
        Set m_objObject = Nothing
        CalculateSize
        picItems_Resize
        Redraw
        Exit Sub
    End If
    If (TypeOf Obj Is IInspectable) Or (TypeOf Obj Is IInspectableCollection) Or (Force) Or (InspectAny) Then
    Else
        Erase m_oiItems
        ReDim m_oiItems(0 To 0)
        Set m_objObject = Nothing
        CalculateSize
        picItems_Resize
        Redraw
        Exit Sub
    End If
    If TopLevel Then
        Set m_colObjectStack = New Collection
        Set m_colNameStack = New Collection
    End If
    l_lngIndex = 1
    For Each l_objFind In m_colObjectStack
        If l_objFind Is m_objObject Then
            l_lngOldIndex = l_lngIndex
            Exit For
        End If
        l_lngIndex = l_lngIndex + 1
    Next l_objFind
    Set m_objObject = Obj
    If Not IgnoreStack Then
        For Each l_objFind In m_colObjectStack
            If l_objFind Is Obj Then
                l_booFound = True
                Exit For
            End If
        Next l_objFind
        If Not l_booFound Then
            If l_lngOldIndex < m_colObjectStack.Count Then
                Do While m_colObjectStack.Count > l_lngOldIndex
                    m_colObjectStack.Remove m_colObjectStack.Count
                    m_colNameStack.Remove m_colNameStack.Count
                Loop
            End If
            m_colObjectStack.Add Obj
            m_colNameStack.Add Title
        End If
    End If
    Reinspect
End Sub

Public Sub Reinspect()
On Error Resume Next
Dim l_iifObject As InterfaceInfo
Dim l_lngItems As Long
Dim l_lngItemCount As Long
Dim l_booAddNew As Boolean
Dim l_lngStack As Long, l_booFound As Boolean
Dim l_strInfo As String
Dim l_lngOldIndex As Long, l_lngIndex As Long, l_objFind As Object, l_lngOffset As Long
Dim l_strSelectedItem As String
Dim l_colObject As IInspectableCollection
    EditBoxChanged
    m_booEditBoxActive = False
    l_strSelectedItem = m_oiItems(m_lngSelectedItem).Name
    Screen.MousePointer = 11
    ReDim m_oiItems(0 To 0)
    txtEdit.Visible = False
    txtEdit.Locked = True
    txtEdit.Text = ""
    cmdElipsis.Visible = False
    cmdDropDown.Visible = False
    Screen.MousePointer = 0
    Set l_iifObject = InterfaceInfoFromObject(m_objObject)
    If l_iifObject Is Nothing Then
        Erase m_oiItems
        ReDim m_oiItems(0 To 0)
        Set m_objObject = Nothing
        Screen.MousePointer = 0
        Exit Sub
    End If
    If l_iifObject.Members.Count < 1 Then
        Erase m_oiItems
        Set m_objObject = Nothing
        Screen.MousePointer = 0
        Exit Sub
    End If
    l_booFound = False
    With l_iifObject.Members
        For l_lngItems = 1 To .Count
            With .Item(l_lngItems)
                If ((.AttributeMask And 1) = 0) And ((.AttributeMask And 64) = 0) Then
                    l_booAddNew = True
                    Select Case .InvokeKind
                    Case VbGet, VbLet, VbSet, 0
                        If .Parameters.Count = 0 Then
                            l_strInfo = .HelpString
                            If Left(l_strInfo, 1) = "*" Then
                            Else
                                If (l_lngItemCount < 1) Then
                                Else
                                    If (m_oiItems(l_lngItemCount - 1).Name = .Name) Then
                                        l_booAddNew = False
                                    End If
                                End If
                                If (l_booAddNew) Then
                                    l_lngItemCount = l_lngItemCount + 1
                                    If UBound(m_oiItems) < (l_lngItemCount - 1) Then
                                        ReDim Preserve m_oiItems(0 To l_lngItemCount - 1)
                                    End If
                                    Erase m_oiItems(l_lngItemCount).DropdownValues
                                    Erase m_oiItems(l_lngItemCount).DropdownText
                                    m_oiItems(l_lngItemCount - 1).Name = .Name
                                    m_oiItems(l_lngItemCount - 1).SpecialType = OIT_Normal
                                    m_oiItems(l_lngItemCount - 1).DataType = .ReturnType.VarType
                                    If (.ReturnType.TypeInfo Is Nothing) Then
                                    ElseIf .ReturnType.TypeInfo.TypeKind = TKIND_ENUM Then
                                        m_oiItems(l_lngItemCount - 1).SpecialType = OIT_Enum
                                        ReDim m_oiItems(l_lngItemCount - 1).DropdownValues(0 To .ReturnType.TypeInfo.Members.Count - 1)
                                        ReDim m_oiItems(l_lngItemCount - 1).DropdownText(0 To .ReturnType.TypeInfo.Members.Count - 1)
                                        Dim l_memMember As MemberInfo, l_lngValue As Long
                                        l_lngValue = 0
                                        For Each l_memMember In .ReturnType.TypeInfo.Members
                                            m_oiItems(l_lngItemCount - 1).DropdownText(l_lngValue) = l_memMember.Name
                                            m_oiItems(l_lngItemCount - 1).DropdownValues(l_lngValue) = l_memMember.Value
                                            l_lngValue = l_lngValue + 1
                                        Next l_memMember
                                    End If
                                    If Left(l_strInfo, 1) = "~" Then
                                        l_strInfo = Mid(l_strInfo, 2)
                                        m_oiItems(l_lngItemCount - 1).ShowElipsis = True
                                    End If
                                    If InStr(l_strInfo, "{") Then
                                        Select Case LCase(Trim(Mid(l_strInfo, InStr(l_strInfo, "{") + 1, InStrRev(l_strInfo, "}") - InStr(l_strInfo, "{") - 1)))
                                        Case "hex"
                                            m_oiItems(l_lngItemCount - 1).SpecialType = OIT_Hex
                                        Case "color"
                                            m_oiItems(l_lngItemCount - 1).SpecialType = OIT_Color
                                        Case "filename", "path"
                                            m_oiItems(l_lngItemCount - 1).SpecialType = OIT_Filename
                                        Case "imagefilename", "imagepath"
                                            m_oiItems(l_lngItemCount - 1).SpecialType = OIT_ImageFilename
                                        Case "winfilename"
                                            m_oiItems(l_lngItemCount - 1).SpecialType = OIT_WindowsFilename
                                        End Select
                                        l_strInfo = Left(l_strInfo, InStr(l_strInfo, "{") - 1)
                                    End If
                                    m_oiItems(l_lngItemCount - 1).Description = l_strInfo
                                End If
                                If .InvokeKind = 0 Then
                                    m_oiItems(l_lngItemCount - 1).CallTypes = VbGet Or VbLet Or VbSet
                                Else
                                    m_oiItems(l_lngItemCount - 1).CallTypes = m_oiItems(l_lngItemCount - 1).CallTypes Or .InvokeKind
                                End If
                            End If
                        End If
                    Case Else
                    End Select
                End If
            End With
        Next l_lngItems
    End With
    If TypeOf m_objObject Is IInspectableCollection Then
        Set l_colObject = m_objObject
        l_lngOffset = UBound(m_oiItems)
        ReDim Preserve m_oiItems(0 To UBound(m_oiItems) + l_colObject.ItemCount)
        For l_lngIndex = 1 To l_colObject.ItemCount
            m_oiItems(l_lngIndex + l_lngOffset).Name = "Item " & l_lngIndex
            m_oiItems(l_lngIndex + l_lngOffset).SpecialType = OIT_CollectionItem
            m_oiItems(l_lngIndex + l_lngOffset).CanInspect = True
            m_oiItems(l_lngIndex + l_lngOffset).ShowElipsis = True
            m_oiItems(l_lngIndex + l_lngOffset).CallTypes = VbGet
        Next l_lngIndex
    End If
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        With m_oiItems(l_lngItems)
            If Len(m_strFilter) > 0 Then
                .Filtered = Not (InStr(1, .Name, m_strFilter, vbTextCompare) > 0)
            Else
                .Filtered = False
            End If
        End With
    Next l_lngItems
    CalculateSize
    RefreshValues
    SortItems False
    picItems_Resize
    vsScroll.Value = 0
    SelectItemByName l_strSelectedItem, False
    Redraw
    RefreshEditBox
End Sub

Private Sub cmdDropDown_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngSelection As Long
Dim l_varItems As Variant
    ReleaseCapture
    l_varItems = MenusFromStringArray(m_oiItems(m_lngSelectedItem).DropdownText)
    l_lngSelection = QuickShowMenu(picItems, cmdDropDown.Left * Screen.TwipsPerPixelX, (cmdDropDown.Top + cmdDropDown.Height) * Screen.TwipsPerPixelY, l_varItems, LeftButtonOnly:=True)
    txtEdit.Text = CStr(m_oiItems(m_lngSelectedItem).DropdownValues(l_lngSelection - 1))
    EditBoxChanged
End Sub

Public Sub SetCurrentPropertyValue_(ByVal NewValue As String)
On Error Resume Next
    txtEdit.Text = NewValue
    EditBoxChanged
End Sub

Public Sub CopyImageToGameFolder_(ByVal Filename As String)
On Error Resume Next
    FileCopy Filename, Editor.GamePath & "\" & GetTitle(Filename)
    txtEdit.Text = GetTitle(Filename)
    Editor.RefreshFileSidebar
    EditBoxChanged
End Sub

Private Sub cmdElipsis_Click()
On Error Resume Next
Dim l_objValue As Object
Dim l_lngValue As Long
Dim l_strValue As String
Dim l_booOldLocked As Boolean
    Err.Clear
    l_booOldLocked = txtEdit.Locked
    txtEdit.Locked = True
    Set l_objValue = Nothing
    Set l_objValue = m_oiItems(m_lngSelectedItem).Value
    If l_objValue Is Nothing Then
        If m_oiItems(m_lngSelectedItem).SpecialType = OIT_Color Then
            l_lngValue = SelectColor(m_oiItems(m_lngSelectedItem).Value)
            txtEdit.Text = Hex(l_lngValue)
            EditBoxChanged
        ElseIf m_oiItems(m_lngSelectedItem).SpecialType = OIT_WindowsFilename Then
            l_strValue = SelectFiles(, "Select File...", False)
            If Trim(l_strValue) <> "" Then
                txtEdit.Text = l_strValue
                EditBoxChanged
            End If
        ElseIf m_oiItems(m_lngSelectedItem).SpecialType = OIT_Filename Then
            l_strValue = SelectFiles(, "Select File...", False)
            If Trim(l_strValue) <> "" Then
                If InStr(l_strValue, DefaultEngine.FileSystem.Root) Then
                    l_strValue = Replace(l_strValue, DefaultEngine.FileSystem.Root, "/")
                    l_strValue = Replace(l_strValue, "\", "/")
                    txtEdit.Text = l_strValue
                    EditBoxChanged
                Else
                    Editor.ShowNotice "Question", "The file you have selected is not inside your game folder. The engine will not be able to load it.", Editor.NoticeIcon("question"), Array(Array("Cancel"), Array("Ignore", BindEvent(Me, "SetCurrentPropertyValue_", Array(l_strValue))), Array("Copy To Game Folder", BindEvent(Me, "CopyImageToGameFolder_", Array(l_strValue))))
                End If
            End If
        ElseIf m_oiItems(m_lngSelectedItem).SpecialType = OIT_ImageFilename Then
            l_strValue = SelectFiles("Images|" + libGraphics.SupportedGraphicsFormats, "Select Image...", False)
            If Trim(l_strValue) <> "" Then
                If InStr(l_strValue, DefaultEngine.FileSystem.Root) Then
                    l_strValue = Replace(l_strValue, DefaultEngine.FileSystem.Root, "/")
                    l_strValue = Replace(l_strValue, "\", "/")
                    txtEdit.Text = l_strValue
                    EditBoxChanged
                Else
                    Editor.ShowNotice "Question", "The file you have selected is not inside your game folder. The engine will not be able to load it.", Editor.NoticeIcon("question"), Array(Array("Cancel"), Array("Ignore", BindEvent(Me, "SetCurrentPropertyValue_", Array(l_strValue))), Array("Copy To Game Folder", BindEvent(Me, "CopyImageToGameFolder_", Array(l_strValue))))
                End If
            End If
        Else
            RaiseEvent EllipsisPressed(m_lngSelectedItem)
        End If
    Else
        If (TypeOf l_objValue Is IInspectable) Or (TypeOf l_objValue Is IInspectableCollection) Or InspectAny Then
            Inspect l_objValue, m_oiItems(m_lngSelectedItem).Name, False
        Else
            RaiseEvent EllipsisPressed(m_lngSelectedItem)
        End If
    End If
    txtEdit.Locked = l_booOldLocked
End Sub

Private Sub picHierarchy_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngNames As Long
Dim l_strName As String
Dim l_lngWidth As Long
Dim l_lngX As Long
Dim l_lngSpace As Long
    If m_objObject Is Nothing Then Exit Sub
    If m_colNameStack.Count < 2 Then Exit Sub
    picHierarchy.Font.Underline = False
    picHierarchy.Font.Bold = False
    l_lngSpace = picHierarchy.TextWidth(" » ")
    For l_lngNames = 1 To m_colNameStack.Count
        picHierarchy.Font.Bold = (m_colObjectStack(l_lngNames) Is m_objObject)
        l_lngWidth = l_lngWidth + picHierarchy.TextWidth(m_colNameStack(l_lngNames)) + IIf(l_lngNames < m_colNameStack.Count, l_lngSpace, 0)
    Next l_lngNames
    l_lngX = -(l_lngWidth - (picHierarchy.ScaleWidth - 3))
    l_lngWidth = l_lngX
    For l_lngNames = 1 To m_colNameStack.Count
        l_lngX = l_lngWidth
        picHierarchy.Font.Bold = (m_colObjectStack(l_lngNames) Is m_objObject)
        l_lngWidth = l_lngX + picHierarchy.TextWidth(m_colNameStack(l_lngNames))
        If (X >= l_lngX) And (X < l_lngWidth) Then
            If ((TypeOf m_colObjectStack(l_lngNames) Is IInspectable) Or (TypeOf m_colObjectStack(l_lngNames) Is IInspectableCollection) Or (InspectAny)) And (Not (m_colObjectStack(l_lngNames) Is Nothing)) Then
                Inspect m_colObjectStack(l_lngNames), m_colNameStack(l_lngNames), False
                Exit Sub
            End If
        End If
        l_lngWidth = l_lngWidth + IIf(l_lngNames < m_colNameStack.Count, l_lngSpace, 0)
    Next l_lngNames
End Sub

Private Sub picHierarchy_Paint()
On Error Resume Next
Dim l_lngNames As Long
Dim l_strName As String
Dim l_lngWidth As Long
Dim l_lngX As Long
Dim l_lngSpace As Long
    picHierarchy.Line (1, 1)-(picHierarchy.ScaleWidth - 2, picHierarchy.ScaleHeight - 2), vbButtonFace, BF
    If Not m_booVisible Then Exit Sub
    If m_objObject Is Nothing Then Exit Sub
    picHierarchy.Font.Underline = False
    picHierarchy.Font.Bold = False
    l_lngSpace = picHierarchy.TextWidth(" » ")
    For l_lngNames = 1 To m_colNameStack.Count
        picHierarchy.Font.Bold = (m_colObjectStack(l_lngNames) Is m_objObject)
        l_lngWidth = l_lngWidth + picHierarchy.TextWidth(m_colNameStack(l_lngNames)) + IIf(l_lngNames < m_colNameStack.Count, l_lngSpace, 0)
    Next l_lngNames
    l_lngX = -(l_lngWidth - (picHierarchy.ScaleWidth - 3))
    picHierarchy.CurrentX = l_lngX
    picHierarchy.CurrentY = 1
    For l_lngNames = 1 To m_colNameStack.Count
        picHierarchy.Font.Bold = (m_colObjectStack(l_lngNames) Is m_objObject)
        picHierarchy.Font.Underline = (TypeOf m_colObjectStack(l_lngNames) Is IInspectable) Or (InspectAny) Or (TypeOf m_colObjectStack(l_lngNames) Is IInspectableCollection)
        picHierarchy.Print m_colNameStack(l_lngNames);
        picHierarchy.Font.Underline = False
        picHierarchy.Font.Bold = False
        If l_lngNames < m_colNameStack.Count Then
            picHierarchy.Print " » ";
        End If
    Next l_lngNames
    picHierarchy.Line (0, 0)-(picHierarchy.ScaleWidth - 1, picHierarchy.ScaleHeight - 1), vbButtonShadow, B
    picHierarchy.Line (txtFilter.Left + txtFilter.Width, 0)-(picHierarchy.ScaleWidth - 1, picHierarchy.ScaleHeight - 1), vbButtonShadow, B
End Sub

Private Sub picHierarchy_Resize()
On Error Resume Next
    txtFilter.Move 1, 1, ClipValue(picHierarchy.Width * 0.4, 10, 150), picHierarchy.Height - 2
    picHierarchy_Paint
End Sub

Private Sub picInfo_Paint()
On Error Resume Next
Dim l_rcRect As Win32.Rect
Dim l_lngLength As Long
    picInfo.Line (0, 0)-(picInfo.ScaleWidth - 1, picInfo.ScaleHeight - 1), vbButtonShadow, B
    picInfo.Line (1, 1)-(picInfo.ScaleWidth - 2, picInfo.ScaleHeight - 2), vbButtonFace, BF
    If Not m_booVisible Then Exit Sub
    If m_objObject Is Nothing Then Exit Sub
    If m_lngSelectedItem < LBound(m_oiItems) Then Exit Sub
    If m_lngSelectedItem > UBound(m_oiItems) Then Exit Sub
    picInfo.CurrentX = 2
    picInfo.CurrentY = 2
    picInfo.Font.Bold = True
    l_lngLength = Len(m_oiItems(m_lngSelectedItem).Name)
    If l_lngLength < 1 Then Exit Sub
    picInfo.Print m_oiItems(m_lngSelectedItem).Name
    l_lngLength = 0
    l_lngLength = Len(m_oiItems(m_lngSelectedItem).Description)
    If l_lngLength < 1 Then Exit Sub
    With l_rcRect
        .Left = 2
        .Top = 2 + picInfo.TextHeight(m_oiItems(m_lngSelectedItem).Name)
        .Bottom = picInfo.ScaleHeight - 2
        .Right = picInfo.ScaleWidth - 2
    End With
    picInfo.Font.Bold = False
    DrawText picInfo.hdc, m_oiItems(m_lngSelectedItem).Description, l_lngLength, l_rcRect, DT_NOPREFIX Or DT_WORDBREAK
End Sub

Private Sub picInfo_Resize()
On Error Resume Next
    picInfo_Paint
End Sub

Private Sub picItems_DblClick()
On Error Resume Next
    If (m_lngSelectedItem >= 0) And (m_lngSelectedItem <= UBound(m_oiItems)) Then
        If cmdElipsis.Visible Then
            cmdElipsis_Click
        ElseIf cmdDropDown.Visible Then
            txtEdit.Text = CStr(Not (CBool(txtEdit.Text)))
            EditBoxChanged
        Else
            RaiseEvent ItemTitleDoubleClick(m_lngSelectedItem)
        End If
    End If
End Sub

Private Sub picItems_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngItems As Long
Dim l_lngY As Long
Dim l_lngIndex As Long
    EditBoxChanged
    l_lngIndex = -1
    l_lngY = picHierarchy.Height - vsScroll.Value
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        If Not (m_oiItems(l_lngItems).Filtered) Then
            If (m_oiItems(l_lngItems).CallTypes And VbGet) = VbGet Then
                If (Y >= l_lngY) And (Y < (l_lngY + m_lngItemHeight)) Then
                    l_lngIndex = l_lngItems
                    Exit For
                End If
                l_lngY = l_lngY + m_lngItemHeight
            End If
        End If
    Next l_lngItems
    If (l_lngIndex >= 0) And (l_lngIndex <= UBound(m_oiItems)) Then
        m_lngSelectedItem = l_lngIndex
        picItems_Paint
        picInfo_Paint
        RefreshEditBox
        RaiseEvent ItemSelected(l_lngIndex)
        If m_lngSelectedItem = l_lngIndex Then
            If Button = 2 Then
'                Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
'                    Menus(MenuString("Cu&t", , , "CUT"), MenuString("&Copy", , , "COPY"), MenuString("&Paste", , , "PASTE")), _
'                    frmIcons.ilContextMenus)
'                Case 1
'                    Clipboard.Clear
'                    Select Case VarType(m_oiItems.Value)
'                    Case vbString, vbLong, vbDouble, vbSingle, vbInteger, vbByte, vbCurrency
'                        Clipboard.SetText m_oiItems.ValueText
'                    Case Else
'                    End Select
'                Case 2
'                    Clipboard.Clear
'                    Select Case VarType(m_oiItems.Value)
'                    Case vbString, vbLong, vbDouble, vbSingle, vbInteger, vbByte, vbCurrency
'                        Clipboard.SetText m_oiItems.ValueText
'                    Case Else
'                    End Select
'                Case 3
'                Case Else
'                End Select
            End If
        End If
    End If
End Sub

Private Sub picItems_Resize()
On Error Resume Next
Dim l_lngMax As Long, l_lngValue As Long
    picHierarchy.Move 0, 0, picItems.ScaleWidth, IIf(m_booShowHierarchy, picHierarchy.TextHeight("AaBbYyZz") + 4, 0)
    vsScroll.Move picItems.ScaleWidth - vsScroll.Width, picHierarchy.Height, vsScroll.Width, picItems.ScaleHeight - picHierarchy.Height
    l_lngMax = m_lngTotalHeight - (picItems.ScaleHeight - picHierarchy.Height)
    If l_lngMax > 0 Then
        l_lngValue = vsScroll.Value
        If Not vsScroll.Enabled Then vsScroll.Enabled = True
        vsScroll.LargeChange = m_lngItemHeight * 4
        vsScroll.SmallChange = m_lngItemHeight
        vsScroll.Max = l_lngMax
        l_lngValue = ClipValue(l_lngValue, 0, l_lngMax)
        If vsScroll.Value <> l_lngValue Then vsScroll.Value = l_lngValue
    Else
        If vsScroll.Enabled Then vsScroll.Enabled = False
        If vsScroll.Value <> 0 Then vsScroll.Value = 0
    End If
    RefreshEditBox
    picItems_Paint
End Sub

Private Sub picSplit_Resize()
On Error Resume Next
    If m_booShowInfobox Then
        m_splSplitter.Resize
    Else
        picItems.Move 0, 0, picSplit.ScaleWidth, picSplit.ScaleHeight
    End If
    picInfo.Visible = m_booShowInfobox
End Sub

Private Sub picSplit_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If m_booShowInfobox Then m_splSplitter.MouseDown Button, Shift, X, Y
End Sub

Private Sub picSplit_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If m_booShowInfobox Then m_splSplitter.MouseMove Button, Shift, X, Y
End Sub

Private Sub picSplit_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If m_booShowInfobox Then m_splSplitter.MouseUp Button, Shift, X, Y
End Sub

Private Sub DrawColorBuffer(ByVal Color As Long)
On Error Resume Next
    m_imgColorBuffer.TileBlit m_imgColorBuffer.Rectangle, m_imgColorBufferBG, 1, BlitMode_Normal
    m_imgColorBuffer.Fill m_imgColorBuffer.Rectangle, Color, RenderMode_SourceAlpha
    m_imgColorBuffer.Box m_imgColorBuffer.Rectangle, F2RGB(0, 0, 0, 220), RenderMode_SourceAlpha
End Sub

Private Sub picItems_Paint()
On Error Resume Next
Dim l_lngItems As Long
Dim l_lngY As Long
Dim l_lngNameWidth As Long
    If Not m_booVisible Then Exit Sub
    If m_imgColorBuffer Is Nothing Then
        Set m_imgColorBuffer = F2Image(m_lngItemHeight - 3, m_lngItemHeight - 3)
    Else
        If m_imgColorBuffer.Width <> (m_lngItemHeight - 3) Or m_imgColorBuffer.Height <> (m_lngItemHeight - 3) Then
            m_imgColorBuffer.Resize m_lngItemHeight - 3, m_lngItemHeight - 3
        End If
    End If
    If m_imgColorBufferBG Is Nothing Then
        Set m_imgColorBufferBG = F2Image(2, 2)
        m_imgColorBufferBG.SetPixel 0, 0, F2RGB(220, 220, 220, 255)
        m_imgColorBufferBG.SetPixel 1, 1, F2RGB(220, 220, 220, 255)
        m_imgColorBufferBG.SetPixel 1, 0, F2RGB(35, 35, 35, 255)
        m_imgColorBufferBG.SetPixel 0, 1, F2RGB(35, 35, 35, 255)
        Set m_imgColorBufferBG = m_imgColorBufferBG.Resample(8, 8, ResampleMode_Linear)
    End If
    If m_objObject Is Nothing Then
        picItems.Cls
        Exit Sub
    End If
    l_lngY = picHierarchy.Height - vsScroll.Value
    l_lngNameWidth = m_lngNameWidth
    If (l_lngNameWidth > ((picItems.ScaleWidth - vsScroll.Width - MinimumEditWidth))) Then
        l_lngNameWidth = ((picItems.ScaleWidth - vsScroll.Width - MinimumEditWidth))
    End If
    For l_lngItems = LBound(m_oiItems) To UBound(m_oiItems)
        If Not (m_oiItems(l_lngItems).Filtered) Then
            If (m_oiItems(l_lngItems).CallTypes And VbGet) = VbGet Then
                If (l_lngY + m_lngItemHeight) > 0 Then
                    If (l_lngY < picItems.ScaleHeight) Then
                        If (m_lngSelectedItem = l_lngItems) Then
                            picItems.Line (0, l_lngY)-(l_lngNameWidth + 4, l_lngY + m_lngItemHeight), vbButtonShadow, B
                            picItems.Line (1, l_lngY + 1)-(l_lngNameWidth + 3, l_lngY + m_lngItemHeight - 1), vbHighlight, BF
                        Else
                            picItems.Line (0, l_lngY)-(l_lngNameWidth + 4, l_lngY + m_lngItemHeight), vbButtonShadow, B
                            picItems.Line (1, l_lngY + 1)-(l_lngNameWidth + 3, l_lngY + m_lngItemHeight - 1), vbButtonFace, BF
                        End If
                        picItems.CurrentX = 2
                        picItems.CurrentY = l_lngY + 2
                        If (m_lngSelectedItem = l_lngItems) Then
                            picItems.ForeColor = vbHighlightText
                        Else
                            picItems.ForeColor = vbButtonText
                        End If
                        picItems.Print Replace(m_oiItems(l_lngItems).Name, vbCrLf, "<cr>")
                    
                        picItems.Line (l_lngNameWidth + 4, l_lngY)-(picItems.ScaleWidth - 1 - vsScroll.Width, l_lngY + m_lngItemHeight), vbButtonShadow, B
                        picItems.Line (l_lngNameWidth + 5, l_lngY + 1)-(picItems.ScaleWidth - 2 - vsScroll.Width, l_lngY + m_lngItemHeight - 1), vbWindowBackground, BF
                    
                        Select Case m_oiItems(l_lngItems).SpecialType
                        Case OIT_Color
                            picItems.CurrentX = l_lngNameWidth + 8 + m_imgColorBuffer.Width
                            picItems.CurrentY = l_lngY + 2
                            DrawColorBuffer CLng(m_oiItems(l_lngItems).Value)
                            CopyImageToDC picItems.hdc, F2Rect(l_lngNameWidth + 6, l_lngY + 2, m_imgColorBuffer.Width, m_imgColorBuffer.Height, False), m_imgColorBuffer
                        Case Else
                            picItems.CurrentX = l_lngNameWidth + 6
                            picItems.CurrentY = l_lngY + 2
                        End Select
                        
                        If (m_oiItems(l_lngItems).ValueText = "{Error}") Then
                            picItems.ForeColor = vbButtonShadow
                        ElseIf (m_oiItems(l_lngItems).ValueText = "{Multiple Values}") Then
                            picItems.ForeColor = vbButtonShadow
                        ElseIf (m_oiItems(l_lngItems).CallTypes And VbLet) = VbLet Then
                            picItems.ForeColor = vbWindowText
                        ElseIf (TypeOf m_oiItems(l_lngItems).Value Is IInspectorType) Then
                            picItems.ForeColor = vbWindowText
                        Else
                            picItems.ForeColor = vbButtonShadow
                        End If
                        
                        If (m_lngSelectedItem = l_lngItems) Then
                            If (txtEdit.Visible = True) Then
                            Else
                                picItems.Print Replace(m_oiItems(l_lngItems).ValueText, vbCrLf, "<cr>")
                            End If
                        Else
                            picItems.Print Replace(m_oiItems(l_lngItems).ValueText, vbCrLf, "<cr>")
                        End If
                    End If
                End If
                
                l_lngY = l_lngY + m_lngItemHeight
            End If
        End If
    Next l_lngItems
    picItems.Line (0, l_lngY + 1)-(picItems.ScaleWidth - vsScroll.Width, picItems.ScaleHeight), vbButtonFace, BF
End Sub

Private Sub txtEdit_Change()
On Error Resume Next
End Sub

Private Sub txtEdit_GotFocus()
On Error Resume Next
    m_booEditBoxActive = True
End Sub

Private Sub txtEdit_KeyPress(KeyAscii As Integer)
On Error Resume Next
    If KeyAscii = 13 Then
        KeyAscii = 0
        EditBoxChanged
    End If
End Sub

Private Sub txtEdit_LostFocus()
On Error Resume Next
    If txtEdit.Locked Then Exit Sub
    m_booEditBoxActive = False
    EditBoxChanged
End Sub

Private Sub EditBoxChanged()
On Error Resume Next
Dim l_objObject As Object, l_lngObject As Long
Dim l_booCancel As Boolean
Dim l_varOldValue As Variant
Dim l_sngValue As Single, l_dblValue As Double
Dim l_intValue As Integer, l_lngValue As Long
Dim l_bytValue As Byte, l_strValue As String
Dim l_varValue As Variant
Dim l_booValue As Boolean
Dim l_itValue As IInspectorType
Dim l_vtType As VbVarType
Dim l_strText As String
Dim l_lngValues As Long
Dim l_booError As Boolean
    l_strText = txtEdit.Text
    If l_strText = "{Multiple Values}" Then Exit Sub
    If l_strText = m_oiItems(m_lngSelectedItem).ValueText Then Exit Sub
    Select Case m_oiItems(m_lngSelectedItem).SpecialType
    Case OIT_Color, OIT_Hex
        l_strText = "&H" & l_strText
    Case OIT_Enum
        Err.Clear
        l_booError = UBound(m_oiItems(m_lngSelectedItem).DropdownText) < LBound(m_oiItems(m_lngSelectedItem).DropdownText)
        If Err <> 0 Then l_booError = True
        If Not l_booError Then
            For l_lngValues = LBound(m_oiItems(m_lngSelectedItem).DropdownText) To UBound(m_oiItems(m_lngSelectedItem).DropdownText)
                If Trim(LCase(l_strText)) = Trim(LCase(m_oiItems(m_lngSelectedItem).DropdownText(l_lngValues))) Then
                    l_strText = CStr(m_oiItems(m_lngSelectedItem).DropdownValues(l_lngValues))
                End If
            Next l_lngValues
        End If
    Case Else
    End Select
    RaiseEvent BeforeItemChange(l_booCancel)
    If l_booCancel Then Exit Sub
    If TypeOf m_oiItems(m_lngSelectedItem).Value Is IInspectorType Then
        Set l_itValue = m_oiItems(m_lngSelectedItem).Value
        l_itValue.FromString l_strText
        RefreshValues
        RefreshEditBox
        RaiseEvent AfterItemChange(l_varOldValue, l_itValue)
        Exit Sub
    End If
    l_varOldValue = m_oiItems(m_lngSelectedItem).Value
    l_vtType = VarType(l_varOldValue)
    Err.Clear
    If m_booMultiple Then
        Select Case l_vtType
        Case vbBoolean
            l_booValue = CBool(l_strText)
            l_varValue = l_booValue
        Case vbSingle
            l_sngValue = CSng(l_strText)
            l_varValue = l_sngValue
        Case vbDouble
            l_dblValue = CDbl(l_strText)
            l_varValue = l_dblValue
        Case vbLong
            l_lngValue = CLng(l_strText)
            l_varValue = l_lngValue
        Case vbInteger
            l_intValue = CInt(l_strText)
            l_varValue = l_intValue
        Case vbByte
            l_bytValue = CByte(l_strText)
            l_varValue = l_bytValue
        Case vbString
            l_strValue = CStr(l_strText)
            l_varValue = l_strValue
        Case Else
        End Select
        For l_lngObject = LBound(m_objObjects) To UBound(m_objObjects)
            Set l_objObject = m_objObjects(l_lngObject)
            Select Case l_vtType
            Case vbBoolean
                CallByName l_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_booValue
            Case vbSingle
                CallByName l_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_sngValue
            Case vbDouble
                CallByName l_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_dblValue
            Case vbLong
                CallByName l_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_lngValue
            Case vbInteger
                CallByName l_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_intValue
            Case vbByte
                CallByName l_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_bytValue
            Case vbString
                CallByName l_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_strValue
            Case Else
            End Select
        Next l_lngObject
    Else
        Select Case l_vtType
        Case vbBoolean
            l_booValue = CBool(l_strText)
            CallByName m_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_booValue
            RaiseEvent AfterItemChange(l_varOldValue, l_booValue)
        Case vbSingle
            l_sngValue = CSng(l_strText)
            CallByName m_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_sngValue
            RaiseEvent AfterItemChange(l_varOldValue, l_sngValue)
        Case vbDouble
            l_dblValue = CDbl(l_strText)
            CallByName m_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_dblValue
            RaiseEvent AfterItemChange(l_varOldValue, l_dblValue)
        Case vbLong
            l_lngValue = CLng(l_strText)
            CallByName m_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_lngValue
            RaiseEvent AfterItemChange(l_varOldValue, l_lngValue)
        Case vbInteger
            l_intValue = CInt(l_strText)
            CallByName m_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_intValue
            RaiseEvent AfterItemChange(l_varOldValue, l_intValue)
        Case vbByte
            l_bytValue = CByte(l_strText)
            CallByName m_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_bytValue
            RaiseEvent AfterItemChange(l_varOldValue, l_bytValue)
        Case vbString
            l_strValue = CStr(l_strText)
            CallByName m_objObject, m_oiItems(m_lngSelectedItem).Name, VbLet, l_strValue
            RaiseEvent AfterItemChange(l_varOldValue, l_strValue)
        Case Else
        End Select
    End If
    RefreshValues
    RefreshEditBox
End Sub

Private Sub txtEdit_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_booEditBoxActive = True
End Sub

Private Sub txtFilter_Change()
On Error Resume Next
    m_strFilter = txtFilter.Text
    RefreshFilter
End Sub

Private Sub txtFilter_GotFocus()
On Error Resume Next
    m_booFilterActive = True
End Sub

Private Sub txtFilter_LostFocus()
On Error Resume Next
    m_booFilterActive = False
End Sub

Private Sub txtFilter_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_booFilterActive = True
End Sub

Private Sub UserControl_EnterFocus()
On Error Resume Next
    If Not m_booHaveFocus Then
        m_booHaveFocus = True
        RefreshValues
        RefreshEditBox
    End If
End Sub

Private Sub UserControl_Hide()
On Error Resume Next
    EditBoxChanged
    m_booHaveFocus = False
    m_booVisible = False
    ClearStack
    Set m_objObject = Nothing
    Set m_imgColorBuffer = Nothing
    Erase m_oiItems
End Sub

Private Sub UserControl_Initialize()
    m_booShowInfobox = True
    m_booShowHierarchy = True
End Sub

Private Sub UserControl_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
Dim l_lngIndex As Long
    Select Case KeyCode
    Case vbKeyReturn
        KeyCode = 0
        EditBoxChanged
    Case vbKeyDown
        EditBoxChanged
        l_lngIndex = m_lngSelectedItem + 1
        KeyCode = 0
    Case vbKeyUp
        EditBoxChanged
        l_lngIndex = m_lngSelectedItem - 1
        KeyCode = 0
    Case Else
        Exit Sub
    End Select
    If (l_lngIndex >= 0) And (l_lngIndex <= UBound(m_oiItems)) Then
        m_lngSelectedItem = l_lngIndex
        picItems_Paint
        picInfo_Paint
        RefreshEditBox
        RaiseEvent ItemSelected(m_lngSelectedItem)
    End If
End Sub

Private Sub UserControl_ExitFocus()
On Error Resume Next
    If m_booHaveFocus Then
        m_booHaveFocus = False
    End If
End Sub

Private Sub UserControl_LostFocus()
On Error Resume Next
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
    picSplit.Move 0, 0, UserControl.ScaleWidth, UserControl.ScaleHeight
End Sub

Private Sub UserControl_Show()
On Error Resume Next
    m_booVisible = True
    Set m_imgColorBuffer = F2Image(1, 1)
    vsScroll.Width = GetScrollbarSize(vsScroll) + 1
    With m_splSplitter
        .Orientation = cSPLTOrientationHorizontal
        .Bind picItems, picInfo
        .Orientation = cSPLTOrientationHorizontal
        .KeepProportion = True
        .MinimumSize(cSPLTRightOrBottomPanel) = 25
        .MaximumSize(cSPLTRightOrBottomPanel) = 80
    End With
    picInfo.Height = 25
    UserControl_Resize
    picSplit_Resize
    picItems_Resize
    m_splSplitter.Resize
    RefreshEditBox
    picItems_Paint
End Sub

Private Sub UserControl_Terminate()
On Error Resume Next
    m_booHaveFocus = False
    m_booVisible = False
    ClearStack
    Set m_objObject = Nothing
    Erase m_oiItems
End Sub

Private Sub vsScroll_Change()
On Error Resume Next
    txtEdit.Top = m_lngEditY - vsScroll.Value + 2 + picHierarchy.Height
    cmdDropDown.Top = m_lngEditY - vsScroll.Value + 1 + picHierarchy.Height
    cmdElipsis.Top = m_lngEditY - vsScroll.Value + 1 + picHierarchy.Height
    picItems_Paint
End Sub


