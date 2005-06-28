VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Begin VB.Form frmUserData 
   BorderStyle     =   0  'None
   ClientHeight    =   7335
   ClientLeft      =   0
   ClientTop       =   15
   ClientWidth     =   9195
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmUserData.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   489
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   613
   ShowInTaskbar   =   0   'False
   Begin VB.Timer tmrRedrawUI 
      Enabled         =   0   'False
      Interval        =   33
      Left            =   3990
      Top             =   3420
   End
   Begin ngPlugins.ObjectInspector insProperties 
      Height          =   3600
      Left            =   2190
      TabIndex        =   2
      Top             =   1860
      Width           =   4800
      _ExtentX        =   8467
      _ExtentY        =   6350
   End
   Begin VB.PictureBox picUI 
      AutoRedraw      =   -1  'True
      Height          =   1485
      Left            =   0
      ScaleHeight     =   95
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   143
      TabIndex        =   1
      Top             =   0
      Visible         =   0   'False
      Width           =   2205
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   -15
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin vbalDTab6.vbalDTabControl dtViews 
      Height          =   7290
      Left            =   1725
      TabIndex        =   0
      Top             =   540
      Width           =   9150
      _ExtentX        =   16140
      _ExtentY        =   12859
      AllowScroll     =   0   'False
      TabAlign        =   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty SelectedFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShowCloseButton =   0   'False
      MoveableTabs    =   0   'False
   End
End
Attribute VB_Name = "frmUserData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
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
Private Declare Function LBItemFromPt Lib "comctl32.dll" _
  (ByVal hwnd As Long, ByVal ptx As Long, ByVal pty As Long, ByVal bAutoScroll As Long) As Long

Implements iExtendedForm
Implements iEditingCommands
Implements iCustomMenus
Implements iDocument

Private Enum UserDataEditorViews
    View_Properties
    View_Custom
End Enum

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private m_imgCustomDisplay As Fury2Image

Private m_colUndo As New Engine.Fury2Collection
Private m_colRedo As New Engine.Fury2Collection

Private m_objUI As Object
Private m_cntUI As Fury2UserdataUI
Private m_objData As Object
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin
Private m_lngOldBitmap

Private m_booVisible As Boolean

Private WithEvents m_tbrToolbar As ngToolbar
Attribute m_tbrToolbar.VB_VarHelpID = -1
Private m_lngCurrentView As UserDataEditorViews

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iDocument_DocumentIcon = Editor.LoadResources("ng").ItemData("icons\userdata.png")
End Property

Public Property Set Data(ByRef NewData As Object)
On Error Resume Next
    Set m_objData = NewData
    Set m_objUI = m_objData.CreateEditorUI()
    If m_objUI Is Nothing Then
        Set m_objUI = Engine.ScriptEngine.Namespace.CreateEditorUI(m_objData)
    End If
    Set m_cntUI = m_objUI.This
    Me.Caption = IIf(Trim(m_strFilename) = "", "Untitled.f2data (Object)", GetTitle(m_strFilename) & " (" & TypeName(m_objData) & ")")
    Err.Clear
    Redraw
End Property

Public Property Get Data() As Object
On Error Resume Next
    Set Data = m_objData
End Property

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Public Property Get ActiveType() As String
On Error Resume Next
    Select Case LCase(Trim(Me.ActiveControl.Name))
    Case Else
    End Select
End Property

Public Sub LockRedraw(Window As Long, State As Boolean)
On Error Resume Next
    SendMessage Window, WM_SETREDRAW, Abs(CLng(State)), 0
End Sub

Public Sub AllocateBuffers()
On Error Resume Next
    picUI.AutoRedraw = True
    m_lngOldBitmap = GetCurrentObject(picUI.hdc, Object_Bitmap)
    Set m_imgCustomDisplay = F2DIBSection(1, 1, picUI.hdc)
    SelectObject picUI.hdc, m_imgCustomDisplay.DIBHandle
End Sub

Public Sub DeallocateBuffers()
On Error Resume Next
    SelectObject picUI.hdc, m_lngOldBitmap
    Set m_imgCustomDisplay = Nothing
    m_lngOldBitmap = 0
    picUI.AutoRedraw = False
End Sub

Public Sub Redraw()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Properties
        insProperties.InspectAny = True
        insProperties.Inspect m_objData, "Data", True, False, True
    Case View_Custom
        m_objUI.Refresh
        RedrawUI
    Case Else
    End Select
End Sub

Private Sub FixRectCoords(ByRef X1 As Long, ByRef Y1 As Long, ByRef X2 As Long, ByRef Y2 As Long)
On Error Resume Next
Dim Temp As Long
    If X1 > X2 Then
        Temp = X1
        X1 = X2
        X2 = Temp
    End If
    If Y1 > Y2 Then
        Temp = Y1
        Y1 = Y2
        Y2 = Temp
    End If
End Sub

Private Function SmartApply(Obj As iUndoEntry) As Boolean
On Error Resume Next
Dim l_ouUndo As cObjectUndoEntry
Dim l_prUndo As cPropertyUndoEntry
    SmartApply = Obj.Apply()
    If TypeOf Obj Is cTileUndoEntry Then
    ElseIf TypeOf Obj Is cObjectUndoEntry Then
    ElseIf TypeOf Obj Is cPropertyUndoEntry Then
    ElseIf TypeOf Obj Is cMultiUndoEntry Then
    End If
End Function

Public Function Redo() As Boolean
On Error Resume Next
Dim l_undRedo As iUndoEntry
Dim l_undUndo As iUndoEntry
    If m_colRedo.Count > 0 Then
        BeginProcess "Preparing to Redo..."
        Set l_undRedo = m_colRedo.Item(1)
        Set l_undUndo = l_undRedo.CreateReverse()
        m_colRedo.Remove 1
        m_colUndo.Add l_undUndo
        UpdateProcess 0, "Performing Redo..."
        Redo = SmartApply(l_undRedo)
        EndProcess
        Editor.ActionUpdate
    End If
End Function

Public Function Undo() As Boolean
On Error Resume Next
Dim l_undRedo As iUndoEntry
Dim l_undUndo As iUndoEntry
    If m_colUndo.Count > 0 Then
        BeginProcess "Preparing to Undo..."
        Set l_undUndo = m_colUndo.Item(m_colUndo.Count)
        Set l_undRedo = l_undUndo.CreateReverse()
        m_colUndo.Remove m_colUndo.Count
        m_colRedo.Add l_undRedo, , 1
        UpdateProcess 0, "Performing Undo..."
        Undo = SmartApply(l_undUndo)
        EndProcess
        Editor.ActionUpdate
    End If
End Function

Public Sub MultiPropertyUndoPush(ByRef Obj As Object, ByRef Methods As Variant, ByRef Values As Variant)
On Error Resume Next
Dim l_undUndo As cMultiUndoEntry
Dim l_undProp As cPropertyUndoEntry
Dim l_lngIndex As Long
    If Obj Is Nothing Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cMultiUndoEntry
    With l_undUndo.Entries
        For l_lngIndex = LBound(Methods) To UBound(Methods)
            Set l_undProp = New cPropertyUndoEntry
            With l_undProp
                Set .Object = Obj
                .MethodName = CStr(Methods(l_lngIndex))
                If (VarType(Values(l_lngIndex)) And vbObject) = vbObject Then
                    Set .Value = Values(l_lngIndex)
                Else
                    .Value = Values(l_lngIndex)
                End If
            End With
            .Add l_undProp
        Next l_lngIndex
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub PropertyUndoPush(ByRef Obj As Object, ByRef Method As String, ByRef Value As Variant)
On Error Resume Next
Dim l_undUndo As cPropertyUndoEntry
    If Obj Is Nothing Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cPropertyUndoEntry
    With l_undUndo
        Set .Object = Obj
        .MethodName = Method
        If (VarType(Value) And vbObject) = vbObject Then
            Set .Value = Value
        Else
            .Value = Value
        End If
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub ObjectUndoPush(ByRef Container As Object, ByRef Value As Object, ByVal Index As Long, ByVal Operation As ObjectUndoOperations)
On Error Resume Next
Dim l_undUndo As cObjectUndoEntry
    If Container Is Nothing Then Exit Sub
    If Value Is Nothing Then Exit Sub
    If Index < 1 Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cObjectUndoEntry
    With l_undUndo
        Set .Container = Container
        Set .Value = Value
        .Index = Index
        .Operation = Operation
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub Cleanup()
On Error Resume Next
    Set m_cntUI = Nothing
    Set m_objData = Nothing
    Set m_fpgPlugin = Nothing
End Sub

Public Sub InitViews()
On Error Resume Next
    dtViews.Tabs.Add "t" & CStr(View_Properties), , "Properties"
    dtViews.Tabs.Add "t" & CStr(View_Custom), , "Custom"
End Sub

Public Sub RefreshAll()
On Error Resume Next
    Redraw
    ViewChanged
End Sub

Public Sub ViewChanged()
On Error Resume Next
Dim l_objObject As Object
    Screen.MousePointer = 11
    insProperties.Visible = False
    picUI.Visible = False
    dtViews_Resize
    Select Case m_lngCurrentView
    Case View_Properties
        insProperties.Visible = True
        tmrRedrawUI.Enabled = False
    Case View_Custom
        picUI.Visible = True
        tmrRedrawUI.Enabled = True
    Case Else
    End Select
    Redraw
    Screen.MousePointer = 0
End Sub

Public Sub RedrawUI()
On Error Resume Next
    If m_imgCustomDisplay Is Nothing Then Exit Sub
    If (m_imgCustomDisplay.Width <> picUI.ScaleWidth) Or (m_imgCustomDisplay.Height <> picUI.ScaleHeight) Then
        SelectObject picUI.hdc, m_lngOldBitmap
        Set m_imgCustomDisplay = F2DIBSection(picUI.ScaleWidth, picUI.ScaleHeight, picUI.hdc)
        Set m_cntUI.This.Rectangle = m_imgCustomDisplay.Rectangle
        SelectObject picUI.hdc, m_imgCustomDisplay.DIBHandle
    End If
    With m_imgCustomDisplay
        .Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        If Not (m_cntUI Is Nothing) Then
            m_cntUI.Redraw m_imgCustomDisplay
        End If
    End With
    gdi32.Flush
    picUI.Refresh
End Sub

Private Function ClipboardContainsFormat(Format As TilesetEditorClipboardFormats) As Boolean
On Error Resume Next
Dim l_objPlugin As TilesetEditor
    Set l_objPlugin = m_fpgPlugin
    With l_objPlugin.CustomClipboard
        .GetCurrentFormats Me.hwnd
        ClipboardContainsFormat = .HasCurrentFormat(l_objPlugin.ClipboardFormat(Format))
    End With
End Function

Private Function ClipboardFormat(Format As TilesetEditorClipboardFormats) As Long
On Error Resume Next
Dim l_objPlugin As TilesetEditor
    Set l_objPlugin = m_fpgPlugin
    ClipboardFormat = l_objPlugin.ClipboardFormat(Format)
End Function

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Private Function CustomClipboard() As cCustomClipboard
On Error Resume Next
Dim l_objPlugin As TilesetEditor
    Set l_objPlugin = m_fpgPlugin
    Set CustomClipboard = l_objPlugin.CustomClipboard
End Function

Private Function Engine() As Fury2Engine
On Error Resume Next
    Set Engine = Editor.Engine
End Function

Private Function Editor() As Object
On Error Resume Next
Dim l_objPlugin As UserDataEditor
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Function

Private Sub dtViews_Resize()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Properties
        insProperties.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case View_Custom
        picUI.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case Else
    End Select
End Sub

Private Sub dtViews_TabSelected(TheTab As vbalDTab6.cTab)
On Error Resume Next
    m_lngCurrentView = CLng(Mid(TheTab.key, 2))
    ViewChanged
End Sub

Public Sub Form_Activate()
On Error Resume Next
    Set insProperties.Editor = Editor
    AllocateBuffers
    Form_Resize
    dtViews_Resize
    ViewChanged
    Redraw
End Sub

Private Sub Form_Deactivate()
On Error Resume Next
    DeallocateBuffers
End Sub

Private Sub Form_Load()
On Error Resume Next
'    vsTileset.Width = GetScrollbarSize(vsTileset)
    InitViews
    Form_Activate
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    Cleanup
End Sub

Private Sub Form_Resize()
On Error GoTo 0
    If WindowState = 2 And m_booVisible = False Then
        m_booVisible = True
        RefreshAll
    End If
    dtViews.Move 2, 2, Me.ScaleWidth - 4, Me.ScaleHeight - 4
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu
    End With
End Sub

Private Sub iCustomMenus_MenuClick(key As String)
On Error Resume Next
Dim l_strParameter As String, l_lngParameter As Long
Dim CommandName As String
    CommandName = key
    If InStr(CommandName, "(") Then
        l_strParameter = Trim(Mid(CommandName, InStr(CommandName, "(") + 1))
        If Right(l_strParameter, 1) = ")" Then l_strParameter = Trim(Left(l_strParameter, Len(l_strParameter) - 1))
        CommandName = Left(CommandName, InStr(CommandName, "(") - 1)
        CommandName = Replace(CommandName, ":", "_")
        If Left(l_strParameter, 1) = """" Then
            ' String
            l_strParameter = CStr(Mid(l_strParameter, 2, Len(l_strParameter) - 2))
            CallByName Me, CommandName, VbMethod, l_strParameter
        Else
            ' Integer
            l_lngParameter = CLng(l_strParameter)
            CallByName Me, CommandName, VbMethod, l_lngParameter
        End If
    Else
        CommandName = Replace(CommandName, ":", "_")
        CallByName Me, CommandName, VbMethod
    End If
    Err.Clear
End Sub

Private Property Get iDocument_CanSave() As Boolean
On Error Resume Next
    iDocument_CanSave = True
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = m_strFilename
End Property

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = m_fpgPlugin
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
    Set m_fpgPlugin = RHS
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
Dim l_vfFile As VirtualFile
    Err.Clear
    Set l_vfFile = F2File()
    Engine.SaveUserDataToFile l_vfFile, m_objData
    l_vfFile.SaveFile Filename
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
    Redraw
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "User Data"
End Property

Private Sub iEditingCommands_CanCopy(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanCut(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanDelete(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanPaste(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanRedo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
        NewValue = (m_colRedo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_CanSelectAll(NewValue As Boolean)
End Sub

Private Sub iEditingCommands_CanSelectNone(NewValue As Boolean)
End Sub

Private Sub iEditingCommands_CanUndo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
        NewValue = (m_colUndo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_Copy()
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Cut()
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Delete()
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Paste()
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Redo()
On Error Resume Next
'    Me.Redo
    Redraw
End Sub

Private Sub iEditingCommands_SelectAll()
End Sub

Private Sub iEditingCommands_SelectNone()
End Sub

Private Sub iEditingCommands_Undo()
On Error Resume Next
'    Me.Undo
    Redraw
End Sub

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Private Sub insProperties_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    ' lol
    ' Redraw
End Sub

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2data (Object)", GetTitle(Name) & " (" & TypeName(m_objData) & ")")
End Sub

Private Sub picUI_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    m_cntUI.KeyDown KeyCode, Shift
End Sub

Private Sub picUI_KeyPress(KeyAscii As Integer)
On Error Resume Next
    m_cntUI.KeyPress KeyAscii
End Sub

Private Sub picUI_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    m_cntUI.KeyUp KeyCode, Shift
End Sub

Private Sub picUI_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_cntUI.MouseDown Button, Shift, X, Y
End Sub

Private Sub picUI_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_cntUI.MouseMove Button, Shift, X, Y
End Sub

Private Sub picUI_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_cntUI.MouseUp Button, Shift, X, Y
End Sub

Private Sub picUI_Resize()
On Error Resume Next
    RedrawUI
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

Private Sub tmrRedrawUI_Timer()
On Error Resume Next
Static l_booHere As Boolean
    Do While l_booHere
        DoEvents
    Loop
    l_booHere = True
    RedrawUI
    l_booHere = False
End Sub
