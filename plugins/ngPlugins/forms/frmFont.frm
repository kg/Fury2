VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Begin VB.Form frmFont 
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
   Icon            =   "frmFont.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   489
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   613
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picCharacters 
      BorderStyle     =   0  'None
      Height          =   5580
      Left            =   2145
      ScaleHeight     =   372
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   393
      TabIndex        =   3
      Top             =   840
      Visible         =   0   'False
      Width           =   5895
   End
   Begin VB.PictureBox picPreview 
      Height          =   1485
      Left            =   0
      ScaleHeight     =   95
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   143
      TabIndex        =   2
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
   Begin ngPlugins.ObjectInspector insOverview 
      Height          =   3045
      Left            =   855
      TabIndex        =   0
      Top             =   990
      Visible         =   0   'False
      Width           =   1710
      _ExtentX        =   3016
      _ExtentY        =   5371
   End
   Begin vbalDTab6.vbalDTabControl dtViews 
      Height          =   7290
      Left            =   1725
      TabIndex        =   1
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
Attribute VB_Name = "frmFont"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Declare Function LBItemFromPt Lib "comctl32.dll" _
  (ByVal hwnd As Long, ByVal ptx As Long, ByVal pty As Long, ByVal bAutoScroll As Long) As Long
   
Implements iExtendedForm
Implements iEditingCommands
Implements iCustomMenus
Implements iDocument

Private Enum FontEditorViews
    View_Overview
    View_Characters
    View_Preview
End Enum

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private m_imgPreview As Fury2Image

Private m_colUndo As New Engine.Fury2Collection
Private m_colRedo As New Engine.Fury2Collection

Private m_fntFont As Fury2Font
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Private m_booVisible As Boolean

Private m_tbhHandler As iToolbarHandler
Private m_lngCurrentView As FontEditorViews

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Public Sub Recolor()
On Error Resume Next
    m_fntFont.Recolor SelectColor(F2White)
    Redraw
End Sub

Public Sub AddDropShadow()
On Error Resume Next
    m_fntFont.AddDropShadow SelectColor(F2Black), 1, 1, 1
    Redraw
End Sub

Public Sub AddOutline()
On Error Resume Next
    m_fntFont.AddOutline SelectColor(F2Black), 1
    Redraw
End Sub

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

Public Property Get Font_() As Fury2Font
    Set Font = m_fntFont
End Property

Public Sub AllocateBuffers()
On Error Resume Next
    Set m_imgPreview = F2Image(1, 1)
End Sub

Public Sub DeallocateBuffers()
On Error Resume Next
    Set m_imgPreview = Nothing
End Sub

Public Sub Redraw()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Inspect m_fntFont, "Font", True, True
    Case View_Characters
'        RedrawCharacters
    Case View_Preview
        RedrawPreview
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
    Set m_fpgPlugin = Nothing
End Sub

Public Sub InitViews()
On Error Resume Next
    dtViews.Tabs.Add "t" & CStr(View_Overview), , "Overview"
    dtViews.Tabs.Add "t" & CStr(View_Characters), , "Characters"
    dtViews.Tabs.Add "t" & CStr(View_Preview), , "Preview"
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
    insOverview.Visible = False
    picPreview.Visible = False
    dtViews_Resize
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Visible = True
    Case View_Preview
        picPreview.Visible = True
    Case Else
    End Select
    Redraw
    Screen.MousePointer = 0
End Sub

Public Sub RedrawPreview()
On Error Resume Next
    If (m_imgPreview.Width <> picPreview.ScaleWidth) Or (m_imgPreview.Height <> picPreview.ScaleHeight) Then
        m_imgPreview.Resize picPreview.ScaleWidth, picPreview.ScaleHeight
    End If
    With m_imgPreview
        .Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        m_fntFont.Draw m_imgPreview, "The quick brown fox jumped over the lazy dogs." & vbCrLf & _
            "My sphinx is made of quartz." & vbCrLf & _
            "1234567890-= !@#$%^&*()_+", m_imgPreview.Rectangle
    End With
    picPreview_Paint
End Sub

Private Function ClipboardContainsFormat(Format As FontEditorClipboardFormats) As Boolean
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    With l_objPlugin.CustomClipboard
        .GetCurrentFormats Me.hwnd
        ClipboardContainsFormat = .HasCurrentFormat(l_objPlugin.ClipboardFormat(Format))
    End With
End Function

Private Function ClipboardFormat(Format As FontEditorClipboardFormats) As Long
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    ClipboardFormat = l_objPlugin.ClipboardFormat(Format)
End Function

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Private Function CustomClipboard() As cCustomClipboard
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    Set CustomClipboard = l_objPlugin.CustomClipboard
End Function

Private Function Editor() As Object
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Function

Private Sub dtViews_Resize()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case View_Preview
        picPreview.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case Else
    End Select
End Sub

Private Sub dtViews_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    m_lngCurrentView = CLng(Mid(theTab.key, 2))
    ViewChanged
End Sub

Public Sub Form_Activate()
On Error GoTo 0
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
'    vsFont.Width = GetScrollbarSize(vsFont)
    Set m_fntFont = DefaultEngine.F2Font()
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
    With Handler
        .DestroyMenu "Tools"
        .DestroyMenu "AddDropShadow"
        .DestroyMenu "AddOutline"
        .DestroyMenu "Recolor"
        .DestroyMenu "ToolsEndSeparator"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DefineMenu "&Tools", "Tools"
        .DefineMenu "Add Drop Shadow", "AddDropShadow", "Tools"
        .DefineMenu "Add Outline", "AddOutline", "Tools"
        .DefineMenu "Recolor", "Recolor", "Tools"
        .DefineMenu "-", "ToolsEndSeparator", "Tools"
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
    Kill Filename
    Err.Clear
    Set l_vfFile = F2File()
    SaveToFile m_fntFont, l_vfFile
    l_vfFile.SaveFile Filename
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
    Redraw
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Font"
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

Private Sub insOverview_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    ' lol
    ' Redraw
End Sub

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2font", GetTitle(Name))
End Sub

Friend Sub SetFont(Font As Fury2Font)
On Error Resume Next
    Set m_fntFont = Font
End Sub

Private Sub picPreview_Paint()
On Error Resume Next
    CopyImageToDC picPreview.hdc, m_imgPreview.Rectangle, m_imgPreview
End Sub

Private Sub picPreview_Resize()
On Error Resume Next
    RedrawPreview
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

