VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cEditor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'
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
Implements Fury2Debugger
Private Const EM_CANUNDO = &HC6
Private Const EM_UNDO = &HC7

Private Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long

Public CanCut As Boolean, CanCopy As Boolean, CanPaste As Boolean, CanDelete As Boolean, CanSelectAll As Boolean, CanSelectNone As Boolean, CanUndo As Boolean, CanRedo As Boolean
Private m_strGamePath As String
Private m_docChild As cChildManager
Private m_aclPause As cAccelerator
Private m_amgAccelerators As cAcceleratorManager
Private m_dbgDebugger As frmGameDebugger
Private WithEvents m_clpClipboard As cClipboardViewer
Attribute m_clpClipboard.VB_VarHelpID = -1
Private m_resResources As ngResourceFile
Private m_optOptions As EditorOptions
Private m_sngPauseClicked As Single

Public Property Get Document() As Object
On Error Resume Next
    Set Document = frmMain.ActiveChild.Form
End Property

Public Property Get Documents() As Engine.Fury2Collection
On Error Resume Next
    Set Documents = frmMain.Documents
End Property

Public Sub RefreshFileSidebar()
On Error Resume Next
    frmMain.RefreshFileSidebar
End Sub

Public Function NoticeIcon(ByVal Name As String) As Fury2Image
On Error Resume Next
    Set NoticeIcon = m_resResources.ItemData("notice\" & Name & ".png")
End Function

Public Sub DismissNotice()
On Error Resume Next
    frmMain.DismissNotice
End Sub

Public Sub ShowNotice(Optional Title As String = "", Optional Text As String = "", Optional Icon As Fury2Image = Nothing, Optional Options As Variant = Nothing, Optional AutoClose As Boolean = True)
On Error Resume Next
    frmMain.ShowNotice Title, Text, Icon, Options, AutoClose
End Sub

Public Sub AssociateFileType(ByRef Extension As String, ByRef Name As String, ByRef Description As String, ByRef Icon As String)
On Error Resume Next
Dim l_regRegistry As cRegistry
    Set l_regRegistry = New cRegistry
    If InIDE Then
        l_regRegistry.CreateEXEAssociationEx App.Path & "\..\..\binary\sys\ngIDE.exe", "ng" & Name, Description, Extension, "&Open in ngIDE", False, , False, , False, , App.Path & "\..\..\binary\sys\icons\" & Icon, 0
    Else
        l_regRegistry.CreateEXEAssociationEx App.Path & "\ngIDE.exe", "ng" & Name, Description, Extension, "&Open in ngIDE", False, , False, , False, , App.Path & "\icons\" & Icon, 0
    End If
End Sub

Public Property Get AcceleratorManager() As cAcceleratorManager
On Error Resume Next
    Set AcceleratorManager = m_amgAccelerators
End Property

Public Sub Action_ActivateWindow(ByVal Index)
On Error Resume Next
    frmMain.ActivateChild CLng(Index)
    Err.Clear
End Sub

Public Sub Action_CloseAllWindows()
On Error Resume Next
    frmMain.CloseAllChildren False
    Err.Clear
End Sub

Public Sub Action_CloseWindow()
On Error Resume Next
Dim l_lngIndex As Long
    frmMain.ActiveChild.Hide
    Err.Clear
End Sub

Public Function Action_Copy() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    SetBusyState True
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            Clipboard.Clear
            Clipboard.SetText l_ctlControl.SelText, vbCFText
            ActionUpdate
            Action_Copy = True
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanCopy l_booResult
        Action_Copy = l_booResult
        If l_booResult Then l_clpDocument.Copy
    End If
    SetBusyState False
    Err.Clear
End Function

Public Function Action_Cut() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    SetBusyState True
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            Clipboard.Clear
            Clipboard.SetText l_ctlControl.SelText, vbCFText
            l_ctlControl.SelText = ""
            ActionUpdate
            Action_Cut = True
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanCut l_booResult
        Action_Cut = l_booResult
        If l_booResult Then l_clpDocument.Cut
    End If
    SetBusyState False
    Err.Clear
End Function

Public Function Action_Delete() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            If l_ctlControl.SelLength > 0 Then
                l_ctlControl.SelText = ""
                Action_Delete = True
            End If
            ActionUpdate
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanDelete l_booResult
        Action_Delete = l_booResult
        If l_booResult Then
            l_clpDocument.Delete
        End If
    End If
    Err.Clear
End Function

Public Sub Action_PreviousWindow()
On Error Resume Next
Dim l_lngIndex As Long
    l_lngIndex = frmMain.Documents.Find(frmMain.ActiveChild)
    If l_lngIndex = 1 Then
        l_lngIndex = frmMain.Documents.Count
    Else
        l_lngIndex = l_lngIndex - 1
    End If
    frmMain.ActivateChild l_lngIndex
    Err.Clear
End Sub

Public Sub Action_NextWindow()
On Error Resume Next
Dim l_lngIndex As Long
    l_lngIndex = frmMain.Documents.Find(frmMain.ActiveChild)
    If l_lngIndex = frmMain.Documents.Count Then
        l_lngIndex = 1
    Else
        l_lngIndex = l_lngIndex + 1
    End If
    frmMain.ActivateChild l_lngIndex
    Err.Clear
End Sub

Public Function Action_Paste() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    SetBusyState True
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            If Clipboard.GetFormat(vbCFText) Then
                l_ctlControl.SelText = Clipboard.GetText(vbCFText)
                Action_Paste = True
            End If
            ActionUpdate
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanPaste l_booResult
        Action_Paste = l_booResult
        If l_booResult Then l_clpDocument.Paste
    End If
    SetBusyState False
    Err.Clear
End Function

Public Function Action_Redo() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    SetBusyState True
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            ' TextBoxes treat Redo the same as Undo
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanRedo l_booResult
        Action_Redo = l_booResult
        If l_booResult Then l_clpDocument.Redo
    End If
    SetBusyState False
    Err.Clear
End Function

Public Function Action_SelectAll() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    SetBusyState True
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            l_ctlControl.SelStart = 0
            l_ctlControl.SelLength = Len(l_ctlControl.Text)
            Action_SelectAll = True
            ActionUpdate
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanSelectAll l_booResult
        Action_SelectAll = l_booResult
        If l_booResult Then l_clpDocument.SelectAll
    End If
    SetBusyState False
    Err.Clear
End Function

Public Function Action_SelectNone() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    SetBusyState True
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            l_ctlControl.SelLength = 0
            Action_SelectNone = True
            ActionUpdate
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanSelectNone l_booResult
        Action_SelectNone = l_booResult
        If l_booResult Then l_clpDocument.SelectNone
    End If
    SetBusyState False
    Err.Clear
End Function

Public Function Action_Undo() As Boolean
On Error Resume Next
Dim l_clpDocument As iEditingCommands, l_ctlControl As Control
    SetBusyState True
    Set l_clpDocument = m_docChild.Form
    Err.Clear
    If l_clpDocument Is Nothing Then
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If TypeOf l_ctlControl Is TextBox Then
            Action_Undo = CBool(SendMessage(l_ctlControl.hwnd, EM_CANUNDO, 0, 0))
            SendMessage l_ctlControl.hwnd, EM_UNDO, 0, 0
            ActionUpdate
        End If
    Else
Dim l_booResult As Boolean
        l_clpDocument.CanUndo l_booResult
        Action_Undo = l_booResult
        If l_booResult Then l_clpDocument.Undo
    End If
    SetBusyState False
    Err.Clear
End Function

Public Sub ActionUpdate()
On Error Resume Next
Dim l_edcDocument As iEditingCommands, l_ctlControl As Control
    Set m_docChild = frmMain.ActiveChild
    CanCut = False
    CanCopy = False
    CanPaste = False
    CanDelete = False
    CanSelectAll = False
    CanSelectNone = False
    CanUndo = False
    CanRedo = False
    
    If m_docChild Is Nothing Then
    ElseIf m_docChild.Form.Visible = False Then
    Else
        Set l_edcDocument = m_docChild.Form
        Err.Clear
        Set l_ctlControl = m_docChild.Form.ActiveControl
        If Not (l_ctlControl Is Nothing) Then
            If TypeOf l_ctlControl Is TextBox Then
                CanCut = (l_ctlControl.SelLength > 0)
                CanCopy = CanCut
                CanPaste = Clipboard.GetFormat(vbCFText)
                CanDelete = CanCut
                CanSelectAll = (Len(l_ctlControl.Text) > 0) And (l_ctlControl.SelLength < Len(l_ctlControl.Text))
                CanSelectNone = l_ctlControl.SelLength > 0
                CanUndo = SendMessage(l_ctlControl.hwnd, EM_CANUNDO, 0, 0) <> 0
                CanRedo = False
            End If
        End If
        If Not (l_edcDocument Is Nothing) Then
            With l_edcDocument
                .CanCut CanCut
                .CanCopy CanCopy
                .CanPaste CanPaste
                .CanDelete CanDelete
                .CanSelectAll CanSelectAll
                .CanSelectNone CanSelectNone
                .CanUndo CanUndo
                .CanRedo CanRedo
            End With
        End If
    End If
    With frmMain.tbrMain
        .Buttons("Action:Undo").Enabled = CanUndo
        .Buttons("Action:Redo").Enabled = CanRedo
        .Buttons("Action:Cut").Enabled = CanCut
        .Buttons("Action:Copy").Enabled = CanCopy
        .Buttons("Action:Paste").Enabled = CanPaste
        .Buttons("Action:Delete").Enabled = CanDelete
        .Buttons("Action:SelectAll").Enabled = CanSelectAll
        .Buttons("Action:SelectNone").Enabled = CanSelectNone
    End With
    g_aclUndo.Enabled = CanUndo
    g_aclRedo.Enabled = CanRedo
    g_aclCut.Enabled = CanCut
    g_aclCopy.Enabled = CanCopy
    g_aclPaste.Enabled = CanPaste
    g_aclDelete.Enabled = CanDelete
    g_aclSelectAll.Enabled = CanSelectAll
    g_aclSelectNone.Enabled = CanSelectNone
End Sub

Public Sub ActivateDocument(ByRef Document As iDocument)
On Error Resume Next
Dim l_lngIndex As Long
    l_lngIndex = frmMain.Documents.Find(Document)
    frmMain.ActivateChild l_lngIndex
End Sub

Public Property Get ActiveDocument() As iDocument
On Error Resume Next
    Set ActiveDocument = m_docChild.Document
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_optOptions = New EditorOptions
    Set m_clpClipboard = New cClipboardViewer
    Set m_amgAccelerators = New cAcceleratorManager
    ngCommon.SetHook Me
    Set m_resResources = LoadResources("common")
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    m_clpClipboard.StopClipboardChangeNotification
    Set m_clpClipboard = Nothing
    m_amgAccelerators.Detach
    Set m_amgAccelerators = Nothing
End Sub

Public Sub CloseDocument(ByRef Doc As iDocument)
On Error Resume Next
Dim l_lngIndex As Long, l_lngDocIndex As Long
Dim l_mgrForm As cChildManager
    For Each l_mgrForm In frmMain.Documents
        l_lngIndex = l_lngIndex + 1
        Err.Clear
        If (l_mgrForm.Form Is Doc) Then
            l_lngDocIndex = l_lngIndex
            Exit For
        End If
    Next l_mgrForm
    frmMain.Documents(l_lngDocIndex).Hide
    DoEvents
    Err.Clear
    frmMain.Documents(l_lngDocIndex).Activate
    If Err <> 0 Then
        Err.Clear
        frmMain.Documents(1).Activate
    End If
End Sub

Public Sub CustomMenu(Index)
On Error Resume Next
Dim l_cmnDocument As iCustomMenus
    Set l_cmnDocument = frmMain.ActiveChild.Form
    If l_cmnDocument Is Nothing Then
        ' This shouldn't happen
    Else
        l_cmnDocument.MenuClick CStr(Index)
    End If
    Err.Clear
End Sub

Public Sub Editor_Exit()
On Error Resume Next
    ExitProgram
End Sub

Public Property Get Engine() As Fury2Engine
On Error Resume Next
    Set Engine = g_engEngine
End Property

Public Sub Event_ClipboardChanged()
On Error Resume Next
    ActionUpdate
End Sub

Public Function Event_DocumentActivate(ByRef Document As cChildManager) As Boolean
On Error Resume Next
    If Not (m_docChild Is Nothing) Then
        If m_docChild.Toolbar Is Nothing Then Else m_docChild.Toolbar.HideToolbar frmMain.tbrLeft
        If m_docChild.Accelerators Is Nothing Then Else m_docChild.Accelerators.UndefineAccelerators m_amgAccelerators
    End If
    frmMain.HideInactiveWindows
    frmMain.RefreshActiveDocument
    frmMain.RefreshDocumentTabs
    Set m_docChild = Document
    If Not (m_docChild Is Nothing) Then
        If m_docChild.Toolbar Is Nothing Then Else m_docChild.Toolbar.ShowToolbar frmMain.tbrLeft
        If m_docChild.Accelerators Is Nothing Then Else m_docChild.Accelerators.DefineAccelerators m_amgAccelerators
    End If
    SetLocation
    SetStatus
    Event_FocusChanged
End Function

Public Function Event_DocumentClose(ByRef Document As cChildManager) As Boolean
On Error Resume Next
'    Document.Toolbars.HideToolbars frmMain
'    If m_docChild Is Document Then Set m_docChild = Nothing
    Event_DocumentActivate Nothing
    frmMain.DocumentClosed Document
End Function

Public Sub Event_FilesystemChanged()
On Error Resume Next
    frmMain.RefreshFileSidebar
    frmMain.RefreshDocumentTabs
End Sub

Public Sub Event_FocusChanged()
On Error Resume Next
    ActionUpdate
End Sub

Public Sub File_New(Optional PluginIndex = -1)
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
Dim l_docDocument As iDocument
    If PluginIndex = -1 Then Exit Sub
    SetBusyState True
    Set l_plgPlugin = g_colFileTypePlugins(CLng(PluginIndex))
    Set l_docDocument = l_plgPlugin.CreateNew
    If l_docDocument Is Nothing Then
    Else
        frmMain.ShowChild l_docDocument
    End If
    SetBusyState False
    Err.Clear
End Sub

Public Sub File_Open(Optional Filename = "")
On Error Resume Next
Dim l_varFilenames As Variant
Dim l_strFilename As String
    Err.Clear
    l_strFilename = CStr(Filename)
    If Trim(l_strFilename) = "" Then
        l_varFilenames = SelectLocalFiles
    Else
        l_varFilenames = Array(l_strFilename)
    End If
    If Trim(l_varFilenames(0)) = "" Then Exit Sub
    OpenFiles l_varFilenames
    Err.Clear
End Sub

Public Sub File_Save(Optional ByRef Document As iDocument = Nothing)
On Error Resume Next
Dim l_docDocument As iDocument
    Set l_docDocument = Document
    If l_docDocument Is Nothing Then Set l_docDocument = frmMain.ActiveChild.Document
    If l_docDocument Is Nothing Then Exit Sub
    If Not l_docDocument.CanSave Then Exit Sub
    If Trim(l_docDocument.Filename = "") Then
        File_SaveAs l_docDocument
    Else
        If l_docDocument.Save(l_docDocument.Filename) Then
            Dim l_rdRecentDocuments As New cRecentDocuments
            l_rdRecentDocuments.AddToRecentDocs l_docDocument.Filename
            Event_FilesystemChanged
        Else
            ShowNotice "Error", "Error saving file """ & l_docDocument.Filename & """:" & vbCrLf & Err.Description, NoticeIcon("error")
        End If
    End If
End Sub

Public Sub File_SaveAll()
On Error Resume Next
Dim l_docDocument As iDocument
    For Each l_docDocument In frmMain.Documents
        File_Save l_docDocument
    Next l_docDocument
End Sub

Public Sub File_SaveAs(Optional ByRef Document As iDocument = Nothing)
On Error Resume Next
Dim l_docDocument As iDocument
Dim l_strFilename As String
    Set l_docDocument = Document
    If l_docDocument Is Nothing Then Set l_docDocument = frmMain.ActiveChild.Document
    Set l_docDocument = frmMain.ActiveChild.Document
    If l_docDocument Is Nothing Then Exit Sub
    If Not l_docDocument.CanSave Then Exit Sub
    l_strFilename = SelectNewFilename(l_docDocument)
    If Len(Trim(l_strFilename)) > 0 Then
        If l_docDocument.Save(l_strFilename) Then
            Dim l_rdRecentDocuments As New cRecentDocuments
            l_rdRecentDocuments.AddToRecentDocs l_docDocument.Filename
            AddRecentFile l_docDocument.Filename
            Event_FilesystemChanged
        Else
            ShowNotice "Error", "Error saving file """ & l_docDocument.Filename & """:" & vbCrLf & Err.Description, NoticeIcon("error")
        End If
    End If
End Sub

Public Property Get Filesystem() As Fury2Filesystem
On Error Resume Next
    Set Filesystem = DefaultEngine.Filesystem
End Property

Public Function FindCapablePlugin(ByRef Filename As String) As iFileTypePlugin
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
    For Each l_plgPlugin In g_colFileTypePlugins
        If l_plgPlugin.CanOpenFile(Filename) Then
            Set FindCapablePlugin = l_plgPlugin
            Exit For
        End If
    Next l_plgPlugin
End Function

Public Function FindFileTypePlugin(ByRef Name As String) As iFileTypePlugin
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
    For Each l_plgPlugin In g_colFileTypePlugins
        If Trim(LCase(l_plgPlugin.FileTypeName)) = Trim(LCase(Name)) Then
            Set FindFileTypePlugin = l_plgPlugin
            Exit For
        End If
    Next l_plgPlugin
End Function

Private Sub Fury2Debugger_CriticalError(Source As String, Location As String, Description As String)
On Error Resume Next
    g_dbgDebugger.GameEngine.Halted = True
    m_dbgDebugger.ConsoleAppend Description & " (" & Source & ", " & Location & ")", m_dbgDebugger.TextColor(TextColors_CriticalError)
    m_dbgDebugger.ShowConsole
    ShowNotice "Warning", "Your game was paused due to a critical error.", NoticeIcon("warning"), Array(Array("OK"), Array("Unpause Game", BindEvent(Me, "Game_Pause")))
End Sub

Private Function Fury2Debugger_DebugCreateOutputPlugin() As Object
End Function

Private Sub Fury2Debugger_DebugFrameEnd()
End Sub

Private Sub Fury2Debugger_DebugFrameStart()
End Sub

Private Sub Fury2Debugger_DebugGameEnd()
On Error Resume Next
    GameIsRunning = False
    AcceleratorManager.RemoveAccelerator m_aclPause
    Set m_aclPause = Nothing
    frmMain.RefreshGameState
    LogOutput "Game ended"
End Sub

Private Sub Fury2Debugger_DebugGameStart()
On Error Resume Next
    GameIsRunning = True
    Set m_aclPause = AcceleratorManager.AddAccelerator(vbKeyPause, BindEvent(Me, "Game_Pause"), True)
    frmMain.RefreshGameState
    LogOutput "Game started"
End Sub

Private Sub Fury2Debugger_DebugHalted()
On Error Resume Next
    GameIsPaused = True
    frmMain.RefreshGameState
    m_dbgDebugger.RefreshView
    LogOutput "Game paused"
End Sub

Private Sub Fury2Debugger_DebugOut(Text As String)
On Error Resume Next
    m_dbgDebugger.ConsoleAppend Text, m_dbgDebugger.TextColor(TextColors_DebugMessage)
End Sub

Private Sub Fury2Debugger_DebugRedrawEnd()

End Sub

Private Sub Fury2Debugger_DebugRedrawStart()

End Sub

Private Sub Fury2Debugger_DebugSubthreadEnd()
End Sub

Private Sub Fury2Debugger_DebugSubthreadStart()
End Sub

Private Sub Fury2Debugger_DebugTextOut(Text As String)
On Error Resume Next
    m_dbgDebugger.ConsoleAppend Text
End Sub

Private Sub Fury2Debugger_DebugUnhalted()
On Error Resume Next
    GameIsPaused = False
    frmMain.RefreshGameState
    m_dbgDebugger.RefreshView
    LogOutput "Game unpaused"
End Sub

Private Sub Fury2Debugger_Error(Error As String, Details As String)
On Error Resume Next
    m_dbgDebugger.ConsoleAppend Error & vbCrLf & Details, m_dbgDebugger.TextColor(TextColors_Error)
End Sub

Private Sub Fury2Debugger_EventBound(NewEvent As Engine.Fury2Event)
On Error Resume Next
End Sub

Private Sub Fury2Debugger_InterruptPoll(Interrupt As Boolean)
On Error Resume Next
    If g_dbgDebugger.GameEngine.Halted Then
        If (HiResTimer - m_sngPauseClicked) >= Options.ScriptTimeout Then
            ShowNotice "Warning", "Your game failed to pause within " & Options.ScriptTimeout & " second(s), so the currently executing script was interrupted.", NoticeIcon("warning")
            Interrupt = True
            LogOutput "Game interrupted"
        End If
    End If
End Sub

Private Sub Fury2Debugger_MapSwitch(NewMap As String)
On Error Resume Next
End Sub

Public Sub Game_New()
On Error Resume Next
    CreateGame
End Sub

Public Sub Game_Open(Optional Filename = "")
On Error Resume Next
Dim l_strFilename As String
    Err.Clear
    l_strFilename = CStr(Filename)
    If Trim(l_strFilename) = "" Then
        l_strFilename = GetPath(SelectLocalFile("Games|game.f2config", "Open Game..."))
    Else
    End If
    If Trim(l_strFilename) = "" Then
    Else
        OpenGame l_strFilename
    End If
End Sub

Public Sub Game_Reload()
On Error Resume Next
Dim l_varDocs As Variant
Dim l_strDocs As String
Dim l_docDocument As cChildManager
    For Each l_docDocument In frmMain.Documents
        If (Trim(l_docDocument.Document.Filename) <> "") Then
            If Not (l_docDocument.Document.Plugin Is Nothing) Then
                If (TypeOf l_docDocument.Document.Plugin Is iFileTypePlugin) Then
                    If Len(l_strDocs) > 0 Then l_strDocs = l_strDocs & " "
                    l_strDocs = l_strDocs & """" & l_docDocument.Document.Filename & """"
                End If
            End If
        End If
    Next l_docDocument
    Action_CloseAllWindows
    OpenGame GamePath
    If (g_edEditor.Options.OpenPreviousDocumentsAtStartup) Then
        l_varDocs = ParseFileList(l_strDocs)
        g_edEditor.OpenFiles l_varDocs
    End If
End Sub

Public Sub Game_Pause()
On Error Resume Next
    g_dbgDebugger.GameEngine.Halted = Not g_dbgDebugger.GameEngine.Halted
    m_sngPauseClicked = HiResTimer
End Sub

Public Sub Game_Run()
On Error Resume Next
    If InIDE Then
        Shell App.Path & "\..\..\binary\sys\fury².exe " & GamePath
    Else
        Shell App.Path & "\fury².exe " & GamePath
    End If
    Err.Clear
End Sub

Public Sub Game_Debug()
On Error Resume Next
    If GameIsRunning Then
        If GameIsPaused Then
            g_dbgDebugger.GameEngine.Halted = False
        End If
        g_dbgDebugger.GameEngine.Quit
    Else
        frmMain.tmrPlayGame.Enabled = True
    End If
    Err.Clear
End Sub

Friend Property Let GamePath(NewPath As String)
    m_strGamePath = NewPath
End Property

Public Property Get GamePath() As String
On Error Resume Next
    GamePath = m_strGamePath
End Property

Public Sub Help_ChangeLog()
On Error Resume Next
    Load frmChangelog
    frmChangelog.txtText.Text = Resources.ItemData("changelog.txt")
    frmChangelog.Show vbModal, frmMain
End Sub

Public Sub Help_About()
On Error Resume Next
    If g_engEngine Is Nothing Then
        Beep
        Exit Sub
    End If
    Load frmAbout
    frmAbout.Show vbModal, frmMain
End Sub

Public Sub Help_OnlineDocs()
On Error Resume Next
    Call ShellExecute(frmMain.hwnd, "Open", "http://fury2.luminance.org/wiki/default.aspx/Documentation.Index", "", "", vbNormalFocus)
End Sub

Public Sub Help_OnlineTutorials()
On Error Resume Next
    Call ShellExecute(frmMain.hwnd, "Open", "http://fury2.luminance.org/wiki/default.aspx/Documentation.Tutorials", "", "", vbNormalFocus)
End Sub

Public Sub HideDebugger()
On Error Resume Next
    CloseDocument m_dbgDebugger
    Unload m_dbgDebugger
    Set m_dbgDebugger = Nothing
End Sub

Friend Sub InitClipboard()
On Error Resume Next
    m_clpClipboard.InitClipboardChangeNotification frmMain.hwnd
End Sub

Public Sub LoadOptions()
On Error Resume Next
Dim l_bytSetting() As Byte
Dim l_lngUBound As Long
Dim l_vfSetting As VirtualFile
    l_bytSetting = ReadRegData("Options")
    l_lngUBound = -1
    l_lngUBound = UBound(l_bytSetting)
    If l_lngUBound > 0 Then
        Set l_vfSetting = F2File
        l_vfSetting.Data = l_bytSetting
        Set m_optOptions = New EditorOptions
        l_vfSetting.Load m_optOptions
    End If
End Sub

Public Function LoadResources(ByRef Filename As String) As ngResourceFile
On Error Resume Next
    If InStr(Filename, ":") Then
        Set LoadResources = LoadResourceFile(Filename & ".zip")
    Else
        If InIDE Then
            Set LoadResources = LoadResourceFile(App.Path & "\..\..\binary\sys\resources\" & Filename & ".zip")
        Else
            Set LoadResources = LoadResourceFile(App.Path & "\resources\" & Filename & ".zip")
        End If
    End If
End Function

Public Sub LoadSettings(ByRef Name As String, ByRef Settings As IVirtualFileSaveable)
On Error Resume Next
Dim l_bytSetting() As Byte
Dim l_lngUBound As Long
Dim l_vfSetting As VirtualFile
    l_bytSetting = ReadRegData("Plugins\Options\" & Name)
    l_lngUBound = -1
    l_lngUBound = UBound(l_bytSetting)
    If l_lngUBound > 0 Then
        Set l_vfSetting = F2File
        l_vfSetting.Data = l_bytSetting
        l_vfSetting.Load Settings
    End If
End Sub

Public Sub LogOutput(Text)
On Error Resume Next
    frmMain.LogAppend CStr(Text)
    Debug.Print "> " & Text
End Sub

Private Sub m_clpClipboard_ClipboardChanged()
On Error Resume Next
    Event_ClipboardChanged
End Sub

Public Sub NewDocument(ByRef Doc As iDocument)
On Error Resume Next
    If Doc Is Nothing Then Exit Sub
    SetBusyState True
    frmMain.ShowChild Doc
    frmMain.HideInactiveWindows
    DoEvents
    SetBusyState False
    Err.Clear
End Sub

Public Function OpenFile(ByRef Filename As String) As iDocument
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
Dim l_rdRecentDocuments As New cRecentDocuments
Dim l_docDocument As cChildManager
    SetBusyState True
    For Each l_docDocument In frmMain.Documents
        If LCase(Trim(l_docDocument.Document.Filename)) = LCase(Trim(Filename)) Then
            SetBusyState False
            Exit Function
        End If
    Next l_docDocument
    If FileExists(Filename) Then
        For Each l_plgPlugin In g_colFileTypePlugins
            If l_plgPlugin.CanOpenFile(Filename) Then
                Err.Clear
                Set OpenFile = l_plgPlugin.LoadFromFilename(Filename)
                If OpenFile Is Nothing Then
                    Set OpenFile = l_plgPlugin.LoadFromFile(F2File(Filename))
                End If
                If OpenFile Is Nothing Then
                Else
                    l_rdRecentDocuments.AddToRecentDocs Filename
                    AddRecentFile Filename
                End If
                Exit For
            End If
        Next l_plgPlugin
    Else
    End If
    SetBusyState False
End Function

Public Sub OpenFiles(ByRef Files As Variant)
On Error Resume Next
Dim l_docDocument As iDocument
Dim l_cchDocument As cChildManager
Dim l_lngFiles As Long
Dim l_booFound As Boolean
    If UBound(Files) < 0 Then
        Err.Clear
        Exit Sub
    End If
    SetStatus "Opening Files"
    frmMain.SetProgress 0
    If UBound(Files) - 1 > 32 Then
        ShowNotice "Warning", "You can only open up to 32 files in one operation, for performance and stability reasons. Only 32 of the files you selected were opened.", NoticeIcon("warning")
        ReDim Preserve Files(0 To 31)
    End If
    Dim l_strGame As String, l_lngFile As Long
    Do While Len(l_strGame) = 0
        l_strGame = LocateGame(CStr(Files(l_lngFile)))
        l_lngFile = l_lngFile + 1
        If l_lngFile > UBound(Files) Then
            Exit Do
        End If
    Loop
    If l_strGame <> "" Then
        If Not GameIsLoaded Then
            If g_edEditor.Options.AutoOpenGames Then
                OpenGame l_strGame
                ShowNotice "Information", "The game """ & g_edEditor.Engine.GameName & """ was automatically loaded.", NoticeIcon("information")
            End If
        Else
            If Right(l_strGame, 1) = "\" Then
                l_strGame = Left(l_strGame, Len(l_strGame) - 1)
            End If
            If Trim(LCase(l_strGame)) <> Trim(LCase(GamePath)) Then
                If g_edEditor.Options.AutoSwitchGames Then
                    OpenGame l_strGame
                    ShowNotice "Warning", "The game """ & g_edEditor.Engine.GameName & """ was automatically loaded so a file could be opened. Your current game and all associated files were closed.", NoticeIcon("warning")
                End If
            End If
            If LCase(Trim(GamePath)) <> LCase(Trim(l_strGame)) Then Exit Sub
        End If
    End If
    If Not GameIsLoaded Then
        ShowNotice "Error", "You must have a game open to load files, and the editor was unable to open the game that the selected file(s) belong to.", NoticeIcon("error"), , False
        Exit Sub
    End If
    SetBusyState True
    For l_lngFiles = LBound(Files) To UBound(Files)
        Set l_docDocument = Nothing
        l_booFound = False
        For Each l_cchDocument In frmMain.Documents
            If LCase(GetShortFileName(l_cchDocument.Document.Filename)) = LCase(GetShortFileName(CStr(Files(l_lngFiles)))) Then
                l_cchDocument.Activate
                l_booFound = True
                Exit For
            End If
        Next l_cchDocument
        If Not l_booFound Then
            If Len(Trim(Files(l_lngFiles))) > 0 Then
                Set l_docDocument = OpenFile(CStr(Files(l_lngFiles)))
            End If
            If l_docDocument Is Nothing Then
                ShowNotice "Error", "Unable to load """ + Files(l_lngFiles) + """.", NoticeIcon("error"), Array(Array("OK"), Array("Retry", BindEvent(Me, "OpenFiles", Array(Array(CStr(Files(l_lngFiles)))))))
            Else
                frmMain.ShowChild l_docDocument
            End If
        End If
        frmMain.HideInactiveWindows
        frmMain.SetProgress (l_lngFiles) / (UBound(Files))
    Next l_lngFiles
    SetStatus
    frmMain.SetProgress
    SetBusyState False
'    frmMain.Documents(frmMain.Documents.Count).Activate
    frmMain.RefreshWindows
End Sub

Public Property Get Options() As EditorOptions
On Error Resume Next
    Set Options = m_optOptions
End Property

Friend Property Set Options(ByVal NewValue As EditorOptions)
On Error Resume Next
    Set m_optOptions = NewValue
End Property

Public Sub Plugins_Activate(Index)
On Error Resume Next
Dim l_plgPlugin As iPlugin
    Set l_plgPlugin = g_colPlugins(CLng(Index))
    If l_plgPlugin Is Nothing Then
        Beep
        Exit Sub
    End If
    l_plgPlugin.Activate
    Err.Clear
End Sub

Public Sub Plugins_Manage()
On Error Resume Next
    frmPluginManager.Show vbModal, frmMain
    frmMain.RefreshPluginToolbar
    Err.Clear
End Sub

Public Property Get Resources() As ngResourceFile
    Set Resources = m_resResources
End Property

Public Sub SaveOptions()
On Error Resume Next
Dim l_vfSetting As VirtualFile
    Set l_vfSetting = F2File
    l_vfSetting.Save m_optOptions
    WriteRegData "Options", l_vfSetting.Data
End Sub

Public Sub SaveSettings(ByRef Name As String, ByRef Settings As IVirtualFileSaveable)
On Error Resume Next
Dim l_vfSetting As VirtualFile
    Set l_vfSetting = F2File
    l_vfSetting.Save Settings
    WriteRegData "Plugins\Options\" & Name, l_vfSetting.Data
End Sub

Public Sub SetBusyState(ByVal Busy As Boolean)
On Error Resume Next
    mdlEditor.SetBusyState Busy
End Sub

Public Sub SetLocation(Optional ByRef Value As String = "")
On Error Resume Next
    frmMain.SetLocation Value
End Sub

Public Sub SetProgress(Optional ByVal Value As Single = 0)
On Error Resume Next
    frmMain.SetProgress Value
End Sub

Public Sub SetStatus(Optional ByRef Value As String = "Ready")
On Error Resume Next
    frmMain.SetStatus Value
End Sub

Public Sub Show_FileSidebar()
On Error Resume Next
    frmMain.picFileSidebar.Visible = Not frmMain.picFileSidebar.Visible
    frmMain.RefreshFileSidebar
    Err.Clear
End Sub

Public Sub Show_Log()
On Error Resume Next
    frmMain.picLog.Visible = Not frmMain.picLog.Visible
    Err.Clear
End Sub

Public Sub Show_GameToolbar()
On Error Resume Next
    frmMain.tbrGame.Visible = Not frmMain.tbrGame.Visible
    frmMain.RefreshToolbars
    Err.Clear
End Sub

Public Sub Show_MainToolbar()
On Error Resume Next
    frmMain.tbrMain.Visible = Not frmMain.tbrMain.Visible
    frmMain.RefreshToolbars
    Err.Clear
End Sub

Public Sub Show_Options()
On Error Resume Next
    frmOptions.Show vbModal, frmMain
    Err.Clear
End Sub

Public Sub Show_PluginToolbar()
On Error Resume Next
    frmMain.tbrPlugins.Visible = Not frmMain.tbrPlugins.Visible
    frmMain.RefreshToolbars
    Err.Clear
End Sub

Public Sub Show_WindowList()
On Error Resume Next
    frmWindowList.Show vbModal, frmMain
    Err.Clear
End Sub

Public Sub ShowDebugger()
On Error Resume Next
    Set m_dbgDebugger = New frmGameDebugger
    Load m_dbgDebugger
    NewDocument m_dbgDebugger
End Sub

Public Sub Trace(Text)
On Error Resume Next
    Debug.Print "> " & Text
End Sub

Public Function SelectFile(Optional Filter As String = "", Optional Title As String = "Open...") As String
On Error Resume Next
    SelectFile = SelectLocalFile(Filter, Title)
End Function

Public Function SelectSaveFilename(Optional OldFilename As String = "", Optional Filter As String = "", Optional Title As String = "Save...") As String
On Error Resume Next
Dim l_dlgDialog As New GCommonDialog
Dim l_strFilename As String
Dim l_booOldState As Boolean
    l_strFilename = OldFilename
    l_booOldState = g_edEditor.AcceleratorManager.Enabled
    g_edEditor.AcceleratorManager.Enabled = False
    If l_dlgDialog.VBGetSaveFileName(l_strFilename, , True, Filter & "|All Files|*.*", , , Title) Then
        SelectSaveFilename = l_strFilename
    End If
    g_edEditor.AcceleratorManager.Enabled = l_booOldState
End Function

Public Sub Macro_Run(Optional ByVal Filename As String = "")
On Error Resume Next
    If Filename = "" Then Filename = SelectFile("Macros|*.f2script;*.f2macro", "Select Macro")
    If Len(Trim(Filename)) < 1 Then Exit Sub
    RunMacroFromFile Filename
End Sub

Public Sub Macro_RunCustom()
On Error Resume Next
    RunMacro PromptForScript
End Sub

Public Property Get Version() As String
On Error Resume Next
    Version = "ngIDE " + Choose(App.Major + 1, "Alpha", "Beta", "Release") + " " + CStr(App.Minor) + " Build " + CStr(App.Revision)
End Property

Public Sub RunMacroFromFile(ByVal Filename As String)
On Error Resume Next
    RunMacro ReadTextFile(Filename)
End Sub

Public Sub RunMacro(ByRef Text As String)
On Error Resume Next
    LogOutput "Running macro"
    Engine.ScriptEngine.AddCode Text
End Sub

Public Function PromptForScript(Optional ByVal Title As String = "Enter Script", Optional ByRef Default As String = "") As String
On Error Resume Next
    Load frmScriptInput
    frmScriptInput.Caption = Title
    frmScriptInput.scScript.Text = Default
    frmScriptInput.Show vbModal, frmMain
    If frmScriptInput.Cancelled Then
    Else
        PromptForScript = frmScriptInput.Text
    End If
    Unload frmScriptInput
End Function

Public Function InspectObject(ByVal Obj As Object) As String
On Error Resume Next
Dim l_strResult As String
Dim l_iifObject As InterfaceInfo
Dim l_lngItems As Long
Dim l_lngItemCount As Long
Dim l_booAddNew As Boolean
Dim l_lngStack As Long, l_booFound As Boolean
Dim l_strInfo As String
    Set l_iifObject = InterfaceInfoFromObject(Obj)
    If l_iifObject Is Nothing Then
        Exit Function
    End If
    l_strResult = l_iifObject.Name & vbCrLf
    With l_iifObject.Members
        For l_lngItems = 1 To .Count
            With .Item(l_lngItems)
                l_strResult = l_strResult & .Name & vbCrLf
            End With
        Next l_lngItems
    End With
    InspectObject = l_strResult
End Function
