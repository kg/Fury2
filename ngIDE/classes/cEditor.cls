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
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private Const EM_CANUNDO = &HC6
Private Const EM_UNDO = &HC7

Private Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long

Public CanCut As Boolean, CanCopy As Boolean, CanPaste As Boolean, CanDelete As Boolean, CanSelectAll As Boolean, CanSelectNone As Boolean, CanUndo As Boolean, CanRedo As Boolean
Private m_strGamePath As String
Private m_docChild As cChildManager
Private m_amgAccelerators As cAcceleratorManager
Private WithEvents m_clpClipboard As cClipboardViewer
Attribute m_clpClipboard.VB_VarHelpID = -1

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

Public Sub SetStatus(Optional ByRef Value As String = "Ready")
On Error Resume Next
    frmMain.SetStatus Value
End Sub

Public Sub SetLocation(Optional ByRef Value As String = "")
On Error Resume Next
    frmMain.SetLocation Value
End Sub

Public Sub SetProgress(Optional ByVal Value As Single = 0)
On Error Resume Next
    frmMain.SetProgress Value
End Sub

Public Property Get Filesystem() As Fury2Filesystem
On Error Resume Next
    Set Filesystem = Fury2Globals.Engine.Filesystem
End Property

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

Public Property Get AcceleratorManager() As cAcceleratorManager
On Error Resume Next
    Set AcceleratorManager = m_amgAccelerators
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_clpClipboard = New cClipboardViewer
    Set m_amgAccelerators = New cAcceleratorManager
    ngCommon.SetHook Me
End Sub

Friend Sub InitClipboard()
On Error Resume Next
    m_clpClipboard.InitClipboardChangeNotification frmMain.hwnd
End Sub

Private Sub m_clpClipboard_ClipboardChanged()
On Error Resume Next
    Event_ClipboardChanged
End Sub

Public Property Get Engine() As Fury2Engine
On Error Resume Next
    Set Engine = Fury2Globals.Engine
End Property

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
        .ButtonEnabled("Action:Undo") = CanUndo
        .ButtonEnabled("Action:Redo") = CanRedo
        .ButtonEnabled("Action:Cut") = CanCut
        .ButtonEnabled("Action:Copy") = CanCopy
        .ButtonEnabled("Action:Paste") = CanPaste
        .ButtonEnabled("Action:Delete") = CanDelete
        .ButtonEnabled("Action:SelectAll") = CanSelectAll
        .ButtonEnabled("Action:SelectNone") = CanSelectNone
    End With
End Sub

Public Sub Event_FocusChanged()
On Error Resume Next
    ActionUpdate
End Sub

Public Sub Event_ClipboardChanged()
On Error Resume Next
    ActionUpdate
End Sub

Public Sub Event_FilesystemChanged()
On Error Resume Next
    frmMain.RefreshFileSidebar
End Sub

Public Function Event_DocumentActivate(ByRef Document As cChildManager) As Boolean
On Error Resume Next
    If Not (m_docChild Is Nothing) Then
        If m_docChild.Toolbars Is Nothing Then Else m_docChild.Toolbars.HideToolbars frmMain
        If m_docChild.Accelerators Is Nothing Then Else m_docChild.Accelerators.UndefineAccelerators m_amgAccelerators
    End If
    frmMain.HideInactiveWindows
    frmMain.RefreshActiveDocument
    Set m_docChild = Document
    If Not (m_docChild Is Nothing) Then
        If m_docChild.Toolbars Is Nothing Then Else m_docChild.Toolbars.ShowToolbars frmMain
        If m_docChild.Accelerators Is Nothing Then Else m_docChild.Accelerators.DefineAccelerators m_amgAccelerators
    End If
    Event_FocusChanged
End Function

Public Function Event_DocumentClose(ByRef Document As cChildManager) As Boolean
On Error Resume Next
'    Document.Toolbars.HideToolbars frmMain
'    If m_docChild Is Document Then Set m_docChild = Nothing
    Event_DocumentActivate Nothing
    frmMain.DocumentClosed Document
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
        MsgBox "You can only open up to 32 files at once, for performance and stability reasons. Please select less files.", vbCritical, "Error"
        Exit Sub
    End If
    SetBusyState True
    For l_lngFiles = LBound(Files) To UBound(Files)
        Set l_docDocument = Nothing
        l_booFound = False
        For Each l_cchDocument In frmMain.Documents
            If LCase(Trim(l_cchDocument.Document.Filename)) = LCase(Trim(CStr(Files(l_lngFiles)))) Then
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
                MsgBox "Unable to load """ + Files(l_lngFiles) + """.", vbExclamation, "Error"
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
    SetBusyState False
End Function

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

Public Sub Help_OnlineDocs()
On Error Resume Next
    Call ShellExecute(frmMain.hwnd, "Open", "http://fury2.luminance.org/wiki/default.aspx/Documentation.Index", "", "", vbNormalFocus)
End Sub

Public Sub Help_OnlineTutorials()
On Error Resume Next
    Call ShellExecute(frmMain.hwnd, "Open", "http://fury2.luminance.org/wiki/default.aspx/Documentation.Tutorials", "", "", vbNormalFocus)
End Sub

Public Sub Help_About()
On Error Resume Next
    Load frmAbout
    frmAbout.Show vbModal, frmMain
End Sub

Friend Property Let GamePath(NewPath As String)
    m_strGamePath = NewPath
End Property

Public Property Get GamePath() As String
On Error Resume Next
    GamePath = m_strGamePath
End Property

Public Property Get ActiveDocument() As iDocument
On Error Resume Next
    Set ActiveDocument = m_docChild.Document
End Property

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
        l_clpDocument.SelectAll
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
        l_clpDocument.SelectNone
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
        l_clpDocument.Cut
    End If
    SetBusyState False
    Err.Clear
End Function

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
        l_clpDocument.Copy
    End If
    SetBusyState False
    Err.Clear
End Function

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
        l_clpDocument.Paste
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
        l_clpDocument.Undo
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
        l_clpDocument.Redo
    End If
    SetBusyState False
    Err.Clear
End Function

Public Sub ActivateDocument(ByRef Document As iDocument)
On Error Resume Next
Dim l_lngIndex As Long
    l_lngIndex = frmMain.Documents.Find(Document)
    frmMain.ActivateChild l_lngIndex
End Sub

Public Sub Action_ActivateWindow(ByVal Index)
On Error Resume Next
    frmMain.ActivateChild CLng(Index)
    Err.Clear
End Sub

Public Sub Show_WindowList()
On Error Resume Next
    frmWindowList.Show vbModal, frmMain
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

Public Sub Action_CloseWindow()
On Error Resume Next
Dim l_lngIndex As Long
    frmMain.ActiveChild.Hide
    Err.Clear
End Sub

Public Sub Action_CloseAllWindows()
On Error Resume Next
    frmMain.CloseAllChildren
    Err.Clear
End Sub

Public Sub Show_MainToolbar()
On Error Resume Next
    If frmMain.tbrMain.Tag = "" Then
        frmMain.HideDockObject "Main Toolbar"
    Else
        frmMain.ShowDockObject "Main Toolbar"
    End If
    Err.Clear
End Sub

Public Sub Show_GameToolbar()
On Error Resume Next
    If frmMain.tbrGame.Tag = "" Then
        frmMain.HideDockObject "Game Toolbar"
    Else
        frmMain.ShowDockObject "Game Toolbar"
    End If
    Err.Clear
End Sub

Public Sub Show_PluginToolbar()
On Error Resume Next
    If frmMain.tbrPlugins.Tag = "" Then
        frmMain.HideDockObject "Plugin Toolbar"
    Else
        frmMain.ShowDockObject "Plugin Toolbar"
    End If
    Err.Clear
End Sub

Public Sub Show_FileSidebar()
On Error Resume Next
    If (frmMain.FindOwned("File Sidebar") Is Nothing) Then
        frmMain.ShowDockObject "File Sidebar"
    Else
        frmMain.HideDockObject "File Sidebar"
    End If
    Err.Clear
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

Public Sub Game_Play()
On Error Resume Next
    If InIDE Then
        Shell App.Path & "\..\..\binary\sys\fury².exe " & GamePath
    Else
        Shell App.Path & "\fury².exe " & GamePath
    End If
    Err.Clear
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

Public Sub File_Save()
On Error Resume Next
Dim l_docDocument As iDocument
    Set l_docDocument = frmMain.ActiveChild.Document
    If Trim(l_docDocument.Filename = "") Then
        File_SaveAs
    Else
        If l_docDocument.Save(l_docDocument.Filename) Then
            Dim l_rdRecentDocuments As New cRecentDocuments
            l_rdRecentDocuments.AddToRecentDocs l_docDocument.Filename
            Event_FilesystemChanged
        Else
            MsgBox "Error saving file.", vbExclamation, "Error"
        End If
    End If
End Sub

Public Sub File_SaveAs()
On Error Resume Next
Dim l_docDocument As iDocument
Dim l_strFilename As String
    Set l_docDocument = frmMain.ActiveChild.Document
    l_strFilename = SelectNewFilename(l_docDocument)
    If Len(Trim(l_strFilename)) > 0 Then
        If l_docDocument.Save(l_strFilename) Then
            Dim l_rdRecentDocuments As New cRecentDocuments
            l_rdRecentDocuments.AddToRecentDocs l_docDocument.Filename
            AddRecentFile l_docDocument.Filename
            Event_FilesystemChanged
        Else
            MsgBox "Error saving file.", vbExclamation, "Error"
        End If
    End If
End Sub

Public Sub Editor_Exit()
On Error Resume Next
    ExitProgram
End Sub

Public Sub Trace(Text)
On Error Resume Next
    Debug.Print "Plugin: " & Text
End Sub

Public Sub LogOutput(Text)
On Error Resume Next
    Debug.Print "Engine: " & Text
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    m_clpClipboard.StopClipboardChangeNotification
    Set m_clpClipboard = Nothing
    m_amgAccelerators.Detach
    Set m_amgAccelerators = Nothing
End Sub
