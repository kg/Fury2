Attribute VB_Name = "mdlEditor"
Option Explicit
Global g_edEditor As cEditor
Global g_strVersion As String
Global g_booMainWindowLoaded As Boolean
Private m_lngBusyCount As Long

Public Sub Main()
On Error Resume Next
    InitCommonControls
    F2Init
    Set g_edEditor = New cEditor
    Err.Clear
    g_strVersion = Engine.Fury2Globals.GetEngineVersion()
    If (Err <> 0) Or Len(Trim(g_strVersion)) = "" Then
        Select Case MsgBox("Unable to load engine. Click Retry to attempt to repair installation.", vbExclamation Or vbRetryCancel, "Error")
        Case vbRetry
            InstallEngine
            Err.Clear
            g_strVersion = Engine.Fury2Globals.GetEngineVersion()
            If (Err <> 0) Or Len(Trim(g_strVersion)) = "" Then
                MsgBox "Installation failed.", vbCritical, "Error"
                F2Shutdown
                End
            End If
        Case Else
            F2Shutdown
            End
        End Select
    End If
    Err.Clear
    Load frmIcons
    
    DefineMenus
    
    Load frmMain
    frmMain.Show
    
    SetBusyState True
    
    InitPlugins
    LoadPlugins
    
    g_edEditor.InitClipboard
    
    SetMenuHandler "Main Menu", frmMain
    
    InitFilesystem App.Path
    InitAccelerators
    
    g_edEditor.AcceleratorManager.Attach frmMain.hwnd
    g_edEditor.Event_FocusChanged
    
    SetBusyState False
End Sub

Public Sub TerminateProgram()
On Error Resume Next
Dim l_lngForms As Long
    SetBusyState True
    frmMain.CloseAllChildren
    ShutdownPlugins
    ShutdownFilesystem
    frmMain.Hide
    Unload frmMain
    CleanupMenus
    Unload frmIcons
    Do While Forms.Count >= 2
        If Forms(l_lngForms) Is frmTerminate Then
            l_lngForms = l_lngForms + 1
        Else
            Unload Forms(l_lngForms)
        End If
    Loop
    frmTerminate.Hide
    Unload frmTerminate
    ShutdownEngine
    F2Shutdown
    SetBusyState False
    If InIDE Then
        End
    Else
        TerminateProcess GetCurrentProcess, 0
    End If
End Sub

Public Sub ExitProgram()
On Error Resume Next
Dim l_docDocument As cChildManager
Dim l_lngCount As Long
    If GameIsRunning Then
        If GameIsPaused Then
            g_dbgDebugger.GameEngine.Halted = False
        End If
        g_dbgDebugger.GameEngine.Quit
        DoEvents
    End If
    If frmMain.WindowState = 1 Then frmMain.WindowState = 0
    For Each l_docDocument In frmMain.Documents
        If l_docDocument.Document.CanSave Then l_lngCount = l_lngCount + 1
    Next l_docDocument
    If l_lngCount > 0 Then
        Load frmSaveOpenDocuments
        frmSaveOpenDocuments.Show vbModal, frmMain
        If frmSaveOpenDocuments.Cancelled Then
            Unload frmSaveOpenDocuments
            Exit Sub
        Else
            Unload frmSaveOpenDocuments
        End If
    End If
    SetBusyState True
    SetStatus "Shutting Down"
    Load frmTerminate
    frmTerminate.Show
    frmTerminate.tmrKill.Enabled = True
    SetBusyState False
End Sub

Public Function InIDE() As Boolean
On Error Resume Next
    Err.Clear
    Debug.Assert 1 / 0
    If Err.Number <> 0 Then
        Err.Clear
        InIDE = True
    Else
        Err.Clear
    End If
End Function

Public Function StripEndCharacters(ByRef Text As String, ByVal NumberOfCharacters As Long) As String
On Error Resume Next
    StripEndCharacters = Left(Text, Len(Text) - NumberOfCharacters)
End Function

Public Function DoCommand(ByVal CommandName As String, ParamArray p() As Variant) As Boolean
On Error Resume Next
Dim l_strParameter As String
    If InStr(CommandName, "(") Then
        l_strParameter = Trim(Mid(CommandName, InStr(CommandName, "(") + 1))
        If Right(l_strParameter, 1) = ")" Then l_strParameter = Trim(Left(l_strParameter, Len(l_strParameter) - 1))
        If Left(l_strParameter, 1) = """" Then
            ' String
            DoCommand = DoCommand(Left(CommandName, InStr(CommandName, "(") - 1), CStr(Mid(l_strParameter, 2, Len(l_strParameter) - 2)))
        Else
            ' Integer
            DoCommand = DoCommand(Left(CommandName, InStr(CommandName, "(") - 1), CLng(l_strParameter))
        End If
        Exit Function
    End If
    CommandName = Replace(CommandName, ":", "_")
    Err.Clear
    Select Case UBound(p)
    Case 0
        CallByName g_edEditor, CommandName, VbMethod, p(0)
    Case 1
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1)
    Case 2
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1), p(2)
    Case 3
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1), p(2), p(3)
    Case 4
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1), p(2), p(3), p(4)
    Case 5
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1), p(2), p(3), p(4), p(5)
    Case 6
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1), p(2), p(3), p(4), p(5), p(6)
    Case 7
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1), p(2), p(3), p(4), p(5), p(6), p(7)
    Case 8
        CallByName g_edEditor, CommandName, VbMethod, p(0), p(1), p(2), p(3), p(4), p(5), p(6), p(7), p(8)
    Case Else
        Err.Clear
        CallByName g_edEditor, CommandName, VbMethod
    End Select
    If Err = 0 Then
        DoCommand = True
    End If
End Function

Public Sub SetBusyState(ByVal State As Boolean)
On Error Resume Next
    If State Then
        m_lngBusyCount = m_lngBusyCount + 1
    Else
        m_lngBusyCount = m_lngBusyCount - 1
    End If
    If m_lngBusyCount <= 0 Then
        Screen.MousePointer = 0
        If g_booMainWindowLoaded Then frmMain.Enabled = True
    Else
        Screen.MousePointer = 11
        If g_booMainWindowLoaded Then frmMain.Enabled = False
    End If
End Sub

Public Sub SetStatus(Optional ByVal Status As String = "Ready")
On Error Resume Next
    If g_booMainWindowLoaded Then frmMain.SetStatus Status
End Sub
