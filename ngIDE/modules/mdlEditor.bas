Attribute VB_Name = "mdlEditor"
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
Global g_edEditor As cEditor
Global g_strVersion As String
Global g_booMainWindowLoaded As Boolean
Private m_lngBusyCount As Long

Public Sub Main()
On Error Resume Next
Dim l_varFiles As Variant
    InitCommonControls
    F2Init
    Set g_edEditor = New cEditor
    g_edEditor.LoadOptions
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
    
    frmMain.RefreshActiveDocument
    frmMain.RefreshGameState
    
    If (g_edEditor.Options.OpenPreviousGameAtStartup) Then
        OpenGame ReadRegSetting("Previous Game", "")
    End If
    
    If (g_edEditor.Options.OpenPreviousDocumentsAtStartup) Then
        l_varFiles = ParseFileList(ReadRegSetting("Previous Documents", ""))
        g_edEditor.OpenFiles l_varFiles
    End If
    
    If Trim(Command$) <> "" Then
        l_varFiles = ParseFileList(Command$)
        g_edEditor.OpenFiles l_varFiles
    End If
    
    SetBusyState False
End Sub

Public Sub TerminateProgram()
On Error Resume Next
Dim l_lngForms As Long
    SetBusyState True
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
    g_edEditor.SaveOptions
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
Dim l_strDocs As String
    If GameIsRunning Then
        If GameIsPaused Then
            g_dbgDebugger.GameEngine.Halted = False
        End If
        g_dbgDebugger.GameEngine.Quit
        DoEvents
    End If
    If frmMain.WindowState <> 0 Then frmMain.WindowState = 0
    If g_edEditor.Options.OpenPreviousGameAtStartup Then
        mdlRegistry.WriteRegSetting "Previous Game", g_edEditor.GamePath
    End If
    If g_edEditor.Options.OpenPreviousDocumentsAtStartup Then
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
        mdlRegistry.WriteRegSetting "Previous Documents", l_strDocs
    End If
    frmMain.CloseAllChildren g_edEditor.Options.PromptSaveWhenClosing
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

Public Function ParseFileList(ByRef Files As String) As Variant
On Error Resume Next
Dim l_varFiles As Variant
Dim l_lngFileCount As Long
Dim l_lngCharacter As Long, l_strCharacter As String
Dim l_strFilename As String
Dim l_bytFiles() As Byte
Dim l_booQuotes As Boolean, l_booEnd As Boolean
    If Len(Trim(Files)) = 0 Then Exit Function
    l_bytFiles = StrConv(Files, vbFromUnicode)
    ReDim l_varFiles(0 To 3)
    For l_lngCharacter = LBound(l_bytFiles) To UBound(l_bytFiles)
        l_strCharacter = Chr(l_bytFiles(l_lngCharacter))
        Select Case l_strCharacter
        Case " "
            If l_booQuotes Then
                l_strFilename = l_strFilename & l_strCharacter
            Else
                l_booEnd = True
            End If
        Case """"
            If l_booQuotes Then
                l_booQuotes = False
                l_booEnd = True
            Else
                l_booQuotes = True
            End If
        Case Else
            l_strFilename = l_strFilename & l_strCharacter
        End Select
        If l_booEnd Then
            If Len(Trim(l_strFilename)) > 0 Then
                l_varFiles(l_lngFileCount) = l_strFilename
                l_strFilename = ""
                l_lngFileCount = l_lngFileCount + 1
                If l_lngFileCount > UBound(l_varFiles) Then
                    ReDim Preserve l_varFiles(0 To UBound(l_varFiles) + 4)
                End If
            End If
            l_booEnd = False
        End If
    Next l_lngCharacter
    If Len(Trim(l_strFilename)) > 0 Then
        l_varFiles(l_lngFileCount) = l_strFilename
        l_lngFileCount = l_lngFileCount + 1
    End If
    ReDim Preserve l_varFiles(0 To l_lngFileCount - 1)
    ParseFileList = l_varFiles
End Function
