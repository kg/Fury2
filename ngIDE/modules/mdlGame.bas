Attribute VB_Name = "mdlGame"
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
Public g_dbgDebugger As GameDebugger
Public GameIsRunning As Boolean, GameIsPaused As Boolean

Public Property Get GameIsLoaded() As Boolean
On Error Resume Next
    GameIsLoaded = Len(Trim(g_edEditor.GamePath)) >= 3
End Property

Public Function LocateGame(ByRef Filename As String) As String
On Error Resume Next
Dim l_strPath As String
Dim l_lngDepth As Long
    l_lngDepth = 1
    l_strPath = GetPath(Filename)
    Do
        If l_lngDepth > 8 Then Exit Do
        If FileExists(l_strPath & "\game.f2config") Then
            LocateGame = l_strPath
            Exit Function
        End If
        l_strPath = GetPath(l_strPath)
        l_lngDepth = l_lngDepth + 1
    Loop
End Function

Public Sub CreateGame()
On Error Resume Next
Dim l_strFolder As String
Dim l_bffFolder As New cBrowseForFolder
Dim l_cfgConfig As Fury2ConfigurationFile
Dim l_booOldState As Boolean
    l_bffFolder.UseNewUI = True
    l_bffFolder.EditBox = True
    l_bffFolder.Title = "Select Game Folder"
    l_bffFolder.FileSystemOnly = True
    l_booOldState = g_edEditor.AcceleratorManager.Enabled
    g_edEditor.AcceleratorManager.Enabled = False
    l_strFolder = l_bffFolder.BrowseForFolder()
    g_edEditor.AcceleratorManager.Enabled = l_booOldState
    If Trim(l_strFolder) <> "" Then
        g_edEditor.LogOutput "Creating game"
        OpenGame l_strFolder
        Set l_cfgConfig = DefaultEngine.Configuration
        l_cfgConfig.InitDefaultSettings
        l_cfgConfig.Save "game.f2config"
        g_edEditor.ShowNotice "Game Created", "Your new game has been created. Make sure to open the game.f2config file and change the settings to your liking!", g_edEditor.NoticeIcon("information"), Array(Array("OK"), Array("Open game.f2config", BindEvent(g_edEditor, "OpenFiles", Array(Array(g_edEditor.GamePath & "\game.f2config")))))
    End If
End Sub

Public Sub OpenGame(ByVal Path As String)
On Error Resume Next
    If Trim(Path) = "" Then Exit Sub
    If GameIsRunning Then
        If GameIsPaused Then
            g_dbgDebugger.GameEngine.Halted = False
        End If
        g_dbgDebugger.GameEngine.Quit
        DoEvents
    End If
    g_edEditor.LogOutput "Loading game """ & Path & """"
    frmMain.CloseAllChildren g_edEditor.Options.PromptWhenSwitchingGames
    SetBusyState True
    ShutdownEngine
    InitEngine Path
    g_edEditor.GamePath = Path
    g_edEditor.Event_FilesystemChanged
    frmMain.Caption = "Editor² - " & g_edEditor.Engine.GameName
    frmMain.RefreshGameState
    frmMain.RefreshPluginToolbar
    AddRecentGame Path
    SetBusyState False
End Sub

Public Sub PlayGame()
On Error Resume Next
    g_edEditor.LogOutput "Debugging game"
    Set g_dbgDebugger = New GameDebugger
    Set g_dbgDebugger.Hook = g_edEditor
    g_edEditor.ShowDebugger
    g_dbgDebugger.LoadGame g_edEditor.GamePath
    g_edEditor.HideDebugger
End Sub
