Attribute VB_Name = "mdlGame"
Option Explicit

Public Property Get GameIsLoaded() As Boolean
On Error Resume Next
    GameIsLoaded = Len(Trim(g_edEditor.GamePath)) >= 3
End Property

Public Sub CreateGame()
On Error Resume Next
Dim l_strFolder As String
Dim l_bffFolder As New cBrowseForFolder
Dim l_cfgConfig As Fury2ConfigurationFile
    l_bffFolder.UseNewUI = True
    l_bffFolder.EditBox = True
    l_bffFolder.Title = "Select Game Folder"
    l_bffFolder.FileSystemOnly = True
    l_strFolder = l_bffFolder.BrowseForFolder()
    If Trim(l_strFolder) <> "" Then
        OpenGame l_strFolder
        Set l_cfgConfig = New Fury2ConfigurationFile
        l_cfgConfig.InitDefaultSettings
        l_cfgConfig.Save "game.f2config"
    End If
End Sub

Public Sub OpenGame(ByVal Path As String)
On Error Resume Next
    If frmMain.Documents.Count > 0 Then
        If MsgBox("Opening a new game will close all your currently open documents. Continue?", vbYesNo Or vbExclamation, "Warning") = vbYes Then
            frmMain.CloseAllChildren
        Else
            Exit Sub
        End If
    End If
    SetBusyState True
    ShutdownEngine
    InitEngine Path
    g_edEditor.GamePath = Path
    g_edEditor.Event_FilesystemChanged
    frmMain.Caption = "Editor² - " & g_edEditor.Engine.GameName
    frmMain.RefreshGameState
    AddRecentGame Path
    SetBusyState False
End Sub
