Attribute VB_Name = "mdlGame"
Option Explicit

Public Property Get GameIsLoaded() As Boolean
On Error Resume Next
    GameIsLoaded = Len(Trim(g_edEditor.GamePath)) >= 3
End Property

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
    frmMain.RefreshFileSidebar
    frmMain.Caption = "Editor² - " & g_edEditor.Engine.GameName
    frmMain.RefreshGameState
    AddRecentGame Path
    SetBusyState False
End Sub
