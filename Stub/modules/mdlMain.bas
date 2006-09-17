Attribute VB_Name = "mdlMain"
'
'    Stub (Fury² Game Creation System Loader Stub)
'    Copyright (C) 2003 Kevin Gadd
'
'    This program is free software; you can redistribute it and/or
'    modify it under the terms of the GNU General Public License
'    as published by the Free Software Foundation; either version 2
'    of the License, or (at your option) any later version.
'
'    This program is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with this program; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
'

Option Explicit

Public Declare Sub PostQuitMessage Lib "user32" (ByVal nExitCode As Long)

Private UnzipQueue As Collection
Private GameName As String

Public Sub CreateGameRoot()
On Error Resume Next
    MkDir GetFileRoot() & "\Games"
    MkDir GetFileRoot() & "\Games\" & GameName
End Sub

Public Function GetGameRoot() As String
On Error Resume Next
    If Len(GameName) > 0 Then
        GetGameRoot = GetFileRoot & "\Games\" & GameName
    End If
End Function

Public Sub CreateFileRoot()
On Error Resume Next
    MkDir GetShellFolderPath(ShellFolder_ApplicationData) & "\Squared Interactive"
    MkDir GetShellFolderPath(ShellFolder_ApplicationData) & "\Squared Interactive\Fury2"
    MkDir GetShellFolderPath(ShellFolder_ApplicationData) & "\Squared Interactive\Fury2\System"
    MkDir GetShellFolderPath(ShellFolder_ApplicationData) & "\Squared Interactive\Fury2\Temp"
    Err.Clear
End Sub

Public Function GetFileRoot() As String
On Error Resume Next
    GetFileRoot = GetShellFolderPath(ShellFolder_ApplicationData) & "\Squared Interactive\Fury2"
End Function

Function LoadLibraries() As Boolean
On Error Resume Next
Dim l_colLibraries As Collection
Dim l_lngLibrary As Long
Dim l_strFilename As String
Dim l_lngResult As Long
Dim l_booFailed As Boolean
Dim l_lngCount As Long
Dim l_booOptional As Boolean
    If InIDE Then
        Compromise.SetEnabled 0
        LoadLibraries = True
        Exit Function
    End If
    If Compromise.IsSupported() = 0 Then
        LoadLibraries = True
        Exit Function
    End If
    Err.Clear
    Compromise.LoadCompromise
    If Err <> 0 Then
        MsgBox Err.Description & "(" & Err.Number & ")", vbExclamation, "Error"
        LoadLibraries = False
        Exit Function
    End If
    Set l_colLibraries = New Collection
    l_colLibraries.Add "!vbscript.dll"
    l_colLibraries.Add "scriptengine.dll"
    l_colLibraries.Add "graphics.dll"
    l_colLibraries.Add "script2.dll"
    l_colLibraries.Add "sound2.dll"
    l_colLibraries.Add "video.dll"
    l_colLibraries.Add "engine.dll"
    l_colLibraries.Add "filesystem.dll"
    l_colLibraries.Add "!http.dll"
    Load frmLoading
    frmLoading.Show
    frmLoading.SetFocus
    frmLoading.Caption = "Loading..."
    frmLoading.SetProgress 0
    Compromise.Initialize
    l_lngCount = l_colLibraries.Count
    For l_lngLibrary = 1 To l_lngCount
        l_strFilename = l_colLibraries(l_lngLibrary)
        If Left(l_strFilename, 1) = "!" Then
            l_strFilename = Mid(l_strFilename, 2)
            l_booOptional = True
        Else
            l_booOptional = False
        End If
        If Not InIDE Then
            l_lngResult = 0
            If FileExists(App.Path + "\" + l_strFilename) Then
                l_lngResult = Compromise.Unregister(App.Path + "\" + l_strFilename)
                l_lngResult = Compromise.Register(App.Path + "\" + l_strFilename)
            End If
            If (l_lngResult = 1) Then
            Else
                l_lngResult = 0
                If FileExists(GetFileRoot() + "\System\" + l_strFilename) Then
                    l_lngResult = Compromise.Unregister(GetFileRoot() + "\System\" + l_strFilename)
                    l_lngResult = Compromise.Register(GetFileRoot() + "\System\" + l_strFilename)
                End If
                If (l_lngResult = 1) Then
                Else
                    If l_booOptional Then
                    Else
                        MsgBox "Unable to load: " + l_colLibraries(l_lngLibrary), vbExclamation, "Error"
                        l_booFailed = True
                    End If
                End If
            End If
        End If
        frmLoading.SetProgress l_lngLibrary / l_colLibraries.Count
    Next l_lngLibrary
    frmLoading.Hide
    Unload frmLoading
    LoadLibraries = Not l_booFailed
End Function

Function InIDE() As Boolean
On Error Resume Next
    Err.Clear
    Debug.Assert 1 / 0
    If Err <> 0 Then
        Err.Clear
        InIDE = True
    Else
        Err.Clear
    End If
End Function

Sub Main()
On Error Resume Next
Dim l_strFolder As String
Dim l_bfBrowse As cBrowseForFolder
Dim l_engEngine As Fury2Engine
Dim l_strLoadError As String
    ChDrive Left(App.Path, 2)
    ChDir App.Path
    If Not UnpackResources Then
        Exit Sub
    End If
    If Not LoadLibraries Then
        Exit Sub
    End If
    If Not PerformUnzip Then
        Exit Sub
    End If
    DoEvents
    Load frmNull
    Err.Clear
    l_strFolder = Command$
    If Len(GameName) > 0 Then
        l_strFolder = GetGameRoot()
    ElseIf FileExists("game.f2config") Then
        l_strFolder = CurDir()
    ElseIf FileExists("game\game.f2config") Then
        l_strFolder = CurDir() & "\game\"
    End If
    If (Trim(l_strFolder) = "") Then
        l_strFolder = App.Path
        Set l_bfBrowse = New cBrowseForFolder
        l_bfBrowse.EditBox = True
        l_bfBrowse.FileSystemOnly = True
        l_bfBrowse.InitialDir = App.Path
        l_bfBrowse.UseNewUI = True
        l_bfBrowse.Title = "Select Game"
        l_strFolder = l_bfBrowse.BrowseForFolder
        If Len(Trim(l_strFolder)) <= 0 Then
            End
        End If
        Err.Clear
        Fury2Load l_strFolder, EM_Normal, frmNull, , l_engEngine
    Else
        Err.Clear
        Fury2Load l_strFolder, EM_Normal, frmNull, , l_engEngine
    End If
    If (Err.Number <> 0) Or (frmNull.Loaded = False) Or (l_engEngine Is Nothing) Then
        l_strLoadError = Err.Description & "(" & Err.Number & ")" & vbCrLf
        l_strLoadError = l_strLoadError & Engine.LoadError
        MsgBox "Unable to load Fury²." & vbCrLf & l_strLoadError, vbCritical, "Error"
        Unload frmNull
        End
    End If
End Sub

Function UnpackResources() As Boolean
On Error Resume Next
Dim l_lngModule As Long
Dim l_strManifest As String
Dim l_strOptions As String
Dim l_strFiles() As String
Dim l_strArgs() As String
Dim l_strFolders() As String
Dim l_strPath As String
Dim l_lngFiles As Long, l_lngFolders As Long
Dim l_strOutFile As String, l_booForce As Boolean
Dim l_strErrors As String
    UnpackResources = True
    Set UnzipQueue = New Collection
    l_lngModule = GetModuleHandleFromPointer(0)
    l_strManifest = ReadResourceTextEx(l_lngModule, 32766)
    If Len(l_strManifest) > 2 Then
        CreateFileRoot
        ChDrive Left(GetFileRoot(), 2)
        ChDir GetFileRoot()
        Load frmLoading
        frmLoading.Show
        frmLoading.Caption = "Unpacking..."
        frmLoading.SetFocus
        l_strFiles = Split(l_strManifest, vbCrLf)
        frmLoading.SetProgress 0
        For l_lngFiles = LBound(l_strFiles) To UBound(l_strFiles)
            frmLoading.SetProgress (l_lngFiles / UBound(l_strFiles))
            If InStr(l_strFiles(l_lngFiles), "=") Then
                l_strArgs = Split(l_strFiles(l_lngFiles), "=")
                Select Case LCase(Trim(l_strArgs(0)))
                Case "gamename"
                    GameName = l_strArgs(1)
                Case Else
                End Select
            ElseIf InStr(l_strFiles(l_lngFiles), "|") Then
                l_strArgs = Split(l_strFiles(l_lngFiles), "|")
                Err.Clear
                l_strOutFile = l_strArgs(1)
                l_strOutFile = Replace(l_strOutFile, "$root$", GetFileRoot())
                l_strOutFile = Replace(l_strOutFile, "$sys$", GetFileRoot() + "\System")
                l_strOutFile = Replace(l_strOutFile, "$game$", GetGameRoot())
                If InStr(l_strOutFile, "\") Then
                    l_strPath = ""
                    l_strFolders = Split(l_strOutFile, "\")
                    For l_lngFolders = LBound(l_strFolders) To UBound(l_strFolders) - 1
                        l_strPath = l_strPath & l_strFolders(l_lngFolders)
                        MkDir l_strPath
                        l_strPath = l_strPath & "\"
                    Next l_lngFolders
                End If
                If InStr(l_strOutFile, "!unzip!") Then
                    l_strOutFile = Replace(l_strOutFile, "!unzip!", "")
                    UnzipQueue.Add l_strOutFile
                    l_booForce = True
                Else
                    l_booForce = False
                End If
                Err.Clear
                If l_booForce Then
                    ReadResourceFileEx l_lngModule, DefaultResType, CLng(l_strArgs(0)), l_strOutFile
                Else
                    ReadResourceFileEx l_lngModule, DefaultResType, CLng(l_strArgs(0)), l_strOutFile, CDate(l_strArgs(2))
                End If
                If Err <> 0 Then
                    l_strErrors = l_strErrors & l_strOutFile & ":" & Err.Description & vbCrLf
                    UnpackResources = False
                End If
            End If
        Next l_lngFiles
        If Not UnpackResources Then
            MsgBox "Errors while unpacking:" & vbCrLf & l_strErrors, vbCritical, "Unpack Failed"
        End If
        frmLoading.SetProgress 1
        frmLoading.Hide
        Unload frmLoading
        ChDrive Left(GetFileRoot(), 2)
        ChDir GetFileRoot() + "\System\"
    End If
End Function

Sub CopyNewFiles(ByRef FromPath As String, ByRef ToPath As String, ByVal Recursive As Boolean)
On Error Resume Next
Dim l_strFilePath As String
Dim l_strFilename As String
Dim l_strPattern As String
Dim l_strFolder As String
Dim l_colFolders As Collection
Dim l_lngFolders As Long
Dim l_strOutPath As String
    l_strFilePath = Left(FromPath, InStrRev(FromPath, "\"))
    l_strPattern = Replace(FromPath, l_strFilePath, "", , 1)
    l_strFilename = Dir(FromPath, vbNormal)
    Do While Len(Trim(l_strFilename)) > 0
        Err.Clear
        If FileExists(ToPath & l_strFilename) Then
            If FileDateTime(ToPath & l_strFilename) >= FileDateTime(l_strFilePath & l_strFilename) Then
                Kill l_strFilePath & l_strFilename
                Err.Clear
            Else
                Err.Clear
                Kill ToPath & l_strFilename
                If Err <> 0 Then
                    MsgBox "Error while decompressing file " & l_strFilename & ":" & vbCrLf & Err.Description
                Else
                    Err.Clear
                    Name l_strFilePath & l_strFilename As ToPath & l_strFilename
                    If Err <> 0 Then
                        MsgBox "Error while decompressing file " & l_strFilename & ":" & vbCrLf & Err.Description
                    End If
                End If
            End If
        Else
            Err.Clear
            Name l_strFilePath & l_strFilename As ToPath & l_strFilename
            If Err <> 0 Then
                MsgBox "Error while decompressing file " & l_strFilename & ":" & vbCrLf & Err.Description
            End If
        End If
        l_strFilename = Dir
    Loop
    If Recursive Then
        Set l_colFolders = New Collection
        l_strFolder = Dir(l_strFilePath, vbDirectory)
        Do While Len(Trim(l_strFolder)) > 0
            If (GetAttr(l_strFilePath & l_strFolder) And vbDirectory) = vbDirectory Then
                If l_strFolder = "." Or l_strFolder = ".." Then
                Else
                    l_colFolders.Add l_strFolder
                End If
            End If
            l_strFolder = Dir(, vbDirectory)
        Loop
        If l_colFolders.Count > 0 Then
            For l_lngFolders = 1 To l_colFolders.Count
                l_strFolder = l_colFolders(l_lngFolders)
                MkDir ToPath & l_strFolder
                CopyNewFiles l_strFilePath & l_strFolder & "\" & l_strPattern, ToPath & l_strFolder & "\", Recursive
                RmDir l_strFilePath & l_strFolder
            Next l_lngFolders
        End If
    End If
End Sub

Function PerformUnzip() As Boolean
On Error Resume Next
#If 0 Then
Dim l_zipZip As XZip.Zip
Dim l_lngArchives As Long
Dim l_strFile As String
Dim l_strFilePath As String
Dim l_colItems As XZip.Items
Dim l_itmItem As XZip.Item
Dim l_strItemPath As String
    PerformUnzip = True
    Set l_zipZip = New Zip
    If UnzipQueue.Count > 0 Then
        Load frmLoading
        frmLoading.Show
        frmLoading.Caption = "Decompressing..."
        frmLoading.SetFocus
        frmLoading.SetProgress 0
        For l_lngArchives = 1 To UnzipQueue.Count
            l_strFile = UnzipQueue(l_lngArchives)
            l_strFilePath = Left(l_strFile, InStrRev(l_strFile, "\"))
            l_zipZip.UnPack l_strFile, GetFileRoot & "\Temp\"
            If l_zipZip.ErrorCode <> 0 Then
                MsgBox l_zipZip.ErrorDescription
                PerformUnzip = False
            Else
                Kill l_strFile
                CopyNewFiles GetFileRoot & "\Temp\*.*", l_strFilePath, True
            End If
            frmLoading.SetProgress (l_lngArchives) / (UnzipQueue.Count)
        Next l_lngArchives
        frmLoading.SetProgress 1
        frmLoading.Hide
        Unload frmLoading
    End If
    Set UnzipQueue = Nothing
#Else
    PerformUnzip = True
#End If
End Function

