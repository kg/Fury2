Attribute VB_Name = "mdlRecent"
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
Public Const c_lngMaxRecentFiles As Long = 8
Public Const c_lngMaxRecentGames As Long = 4

Public Sub AddRecentFile(Filename As String)
On Error Resume Next
Dim l_lngFileCount As Long, l_lngFiles As Long
Dim l_lngOldPosition As Long, l_lngNewPosition As Long
Dim l_strFilename As String
Dim l_lngMaxRecentFiles As Long
    l_lngMaxRecentFiles = ReadRegSetting("Recent\Files\Maximum", c_lngMaxRecentFiles)
    l_lngFileCount = ReadRegSetting("Recent\Files\Count", 0)
    If l_lngFileCount Then
        For l_lngFiles = 1 To l_lngFileCount
            l_strFilename = ReadRegSetting("Recent\Files\File " & l_lngFiles, "")
            If Trim(LCase(l_strFilename)) = Trim(LCase(Filename)) Then
                l_lngOldPosition = l_lngFiles
                Exit For
            End If
        Next l_lngFiles
        If l_lngOldPosition > 1 Then
            ' Shuffle it up to the top
            
            ' Shuffle the files before this one down by one, to replace it
            For l_lngFiles = l_lngOldPosition - 1 To 1 Step -1
                l_lngNewPosition = l_lngFiles + 1
                l_strFilename = ReadRegSetting("Recent\Files\File " & l_lngFiles, "")
                ' Don't save files that end up past the limit
                If l_lngNewPosition <= l_lngMaxRecentFiles Then
                    WriteRegSetting "Recent\Files\File " & l_lngNewPosition, l_strFilename
                End If
            Next l_lngFiles
            
            ' Write the new file to the registry and update the file count
            WriteRegSetting "Recent\Files\File 1", Filename
            WriteRegSetting "Recent\Files\Count", ClipValue(l_lngFileCount + 1, 1, l_lngMaxRecentFiles)
        ElseIf l_lngOldPosition = 0 Then
            ' Insert it at the beginning
            
            ' Shuffle the existing files down by one
            For l_lngFiles = l_lngFileCount To 1 Step -1
                l_lngNewPosition = l_lngFiles + 1
                l_strFilename = ReadRegSetting("Recent\Files\File " & l_lngFiles, "")
                ' Don't save files that end up past the limit
                If l_lngNewPosition <= l_lngMaxRecentFiles Then
                    WriteRegSetting "Recent\Files\File " & l_lngNewPosition, l_strFilename
                End If
            Next l_lngFiles
            
            ' Write the new file to the registry and update the file count
            WriteRegSetting "Recent\Files\File 1", Filename
            WriteRegSetting "Recent\Files\Count", ClipValue(l_lngFileCount + 1, 1, l_lngMaxRecentFiles)
        End If
    Else
        ' Write the file and the new file count to the registry
        WriteRegSetting "Recent\Files\Count", 1
        WriteRegSetting "Recent\Files\File 1", Filename
    End If
End Sub

Public Sub AddRecentGame(Path As String)
On Error Resume Next
Dim l_lngGameCount As Long, l_lngGames As Long
Dim l_lngOldPosition As Long, l_lngNewPosition As Long
Dim l_strPath As String
Dim l_lngMaxRecentGames As Long
    l_lngMaxRecentGames = ReadRegSetting("Recent\Games\Maximum", c_lngMaxRecentGames)
    l_lngGameCount = ReadRegSetting("Recent\Games\Count", 0)
    If l_lngGameCount Then
        For l_lngGames = 1 To l_lngGameCount
            l_strPath = ReadRegSetting("Recent\Games\Game " & l_lngGames, "")
            If Trim(LCase(l_strPath)) = Trim(LCase(Path)) Then
                l_lngOldPosition = l_lngGames
                Exit For
            End If
        Next l_lngGames
        If l_lngOldPosition > 1 Then
            ' Shuffle it up to the top
            
            ' Shuffle the Games before this one down by one, to replace it
            For l_lngGames = l_lngOldPosition - 1 To 1 Step -1
                l_lngNewPosition = l_lngGames + 1
                l_strPath = ReadRegSetting("Recent\Games\Game " & l_lngGames, "")
                ' Don't save Games that end up past the limit
                If l_lngNewPosition <= l_lngMaxRecentGames Then
                    WriteRegSetting "Recent\Games\Game " & l_lngNewPosition, l_strPath
                End If
            Next l_lngGames
            
            ' Write the new Game to the registry and update the Game count
            WriteRegSetting "Recent\Games\Game 1", Path
            WriteRegSetting "Recent\Games\Count", ClipValue(l_lngGameCount + 1, 1, l_lngMaxRecentGames)
        ElseIf l_lngOldPosition = 0 Then
            ' Insert it at the beginning
            
            ' Shuffle the existing Games down by one
            For l_lngGames = l_lngGameCount To 1 Step -1
                l_lngNewPosition = l_lngGames + 1
                l_strPath = ReadRegSetting("Recent\Games\Game " & l_lngGames, "")
                ' Don't save Games that end up past the limit
                If l_lngNewPosition <= l_lngMaxRecentGames Then
                    WriteRegSetting "Recent\Games\Game " & l_lngNewPosition, l_strPath
                End If
            Next l_lngGames
            
            ' Write the new Game to the registry and update the Game count
            WriteRegSetting "Recent\Games\Game 1", Path
            WriteRegSetting "Recent\Games\Count", ClipValue(l_lngGameCount + 1, 1, l_lngMaxRecentGames)
        End If
    Else
        ' Write the Game and the new Game count to the registry
        WriteRegSetting "Recent\Games\Count", 1
        WriteRegSetting "Recent\Games\Game 1", Path
    End If
End Sub

