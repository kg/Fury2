VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "EditorOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
'
'    ngCommon (Fury² Game Creation System Next-Generation Editor Common Library)
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

' This is in ngCommon because it has to be exported for ObjectInspector to work

Option Explicit
Implements IVirtualFileSaveable
Implements IInspectable
Public SaveWindowPositions As Boolean
Attribute SaveWindowPositions.VB_VarDescription = "Saves the location of editor dialogs so that when they are opened, they appear in their previous position."
Public OpenPreviousGameAtStartup As Boolean
Attribute OpenPreviousGameAtStartup.VB_VarDescription = "Saves the filename of the current game at exit and reopens it at startup."
Public OpenPreviousDocumentsAtStartup As Boolean
Attribute OpenPreviousDocumentsAtStartup.VB_VarDescription = "Saves the filenames of all open documents on exit and reopens them at startup."
Public AutoOpenGames As Boolean
Attribute AutoOpenGames.VB_VarDescription = "If a game is not loaded, the editor will attempt to locate and open the game that files belong to when opening them."
Public AutoSwitchGames As Boolean
Attribute AutoSwitchGames.VB_VarDescription = "Enables switching games automatically when loading files that belong to a game other than the one that is currently loaded."
Public PromptWhenSwitchingGames As Boolean
Attribute PromptWhenSwitchingGames.VB_VarDescription = "Prompts before switching games automatically when loading files."
Public PromptSaveWhenClosing As Boolean
Attribute PromptSaveWhenClosing.VB_VarDescription = "Prompts to save all open files when closing them as a group."
Public NoticeAutoCloseTime As Single
Public ScriptTimeout As Single

Public Function Duplicate() As EditorOptions
On Error Resume Next
Dim l_optNew As EditorOptions
    Set l_optNew = New EditorOptions
    With l_optNew
        .AutoOpenGames = AutoOpenGames
        .AutoSwitchGames = AutoSwitchGames
        .OpenPreviousDocumentsAtStartup = OpenPreviousDocumentsAtStartup
        .OpenPreviousGameAtStartup = OpenPreviousGameAtStartup
        .PromptSaveWhenClosing = PromptSaveWhenClosing
        .PromptWhenSwitchingGames = PromptWhenSwitchingGames
        .SaveWindowPositions = SaveWindowPositions
        .NoticeAutoCloseTime = NoticeAutoCloseTime
    End With
    Set Duplicate = l_optNew
End Function

Private Sub Class_Initialize()
On Error Resume Next
    SaveWindowPositions = True
    OpenPreviousGameAtStartup = False
    OpenPreviousDocumentsAtStartup = False
    AutoOpenGames = True
    AutoSwitchGames = True
    PromptWhenSwitchingGames = True
    PromptSaveWhenClosing = True
    NoticeAutoCloseTime = 10
    ScriptTimeout = 2
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load SaveWindowPositions
        .Load OpenPreviousGameAtStartup
        .Load OpenPreviousDocumentsAtStartup
        .Load AutoOpenGames
        .Load AutoSwitchGames
        .Load PromptWhenSwitchingGames
        .Load PromptSaveWhenClosing
        .Load NoticeAutoCloseTime
        .Load ScriptTimeout
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save SaveWindowPositions
        .Save OpenPreviousGameAtStartup
        .Save OpenPreviousDocumentsAtStartup
        .Save AutoOpenGames
        .Save AutoSwitchGames
        .Save PromptWhenSwitchingGames
        .Save PromptSaveWhenClosing
        .Save NoticeAutoCloseTime
        .Save ScriptTimeout
    End With
End Sub
