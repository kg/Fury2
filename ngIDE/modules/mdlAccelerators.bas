Attribute VB_Name = "mdlAccelerators"
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

Sub InitAccelerators()
On Error Resume Next
    With g_edEditor.AcceleratorManager
        .DeferredInvoke = True
        .AddAccelerator vbKeyO, BindEvent(g_edEditor, "File_Open"), True
        .AddAccelerator vbKeyS, BindEvent(g_edEditor, "File_Save"), True
        .AddAccelerator vbKeyS, BindEvent(g_edEditor, "File_SaveAll"), True, True
        .AddAccelerator vbKeyX, BindEvent(g_edEditor, "Action_Cut"), True
        .AddAccelerator vbKeyC, BindEvent(g_edEditor, "Action_Copy"), True
        .AddAccelerator vbKeyV, BindEvent(g_edEditor, "Action_Paste"), True
        .AddAccelerator vbKeyZ, BindEvent(g_edEditor, "Action_Undo"), True
        .AddAccelerator vbKeyZ, BindEvent(g_edEditor, "Action_Redo"), True, True
        .AddAccelerator vbKeyA, BindEvent(g_edEditor, "Action_SelectAll"), True
        .AddAccelerator vbKeyD, BindEvent(g_edEditor, "Action_SelectNone"), True
        .AddAccelerator vbKeyDelete, BindEvent(g_edEditor, "Action_Delete")
        .AddAccelerator vbKeyF4, BindEvent(g_edEditor, "Action_CloseWindow"), True
        .AddAccelerator vbKeyF6, BindEvent(g_edEditor, "Action_NextWindow"), True
        .AddAccelerator vbKeyF8, BindEvent(g_edEditor, "Show_FileSidebar")
        .AddAccelerator vbKeyF9, BindEvent(g_edEditor, "Game_Play")
    End With
End Sub
