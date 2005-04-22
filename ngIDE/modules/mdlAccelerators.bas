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
Global g_aclSave As cAccelerator
Global g_aclCut As cAccelerator
Global g_aclCopy As cAccelerator
Global g_aclPaste As cAccelerator
Global g_aclDelete As cAccelerator
Global g_aclUndo As cAccelerator
Global g_aclRedo As cAccelerator
Global g_aclSelectAll As cAccelerator
Global g_aclSelectNone As cAccelerator

Sub InitAccelerators()
On Error Resume Next
    With g_edEditor.AcceleratorManager
        .DeferredInvoke = True
        .AddAccelerator vbKeyO, BindEvent(g_edEditor, "File_Open"), True
        Set g_aclSave = .AddAccelerator(vbKeyS, BindEvent(g_edEditor, "File_Save"), True)
        .AddAccelerator vbKeyS, BindEvent(g_edEditor, "File_SaveAll"), True, True
        Set g_aclCut = .AddAccelerator(vbKeyX, BindEvent(g_edEditor, "Action_Cut"), True)
        Set g_aclCopy = .AddAccelerator(vbKeyC, BindEvent(g_edEditor, "Action_Copy"), True)
        Set g_aclPaste = .AddAccelerator(vbKeyV, BindEvent(g_edEditor, "Action_Paste"), True)
        Set g_aclUndo = .AddAccelerator(vbKeyZ, BindEvent(g_edEditor, "Action_Undo"), True)
        Set g_aclRedo = .AddAccelerator(vbKeyZ, BindEvent(g_edEditor, "Action_Redo"), True, True)
        Set g_aclSelectAll = .AddAccelerator(vbKeyA, BindEvent(g_edEditor, "Action_SelectAll"), True)
        Set g_aclSelectNone = .AddAccelerator(vbKeyD, BindEvent(g_edEditor, "Action_SelectNone"), True)
        Set g_aclDelete = .AddAccelerator(vbKeyDelete, BindEvent(g_edEditor, "Action_Delete"))
        .AddAccelerator vbKeyF4, BindEvent(g_edEditor, "Action_CloseWindow"), True
        .AddAccelerator vbKeyF6, BindEvent(g_edEditor, "Action_NextWindow"), True
        .AddAccelerator vbKeyTab, BindEvent(g_edEditor, "Action_NextWindow"), True
        .AddAccelerator vbKeyF6, BindEvent(g_edEditor, "Action_PreviousWindow"), True, True
        .AddAccelerator vbKeyTab, BindEvent(g_edEditor, "Action_PreviousWindow"), True, True
        .AddAccelerator vbKeyF8, BindEvent(g_edEditor, "Show_FileSidebar")
        .AddAccelerator vbKeyF9, BindEvent(g_edEditor, "Game_Debug")
    End With
End Sub
