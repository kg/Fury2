Attribute VB_Name = "mdlScrollbars"
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
Private Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
Private Const SM_CYHSCROLL = 3
Private Const SM_CXVSCROLL = 2

Function GetScrollbarSize(Scrollbar As FlatScrollBar) As Long
On Error Resume Next
    If Scrollbar.Orientation = efsoHorizontal Then
        GetScrollbarSize = GetSystemMetrics(SM_CYHSCROLL)
    ElseIf Scrollbar.Orientation = efsoVertical Then
        GetScrollbarSize = GetSystemMetrics(SM_CXVSCROLL)
    Else
        GetScrollbarSize = 0
    End If
End Function

