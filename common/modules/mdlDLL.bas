Attribute VB_Name = "mdlDLL"
'
'    Engine (Fury² Game Creation System Runtime Engine)
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

'Private Declare Function DllRegister Lib "softfx" Alias "_RegisterServer@4" (ByVal Filename As String) As Long
'Private Declare Function DllUnregister Lib "softfx" Alias "_UnregisterServer@4" (ByVal Filename As String) As Long
'
'Public Function RegisterServer(DllServerPath As String, bRegister As Boolean) As Boolean
'On Error Resume Next
'    If InStr(DllServerPath, ":") Then
'    Else
'        If Left(DllServerPath, 1) = "\" Then
'            DllServerPath = Left(CurDir, 2) + DllServerPath
'        Else
'            DllServerPath = CurDir + IIf(Right(CurDir, 1) = "\", "", "\") + DllServerPath
'        End If
'    End If
'    If bRegister Then
'        DllRegister DllServerPath
'    Else
'        DllUnregister DllServerPath
'    End If
'End Function

Public Function RegisterServer(DllServerPath As String, bRegister As Boolean) As Boolean
On Error Resume Next
    Screen.MousePointer = 11
    If InStr(DllServerPath, ":") Then
    Else
        If Left(DllServerPath, 1) = "\" Then
            DllServerPath = Left(CurDir, 2) + DllServerPath
        Else
            DllServerPath = CurDir + IIf(Right(CurDir, 1) = "\", "", "\") + DllServerPath
        End If
    End If
    If bRegister Then
        Shell "regsvr32 /s """ & DllServerPath & """", vbMinimizedNoFocus
    Else
        Shell "regsvr32 /s /u """ & DllServerPath & """", vbMinimizedNoFocus
    End If
    Sleep 2500
    Screen.MousePointer = 0
End Function

