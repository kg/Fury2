VERSION 5.00
Begin VB.Form frmNull 
   BorderStyle     =   0  'None
   ClientHeight    =   90
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   90
   Icon            =   "frmNull.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   6
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
End
Attribute VB_Name = "frmNull"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
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

Public Sub Quit()
On Error Resume Next
    DoEvents
    Unload Me
    If InIDE Then
        End
    Else
        Fury2Terminate
    End If
End Sub
