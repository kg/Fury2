VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2FSModule"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
'
'    libFilesystem (Fury² Virtual Filesystem Library)
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

Public Property Get Initialized() As Boolean
End Property

Public Property Get Enabled() As Boolean
End Property

Public Property Let Enabled(ByVal RHS As Boolean)
End Property

Public Property Get MountPoint() As String
End Property

Public Property Let MountPoint(ByVal RHS As String)
End Property

Public Property Get FileCount() As Long
End Property

Public Property Get FolderCount() As Long
End Property

Public Property Get File(Index As Long) As String
End Property

Public Property Get Folder(Index As Long) As String
End Property

Public Function EnumFiles(ByVal Path As String, ByVal PathAppend As String, ByRef Count As Long, ByVal Filter As String, ByVal Recursive As Boolean, ByRef Target() As FileEnumerationEntry, ByVal Index As Long) As Long
End Function

Public Function EnumFolders(ByVal Path As String, ByVal PathAppend As String, ByRef Count As Long, ByVal Recursive As Boolean, ByRef Target() As FileEnumerationEntry, ByVal Index As Long) As Long
End Function
