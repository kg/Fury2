VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Package2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "A virtual filesystem package."
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
Option Compare Text
Implements Fury2FSModule
Private m_booEnabled As Boolean
Private m_fsFilesystem As Fury2Filesystem
Private m_strFolders() As String
Private m_lngFolderCount As Long
Private m_strFilename As String
Private m_arcArchive As Archive
Private m_strMountPoint As String

Friend Sub LoadArchive(ByRef Filename As String)
On Error Resume Next
    Set m_arcArchive = New Archive
    m_arcArchive.Open Filename, OM_READONLY
End Sub

Friend Property Get Archive() As Archive
    Set Archive = m_arcArchive
End Property

Public Property Get Name() As String
Attribute Name.VB_UserMemId = 0
Attribute Name.VB_MemberFlags = "200"
On Error Resume Next
    Name = m_strFilename
End Property

Public Property Get Loaded() As Boolean
On Error Resume Next
    Loaded = Trim(m_arcArchive.Name) <> ""
End Property

Friend Sub SetFilesystem(FS As Fury2Filesystem)
On Error Resume Next
    Set m_fsFilesystem = FS
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    m_booEnabled = True
    m_strMountPoint = "/"
End Sub

Private Sub Class_Terminate()
On Error Resume Next
End Sub

Private Property Let Fury2FSModule_Enabled(ByVal RHS As Boolean)
    m_booEnabled = RHS
End Property

Private Property Get Fury2FSModule_Enabled() As Boolean
    Fury2FSModule_Enabled = m_booEnabled
End Property

Private Function Fury2FSModule_EnumFiles(ByVal Path As String, ByVal PathAppend As String, Count As Long, ByVal Filter As String, ByVal Recursive As Boolean, Target() As FileEnumerationEntry) As Long
End Function

Private Function Fury2FSModule_EnumFolders(ByVal Path As String, ByVal PathAppend As String, Count As Long, ByVal Recursive As Boolean, Target() As FolderEnumerationEntry) As Long
End Function

Private Property Get Fury2FSModule_File(ByVal Index As Long) As FileEnumerationEntry
On Error Resume Next
Dim l_feFile As FileEnumerationEntry
Dim l_infInfo As FileInfo
    Set l_infInfo = m_arcArchive.GetFileInfo(Index)
    l_feFile.Filename = l_infInfo.Filename
    Fury2FSModule_File = l_feFile
End Property

Private Property Get Fury2FSModule_FileCount() As Long
    Fury2FSModule_FileCount = m_arcArchive.FileCount
End Property

Private Property Get Fury2FSModule_FileExists(Path As String) As Boolean

End Property

Private Property Get Fury2FSModule_Folder(ByVal Index As Long) As FolderEnumerationEntry
On Error Resume Next
Dim l_feFolder As FolderEnumerationEntry
    l_feFolder.Path = m_strFolders(Index)
End Property

Private Property Get Fury2FSModule_FolderCount() As Long
    Fury2FSModule_FolderCount = m_lngFolderCount
End Property

Private Property Get Fury2FSModule_FolderExists(Path As String) As Boolean
End Property

Private Function Fury2FSModule_LocateFile(Filename As String) As String
End Function

Private Property Let Fury2FSModule_MountPoint(ByVal RHS As String)
    m_strMountPoint = RHS
End Property

Private Property Get Fury2FSModule_MountPoint() As String
    Fury2FSModule_MountPoint = m_strMountPoint
End Property

Private Property Get Fury2FSModule_NamedFile(Name As String) As FileEnumerationEntry
End Property

Private Property Get Fury2FSModule_NamedFolder(Name As String) As FolderEnumerationEntry
End Property
