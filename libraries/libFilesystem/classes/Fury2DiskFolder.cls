VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2DiskFolder"
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
Option Compare Text
Implements Fury2FSModule
Private m_booEnabled As Boolean
Private m_fsFilesystem As Fury2Filesystem
Private m_strMountPoint As String
Private m_strPath As String

Friend Sub SetPath(Path As String)
On Error Resume Next
    m_strPath = Path
End Sub

Friend Sub SetFilesystem(FS As Fury2Filesystem)
On Error Resume Next
    Set m_fsFilesystem = FS
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    m_strMountPoint = "/"
    m_booEnabled = True
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
    SearchForFiles m_strPath & Replace(Path, "/", "\"), Path, Count, Filter, Recursive, Target, Me
End Function

Private Function Fury2FSModule_EnumFolders(ByVal Path As String, ByVal PathAppend As String, Count As Long, ByVal Recursive As Boolean, Target() As FolderEnumerationEntry) As Long
    SearchForFolders m_strPath & Replace(Path, "/", "\"), PathAppend, Count, Recursive, Target, Me
End Function

Private Property Get Fury2FSModule_File(ByVal Index As Long) As FileEnumerationEntry
On Error Resume Next
Dim l_feFile As FileEnumerationEntry
    l_feFile = FileByIndex(m_strPath, Index)
    l_feFile.Filename = Replace(Replace(l_feFile.Filename, m_strPath, m_strMountPoint), "\", "/")
    Set l_feFile.Parent = Me
    Fury2FSModule_File = l_feFile
End Property

Private Property Get Fury2FSModule_FileCount() As Long
    Fury2FSModule_FileCount = CountFiles(m_strPath)
End Property

Private Property Get Fury2FSModule_FileExists(Path As String) As Boolean
    Fury2FSModule_FileExists = Len(Trim(Dir(m_strPath + Replace(Path, "/", "\")))) > 0
End Property

Private Property Get Fury2FSModule_Folder(ByVal Index As Long) As FolderEnumerationEntry
On Error Resume Next
Dim l_feFolder As FolderEnumerationEntry
    l_feFolder = FolderByIndex(m_strPath, Index)
    l_feFolder.Path = Replace(Replace(l_feFolder.Path, m_strPath, m_strMountPoint), "\", "/")
    Set l_feFolder.Parent = Me
    Fury2FSModule_Folder = l_feFolder
End Property

Private Property Get Fury2FSModule_FolderCount() As Long
    Fury2FSModule_FolderCount = CountFolders(m_strPath)
End Property

Private Property Get Fury2FSModule_FolderExists(Path As String) As Boolean
    Fury2FSModule_FolderExists = Len(Trim(Dir(m_strPath + Replace(Path, "/", "\"), vbDirectory))) > 0
End Property

Private Function Fury2FSModule_LocateFile(Filename As String) As String
    Fury2FSModule_LocateFile = m_strPath & Replace(Filename, "/", "\")
End Function

Private Property Let Fury2FSModule_MountPoint(ByVal RHS As String)
    m_strMountPoint = RHS
End Property

Private Property Get Fury2FSModule_MountPoint() As String
    Fury2FSModule_MountPoint = m_strMountPoint
End Property

Private Property Get Fury2FSModule_NamedFile(Name As String) As FileEnumerationEntry
On Error Resume Next
Dim l_feFile As FileEnumerationEntry
    l_feFile = FileByName(m_strPath & Replace(Name, "/", "\"))
    l_feFile.Filename = Replace(Replace(l_feFile.Filename, m_strPath, m_strMountPoint), "\", "/")
    Set l_feFile.Parent = Me
    Fury2FSModule_NamedFile = l_feFile
End Property

Private Property Get Fury2FSModule_NamedFolder(Name As String) As FolderEnumerationEntry
On Error Resume Next
Dim l_feFolder As FolderEnumerationEntry
    l_feFolder = FolderByName(m_strPath & Replace(Name, "/", "\"))
    l_feFolder.Path = Replace(Replace(l_feFolder.Path, m_strPath, m_strMountPoint), "\", "/")
    Set l_feFolder.Parent = Me
    Fury2FSModule_NamedFolder = l_feFolder
End Property
