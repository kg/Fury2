VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2File"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "A class that represents a file in the virtual filesystem."
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
Private m_feFile As FileEnumerationEntry
Private m_fsFilesystem As Fury2Filesystem
Private m_strRealPath As String

Public Sub SetData(File As FileEnumerationEntry, FileSystem As Fury2Filesystem)
On Error Resume Next
    m_feFile = File
    m_feFile.Filename = Replace(m_feFile.Filename, "//", "/")
    Set m_fsFilesystem = FileSystem
End Sub

Public Property Get Extension() As String
On Error Resume Next
Dim l_lngPos As Long
    Extension = m_fsFilesystem.GetTitle(m_feFile.Filename)
    l_lngPos = InStrRev(Extension, ".")
    If l_lngPos Then
        Extension = Mid(Extension, l_lngPos + 1)
    Else
        Extension = ""
    End If
End Property

Public Property Get Title() As String
Attribute Title.VB_Description = "The title of the file."
    Title = m_fsFilesystem.GetTitle(m_feFile.Filename)
End Property

Public Property Get Path() As String
Attribute Path.VB_Description = "The path of the file."
    Path = m_fsFilesystem.GetPath(m_feFile.Filename)
End Property

Public Property Get Name() As String
Attribute Name.VB_Description = "The full name of the file. (Includes path.)"
Attribute Name.VB_UserMemId = 0
Attribute Name.VB_MemberFlags = "200"
    Name = m_feFile.Filename
End Property

' Engine use only
Public Function GetRealFilename() As String
Attribute GetRealFilename.VB_Description = "Extracts the file to the hard disk if necessary and returns a Real filename that can be used to access it. This file will be removed when this class is destroyed, if it did not exist when this method was first called."
Attribute GetRealFilename.VB_MemberFlags = "40"
On Error Resume Next
Dim l_strFilename As String
    ' If the filename is already cached, return it
    l_strFilename = m_strRealPath
    If Len(Trim(l_strFilename)) < 1 Then
        m_strRealPath = m_feFile.Parent.LocateFile(m_feFile.Filename)
        l_strFilename = m_strRealPath
    End If
    GetRealFilename = l_strFilename
End Function

' Make sure the file is on the hard disk
Public Sub Extract()
Attribute Extract.VB_Description = "If the file is in a package, it is extracted to the hard disk. Otherwise, nothing happens."
On Error Resume Next
    GetRealFilename
End Sub

Public Function LoadImage() As Fury2Image
On Error Resume Next
Dim l_strFilename As String
    Err.Clear
    l_strFilename = GetRealFilename
    Set LoadImage = F2LoadImage(l_strFilename)
End Function

' Load the file into a VirtualFile object
Public Function Load() As VirtualFile
Attribute Load.VB_Description = "Returns a VirtualFile object containing the contents of the file, or Nothing if it no longer exists."
On Error Resume Next
Dim l_strFilename As String
    Err.Clear
    l_strFilename = GetRealFilename
    Set Load = F2File(l_strFilename)
End Function
 
' Save a VirtualFile object into the file
Public Sub Save(ByVal File As VirtualFile)
Attribute Save.VB_Description = "Saves a VirtualFile object into the file. (Note that this only saves the data temporarily; packages will not be altered.)"
On Error Resume Next
Dim l_strFilename As String
Dim l_objFile As VirtualFile
    Err.Clear
    Set l_objFile = File
    If l_objFile Is Nothing Then Exit Sub
    l_strFilename = GetRealFilename
    l_objFile.SaveFile l_strFilename
End Sub

' Load the file into a string
Public Property Get Text() As String
Attribute Text.VB_Description = "Returns the text of the file in a String, or vbNullString if it no longer exists."
On Error Resume Next
    Text = m_fsFilesystem.ReadFile(GetRealFilename)
End Property

Public Property Let Text(NewText As String)
On Error Resume Next
Dim l_strFilename As String
    l_strFilename = GetRealFilename
    Call m_fsFilesystem.WriteFile(l_strFilename, NewText)
End Property

Public Property Get Parent() As Fury2FSModule
    Set Parent = m_feFile.Parent
End Property

Public Property Get Size() As Long
    Size = m_feFile.Size
End Property

Public Property Get CreatedDate() As Date
    CreatedDate = m_feFile.CreatedDate
End Property

Public Property Get ModifiedDate() As Date
    ModifiedDate = m_feFile.ModifiedDate
End Property
