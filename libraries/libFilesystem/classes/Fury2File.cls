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
Private m_booExtracted As Boolean, m_strRealPath As String

Public Property Get Location() As Long
    Location = m_feFile.Location
End Property

Public Property Get Ownership() As Boolean
Attribute Ownership.VB_Description = "If True, this class owns the file and will remove it from the hard disk when it is destroyed. If False, you can destroy this class and its file will stay on the hard disk."
    Ownership = m_booExtracted
End Property

Public Property Let Ownership(NewOwnership As Boolean)
    m_booExtracted = NewOwnership
End Property

Public Sub SetData(File As FileEnumerationEntry, FileSystem As Fury2Filesystem)
    m_feFile = File
    Set m_fsFilesystem = FileSystem
End Sub

Public Property Get Extension() As String
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
    If Trim(l_strFilename) = "" Then
        If m_fsFilesystem.FileExists(m_feFile.Filename) Then
            ' Otherwise check if it's already on the hard disk
            l_strFilename = m_fsFilesystem.GetFile(m_feFile.Filename)
            If Trim(l_strFilename) = "" Then
                ' Otherwise extract it to the hard disk
                l_strFilename = m_fsFilesystem.ExtractFile(m_feFile.Filename)
                If Trim(l_strFilename) = "" Then
                    ' Failed :(
                Else
                    ' Set ownership so we can clean up our mess
                    m_booExtracted = True
                    ' Return the filename and store it
                    GetRealFilename = l_strFilename
                    m_strRealPath = l_strFilename
                End If
            Else
                ' Return the filename and store it
                GetRealFilename = l_strFilename
                m_strRealPath = l_strFilename
            End If
        Else
            GetRealFilename = m_fsFilesystem.CreateFile(m_feFile.Filename)
        End If
    Else
        ' Return the filename
        GetRealFilename = l_strFilename
    End If
End Function

' Make sure the file is on the hard disk
Public Sub Extract()
Attribute Extract.VB_Description = "If the file is in a package, it is extracted to the hard disk. Otherwise, nothing happens."
On Error Resume Next
    GetRealFilename
End Sub

' If we have ownership of the file, delete the hard disk copy
Public Sub Free()
Attribute Free.VB_Description = "Removes the file from the hard disk if this class owns it."
On Error Resume Next
    If m_booExtracted Then
        m_booExtracted = False
        Err.Clear
        Kill m_strRealPath
        If Err.Number <> 0 Then
            m_fsFilesystem.QueueForDeletion m_strRealPath
        End If
        m_strRealPath = ""
    End If
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
Public Sub Save(File)
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

' Repack the file into its source archive
Public Sub Repack()
On Error Resume Next
#If SawZIP = 1 Then
If m_fsFilesystem.EnablePackages Then
Dim l_filFile As SAWZIPLib.File
Dim l_filNew As SAWZIPLib.File
Dim l_strFilename As String, l_strLocalFilename As String, l_strNewFilename As String
Dim l_lngFile As Long
Dim l_arcArchive As SAWZIPLib.Archive
    If m_feFile.Location <> Location_Package Then Exit Sub ' Not in an archive
    l_strLocalFilename = GetRealFilename
    Set l_arcArchive = m_fsFilesystem.Package(m_feFile.LocationIndex).Archive
    With l_arcArchive
        ' Enumerate all files in the package
        l_lngFile = 1
        For Each l_filFile In .Files
            With l_filFile
                If Err <> 0 Or l_filFile Is Nothing Then Exit For

                ' Files only
                If Not .Directory Then
                    ' Get the name and properly format it
                    l_strFilename = "/" + Replace(.Name, "\", "/")
                    If Trim(LCase(l_strFilename)) = Trim(LCase(m_feFile.Filename)) Then

                        l_arcArchive.Files.Remove l_lngFile - 1 ' 0-based? WTF
                        Exit For

                    End If
                End If

            End With
            Err.Clear
            l_lngFile = l_lngFile + 1
        Next l_filFile
    End With
    l_arcArchive.Files.AddFileByName l_strLocalFilename, 9, False, True
End If
#End If
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
    Call m_fsFilesystem.WriteFile(GetRealFilename, NewText)
End Property

Private Sub Class_Terminate()
    ' Destroy the file if we have ownership
    Free
End Sub
