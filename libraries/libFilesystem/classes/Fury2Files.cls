VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Files"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "A list of files returned by the EnumFiles function."
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
Private m_feFiles() As FileEnumerationEntry
Private m_fsFilesystem As Fury2Filesystem

Friend Sub SetData(Files() As FileEnumerationEntry, FileSystem As Fury2Filesystem)
On Error Resume Next
    m_feFiles = Files
    Set m_fsFilesystem = FileSystem
End Sub

Public Property Get NameCollection() As Fury2Collection
On Error Resume Next
Dim l_lngFiles As Long
    l_lngFiles = -1
    l_lngFiles = UBound(m_feFiles)
    If l_lngFiles = -1 Or Err <> 0 Then Exit Property
    Set NameCollection = New Fury2Collection
    For l_lngFiles = LBound(m_feFiles) To UBound(m_feFiles)
        NameCollection.Add m_feFiles(l_lngFiles).Filename
    Next l_lngFiles
End Property

Public Property Get Names() As Variant
On Error Resume Next
Dim l_strNames As Variant
Dim l_lngFiles As Long
    l_lngFiles = -1
    l_lngFiles = UBound(m_feFiles)
    If l_lngFiles = -1 Or Err <> 0 Then Exit Property
    ReDim l_strNames(0 To UBound(m_feFiles))
    For l_lngFiles = LBound(m_feFiles) To UBound(m_feFiles)
        l_strNames(l_lngFiles) = m_feFiles(l_lngFiles).Filename
    Next l_lngFiles
    Names = l_strNames
End Property

Public Property Get List() As String
On Error Resume Next
Dim l_lngFiles As Long
Dim l_strBuffer As String, l_lngBufferLength As Long
    If UBound(m_feFiles) = LBound(m_feFiles) Then
        List = m_feFiles(LBound(m_feFiles)).Filename
        Exit Property
    End If
    l_lngBufferLength = 0
    l_strBuffer = Space(512)
    For l_lngFiles = LBound(m_feFiles) To UBound(m_feFiles)
        With m_feFiles(l_lngFiles)
            Do While l_lngBufferLength + Len(.Filename) >= Len(l_strBuffer)
                l_strBuffer = l_strBuffer + Space(512)
            Loop
            Mid(l_strBuffer, l_lngBufferLength + 1, Len(.Filename) + 2) = .Filename + vbCrLf
            l_lngBufferLength = l_lngBufferLength + Len(.Filename) + 2
        End With
    Next l_lngFiles
    List = Left(l_strBuffer, l_lngBufferLength - 2)
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = (UBound(m_feFiles) - LBound(m_feFiles)) + 1
End Property

Public Property Get File(Index) As Fury2File
Attribute File.VB_UserMemId = 0
Attribute File.VB_MemberFlags = "200"
On Error Resume Next
Dim l_filFile As Fury2File
    If CLng(Index) - 1 < LBound(m_feFiles) Or CLng(Index) - 1 > UBound(m_feFiles) Then Exit Property
    Set l_filFile = New Fury2File
    l_filFile.SetData m_feFiles(CLng(Index) - 1), m_fsFilesystem
    Set File = l_filFile
End Property
