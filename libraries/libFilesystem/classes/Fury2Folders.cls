VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Folders"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "A list of folders returned by the EnumFolders function."
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
Private m_feFolders() As FolderEnumerationEntry
Private m_fsFilesystem As Fury2Filesystem

Public Sub Sort(Optional Ascending As Boolean)
On Error Resume Next
'    Call QuickSort(m_feFolders, 0, UBound(m_feFolders), IIf(Ascending, 1, -1))
End Sub

Private Sub QuickSort(ByRef Arr() As String, ByVal l As Long, ByVal r As Long, ByVal flag As Integer)
'    If (r <= l) Then Exit Sub
'    Dim i As Long
'    Dim j As Long
'    Dim temp As String
'    Dim ret As Integer
'    i = l - 1
'    j = r
'
'    Do
'        Do
'            i = i + 1
'            ret = StrComp(Arr(i), Arr(r))
'            ret = ret * flag
'        Loop While (ret < 0)
'
'        Do While (j > 0)
'            j = j - 1
'            ret = StrComp(Arr(j), Arr(r))
'            ret = ret * flag
'            If (ret <= 0) Then Exit Do
'        Loop
'        If (i > j) Then Exit Do
'        temp = Arr(i)
'        Arr(i) = Arr(j)
'        Arr(j) = temp
'    Loop
'    temp = Arr(i)
'    Arr(i) = Arr(r)
'    Arr(r) = temp
'    Call QuickSort(Arr, l, i - 1, flag)
'    Call QuickSort(Arr, i + 1, r, flag)
End Sub

Friend Sub SetData(Folders() As FolderEnumerationEntry, FileSystem As Fury2Filesystem)
On Error Resume Next
    m_feFolders = Folders
    Set m_fsFilesystem = FileSystem
End Sub

Public Property Get Names() As Variant
On Error Resume Next
Dim l_strNames As Variant
Dim l_lngFolders As Long
    l_lngFolders = -1
    l_lngFolders = UBound(m_feFolders)
    If l_lngFolders = -1 Or Err <> 0 Then Exit Property
    ReDim l_strNames(0 To UBound(m_feFolders))
    For l_lngFolders = LBound(m_feFolders) To UBound(m_feFolders)
        l_strNames(l_lngFolders) = m_feFolders(l_lngFolders).Path
    Next l_lngFolders
    Names = l_strNames
End Property

Public Property Get List() As String
On Error Resume Next
Dim l_lngFolders As Long
Dim l_strBuffer As String, l_lngBufferLength As Long
    If UBound(m_feFolders) = LBound(m_feFolders) Then
        List = m_feFolders(LBound(m_feFolders)).Path
        Exit Property
    End If
    l_lngBufferLength = 0
    l_strBuffer = Space(512)
    For l_lngFolders = LBound(m_feFolders) To UBound(m_feFolders)
        With m_feFolders(l_lngFolders)
            Do While l_lngBufferLength + Len(.Path) >= Len(l_strBuffer)
                l_strBuffer = l_strBuffer + Space(512)
            Loop
            Mid(l_strBuffer, l_lngBufferLength + 1, Len(.Path) + 2) = .Path + vbCrLf
            l_lngBufferLength = l_lngBufferLength + Len(.Path) + 2
        End With
    Next l_lngFolders
    List = Left(l_strBuffer, l_lngBufferLength - 2)
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = (UBound(m_feFolders) - LBound(m_feFolders)) + 1
End Property

Public Property Get Folder(ByVal Index As Long) As Fury2Folder
Attribute Folder.VB_UserMemId = 0
Attribute Folder.VB_MemberFlags = "200"
On Error Resume Next
Dim l_fldFolder As Fury2Folder
    If Index - 1 < LBound(m_feFolders) Or Index - 1 > UBound(m_feFolders) Then Exit Property
    Set l_fldFolder = New Fury2Folder
    l_fldFolder.SetData m_feFolders(Index - 1)
    Set Folder = l_fldFolder
End Property
