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
Private m_strFolders() As String
Private m_fsFilesystem As Fury2Filesystem

Public Sub Sort(Optional Ascending As Boolean)
On Error Resume Next
    Call QuickSort(m_strFolders, 0, UBound(m_strFolders), IIf(Ascending, 1, -1))
End Sub

Private Sub QuickSort(ByRef Arr() As String, ByVal l As Long, ByVal r As Long, ByVal flag As Integer)
    If (r <= l) Then Exit Sub
    Dim i As Long
    Dim j As Long
    Dim temp As String
    Dim ret As Integer
    i = l - 1
    j = r

    Do
        Do
            i = i + 1
            ret = StrComp(Arr(i), Arr(r))
            ret = ret * flag
        Loop While (ret < 0)

        Do While (j > 0)
            j = j - 1
            ret = StrComp(Arr(j), Arr(r))
            ret = ret * flag
            If (ret <= 0) Then Exit Do
        Loop
        If (i > j) Then Exit Do
        temp = Arr(i)
        Arr(i) = Arr(j)
        Arr(j) = temp
    Loop
    temp = Arr(i)
    Arr(i) = Arr(r)
    Arr(r) = temp
    Call QuickSort(Arr, l, i - 1, flag)
    Call QuickSort(Arr, i + 1, r, flag)
End Sub

Friend Sub SetData(Folders() As String, FileSystem As Fury2Filesystem)
On Error Resume Next
    m_strFolders = Folders
    Set m_fsFilesystem = FileSystem
End Sub

Public Property Get List() As String
    List = Replace(Join(m_strFolders, vbCrLf), "·", "")
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = (UBound(m_strFolders) - LBound(m_strFolders)) + 1
End Property

Public Property Get Folder(Index As Long) As Fury2Folder
Attribute Folder.VB_UserMemId = 0
Attribute Folder.VB_MemberFlags = "200"
On Error Resume Next
Dim l_strFolder As String
Dim l_fdrFolder As Fury2Folder
    If Index - 1 < LBound(m_strFolders) Or Index - 1 > UBound(m_strFolders) Then Exit Property
    l_strFolder = m_strFolders(Index - 1)
    Set l_fdrFolder = New Fury2Folder
    If Left(l_strFolder, 1) = "·" Then
        l_fdrFolder.SetData Mid(l_strFolder, 2), m_fsFilesystem, 1
    Else
        l_fdrFolder.SetData l_strFolder, m_fsFilesystem, 0
    End If
    Set Folder = l_fdrFolder
End Property

Public Property Get Names() As Variant
On Error Resume Next
Dim l_strNames As Variant
Dim l_lngFolders As Long
    l_lngFolders = -1
    l_lngFolders = UBound(m_strFolders)
    If l_lngFolders = -1 Or Err <> 0 Then Exit Property
    ReDim l_strNames(0 To UBound(m_strFolders))
    For l_lngFolders = LBound(m_strFolders) To UBound(m_strFolders)
        l_strNames(l_lngFolders) = Replace(m_strFolders(l_lngFolders), "·", "")
    Next l_lngFolders
    Names = l_strNames
End Property

