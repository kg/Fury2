Attribute VB_Name = "mdlRecursiveSearch"
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

'    Based on the Recursive Search example by Randy Birch.
'    The original example is Copyright ©1996-2003 VBnet, Randy Birch

Private Const INVALID_HANDLE_VALUE As Long = -1
Private Const DOT As Long = 46
Private Const BACKSLASH As String * 1 = "\"

Public Type WIN32_FIND_DATA
    FileAttributes As Long
    CreationTime As FileTime
    LastAccessTime As FileTime
    LastWriteTime As FileTime
    FileSize As LargeInt
    Reserved As LargeInt
    Filename As String * Maximum_Path_Length
    AlternateFilename As String * 14
End Type

Private Declare Function FindClose Lib "kernel32" _
  (ByVal hFindFile As Long) As Long
   
Private Declare Function FindFirstFile Lib "kernel32" _
   Alias "FindFirstFileA" _
  (ByVal lpFileName As String, _
   ByRef lpFindFileData As WIN32_FIND_DATA) As Long
   
Private Declare Function FindNextFile Lib "kernel32" _
   Alias "FindNextFileA" _
  (ByVal hFindFile As Long, _
   lpFindFileData As WIN32_FIND_DATA) As Long

Public Function SearchForFiles(ByVal Path As String, ByVal PathAppend As String, ByRef Count As Long, ByVal Filter As String, ByVal Recursive As Boolean, ByRef Target() As FileEnumerationEntry) As Long
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim lCount As Long
Dim oldCount As Long
Dim sName As String
    If InStr(Filter, ";") Then
        Dim l_varFilters As Variant, l_lngFilters As Long
        l_varFilters = Split(Filter, ";")
        oldCount = Count
        lCount = Count
        For l_lngFilters = LBound(l_varFilters) To UBound(l_varFilters)
            SearchForFiles Path, PathAppend, lCount, CStr(l_varFilters(l_lngFilters)), Recursive, Target
        Next l_lngFilters
        Count = lCount
        SearchForFiles = lCount - oldCount
    End If
    hFile = FindFirstFile(Path & Filter, WFD)
    oldCount = Count
    lCount = Count
    
    If hFile <> INVALID_HANDLE_VALUE Then
        Do
            If (WFD.FileAttributes And vbHidden) Then
            ElseIf (WFD.FileAttributes And vbDirectory) Then
            Else
                sName = TrimNull(WFD.Filename)
                ' File
                If PathMatchSpec(sName, Filter) Then
                    lCount = lCount + 1
                    If (lCount - 1) > UBound(Target) Then
                        ReDim Preserve Target(LBound(Target) To ((UBound(Target) + 1) * 2) - 1)
                    End If
                    With Target(lCount - 1)
                        .Filename = Replace(PathAppend + sName, "\", "/")
                        .Location = Location_Disk
                        .LocationIndex = 0
                    End With
                End If
            End If
        Loop While FindNextFile(hFile, WFD)
    End If
    
    Call FindClose(hFile)
    
    If Recursive Then
        hFile = FindFirstFile(Path & "*.*", WFD)
        If hFile <> INVALID_HANDLE_VALUE Then
            Do
                If (WFD.FileAttributes And vbHidden) Then
                ElseIf (WFD.FileAttributes And vbDirectory) Then
                    sName = TrimNull(WFD.Filename)
                    ' Folder
                    If (Asc(sName) <> DOT) Then
                        ' Recurse
                        SearchForFiles Path & sName & "\", PathAppend & sName & "/", lCount, Filter, Recursive, Target
                    End If
                Else
                End If
            Loop While FindNextFile(hFile, WFD)
        End If
        Call FindClose(hFile)
    End If
    
    Count = lCount
    SearchForFiles = lCount - oldCount

End Function

Public Function SearchForFolders(ByVal Path As String, ByVal PathAppend As String, ByRef Count As Long, ByVal Recursive As Boolean, ByRef Target() As String) As Long
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim lCount As Long
Dim oldCount As Long
Dim sName As String, sFixedName As String
Dim bDuplicate As Boolean
Dim l_lngFolders As Long
    hFile = FindFirstFile(Path, WFD)
    oldCount = Count
    lCount = Count
    
    If hFile <> INVALID_HANDLE_VALUE Then
        Do
            If (WFD.FileAttributes And vbHidden) Then
            ElseIf (WFD.FileAttributes And vbDirectory) Then
                sName = TrimNull(WFD.Filename)
                If (Asc(sName) <> DOT) Then
                    sFixedName = Replace(PathAppend + sName, "\", "/")
                    bDuplicate = False
                    For l_lngFolders = 0 To lCount - 1
                        If (LCase(Target(l_lngFolders)) = LCase(sFixedName)) Or (LCase(Mid(Target(l_lngFolders), 2)) = LCase(sFixedName)) Then
                            bDuplicate = True
                            Exit For
                        End If
                    Next l_lngFolders
                    If Not bDuplicate Then
                        lCount = lCount + 1
                        If (lCount - 1) > UBound(Target) Then
                            ReDim Preserve Target(LBound(Target) To ((UBound(Target) + 1) * 2) - 1)
                        End If
                        Target(lCount - 1) = sFixedName
                    End If
                    If Recursive Then SearchForFolders Left(Path, InStrRev(Path, "\")) & sName & "\" & Mid(Path, InStrRev(Path, "\") + 1), PathAppend & sName & "/", lCount, Recursive, Target
                End If
            End If
        Loop While FindNextFile(hFile, WFD)
    End If
    
    Call FindClose(hFile)
    
    Count = lCount
    SearchForFolders = lCount - oldCount

End Function

Public Function CountFolders(ByVal Path As String) As Long
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim lCount As Long
Dim sName As String
    hFile = FindFirstFile(Path & "*.*", WFD)
    
    If hFile <> INVALID_HANDLE_VALUE Then
        Do
            If (WFD.FileAttributes And vbHidden) Then
            ElseIf (WFD.FileAttributes And vbDirectory) Then
                ' Folder
                If (Asc(WFD.Filename) <> DOT) Then
                    ' Recurse
                    lCount = lCount + CountFolders(Path & TrimNull(WFD.Filename) & "\") + 1
                End If
            Else
            End If
        Loop While FindNextFile(hFile, WFD)
    End If
    
    CountFolders = lCount
  
    Call FindClose(hFile)
End Function

Public Function CountFiles(ByVal Path As String) As Long
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim lCount As Long
Dim sName As String
    hFile = FindFirstFile(Path & "*.*", WFD)
    
    If hFile <> INVALID_HANDLE_VALUE Then
        Do
            If (WFD.FileAttributes And vbHidden) Then
            ElseIf (WFD.FileAttributes And vbDirectory) Then
                ' Folder
                If (Asc(WFD.Filename) <> DOT) Then
                    ' Recurse
                    lCount = lCount + CountFiles(Path & TrimNull(WFD.Filename) & "\")
                End If
            Else
                lCount = lCount + 1
            End If
        Loop While FindNextFile(hFile, WFD)
    End If
    
    CountFiles = lCount
  
    Call FindClose(hFile)
End Function

Public Function FileByIndex(ByVal Path As String, ByVal Index As Long, Optional ByRef lCount As Long = 0) As String
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim sName As String
    hFile = FindFirstFile(Path & "*.*", WFD)
    
    If hFile <> INVALID_HANDLE_VALUE Then
        Do
            If (WFD.FileAttributes And vbHidden) Then
            ElseIf (WFD.FileAttributes And vbDirectory) Then
                ' Folder
                If (Asc(WFD.Filename) <> DOT) Then
                    ' Recurse
                    FileByIndex = FileByIndex(Path & TrimNull(WFD.Filename) & "\", Index, lCount)
                    If FileByIndex <> vbNullString Then Exit Function
                End If
            Else
                If lCount = Index Then
                    FileByIndex = Path & TrimNull(WFD.Filename)
                    Exit Function
                End If
                lCount = lCount + 1
            End If
        Loop While FindNextFile(hFile, WFD)
    End If
    
    FileByIndex = vbNullString
  
    Call FindClose(hFile)
End Function

Private Function QualifyPath(sPath As String) As String
    If Right(sPath, 1) <> BACKSLASH Then
        QualifyPath = sPath & BACKSLASH
    Else
        QualifyPath = sPath
    End If
End Function

Private Function TrimNull(ByRef startstr As String) As String
    If Len(startstr) <= 0 Or StrPtr(startstr) < &HFF Or VarPtr(startstr) < &HFF Then Exit Function
    TrimNull = Left(startstr, StringLength(startstr))
End Function
