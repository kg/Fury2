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

Private Type SYSTEMTIME
    wYear As Integer
    wMonth As Integer
    wDayOfWeek As Integer
    wDay As Integer
    wHour As Integer
    wMinute As Integer
    wSecond As Integer
    wMilliseconds As Integer
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
Private Declare Function FileTimeToLocalFileTime Lib "kernel32" _
                                                 (lpFileTime As FileTime, _
                                                  lpLocalFileTime As FileTime) As Long
                                                  
' Convert the FILETIME structure into a Date.
Private Function FileTimeToDate(ft As FileTime) As Date
' FILETIME units are 100s of nanoseconds.
Const TICKS_PER_SECOND = 10000000

Dim lo_time As Double
Dim hi_time As Double
Dim seconds As Double
Dim hours As Double
Dim the_date As Date
Dim localTime As FileTime
    FileTimeToLocalFileTime ft, localTime

    ' Get the low order data.
    If localTime.LowDateTime < 0 Then
        lo_time = 2 ^ 31 + (localTime.LowDateTime And &H7FFFFFFF)
    Else
        lo_time = localTime.LowDateTime
    End If

    ' Get the high order data.
    If localTime.HighDateTime < 0 Then
        hi_time = 2 ^ 31 + (localTime.HighDateTime And _
            &H7FFFFFFF)
    Else
        hi_time = localTime.HighDateTime
    End If

    ' Combine them and turn the result into hours.
    seconds = (lo_time + 2 ^ 32 * hi_time) / _
        TICKS_PER_SECOND
    hours = CLng(seconds / 3600)
    seconds = seconds - hours * 3600

    ' Make the date.
    the_date = DateAdd("h", hours, "1/1/1601 0:00 AM")
    the_date = DateAdd("s", seconds - 1, the_date)
    FileTimeToDate = the_date
End Function

Public Function SearchForFiles(ByVal Path As String, ByVal PathAppend As String, ByRef Count As Long, ByVal Filter As String, ByVal Recursive As Boolean, ByRef Target() As FileEnumerationEntry, Optional ByRef Parent As Fury2FSModule = Nothing) As Long
On Error Resume Next
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
                        ReDim Preserve Target(LBound(Target) To UBound(Target) + 16)
                    End If
                    With Target(lCount - 1)
                        .Filename = Replace(PathAppend + sName, "\", "/")
                        .Size = WFD.FileSize.High
                        .CreatedDate = FileTimeToDate(WFD.CreationTime)
                        .ModifiedDate = FileTimeToDate(WFD.LastWriteTime)
                        Set .Parent = Parent
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

Public Function SearchForFolders(ByVal Path As String, ByVal PathAppend As String, ByRef Count As Long, ByVal Recursive As Boolean, ByRef Target() As FolderEnumerationEntry, Optional ByRef Parent As Fury2FSModule = Nothing) As Long
On Error Resume Next
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
                        If (LCase(Target(l_lngFolders).Path) = LCase(sFixedName)) Or (LCase(Mid(Target(l_lngFolders).Path, 2)) = LCase(sFixedName)) Then
                            bDuplicate = True
                            Exit For
                        End If
                    Next l_lngFolders
                    If Not bDuplicate Then
                        lCount = lCount + 1
                        If (lCount - 1) > UBound(Target) Then
                            ReDim Preserve Target(LBound(Target) To UBound(Target) + 16)
                        End If
                        Target(lCount - 1).Path = sFixedName
                        Set Target(lCount - 1).Parent = Parent
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

Public Function FileByIndex(ByVal Path As String, ByVal Index As Long, Optional ByRef lCount As Long = 0) As FileEnumerationEntry
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim sName As String
Dim l_feFile As FileEnumerationEntry
    hFile = FindFirstFile(Path & "*.*", WFD)
    l_feFile.Filename = vbNullString
    
    If hFile <> INVALID_HANDLE_VALUE Then
        Do
            If (WFD.FileAttributes And vbHidden) Then
            ElseIf (WFD.FileAttributes And vbDirectory) Then
                ' Folder
                If (Asc(WFD.Filename) <> DOT) Then
                    ' Recurse
                    FileByIndex = FileByIndex(Path & TrimNull(WFD.Filename) & "\", Index, lCount)
                    If FileByIndex.Filename <> vbNullString Then Exit Function
                End If
            Else
                If lCount = Index Then
                    l_feFile.Filename = Path & TrimNull(WFD.Filename)
                    l_feFile.CreatedDate = FileTimeToDate(WFD.CreationTime)
                    l_feFile.ModifiedDate = FileTimeToDate(WFD.LastWriteTime)
                    l_feFile.Size = WFD.FileSize.High
                    FileByIndex = l_feFile
                    Call FindClose(hFile)
                    Exit Function
                End If
                lCount = lCount + 1
            End If
        Loop While FindNextFile(hFile, WFD)
    End If
    
    FileByIndex = l_feFile
  
    Call FindClose(hFile)
End Function

Public Function FolderByIndex(ByVal Path As String, ByVal Index As Long, Optional ByRef lCount As Long = 0) As FolderEnumerationEntry
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim sName As String
Dim l_feFolder As FolderEnumerationEntry
    hFile = FindFirstFile(Path & "*.*", WFD)
    l_feFolder.Path = vbNullString
    
    If hFile <> INVALID_HANDLE_VALUE Then
        Do
            If (WFD.FileAttributes And vbHidden) Then
            ElseIf (WFD.FileAttributes And vbDirectory) Then
                If (Asc(WFD.Filename) <> DOT) Then
                    ' Folder
                    If lCount = Index Then
                        l_feFolder.Path = Path & TrimNull(WFD.Filename)
                        FolderByIndex = l_feFolder
                        Call FindClose(hFile)
                        Exit Function
                    Else
                        lCount = lCount + 1
                        ' Recurse
                        FolderByIndex = FolderByIndex(Path & TrimNull(WFD.Filename) & "\", Index, lCount)
                        If FolderByIndex.Path <> vbNullString Then Exit Function
                    End If
                End If
            Else
            End If
        Loop While FindNextFile(hFile, WFD)
    End If
    
    FolderByIndex = l_feFolder
  
    Call FindClose(hFile)
End Function

Public Function FileByName(ByRef Path As String) As FileEnumerationEntry
Dim WFD As WIN32_FIND_DATA
Dim hFile As Long
Dim sName As String
Dim l_feFile As FileEnumerationEntry
    hFile = FindFirstFile(Path, WFD)
    l_feFile.Filename = vbNullString
    
    If hFile <> INVALID_HANDLE_VALUE Then
        l_feFile.Filename = Path
        l_feFile.CreatedDate = FileTimeToDate(WFD.CreationTime)
        l_feFile.ModifiedDate = FileTimeToDate(WFD.LastWriteTime)
        l_feFile.Size = WFD.FileSize.High
        FileByName = l_feFile
    End If
    
    FileByName = l_feFile
  
    Call FindClose(hFile)
End Function

Public Function FolderByName(ByRef Path As String) As FolderEnumerationEntry
On Error Resume Next
Dim l_feFolder As FolderEnumerationEntry
    l_feFolder.Path = Path
    FolderByName = l_feFolder
End Function

Private Function MatchName(ByRef Path As String, ByRef Filename As String, ByRef Match As String) As Boolean
On Error Resume Next
Dim l_strTarget As String
    l_strTarget = Path & Filename
    If Len(Match) > Len(l_strTarget) Then
    Else
        MatchName = LCase(Right(l_strTarget, Len(Match))) = LCase(Match)
    End If
End Function

Private Function QualifyPath(sPath As String) As String
On Error Resume Next
    If Right(sPath, 1) <> "\" Then
        QualifyPath = sPath & "\"
    Else
        QualifyPath = sPath
    End If
End Function

Private Function TrimNull(ByRef startstr As String) As String
On Error Resume Next
    If Len(startstr) <= 0 Or StrPtr(startstr) < &HFF Or VarPtr(startstr) < &HFF Then Exit Function
    TrimNull = Left(startstr, StringLength(startstr))
End Function

Public Function StartsWith(ByRef Text As String, ByRef Find As String) As Boolean
On Error Resume Next
    If Len(Find) > Len(Text) Then Exit Function
    StartsWith = Left(Text, Len(Find)) = Find
End Function

Public Function ReplaceFirst(ByRef Text As String, ByRef Find As String, ByRef ToReplace As String) As String
On Error Resume Next
    ReplaceFirst = Replace(Text, Find, ToReplace, , 1)
End Function
