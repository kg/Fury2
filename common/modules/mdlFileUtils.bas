Attribute VB_Name = "mdlFileUtils"
'
'    Engine (Fury² Game Creation System Runtime Engine)
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

Global Const g_strHeader = "Fury²"

Function ValidateFileHeader(ByRef File As VirtualFile)
On Error Resume Next
Dim m_strHdr As String
    m_strHdr = Space(Len(g_strHeader))
    File.Load m_strHdr
    ValidateFileHeader = (m_strHdr = g_strHeader)
End Function

Sub Save2DIntArray(ByRef Arr() As Integer, ByRef File As VirtualFile)
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    With File
    .WriteSegment_Begin
        Err.Clear
        m_lngWidth = UBound(Arr, 1) + 1
        m_lngHeight = UBound(Arr, 2) + 1
        If Err.Number <> 0 Then m_lngWidth = 0: m_lngHeight = 0
        .Save m_lngWidth
        .Save m_lngHeight
        .RawSave VarPtr(Arr(0, 0)), (m_lngWidth * m_lngHeight) * 2
    .WriteSegment_End
    End With
End Sub

Sub Load2DIntArray(ByRef Arr() As Integer, ByRef File As VirtualFile)
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    With File
    .ReadSegment_Begin
        .Load m_lngWidth
        .Load m_lngHeight
        If m_lngWidth <= 0 Or m_lngHeight <= 0 Then
            Erase Arr
        Else
            ReDim Arr(0 To m_lngWidth - 1, 0 To m_lngHeight - 1)
            .RawLoad VarPtr(Arr(0, 0)), (m_lngWidth * m_lngHeight) * 2
        End If
    .ReadSegment_End
    End With
End Sub

Sub Save2DByteArray(ByRef Arr() As Byte, ByRef File As VirtualFile)
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    With File
    .WriteSegment_Begin
        Err.Clear
        m_lngWidth = UBound(Arr, 1) + 1
        m_lngHeight = UBound(Arr, 2) + 1
        If Err.Number <> 0 Then m_lngWidth = 0: m_lngHeight = 0
        .Save m_lngWidth
        .Save m_lngHeight
        .RawSave VarPtr(Arr(0, 0)), (m_lngWidth * m_lngHeight)
    .WriteSegment_End
    End With
End Sub

Sub Load2DByteArray(ByRef Arr() As Byte, ByRef File As VirtualFile)
On Error Resume Next
Dim m_lngWidth As Long, m_lngHeight As Long
    With File
    .ReadSegment_Begin
        .Load m_lngWidth
        .Load m_lngHeight
        If m_lngWidth <= 0 Or m_lngHeight <= 0 Then
            Erase Arr
        Else
            ReDim Arr(0 To m_lngWidth - 1, 0 To m_lngHeight - 1)
            .RawLoad VarPtr(Arr(0, 0)), (m_lngWidth * m_lngHeight)
        End If
    .ReadSegment_End
    End With
End Sub

Function FileExists(ByRef Filename As String) As Boolean
On Error Resume Next
Dim l_lngLength As Long
    Err.Clear
    l_lngLength = -1
    l_lngLength = FileLen(Filename)
    If Err <> 0 Then Exit Function
    If l_lngLength < 0 Then Exit Function
    FileExists = True
End Function
