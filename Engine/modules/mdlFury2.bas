Attribute VB_Name = "mdlFury2"
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

Public m_booIDE As Boolean
Private m_lngLog As Long
'Public m_booEditor As Boolean
Public m_booTrace As Boolean
'Public m_booCritical As Boolean
'Public m_booShutdown As Boolean
Public DefaultEngine As Fury2Engine
'Public m_Engine As Fury2Engine
Public m_Globals As New Fury2Globals
Public SystemRoot As String
Public m_booMouseVisible As Boolean, m_booOldMouse As Boolean
Public g_lngMouseX As Single, g_lngMouseY As Single, g_lngMouseButtons As Long

Public Function FRound(ByVal Value As Single) As Long
On Error Resume Next
    FRound = Floor(Value + 0.5)
End Function

Function ValidizeFileName(Fn) As String
On Error Resume Next
Dim m_strText As String, m_strStrip As String
    m_strText = Trim(LCase(Fn))
    m_strStrip = Trim(LCase(App.Path))
    If Right(m_strStrip, 1) = "\" Then Else m_strStrip = m_strStrip + "\"
    If InStr(m_strText, m_strStrip) Then
        m_strText = Mid(m_strText, InStr(m_strText, m_strStrip) + Len(m_strStrip))
    End If
    m_strText = Replace(m_strText, " ", "_")
    m_strText = Replace(m_strText, ".", "")
    m_strText = Replace(m_strText, ",", "")
    m_strText = Replace(m_strText, "'", "")
    m_strText = Replace(m_strText, """", "")
    m_strText = Replace(m_strText, "\", "_")
    m_strText = Replace(m_strText, ":", "_")
    If Len(m_strText) > c_lngMaxNameLength Then
        m_strText = Right(m_strText, c_lngMaxNameLength)
    End If
    ValidizeFileName = m_strText
End Function

Function iMax(v1 As Long, v2 As Long) As Long
    iMax = IIf(v1 > v2, v1, v2)
End Function

Function iMin(v1 As Long, v2 As Long) As Long
    iMin = IIf(v1 < v2, v1, v2)
End Function

Sub ProgressUpdate(Amount As Single)
On Error Resume Next
'    If m_booEditor Then
'        Load frmProgress
'        frmProgress.Show
'        frmProgress.SetProgress Amount
'    End If
End Sub

Sub ProgressHide()
On Error Resume Next
'    If m_booEditor Then
'        Unload frmProgress
'    End If
End Sub

Sub LoadRect(ByVal FileHandle As Integer, ByRef Rct As Fury2Rect)
On Error Resume Next
Dim m_lngValue As Long
    Err.Clear
    If Rct Is Nothing Then Set Rct = New Fury2Rect
    Get #FileHandle, , m_lngValue
    Rct.Left = m_lngValue
    Get #FileHandle, , m_lngValue
    Rct.Top = m_lngValue
    Get #FileHandle, , m_lngValue
    Rct.Right = m_lngValue
    Get #FileHandle, , m_lngValue
    Rct.Bottom = m_lngValue
    Err.Clear
End Sub

Public Function DefaultPath() As String
    DefaultPath = "/"
End Function

Public Function RootPath() As String
    RootPath = SystemRoot
End Function

Public Function FixPath(ByVal Filename As String) As String
    Filename = Replace(Filename, "/", "\")
    If InStr(Filename, ":") Then
        FixPath = Filename
    ElseIf Left(Filename, 1) = "\" Then
        FixPath = Left(App.Path, 3) + Mid(Filename, 2)
    Else
        FixPath = App.Path + IIf(Right(App.Path, 1) = "\", "", "\") + Filename
    End If
End Function

Function InIDE() As Boolean
On Error Resume Next
    Err.Clear
    Debug.Assert 1 / 0
    If Err.Number <> 0 Then
        Err.Clear
        InIDE = True
    Else
        Err.Clear
    End If
End Function

Public Function vbMax(ParamArray Values() As Variant)
On Error Resume Next
Dim biggestValue, biggestIndex As Long, checkAll As Long
    biggestValue = 0
    For checkAll = LBound(Values) To UBound(Values)
        If Values(checkAll) > biggestValue Then biggestIndex = checkAll: biggestValue = Values(checkAll)
    Next checkAll
    vbMax = biggestValue
End Function

Public Sub SetFormSize(ByRef Frm As Form, X As Long, Y As Long, Optional Center As Boolean = False)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
Dim FrmWidth As Long, FrmHeight As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    FrmWidth = CLng(X * Screen.TwipsPerPixelX) + (BW * 2)
    FrmHeight = CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    If Center = True Then
        Frm.Move (Screen.Width - FrmWidth) \ 2, (Screen.Height - FrmHeight) \ 2, FrmWidth, FrmHeight
    Else
        Frm.Move Frm.Left, Frm.Top, FrmWidth, FrmHeight
    End If
    Frm.ScaleMode = OldMode
End Sub
'
'Public Sub PutString(ByVal FileHandle As Integer, ByVal Text As String)
'On Error GoTo psErrorHandler
'Dim TextArray() As Byte, TextLength As Long
'Dim TextStr As String, m_strSpot As String
'    m_strSpot = "entry"
'    Err.Clear
'    TextStr = CStr(Text)
'    m_strSpot = "copy"
'    Err.Clear
'    If Len(TextStr) <= 0 Then
'        TextLength = 0
'        Put #FileHandle, , TextLength
'    Else
'        m_strSpot = "Start save"
'        TextLength = Len(TextStr)
'        m_strSpot = "get length"
'        Err.Clear
'        Put #FileHandle, , TextLength
'        m_strSpot = "save length"
'        TextArray = StrConv(TextStr, vbFromUnicode)
'        m_strSpot = "convert text"
'        Put #FileHandle, , TextArray
'        m_strSpot = "save text"
'    End If
'    Err.Clear
'    Exit Sub
'
'psErrorHandler:
'    If Err.Number = 0 Then Resume Next
'    MsgBox "Critical Error:" + vbCrLf + "A client application tried to create a Fury² system object and then save it." + vbCrLf + "This will not work. You must create all objects you wish to save, using internal Fury² functions.", vbExclamation, "Error"
'    Err.Clear
'    Exit Sub
'End Sub
'
'Public Sub GetString(ByVal FileHandle As Integer, ByRef Text As String)
'On Error Resume Next
'Dim TextLength As Long, TextLength2 As Long
'Dim TextArray() As Byte
'    Get #FileHandle, , TextLength
'    If TextLength <= 0 Then
'        Text = ""
'    Else
'        ReDim TextArray(0 To TextLength - 1)
'        Get #FileHandle, , TextArray
'        Text = StrConv(TextArray, vbUnicode)
'    End If
'    Err.Clear
'End Sub

'Public Function OpenFile(Filename As String) As Long
'On Error Resume Next
'Dim FileHandle As Integer
'    FileHandle = FreeFile
'    Open Filename For Binary As #FileHandle
'    OpenFile = FileHandle
'End Function
'
'Public Sub CloseFile(ByRef FileHandle As Integer)
'On Error Resume Next
'    Close #FileHandle
'    FileHandle = 0
'End Sub

Public Function Ceil(Value As Single) As Integer
    If CSng(Value) > (Int(Value)) Then
        Ceil = Int(Value) + 1
    Else
        Ceil = Int(Value)
    End If
End Function

Public Sub SetTopmost(Window As Form, Topmost As Boolean)
On Error Resume Next
Dim lResult As Long
    If Topmost Then
        Window.ZOrder
        lResult = SetWindowPos(Window.hwnd, Topmost, 0, 0, 0, 0, NoMove Or NoSize)
        Window.ZOrder
    Else
        lResult = SetWindowPos(Window.hwnd, NotTopMost, 0, 0, 0, 0, NoMove Or NoSize)
    End If
End Sub

Public Sub SystemLogEvent(Text As String)
On Error Resume Next
    Debug.Print Text
End Sub
