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
Public m_booEditor As Boolean
Public m_booTrace As Boolean
Public m_booCritical As Boolean
Public m_booShutdown As Boolean
Public m_Engine As Fury2Engine
Public m_Globals As New Fury2Globals
Public m_Backbuffer As Fury2Image
Public m_GFX As Object
Public WindowIcon As IPictureDisp, AppIcon As IPictureDisp
Public SystemRoot As String
Public m_booMouseVisible As Boolean, m_booOldMouse As Boolean
Public g_lngMouseX As Single, g_lngMouseY As Single, g_lngMouseButtons As Long
Public m_Notify As Object, m_booNotifyNoLogging As Boolean
Public m_imgMouseBuffer As Fury2Image
Public LogItems As Fury2Collection

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
    If m_booEditor Then
        Load frmProgress
        frmProgress.Show
        frmProgress.SetProgress Amount
    End If
End Sub

Sub ProgressHide()
On Error Resume Next
    If m_booEditor Then
        Unload frmProgress
    End If
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

Sub ShowError(Error, Details)
On Error Resume Next
    #If DebugFeatures = 1 Then
    If m_Engine.Running Then Else Exit Sub
    If Trim(LCase(Details)) = "name redefined" Then Exit Sub
    m_Engine.ErrorOccurred = True
    LogEntry Error & vbCrLf & Details
    #End If
End Sub

Public Function DefaultPath() As String
    DefaultPath = "/"
End Function

Public Function LibraryPath() As String
    LibraryPath = "/library/"
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

Sub LogEntry(Optional Text)
On Error Resume Next
Dim l_strCBoard As String
Dim l_strText As String
    l_strText = CStr(Text)
    If m_Engine.LogToClipboard Then
        l_strCBoard = Clipboard.GetText
        Clipboard.Clear
        Clipboard.SetText l_strCBoard + vbCrLf + "> " + l_strText
    End If
    #If DebugFeatures = 1 Then
    If m_booShutdown Then Exit Sub
    If Not (LogItems Is Nothing) Then
        If InStr(l_strText, vbCrLf) Then
            Dim l_varLines As Variant
            Dim l_lngLines As Long
            l_varLines = Split(l_strText, vbCrLf)
            For l_lngLines = LBound(l_varLines) To UBound(l_varLines)
                LogItems.Add CStr(l_varLines(l_lngLines))
            Next l_lngLines
        Else
            LogItems.Add l_strText
        End If
        Do While LogItems.Count > 100
            LogItems.Remove 1
        Loop
    End If
    Debug.Print l_strText
    If (m_Notify Is Nothing) Or (m_booNotifyNoLogging) Then
    Else
        Err.Clear
        m_Notify.LogOutput l_strText
        If Err <> 0 Then m_booNotifyNoLogging = True
    End If
    #End If
End Sub

Sub Init(Optional Parameters As String = "")
On Error Resume Next
Dim Obj As Object
    Randomize Timer
    m_booMouseVisible = True
    
    If m_Engine.LoadGame(Parameters) Then
        MainLoop
    Else
        If m_booEditor Then
        Else
            MsgBox "Unable to load game!"
        End If
    End If
End Sub

Sub MainLoop()
On Error Resume Next
    m_GFX.Window.SetFocus
    If m_Engine.ErrorOccurred Then m_Engine.ShowConsole
    m_Engine.Cameras.Dirty
    m_Notify.Begin
    Do
        m_Engine.Game
        If m_Engine.TerminateEngine Then
            Exit Do
        ElseIf m_Engine.Terminating Then
            m_Engine.Terminating = False
            m_Engine.TextOut "Reloading Game"
            m_Engine.LoadGame m_Engine.CurrentGame
            m_Engine.Game
        Else
            Exit Do
        End If
    Loop
    Shutdown
End Sub

Sub Shutdown()
On Error Resume Next
Dim m_frmForm As Form
Dim m_lngForms As Long
Dim m_lngWait As Long
Dim Obj As Object
'    If m_booMultiplayerServer Then mdlMultiplayer.StopServer
    m_Engine.ScriptContext = "Shutdown"
    m_Engine.StopBGM
    m_Engine.SoundEngine.FreeAll
    If m_booIDE Then LogEntry "Ending Game"
    m_Engine.UnloadAllMaps
    m_Engine.ScriptEngine.Exec "Engine_Shutdown"
    For m_lngWait = 1 To 50
        SleepEx 1, True
        DoEvents
    Next m_lngWait
    m_booShutdown = True
    m_Engine.ReleaseScriptEngine
    m_GFX.HookEvents Nothing
    m_GFX.Shutdown
'    ShutdownSE
    Set m_GFX = Nothing
    Set m_Backbuffer = Nothing
    For Each Obj In m_Engine.EventHooks
        Obj.Shutdown
        Set Obj = Nothing
    Next Obj
    ShowCursor True
    Set m_Engine = Nothing
    For m_lngForms = Forms.Count To 0 Step -1
        Unload Forms(m_lngForms)
    Next m_lngForms
    F2Shutdown
    m_Notify.Quit
End Sub

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

Sub Main(Params As String)
On Error Resume Next
    F2Init
    Set m_Engine = New Fury2Engine
    m_booIDE = InIDE
    If Not (Trim(Params) = "") Then
        If Left(Params, 1) = """" Then Params = Mid(Params, 2)
        If Right(Params, 1) = """" Then Params = Left(Params, Len(Params) - 1)
    End If
    Randomize Timer
    Err.Clear
    Init Params
End Sub

Sub LoadGFXPlugin(Name)
On Error Resume Next
Dim m_TempObj As Object
    If m_GFX Is Nothing Then
    Else
        m_GFX.Shutdown
        Set m_GFX = Nothing
    End If
    If LCase(CStr(Trim(Name))) = "directdraw" Then
        Name = "DDraw"
    ElseIf LCase(CStr(Trim(Name))) = "direct3d" Then
        Name = "DX8"
    ElseIf LCase(CStr(Trim(Name))) = "d3d" Then
        Name = "DX8"
    End If
    Err.Clear
    Set m_TempObj = CreateObject("video_" + CStr(Trim(Name)) + "." + CStr(Trim(Name)) + "Engine")
    If Err.Number <> 0 Then
        Err.Clear
        Call RegisterServer(RootPath + "video_" + Name + ".dll", False)
        Call RegisterServer(RootPath + "video_" + Name + ".dll", True)
        Set m_TempObj = CreateObject("video_" + CStr(Trim(Name)) + "." + CStr(Trim(Name)) + "Engine")
        If Err.Number <> 0 Then MsgBox "Error while creating output plugin:" + vbCrLf + Err.Description + "(" + CStr(Err.Number) + ")"
    End If
    If m_TempObj Is Nothing Then Exit Sub
    Set m_GFX = m_TempObj
    m_GFX.HookEvents m_Engine
    Set m_TempObj = Nothing
End Sub

Public Function vbMax(ParamArray Values() As Variant)
On Error Resume Next
Dim biggestValue, biggestIndex As Long, checkAll As Long
    biggestValue = 0
    For checkAll = LBound(Values) To UBound(Values)
        If Values(checkAll) > biggestValue Then biggestIndex = checkAll: biggestValue = Values(checkAll)
    Next checkAll
    vbMax = biggestValue
End Function

Public Sub SetMousePos(ByRef Frm As Form, X, Y)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
Dim FrmWidth As Long, FrmHeight As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    Frm.ScaleMode = OldMode
    SetCursorPos (X * (Frm.ScaleWidth / m_Engine.ScreenWidth)) + (BW / Screen.TwipsPerPixelX) + (Frm.Left / Screen.TwipsPerPixelX), (Y * (Frm.ScaleHeight / m_Engine.ScreenHeight)) + ((BW + TH) / Screen.TwipsPerPixelY) + (Frm.Top / Screen.TwipsPerPixelY)
End Sub

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
        lResult = SetWindowPos(Window.HWnd, Topmost, 0, 0, 0, 0, NoMove Or NoSize)
        Window.ZOrder
    Else
        lResult = SetWindowPos(Window.HWnd, NotTopMost, 0, 0, 0, 0, NoMove Or NoSize)
    End If
End Sub

Public Sub CriticalError(Source As String, Location As String, Optional Description As String = "Unknown Error")
On Error Resume Next
Dim m_lngHandle As Long
Dim m_strError As String
    If m_booEditor Then
        If Err.Number <> 0 Then
            m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
            Err.Description + "(" + CStr(Err.Number) + ")" + vbCrLf + _
            "From: """ + Err.Source + """" + vbCrLf + _
            "In english: """ + Description + """" + vbCrLf + _
            "Context: " + m_Engine.ScriptContext + vbCrLf + _
            "Frames rendered: " + CStr(m_Engine.FrameCount)
        Else
            m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
            "In english: """ + Description + """" + vbCrLf + _
            "Context: " + m_Engine.ScriptContext + vbCrLf + _
            "Frames rendered: " + CStr(m_Engine.FrameCount)
        End If
'        MsgBox m_strError, vbInformation, "Error"
        Exit Sub
    End If
    If m_booIDE Then Stop
    m_booCritical = True
    If m_GFX.Fullscreen Then m_GFX.GoWindowed
    If Err.Number <> 0 Then
        m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
        Err.Description + "(" + CStr(Err.Number) + ")" + vbCrLf + _
        "From: """ + Err.Source + """" + vbCrLf + _
        "In english: """ + Description + """" + vbCrLf + _
        "Context: " + m_Engine.ScriptContext + vbCrLf + _
        "Frames rendered: " + CStr(m_Engine.FrameCount)
    Else
        m_strError = "Critical error in """ + Source + """ at """ + Location + """, Time: " + CStr(Now) + vbCrLf + _
        "In english: """ + Description + """" + vbCrLf + _
        "Context: " + m_Engine.ScriptContext + vbCrLf + _
        "Frames rendered: " + CStr(m_Engine.FrameCount)
    End If
    m_lngHandle = FreeFile
    Open App.Path + IIf(Right(App.Path, 1) = "\", "", "\") + "error.wtf" For Append As #m_lngHandle
    Print #m_lngHandle, m_strError
    Close #m_lngHandle
    m_Engine.Mouse.Visible = True
    Load frmCriticalError
    frmCriticalError.Show
    SetTopmost frmCriticalError, True
    frmCriticalError.lblInfo.Caption = m_strError
    frmCriticalError.SetFocus
    DoEvents
    Do While m_booCritical
        DoEvents
        Sleep (1)
    Loop
End Sub

Public Sub SystemLogEvent(Text As String)
On Error Resume Next
    Debug.Print Text
End Sub
