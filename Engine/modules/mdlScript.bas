Attribute VB_Name = "mdlScript"
Option Explicit
Private Declare Function SuspendThread Lib "kernel32" (ByVal hThread As Long) As Long
Private Declare Function ResumeThread Lib "kernel32" (ByVal hThread As Long) As Long
Private Declare Function GetCurrentThread Lib "kernel32" () As Long
Private Declare Function DuplicateHandle Lib "kernel32" (ByVal hSourceProcessHandle As Long, ByVal hSourceHandle As Long, ByVal hTargetProcessHandle As Long, lpTargetHandle As Long, ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, ByVal dwOptions As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
Private Declare Function GetCurrentProcess Lib "kernel32" () As Long
Private Const DUPLICATE_SAME_ACCESS = &H2
Global g_lngThread As Long

Public Function GetMyThreadID() As Long
On Error Resume Next
Dim l_lngPseudoThread As Long
Dim l_lngThread As Long
Dim l_lngProcess As Long
    l_lngProcess = GetCurrentProcess()
    l_lngPseudoThread = GetCurrentThread()
    DuplicateHandle l_lngProcess, l_lngPseudoThread, l_lngProcess, l_lngThread, 0, 1, DUPLICATE_SAME_ACCESS
    GetMyThreadID = l_lngThread
End Function

Public Sub FreeThreadID(ID As Long)
On Error Resume Next
    If ID = 0 Then Exit Sub
    CloseHandle ID
End Sub

Public Sub StopThread(ID As Long)
On Error Resume Next
    If ID = 0 Then Exit Sub
    SuspendThread ID
End Sub

Public Sub ContinueThread(ID As Long)
On Error Resume Next
    If ID = 0 Then Exit Sub
    ResumeThread ID
End Sub
