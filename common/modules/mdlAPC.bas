Attribute VB_Name = "mdlAPC"
Option Explicit
Public Declare Function GetCurrentThread Lib "kernel32" () As Long
Public Declare Function QueueUserAPC Lib "kernel32" (ByVal pFunc As Long, ByVal hThread As Long, ByRef lData As Long) As Long

Public QueuedCalls As New Collection
Public APCThread As Long

Public Sub APCall(Obj As Object, Method As String)
On Error Resume Next
Dim l_apcCall As APCCall
Dim l_strKey As String
    Set l_apcCall = New APCCall
    Set l_apcCall.Obj = Obj
    l_apcCall.MethodName = Method
    l_strKey = CStr(CLng(Rnd * 65535))
    QueuedCalls.Add l_apcCall, l_strKey
    Call QueueUserAPC(AddressOf APCProc, APCThread, CLng(l_strKey))
    SleepEx 1, True
End Sub

Public Sub APCProc(ByRef lData As Long)
On Error Resume Next
Dim l_apcCall As APCCall
Dim l_strKey As String
    l_strKey = CStr(lData)
    Set l_apcCall = QueuedCalls.Item(l_strKey)
    If l_apcCall Is Nothing Then
    Else
        l_apcCall.Exec
        QueuedCalls.Remove l_strKey
        Set l_apcCall = Nothing
    End If
    Err.Clear
End Sub
