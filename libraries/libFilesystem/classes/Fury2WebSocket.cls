VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2WebSocket"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private m_sckSocket As TCPSocket
Private m_booConnected As Boolean
Private m_strDataBuffer As String, m_lngDataWaiting As Long

Public Sub EraseData()
On Error Resume Next
    m_strDataBuffer = ""
    m_lngDataWaiting = 0
End Sub

Public Property Get Data() As String
    Data = m_strDataBuffer
End Property

Public Property Get DataWaiting() As Long
    DataWaiting = m_lngDataWaiting
End Property

Public Property Get Connected() As Boolean
    Connected = m_booConnected
End Property

Private Sub ProcessError()
On Error Resume Next
    MsgBox m_sckSocket.ErrorDescription & "(" & m_sckSocket.ErrorCode & ")"
    Disconnect
End Sub

Private Sub ProcessIncomingData()
On Error Resume Next
Dim l_strDataBuffer As String, l_lngAmountRecieved As Long
    If m_sckSocket.ReceiveData(l_strDataBuffer, l_lngAmountRecieved) Then
        m_strDataBuffer = m_strDataBuffer & l_strDataBuffer
        m_lngDataWaiting = m_lngDataWaiting + l_lngAmountRecieved
    End If
End Sub

Public Function EscapeString(ByVal Text As String) As String
On Error Resume Next
Dim l_lngChars As Long
Dim l_strChar As String * 1, l_intChar As Integer
    If Len(Text) = 0 Then Exit Function
    For l_lngChars = 1 To Len(Text)
        l_strChar = Mid(Text, l_lngChars, 1)
        l_intChar = AscW(l_strChar)
    Next l_lngChars
End Function

Public Function SendData(Data As String) As Boolean
On Error Resume Next
    SendData = m_sckSocket.SendData(Data)
End Function

Public Function Request(Path As String, Optional RequestType As String = "GET", Optional HTTPVersion As String = "1.0") As Boolean
On Error Resume Next
    Request = SendData(RequestType & " " & EscapeString(Path) & " " & HTTPVersion & vbCrLf & vbCrLf)
End Function

Public Sub Poll()
On Error Resume Next
    If Not m_booConnected Then Exit Sub
    If m_sckSocket.ErrorCode <> 0 Then
        ProcessError
    Else
        ProcessIncomingData
    End If
    DoEvents
End Sub

Public Sub connect(Host, Port)
'On Error Resume Next
    If Connected Then Disconnect
    m_sckSocket.LocalHostIP = "127.0.0.1"
    m_sckSocket.RemoteHostIP = "69.0.241.204"
    m_sckSocket.RemotePort = CLng(Port)
    m_sckSocket.OpenConnection
    MsgBox m_sckSocket.ErrorDescription & "(" & m_sckSocket.ErrorCode & ")"
    m_booConnected = True
End Sub

Public Sub Disconnect()
On Error Resume Next
    m_sckSocket.ShutdownConnection
    m_booConnected = False
    m_lngDataWaiting = 0
    m_strDataBuffer = ""
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set m_sckSocket = New TCPSocket
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Disconnect
    Set m_sckSocket = Nothing
End Sub
