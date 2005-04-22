VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "VBENetAddress"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_adrAddress As ENetAddress

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = Text
End Function

Friend Property Let Address(ByRef NewAddress As ENetAddress)
On Error Resume Next
    m_adrAddress = NewAddress
End Property

Friend Property Get Address() As ENetAddress
On Error Resume Next
    Address = m_adrAddress
End Property

Public Property Get Host() As String
On Error Resume Next
Dim l_bytBuffer() As Byte, l_strBuffer As String
    ReDim l_bytBuffer(0 To 1023)
    Debug.Print "Reverse resolving address..."
    If enet_address_get_host(m_adrAddress, l_bytBuffer(0), 1023) = 0 Then
        l_strBuffer = StrConv(l_bytBuffer, vbUnicode)
        l_strBuffer = left(l_strBuffer, InStr(l_strBuffer, Chr(0)) - 1)
        Debug.Print "Address resolved to """ & l_strBuffer & """."
    Else
        Debug.Print "Address resolution failed."
    End If
End Property

Public Property Let Host(ByRef NewHost As String)
On Error Resume Next
Dim l_bytIP() As Byte
Dim l_lngAddress As Long
    If Len(Trim(NewHost)) < 1 Then
        l_lngAddress = ENET_HOST_ANY
        CopyMemory m_adrAddress.IP(0), l_lngAddress, 4
    Else
        Debug.Print "Resolving address """ & NewHost & """..."
        If enet_address_set_host(m_adrAddress, NewHost) = 0 Then
            l_bytIP = IP
            Debug.Print "Address resolved to " & l_bytIP(0) & "." & l_bytIP(1) & "." & l_bytIP(2) & "." & l_bytIP(3) & "."
        Else
            Debug.Print "Address resolution failed."
        End If
    End If
End Property

Public Property Let Text(ByRef NewText As String)
On Error Resume Next
Dim l_strNewPort As String
Dim l_strServer As String
Dim l_lngPos As Long
    l_lngPos = InStr(NewText, ":")
    If l_lngPos Then
        l_strNewPort = Mid(NewText, l_lngPos + 1)
        Port = CLng(l_strNewPort)
        l_strServer = left(NewText, l_lngPos - 1)
    Else
        l_strServer = NewText
    End If
    Host = l_strServer
End Property

Public Property Get Text() As String
On Error Resume Next
Dim l_bytIP() As Byte
Dim l_lngPort As Long
    l_bytIP = IP
    l_lngPort = Port
    Text = l_bytIP(0) & "." & l_bytIP(1) & "." & l_bytIP(2) & "." & l_bytIP(3) & ":" & l_lngPort
End Property

Public Property Get IP() As Byte()
On Error Resume Next
    IP = m_adrAddress.IP
End Property

Public Property Let IP(ByRef NewIP() As Byte)
On Error Resume Next
    If LBound(NewIP) <> 0 Or UBound(NewIP) <> 3 Then
        Err.Raise vbObjectError + 2, "VBENetAddress.IP", "IP must be an array of 4 bytes"
        Exit Property
    End If
    CopyMemory m_adrAddress.IP(0), NewIP(0), 4
End Property

Public Property Get Port() As Long
On Error Resume Next
    Port = vbenet_uint16_to_int32(m_adrAddress.Port)
End Property

Public Property Let Port(ByVal NewPort As Long)
On Error Resume Next
    m_adrAddress.Port = vbenet_int32_to_uint16(NewPort)
End Property
