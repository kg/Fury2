VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "VBENetPacket"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_booOwner As Boolean
Private m_lngPacket As Long
Private m_pktPacket As ENetPacket

Public Function GetFile() As VirtualFile
On Error Resume Next
Dim l_vfNew As VirtualFile
    Set l_vfNew = New VirtualFile
    l_vfNew.Data = Data
    Set GetFile = l_vfNew
End Function

Friend Sub Refresh()
On Error Resume Next
    If m_lngPacket = 0 Then Exit Sub
    CopyMemory m_pktPacket, ByVal m_lngPacket, Len(m_pktPacket)
End Sub

Public Property Get Reliable() As Boolean
On Error Resume Next
    Reliable = (m_pktPacket.Flags = 1)
End Property

Public Property Get Owner() As Boolean
On Error Resume Next
    Owner = m_booOwner
End Property

Friend Property Let Owner(ByVal NewValue As Boolean)
On Error Resume Next
    m_booOwner = NewValue
End Property

Friend Sub Create(ByVal Length As Long, ByVal Flags As Long)
On Error Resume Next
    If Length < 1 Then Exit Sub
    m_lngPacket = enet_packet_create(ByVal 0, Length, Flags)
    m_booOwner = True
    Refresh
End Sub

Friend Sub Attach(ByVal Packet As Long)
On Error Resume Next
    If Packet = 0 Then Exit Sub
    m_lngPacket = Packet
    m_booOwner = True
    Refresh
End Sub

Friend Property Get PrivateData() As ENetPacket
On Error Resume Next
    PrivateData = m_pktPacket
End Property

Public Property Get Length() As Long
On Error Resume Next
    Length = m_pktPacket.DataLength
End Property

Public Property Let Length(ByVal NewLength As Long)
On Error Resume Next
    If m_lngPacket = 0 Then
        Create NewLength, 0
    Else
        enet_packet_resize m_lngPacket, NewLength
        Refresh
    End If
End Property

Public Property Get DataPointer() As Long
On Error Resume Next
    DataPointer = m_pktPacket.pData
End Property

Public Property Get Text() As String
On Error Resume Next
    Text = Data
End Property

Public Property Get Data() As Byte()
On Error Resume Next
Dim l_bytData() As Byte
    If m_pktPacket.pData = 0 Then Exit Property
    ReDim l_bytData(0 To m_pktPacket.DataLength - 1)
    CopyMemory l_bytData(0), ByVal m_pktPacket.pData, m_pktPacket.DataLength
    Data = l_bytData
End Property

Public Property Let Data(ByRef NewData() As Byte)
On Error Resume Next
Dim l_lngLength As Long
    l_lngLength = UBound(NewData) - LBound(NewData) + 1
    If l_lngLength < 1 Then Exit Property
    If m_pktPacket.pData = 0 Then Exit Property
    If l_lngLength <> m_pktPacket.DataLength Then
        Length = l_lngLength
    End If
    CopyMemory ByVal m_pktPacket.pData, NewData(LBound(NewData)), l_lngLength
End Property

Public Function CheckHeader(ByRef Header() As Byte) As Boolean
On Error Resume Next
Dim l_bytData() As Byte
Dim l_lngLength As Long
Dim l_lngIndex As Long
    l_lngLength = UBound(Header) - LBound(Header) + 1
    If l_lngLength < 1 Then Exit Function
    If m_pktPacket.pData = 0 Then Exit Function
    If l_lngLength > m_pktPacket.DataLength Then Exit Function
    ReDim l_bytData(0 To l_lngLength - 1)
    CopyMemory l_bytData(0), ByVal m_pktPacket.pData, l_lngLength
    For l_lngIndex = 0 To UBound(l_bytData)
        If l_bytData(l_lngIndex) <> Header(LBound(Header) + l_lngIndex) Then
            Exit Function
        End If
    Next l_lngIndex
    CheckHeader = True
End Function

Public Sub Free()
On Error Resume Next
    If m_lngPacket <> 0 Then
        If m_booOwner Then
            enet_packet_destroy m_lngPacket
        End If
        m_lngPacket = 0
        m_booOwner = False
    End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_lngPacket
End Property

