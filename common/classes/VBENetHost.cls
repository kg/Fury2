VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "VBENetHost"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Public Event Connect(ByVal Peer As VBENetPeer)
Public Event Recieve(ByVal Peer As VBENetPeer, ByVal Channel As Long, ByVal Packet As VBENetPacket)
Public Event Disconnect(ByVal Peer As VBENetPeer)

Public ENet As VBEnet
Public Tag As Variant
Private m_colPeers As Collection
Private m_lngHost As Long
Private m_adrAddress As VBENetAddress

Public Property Get Peers() As Collection
On Error Resume Next
    Set Peers = m_colPeers
End Property

Friend Function CreatePeerHandle(ByVal Handle As Long) As VBENetPeer
On Error Resume Next
Dim l_perPeer As VBENetPeer
    Set l_perPeer = New VBENetPeer
    l_perPeer.Create Me, Handle
    m_colPeers.Add l_perPeer, "p" & Handle
    Set CreatePeerHandle = l_perPeer
End Function

Friend Sub DispatchEvent(ByRef TheEvent As ENetEvent)
On Error Resume Next
Dim l_perPeer As VBENetPeer
Dim l_pktPacket As VBENetPacket
    Set l_perPeer = m_colPeers("p" & TheEvent.pPeer)
    If l_perPeer Is Nothing Then
        Set l_perPeer = CreatePeerHandle(TheEvent.pPeer)
    End If
    l_perPeer.Refresh
    Select Case TheEvent.Type
    Case ENET_EVENT_TYPE_CONNECT
        Debug.Print "Event: Connect"
        RaiseEvent Connect(l_perPeer)
    Case ENET_EVENT_TYPE_RECIEVE
        Debug.Print "Event: Recieve"
        Set l_pktPacket = New VBENetPacket
        l_pktPacket.Attach TheEvent.pPacket
        RaiseEvent Recieve(l_perPeer, TheEvent.ChannelID, l_pktPacket)
    Case ENET_EVENT_TYPE_DISCONNECT
        Debug.Print "Event: Disconnect"
        RaiseEvent Disconnect(l_perPeer)
        l_perPeer.Free
        m_colPeers.Remove "p" & TheEvent.pPeer
    Case Else
    End Select
End Sub

Public Sub Flush()
On Error Resume Next
    If m_lngHost = 0 Then Exit Sub
    enet_host_flush m_lngHost
End Sub

Public Function Poll(Optional ByVal Timeout As Long = 0) As Boolean
On Error Resume Next
Dim l_evtEvent As ENetEvent
    If m_lngHost = 0 Then Exit Function
    Do While enet_host_service(m_lngHost, l_evtEvent, Timeout)
        Poll = True
        DispatchEvent l_evtEvent
    Loop
End Function

Public Function Broadcast(ByVal Packet As VBENetPacket, Optional ByVal Channel As Long = 0) As Boolean
On Error Resume Next
    If m_lngHost = 0 Then Exit Function
    Debug.Print "Broadcasting packet..."
    enet_host_broadcast m_lngHost, Channel, Packet.Handle
    Packet.Owner = False
    Broadcast = True
End Function

Public Function BroadcastBytes(ByRef Data() As Byte, Optional ByVal Channel As Long = 0, Optional PacketFlags As Long = 0) As Boolean
On Error Resume Next
Dim l_lngPacket As Long, l_lngLength As Long
    If m_lngHost = 0 Then Exit Function
    l_lngLength = UBound(Data) - LBound(Data) + 1
    If l_lngLength < 1 Then Exit Function
    Debug.Print "Creating temporary packet..."
    l_lngPacket = enet_packet_create(Data(LBound(Data)), l_lngLength, PacketFlags)
    Debug.Print "Broadcasting temporary packet..."
    enet_host_broadcast m_lngHost, Channel, l_lngPacket
    BroadcastBytes = True
End Function

Public Sub Free()
On Error Resume Next
Dim l_perPeer As VBENetPeer
    For Each l_perPeer In m_colPeers
        l_perPeer.Free
    Next l_perPeer
    Set m_colPeers = New Collection
    If m_lngHost <> 0 Then
        enet_host_destroy m_lngHost
        m_lngHost = 0
    End If
End Sub

Friend Function Create(ByVal Address As VBENetAddress, ByVal ConnectionCount As Long, ByVal IncomingBandwidth As Long, ByVal OutgoingBandwidth As Long) As Boolean
On Error Resume Next
    Err.Clear
    If Address Is Nothing Then
        m_lngHost = enet_host_create_client(0, ConnectionCount, IncomingBandwidth, OutgoingBandwidth)
    Else
        m_lngHost = enet_host_create(Address.Address, ConnectionCount, IncomingBandwidth, OutgoingBandwidth)
    End If
    Create = (m_lngHost <> 0) And (Err.Number = 0)
End Function

Public Function Connect(ByVal Address As VBENetAddress, Optional ByVal ChannelCount As Long = 1) As VBENetPeer
On Error Resume Next
Dim l_lngPeer As Long
Dim l_perPeer As VBENetPeer
    If m_lngHost = 0 Then Exit Function
    If Address Is Nothing Then Exit Function
    If ChannelCount < 1 Then Exit Function
    Debug.Print "Connecting to """ & Address.Text & """..."
    l_lngPeer = enet_host_connect(m_lngHost, Address.Address, ChannelCount)
    If l_lngPeer = 0 Then
        ' Failed
        Exit Function
    Else
        ' Succeeded
        Set l_perPeer = CreatePeerHandle(l_lngPeer)
        Set Connect = l_perPeer
    End If
End Function

Public Property Get Address() As VBENetAddress
On Error Resume Next
    Set Address = m_adrAddress
End Property

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_lngHost
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colPeers = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub
