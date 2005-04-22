Attribute VB_Name = "mdlENet"
Option Explicit

Public Type ENetListNode
    PreviousNode As Long
    NextNode As Long
End Type

Public Type ENetList
    Sentinel As ENetListNode
End Type

Public Enum ENetSocketType
    ENET_SOCKET_TYPE_STREAM = 1
    ENET_SOCKET_TYPE_DATAGRAM = 2
End Enum

Public Enum ENetSocketWait
    ENET_SOCKET_WAIT_NONE = 0
    ENET_SOCKET_WAIT_SEND = 1
    ENET_SOCKET_WAIT_RECIEVE = 2
End Enum

Public Const ENET_HOST_ANY As Long = 0

Public Type ENetAddress
    IP(0 To 3) As Byte
    Port As Integer
End Type

Public Const ENET_PACKET_FLAG_RELIABLE As Long = 1

Public Type ENetPacket
    ReferenceCount As Long ' Internal use only
    Flags As Long ' Unsigned
    pData As Long ' Byte
    DataLength As Long ' size_t
End Type

Public Type ENetAcknowledgement
    AcknowledgementList As ENetListNode
    SentTime As Long ' Unsigned
'    Command As ENetProtocol
End Type

Public Type ENetOutgoingCommand
    OutgoingCommandList As ENetListNode
    ReliableSequenceNumber As Long ' Unsigned
    UnreliableSequenceNumber As Long ' Unsigned
    SentTime As Long ' Unsigned
    RoundTripTimeout As Long ' Unsigned
    RoundTripTimeoutLimit As Long ' Unsigned
    FragmentOffset As Long ' Unsigned
    FragmentLength As Long ' Unsigned
'    Command As ENetProtocol
'    pPacket As Long ' ENetPacket
End Type

Public Type ENetIncomingCommand
    IncomingCommandList As ENetListNode
    ReliableSequenceNumber As Long ' Unsigned
    UnreliableSequenceNumber As Long ' Unsigned
'    Command As ENetProtocol
'    FragmentCount As Long ' Unsigned
'    FragmentsRemaining As Long ' Unsigned
'    pFragments As Long ' Unsigned Long
'    pPacket As Long ' ENetPacket
End Type

Public Enum ENetPeerState
    ENET_PEER_STATE_DISCONNECTED = 0
    ENET_PEER_STATE_CONNECTING = 1
    ENET_PEER_STATE_ACKNOWLEDGING_CONNECT = 2
    ENET_PEER_STATE_CONNECTED = 3
    ENET_PEER_STATE_DISCONNECTING = 4
    ENET_PEER_STATE_ACKNOWLEDGING_DISCONNECT = 5
    ENET_PEER_STATE_ZOMBIE = 6
End Enum

Public Const ENET_HOST_RECIEVE_BUFFER_SIZE As Long = 262144
Public Const ENET_HOST_BANDWIDTH_THROTTLE_INTERVAL  As Long = 1000
Public Const ENET_HOST_DEFAULT_MTU As Long = 1400
Public Const ENET_PEER_DEFAULT_ROUND_TRIP_TIME As Long = 500
Public Const ENET_PEER_DEFAULT_PACKET_THROTTLE As Long = 32
Public Const ENET_PEER_PACKET_THROTTLE_SCALE As Long = 32
Public Const ENET_PEER_PACKET_THROTTLE_COUNTER As Long = 7
Public Const ENET_PEER_PACKET_THROTTLE_ACCELERATION As Long = 2
Public Const ENET_PEER_PACKET_THROTTLE_DECELERATION As Long = 2
Public Const ENET_PERR_PACKET_THROTTLE_INTERVAL As Long = 5000
Public Const ENET_PEER_PACKET_LOSS_SCALE As Long = 65536
Public Const ENET_PEER_PACKET_LOSS_INTERVAL As Long = 10000
Public Const ENET_PEER_WINDOW_SIZE_SCALE As Long = 65536
Public Const ENET_PEER_TIMEOUT_LIMIT As Long = 32
Public Const ENET_PEER_PING_INTERVAL As Long = 500

Public Type ENetChannel
    OutgoingReliableSequenceNumber As Long
    OutgoingUnreliableSequenceNumber As Long
    IncomingReliableSequenceNumber As Long
    IncomingUnreliableSequenceNumber As Long
    IncomingReliableCommands As ENetList
    IncomingUnreliableCommands As ENetList
End Type

Public Type ENetPeer
    pHost As Long ' ENetHost
    OutgoingPeerID As Integer
    IncomingPeerID As Integer
    Challenge As Long
    Address As ENetAddress
    pData As Long ' void
    State As ENetPeerState
    pChannels As Long ' ENetChannel
    ChannelCount As Long ' size_t
    IncomingBandwidth As Long
    OutgoingBandwidth As Long
    IncomingBandwidthThrottleEpoch As Long
    OutgoingBandwidthThrottleEpoch As Long
    IncomingDataTotal As Long
    OutgoingDataTotal As Long
    LastSendTime As Long
    LastReceiveTime As Long
    NextTimeout As Long
    PacketLossEpoch As Long
    PacketsSent As Long
    PacketsLost As Long
    PacketLoss As Long
    PacketLossVariance As Long
    PacketThrottle As Long
    PacketThrottleLimit As Long
    PacketThrottleCounter As Long
    PacketThrottleEpoch As Long
    PacketThrottleAcceleration As Long
    PacketThrottleDeceleration As Long
    PacketThrottleInterval As Long
    LastRoundTripTime As Long
    LowestRoundTripTime As Long
    LastRoundTripTimeVariance As Long
    HighestRoundTripTimeVariance As Long
    RoundTripTime As Long
    RoundTripTimeVariance As Long
    MTU As Integer
    WindowSize As Long
    ReliableDataInTransit As Long
    OutgoingReliableSequenceNumber As Long
    Acknowledgements As ENetList
    SentReliableCommands As ENetList
    SentUnreliableCommands As ENetList
    OutgoingReliableCommands As ENetList
    OutgoingUnreliableCommands As ENetList
End Type

Public Type ENetHost
    Socket As Long ' ENetSocket
    Address As ENetAddress
    IncomingBandwidth As Long
    OutgoingBandwidth As Long
    BandwidthThrottleEpoch As Long
    MTU As Long
    RecalculateBandwidthLimits As Long
    pPeers As Long ' ENetPeer
    PeerCount As Long ' size_t
    pLastServicedPeer As Long ' ENetPeer
    PacketSize As Long ' size_t
    ' --- STRUCTURE FROM HERE ON REQUIRES THE USE OF DARK C MAGICS I AM UNWILLING TO PARTAKE OF AT THE CURRENT TIME --
'    Commands(ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS) As ENetProtocol
'    CommandCount As Long ' size_t
'    Buffers(ENET_BUFFER_MAXIMUM) As ENetBuffer
'    BufferCount As Long ' size_t
'    RecievedAddress As ENetAddress
'    RecievedData(ENET_PROTOCOL_MAXIMUM_MTU) As Byte
'    RecievedDataLength as Long
End Type

Public Enum ENetEventType
    ENET_EVENT_TYPE_NONE = 0
    ENET_EVENT_TYPE_CONNECT = 1
    ENET_EVENT_TYPE_DISCONNECT = 2
    ENET_EVENT_TYPE_RECIEVE = 3
End Enum

Public Type ENetEvent
    Type As ENetEventType
    pPeer As Long ' ENetPeer
    ChannelID As Byte
    pPacket As Long ' ENetPacket
End Type

Public Type ENetBuffer
    Length As Long
    pData As Long ' void
End Type

Public Declare Function enet_initialize Lib "VBEnet" Alias "_enet_initialize@0" () As Long
Public Declare Function enet_deinitialize Lib "VBEnet" Alias "_enet_deinitialize@0" () As Long

Public Declare Function enet_time_get Lib "VBEnet" Alias "_enet_time_get@0" () As Long
Public Declare Sub enet_time_set Lib "VBEnet" Alias "_enet_time_set@4" (ByVal A As Long)

Public Declare Function enet_socket_create Lib "VBEnet" Alias "_enet_socket_create@8" (ByVal SocketType As ENetSocketType, ByRef Address As ENetAddress) As Long
Public Declare Function enet_socket_accept Lib "VBEnet" Alias "_enet_socket_accept@8" (ByVal Socket As Long, ByRef Address As ENetAddress) As Long
Public Declare Function enet_socket_connect Lib "VBEnet" Alias "_enet_socket_connect@8" (ByVal Socket As Long, ByRef Address As ENetAddress) As Long
Public Declare Function enet_socket_send Lib "VBEnet" Alias "_enet_socket_send@16" (ByVal Socket As Long, ByRef Address As ENetAddress, ByRef Buffer As ENetBuffer, ByVal A As Long) As Long
Public Declare Function enet_socket_recieve Lib "VBEnet" Alias "_enet_socket_recieve@16" (ByVal Socket As Long, ByRef Address As ENetAddress, ByRef Buffer As ENetBuffer, ByVal A As Long) As Long
Public Declare Function enet_socket_wait Lib "VBEnet" Alias "_enet_socket_wait@12" (ByVal Socket As Long, ByRef A As Long, ByVal B As Long) As Long
Public Declare Sub enet_socket_destroy Lib "VBEnet" Alias "_enet_socket_destroy@4" (ByVal Socket As Long)

Public Declare Function enet_address_set_host Lib "VBEnet" Alias "_enet_address_set_host@8" (ByRef Address As ENetAddress, ByVal HostName As String) As Long
Public Declare Function enet_address_get_host Lib "VBEnet" Alias "_enet_address_get_host@12" (ByRef Address As ENetAddress, HostName As Any, ByVal HostNameLength As Long) As Long

Public Declare Function enet_packet_create Lib "VBEnet" Alias "_enet_packet_create@12" (Data As Any, ByVal DataLength As Long, ByVal Flags As Long) As Long
Public Declare Sub enet_packet_destroy Lib "VBEnet" Alias "_enet_packet_destroy@4" (ByVal Packet As Long)
Public Declare Function enet_packet_resize Lib "VBEnet" Alias "_enet_packet_resize@8" (ByVal Packet As Long, ByVal DataLength As Long) As Long

Public Declare Function enet_host_create Lib "VBEnet" Alias "_enet_host_create@16" (ByRef Address As ENetAddress, ByVal PeerCount As Long, ByVal IncomingBandwidth As Long, ByVal OutgoingBandwidth As Long) As Long
Public Declare Function enet_host_create_client Lib "VBEnet" Alias "_enet_host_create@16" (ByVal Unused As Long, ByVal PeerCount As Long, ByVal IncomingBandwidth As Long, ByVal OutgoingBandwidth As Long) As Long
Public Declare Sub enet_host_destroy Lib "VBEnet" Alias "_enet_host_destroy@4" (ByVal Host As Long)
Public Declare Function enet_host_connect Lib "VBEnet" Alias "_enet_host_connect@12" (ByVal Host As Long, ByRef Address As ENetAddress, ByVal ChannelCount As Long) As Long
Public Declare Function enet_host_service Lib "VBEnet" Alias "_enet_host_service@12" (ByVal Host As Long, ByRef NewEvent As ENetEvent, ByVal Timeout As Long) As Long
Public Declare Sub enet_host_flush Lib "VBEnet" Alias "_enet_host_flush@4" (ByVal Host As Long)
Public Declare Sub enet_host_broadcast Lib "VBEnet" Alias "_enet_host_broadcast@12" (ByVal Host As Long, ByVal Channel As Byte, ByVal Packet As Long)
Public Declare Sub enet_host_bandwidth_limit Lib "VBEnet" Alias "_enet_host_bandwidth_limit@12" (ByVal Host As Long, ByVal IncomingBandwidth As Long, ByVal OutgoingBandwidth As Long)
Public Declare Sub enet_host_bandwidth_throttle Lib "VBEnet" Alias "_enet_host_bandwidth_throttle@4" (ByVal Host As Long)

Public Declare Function enet_peer_send Lib "VBEnet" Alias "_enet_peer_send@12" (ByVal Peer As Long, ByVal Channel As Byte, ByVal Packet As Long) As Long
Public Declare Function enet_peer_recieve Lib "VBEnet" Alias "_enet_peer_recieve@8" (ByVal Peer As Long, ByVal Channel As Byte) As Long
Public Declare Sub enet_peer_ping Lib "VBEnet" Alias "_enet_peer_ping@4" (ByVal Peer As Long)
Public Declare Sub enet_peer_reset Lib "VBEnet" Alias "_enet_peer_reset@4" (ByVal Peer As Long)
Public Declare Sub enet_peer_disconnect Lib "VBEnet" Alias "_enet_peer_disconnect@4" (ByVal Peer As Long)
Public Declare Sub enet_peer_disconnect_now Lib "VBEnet" Alias "_enet_peer_disconnect_now@4" (ByVal Peer As Long)
Public Declare Sub enet_peer_throttle_configure Lib "VBEnet" Alias "_enet_peer_throttle_configure@16" (ByVal Peer As Long, ByVal Interval As Long, ByVal Acceleration As Long, ByVal Deceleration As Long)
Public Declare Function enet_peer_throttle Lib "VBEnet" Alias "_enet_peer_throttle@8" (ByVal Peer As Long, ByVal A As Long) As Long
Public Declare Sub enet_reset_queues Lib "VBEnet" Alias "_enet_peer_reset_queues@4" (ByVal Peer As Long)
Public Declare Function enet_peer_queue_outgoing_command Lib "VBEnet" Alias "_enet_peer_queue_outgoing_command@20" (ByVal Peer As Long, ByVal Protocol As Long, ByVal Packet As Long, ByVal A As Long, ByVal B As Integer) As Long
Public Declare Function enet_peer_queue_incoming_command Lib "VBEnet" Alias "_enet_peer_queue_incoming_command@16" (ByVal Peer As Long, ByVal Protocol As Long, ByVal Packet As Long, ByVal A As Long) As Long
Public Declare Function enet_peer_queue_acknowledgement Lib "VBEnet" Alias "_enet_peer_queue_acknowledgement@12" (ByVal Peer As Long, ByVal Protocol As Long, ByVal A As Long) As Long

Public Declare Function vbenet_translate Lib "VBEnet" Alias "_vbenet_translate@4" (Data As Any) As Long
Public Declare Function vbenet_uint16_to_int32 Lib "VBEnet" Alias "_vbenet_uint16_to_int32@4" (ByVal Data As Integer) As Long
Public Declare Function vbenet_int32_to_uint16 Lib "VBEnet" Alias "_vbenet_int32_to_uint16@4" (ByVal Data As Long) As Integer

Public Function PerfCounter() As Double
On Error Resume Next
Dim PCounter As Currency, PFrequency As Currency
    Call QueryPerformanceFrequency(PFrequency)
    Call QueryPerformanceCounter(PCounter)
    PerfCounter = PCounter / PFrequency
End Function

