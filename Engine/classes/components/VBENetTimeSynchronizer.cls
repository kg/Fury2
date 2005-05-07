VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "VBENetTimeSynchronizer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Const c_lngSyncPacketCount As Long = 64
Public Event SyncBegin()
Public Event SyncUpdate(ByVal CurrentOffset As Double)
Public Event SyncComplete(ByVal CurrentOffset As Double, ByVal AverageOffset As Double)
Private m_bytHeader() As Byte
Private m_lngHeaderLength As Long
Private WithEvents m_hosServer As VBENetHost
Attribute m_hosServer.VB_VarHelpID = -1
Private WithEvents m_hosClient As VBENetHost
Attribute m_hosClient.VB_VarHelpID = -1
Private m_perClient As VBENetPeer
Attribute m_perClient.VB_VarHelpID = -1
Private m_booSynchronizing As Boolean
Private m_dblVirtualTime As Double
Private m_dblTimeOffset As Double
Private m_dblOffsetHistory() As Double
Private m_lngPacketCount As Long
Public BeginEvent As Fury2Event
Public ProgressEvent As Fury2Event
Public CompleteEvent As Fury2Event

Public Sub Free()
On Error Resume Next
    Set m_perClient = Nothing
    Set m_hosServer = Nothing
    Set m_hosClient = Nothing
    Set BeginEvent = Nothing
    Set ProgressEvent = Nothing
    Set CompleteEvent = Nothing
End Sub

Public Property Get TimeOffset() As Double
On Error Resume Next
    TimeOffset = m_dblTimeOffset
End Property

Private Sub Class_Initialize()
On Error Resume Next
    m_bytHeader = StrConv("TimeSync", vbFromUnicode)
    m_lngHeaderLength = UBound(m_bytHeader) - LBound(m_bytHeader) + 1
End Sub

Private Sub m_hosClient_Recieve(ByVal Peer As VBENetPeer, ByVal Channel As Long, ByVal Packet As VBENetPacket, ByRef Consume As Boolean)
On Error Resume Next
Dim l_pktPacket As VBENetPacket
Dim l_bytData() As Byte, l_lngData As Long, l_dblData As Double, l_dblNow As Double
Dim l_dblAverage As Double, l_lngIndex As Long
    If Not m_booSynchronizing Then Exit Sub
    If Packet.CheckHeader(m_bytHeader) Then
        Consume = True
        ' Sync packet from server
        l_dblNow = m_hosClient.Engine.HiTimer
        l_bytData = Packet.Data
        CopyMemory l_lngData, l_bytData(m_lngHeaderLength), 4
        CopyMemory l_dblData, l_bytData(m_lngHeaderLength + 4), 8
'        Debug.Print "Recieved sync packet from server, Latency " & l_lngData & ", Time Offset " & l_dblData
        m_dblTimeOffset = m_dblTimeOffset + l_dblData
        m_dblOffsetHistory(m_lngPacketCount) = m_dblTimeOffset
        m_lngPacketCount = m_lngPacketCount + 1
        RaiseEvent SyncUpdate(m_dblTimeOffset)
        If ProgressEvent Is Nothing Then
        Else
            ProgressEvent.Invoke m_lngPacketCount / c_lngSyncPacketCount
        End If
        If m_lngPacketCount < c_lngSyncPacketCount Then
            SendClientPacket
        Else
            For l_lngIndex = 0 To m_lngPacketCount - 1
                l_dblAverage = l_dblAverage + (m_dblOffsetHistory(l_lngIndex) / CDbl(m_lngPacketCount))
            Next l_lngIndex
            RaiseEvent SyncComplete(m_dblTimeOffset, l_dblAverage)
            If CompleteEvent Is Nothing Then
            Else
                CompleteEvent.Invoke m_dblTimeOffset
            End If
            m_booSynchronizing = False
        End If
    End If
End Sub

Private Sub m_hosServer_Connect(ByVal Peer As VBENetPeer)
On Error Resume Next
End Sub

Private Sub m_hosServer_Disconnect(ByVal Peer As VBENetPeer)
On Error Resume Next
End Sub

Private Sub m_hosServer_Recieve(ByVal Peer As VBENetPeer, ByVal Channel As Long, ByVal Packet As VBENetPacket, ByRef Consume As Boolean)
On Error Resume Next
Dim l_pktPacket As VBENetPacket
Dim l_bytData() As Byte, l_lngData As Long, l_dblData As Double, l_dblNow As Double
    If Packet.CheckHeader(m_bytHeader) Then
        Consume = True
        ' Sync packet from client
        l_dblNow = m_hosServer.Engine.HiTimer
        l_bytData = Packet.Data
        CopyMemory l_lngData, l_bytData(m_lngHeaderLength), 4
        CopyMemory l_dblData, l_bytData(m_lngHeaderLength + 4), 8
        SendServerPacket Peer, (l_dblNow - l_dblData)
    End If
End Sub

Public Sub AttachServer(ByRef Server As VBENetHost)
On Error Resume Next
    Set m_hosServer = Server
    Set m_hosClient = Nothing
    Set m_perClient = Nothing
End Sub

Public Sub AttachClient(ByRef ClientServerConnection As VBENetPeer)
On Error Resume Next
    Set m_hosServer = Nothing
    Set m_perClient = ClientServerConnection
    Set m_hosClient = m_perClient.Host
End Sub

Public Sub BeginSynchronization()
On Error Resume Next
    m_booSynchronizing = True
    m_dblTimeOffset = 0
    m_lngPacketCount = 0
    ReDim m_dblOffsetHistory(0 To c_lngSyncPacketCount - 1)
    RaiseEvent SyncBegin
    If BeginEvent Is Nothing Then
    Else
        BeginEvent.Invoke
    End If
    SendClientPacket
End Sub

Friend Sub SendClientPacket()
On Error Resume Next
Dim l_pktPacket As VBENetPacket
Dim l_bytData() As Byte, l_lngData As Long, l_dblData As Double
    m_perClient.Refresh
    Set l_pktPacket = m_hosClient.Engine.CreatePacket(m_lngHeaderLength + 12, True)
    l_bytData = l_pktPacket.Data
    CopyMemory l_bytData(0), m_bytHeader(0), m_lngHeaderLength
    l_lngData = m_perClient.Latency
    CopyMemory l_bytData(m_lngHeaderLength), l_lngData, 4
    l_dblData = m_hosClient.Engine.HiTimer + m_dblTimeOffset
    CopyMemory l_bytData(m_lngHeaderLength + 4), l_dblData, 8
    l_pktPacket.Data = l_bytData
    m_perClient.Send l_pktPacket
End Sub

Friend Sub SendServerPacket(ByVal Peer As VBENetPeer, ByVal TimeOffset As Double)
On Error Resume Next
Dim l_pktPacket As VBENetPacket
Dim l_bytData() As Byte, l_lngData As Long, l_dblData As Double
    m_perClient.Refresh
    Set l_pktPacket = m_hosServer.Engine.CreatePacket(m_lngHeaderLength + 12, True)
    l_bytData = l_pktPacket.Data
    CopyMemory l_bytData(0), m_bytHeader(0), m_lngHeaderLength
    l_lngData = Peer.Latency
    CopyMemory l_bytData(m_lngHeaderLength), l_lngData, 4
    l_dblData = TimeOffset
    CopyMemory l_bytData(m_lngHeaderLength + 4), l_dblData, 8
    l_pktPacket.Data = l_bytData
    Peer.Send l_pktPacket
End Sub
