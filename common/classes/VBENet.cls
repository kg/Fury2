VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "VBENet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private m_booInitialized As Boolean

Public Property Get Initialized() As Boolean
On Error Resume Next
    Initialized = m_booInitialized
End Property

Public Function Initialize() As Boolean
On Error Resume Next
    If m_booInitialized Then Exit Function
    If enet_initialize() = 0 Then
        m_booInitialized = True
        Initialize = True
        Debug.Print "ENet Initialized."
    Else
        Exit Function
    End If
End Function

Public Function CreatePacket(Optional ByVal Length As Long = 0, Optional ByVal Reliable As Boolean = False) As VBENetPacket
On Error Resume Next
Dim l_pktPacket As VBENetPacket
    Set l_pktPacket = New VBENetPacket
    If Length > 0 Then
        l_pktPacket.Create Length, Abs(CLng(Reliable))
    End If
    Set CreatePacket = l_pktPacket
End Function

Public Function CreateAddress(ByRef Host As String, ByVal Port As Long) As VBENetAddress
On Error Resume Next
Dim l_adrAddress As VBENetAddress
    Set l_adrAddress = New VBENetAddress
    l_adrAddress.Host = Host
    l_adrAddress.Port = Port
    Set CreateAddress = l_adrAddress
End Function

Public Function CreateAddressFromString(ByRef Text As String) As VBENetAddress
On Error Resume Next
Dim l_adrAddress As VBENetAddress
    Set l_adrAddress = New VBENetAddress
    l_adrAddress.Text = Text
    Set CreateAddressFromString = l_adrAddress
End Function

Public Function CreateServerHost(ByVal Address As VBENetAddress, Optional ByVal ClientCount As Long = 32, Optional ByVal IncomingBandwidthLimit As Long = 0, Optional ByVal OutgoingBandwidthLimit As Long = 0) As VBENetHost
On Error Resume Next
Dim l_hosHost As VBENetHost
    Set l_hosHost = New VBENetHost
    Set l_hosHost.ENet = Me
    If l_hosHost.Create(Address, ClientCount, IncomingBandwidthLimit, OutgoingBandwidthLimit) Then
        Set CreateServerHost = l_hosHost
    End If
End Function

Public Function CreateClientHost(Optional ByVal ServerCount As Long = 1, Optional ByVal IncomingBandwidthLimit As Long = 0, Optional ByVal OutgoingBandwidthLimit As Long = 0) As VBENetHost
On Error Resume Next
Dim l_hosHost As VBENetHost
    Set l_hosHost = New VBENetHost
    Set l_hosHost.ENet = Me
    If l_hosHost.Create(Nothing, ServerCount, IncomingBandwidthLimit, OutgoingBandwidthLimit) Then
        Set CreateClientHost = l_hosHost
    End If
End Function

Public Sub Deinitialize()
On Error Resume Next
    If Not m_booInitialized Then Exit Sub
    m_booInitialized = False
    enet_deinitialize
    Debug.Print "ENet Deinitialized."
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    If Initialize() Then
        Err.Clear
    Else
        Err.Raise vbObjectError + 1, "ENet_Initialize", "ENet initialization failed"
    End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Deinitialize
End Sub
