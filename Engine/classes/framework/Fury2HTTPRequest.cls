VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2HTTPRequest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements Fury2Object
Implements IVirtualFileSaveable
Private m_sngProgress As Single
Private m_lngError As Long
Private m_strError As String
Private m_booBusy As Boolean
Private m_booDone As Boolean
Private m_strFilename As String
Private m_strText As String
Private m_strURL As String
Private WithEvents m_reqRequest As HTTPConnect
Attribute m_reqRequest.VB_VarHelpID = -1
Public ProgressEvent As Fury2Event
Public CompleteEvent As Fury2Event
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Property Get Length() As Long
On Error Resume Next
    Length = Len(m_strText)
End Property

Public Function LoadImage() As Fury2Image
On Error Resume Next
    SaveFile m_strFilename
    Set LoadImage = Engine.LoadImage(m_strFilename)
End Function

Public Sub SaveFile(Filename As String)
On Error Resume Next
Dim l_vfFile As New VirtualFile
    l_vfFile.Save Me
    Engine.Filesystem.File(Filename).Save l_vfFile
    Set l_vfFile = Nothing
End Sub

Public Property Get Filename() As String
On Error Resume Next
    Filename = m_strFilename
End Property

Public Property Get Text() As String
On Error Resume Next
    Text = m_strText
End Property

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = m_strText
End Function

Private Sub Class_Initialize()
On Error Resume Next
    Set m_reqRequest = New HTTPConnect
End Sub

Public Sub Go(ByRef URL As String, Optional ByVal Port As Long = 80)
On Error Resume Next
    m_strURL = URL
    m_strFilename = Mid(m_strURL, InStrRev(m_strURL, "/") + 1)
    Engine.HTTPRequestCount = Engine.HTTPRequestCount + 1
    m_reqRequest.FetchURL URL, Port, , , , , , Engine.HTTPRequestCount
End Sub

Public Property Get Error() As Long
On Error Resume Next
    Error = m_lngError
End Property

Public Property Get ErrorString() As String
On Error Resume Next
    ErrorString = m_strError
End Property

Public Property Get Done() As Boolean
On Error Resume Next
    Done = m_booDone
End Property

Public Property Get Busy() As Boolean
On Error Resume Next
    Busy = m_booBusy
End Property

Public Property Get Progress() As Single
On Error Resume Next
    Progress = m_sngProgress
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    File.WriteString m_strText
End Sub

Private Sub m_reqRequest_OnCompleted(URL As String, portIn As Long, RetValue As String, isBinary As Boolean, success As Boolean, uniqueID As String)
On Error Resume Next
Dim l_lngFileHandle As Long
    m_strFilename = Mid(m_strURL, InStrRev(m_strURL, "/") + 1)
    m_strURL = URL
    If isBinary Then
        m_reqRequest.SaveFile App.Path & "\http.tmp", True
        l_lngFileHandle = FreeFile
        Open App.Path & "\http.tmp" For Binary Access Read As #l_lngFileHandle
            m_strText = Space(LOF(l_lngFileHandle))
            Get #l_lngFileHandle, 1, m_strText
        Close #l_lngFileHandle
        Kill App.Path & "\http.tmp"
    Else
        m_strText = RetValue
    End If
    m_lngError = 0
    m_strError = ""
    m_booBusy = False
    m_booDone = True
    If Not (CompleteEvent Is Nothing) Then CompleteEvent.Invoke
    Set m_reqRequest = New HTTPConnect
End Sub

Private Sub m_reqRequest_OnError(ByVal Number As Integer, Description As String, ByVal Scode As Long, ByVal Source As String, ByVal HelpFile As String, ByVal HelpContext As Long, CancelDisplay As Boolean, uniqueID As String)
On Error Resume Next
    m_lngError = Number
    m_strError = Description
    m_booBusy = False
    m_booDone = True
    Set m_reqRequest = New HTTPConnect
End Sub

Private Sub m_reqRequest_OnPercentComplete(ByVal val As Integer, uniqueID As String)
On Error Resume Next
    m_sngProgress = CSng(val)
    If Not (ProgressEvent Is Nothing) Then ProgressEvent.Invoke m_sngProgress
End Sub
