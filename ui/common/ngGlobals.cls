VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngGlobals"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_colResourceCache As Collection

Public Function BindEvent(ByRef Obj As Object, ByRef Method As String, Optional ByRef Parameters = Nothing) As ngEvent
On Error Resume Next
Dim l_evtEvent As ngEvent
    Set l_evtEvent = New ngEvent
    l_evtEvent.Bind Obj, Method, Parameters
    Set BindEvent = l_evtEvent
End Function

Public Function LoadResourceFile(ByRef Filename As String) As ngResourceFile
On Error Resume Next
Dim l_szaArchive As SAWZipNG.Archive
Dim l_rfFile As ngResourceFile
    Set l_rfFile = m_colResourceCache(Filename)
    If l_rfFile Is Nothing Then
        Set l_szaArchive = New SAWZipNG.Archive
        Err.Clear
        Call l_szaArchive.Open(Filename, OM_READONLY)
        If Err <> 0 Then
            Err.Clear
            Exit Function
        End If
        If l_szaArchive.Closed Then
            Exit Function
        End If
        Set l_rfFile = New ngResourceFile
        Set l_rfFile.Archive = l_szaArchive
        l_rfFile.Filename = l_szaArchive.ArchivePath
        l_rfFile.Refresh
        m_colResourceCache.Add l_rfFile
    End If
    Set LoadResourceFile = l_rfFile
End Function

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colResourceCache = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
Dim l_rfFile As ngResourceFile
    For Each l_rfFile In m_colResourceCache
        l_rfFile.Free
    Next l_rfFile
    Set m_colResourceCache = Nothing
End Sub
