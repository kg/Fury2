VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngResourceFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Const c_dblMaxAge As Double = 10 * 60
Private Declare Function GetTempPath Lib "kernel32" Alias "GetTempPathA" (ByVal nBufferLength As Long, ByVal lpBuffer As String) As Long
Private m_colCollection As Collection
Private m_strFilename As String
'Private m_szaArchive As SAWZipNG.Archive
Private m_zipZip As Zip
Private m_dblOpenedAt As Double

Friend Sub Free()
On Error Resume Next
Dim l_resResource As ngResource
    Set m_zipZip = Nothing
    For Each l_resResource In m_colCollection
        l_resResource.Free
    Next l_resResource
    Set m_colCollection = Nothing
End Sub

Public Property Get Filename() As String
    Filename = m_strFilename
End Property

Friend Property Let Filename(ByRef NewValue As String)
    m_strFilename = NewValue
End Property

Friend Property Get Archive() As Zip
    Set Archive = m_zipZip
End Property

Friend Property Set Archive(ByVal NewValue As Zip)
    Set m_zipZip = NewValue
End Property

'Friend Property Get Archive() As SAWZipNG.Archive
'    Set Archive = m_szaArchive
'End Property
'
'Friend Property Set Archive(ByVal NewValue As SAWZipNG.Archive)
'    Set m_szaArchive = NewValue
'End Property

Friend Function ReadFile(ByRef Filename As String) As Byte()
On Error Resume Next
Dim l_lngIndex As Long
Dim l_strFilename As String, l_strPath As String
Dim l_lngLength As Long
Dim l_lngHandle As Long
Dim l_bytFile() As Byte
    If Len(Filename) < 1 Then Exit Function
    l_strPath = Space(256)
    l_lngLength = GetTempPath(255, l_strPath)
    If l_lngLength > 0 Then
        m_zipZip.UnPack m_strFilename, l_strPath, Replace(Filename, "\", "/")
        l_strPath = Left(l_strPath, l_lngLength)
        l_lngHandle = FreeFile()
        Open l_strPath & Filename For Binary Access Read As #l_lngHandle
            ReDim l_bytFile(0 To LOF(l_lngHandle) - 1)
            Get #l_lngHandle, 1, l_bytFile
        Close #l_lngHandle
        Kill l_strPath & Filename
        ReadFile = l_bytFile
    End If
End Function

Friend Function ExtractFile(ByRef Filename As String) As String
On Error Resume Next
Dim l_lngIndex As Long
Dim l_strFilename As String, l_strPath As String
Dim l_lngLength As Long
    If Len(Filename) < 1 Then Exit Function
    l_strPath = Space(256)
    l_lngLength = GetTempPath(255, l_strPath)
    If l_lngLength > 0 Then
        l_strPath = Left(l_strPath, l_lngLength)
        m_zipZip.UnPack m_strFilename, l_strPath, Replace(Filename, "\", "/")
        ExtractFile = l_strPath & Filename
    End If
End Function

Friend Sub Reopen()
On Error Resume Next
    m_dblOpenedAt = Timer
'    m_szaArchive.Close
'    m_szaArchive.Open m_strFilename, OM_READONLY
End Sub

Friend Sub Refresh()
On Error Resume Next
Dim l_lngFiles As Long
'Dim l_fiInfo As SAWZipNG.FileInfo
Dim l_fiInfo As XZip.Item
Dim l_itmFiles As XZip.Items
Dim l_resResource As ngResource
    m_dblOpenedAt = Timer
    For Each l_resResource In m_colCollection
        l_resResource.Free
    Next l_resResource
    Set m_colCollection = New Collection
    Set l_itmFiles = m_zipZip.Contents(m_strFilename)
    For l_lngFiles = 1 To l_itmFiles.Count
        Set l_resResource = New ngResource
        Set l_fiInfo = l_itmFiles.Item(l_lngFiles)
        If (l_fiInfo.Type = tFile) Then
            l_resResource.Filename = Replace(l_fiInfo.Path + l_fiInfo.Name, "/", "\")
            Select Case LCase(Mid(l_fiInfo.Name, InStrRev(l_fiInfo.Name, ".")))
            Case ".png", ".gif", ".jpg", ".jpeg", ".bmp", ".tga"
                l_resResource.ResourceType = rtyImage
            Case ".ico"
                l_resResource.ResourceType = rtyIcon
            Case ".txt"
                l_resResource.ResourceType = rtyText
            Case ".f2script", ".vbs", ".js", ".f2sf"
                l_resResource.ResourceType = rtyScript
            Case Else
            End Select
            l_resResource.SetParent Me
            m_colCollection.Add l_resResource, Replace(l_fiInfo.Path + l_fiInfo.Name, "/", "\")
        End If
    Next l_lngFiles
End Sub

Public Property Get ItemData(ByVal IndexOrKey) As Variant
On Error Resume Next
Dim l_resResource As ngResource
Dim l_varData As Variant
    Set l_resResource = Item(IndexOrKey)
    If Not (l_resResource Is Nothing) Then
        If l_resResource.ResourceType = rtyImage Then
            Set ItemData = l_resResource.Data
        Else
            ItemData = l_resResource.Data
        End If
    End If
End Property

Public Property Get Item(ByVal IndexOrKey) As ngResource
Attribute Item.VB_UserMemId = 0
On Error Resume Next
    If (Timer - m_dblOpenedAt) > c_dblMaxAge Then
        Refresh
    End If
    If VarType(IndexOrKey) = vbString Then
        IndexOrKey = CStr(IndexOrKey)
    End If
    Err.Clear
    Set Item = m_colCollection(IndexOrKey)
    Err.Clear
End Property

Public Property Get Count() As Long
On Error Resume Next
    If (Timer - m_dblOpenedAt) > c_dblMaxAge Then
        Refresh
    End If
    Count = m_colCollection.Count
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
On Error Resume Next
    Set NewEnum = m_colCollection.[_NewEnum]
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colCollection = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub
