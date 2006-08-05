VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cVersionInfo"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IVirtualFileSaveable

Private m_colValues As Fury2Collection
Private m_colKeys As Fury2Collection
Private m_booBrokenResource As Boolean

Private Const LanguageID As String = "040904B0"
Private Const HeaderString As String = "VS_VERSION_INFO"
Private Const SFIHeaderString As String = "StringFileInfo"
Private Const VFIHeaderString As String = "VarFileInfo"
Private Const TTHeaderString As String = "Translation"
Private Const TTConstant As Long = &H4B00409

Private Const VS_FFI_SIGNATURE As Long = &HFEEF04BD
Private Const VS_FFI_STRUCVERSION As Long = &H10000
Private Const VS_FFI_FILEFLAGSMASK As Long = &H3F&
Private Const VS_FF_DEBUG As Long = &H1
Private Const VS_FF_PRERELEASE As Long = &H2
Private Const VS_FF_PATCHED As Long = &H4
Private Const VS_FF_PRIVATEBUILD As Long = &H8
Private Const VS_FF_INFOINFERRED As Long = &H10
Private Const VS_FF_SPECIALBUILD As Long = &H20
Private Const VOS_UNKNOWN As Long = &H0
Private Const VOS_DOS As Long = &H10000
Private Const VOS_OS216 As Long = &H20000
Private Const VOS_OS232 As Long = &H30000
Private Const VOS_NT As Long = &H40000
Private Const VOS__BASE As Long = &H0
Private Const VOS__WINDOWS16 As Long = &H1
Private Const VOS__PM16 As Long = &H2
Private Const VOS__PM32 As Long = &H3
Private Const VOS__WINDOWS32 As Long = &H4
Private Const VOS_DOS_WINDOWS16 As Long = &H10001
Private Const VOS_DOS_WINDOWS32 As Long = &H10004
Private Const VOS_OS216_PM16 As Long = &H20002
Private Const VOS_OS232_PM32 As Long = &H30003
Private Const VOS_NT_WINDOWS32 As Long = &H40004
Private Const VFT_UNKNOWN As Long = &H0
Private Const VFT_APP As Long = &H1
Private Const VFT_DLL As Long = &H2
Private Const VFT_DRV As Long = &H3
Private Const VFT_FONT As Long = &H4
Private Const VFT_VXD As Long = &H5
Private Const VFT_STATIC_LIB As Long = &H7
Private Const VFT2_UNKNOWN As Long = &H0
Private Const VFT2_DRV_PRINTER As Long = &H1
Private Const VFT2_DRV_KEYBOARD As Long = &H2
Private Const VFT2_DRV_LANGUAGE As Long = &H3
Private Const VFT2_DRV_DISPLAY As Long = &H4
Private Const VFT2_DRV_MOUSE As Long = &H5
Private Const VFT2_DRV_NETWORK As Long = &H6
Private Const VFT2_DRV_SYSTEM As Long = &H7
Private Const VFT2_DRV_INSTALLABLE As Long = &H8
Private Const VFT2_DRV_SOUND As Long = &H9
Private Const VFT2_DRV_COMM As Long = &HA

Private Type VS_FIXEDFILEINFO
   dwSignature As Long
   dwStrucVersionl As Integer
   dwStrucVersionh As Integer
   dwFileVersionMSl As Integer
   dwFileVersionMSh As Integer
   dwFileVersionLSl As Integer
   dwFileVersionLSh As Integer
   dwProductVersionMSl As Integer
   dwProductVersionMSh As Integer
   dwProductVersionLSl As Integer
   dwProductVersionLSh As Integer
   dwFileFlagsMask As Long
   dwFileFlags As Long
   dwFileOS As Long
   dwFileType As Long
   dwFileSubtype As Long
   dwFileDateMS As Long
   dwFileDateLS As Long
End Type

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colValues = New Fury2Collection
    Set m_colKeys = New Fury2Collection
End Sub

Public Property Get Count() As Long
On Error Resume Next
    Count = m_colKeys.Count
End Property

Public Property Get Keys(ByVal Index As Long) As String
On Error Resume Next
    Keys = m_colKeys(Index)
End Property

Public Property Get Values(ByRef key As String) As String
On Error Resume Next
Dim l_lngIndex As Long
    Err.Clear
    Values = ""
    l_lngIndex = m_colKeys.Find(key)
    Values = m_colValues(l_lngIndex)
    If Err <> 0 Then
        Values = ""
    End If
End Property

Public Property Let Values(ByRef key As String, ByRef Value As String)
On Error Resume Next
    Add CStr(key), Value
End Property

Public Sub Add(ByRef key As String, ByRef Value As String)
On Error Resume Next
Dim l_lngKey As Long
    Remove key
    m_colKeys.Add key
    m_colValues.Add Value
End Sub

Public Sub Remove(ByRef key)
On Error Resume Next
Dim l_lngKey As Long
    Do
        l_lngKey = m_colKeys.Find(key)
        If l_lngKey < 1 Then Exit Do
        m_colKeys.Remove l_lngKey
        m_colValues.Remove l_lngKey
    Loop
End Sub

Private Function BuildFixedInfo() As VS_FIXEDFILEINFO
On Error Resume Next
Dim l_infInfo As VS_FIXEDFILEINFO
Dim l_strParts() As String
    With l_infInfo
        .dwSignature = VS_FFI_SIGNATURE
        .dwStrucVersionl = 0
        .dwStrucVersionh = 1
        l_strParts = Split(Split(Values("FileVersion"), " ")(0), ".")
        .dwFileVersionMSl = CLng(l_strParts(1))
        .dwFileVersionMSh = CLng(l_strParts(0))
        .dwFileVersionLSl = CLng(l_strParts(2))
        .dwFileVersionLSh = CLng(l_strParts(3))
        l_strParts = Split(Split(Values("ProductVersion"), " ")(0), ".")
        .dwProductVersionMSl = CLng(l_strParts(1))
        .dwProductVersionMSh = CLng(l_strParts(0))
        .dwProductVersionLSl = CLng(l_strParts(2))
        .dwProductVersionLSh = CLng(l_strParts(3))
        .dwFileFlagsMask = 0 ' VS_FFI_FILEFLAGSMASK
        .dwFileFlags = 0 ' VS_FF_PATCHED
        .dwFileOS = VOS__WINDOWS32
        .dwFileType = VFT_APP
        .dwFileSubtype = 0
        .dwFileDateMS = 0
        .dwFileDateLS = 0
    End With
    BuildFixedInfo = l_infInfo
End Function

Private Sub WriteStringEntry(ByRef File As VirtualFile, ByRef key As String, ByRef Value As String)
On Error Resume Next
Dim l_lngKeyLen As Long
Dim l_lngValueLen As Long
Dim l_lngLength As Long
Dim l_strValue As String, l_strKey As String
    If Len(Trim(key)) = 0 Then Exit Sub
    l_strKey = key & Chr(0)
    If (Len(l_strKey) Mod 2) = 0 Then
        l_strKey = l_strKey & Chr(0)
    End If
    l_strValue = Value & Chr(0)
    l_lngKeyLen = Len(l_strKey)
    l_lngValueLen = Len(l_strValue)
    l_lngLength = (2 * 3) + (l_lngKeyLen * 2) + (l_lngValueLen * 2)
    If (l_lngLength Mod 4) <> 0 Then
        l_strValue = l_strValue & Chr(0)
        l_lngLength = l_lngLength + 2
    End If
    With File
        .Save CInt(l_lngLength) ' length of block (in bytes)
        .Save CInt(l_lngValueLen) ' value length (in wchars)
        .Save CInt(1) ' text
        .WriteStringUnicode l_strKey
        .WriteStringUnicode l_strValue
    End With
End Sub

Private Sub ReadString(ByRef File As VirtualFile)
On Error Resume Next
Dim l_lngKeyLen As Long
Dim l_lngValueLen As Long
Dim l_intValueLength As Integer
Dim l_intLength As Integer
Dim l_strKey As String
Dim l_strValue As String
    With File
        .Load l_intLength
        If l_intLength = 0 Then Exit Sub ' Skip
        .Load l_intValueLength
        .Skip 2 ' type
        l_lngValueLen = l_intValueLength * 2
        If ((l_lngValueLen + (2 * 3)) >= l_intLength) Or (m_booBrokenResource) Then
            ' fucking invalid resources
            l_lngValueLen = l_intValueLength
            m_booBrokenResource = True
        End If
        l_lngKeyLen = l_intLength - (l_lngValueLen + (2 * 3))
        l_strKey = Trim(Replace(CStr(.LoadBytes(l_lngKeyLen)), ChrW(0), ""))
'        If (l_lngKeyLen Mod 4) <> 0 Then
'            .Skip 2 ' padding
'        End If
        l_strValue = .LoadBytes(l_lngValueLen - 2)
        .Skip 2 ' null terminator
        Add l_strKey, l_strValue
        Debug.Print l_strKey & "=" & l_strValue
    End With
End Sub

Private Sub ReadStringTable(ByRef File As VirtualFile)
On Error Resume Next
Dim l_intLength As Integer
Dim l_lngStartPos As Long, l_lngEndPos As Long
    With File
        Do
            l_lngStartPos = .Position
            .Load l_intLength
            If l_intLength <> 0 Then Exit Do
        Loop
        l_lngEndPos = l_lngStartPos + l_intLength
        .Skip 2 ' padding
        .Skip 2 ' type
        .Skip 16 ' language id
        .Skip 2 ' padding
        Do While (.Position < l_lngEndPos)
            ReadString File
        Loop
    End With
End Sub

Private Function BuildStringTable() As Byte()
On Error Resume Next
Dim l_filFile As VirtualFile
Dim l_lngKeys As Long
Dim l_strKey As String, l_strValue As String
    Set l_filFile = New VirtualFile
    With l_filFile
        .Save CInt(0) ' null
        .Save CInt(1) ' text
        .WriteStringUnicode LanguageID ' language id
        .Save CInt(0) ' padding
        If m_colKeys.Count > 0 Then
            For l_lngKeys = 1 To m_colKeys.Count
                l_strKey = Keys(l_lngKeys)
                l_strValue = Values(l_strKey)
                WriteStringEntry l_filFile, l_strKey, l_strValue
                'Debug.Print l_strKey & "=" & l_strValue
            Next l_lngKeys
        End If
    End With
    BuildStringTable = l_filFile.Data
End Function

Private Function BuildStringFileInfo() As Byte()
On Error Resume Next
Dim l_filFile As VirtualFile
Dim l_bytData() As Byte
    Set l_filFile = New VirtualFile
    With l_filFile
        .Save CInt(0) ' placeholder (sfi length)
        .Save CInt(0) ' null
        .Save CInt(1) ' text
        .WriteStringUnicode SFIHeaderString ' header
        .Save CInt(0) ' padding
        l_bytData = BuildStringTable()
        .Save CInt(UBound(l_bytData) - LBound(l_bytData) + 1) + 2 ' stringtable length
        .WriteData l_bytData ' stringtable
        .MoveCursor 1
        .Save CInt(l_filFile.Length) ' sfi length
    End With
    BuildStringFileInfo = l_filFile.Data
End Function

Private Function BuildTranslationTable() As Byte()
On Error Resume Next
Dim l_filFile As VirtualFile
Dim l_lngKeys As Long
Dim l_strKey As String, l_strValue As String
    Set l_filFile = New VirtualFile
    With l_filFile
        .Save CInt(4) ' size
        .Save CInt(0) ' binary
        .WriteStringUnicode TTHeaderString
        .Save CInt(0) ' null terminator
        .Save CInt(0) ' padding
        .Save CLng(TTConstant)
    End With
    BuildTranslationTable = l_filFile.Data
End Function

Private Function BuildVarFileInfo() As Byte()
On Error Resume Next
Dim l_filFile As VirtualFile
Dim l_bytData() As Byte
    Set l_filFile = New VirtualFile
    With l_filFile
        .Save CInt(0) ' placeholder (vfi length)
        .Save CInt(0) ' null
        .Save CInt(0) ' binary
        .WriteStringUnicode VFIHeaderString ' header
        l_bytData = BuildTranslationTable()
        .Save CInt(UBound(l_bytData) - LBound(l_bytData) + 1) + 2 ' transtable length
        .WriteData l_bytData ' transtable
        .MoveCursor 1
        .Save CInt(l_filFile.Length) ' vfi length
    End With
    BuildVarFileInfo = l_filFile.Data
End Function

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_intLength As Integer
Dim l_intValueLength As Integer
Dim l_intDataType As Integer
Dim l_lngSignature As Long
Dim l_infInfo As VS_FIXEDFILEINFO
Dim l_intBlockLength As Integer
Dim l_strHeader As String
Dim l_lngPos As Long
    m_booBrokenResource = False
    With File
        .Load l_intLength ' Data length
        .Load l_intValueLength ' Value length
        .Load l_intDataType ' Type of version resource (1=text, 0=binary)
        l_strHeader = .LoadBytes(Len(HeaderString) * 2)
        If l_strHeader <> HeaderString Then
            ' Invalid header text
            Debug.Assert False
            Exit Sub
        End If
        .Skip 4 ' padding
        .RawLoad VarPtr(l_infInfo), Len(l_infInfo) ' fixed info
        If l_infInfo.dwSignature <> VS_FFI_SIGNATURE Then
            Debug.Assert False
            Exit Sub
        End If
        Do
            If .EOF Then Exit Do
            .Load l_intBlockLength
            If l_intBlockLength <> 0 Then
                ' A block has been found
                .Skip 2 ' null
                .Skip 2 ' type
                l_lngPos = .Position
                l_strHeader = .LoadBytes(l_intBlockLength - 6)
                If InStr(l_strHeader, SFIHeaderString) = 1 Then
                    ' StringFileInfo
                    .Position = l_lngPos
                    .Skip Len(SFIHeaderString) * 2 ' header text
                    .Skip 2 ' padding
                    ReadStringTable File
                Else
                    ' We don't care about the contents of this block
                End If
            End If
        Loop
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_infInfo As VS_FIXEDFILEINFO
Dim l_bytSFI() As Byte, l_bytVFI() As Byte
    l_infInfo = BuildFixedInfo
    l_bytSFI = BuildStringFileInfo
    l_bytVFI = BuildVarFileInfo
    With File
        .Save CInt(0) ' placeholder
        .Save CInt(Len(l_infInfo)) ' sizeof(fixedfileinfo)
        .Save CInt(0) ' binary
        .WriteStringUnicode HeaderString ' header
        .Save CLng(0) ' padding
        .RawSave VarPtr(l_infInfo), Len(l_infInfo) ' fixed info
        ' no padding
        .WriteData l_bytVFI
        .WriteData l_bytSFI
        .MoveCursor 1
        .Save CInt(.Length)  ' file length
    End With
End Sub
