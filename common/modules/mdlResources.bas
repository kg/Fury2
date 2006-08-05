Attribute VB_Name = "mdlResources"
Option Explicit
Public Const DefaultResLanguage As Integer = 0

Public Const DefaultResType As Long = 10

Public Const ReadBufferSize As Long = 32767

Public Type StringSet
    Strings() As String
End Type

Public Type ResourceEntry
    Language As Long
    Id As Long
    Type As Long
    Size As Long
    Data() As Byte
End Type

Public Type ResourceSet
    Resources() As ResourceEntry
End Type

Public Type PackManifest
    Handle As Long
    FileCount As Long
    FilesPacked As Long
    Manifest As String
    Filename As String
    Folder As String
End Type

Private Type EnumResourceNamesData
    Names() As String
    NameCount As Long
End Type

Private Type EnumResourceTypesData
    Types() As Long
    TypeCount As Long
End Type
 
Private Type FILETIME
    dwLowDate  As Long
    dwHighDate As Long
End Type

Private Type SYSTEMTIME
    wYear      As Integer
    wMonth     As Integer
    wDayOfWeek As Integer
    wDay       As Integer
    wHour      As Integer
    wMinute    As Integer
    wSecond    As Integer
    wMillisecs As Integer
End Type
'
'Public Type IconDir
'    Header As IconDirHeader
'    Entries() As IconDirEntry
'End Type
'
'Public Type IconDirHeader
'    wReserved As Integer
'    wType As Integer
'    wCount As Integer
'End Type
'
'Public Type IconDirEntry
'    bWidth As Byte
'    bHeight As Byte
'    bColorCount As Byte
'    bReserved As Byte
'    wPlanes As Integer
'    wBitCount As Integer
'    dwBytesInRes As Long
'    wId As Integer
'End Type

Public Const RT_CURSOR = 1&
Public Const RT_BITMAP = 2&
Public Const RT_ICON = 3&
Public Const RT_MENU = 4&
Public Const RT_DIALOG = 5&
Public Const RT_STRING = 6&
Public Const RT_FONTDIR = 7&
Public Const RT_FONT = 8&
Public Const RT_ACCELERATOR = 9&
Public Const RT_RCDATA = 10&
Public Const RT_MESSAGETABLE = 11&
Public Const RT_GROUP_CURSOR = 12&
Public Const RT_GROUP_ICON = 14&
Public Const RT_VERSION = 16&
Public Const RT_DLGINCLUDE = 17&
Public Const RT_PLUGPLAY = 19&
Public Const RT_VXD = 20&
Public Const RT_ANICURSOR = 21&
Public Const RT_ANIICON = 22&
Public Const RT_HTML = 23&

Private Const FILE_SHARE_READ = &H1
Private Const FILE_SHARE_WRITE = &H2
Private Const OPEN_ALWAYS As Long = 4
Private Const OPEN_EXISTING As Long = 3
Private Const TRUNCATE_EXISTING As Long = 5
Private Const GENERIC_WRITE As Long = &H40000000
  
Private Declare Function CreateFile Lib "kernel32" Alias _
   "CreateFileA" (ByVal lpFileName As String, _
   ByVal dwDesiredAccess As Long, _
   ByVal dwShareMode As Long, _
   ByVal lpSecurityAttributes As Long, _
   ByVal dwCreationDisposition As Long, _
   ByVal dwFlagsAndAttributes As Long, _
   ByVal hTemplateFile As Long) _
   As Long
   
Private Declare Function WriteFile Lib "kernel32" _
   (ByVal hFile As Long, _
   ByVal pData As Long, _
   ByVal dwCount As Long, _
   ByRef dwBytesWritten As Long, _
   ByVal pOverlapped As Long) As Long

Private Declare Function LocalFileTimeToFileTime Lib _
     "kernel32" (lpLocalFileTime As FILETIME, _
      lpFileTime As FILETIME) As Long

Private Declare Function SetFileTime Lib "kernel32" _
   (ByVal hFile As Long, ByVal MullP As Long, _
    ByVal NullP2 As Long, lpLastWriteTime _
    As FILETIME) As Long

Private Declare Function SystemTimeToFileTime Lib _
    "kernel32" (lpSystemTime As SYSTEMTIME, lpFileTime _
    As FILETIME) As Long
    
Private Declare Function CloseHandle Lib "kernel32" _
   (ByVal hObject As Long) As Long

Private Function EnumResourceLanguagesProc(ByVal hModule As Long, ByVal lpszType As Long, ByVal lpszName As Long, ByVal wLanguage As Integer, ByRef lParam As Long) As Long
On Error Resume Next
    If (wLanguage <> 0) Then
        lParam = CLng(wLanguage)
        EnumResourceLanguagesProc = 0
    Else
        EnumResourceLanguagesProc = 1
    End If
End Function

Private Function EnumResourceNamesProc(ByVal hModule As Long, ByVal lpszType As Long, ByVal lpszName As Long, ByRef lParam As EnumResourceNamesData) As Long
On Error Resume Next
Dim l_bytData() As Byte
Dim l_strName As String, l_lngNameLength As Long
    If (lpszName <= &HFFFF&) Then
        l_strName = "#" & CStr(lpszName)
    Else
        l_lngNameLength = StringLengthFromPointer(ByVal lpszName)
        ReDim l_bytData(0 To l_lngNameLength - 1)
        CopyMemory ByVal VarPtr(l_bytData(0)), ByVal lpszName, l_lngNameLength
        l_strName = StrConv(l_bytData, vbUnicode)
    End If
    lParam.NameCount = lParam.NameCount + 1
    If lParam.NameCount >= UBound(lParam.Names) Then
        ReDim Preserve lParam.Names(0 To UBound(lParam.Names) + 32)
    End If
    lParam.Names(lParam.NameCount - 1) = l_strName
    EnumResourceNamesProc = 1
End Function

Private Function EnumResourceTypesProc(ByVal hModule As Long, ByVal lpszType As Long, ByRef lParam As EnumResourceTypesData) As Long
On Error Resume Next
    lParam.TypeCount = lParam.TypeCount + 1
    If lParam.TypeCount >= UBound(lParam.Types) Then
        ReDim Preserve lParam.Types(0 To UBound(lParam.Types) + 32)
    End If
    lParam.Types(lParam.TypeCount - 1) = lpszType
    EnumResourceTypesProc = 1
End Function

Public Function ReadResources(ByVal hModule As Long, ByRef Out As ResourceSet) As Long
On Error Resume Next
Dim l_lngTypes() As Long
Dim l_strNames() As StringSet
Dim l_lngResource As Long, l_lngType As Long
Dim l_lngCount As Long, l_lngIndex As Long
Dim l_lngID As Long, l_strID As String
    l_lngTypes = EnumResourceTypes(hModule)
    ReDim l_strNames(LBound(l_lngTypes) To UBound(l_lngTypes))
    For l_lngType = LBound(l_lngTypes) To UBound(l_lngTypes)
        l_strNames(l_lngType).Strings = EnumResources(hModule, l_lngTypes(l_lngType))
        l_lngCount = l_lngCount + UBound(l_strNames(l_lngType).Strings) - LBound(l_strNames(l_lngType).Strings) + 1
    Next l_lngType
    ReDim Out.Resources(0 To l_lngCount - 1)
    l_lngType = 0
    For l_lngResource = 0 To l_lngCount - 1
        If (l_lngIndex > UBound(l_strNames(l_lngType).Strings)) Then
            l_lngIndex = 0
            l_lngType = l_lngType + 1
        End If
        l_strID = l_strNames(l_lngType).Strings(l_lngIndex)
        If Left(l_strID, 1) = "#" Then
            l_lngID = CLng(Mid(l_strID, 2))
            With Out.Resources(l_lngResource)
                .Id = l_lngID
                .Type = l_lngTypes(l_lngType)
                .Language = GetResourceLanguage(hModule, .Type, .Id)
                .Size = ReadResourceBytesEx(hModule, .Type, .Id, .Data)
                ReadResourceFileEx hModule, .Type, .Id, "C:\resource_" & .Type & "_" & .Id & ".bin"
            End With
        Else
        End If
        l_lngIndex = l_lngIndex + 1
    Next l_lngResource
End Function

Public Function EnumResources(ByVal hModule As Long, Optional ByVal ResType As Long = DefaultResType) As String()
On Error Resume Next
Dim l_datData As EnumResourceNamesData
    ReDim l_datData.Names(0 To 31)
    l_datData.NameCount = 0
    EnumResourceNamesID hModule, ResType, AddressOf EnumResourceNamesProc, ByVal VarPtr(l_datData)
    If (l_datData.NameCount = 0) Then
        ReDim l_datData.Names(0 To 0)
    Else
        ReDim Preserve l_datData.Names(0 To l_datData.NameCount - 1)
    End If
    EnumResources = l_datData.Names
End Function

Public Function EnumResourceTypes(ByVal hModule As Long) As Long()
On Error Resume Next
Dim l_datData As EnumResourceTypesData
    ReDim l_datData.Types(0 To 31)
    l_datData.TypeCount = 0
    Win32.EnumResourceTypes hModule, AddressOf EnumResourceTypesProc, ByVal VarPtr(l_datData)
    If (l_datData.TypeCount = 0) Then
        ReDim l_datData.Types(0 To 0)
    Else
        ReDim Preserve l_datData.Types(0 To l_datData.TypeCount - 1)
    End If
    EnumResourceTypes = l_datData.Types
End Function

Public Function GetResourceLanguage(ByVal hModule As Long, ByVal ResType As Long, ByVal ResID As Long) As Long
On Error Resume Next
Dim l_lngValue As Long
    Win32.EnumResourceLanguagesEx hModule, ResType, ResID, AddressOf EnumResourceLanguagesProc, ByVal VarPtr(l_lngValue)
    GetResourceLanguage = l_lngValue
End Function

Public Function ReadResourceText(ByVal hModule As Long, ByRef ResourceName As String) As String
On Error Resume Next
Dim l_lngResource As Long
Dim l_lngHandle As Long
Dim l_lngPointer As Long
Dim l_lngSize As Long
Dim l_strText As String
    ReadResourceText = vbNullString
    l_lngResource = FindResourceID(hModule, UCase(ResourceName), DefaultResType)
    If (l_lngResource <> 0) Then
        l_lngSize = SizeofResource(hModule, l_lngResource)
        l_lngHandle = LoadResource(hModule, l_lngResource)
        If (l_lngHandle <> 0) Then
            l_lngPointer = LockResource(l_lngHandle)
            If (l_lngPointer <> 0) Then
                l_strText = String(l_lngSize \ 2, ChrW(0))
                CopyMemory ByVal StrPtr(l_strText), ByVal l_lngPointer, l_lngSize
                ReadResourceText = l_strText
            End If
        End If
    End If
End Function

Public Function ReadResourceTextEx(ByVal hModule As Long, ByVal ResourceId As Long) As String
On Error Resume Next
Dim l_lngResource As Long
Dim l_lngHandle As Long
Dim l_lngPointer As Long
Dim l_lngSize As Long
Dim l_strText As String
    ReadResourceTextEx = vbNullString
    l_lngResource = FindResourceExplicit(hModule, DefaultResType, ResourceId, DefaultResLanguage)
    If (l_lngResource <> 0) Then
        l_lngSize = SizeofResource(hModule, l_lngResource)
        l_lngHandle = LoadResource(hModule, l_lngResource)
        If (l_lngHandle <> 0) Then
            l_lngPointer = LockResource(l_lngHandle)
            If (l_lngPointer <> 0) Then
                l_strText = String(l_lngSize \ 2, ChrW(0))
                CopyMemory ByVal StrPtr(l_strText), ByVal l_lngPointer, l_lngSize
                ReadResourceTextEx = l_strText
            End If
        End If
    End If
End Function

Public Function ReadResourceBytes(ByVal hModule As Long, ByRef ResourceName As String, ByRef Data() As Byte) As Long
On Error Resume Next
Dim l_lngResource As Long
Dim l_lngHandle As Long
Dim l_lngPointer As Long
Dim l_lngSize As Long
    ReadResourceBytes = 0
    l_lngResource = FindResourceID(hModule, UCase(ResourceName), DefaultResType)
    If (l_lngResource <> 0) Then
        l_lngSize = SizeofResource(hModule, l_lngResource)
        l_lngHandle = LoadResource(hModule, l_lngResource)
        If (l_lngHandle <> 0) Then
            l_lngPointer = LockResource(l_lngHandle)
            If (l_lngPointer <> 0) Then
                ReDim Data(0 To l_lngSize - 1)
                CopyMemory ByVal VarPtr(Data(0)), ByVal l_lngPointer, l_lngSize
                ReadResourceBytes = l_lngSize
            End If
        End If
    End If
End Function

Public Function ReadResourceBytesEx(ByVal hModule As Long, ByVal ResourceType As Long, ByVal ResourceId As Long, ByRef Data() As Byte) As Long
On Error Resume Next
Dim l_lngResource As Long
Dim l_lngHandle As Long
Dim l_lngPointer As Long
Dim l_lngSize As Long
    ReadResourceBytesEx = 0
    l_lngResource = FindResourceExplicit(hModule, ResourceType, ResourceId, DefaultResLanguage)
    If (l_lngResource <> 0) Then
        l_lngSize = SizeofResource(hModule, l_lngResource)
        l_lngHandle = LoadResource(hModule, l_lngResource)
        If (l_lngHandle <> 0) Then
            l_lngPointer = LockResource(l_lngHandle)
            If (l_lngPointer <> 0) Then
                ReDim Data(0 To l_lngSize - 1)
                CopyMemory ByVal VarPtr(Data(0)), ByVal l_lngPointer, l_lngSize
                ReadResourceBytesEx = l_lngSize
            End If
        End If
    End If
End Function

Public Function ReadResourceFileEx(ByVal hModule As Long, ByVal ResourceType As Long, ByVal ResourceId As Long, ByRef OutputFile As String, Optional ByVal NewDate As Date = #1/1/1900#) As Long
On Error Resume Next
Dim l_lngResource As Long
Dim l_lngHandle As Long
Dim l_lngPointer As Long
Dim l_lngSize As Long, l_lngReadSize As Long
Dim l_lngFile As Long
Dim l_lngPosition As Long
    ReadResourceFileEx = 0
    l_lngResource = FindResourceExplicit(hModule, ResourceType, ResourceId, DefaultResLanguage)
    If (l_lngResource <> 0) Then
        l_lngSize = SizeofResource(hModule, l_lngResource)
        If FileExists(OutputFile) Then
'            MsgBox "Old file is " & FileDateTime(OutputFile) & ", new file is " & NewDate
            If (FileDateTime(OutputFile) > NewDate) Then
                ReadResourceFileEx = FileLen(OutputFile)
                Exit Function
            End If
        End If
        l_lngHandle = LoadResource(hModule, l_lngResource)
        If (l_lngHandle <> 0) Then
            l_lngPointer = LockResource(l_lngHandle)
            If (l_lngPointer <> 0) Then
                WriteFileWithDate OutputFile, l_lngPointer, l_lngSize, NewDate
                ReadResourceFileEx = l_lngSize
            End If
        End If
    End If
End Function

Public Function ReadResourceFile(ByVal hModule As Long, ByRef ResourceName As String, ByRef OutputFile As String, Optional ByVal NewDate As Date = #1/1/1900#) As Long
On Error Resume Next
Dim l_lngResource As Long
Dim l_lngHandle As Long
Dim l_lngPointer As Long
Dim l_lngSize As Long, l_lngReadSize As Long
Dim l_lngFile As Long
Dim l_bytBuffer() As Byte
Dim l_lngPosition As Long
    ReadResourceFile = 0
    ReDim l_bytBuffer(0 To ReadBufferSize - 1)
    l_lngResource = FindResourceID(hModule, UCase(ResourceName), DefaultResType)
    If (l_lngResource <> 0) Then
        l_lngSize = SizeofResource(hModule, l_lngResource)
        If FileExists(OutputFile) Then
'            MsgBox "Old file is " & FileDateTime(OutputFile) & ", new file is " & NewDate
            If (FileDateTime(OutputFile) > NewDate) Then
                ReadResourceFile = FileLen(OutputFile)
                Exit Function
            End If
        End If
        l_lngHandle = LoadResource(hModule, l_lngResource)
        If (l_lngHandle <> 0) Then
            l_lngPointer = LockResource(l_lngHandle)
            If (l_lngPointer <> 0) Then
                WriteFileWithDate OutputFile, l_lngPointer, l_lngSize, NewDate
                ReadResourceFile = l_lngSize
            End If
        End If
    End If
End Function

#If ResPack Then
Public Function BeginWriteResources(ByRef Filename As String) As Long
On Error Resume Next
    BeginWriteResources = BeginUpdateResource(Filename, 1)
End Function

Public Function WriteResourceText(ByVal Handle As Long, ByRef ResourceName As String, ByRef Data As String) As Long
On Error Resume Next
Dim l_lngLength As Long
    l_lngLength = Len(Data)
    If l_lngLength Mod 2 = 1 Then
        l_lngLength = l_lngLength + 1
    End If
    WriteResourceText = UpdateResourceByType(Handle, DefaultResType, UCase(ResourceName), DefaultResLanguage, ByVal StrPtr(Data), l_lngLength * 2)
End Function

Public Function WriteResourceTextEx(ByVal Handle As Long, ByVal ResourceId As Long, ByRef Data As String) As Long
On Error Resume Next
Dim l_lngLength As Long
    l_lngLength = Len(Data)
    If l_lngLength Mod 2 = 1 Then
        l_lngLength = l_lngLength + 1
    End If
    WriteResourceTextEx = UpdateResourceByTypeAndID(Handle, DefaultResType, ResourceId, DefaultResLanguage, ByVal StrPtr(Data), l_lngLength * 2)
End Function

Public Function WriteResourceEntries(ByVal Handle As Long, ByRef Entries As ResourceSet, Optional ByVal ResType As Long = -1) As Long
On Error Resume Next
Dim l_lngEntries As Long
    WriteResourceEntries = 0
    For l_lngEntries = LBound(Entries.Resources) To UBound(Entries.Resources)
        With Entries.Resources(l_lngEntries)
            If (ResType = -1) Or (ResType = .Type) Then
                If WriteResourceEntry(Handle, Entries.Resources(l_lngEntries)) Then
                    WriteResourceEntries = WriteResourceEntries + 1
                End If
            End If
        End With
    Next l_lngEntries
End Function

Public Function ReadResourceEntryByType(ByRef Entries As ResourceSet, ByVal ResType As Long) As Byte()
On Error Resume Next
Dim l_lngEntries As Long
    For l_lngEntries = LBound(Entries.Resources) To UBound(Entries.Resources)
        With Entries.Resources(l_lngEntries)
            If (ResType = .Type) Then
                ReadResourceEntryByType = .Data
                Exit Function
            End If
        End With
    Next l_lngEntries
End Function

Public Function WriteResourceEntry(ByVal Handle As Long, ByRef Entry As ResourceEntry) As Long
On Error Resume Next
    WriteResourceEntry = UpdateResourceByTypeAndID(Handle, Entry.Type, Entry.Id, Entry.Language, ByVal VarPtr(Entry.Data(LBound(Entry.Data))), Entry.Size)
End Function

Public Function WriteResourceBytes(ByVal Handle As Long, ByRef ResourceName As String, ByRef Data() As Byte) As Long
On Error Resume Next
Dim l_lngLength As Long
    l_lngLength = UBound(Data) - LBound(Data) + 1
    WriteResourceBytes = UpdateResourceByType(Handle, DefaultResType, UCase(ResourceName), DefaultResLanguage, ByVal VarPtr(Data(LBound(Data))), l_lngLength)
End Function

Public Function WriteResourceBytesEx(ByVal Handle As Long, ByVal ResourceType As Long, ByVal ResourceId As Long, ByRef Data() As Byte, Optional ByVal Language As Long = DefaultResLanguage) As Long
On Error Resume Next
Dim l_lngLength As Long
    l_lngLength = UBound(Data) - LBound(Data) + 1
    WriteResourceBytesEx = UpdateResourceByTypeAndID(Handle, ResourceType, ResourceId, Language, ByVal VarPtr(Data(LBound(Data))), l_lngLength)
End Function

Public Function WriteResourceFile(ByVal Handle As Long, ByRef ResourceName As String, ByRef Filename As String) As Long
On Error Resume Next
Dim l_bytData() As Byte
Dim l_lngFile As Long
Dim l_lngLength As Long
    If Not FileExists(Filename) Then Exit Function
    l_lngFile = FreeFile
    Open Filename For Binary Access Read As #l_lngFile
        l_lngLength = LOF(l_lngFile)
        ReDim l_bytData(0 To l_lngLength - 1)
        Get #l_lngFile, 1, l_bytData
    Close #l_lngFile
    WriteResourceFile = UpdateResourceByType(Handle, DefaultResType, UCase(ResourceName), DefaultResLanguage, ByVal VarPtr(l_bytData(0)), l_lngLength)
End Function

Public Function WriteResourceFileEx(ByVal Handle As Long, ByVal ResourceType As Long, ByVal ResourceId As Long, ByRef Filename As String) As Long
On Error Resume Next
Dim l_bytData() As Byte
Dim l_lngFile As Long
Dim l_lngLength As Long
    If Not FileExists(Filename) Then Exit Function
    l_lngFile = FreeFile
    Open Filename For Binary Access Read As #l_lngFile
        l_lngLength = LOF(l_lngFile)
        ReDim l_bytData(0 To l_lngLength - 1)
        Get #l_lngFile, 1, l_bytData
    Close #l_lngFile
    WriteResourceFileEx = UpdateResourceByTypeAndID(Handle, ResourceType, ResourceId, DefaultResLanguage, ByVal VarPtr(l_bytData(0)), l_lngLength)
End Function

Public Function RemoveResource(ByVal Handle As Long, ByRef ResourceName As String) As Long
On Error Resume Next
    RemoveResource = UpdateResourceByType(Handle, DefaultResType, UCase(ResourceName), DefaultResLanguage, ByVal 0, 0)
End Function

Public Function CancelWriteResources(ByVal Handle As Long) As Long
On Error Resume Next
    CancelWriteResources = EndUpdateResource(Handle, 1)
End Function

Public Function FinishWriteResources(ByVal Handle As Long) As Long
On Error Resume Next
    FinishWriteResources = EndUpdateResource(Handle, 0)
End Function

Public Function ReadFileResources(ByRef Filename As String) As ResourceSet
On Error Resume Next
Dim l_lngModule As Long
Dim l_resResources As ResourceSet
    l_lngModule = LoadLibrary(Filename)
    If l_lngModule = 0 Then Exit Function
    ReadResources l_lngModule, l_resResources
    FreeLibrary l_lngModule
    ReadFileResources = l_resResources
End Function

Public Function BeginPackResources(ByRef Filename As String, ByRef Template As String) As PackManifest
On Error Resume Next
Dim l_manManifest As PackManifest
Dim l_strFilename As String
    l_strFilename = Replace(Filename, ".exe", "~.exe")
    Kill l_strFilename
    Err.Clear
    FileCopy Template, l_strFilename
    If Err <> 0 Then Exit Function
    l_manManifest.Handle = BeginWriteResources(l_strFilename)
    l_manManifest.FileCount = 0
    l_manManifest.FilesPacked = 0
    l_manManifest.Manifest = ""
    l_manManifest.Filename = Filename
    l_manManifest.Folder = Left(Filename, InStrRev(Filename, "\"))
    BeginPackResources = l_manManifest
End Function

Public Sub ManifestWrite(ByRef Manifest As PackManifest, ByRef KeyName As String, ByRef Value As String)
On Error Resume Next
    Manifest.Manifest = Manifest.Manifest & KeyName & "=" & Value & vbCrLf
End Sub

Public Sub PackResourceFile(ByRef Manifest As PackManifest, ByRef Filename As String, Optional ByRef OutPath As String = "")
On Error Resume Next
Dim l_strResName As String
Dim l_strOutPath As String
    l_strResName = Format(Manifest.FileCount, "00000")
    l_strOutPath = OutPath & Mid(Filename, InStrRev(Filename, "\") + 1)
    If WriteResourceFileEx(Manifest.Handle, DefaultResType, Manifest.FileCount, Filename) = 1 Then
        Manifest.FileCount = Manifest.FileCount + 1
        Manifest.FilesPacked = Manifest.FilesPacked + 1
        Manifest.Manifest = Manifest.Manifest & l_strResName & "|" & l_strOutPath & "|" & FileDateTime(Filename) & vbCrLf
    Else
    End If
    DoEvents
End Sub

Public Sub PackResourceFiles(ByRef Manifest As PackManifest, ByVal FilePattern As String, Optional ByVal OutPath As String = "", Optional ByVal EnableCompression As Boolean = True, Optional ByVal Recurse As Boolean = False, Optional ByVal Reserved As Long = 0)
On Error Resume Next
Dim l_strFilePath As String
Dim l_strFilename As String
Dim l_strPattern As String
Dim l_strFolder As String
Dim l_colFolders As Collection
Dim l_lngFolders As Long
Dim l_strOutPath As String
    l_strFilePath = Left(FilePattern, InStrRev(FilePattern, "\"))
    l_strPattern = Replace(FilePattern, l_strFilePath, "", , 1)
    l_strFilename = Dir(FilePattern, vbNormal)
#If ResZip Then
    If EnableCompression Then
        If Reserved = 0 Then
            l_strOutPath = OutPath
            OutPath = ""
            Kill Manifest.Folder & "data.zip"
        End If
        Err.Clear
        Dim l_zipZip As Zip
        Set l_zipZip = New Zip
        Do While Len(Trim(l_strFilename)) > 0
            Manifest.FilesPacked = Manifest.FilesPacked + 1
            l_zipZip.Pack l_strFilePath & l_strFilename, Manifest.Folder & "data.zip", True, "/" & Replace(OutPath, "\", "/"), 9
            l_strFilename = Dir
            DoEvents
        Loop
    Else
#End If
        Do While Len(Trim(l_strFilename)) > 0
            PackResourceFile Manifest, l_strFilePath & l_strFilename, OutPath
            l_strFilename = Dir
            DoEvents
        Loop
#If ResZip Then
    End If
#End If
    If Recurse Then
        Set l_colFolders = New Collection
        l_strFolder = Dir(l_strFilePath, vbDirectory)
        Do While Len(Trim(l_strFolder)) > 0
            If (GetAttr(l_strFilePath & l_strFolder) And vbDirectory) = vbDirectory Then
                If l_strFolder = "." Or l_strFolder = ".." Then
                Else
                    l_colFolders.Add l_strFolder
                End If
            End If
            l_strFolder = Dir(, vbDirectory)
        Loop
        If l_colFolders.Count > 0 Then
            For l_lngFolders = 1 To l_colFolders.Count
                l_strFolder = l_colFolders(l_lngFolders)
                PackResourceFiles Manifest, l_strFilePath & l_strFolder & "\" & l_strPattern, OutPath & l_strFolder & "\", EnableCompression, True, Reserved + 1
            Next l_lngFolders
        End If
    End If
#If ResZip Then
    If Reserved = 0 Then
        PackResourceFile Manifest, Manifest.Folder & "data.zip", l_strOutPath & "!unzip!"
        Kill Manifest.Folder & "data.zip"
        DoEvents
    End If
#End If
End Sub

Public Function FinishPackResources(ByRef Manifest As PackManifest) As Boolean
On Error Resume Next
    WriteResourceTextEx Manifest.Handle, 32766, Manifest.Manifest
    Manifest.Manifest = ""
    FinishWriteResources Manifest.Handle
    Manifest.Handle = 0
    Err.Clear
    Kill Manifest.Filename
    Name Replace(Manifest.Filename, ".exe", "~.exe") As Manifest.Filename
    If Err <> 0 Then Exit Function
    FinishPackResources = True
End Function
#End If

Public Function FileExists(ByRef Filename As String) As Boolean
On Error Resume Next
Dim l_lngLength As Long
    l_lngLength = -1
    Err.Clear
    l_lngLength = FileLen(Filename)
    If l_lngLength >= 0 Then
        FileExists = True
    Else
        FileExists = False
    End If
    Err.Clear
End Function

Public Sub WriteFileWithDate(ByVal Filename As String, ByVal pData As Long, ByVal DataLength As Long, Optional ByVal TheDate As Date = #1/1/1900#)
On Error Resume Next
Dim lFileHnd As Long
Dim lRet As Long
Dim lCount As Long
Dim lOpenFlag As Long
    
    If FileExists(Filename) Then
        lOpenFlag = TRUNCATE_EXISTING
    Else
        lOpenFlag = OPEN_ALWAYS
    End If

    lFileHnd = CreateFile(Filename, GENERIC_WRITE, _
        0, ByVal 0&, _
        lOpenFlag, 0, 0)
        
    If lFileHnd = 0 Then Exit Sub
    
    lRet = WriteFile(lFileHnd, pData, DataLength, lCount, 0)

    If TheDate > #1/1/1900# Then
        Dim typFileTime As FILETIME
        Dim typLocalTime As FILETIME
        Dim typSystemTime As SYSTEMTIME
        
        With typSystemTime
            .wYear = Year(TheDate)
            .wMonth = Month(TheDate)
            .wDay = Day(TheDate)
            .wDayOfWeek = Weekday(TheDate) - 1
            .wHour = Hour(TheDate)
            .wMinute = Minute(TheDate)
            .wSecond = Second(TheDate)
        End With
        
        lRet = SystemTimeToFileTime(typSystemTime, typLocalTime)
        lRet = LocalFileTimeToFileTime(typLocalTime, typFileTime)
            
        lRet = SetFileTime(lFileHnd, ByVal 0&, ByVal 0&, _
                 typFileTime)
    End If

    CloseHandle lFileHnd
End Sub

Public Function SetFileDateTime(ByVal Filename As String, ByVal TheDate As Date) As Boolean
On Error Resume Next
If Not FileExists(Filename) Then Exit Function
If Not IsDate(TheDate) Then Exit Function

Dim lFileHnd As Long
Dim lRet As Long

Dim typFileTime As FILETIME
Dim typLocalTime As FILETIME
Dim typSystemTime As SYSTEMTIME

With typSystemTime
    .wYear = Year(TheDate)
    .wMonth = Month(TheDate)
    .wDay = Day(TheDate)
    .wDayOfWeek = Weekday(TheDate) - 1
    .wHour = Hour(TheDate)
    .wMinute = Minute(TheDate)
    .wSecond = Second(TheDate)
End With

lRet = SystemTimeToFileTime(typSystemTime, typLocalTime)
lRet = LocalFileTimeToFileTime(typLocalTime, typFileTime)

lFileHnd = CreateFile(Filename, GENERIC_WRITE, _
    FILE_SHARE_READ Or FILE_SHARE_WRITE, ByVal 0&, _
    OPEN_EXISTING, 0, 0)

If lFileHnd = 0 Then Exit Function
    
lRet = SetFileTime(lFileHnd, ByVal 0&, ByVal 0&, _
         typFileTime)

CloseHandle lFileHnd
SetFileDateTime = lRet > 0

End Function
