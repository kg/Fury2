VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cRecentDocuments"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

'//
'// SHAddToRecentDocs
'//
Private Const SHARD_PIDL = &H1&
Private Const SHARD_PATHA = &H2&
Private Const SHARD_PATHW = &H3&

Private Declare Sub SHAddToRecentDocs Lib "shell32.dll" (ByVal uFlags As Long, pv As Any)

' // Recent docs location:
Private Const CSIDL_RECENT = &H8                      '(user name)\Recent
Private Declare Function SHGetSpecialFolderLocation Lib "shell32.dll" _
   (ByVal hWndOwner As Long, _
    ByVal nFolder As Long, _
    pidl As Long) As Long

Private Declare Function SHGetPathFromIDList Lib "shell32.dll" _
   Alias "SHGetPathFromIDListA" (ByVal pidl As Long, ByVal pszPath As String) As Long
#If ShellLinkEnabled Then
Private Declare Function SHGetMalloc Lib "shell32.dll" (ppMalloc As IMalloc) As Long
#End If
Private Declare Function GetFullPathName Lib "kernel32" Alias "GetFullPathNameA" (ByVal lpFileName As String, ByVal nBufferLength As Long, ByVal lpBuffer As String, lpFilePart As Any) As Long
Private Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As String) As Long

Private Const MAX_PATH = 260

#If ShellLinkEnabled Then
Private alloc As IMalloc
Private m_cLink() As cShellLink
Private m_iCount As Long
#End If

Private Declare Function GetVersion Lib "kernel32" () As Long

Private Function IsNt() As Boolean
Dim lVer As Long
   lVer = GetVersion()
   IsNt = ((lVer And &H80000000) = 0)
End Function

Public Sub AddToRecentDocs(ByVal sFile As String)
   
   If Len(sFile) > 0 Then
      If (IsNt) Then
         SHAddToRecentDocs SHARD_PATHW, ByVal StrPtr(sFile)
      Else
         SHAddToRecentDocs SHARD_PATHA, ByVal sFile
      End If
      If Not (Err.LastDllError = 0) Then
         Select Case Err.LastDllError
         Case 1008
            ' already exists
         Case 6
            ' already exists
         Case Else
            ' error
            Err.Raise Err.LastDllError, App.EXEName, "Failed to add the item: Windows Error " & Err.LastDllError
         End Select
      End If
   End If
   
End Sub

#If ShellLinkEnabled Then
Private Property Get Allocator() As IMalloc
    If alloc Is Nothing Then SHGetMalloc alloc
    Set Allocator = alloc
End Property

Public Property Get Path() As String
Dim pidl As Long
   ' Get pidl of special folder:
   SHGetSpecialFolderLocation 0, CSIDL_RECENT, pidl
   If Err = 0 Then
      On Error GoTo 0
      
      Dim sPath As String
      ' Convert it to a path:
      Path = PathFromPidl(pidl)
      ' Free the pidl:
      Allocator.Free ByVal pidl
      pidl = 0
   Else
      Err.Raise 45501, App.EXEName & ".cRecentDocuments", "Failed to get Recent Documents folder location"
   End If
   
End Property

Public Property Get Count() As Long
Dim sPath As String
   
   m_iCount = 0
   Erase m_cLink
   
   sPath = Path
   If Len(sPath) > 0 Then
      sPath = NormalizePath(sPath)
            
      Dim s As String
      s = Dir(sPath & "*.*")
      Do While Len(s) > 0
         If (s <> ".") And (s <> "..") Then
            m_iCount = m_iCount + 1
            ReDim Preserve m_cLink(1 To m_iCount) As cShellLink
            Set m_cLink(m_iCount) = New cShellLink
            m_cLink(m_iCount).Resolve GetLongPath(sPath & s)
         End If
         s = Dir
      Loop
   End If
   
   Count = m_iCount
   
End Property

Public Property Get Item(ByVal iIndex As Long) As cShellLink
   Set Item = m_cLink(iIndex)
End Property
#End If

Public Sub Clear()
   SHAddToRecentDocs 0, ByVal 0&
End Sub

Private Function PathFromPidl(ByVal pidl As Long) As String
Dim sPath As String
Dim lR As Long
   sPath = String$(MAX_PATH, 0)
   lR = SHGetPathFromIDList(pidl, sPath)
   If lR <> 0 Then
      PathFromPidl = Left$(sPath, lstrlen(sPath))
   End If
End Function
