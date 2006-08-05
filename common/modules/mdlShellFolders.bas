Attribute VB_Name = "mdlShellFolders"
Option Explicit

Public Function GetShellFolderPath(ByVal Folder As ShellFolderIDs) As String
On Error Resume Next
Dim l_bytData() As Byte
Dim l_lngLength As Long
    ReDim l_bytData(0 To 511)
    If GetSpecialFolderPath(0, ByVal VarPtr(l_bytData(0)), Folder, 0) = 1 Then
        l_lngLength = StringLengthFromPointer(ByVal VarPtr(l_bytData(0)))
        ReDim Preserve l_bytData(0 To l_lngLength - 1)
        GetShellFolderPath = StrConv(l_bytData, vbUnicode)
    End If
End Function

