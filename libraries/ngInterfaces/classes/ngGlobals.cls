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

Public Function SelectFiles(Optional ByRef Filter As String = "All Files|*.*", Optional ByRef Title As String = "Open...", Optional ByVal MultiSelect As Boolean = True) As Variant
On Error Resume Next
Dim l_strFilename As String
Dim l_dlgDialog As New GCommonDialog
Dim l_lngItems As Long
Dim l_varResult As Variant, l_varReturn As Variant
    l_dlgDialog.VBGetOpenFileName l_strFilename, , True, MultiSelect, False, True, Filter, , , Title
    If InStr(l_strFilename, Chr(0)) Then
        l_strFilename = Left(l_strFilename, InStr(l_strFilename, Chr(0) & Chr(0)) - 1)
        l_varResult = Split(l_strFilename, Chr(0))
        If UBound(l_varResult) > 0 Then
            ReDim l_varReturn(0 To UBound(l_varResult) - 1)
            For l_lngItems = LBound(l_varResult) + 1 To UBound(l_varResult)
                l_varReturn(l_lngItems - 1) = l_varResult(0) + IIf(Right(l_varResult(0), 1) = "\", "", "\") + l_varResult(l_lngItems)
            Next l_lngItems
            l_varResult = l_varReturn
        End If
        SelectFiles = l_varResult
    Else
        SelectFiles = l_strFilename
    End If
End Function

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
End Function

Public Function EscapeAmpersands(ByRef Text As String) As String
On Error Resume Next
    EscapeAmpersands = Replace(Text, "&", "&&")
End Function

Public Function FixPath(ByRef Path As String) As String
On Error Resume Next
    FixPath = Trim(Path)
    If Right(FixPath, 1) <> "\" Then
        FixPath = FixPath & "\"
    End If
End Function

Public Function GetPath(ByRef Filename As String) As String
On Error Resume Next
Dim l_lngPos As Long
    If Right(Filename, 1) = "\" Then
        GetPath = GetPath(Left(Filename, Len(Filename) - 1))
        Exit Function
    End If
    l_lngPos = InStrRev(Filename, "\")
    If l_lngPos Then
        GetPath = Left(Filename, l_lngPos - 1)
    Else
        GetPath = ""
    End If
End Function

Public Function GetTitle(ByRef Filename As String) As String
On Error Resume Next
Dim l_lngPos As Long
    l_lngPos = InStrRev(Filename, "\")
    If l_lngPos Then
        GetTitle = Mid(Filename, l_lngPos + 1)
    Else
        GetTitle = Filename
    End If
End Function

Public Function GetExtension(ByRef Filename As String) As String
On Error Resume Next
Dim l_lngPos As Long, l_lngPos2 As Long
    l_lngPos2 = -1
    l_lngPos2 = InStrRev(Filename, "\")
    If l_lngPos2 < 1 Then
        l_lngPos = -1
        l_lngPos = InStrRev(Filename, ".")
    Else
        l_lngPos = l_lngPos2 + InStrRev(Mid(Filename, l_lngPos2), ".") - 1
    End If
    If l_lngPos > l_lngPos2 Then
        GetExtension = Mid(Filename, l_lngPos + 1)
    Else
        GetExtension = ""
    End If
End Function

Function SerializeObject(Obj As Object) As String
On Error Resume Next
Dim l_vfFile As VirtualFile
Dim l_strData As String, l_lngLength As Long
    Set l_vfFile = New VirtualFile
    
    l_vfFile.Save CLng(0)
    l_vfFile.WriteSegment_Begin
    l_vfFile.Save Obj
    l_vfFile.WriteSegment_End
    l_vfFile.Save CLng(1)
    
    l_strData = EncodeStr64(StrConv(l_vfFile.SaveToString, vbUnicode))
    l_lngLength = Len(l_strData)
    SerializeObject = "$object:" & LCase(Typename(Obj)) & "$" & vbCrLf & "$length:" & l_lngLength & "$" & vbCrLf & l_strData & vbCrLf & "$end$"
End Function

Function SerializeObject2(Obj As Object) As Byte()
On Error Resume Next
Dim l_vfFile As VirtualFile
    Set l_vfFile = New VirtualFile
    
    l_vfFile.Save CLng(0)
    l_vfFile.WriteSegment_Begin
    l_vfFile.Save Obj
    l_vfFile.WriteSegment_End
    l_vfFile.Save CLng(1)
    
    SerializeObject2 = l_vfFile.Data
End Function

Function GetSerializedObjectType(Data As String) As Variant
On Error Resume Next
Dim l_strType As String, l_lngPos As Long
    GetSerializedObjectType = Array("data", "unknown")
    l_lngPos = InStr(Data, vbCrLf)
    If l_lngPos > 0 Then
        l_strType = Left(Data, l_lngPos - 1)
        If Left(l_strType, 1) = "$" And Right(l_strType, 1) = "$" Then
            GetSerializedObjectType = Split(Mid(l_strType, 2, Len(l_strType) - 2), ":")
        End If
    Else
        Exit Function
    End If
End Function

Function DeserializeObject(Data As String, Obj As Object) As Boolean
On Error Resume Next
Dim l_vfFile As VirtualFile
Dim l_strData As String, l_lngLength As Long
Dim l_strType As String, l_lngPos As Long, l_strLength As String
Dim l_lngData As Long
    Set l_vfFile = New VirtualFile
    
    l_lngPos = InStr(Data, vbCrLf)
    If l_lngPos > 0 Then
        l_strType = Left(Data, l_lngPos - 1)
        If Left(l_strType, 1) = "$" And Right(l_strType, 1) = "$" Then
            l_strLength = Mid(Data, l_lngPos, InStr(l_lngPos + 2, Data, vbCrLf) - l_lngPos)
            l_lngLength = Split(Mid(l_strLength, 2, Len(l_strLength) - 2), ":")(1)
            l_lngPos = InStr(l_lngPos + 2, Data, vbCrLf)
            If l_lngPos > 0 Then
                l_strData = Mid(Data, l_lngPos + 2, l_lngLength)
            End If
        Else
            Exit Function
        End If
    Else
        Exit Function
    End If
    
    l_vfFile.LoadFromString StrConv(DecodeStr64(l_strData), vbFromUnicode)
    l_vfFile.Load l_lngData
    l_vfFile.ReadSegment_Begin
    l_vfFile.Load Obj
    l_vfFile.ReadSegment_End
    l_vfFile.Load l_lngData
    
    DeserializeObject = (l_lngData = 1)
End Function

Function DeserializeObject2(Data() As Byte, Obj As Object) As Boolean
On Error Resume Next
Dim l_vfFile As VirtualFile
Dim l_lngData As Long
    Set l_vfFile = New VirtualFile
    l_vfFile.Data = Data
    
    l_vfFile.Load l_lngData
    If l_lngData = 0 Then
        l_vfFile.ReadSegment_Begin
        l_vfFile.Load Obj
        l_vfFile.ReadSegment_End
        l_vfFile.Load l_lngData
    Else
        l_lngData = 0
    End If
    
    DeserializeObject2 = (l_lngData = 1)
End Function


