Attribute VB_Name = "mdlClipboard"
Option Explicit

Public Function ClipboardDeserializeFile(Clipboard As cCustomClipboard, Format As Long, File As VirtualFile) As Boolean
On Error Resume Next
Dim l_lngLength As Long
    l_lngLength = Clipboard.GetBinaryDataSize(Format)
    If l_lngLength Then
        File.Length = l_lngLength
        Clipboard.CopyBinaryData Format, File.Pointer
        ClipboardDeserializeFile = True
    End If
    Err.Clear
End Function

Public Function ClipboardDeserialize(Clipboard As cCustomClipboard, Format As Long, Object As IVirtualFileSaveable) As Boolean
On Error Resume Next
Dim l_vfData As VirtualFile
Dim l_lngLength As Long
    l_lngLength = Clipboard.GetBinaryDataSize(Format)
    If l_lngLength Then
        Set l_vfData = New VirtualFile
        l_vfData.Length = l_lngLength
        Clipboard.CopyBinaryData Format, l_vfData.Pointer
        If Fury2Globals.LoadFromFile(Object, l_vfData) Then
            ClipboardDeserialize = True
        End If
    End If
    Err.Clear
End Function

Public Function ClipboardSerialize(Clipboard As cCustomClipboard, Format As Long, Object As IVirtualFileSaveable) As Boolean
On Error Resume Next
Dim l_vfData As VirtualFile
Dim l_lngLength As Long
    Set l_vfData = New VirtualFile
    Err.Clear
    If Fury2Globals.SaveToFile(Object, l_vfData) Then
        Clipboard.ClearClipboard
        If Clipboard.SetBinaryData(Format, l_vfData.Data) Then
            ClipboardSerialize = True
        End If
    End If
    Set l_vfData = Nothing
    Err.Clear
End Function
