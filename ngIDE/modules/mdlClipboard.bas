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

Public Function ClipboardSerializeImage(Clipboard As cCustomClipboard, ByVal hwnd As Long, Image As Fury2Image) As Boolean
On Error Resume Next
Dim l_lngFormat As Long
Dim l_vfData As VirtualFile
Dim l_lngLength As Long
Dim l_lngBitmap As Long
    l_lngFormat = Clipboard.AddFormat("Fury2Image")
    Clipboard.ClipboardOpen hwnd
    Set l_vfData = New VirtualFile
    Err.Clear
    If Fury2Globals.SaveToFile(Image, l_vfData) Then
        Clipboard.ClearClipboard
        If Clipboard.SetBinaryData(l_lngFormat, l_vfData.Data) Then
            ClipboardSerializeImage = True
        End If
        Clipboard.SetData CF_BITMAP, F2ImageToGDIPicture(Image)
    End If
    Set l_vfData = Nothing
    Err.Clear
    Clipboard.ClipboardClose
End Function

Public Function ClipboardDeserializeImage(Clipboard As cCustomClipboard, ByVal hwnd As Long) As Fury2Image
On Error Resume Next
Dim l_lngFormat As Long
Dim l_vfData As VirtualFile
Dim l_lngLength As Long
Dim l_lngBitmap As Long
Dim l_imgImage As Fury2Image
    l_lngFormat = Clipboard.AddFormat("Fury2Image")
    Clipboard.ClipboardOpen hwnd
    l_lngLength = Clipboard.GetBinaryDataSize(l_lngFormat)
    If l_lngLength Then
        Set l_vfData = New VirtualFile
        l_vfData.Length = l_lngLength
        Clipboard.CopyBinaryData l_lngFormat, l_vfData.Pointer
        Set l_imgImage = New Fury2Image
        If Fury2Globals.LoadFromFile(l_imgImage, l_vfData) Then
            Set ClipboardDeserializeImage = l_imgImage
        End If
    Else
        l_lngBitmap = Clipboard.GetData(CF_BITMAP)
        If l_lngBitmap Then
            Set ClipboardDeserializeImage = F2ImageFromGDIPicture(l_lngBitmap)
        End If
    End If
    Err.Clear
    Clipboard.ClipboardClose
End Function

Public Function ClipboardContainsImage(Clipboard As cCustomClipboard, ByVal hwnd As Long) As Boolean
On Error Resume Next
Dim l_lngFormat As Long
Dim l_vfData As VirtualFile
Dim l_lngLength As Long
Dim l_lngBitmap As Long
Dim l_imgImage As Fury2Image
    l_lngFormat = Clipboard.AddFormat("Fury2Image")
    Clipboard.GetCurrentFormats hwnd
    ClipboardContainsImage = Clipboard.HasCurrentFormat(CF_BITMAP) Or Clipboard.HasCurrentFormat(l_lngFormat)
End Function
