Attribute VB_Name = "mdlText"
'
'    mdlText - Unicode Text File Utility Routines
'    Copyright (c) 2005 Kevin Gadd
'
'    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
'    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
'    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'

Option Explicit

Public Enum TextFormats
    tfAscii
    tfUTF8
    tfUCS2LittleEndian
    tfUCS2BigEndian
End Enum

Public Function EncodeTextFormat(ByRef Text As String, ByVal NewFormat As TextFormats, Optional ByRef IsUTF8 As Boolean = False) As Byte()
On Error Resume Next
Dim l_bytData() As Byte
    Select Case NewFormat
    Case tfAscii
        EncodeTextFormat = Text
    Case tfUCS2LittleEndian
        EncodeTextFormat = ChrW(&HFEFF) & Text
    Case tfUCS2BigEndian
        EncodeTextFormat = ChrW(&HFEFF) & Text
        SwapEndianness EncodeTextFormat, 2
    Case tfUTF8
        EncodeTextFormat = EncodeUTF8(Text, IsUTF8)
    Case Else
    End Select
End Function

Public Function WriteTextFile(ByRef Filename As String, ByRef Text As String, Optional ByVal Format As TextFormats = tfUTF8) As Boolean
On Error Resume Next
Dim l_lngHandle As Long, l_lngLength As Long
Dim l_bytHeader() As Byte, l_bytData() As Byte
Dim l_fmtFormat As TextFormats
Dim l_booUTF8 As Boolean
    Err.Clear
    l_lngHandle = FreeFile()
    If FileExists(Filename + "_") Then Kill Filename + "_"
    Open Filename + "_" For Binary Access Write As #l_lngHandle
        If Err <> 0 Then
            Err.Raise Err.Number, "WriteTextFile:OpenFile", Err.Description
            Exit Function
        End If
        l_bytData = EncodeTextFormat(Text, Format, l_booUTF8)
        If Err <> 0 Then
            Err.Raise Err.Number, "WriteTextFile:Encode", Err.Description
            Exit Function
        End If
        If (l_booUTF8) Then
            Put #l_lngHandle, 1, CByte(&HEF)
            Put #l_lngHandle, 2, CByte(&HBB)
            Put #l_lngHandle, 3, CByte(&HBF)
            Put #l_lngHandle, 4, l_bytData
        Else
            Put #l_lngHandle, 1, l_bytData
        End If
    Close #l_lngHandle
    If Err <> 0 Then
        Err.Raise Err.Number, "WriteTextFile:" & Err.Source, Err.Description
        Exit Function
    End If
    Err.Clear
    If FileExists(Filename) Then Kill Filename
    Name Filename + "_" As Filename
    If Err <> 0 Then
        Err.Raise Err.Number, "WriteTextFile:Save", Err.Description
        Exit Function
    End If
    WriteTextFile = True
    Err.Clear
End Function

Public Function ReadTextFile(ByRef Filename As String) As String
On Error Resume Next
Dim l_lngHandle As Long, l_lngLength As Long
Dim l_bytHeader() As Byte, l_bytData() As Byte
Dim l_fmtFormat As TextFormats
    Err.Clear
    l_lngHandle = FreeFile()
    Open Filename For Binary Access Read As #l_lngHandle
        If Err <> 0 Then
            Err.Raise Err.Number, "ReadTextFile:OpenFile", Err.Description
            Exit Function
        End If
        l_lngLength = LOF(l_lngHandle)
        If l_lngLength < 3 Then
            l_fmtFormat = tfAscii
            ReDim l_bytData(0 To l_lngLength - 1)
            Get #l_lngHandle, 1, l_bytData
            ReadTextFile = ReadTextData(l_bytData, l_fmtFormat)
        Else
            ReDim l_bytHeader(0 To 2)
            Get #l_lngHandle, 1, l_bytHeader
            If l_bytHeader(0) = &HEF And l_bytHeader(1) = &HBB And l_bytHeader(2) = &HBF Then
                l_fmtFormat = tfUTF8
                ReDim l_bytData(0 To l_lngLength - 4)
                Get #l_lngHandle, 4, l_bytData
                ReadTextFile = ReadTextData(l_bytData, l_fmtFormat)
            ElseIf l_bytHeader(0) = &HFF And l_bytHeader(1) = &HFE Then
                l_fmtFormat = tfUCS2LittleEndian
                ReDim l_bytData(0 To l_lngLength - 3)
                Get #l_lngHandle, 3, l_bytData
                ReadTextFile = ReadTextData(l_bytData, l_fmtFormat)
            ElseIf l_bytHeader(0) = &HFE And l_bytHeader(1) = &HFF Then
                l_fmtFormat = tfUCS2BigEndian
                ReDim l_bytData(0 To l_lngLength - 3)
                Get #l_lngHandle, 3, l_bytData
                ReadTextFile = ReadTextData(l_bytData, l_fmtFormat)
            Else
                l_fmtFormat = tfAscii
                ReDim l_bytData(0 To l_lngLength - 1)
                Get #l_lngHandle, 1, l_bytData
                ReadTextFile = ReadTextData(l_bytData, l_fmtFormat)
            End If
        End If
    Close #l_lngHandle
    If Err <> 0 Then
        Err.Raise Err.Number, "ReadTextFile:" & Err.Source, Err.Description
        Exit Function
    End If
End Function

Public Function ReadTextData(ByRef Data() As Byte, ByVal Format As TextFormats) As String
On Error Resume Next
    Select Case Format
    Case tfAscii
        ReadTextData = StrConv(Data, vbUnicode)
        If Err <> 0 Then
            Err.Raise Err.Number, "ReadTextData:StrConv", Err.Description
            Exit Function
        End If
    Case tfUTF8
        Err.Clear
        ReadTextData = DecodeUTF8(Data)
        If Err <> 0 Then
            Err.Raise Err.Number, "ReadTextData:" & Err.Source, Err.Description
            Exit Function
        End If
    Case tfUCS2LittleEndian
        ReadTextData = Data
    Case tfUCS2BigEndian
        SwapEndianness Data, 2
        If Err <> 0 Then
            Err.Raise Err.Number, "ReadTextData:SwapEndianness", Err.Description
            Exit Function
        End If
        ReadTextData = Data
    End Select
End Function

Public Function EncodeUTF8(ByRef Text As String, Optional ByRef IsUTF8 As Boolean = False) As Byte()
'On Error Resume Next
Dim l_lngCharacter As Long
Dim l_lngLength As Long
Dim l_bytBuffer() As Byte, l_lngBufferLength As Long
Dim l_bytNewData(0 To 2) As Byte
Dim l_lngValue As Long
    Err.Clear
    ReDim l_bytBuffer(0 To 4095)
    l_lngBufferLength = 0
    l_lngCharacter = 1
    Do While l_lngCharacter <= Len(Text)
        l_lngValue = AscW(Mid(Text, l_lngCharacter, 1))
        If (l_lngValue >= &HFFFF&) Then
            ' Character outside supported range. Visual Basic strings are two bytes per character.
            Err.Raise 6, "EncodeUTF8", "Overflow (character value outside of supported range)"
            Exit Function
        ElseIf (l_lngValue >= &H800&) Then
            ' 3 bytes
            l_lngLength = 3
            l_bytNewData(0) = (&HE0) Or (l_lngValue And &HF)
            l_bytNewData(1) = (&H80) Or ((l_lngValue \ 16) And &H3F)
            l_bytNewData(2) = (&H80) Or ((l_lngValue \ 1024) And &H3F)
            IsUTF8 = True
        ElseIf (l_lngValue >= &H80&) Then
            ' 2 bytes
            l_lngLength = 2
            l_bytNewData(0) = (&HC0) Or (l_lngValue And &H1F)
            l_bytNewData(1) = (&H80) Or ((l_lngValue \ 64) And &H3F)
            IsUTF8 = True
        ElseIf (l_lngValue <= &H7F&) Then
            ' 1 byte
            l_lngLength = 1
            l_bytNewData(0) = l_lngValue And &H7F
        End If
        l_lngBufferLength = l_lngBufferLength + l_lngLength
        If (l_lngBufferLength > (UBound(l_bytBuffer) - LBound(l_bytBuffer) + 1)) Then
            ReDim Preserve l_bytBuffer(0 To UBound(l_bytBuffer) + 4096)
        End If
        CopyMemory ByVal VarPtr(l_bytBuffer(l_lngBufferLength - l_lngLength)), ByVal VarPtr(l_bytNewData(0)), l_lngLength
        l_lngCharacter = l_lngCharacter + 1
    Loop
    ReDim Preserve l_bytBuffer(0 To l_lngBufferLength - 1)
    EncodeUTF8 = l_bytBuffer
End Function

Public Function DecodeUTF8(ByRef Data() As Byte) As String
On Error Resume Next
Dim l_lngCharacter As Long
Dim l_lngAccumulator As Long, l_lngOffset As Long, l_lngLength As Long, l_lngMultiplier As Long
Dim l_strBuffer As String
Dim l_lngValue As Long
    Err.Clear
    Do While l_lngCharacter <= UBound(Data)
        l_lngValue = Data(l_lngCharacter)
        If l_lngOffset = 0 Then
            If (l_lngValue > 239) Then
                ' Character outside supported range. Visual Basic strings are two bytes per character.
                Err.Raise 6, "DecodeUTF8", "Overflow (character value outside of supported range)"
                Exit Function
            ElseIf (l_lngValue >= 224) Then
                l_lngLength = 3
                l_lngOffset = 1
                l_lngAccumulator = (l_lngValue - 224) * 4096
            ElseIf (l_lngValue >= 192) Then
                l_lngLength = 2
                l_lngOffset = 1
                l_lngAccumulator = (l_lngValue - 192) * 64
            ElseIf (l_lngValue <= 127) Then
                l_lngLength = 1
                l_lngOffset = 1
                l_lngAccumulator = l_lngValue
            End If
        Else
            l_lngMultiplier = 1
            If l_lngLength = 3 Then
                l_lngMultiplier = IIf(l_lngOffset = 1, 64, 1)
            End If
            l_lngAccumulator = l_lngAccumulator + ((l_lngValue - 128) * (l_lngMultiplier))
            l_lngOffset = l_lngOffset + 1
        End If
        If l_lngOffset = l_lngLength Then
            l_strBuffer = l_strBuffer & ChrW(l_lngAccumulator)
            l_lngOffset = 0
        End If
        l_lngCharacter = l_lngCharacter + 1
    Loop
    DecodeUTF8 = l_strBuffer
End Function

Public Sub SwapEndianness(ByRef Data() As Byte, ByVal Stride As Long)
On Error Resume Next
Dim l_lngCharacter As Long
Dim l_bytTemp As Byte
    Select Case Stride
    Case 2
        For l_lngCharacter = LBound(Data) To UBound(Data) Step 2
            l_bytTemp = Data(l_lngCharacter)
            Data(l_lngCharacter) = Data(l_lngCharacter + 1)
            Data(l_lngCharacter + 1) = l_bytTemp
        Next l_lngCharacter
    Case 4
        For l_lngCharacter = LBound(Data) To UBound(Data) Step 4
            l_bytTemp = Data(l_lngCharacter)
            Data(l_lngCharacter) = Data(l_lngCharacter + 3)
            Data(l_lngCharacter + 3) = l_bytTemp
            l_bytTemp = Data(l_lngCharacter + 1)
            Data(l_lngCharacter + 1) = Data(l_lngCharacter + 2)
            Data(l_lngCharacter + 2) = l_bytTemp
        Next l_lngCharacter
    Case Else
    End Select
End Sub
