Attribute VB_Name = "mdlCompression"
'
'   ::fury² graphics engine compression system::
'
Option Explicit

Public Declare Function zlibCompress Lib "zlib.dll" Alias "compress" (ByRef Dest As Any, ByRef destLen As Long, ByRef Src As Any, ByVal srcLen As Long) As Long
Public Declare Function zlibUnCompress Lib "zlib.dll" Alias "uncompress" (ByRef Dest As Any, ByRef destLen As Long, ByRef Src As Any, ByVal srcLen As Long) As Long

' ZLib requires zlib.dll

Sub LoadZLib32(FileHandle As Long, StartByte As Long, ByRef DestArray() As Long, Width As Long, Height As Long)
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
Dim ByteArray() As Byte
    Get #FileHandle, StartByte, destLen
    Get #FileHandle, StartByte + 4, sourceLen
    ReDim ByteArray(0 To sourceLen - 1)
    Get #FileHandle, StartByte + 8, ByteArray()
    ReDim DestArray(0 To Width - 1, 0 To Height - 1)
    Result = zlibUnCompress(DestArray(0, 0), destLen, ByteArray(0), sourceLen)
End Sub

Sub LoadZLib32B(FileHandle As Long, StartByte As Long, ByRef DestArray() As Byte, Width As Long, Height As Long)
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
Dim ByteArray() As Byte
    Get #FileHandle, StartByte, destLen
    Get #FileHandle, StartByte + 4, sourceLen
    ReDim ByteArray(0 To sourceLen - 1)
    Get #FileHandle, StartByte + 8, ByteArray()
    ReDim DestArray(0 To (Width * 4) - 1, 0 To Height - 1)
    Result = zlibUnCompress(DestArray(0, 0), destLen, ByteArray(0), sourceLen)
End Sub

Sub LoadZLib32BP(FileHandle As Long, StartByte As Long, DestPtr As Long, Width As Long, Height As Long)
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
Dim ByteArray() As Byte
    Get #FileHandle, StartByte, destLen
    Get #FileHandle, StartByte + 4, sourceLen
    ReDim ByteArray(0 To sourceLen - 1)
    Get #FileHandle, StartByte + 8, ByteArray()
    Result = zlibUnCompress(ByVal DestPtr, destLen, ByteArray(0), sourceLen)
End Sub

Sub SaveZLib32(FileHandle As Long, StartByte As Long, ByRef SourceArray() As Long)
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
Dim ByteArray() As Byte
    sourceLen = ((UBound(SourceArray, 1) + 1) * (UBound(SourceArray, 2) + 1)) * 4
    destLen = (sourceLen * 1.02) + 12
    ReDim ByteArray(0 To destLen - 1)
    Result = zlibCompress(ByteArray(0), destLen, SourceArray(0, 0), sourceLen)
    ReDim Preserve ByteArray(0 To destLen - 1)
    Put #FileHandle, StartByte, sourceLen
    Put #FileHandle, StartByte + 4, destLen
    Put #FileHandle, StartByte + 8, ByteArray()
End Sub

Sub SaveZLib32B(FileHandle As Long, StartByte As Long, ByRef SourceArray() As Byte)
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
Dim ByteArray() As Byte
    sourceLen = ((UBound(SourceArray, 1) + 1) * (UBound(SourceArray, 2) + 1))
    destLen = (sourceLen * 1500) \ 1000
    ReDim ByteArray(0 To destLen - 1)
    Result = zlibCompress(ByteArray(0), destLen, SourceArray(0, 0), sourceLen)
    ReDim Preserve ByteArray(0 To destLen - 1)
    Put #FileHandle, StartByte, sourceLen
    Put #FileHandle, StartByte + 4, destLen
    Put #FileHandle, StartByte + 8, ByteArray()
End Sub

Function zlibDecompressBytes(Data() As Byte, destLength As Long) As Byte()
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
    sourceLen = UBound(Data) + 1
    destLen = destLength
    ReDim destBytes(0 To (destLen * 1500) \ 1000)
    Result = zlibUnCompress(destBytes(0), destLen, Data(0), sourceLen)
    If Result <> 0 Then Err.Raise 1024 + Result, "zlib.dll", "an error occured while decompressing the data."
    ReDim Preserve destBytes(0 To destLen - 1)
    zlibDecompressBytes = destBytes()
End Function

Function zlibCompressBytes(Data() As Byte) As Byte()
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
    sourceLen = UBound(Data) + 1
    destLen = (destLen * 1500) \ 1000
    ReDim destBytes(0 To destLen - 1)
    Result = zlibCompress(destBytes(0), destLen, Data(0), sourceLen)
    If Result <> 0 Then Err.Raise 1024 + Result, "zlib.dll", "an error occured while compressing the data."
    ReDim Preserve destBytes(0 To destLen - 1)
    zlibCompressBytes = destBytes()
End Function

Function zlibDecompressBytesP(DataPointer As Long, sourceLength As Long, destLength As Long) As Byte()
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
    sourceLen = sourceLength
    destLen = destLength
    ReDim destBytes(0 To (destLen * 1500) \ 1000)
    Result = zlibUnCompress(destBytes(0), UBound(destBytes) + 1, ByVal DataPointer, sourceLen)
    If Result <> 0 Then Err.Raise 1024 + Result, "zlib.dll", "an error occured while decompressing the data."
    ReDim Preserve destBytes(0 To destLen - 1)
    zlibDecompressBytesP = destBytes()
End Function

Function zlibCompressBytesP(DataPointer As Long, Length As Long) As Byte()
On Error Resume Next
Dim destBytes() As Byte, sourceLen As Long, destLen As Long, Result As Long
    sourceLen = Length
    destLen = (Length * 1500) \ 1000
    ReDim destBytes(0 To destLen - 1)
    Result = zlibCompress(destBytes(0), destLen, ByVal DataPointer, sourceLen)
    If Result <> 0 Then Err.Raise 1024 + Result, "zlib.dll", "an error occured while compressing the data."
    ReDim Preserve destBytes(0 To destLen - 1)
    zlibCompressBytesP = destBytes()
End Function

