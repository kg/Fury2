Attribute VB_Name = "mdlOverride"
Option Explicit
Declare Function GetOverrideKey_ Lib "SoftFX" Alias "_GetOverrideKey@4" (ByVal Index As Long) As Long
Declare Function GetOverrideKeyLength Lib "SoftFX" Alias "_GetOverrideKeyLength@4" (ByVal Index As Long) As Long
Declare Function GetOverrideIndex Lib "SoftFX" Alias "_GetOverrideIndex@4" (ByVal Name As String) As Long
Declare Function AddOverride Lib "SoftFX" Alias "_AddOverride@8" (ByVal Name As String, ByVal Ptr As Long) As Long
Declare Function GetOverrideCount Lib "SoftFX" Alias "_GetOverrideCount@4" (ByVal Name As String) As Long
Declare Function RemoveOverride Lib "SoftFX" Alias "_RemoveOverride@8" (ByVal Name As String, ByVal Ptr As Long) As Long
Declare Function KeyMatches Lib "SoftFX" Alias "_KeyMatches@8" (ByVal Ptr As Long, ByVal Match As String) As Long

Public Function GetOverrideKey(ByVal Index As Long) As String
On Error Resume Next
Dim l_bytBuffer() As Byte
Dim l_lngLength As Long
Dim l_lngPointer As Long
    l_lngPointer = GetOverrideKey_(Index)
    l_lngLength = GetOverrideKeyLength(Index)
    ReDim l_bytBuffer(0 To l_lngLength - 1)
    CopyMemory ByVal VarPtr(l_bytBuffer(0)), ByVal l_lngPointer, l_lngLength
    GetOverrideKey = StrConv(l_bytBuffer, vbUnicode)
End Function

Public Sub DerefRectangle(ByVal Pointer As Long, ByRef Output As Rectangle, ByVal Image As Long)
On Error Resume Next
    If Pointer <> 0 Then
        CopyMemory ByVal VarPtr(Output), ByVal Pointer, LenB(Output)
    Else
        Output.Left = 0
        Output.Top = 0
        Output.Width = GetImageWidth(Image)
        Output.Height = GetImageHeight(Image)
    End If
End Sub

