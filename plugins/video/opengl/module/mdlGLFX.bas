Attribute VB_Name = "mdlGLFX"
Option Explicit
Public Declare Function GLInit Lib "GLFX" Alias "_GLInit@8" (ByVal HWND As Long, ByVal HDC As Long) As Long
Public Declare Function GLShutdown Lib "GLFX" Alias "_GLShutdown@0" () As Long
Public Declare Function GLSetOutputSize Lib "GLFX" Alias "_GLSetOutputSize@8" (ByVal Width As Long, ByVal Height As Long) As Long
Public Declare Function GLSetScaleMode Lib "GLFX" Alias "_GLSetScaleMode@4" (ByVal ScaleMode As Long) As Long
Public Declare Function GLFlip Lib "GLFX" Alias "_GLFlip@0" () As Long
Public Declare Function GLCopySurface Lib "GLFX" Alias "_GLCopySurface@8" (ByVal FromImage As Long, ByVal ToImage As Long) As Long
Public Declare Function GLInstallAllocateHook Lib "GLFX" Alias "_GLInstallAllocateHook@0" () As Long
Public Declare Function GLUninstallAllocateHook Lib "GLFX" Alias "_GLUninstallAllocateHook@0" () As Long
Public Declare Function GLInstallFBAllocateHook Lib "GLFX" Alias "_GLInstallFBAllocateHook@0" () As Long
Public Declare Function GLUninstallFBAllocateHook Lib "GLFX" Alias "_GLUninstallFBAllocateHook@0" () As Long
Public Declare Function GLAllocateBytes Lib "GLFX" Alias "_GLAllocateBytes@4" (ByVal Length As Long) As Long
Public Declare Function GLGetStringLength Lib "GLFX" Alias "_GLGetStringLength@4" (ByVal StringPtr As Long) As Long
Public Declare Function GLSetShaderLoadCallback Lib "GLFX" Alias "_GLSetShaderLoadCallback@4" (ByVal Callback As Long) As Long
Public Declare Function GLGetShader Lib "GLFX" Alias "_GLGetShader@4" (ByVal Shader As String) As Long
Public Declare Function GLShaderBlit Lib "GLFX" Alias "_GLShaderBlit@28" (ByVal Dest As Long, ByVal Source As Long, ByRef DestRect As Rectangle, ByRef SourceRect As Rectangle, ByVal Renderer As Long, ByVal Scaler As Long, ByVal Shader As Long) As Long

Public Const ShaderPath As String = "J:\development\binary\sys\shaders\"

Public Function ParseShader(ByRef Text As String) As String
On Error Resume Next
Dim l_strLines() As String
Dim l_strTokens() As String
Dim l_strLine As String
Dim l_strFilename As String
Dim l_lngLines As Long
    l_strLines = Split(Text, vbCrLf)
    For l_lngLines = LBound(l_strLines) To UBound(l_strLines)
        l_strLine = Trim(l_strLines(l_lngLines))
        If Left(l_strLine, 1) = "#" Then
            l_strTokens = Split(Mid(l_strLine, 2), " ", 2)
            Select Case LCase(l_strTokens(0))
            Case "include"
                l_strFilename = Trim(l_strTokens(1))
                If Left(l_strFilename, 1) = """" Then l_strFilename = Mid(l_strFilename, 2)
                If Right(l_strFilename, 1) = """" Then l_strFilename = Left(l_strFilename, Len(l_strFilename) - 1)
                Debug.Print "Including " & l_strFilename
                l_strLine = ReadTextFile(ShaderPath & l_strFilename & ".sh")
            Case Else
                l_strLine = ""
            End Select
            l_strLines(l_lngLines) = l_strLine
        End If
    Next l_lngLines
    ParseShader = Join(l_strLines, vbCrLf)
End Function

Public Sub ShaderLoadCallback(ByVal NamePtr As Long, ByRef SourcePtr As Long)
On Error Resume Next
Dim l_lngLength As Long
Dim l_bytName() As Byte
Dim l_strName As String
Dim l_strText As String
Dim l_lngNull As Long
    l_lngLength = GLGetStringLength(NamePtr)
    ReDim l_bytName(0 To l_lngLength - 1)
    CopyMemory ByVal VarPtr(l_bytName(0)), ByVal NamePtr, l_lngLength
    l_lngLength = 0
    l_strName = StrConv(l_bytName, vbUnicode)
    Debug.Print "Load shader """ & l_strName & """"
    l_strText = ReadTextFile(ShaderPath & l_strName & ".fs")
    l_strText = ParseShader(l_strText)
    l_lngLength = Len(l_strText) + 1
    Debug.Print "Loaded " & l_lngLength & " characters"
    SourcePtr = GLAllocateBytes(l_lngLength)
    l_strText = StrConv(l_strText, vbFromUnicode)
    CopyMemory ByVal SourcePtr, ByVal StrPtr(l_strText), l_lngLength - 1
    CopyMemory ByVal SourcePtr + l_lngLength - 1, ByVal VarPtr(l_lngNull), 1
End Sub

