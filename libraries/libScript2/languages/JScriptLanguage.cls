VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "JScriptLanguage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IScriptLanguage

Private Function IScriptLanguage_GenerateAssignment(Target As String, Source As String, Optional Object As Boolean = False) As String
On Error Resume Next
    If Object Then
        IScriptLanguage_GenerateAssignment = Target & " = " & Source
    Else
        IScriptLanguage_GenerateAssignment = Target & " = " & Source
    End If
End Function

Private Function IScriptLanguage_GenerateClass(Name As String, Code As String) As String
On Error Resume Next
    IScriptLanguage_GenerateClass = "function " & Name & "()" & vbCrLf & "{" & vbCrLf & Code & vbCrLf & "}" & vbCrLf
End Function

Private Function IScriptLanguage_GenerateClassMethod(Name As String, Args As Variant, Code As String) As String
On Error Resume Next
    If (VarType(Args) And vbArray) = vbArray Then
        IScriptLanguage_GenerateClassMethod = "this." & Name & "= function(" & Join(Args, ",") & ")" & vbCrLf & "{" & vbCrLf & Code & vbCrLf & "};" & vbCrLf
    Else
        IScriptLanguage_GenerateClassMethod = "this." & Name & "= function(" & Args & ")" & vbCrLf & "{" & vbCrLf & Code & vbCrLf & "};" & vbCrLf
    End If
End Function

Private Function IScriptLanguage_GenerateClassVariable(Name As String, Optional IsPublic As Boolean = True) As String
On Error Resume Next
    IScriptLanguage_GenerateClassVariable = "var " & Name & ";"
End Function

Private Function IScriptLanguage_GenerateComment(Text As String) As String
On Error Resume Next
    IScriptLanguage_GenerateComment = "//" & Replace(Text, vbCrLf, vbCrLf + "//")
End Function

Private Function IScriptLanguage_GenerateFunction(Name As String, Args As Variant, Code As String) As String

End Function

Private Function IScriptLanguage_GenerateSubCall(Name As String, Args As Variant) As String
On Error Resume Next
    If (VarType(Args) And vbArray) = vbArray Then
        IScriptLanguage_GenerateSubCall = Name & "(" & Join(Args, ", ") & ")"
    Else
        IScriptLanguage_GenerateSubCall = Name & "(" & Args & ")"
    End If
End Function

Private Function IScriptLanguage_GenerateFunctionCall(Name As String, Args As Variant) As String
On Error Resume Next
    If (VarType(Args) And vbArray) = vbArray Then
        IScriptLanguage_GenerateFunctionCall = Name & "(" & Join(Args, ", ") & ")"
    Else
        IScriptLanguage_GenerateFunctionCall = Name & "(" & Args & ")"
    End If
End Function

Private Function IScriptLanguage_GenerateInstantiation(Class As String) As String
On Error Resume Next
    IScriptLanguage_GenerateInstantiation = "new " & Class
End Function

Private Function IScriptLanguage_GenerateStringLiteral(Text As String) As String
On Error Resume Next
    IScriptLanguage_GenerateStringLiteral = Replace(Replace(Text, """", "\"""), vbCrLf, """ + vbCrLf + """)
End Function

Private Function IScriptLanguage_GenerateVariable(Name As String, Optional Value As String) As String
On Error Resume Next
    If IsMissing(Value) Or Trim(Value) = "" Then
        IScriptLanguage_GenerateVariable = "var " & Name
    Else
        IScriptLanguage_GenerateVariable = "var " & Name & " = " & Value
    End If
End Function

Private Function IScriptLanguage_ImplementsMethod(Code As String, Method As String) As Boolean
On Error Resume Next
    IScriptLanguage_ImplementsMethod = InStr(1, Code, "function " & Method & "(", vbTextCompare) Or InStr(1, Code, "method " & Method & "(", vbTextCompare) Or InStr(1, Code, "this." & Method, vbTextCompare)
End Function

Private Property Get IScriptLanguage_LineTerminator() As String
    IScriptLanguage_LineTerminator = ";" & vbCrLf
End Property

Private Property Get IScriptLanguage_MethodOperator() As String
    IScriptLanguage_MethodOperator = "."
End Property

Private Property Get IScriptLanguage_Name() As String
    IScriptLanguage_Name = "JScript"
End Property

Private Function IScriptLanguage_ParseCode(Code As String) As String
On Error Resume Next
Dim l_strCode As String, l_strSeg As String
Dim l_lngPos As Long
Dim l_lngStart As Long, l_lngEnd As Long
    If Len(Code) < 2 Then IScriptLanguage_ParseCode = Code: Exit Function
    l_lngPos = 1
    l_strCode = Code
    '   Multi-Line String
    Do
        l_lngStart = InStr(l_lngPos, l_strCode, "method ")
        If l_lngStart < l_lngPos Then Exit Do
        l_lngEnd = InStr(l_lngStart, l_strCode, "(")
        If l_lngEnd <= l_lngStart Then Exit Do
        l_strSeg = Mid(l_strCode, l_lngStart, l_lngEnd - l_lngStart)
        l_strSeg = "this." & Mid(l_strSeg, 8) & " = function"
        l_strCode = Left(l_strCode, l_lngStart - 1) + l_strSeg + Mid(l_strCode, l_lngEnd)
        l_lngPos = l_lngStart + (Len(l_strSeg))
    Loop
    IScriptLanguage_ParseCode = l_strCode
End Function

Private Function IScriptLanguage_ReadComment(Text As String) As String

End Function

Private Function IScriptLanguage_ReadFunctionArgs(Text As String) As String

End Function

Private Function IScriptLanguage_ReadFunctionCode(Text As String) As String

End Function

Private Function IScriptLanguage_ReadFunctionName(Text As String) As String

End Function

Private Function IScriptLanguage_ReadStringLiteral(Text As String) As String

End Function

Private Function IScriptLanguage_SplitLines(Text As String) As Variant
    IScriptLanguage_SplitLines = Split(Text, ";")
End Function

Private Property Get IScriptLanguage_ThisKeyword() As String
    IScriptLanguage_ThisKeyword = "this"
End Property

