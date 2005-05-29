VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "PythonLanguage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IScriptLanguage

Private Function DoIndent(ByRef Text As String) As String
On Error Resume Next
Dim l_strLines() As String, l_lngLine As Long
    l_strLines = Split(Text, vbCrLf)
    For l_lngLine = LBound(l_strLines) To UBound(l_strLines)
        l_strLines(l_lngLine) = "  " & l_strLines(l_lngLine)
    Next l_lngLine
    DoIndent = Join(l_strLines, vbCrLf)
End Function

Private Function IScriptLanguage_GenerateAssignment(Target As String, Source As String, Optional Object As Boolean = False) As String
On Error Resume Next
    IScriptLanguage_GenerateAssignment = Target & " = " & Source
End Function

Private Function IScriptLanguage_GenerateClass(Name As String, Code As String) As String
On Error Resume Next
    IScriptLanguage_GenerateClass = "class " & Name & ":" & vbCrLf & DoIndent(Code) & vbCrLf
End Function

Private Function IScriptLanguage_GenerateClassMethod(Name As String, Args As Variant, Code As String) As String
On Error Resume Next
    If (VarType(Args) And vbArray) = vbArray Then
        IScriptLanguage_GenerateClassMethod = "def " & Name & "(self, " & Join(Args, ",") & "):" & vbCrLf & DoIndent(Code) & vbCrLf
    Else
        IScriptLanguage_GenerateClassMethod = "def " & Name & "(self" & IIf(Len(Args) > 0, ", ", "") & Args & "):" & vbCrLf & DoIndent(Code) & vbCrLf
    End If
End Function

Private Function IScriptLanguage_GenerateClassVariable(Name As String, Optional IsPublic As Boolean = True) As String
On Error Resume Next
    Exit Function ' muahahaha
    If IsPublic Then
        IScriptLanguage_GenerateClassVariable = Name
    Else
        IScriptLanguage_GenerateClassVariable = Name
    End If
End Function

Private Function IScriptLanguage_GenerateComment(Text As String) As String
On Error Resume Next
    IScriptLanguage_GenerateComment = "#" & Replace(Text, vbCrLf, vbCrLf + "#")
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
    IScriptLanguage_GenerateInstantiation = Class & "()"
End Function

Private Function IScriptLanguage_GenerateStringLiteral(Text As String) As String
On Error Resume Next
    IScriptLanguage_GenerateStringLiteral = Replace(Replace(Text, """", "\"""), vbCrLf, "\n")
End Function

Private Function IScriptLanguage_GenerateVariable(Name As String, Optional Value As String) As String
On Error Resume Next
    If IsMissing(Value) Or Trim(Value) = "" Then
        IScriptLanguage_GenerateVariable = Name
    Else
        IScriptLanguage_GenerateVariable = Name & " = " & Value
    End If
End Function

Private Function IScriptLanguage_ImplementsMethod(Code As String, Method As String) As Boolean
On Error Resume Next
    IScriptLanguage_ImplementsMethod = InStr(1, Code, "def " & Method, vbTextCompare)
End Function

Private Property Get IScriptLanguage_LineTerminator() As String
    IScriptLanguage_LineTerminator = vbCrLf
End Property

Private Property Get IScriptLanguage_MethodOperator() As String
    IScriptLanguage_MethodOperator = "."
End Property

Private Property Get IScriptLanguage_Name() As String
    IScriptLanguage_Name = "Python"
End Property

Private Function IScriptLanguage_ParseCode(Code As String) As String
Dim m_strText As String
    '   Strip smart quotes and other idiocies
    If Len(Code) < 2 Then IScriptLanguage_ParseCode = Code: Exit Function
    m_strText = Replace(Replace(Replace(Replace(Replace(Code, "”", """"), "“", """"), "…", "..."), "’", "'"), "‘", "'")
    IScriptLanguage_ParseCode = m_strText
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
    IScriptLanguage_SplitLines = Split(Text, vbCrLf)
End Function

Private Property Get IScriptLanguage_ThisKeyword() As String
    IScriptLanguage_ThisKeyword = "self"
End Property
