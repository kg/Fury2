VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "VBScriptLanguage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IScriptLanguage

Private Function IScriptLanguage_GenerateAssignment(Target As String, Source As String, Optional Object As Boolean = False) As String
On Error Resume Next
    If Object Then
        IScriptLanguage_GenerateAssignment = "Set " & Target & " = " & Source
    Else
        IScriptLanguage_GenerateAssignment = Target & " = " & Source
    End If
End Function

Private Function IScriptLanguage_GenerateClass(Name As String, Code As String) As String
On Error Resume Next
    IScriptLanguage_GenerateClass = "Class " & Name & vbCrLf & Code & vbCrLf & "End Class" & vbCrLf
End Function

Private Function IScriptLanguage_GenerateClassMethod(Name As String, Args As Variant, Code As String) As String
On Error Resume Next
    If (VarType(Args) And vbArray) = vbArray Then
        IScriptLanguage_GenerateClassMethod = "Function " & Name & "(" & Join(Args, ",") & ")" & vbCrLf & Code & vbCrLf & "End Function" & vbCrLf
    Else
        IScriptLanguage_GenerateClassMethod = "Function " & Name & "(" & Args & ")" & vbCrLf & Code & vbCrLf & "End Function" & vbCrLf
    End If
End Function

Private Function IScriptLanguage_GenerateClassVariable(Name As String, Optional IsPublic As Boolean = True) As String
On Error Resume Next
    If IsPublic Then
        IScriptLanguage_GenerateClassVariable = "Public " & Name
    Else
        IScriptLanguage_GenerateClassVariable = "Private " & Name
    End If
End Function

Private Function IScriptLanguage_GenerateComment(Text As String) As String
On Error Resume Next
    IScriptLanguage_GenerateComment = "'" & Replace(Text, vbCrLf, vbCrLf + "'")
End Function

Private Function IScriptLanguage_GenerateFunction(Name As String, Args As Variant, Code As String) As String

End Function

Private Function IScriptLanguage_GenerateSubCall(Name As String, Args As Variant) As String
On Error Resume Next
    If (VarType(Args) And vbArray) = vbArray Then
        IScriptLanguage_GenerateSubCall = Name & " " & Join(Args, ", ")
    Else
        IScriptLanguage_GenerateSubCall = Name & " " & Args
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
    IScriptLanguage_GenerateInstantiation = "New " & Class
End Function

Private Function IScriptLanguage_GenerateStringLiteral(Text As String) As String
On Error Resume Next
    IScriptLanguage_GenerateStringLiteral = Replace(Replace(Text, """", """"""), vbCrLf, """ + vbCrLf + """)
End Function

Private Function IScriptLanguage_GenerateVariable(Name As String, Optional Value As String) As String
On Error Resume Next
    If IsMissing(Value) Or Trim(Value) = "" Then
        IScriptLanguage_GenerateVariable = "Dim " & Name
    Else
        IScriptLanguage_GenerateVariable = "Dim " & Name & vbCrLf & Name & " = " & Value
    End If
End Function

Private Function IScriptLanguage_ImplementsMethod(Code As String, Method As String) As Boolean
On Error Resume Next
    IScriptLanguage_ImplementsMethod = InStr(1, Code, "Sub " & Method & "(", vbTextCompare) Or InStr(1, Code, "Function " & Method & "(", vbTextCompare)
End Function

Private Property Get IScriptLanguage_LineTerminator() As String
    IScriptLanguage_LineTerminator = vbCrLf
End Property

Private Property Get IScriptLanguage_MethodOperator() As String
    IScriptLanguage_MethodOperator = "."
End Property

Private Property Get IScriptLanguage_Name() As String
    IScriptLanguage_Name = "VBScript"
End Property

Private Function IScriptLanguage_ParseCode(Code As String) As String
Dim m_strText As String, m_strSeg As String
Dim m_lngStart As Long, m_lngEnd As Long
Dim m_lngPos As Long
    '   Strip smart quotes and other idiocies
    If Len(Code) < 2 Then IScriptLanguage_ParseCode = Code: Exit Function
    m_strText = Replace(Replace(Replace(Replace(Replace(Code, "”", """"), "“", """"), "…", "..."), "’", "'"), "‘", "'")
    m_lngPos = 1
    '   Multi-Line String
    Do
        m_lngStart = InStr(m_lngPos, m_strText, "{{")
        If m_lngStart < m_lngPos Then Exit Do
        m_lngEnd = InStr(m_lngPos, m_strText, "}}")
        If m_lngEnd <= m_lngStart Then Exit Do
        m_strSeg = Mid(m_strText, m_lngStart + 2, (m_lngEnd - m_lngStart) - 2)
        m_strSeg = Replace(m_strSeg, """", """""")
        m_strSeg = Replace(m_strSeg, vbCrLf, """ + vbCrLf + """)
        m_strSeg = """" + m_strSeg + """"
        m_strText = Left(m_strText, m_lngStart - 1) + m_strSeg + Mid(m_strText, m_lngEnd + 2)
        m_lngPos = m_lngStart + (Len(m_strSeg))
    Loop
    m_lngPos = 1
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
    IScriptLanguage_ThisKeyword = "Me"
End Property
