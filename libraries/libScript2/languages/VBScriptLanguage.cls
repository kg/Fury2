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
        IScriptLanguage_GenerateClassMethod = "Public Sub " & Name & "(" & Join(Args, ",") & ")" & vbCrLf & Code & vbCrLf & "End Sub" & vbCrLf
    Else
        IScriptLanguage_GenerateClassMethod = "Public Sub " & Name & "(" & Args & ")" & vbCrLf & Code & vbCrLf & "End Sub" & vbCrLf
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
On Error Resume Next
    If (VarType(Args) And vbArray) = vbArray Then
        IScriptLanguage_GenerateFunction = "Public Function " & Name & "(" & Join(Args, ",") & ")" & vbCrLf & Code & vbCrLf & "End Function" & vbCrLf
    Else
        IScriptLanguage_GenerateFunction = "Public Function " & Name & "(" & Args & ")" & vbCrLf & Code & vbCrLf & "End Function" & vbCrLf
    End If
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
        If Len(Trim(Args)) = 0 Then
            IScriptLanguage_GenerateFunctionCall = Name
        Else
            IScriptLanguage_GenerateFunctionCall = Name & "(" & Args & ")"
        End If
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

Private Function IScriptLanguage_ParseCode(Code As String, State As ScriptEngineState) As String
On Error Resume Next
Dim m_strText As String, m_strSeg As String
Dim m_lngStart As Long, m_lngEnd As Long
Dim m_lngPos As Long
Dim l_varLines As Variant, l_lngLines As Long, l_varTokens As Variant
Dim l_strToken As String, l_strToken2 As String, l_varVariables As Variant
Dim l_clsClass As ScriptClass, l_fnFunction As ScriptFunction, l_varVariable As ScriptVariable, l_argArgument As ScriptArgument
Dim l_lngVariable As Long
Dim l_booData As Boolean
    If Len(Code) < 2 Then IScriptLanguage_ParseCode = Code: Exit Function
    '   Strip smart quotes and other idiocies
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
    l_varLines = IScriptLanguage_SplitLines(m_strText)
    For l_lngLines = LBound(l_varLines) To UBound(l_varLines)
        l_varTokens = Split(Trim(l_varLines(l_lngLines)), " ")
        Select Case LCase(Trim(l_varTokens(0)))
        Case "class"
            Set l_clsClass = New ScriptClass
            l_clsClass.Name = Trim(l_varTokens(1))
            State.Classes.Add l_clsClass
        Case "sub", "function", "dim", "public", "private", "publicdata", "privatedata"
            l_strToken = LCase(Trim(l_varTokens(0)))
            l_strToken2 = l_varTokens(1)
            If InStr(l_strToken, "public") Or InStr(l_strToken, "private") Or l_strToken = "dim" Then
                l_strToken = LCase(Trim(l_varTokens(1)))
                l_strToken2 = l_varTokens(2)
            End If
            Select Case l_strToken
            Case "sub", "function"
                ' Sub/Function
                Set l_fnFunction = New ScriptFunction
                l_fnFunction.Name = Trim(Split(l_strToken2, "(")(0))
                l_varVariables = Split(Replace(Mid(l_varLines(l_lngLines), InStr(l_varLines(l_lngLines), "(") + 1), ")", ""), ",")
                For l_lngVariable = LBound(l_varVariables) To UBound(l_varVariables)
                    Set l_argArgument = New ScriptArgument
                    l_argArgument.Name = Trim(l_varVariables(l_lngVariable))
                    l_fnFunction.Arguments.Add l_argArgument
                Next l_lngVariable
                l_fnFunction.ReturnsValue = (l_strToken = "function")
                If l_clsClass Is Nothing Then
                    State.Functions.Add l_fnFunction, l_fnFunction.Name
                Else
                    l_clsClass.Functions.Add l_fnFunction, l_fnFunction.Name
                End If
            Case Else
                ' Variable
                If InStr(l_strToken, "data") Then
                    l_booData = True
                Else
                    l_booData = False
                End If
                l_varTokens = Split(l_varLines(l_lngLines), ",")
                l_varTokens(0) = Mid(Trim(l_varTokens(0)), InStr(Trim(l_varTokens(0)), " ") + 1)
                For l_lngVariable = LBound(l_varTokens) To UBound(l_varTokens)
                    Set l_varVariable = New ScriptVariable
                    l_varVariable.Name = Trim(l_varTokens(l_lngVariable))
                    If Not (l_fnFunction Is Nothing) Then
                        l_fnFunction.Variables.Add l_varVariable
                    ElseIf Not (l_clsClass Is Nothing) Then
                        l_clsClass.Variables.Add l_varVariable
                    Else
                        State.Variables.Add l_varVariable
                    End If
                Next l_lngVariable
            End Select
        Case "end"
            l_strToken = LCase(Trim(l_varTokens(1)))
            Select Case l_strToken
            Case "class"
                Set l_clsClass = Nothing
                Set l_fnFunction = Nothing
            Case "sub", "function"
                Set l_fnFunction = Nothing
            End Select
        End Select
    Next l_lngLines
    State.LinesOfCode = State.LinesOfCode + UBound(l_varLines) + 1
    m_strText = Replace(m_strText, "PublicData ", "Public ", , , vbTextCompare)
    m_strText = Replace(m_strText, "PrivateData ", "Private ", , , vbTextCompare)
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
On Error Resume Next
Dim l_lngChar As Long
Dim l_strLine As String
Dim l_strChar As String
Dim l_varLines As Variant
Dim l_lngLineCount As Long
Dim l_lngLineChar As Long
Dim l_lngLength As Long
Dim l_booLineBreak As Boolean
Dim l_booContinueLine As Boolean
'    If Not InStr(Text, vbCrLf) Then
'        ReDim l_varLines(0 To 0)
'        l_varLines(0) = Text
'        IScriptLanguage_SplitLines = l_varLines
'    End If
'    ReDim l_varLines(0 To 31)
'    l_lngLength = Len(Text)
'    l_lngChar = 1
'    l_lngLineChar = 1
'    l_strLine = String(256, " ")
'    Do
'        l_strChar = Mid(Text, l_lngChar, 1)
'        If l_strChar = "_" Then
'            l_booContinueLine = True
'        ElseIf l_strChar = vbCr Then
'            If l_booContinueLine Then
'                l_booContinueLine = False
'                l_booLineBreak = False
'            Else
'                l_booLineBreak = True
'            End If
'        ElseIf l_strChar = vbLf Then
'            If l_booLineBreak Then
'                l_varLines(l_lngLineCount) = Left(l_strLine, l_lngLineChar - 1)
'                l_lngLineChar = 1
'                l_lngLineCount = l_lngLineCount + 1
'                If l_lngLineCount > UBound(l_varLines) Then
'                    ReDim Preserve l_varLines(0 To UBound(l_varLines) + 32)
'                End If
'            Else
'                l_booContinueLine = False
'            End If
'        Else
'            If l_lngLineChar > Len(l_strLine) Then
'                l_strLine = l_strLine & String(256, " ")
'            End If
'            Mid(l_strLine, l_lngLineChar, 1) = l_strChar
'            l_lngLineChar = l_lngLineChar + 1
'            l_booContinueLine = False
'            l_booLineBreak = False
'        End If
'        l_lngChar = l_lngChar + 1
'    Loop Until l_lngChar > l_lngLength
'    If (l_lngLineChar > 1) Then
'        l_varLines(l_lngLineCount) = Left(l_strLine, l_lngLineChar - 1)
'        l_lngLineCount = l_lngLineCount + 1
'    End If
'    ReDim Preserve l_varLines(0 To l_lngLineCount - 1)
    IScriptLanguage_SplitLines = Split(Text, vbCrLf)
End Function

Private Property Get IScriptLanguage_ThisKeyword() As String
    IScriptLanguage_ThisKeyword = "Me"
End Property
