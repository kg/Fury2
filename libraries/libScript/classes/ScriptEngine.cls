VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ScriptEngine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
'
'   ::fury2 scripting engine wrapper::
'

Private WithEvents m_Engine As MSScriptControl.ScriptControl
Attribute m_Engine.VB_VarHelpID = -1

Private m_Language As IScriptLanguage

Public Event Error()
Public Event Timeout()

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)

Public Function CreateSingleton(Name As String, Code As String) As Object
On Error Resume Next
Dim l_strScript As String
    Set CreateSingleton = Eval("New" & Name)
    If CreateSingleton Is Nothing Then
        l_strScript = Language.GenerateClass(Name, Code) & Language.LineTerminator & Language.GenerateAssignment("New" & Name, Language.GenerateInstantiation(Name), True)
        AddCode l_strScript
        Set CreateSingleton = Eval("New" & Name)
    End If
End Function

Public Function Exec(Name As String, ParamArray Parameters() As Variant) As Variant
On Error Resume Next
    If m_Engine Is Nothing Then Exit Function
    Err.Clear
    If IsMissing(Parameters) Then
        Exec = m_Engine.Run(Name)
    Else
        ' I'll come up with a beautiful hack for this... later
        Select Case UBound(Parameters)
        Case 0
            Exec = m_Engine.Run(Name, Parameters(0))
        Case 1
            Exec = m_Engine.Run(Name, Parameters(0), Parameters(1))
        Case 2
            Exec = m_Engine.Run(Name, Parameters(0), Parameters(1), Parameters(2))
        Case 3
            Exec = m_Engine.Run(Name, Parameters(0), Parameters(1), Parameters(2), Parameters(3))
        Case 4
            Exec = m_Engine.Run(Name, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4))
        Case 5
            Exec = m_Engine.Run(Name, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5))
        Case 6
            Exec = m_Engine.Run(Name, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6))
        Case 7
            Exec = m_Engine.Run(Name, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6), Parameters(7))
        Case Else
            Exec = m_Engine.Run(Name, Parameters)
        End Select
    End If
    If Err <> 0 Then
        Err.Clear
    End If
End Function

Public Property Get Language() As IScriptLanguage
On Error Resume Next
    Set Language = m_Language
End Property

Public Property Set Language(NewLanguage As IScriptLanguage)
On Error Resume Next
    Set m_Language = NewLanguage
End Property

Public Function LoadLanguage(Language As String) As IScriptLanguage
On Error Resume Next
    Select Case LCase(Trim(Language))
    Case "vbscript", "vb", "vbs", "vba"
        Set LoadLanguage = New VBScriptLanguage
    Case "python", "py"
'        Set LoadLanguage = New PythonLanguage
    Case "jscript", "javascript", "js", "java script", "ecmascript"
        Set LoadLanguage = New JScriptLanguage
    Case Else
    End Select
End Function

Public Function Eval(ByRef Expr As String)
On Error Resume Next
    Err.Clear
    Eval = m_Engine.Eval(ParseCodes(Expr))
    If Err = 438 Then
        Set Eval = m_Engine.Eval(ParseCodes(Expr))
    End If
End Function

Public Sub CreateModule(ByRef Name As String, ByRef Code As String)
On Error Resume Next
    With m_Engine.Modules.Add(Name)
        .AddCode ParseCodes(Code)
    End With
End Sub

Public Function ParseCodes(ByRef Str As String) As String
On Error Resume Next
    ParseCodes = Language.ParseCode(Str)
End Function

Public Property Get Error() As Error
    Set Error = m_Engine.Error
End Property

Public Property Get Obj() As Object
    Set Obj = m_Engine
End Property

Sub AddObject(Name As String, Obj As Object, Optional AddMembers As Boolean = True)
On Error Resume Next
    m_Engine.AddObject Name, Obj, AddMembers
End Sub

Sub AddCode(Code As String)
On Error Resume Next
    m_Engine.AddCode ParseCodes(Code)
End Sub

Sub Init(Optional AllowUI As Boolean = True, Optional Safe As Boolean = False)
On Error Resume Next
    m_Engine.AllowUI = AllowUI
    m_Engine.Language = m_Language.Name
    m_Engine.UseSafeSubset = Safe
    m_Engine.Timeout = NoTimeout
    m_Engine.Reset
End Sub

Sub Reset()
On Error Resume Next
    m_Engine.Reset
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set m_Engine = New ScriptControl
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set m_Engine = Nothing
End Sub

Private Sub m_Engine_Error()
On Error Resume Next
    If m_Engine.Error.Description = "Name Redefined" Then
        m_Engine.Error.Clear
        Err.Clear
    Else
        RaiseEvent Error
        m_Engine.Error.Clear
    End If
End Sub

Private Sub m_Engine_Timeout()
On Error Resume Next
    RaiseEvent Timeout
    m_Engine.Error.Clear
End Sub
