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
Option Explicit
'
'   ::fury2 scripting engine wrapper::
'

Public ScriptsRunning As Long

Private WithEvents m_Engine As SCRHostLib.Host
Attribute m_Engine.VB_VarHelpID = -1

Private m_Language As IScriptLanguage

Public Event Error(ByVal LineNumber As Long, ByVal Character As Long, ByRef Description As String)
Public Event InterruptPoll(ByRef Interrupt As Boolean)
Public Event ExecutionBegin()
Public Event ExecutionEnd()

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)

Public Property Get ScriptRunning() As Boolean
    ScriptRunning = (ScriptsRunning > 0)
End Property

Public Property Get Namespace(Optional Name As String = vbNullString) As Object
On Error Resume Next
Dim l_objModule As Object
    Err.Clear
    Set l_objModule = m_Engine.GetScriptDispatch(Name)
    If l_objModule Is Nothing Or Err.Number <> 0 Then
        Set Namespace = Nothing
        Err.Clear
    Else
        Set Namespace = l_objModule
    End If
End Property

Public Function ModuleExists(Optional Name As String = vbNullString) As Boolean
On Error Resume Next
Dim l_objModule As Object
    Err.Clear
    Set l_objModule = m_Engine.GetScriptDispatch(Name)
    If l_objModule Is Nothing Or Err.Number <> 0 Then
        ModuleExists = False
        Err.Clear
    Else
        ModuleExists = True
    End If
End Function

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
Dim l_objNamespace As Object
Dim l_lngBefore As Long
    If m_Engine Is Nothing Then Exit Function
    Err.Clear
    Set l_objNamespace = Namespace(vbNullString)
    If l_objNamespace Is Nothing Then Exit Function
    Err.Clear
    l_lngBefore = ScriptsRunning
    If IsMissing(Parameters) Then
        Exec = CallByName(l_objNamespace, Name, VbMethod)
    Else
        ' I'll come up with a beautiful hack for this... later
        Select Case UBound(Parameters)
        Case 0
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0))
        Case 1
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0), Parameters(1))
        Case 2
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0), Parameters(1), Parameters(2))
        Case 3
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3))
        Case 4
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4))
        Case 5
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5))
        Case 6
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6))
        Case 7
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6), Parameters(7))
        Case Else
            Exec = CallByName(l_objNamespace, Name, VbMethod, Parameters)
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
    Eval = m_Engine.EvalExpr(ParseCodes(Expr), "")
    If Err.Number = 438 Then
        Err.Clear
        Set Eval = m_Engine.EvalExpr(ParseCodes(Expr), "")
    End If
End Function

Public Sub CreateModule(ByRef Name As String, ByRef Code As String)
On Error Resume Next
    m_Engine.AddNamespace Name
    m_Engine.ParseScriptText Code, Name
End Sub

Public Function ParseCodes(ByRef Str As String) As String
On Error Resume Next
    ParseCodes = Language.ParseCode(Str)
End Function

Public Property Get Obj() As Object
    Set Obj = m_Engine
End Property

Sub AddObject(Name As String, Obj As Object, Optional AddMembers As Boolean = True, Optional AddEvents As Boolean = False, Optional Visible As Boolean = True)
On Error Resume Next
    m_Engine.AddObject Name, AddMembers, AddEvents, Visible, Obj
End Sub

Sub AddCode(Code As String)
On Error Resume Next
    m_Engine.ParseScriptText ParseCodes(Code)
End Sub

Sub Init(Optional ByVal hWnd As Long = 0)
On Error Resume Next
    If m_Engine Is Nothing Then
    Else
        m_Engine.SetScriptState SS_DISCONNECTED
        Set m_Engine = Nothing
    End If
    Set m_Engine = New Host
    m_Engine.hWnd = hWnd
    m_Engine.InitEngineByName Language.Name
    m_Engine.hWnd = hWnd
    m_Engine.SetScriptState SS_STARTED
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set m_Engine = Nothing
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    If m_Engine Is Nothing Then
    Else
        m_Engine.SetScriptState SS_DISCONNECTED
        Set m_Engine = Nothing
    End If
End Sub

Private Function m_Engine_InterruptPoll() As Boolean
On Error Resume Next
Dim l_booResult As Boolean
    RaiseEvent InterruptPoll(l_booResult)
    m_Engine_InterruptPoll = l_booResult
End Function

Private Sub m_Engine_OnEnterScript()
On Error Resume Next
    ScriptsRunning = ScriptsRunning + 1
    RaiseEvent ExecutionBegin
End Sub

Private Sub m_Engine_OnLeaveScript()
On Error Resume Next
    ScriptsRunning = ScriptsRunning - 1
    RaiseEvent ExecutionEnd
End Sub

Private Sub m_Engine_OnScriptError(ByVal ulLineNumber As Long, ByVal ichCharPosition As Long, ByVal bstrDescription As String)
On Error Resume Next
    RaiseEvent Error(ulLineNumber, ichCharPosition, bstrDescription)
End Sub

Private Sub m_Engine_OnScriptTerminate(ByVal varResult As Variant, ByVal bstrError As String)
On Error Resume Next
End Sub
