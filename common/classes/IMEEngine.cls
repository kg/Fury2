VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IMEEngine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Event LanguageChanged()
Public Event CompositionBegin()
Public Event CompositionUpdate()
Public Event CompositionComplete()
Public Context As Long
Public CharacterSet As Long
Public InputLocale As Long
Public OldContext As Long
Public Window As Long
Public Enabled As Boolean
Public Text As String
Public CursorPos As Long
Public SelectedCandidate As Long
Private m_strCandidates() As String
Private m_booComposing As Boolean
Implements ISubclass

Friend Sub RefreshCandidates()
On Error Resume Next
Dim l_lngSize As Long
Dim l_lngPointer As Long, l_lngOffset As Long, l_lngAddress As Long, l_lngNextOffset As Long
Dim l_lngCandidates As Long
Dim l_hdrHeader As CANDIDATELISTHeader
Dim l_bytCandidate() As Byte
    If m_booComposing Then
        l_lngSize = ImmGetCandidateList(Context, 0, 0, 0)
        If l_lngSize >= LenB(l_hdrHeader) Then
            l_lngPointer = AllocateMemory(l_lngSize)
            l_lngSize = ImmGetCandidateList(Context, 0, l_lngPointer, l_lngSize)
            CopyMemory l_hdrHeader, ByVal l_lngPointer, LenB(l_hdrHeader)
            SelectedCandidate = l_hdrHeader.Selection
    '        Debug.Print l_hdrHeader.Count & " candidates: ";
            ReDim m_strCandidates(0 To l_hdrHeader.Count - 1)
            For l_lngCandidates = 0 To l_hdrHeader.Count - 1
                l_lngAddress = AddUInts(l_lngPointer, LenB(l_hdrHeader) + (4 * l_lngCandidates))
                CopyMemory l_lngOffset, ByVal l_lngAddress, 4
                If (l_lngCandidates = l_hdrHeader.Count - 1) Then
                    l_lngNextOffset = l_hdrHeader.Size
                Else
                    l_lngAddress = AddUInts(l_lngPointer, LenB(l_hdrHeader) + (4 * (l_lngCandidates + 1)))
                    CopyMemory l_lngNextOffset, ByVal l_lngAddress, 4
                End If
                l_lngSize = l_lngNextOffset - l_lngOffset
                ReDim l_bytCandidate(0 To l_lngSize)
                CopyMemory ByVal VarPtr(l_bytCandidate(0)), ByVal AddUInts(l_lngPointer, l_lngOffset), l_lngSize
                m_strCandidates(l_lngCandidates) = CStr(l_bytCandidate)
    '            Debug.Print CStr(l_bytCandidate);
            Next l_lngCandidates
    '        Debug.Print
            DeallocateMemory l_lngPointer
        Else
            Erase m_strCandidates
        End If
    Else
        Erase m_strCandidates
    End If
End Sub

Public Property Get Candidates() As String()
On Error Resume Next
    Candidates = m_strCandidates
End Property

Public Property Get Description() As String
On Error Resume Next
Dim l_strName As String
Dim l_lngLength As Long
    l_lngLength = ImmGetDescription(InputLocale, 0, 0)
    If l_lngLength > 0 Then
        l_strName = Space(l_lngLength) + ChrW(0)
        l_lngLength = ImmGetDescription(InputLocale, StrPtr(l_strName), LenB(l_strName) - 2)
        Description = left(l_strName, l_lngLength)
    End If
End Property

Public Property Get GuideLine() As String
On Error Resume Next
Dim l_strText As String
Dim l_lngLength As Long
    l_lngLength = ImmGetGuideLine(Context, GGL_STRING, 0, 0)
    If l_lngLength > 0 Then
        l_strText = Space(l_lngLength \ 2) + ChrW(0)
        l_lngLength = ImmGetGuideLine(Context, GGL_STRING, StrPtr(l_strText), LenB(l_strText) - 2)
        GuideLine = left(l_strText, l_lngLength)
    End If
End Property

Public Sub Hook(ByVal Window As Long)
On Error Resume Next
    If Context <> 0 Then
        Unhook Window
    End If
    If InIDE Then Exit Sub
    Me.Window = Window
    Context = ImmCreateContext()
    AttachMessage Me, Window, WM_INPUTLANGCHANGE
    AttachMessage Me, Window, WM_IME_NOTIFY
    AttachMessage Me, Window, WM_IME_SETCONTEXT
    AttachMessage Me, Window, WM_IME_STARTCOMPOSITION
    AttachMessage Me, Window, WM_IME_COMPOSITION
    AttachMessage Me, Window, WM_IME_ENDCOMPOSITION
    OldContext = ImmAssociateContext(Window, Context)
    ImmSetOpenStatus Context, 1
End Sub

Public Sub Unhook(ByVal Window As Long)
On Error Resume Next
    Call ImmAssociateContext(Window, OldContext)
    ImmDestroyContext Context
    DetachMessage Me, Window, WM_INPUTLANGCHANGE
    DetachMessage Me, Window, WM_IME_NOTIFY
    DetachMessage Me, Window, WM_IME_SETCONTEXT
    DetachMessage Me, Window, WM_IME_STARTCOMPOSITION
    DetachMessage Me, Window, WM_IME_COMPOSITION
    DetachMessage Me, Window, WM_IME_ENDCOMPOSITION
    Context = 0
    OldContext = 0
    Window = 0
End Sub

Public Sub Reinstall()
On Error Resume Next
    If Context <> 0 Then
        Call ImmAssociateContext(Window, OldContext)
        ImmDestroyContext Context
    End If
    Context = ImmCreateContext()
    OldContext = ImmAssociateContext(Window, Context)
    ImmSetOpenStatus Context, 1
End Sub

Public Property Get Composing() As Boolean
On Error Resume Next
    Composing = m_booComposing
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Enabled = True
End Sub

Private Property Let ISubclass_MsgResponse(ByVal RHS As EMsgResponse)
End Property

Private Property Get ISubclass_MsgResponse() As EMsgResponse
On Error Resume Next
    Select Case CurrentMessage
    Case WM_INPUTLANGCHANGE
        ISubclass_MsgResponse = emrConsume
    Case WM_IME_STARTCOMPOSITION
        ISubclass_MsgResponse = emrConsume
    Case WM_IME_COMPOSITION
        ISubclass_MsgResponse = emrConsume
    Case WM_IME_ENDCOMPOSITION
        ISubclass_MsgResponse = emrConsume
    Case WM_IME_SETCONTEXT
        ISubclass_MsgResponse = emrConsume
    Case WM_IME_NOTIFY
        ISubclass_MsgResponse = emrConsume
    Case Else
        ISubclass_MsgResponse = emrPreprocess
    End Select
End Property

Friend Sub CompositionStarted()
On Error Resume Next
    Text = ""
    CursorPos = ImmGetCompositionString(Context, GCS_CURSORPOS, 0, 0)
    m_booComposing = True
    RaiseEvent CompositionBegin
End Sub

Friend Sub CompositionEnded()
On Error Resume Next
    If m_booComposing Then
        RaiseEvent CompositionComplete
        Text = ""
        CursorPos = 0
    End If
    m_booComposing = False
End Sub

Private Function ISubclass_WindowProc(ByVal hwnd As Long, ByVal iMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
On Error Resume Next
Dim l_lngLength As Long
Dim l_bytText() As Byte
    Select Case iMsg
    Case WM_INPUTLANGCHANGE
        CompositionEnded
        Me.CharacterSet = wParam
        Me.InputLocale = lParam
        RaiseEvent LanguageChanged
        Reinstall
        Debug.Print "IME change: "; Description
    Case WM_IME_NOTIFY
'        Debug.Print "WM_IME_NOTIFY ";
        Select Case wParam
        Case IMN_OPENCANDIDATE
'            Debug.Print "IMN_OPENCANDIDATE"
            RefreshCandidates
        Case IMN_CHANGECANDIDATE
'            Debug.Print "IMN_CHANGECANDIDATE"
            RefreshCandidates
        Case IMN_CLOSECANDIDATE
'            Debug.Print "IMN_CLOSECANDIDATE"
            RefreshCandidates
        Case Else
        End Select
    Case WM_IME_STARTCOMPOSITION
        CompositionStarted
        RefreshCandidates
    Case WM_IME_COMPOSITION
        l_lngLength = ImmGetCompositionString(Context, GCS_COMPSTR, 0, 0)
        If l_lngLength < 0 Then
            Debug.Print "Error retrieving composition string: " & l_lngLength
        Else
            ReDim l_bytText(0 To l_lngLength)
            l_lngLength = ImmGetCompositionString(Context, GCS_COMPSTR, ByVal VarPtr(l_bytText(0)), l_lngLength)
            Text = CStr(l_bytText)
            CursorPos = ImmGetCompositionString(Context, GCS_CURSORPOS, 0, 0)
            RaiseEvent CompositionUpdate
        End If
    Case WM_IME_ENDCOMPOSITION
        l_lngLength = ImmGetCompositionString(Context, GCS_RESULTSTR, 0, 0)
        If l_lngLength < 0 Then
            Debug.Print "Error retrieving composition string: " & l_lngLength
        Else
            ReDim l_bytText(0 To l_lngLength)
            l_lngLength = ImmGetCompositionString(Context, GCS_RESULTSTR, ByVal VarPtr(l_bytText(0)), l_lngLength)
            Text = CStr(l_bytText)
            CompositionEnded
            RefreshCandidates
        End If
    Case WM_IME_SETCONTEXT
        ImmSetOpenStatus Context, Abs(CLng(Enabled))
        wParam = 1
    Case Else
    End Select
End Function

