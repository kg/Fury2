VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAcceleratorManager"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IWindowsHook
Private m_booCurrentState(1 To 255) As Boolean
Private m_booCtrlState As Boolean
Private m_booShiftState As Boolean
Private m_colAccelerators As Fury2Collection
Private m_booAttached As Boolean
Private m_lngAttachedWindow As Long

Public Property Get KeyState(Index As Long) As Boolean
On Error Resume Next
    KeyState = m_booCurrentState(Index)
End Property

Public Sub RemoveAccelerator(Accelerator As cAccelerator)
On Error Resume Next
    m_colAccelerators.Remove m_colAccelerators.Find(Accelerator)
End Sub

Public Function AddAccelerator(KeyCode As Long, Hook As Object, Optional HookMethod As String = "AcceleratorPress", Optional CtrlState As Boolean = False, Optional ShiftState As Boolean = False, Optional AltState As Boolean = False) As cAccelerator
On Error Resume Next
Dim l_aclNew As cAccelerator
Dim l_aclFound As cAccelerator
    Set l_aclNew = New cAccelerator
    With l_aclNew
        .KeyCode = KeyCode
        .Alt = AltState
        .Ctrl = CtrlState
        .Shift = ShiftState
        Set .Hook = Hook
        .HookMethod = HookMethod
    End With
    Err.Clear
    Set l_aclFound = m_colAccelerators.Item(l_aclNew.KeyString)
    If l_aclFound Is Nothing Then
        m_colAccelerators.Add l_aclNew, l_aclNew.KeyString
        Set AddAccelerator = l_aclNew
        Set l_aclNew = Nothing
    End If
End Function

Public Property Get Accelerators() As Fury2Collection
On Error Resume Next
    Set Accelerators = m_colAccelerators
End Property

Public Sub Attach(Window As Long)
On Error Resume Next
    If m_booAttached Then
        Detach
    End If
    If m_booAttached Then
    Else
        m_lngAttachedWindow = Window
        m_booAttached = True
        GHook.InstallHook Me, WH_KEYBOARD
    End If
End Sub

Public Sub Detach()
On Error Resume Next
    If m_booAttached Then
        m_booAttached = False
        m_lngAttachedWindow = 0
        GHook.RemoveHook Me, WH_KEYBOARD
    End If
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colAccelerators = New Fury2Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Detach
End Sub

Private Function IWindowsHook_HookProc(ByVal eType As vbalWinHook6.EHTHookTypeConstants, ByVal nCode As Long, ByVal wParam As Long, ByVal lParam As Long, bConsume As Boolean) As Long
On Error Resume Next
Dim l_parKey As vbalWinHook6.cKeyboardlParam
Dim l_aclAccelerator As cAccelerator
Dim l_booResult As Boolean
Dim l_strKey As String
    If GetActiveWindow() <> m_lngAttachedWindow Then Exit Function
    If eType = WH_KEYBOARD Then
        Set l_parKey = KeyboardlParam(lParam)
        If (wParam = vbKeyControl) Or (wParam = vbKeyShift) Then
        ElseIf (l_parKey.KeyDown) Then
            If (m_booCurrentState(wParam)) Or (wParam < 1) Or (wParam > 255) Then
            Else
                l_strKey = Format(wParam, "000") & IIf(m_booCurrentState(vbKeyShift), "1", "0") & IIf(m_booCurrentState(vbKeyControl), "1", "0") & IIf(l_parKey.Alt, "1", "0")
                Set l_aclAccelerator = m_colAccelerators.Item(l_strKey)
                If l_aclAccelerator Is Nothing Then
                ElseIf l_aclAccelerator.Hook Is Nothing Then
                Else
                    Err.Clear
                    l_booResult = True
                    l_booResult = CallByName(l_aclAccelerator.Hook, l_aclAccelerator.HookMethod, VbMethod)
                    If Err <> 0 Then
                        Err.Clear
                        l_booResult = CallByName(l_aclAccelerator.Hook, l_aclAccelerator.HookMethod, VbMethod, l_aclAccelerator)
                        If Err <> 0 Then
                            Err.Clear
                            CallByName l_aclAccelerator.Hook, l_aclAccelerator.HookMethod, VbMethod
                            If Err <> 0 Then
                                CallByName l_aclAccelerator.Hook, l_aclAccelerator.HookMethod, VbMethod, l_aclAccelerator
                                Err.Clear
                            End If
                        End If
                    End If
                    bConsume = l_booResult
                End If
            End If
        End If
        If (l_parKey.KeyDown) Then
            m_booCurrentState(wParam) = True
        ElseIf (l_parKey.KeyUp) Then
            m_booCurrentState(wParam) = False
        End If
        Set l_parKey = Nothing
    End If
End Function
