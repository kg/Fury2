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
Private Declare Function GetKeyState Lib "user32" (ByVal nVirtKey As Long) As Integer
Private Const VK_CONTROL = &H11
Private Const VK_SHIFT = &H10
Private m_booCurrentState(1 To 255) As Boolean
Private m_booCtrlState As Boolean
Private m_booShiftState As Boolean
Private m_colAccelerators As Fury2Collection
Private m_booAttached As Boolean
Private m_lngAttachedWindow As Long
Private m_booDeferredInvoke As Boolean
Private m_booEnabled As Boolean

Public Property Get Enabled() As Boolean
    Enabled = m_booEnabled
End Property

Public Property Let Enabled(ByVal NewValue As Boolean)
    m_booEnabled = NewValue
End Property

Public Property Get DeferredInvoke() As Boolean
    DeferredInvoke = m_booDeferredInvoke
End Property

Public Property Let DeferredInvoke(ByVal NewValue As Boolean)
    m_booDeferredInvoke = NewValue
End Property

Public Property Get KeyState(Index As Long) As Boolean
On Error Resume Next
    KeyState = m_booCurrentState(Index)
End Property

Public Sub RemoveAccelerator(Accelerator As cAccelerator)
On Error Resume Next
    m_colAccelerators.Remove m_colAccelerators.Find(Accelerator)
End Sub

Public Function AddAccelerator(KeyCode As Long, Evt As ngEvent, Optional CtrlState As Boolean = False, Optional ShiftState As Boolean = False, Optional AltState As Boolean = False) As cAccelerator
On Error Resume Next
Dim l_aclNew As cAccelerator
Dim l_aclFound As cAccelerator
    Set l_aclNew = New cAccelerator
    With l_aclNew
        .KeyCode = KeyCode
        .Alt = AltState
        .Ctrl = CtrlState
        .Shift = ShiftState
        Set .Hook = Evt
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
    m_booEnabled = True
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
Dim l_wndActive As Long
    l_wndActive = GetActiveWindow()
    If GetParent(l_wndActive) = 0 Then
        ' This doesn't work right for some reason
        'If l_wndActive <> m_lngAttachedWindow Then Exit Function
    End If
    If eType = WH_KEYBOARD Then
        Set l_parKey = KeyboardlParam(lParam)
        If (wParam = vbKeyControl) Or (wParam = vbKeyShift) Then
        ElseIf (l_parKey.KeyDown) Then
            If (m_booCurrentState(wParam)) Or (wParam < 1) Or (wParam > 255) Then
            Else
                l_strKey = Format(wParam, "000") & IIf(GetKeyState(VK_SHIFT) < 0, "1", "0") & IIf(GetKeyState(VK_CONTROL) < 0, "1", "0") & IIf(l_parKey.Alt, "1", "0")
                Debug.Print l_strKey
                Set l_aclAccelerator = m_colAccelerators.Item(l_strKey)
                If l_aclAccelerator Is Nothing Then
                ElseIf l_aclAccelerator.Hook Is Nothing Then
                Else
                    If m_booEnabled Then
                        Err.Clear
                        l_booResult = True
                        If m_booDeferredInvoke Then
                            frmInvoke.Queue.Add l_aclAccelerator.Hook
                            frmInvoke.tmrInvoke.Enabled = True
                        Else
                            l_booResult = l_aclAccelerator.Hook.Invoke()
                        End If
                        bConsume = l_booResult
                    Else
                        bConsume = False
                    End If
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
