Attribute VB_Name = "HookEvents"
Option Explicit

' max number of available window procedure address
Const MAX_WNDPROC = 10

Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" (ByVal lpPrevWndFunc As Long, ByVal hwnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Const GWL_WNDPROC = -4

Private Type TWndInfo
    ' handle of subclassed window - zero if none
    hwnd As Long
    ' address of original window procedure
    wndProcAddr As Long
    ' address of custom window procedure
    localProcAddr As Long
    ' pointer to CFormEvent object to be notified
    obj_ptr As Long
End Type

' data about forms being subclassed is kept in this structure
Dim wndInfo(1 To MAX_WNDPROC) As TWndInfo

Private Sub InitLocalData()

    ' initialize the wndInfo() array
    On Error Resume Next
    
    wndInfo(1).localProcAddr = ProcAddr(AddressOf WndProc1)
    wndInfo(2).localProcAddr = ProcAddr(AddressOf WndProc2)
    wndInfo(3).localProcAddr = ProcAddr(AddressOf WndProc3)
    wndInfo(4).localProcAddr = ProcAddr(AddressOf WndProc4)
    wndInfo(5).localProcAddr = ProcAddr(AddressOf WndProc5)
    wndInfo(6).localProcAddr = ProcAddr(AddressOf WndProc6)
    wndInfo(7).localProcAddr = ProcAddr(AddressOf WndProc7)
    wndInfo(8).localProcAddr = ProcAddr(AddressOf WndProc8)
    wndInfo(9).localProcAddr = ProcAddr(AddressOf WndProc9)
    wndInfo(10).localProcAddr = ProcAddr(AddressOf WndProc10)

End Sub

Function ProcAddr(ByVal address As Long)
    ' support routine for assigning the address
    ' of a routine to a variable
    ProcAddr = address
End Function

Function HookWindow(obj As CFormEvents, ByVal hwnd As Long) As Integer
    
    '-------------------------------------------------
    ' start the subclassing of a window
    ' Return the index of the slot used
    '  or zero if error (no more available slots)
    '-------------------------------------------------
    
    Dim Index As Integer
    On Error Resume Next
    
    ' do we need to init the wndInfo() array?
    If wndInfo(1).localProcAddr = 0 Then InitLocalData
    
    ' search the first available slot
    Index = 1
    Do
        If wndInfo(Index).hwnd = 0 Then Exit Do
        Index = Index + 1
    Loop Until Index > MAX_WNDPROC
    
    ' return zero if no available slots
    If Index > MAX_WNDPROC Then
        HookWindow = 0
        Exit Function
    End If
    
    ' save data in local structure
    With wndInfo(Index)
        .obj_ptr = ObjPtr(obj)
        .hwnd = hwnd
        ' enforce new window procedure, get old address
        .wndProcAddr = SetWindowLong(hwnd, GWL_WNDPROC, .localProcAddr)
    End With
    
    ' return the index to signal that everything is OK
    HookWindow = Index
End Function

Sub UnhookWindow(Index As Integer)

    ' end the subclassing of a window, free the slot
    
    With wndInfo(Index)
        ' stop subclassing
        SetWindowLong .hwnd, GWL_WNDPROC, .wndProcAddr
    
        ' reset all fields , except localProcAddr
        .hwnd = 0
        .wndProcAddr = 0
        .obj_ptr = 0
    End With
                
End Sub

Function WndProc(ByVal ndx As Integer, ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    
    ' a generic window procedure, called by the
    ' many WndProc<n> procedures in the pool
    Dim preProcess As Boolean
    Dim retValue As Long
    Dim Cancel As Boolean
    Dim obj As CFormEvents
    
    On Error Resume Next
    
    ' some defensive programming
    ' check that hWnd is correct for that slot
    If wndInfo(ndx).hwnd <> hwnd Then
        ' somehow, the object got destroyed without first calling UnhookMessages
        UnhookWindow ndx
        Exit Function
    End If
    
    ' see if the message is to be subclassed, and when
    Select Case uMsg
    Case WM_ACTIVATEAPP, WM_MOVE, WM_MOVING, WM_SIZING, WM_GETMINMAXINFO, WM_NCHITTEST, WM_SETCURSOR, WM_COMPACTING, WM_DISPLAYCHANGE
    Case WM_PAINT
        preProcess = True
    Case Else
        ' for all other msgs, call the original window proc and exit
        WndProc = CallWindowProc(wndInfo(ndx).wndProcAddr, hwnd, uMsg, wParam, lParam)
        Exit Function
    End Select
    
    ' build a reference to the connected object
    CopyMemory obj, wndInfo(ndx).obj_ptr, 4
    ' defensive programmig: check that the object
    ' still exists, and that the hWnds match
    If obj.hwnd <> hwnd Then
        ' this line is executed both if hWnd's don't match
        ' and if the class does not exist (On Error Resume Next)
        
        ' call the standard window procedure
        WndProc = CallWindowProc(wndInfo(ndx).wndProcAddr, hwnd, uMsg, wParam, lParam)
        ' but then stop subclassing
        UnhookWindow ndx
        GoTo WndProc_Exit
    End If
    
    If preProcess Then
        ' message pre-processing
        ' first, notify the event to the CFormEvents object
        WndProc = obj.BeforeMessage(hwnd, uMsg, wParam, lParam, Cancel)
        ' exit now if the object cancelled standard window proc processing
        If Cancel Then GoTo WndProc_Exit
    End If
            
    ' call the standard window procedure
    retValue = CallWindowProc(wndInfo(ndx).wndProcAddr, hwnd, uMsg, wParam, lParam)
    
    ' message post-processing
    ' notify the event to the CFormEvents object
    obj.AfterMessage hwnd, uMsg, wParam, lParam, retValue
    WndProc = retValue
    
WndProc_Exit:
    ' delete temporary object without Setting it to Nothing
    ' which would cause a GPF since ref counter is not correct
    CopyMemory obj, 0&, 4
    
End Function

Function WndProc1(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc1 = WndProc(1, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc2(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc2 = WndProc(2, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc3(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc3 = WndProc(3, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc4(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc4 = WndProc(4, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc5(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc5 = WndProc(5, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc6(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc6 = WndProc(6, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc7(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc7 = WndProc(7, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc8(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc8 = WndProc(8, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc9(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc9 = WndProc(9, hwnd, uMsg, wParam, lParam)
End Function
Function WndProc10(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    WndProc10 = WndProc(10, hwnd, uMsg, wParam, lParam)
End Function

