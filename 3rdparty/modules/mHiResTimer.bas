Attribute VB_Name = "mHiResTimer"
Option Explicit

Private Type TIMECAPS
   wPeriodMin As Long
   wPeriodMax As Long
End Type
Private Declare Function timeGetDevCaps Lib "winmm.dll" (lpTimeCaps As TIMECAPS, ByVal uSize As Long) As Long
Private Declare Function timeBeginPeriod Lib "winmm.dll" (ByVal uPeriod As Long) As Long
Private Declare Function timeEndPeriod Lib "winmm.dll" (ByVal uPeriod As Long) As Long
Private Declare Function timeSetEvent Lib "winmm.dll" (ByVal uDelay As Long, ByVal uResolution As Long, ByVal lpFunction As Long, ByVal dwUser As Long, ByVal uFlags As Long) As Long
Private Declare Function timeKillEvent Lib "winmm.dll" (ByVal uID As Long) As Long
Private Const TIME_ONESHOT = 0  '  program timer for single event
Private Const TIME_PERIODIC = 1  '  program for continuous periodic event
Private Declare Function PostMessage Lib "user32" Alias "PostMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Private Const TIMERR_NOERROR = 0                        '/* no error */
Private Const WM_USER = &H400
Private Const MyTimerMessage = WM_USER + &H2867
Private Const GWL_WNDPROC = (-4)
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Private Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
Private Declare Function GetProp Lib "user32" Alias "GetPropA" (ByVal hwnd As Long, ByVal lpString As String) As Long
Private Declare Function SetProp Lib "user32" Alias "SetPropA" (ByVal hwnd As Long, ByVal lpString As String, ByVal hData As Long) As Long
Private Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" (ByVal lpPrevWndFunc As Long, ByVal hwnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, lpvSource As Any, ByVal cbCopy As Long)

Private m_tTC As TIMECAPS
Private m_bBegin As Boolean
Private m_lID As Long
Private m_hWnd As Long
Private m_iRefCount As Long
Private m_lOldWndProc As Long

Private m_lPtr As Long

Public Sub StartTimer()
Dim lR As Long
   If Not m_bBegin Then
      lR = timeGetDevCaps(m_tTC, Len(m_tTC))
      If (lR = TIMERR_NOERROR) Then
         m_bBegin = True
         If CreateMessageWindow() Then
            timeBeginPeriod m_tTC.wPeriodMin
            m_lID = timeSetEvent(m_tTC.wPeriodMin, m_tTC.wPeriodMin, AddressOf TimerProc, 0, TIME_PERIODIC)
            If (m_lID <> 0) Then
               ' Ok!
               Exit Sub
            Else
               DestroyMessageWindow
            End If
         End If
         ' can't set timer up
         timeEndPeriod m_tTC.wPeriodMin
         m_bBegin = False
      End If
      ' can't get timer
      
   Else
      ' Timer/message window created.
      m_iRefCount = m_iRefCount + 1
   End If
End Sub
Public Sub AddObject(ByRef cThis As cHiResTimer)
Dim lC As Long
   lC = GetProp(m_hWnd, "vbalHiResTmr:Count")
   lC = lC + 1
   SetProp m_hWnd, "vbalHiResTmr:Count", lC
   SetProp m_hWnd, "vbalHiResTmr:Obj" & lC, ObjPtr(cThis)
   m_lPtr = ObjPtr(cThis)
End Sub
Public Sub RemoveObject(ByRef cThis As cHiResTimer)
Dim lC As Long
Dim i As Long
Dim lPtr As Long
Dim lIndex As Long

   lC = GetProp(m_hWnd, "vbalHiResTmr:Count")
   If (lC > 1) Then
      For i = 1 To lC
         lPtr = GetProp(m_hWnd, "vbalHiResTmr:Obj" & i)
         If (lPtr = ObjPtr(cThis)) Then
            lIndex = i
            Exit For
         End If
      Next i
      For i = lIndex To lC - 1
         lPtr = GetProp(m_hWnd, "vbalHiResTmr:Obj" & i + 1)
         SetProp m_hWnd, "vbalHiResTmr:Obj" & i, lPtr
      Next i
   End If
   lC = lC - 1
   SetProp m_hWnd, "vbalHiResTmr:Count", lC
   m_lPtr = 0
End Sub
Private Function CreateMessageWindow() As Boolean
   Load frmMessageWindow
   m_hWnd = frmMessageWindow.hwnd
   m_lOldWndProc = GetWindowLong(m_hWnd, GWL_WNDPROC)
   SetWindowLong frmMessageWindow.hwnd, GWL_WNDPROC, AddressOf WindowProc
   CreateMessageWindow = True
End Function
Private Sub DestroyMessageWindow()
   SetWindowLong m_hWnd, GWL_WNDPROC, m_lOldWndProc
   Unload frmMessageWindow
   m_hWnd = 0
End Sub

Public Sub StopTimer()
   m_iRefCount = m_iRefCount - 1
   If m_iRefCount < 0 Then
      m_iRefCount = 0
   End If
   If m_iRefCount = 0 Then
      DestroyMessageWindow
      If m_lID <> 0 Then
         timeKillEvent m_lID
         m_lID = 0
      End If
      If m_bBegin Then
         timeEndPeriod m_tTC.wPeriodMin
         m_bBegin = False
      End If
   End If
End Sub
Public Function TimerProc( _
      ByVal wTimerID As Long, ByVal iMsg As Long, _
      ByVal dwUser As Long, ByVal dw1 As Long, ByVal dw2 As Long _
   ) As Long
   ' The only functions you are allowed to call
   ' during a High-resolution timer event are
   ' PostMessage, timeGetSystemTime, timeGetTime,
   ' timeSetEvent, timeKillEvent,
   ' midiOutShortMsg, midiOutLongMsg,
   ' and OutputDebugString.
   PostMessage m_hWnd, MyTimerMessage, 0, 0
   
End Function
Public Function WindowProc( _
      ByVal hwnd As Long, ByVal iMsg As Long, _
      ByVal wParam As Long, ByVal lParam As Long _
   ) As Long
Dim lC As Long
Dim i As Long
Dim cHRT As cHiResTimer
Dim lPtr As Long
   If iMsg = MyTimerMessage Then
      lC = GetProp(m_hWnd, "vbalHiResTmr:Count")
      For i = 1 To lC
         lPtr = GetProp(m_hWnd, "vbalHiResTmr:Obj" & i)
         If (lPtr <> 0) Then
            Set cHRT = ObjectFromPtr(lPtr)
            cHRT.FireTimer
         End If
      Next i
   Else
      CallWindowProc m_lOldWndProc, hwnd, iMsg, wParam, lParam
   End If
End Function

Private Property Get ObjectFromPtr(ByVal lPtr As Long) As cHiResTimer
Dim oTHis As cHiResTimer
    CopyMemory oTHis, lPtr, 4
    Set ObjectFromPtr = oTHis
    CopyMemory oTHis, 0&, 4
End Property


