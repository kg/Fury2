VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CFormEvents"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Event ActivateApp()
Event DeactivateApp()
Event Move()
Event GetMinMaxInfo(MaxSizeX As Long, MaxSizeY As Long, MaxPosX As Long, MaxPosY As Long, MinTrackSizeX As Long, MinTrackSizeY As Long, MaxTrackSizeX As Long, MaxTrackSizeY As Long)
Event Paint(X1 As Single, Y1 As Single, X2 As Single, Y2 As Single)
Event NCHitTest(ByVal X As Integer, ByVal Y As Integer, hitCode As Long)
Event MouseEnter(ByVal ctrl As Control)
Event MouseExit(ByVal ctrl As Control)
Event CompactingMemory()
Event DisplayChanged(ByVal newWidth As Integer, ByVal newHeight As Integer, ByVal numberOfColors As Long)
Event Resizing(X1 As Long, Y1 As Long, X2 As Long, Y2 As Long, ByVal draggedBorder As Integer)
Event Moving(X1 As Long, Y1 As Long, X2 As Long, Y2 As Long, ByVal draggedBorder As Integer)

Private wndInfoIndex As Integer
Private m_Form As Form
Private m_hWnd As Long

Private ctrlUnderCursor As Control

Property Get hwnd() As Long
    ' simply return the handle of hooked window
    hwnd = m_hWnd
End Property

Property Get HookedForm() As Form
    'simply return a reference to the hooked window
    Set HookedForm = m_Form
End Property

Property Set HookedForm(new_Form As Form)
    ' start the subclassing of a new form

    ' do nothing if no real change
    If new_Form Is Nothing Then
        If m_hWnd = 0 Then Exit Property
    Else
        If new_Form.hwnd = m_hWnd Then Exit Property
    End If
    
    ' stop current subclassing, if any
    If m_hWnd Then
        UnhookWindow wndInfoIndex
        wndInfoIndex = 0
    End If
    
    ' save new value
    Set m_Form = new_Form
    
    ' exit if trying to assign a null object
    ' (same as stopping subclassing)
    If new_Form Is Nothing Then
        m_hWnd = 0
        Exit Property
    End If
    ' save window handle
    m_hWnd = new_Form.hwnd
    
    ' start subclassing, save returned index
    wndInfoIndex = HookWindow(Me, m_hWnd)
    ' signal any error
    If wndInfoIndex = 0 Then
        Err.Raise 999, "CFormEvents class", "Too many subclassed windows"
    End If
End Property

Private Sub Class_Terminate()
    ' stop subclassing when the object is destroyed
    If wndInfoIndex Then UnhookWindow wndInfoIndex
End Sub

Friend Function BeforeMessage(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long, Cancel As Boolean) As Long
    ' this function is called before the message is
    ' processed by the standard window procedure
    
    Select Case uMsg
    Case WM_PAINT
        ' Windows is requesting the window to repaint itself
        Dim lp As Rect
        GetUpdateRect m_hWnd, lp, False
        RaiseEvent Paint(m_Form.ScaleX(lp.Left, vbPixels), m_Form.ScaleY(lp.Top, vbPixels), _
            m_Form.ScaleX(lp.Right, vbPixels), m_Form.ScaleY(lp.Bottom, vbPixels))
    End Select
End Function

Friend Sub AfterMessage(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long, retVal As Long)
    ' this function is called after the message has been
    ' processed by the standard window procedure
    On Error Resume Next
    
    Select Case uMsg
    Case WM_ACTIVATEAPP
        ' application has been activated or deactivated
        If wParam Then
            RaiseEvent ActivateApp
        Else
            RaiseEvent DeactivateApp
        End If
    Case WM_MOVE
        ' window has moved
        RaiseEvent Move
    Case WM_SIZING, WM_MOVING
        ' window is being resized OR moved
        ' (code is very similar for the two events
        Dim i As Integer
        ReDim coords(0 To 3) As Long, saveCoords(0 To 3) As Long
        ' retrieve current tracking rectangle
        ' uses an array instead of a RECT structure because
        ' it makes comparisons much easier
        CopyMemory coords(0), ByVal lParam, 16
        ' save once again for later comparison
        CopyMemory saveCoords(0), ByVal lParam, 16
        ' raise thre proper event
        If uMsg = WM_SIZING Then
            RaiseEvent Resizing(coords(0), coords(1), coords(2), coords(3), wParam)
        Else
            RaiseEvent Moving(coords(0), coords(1), coords(2), coords(3), wParam)
        End If
        ' we must return True to Windows if user modified any value
        For i = 0 To 3
            If coords(i) <> saveCoords(i) Then retVal = True
        Next
        ' save modified values back in structure
        If retVal Then
            CopyMemory ByVal lParam, coords(0), 16
        End If
    Case WM_GETMINMAXINFO
        ' Windows is querying the window for its
        ' minimum and maximum size and position
        Dim mmInfo As MINMAXINFO
        ' read contents of structure pointed to by lParam
        CopyMemory mmInfo, ByVal lParam, Len(mmInfo)
        ' raise an event in the form module
        With mmInfo
            RaiseEvent GetMinMaxInfo(.ptMaxSize.X, .ptMaxSize.Y, _
                .ptMaxPosition.X, .ptMaxPosition.Y, _
                .ptMinTrackSize.X, .ptMinTrackSize.Y, _
                .ptMaxTrackSize.X, .ptMaxTrackSize.Y)
        End With
        ' copy back in the original structure
        CopyMemory ByVal lParam, mmInfo, Len(mmInfo)
    Case WM_NCHITTEST
        ' Windows is query the form about which component is under
        ' the mouse cursor
        RaiseEvent NCHitTest(lParam And &HFFFF&, lParam \ &H10000, retVal)
    Case WM_SETCURSOR
        Dim ctrlHWnd As Long
        Dim mouseAction As Long
        Dim hitTest As Long
        Dim ctrl As Control
        
        ' wParam holds the handle of the control under the cursor
        ctrlHWnd = wParam
        ' code for mouse action is in high word of lParam
        mouseAction = (lParam \ &H10000)
        ' hit test is in the low word (not used here)
        hitTest = lParam And &HFFFF
        
        If mouseAction = WM_MOUSEMOVE Then
            ' did we already fire an event for this control?
            If ctrlUnderCursor.hwnd <> ctrlHWnd Then
                ' ctrlUnderCursor was Nothing or is different
                ' from current control
                If Not (ctrlUnderCursor Is Nothing) Then
                    RaiseEvent MouseExit(ctrlUnderCursor)
                    Set ctrlUnderCursor = Nothing
                End If
                ' which control is under the cursor ?
                For Each ctrl In m_Form.Controls
                    If ctrl.hwnd <> ctrlHWnd Then
                        ' different hWnd or hWnd property not supported
                    Else
                        ' we found it
                        Set ctrlUnderCursor = ctrl
                        RaiseEvent MouseEnter(ctrlUnderCursor)
                        Exit For
                    End If
                Next
            End If
        End If
    Case WM_COMPACTING
        ' system is low on memory
        RaiseEvent CompactingMemory
    Case WM_DISPLAYCHANGE
        ' screen resolution has changed
        RaiseEvent DisplayChanged(lParam And &HFFFF&, lParam \ &H10000, 2 ^ wParam)
    
    End Select
    
End Sub
