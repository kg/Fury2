Attribute VB_Name = "Funktionen"
Option Explicit
'-------------------------------------------------------------'
' Copyright (c)2000 by Michael Wallner                        '
' http://wallner.homepage.com                                 '
' eMail: michael.wallner@iname.com                            '
'-------------------------------------------------------------'
' Funktionen für das MDIActiveX Control                       '
'-------------------------------------------------------------'
Public Declare Function DestroyWindow Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function FindWindowEx Lib "user32" Alias "FindWindowExA" (ByVal hWnd1 As Long, ByVal hWnd2 As Long, ByVal lpsz1 As String, ByVal lpsz2 As String) As Long

'Ändere Window Style auf Child...
'change windowstyle to child
Public Sub MakeChild(hwnd As Long, Optional undo)
    Dim curStyle As Long
    Dim newStyle As Long

    'Window Style...
    curStyle = GetWindowLong(hwnd, GWL_STYLE)
    If IsMissing(undo) Then
        curStyle = curStyle Or WS_CHILD
        curStyle = curStyle And (Not WS_POPUP)
    Else
        curStyle = curStyle And Not WS_CHILD
        curStyle = curStyle Or WS_POPUP
    End If
    newStyle = SetWindowLong(hwnd, GWL_STYLE, curStyle)

    'Window EXStyle...
    curStyle = GetWindowLong(hwnd, GWL_EXSTYLE)
    curStyle = curStyle Or WS_EX_MDICHILD
    newStyle = SetWindowLong(hwnd, GWL_EXSTYLE, curStyle)

End Sub

'Entferne den Window EXStyle WS_EX_NOPARENTNOTIFY von allen Controls auf der Form
'remove WS_EX_NOPARENTNOTIFY from controls
Public Sub ParentNotify(hwnd)
    Dim hChild As Long
    Dim lWStyle As Long

    hChild = GetWindow(hwnd, GW_CHILD)
    While hChild <> 0
        lWStyle = GetWindowLong(hChild, GWL_EXSTYLE)
        lWStyle = lWStyle And Not WS_EX_NOPARENTNOTIFY
        lWStyle = SetWindowLong(hChild, GWL_EXSTYLE, lWStyle)
        hChild = GetWindow(hChild, GW_HWNDNEXT)
    Wend

End Sub

'Wenn die Größe des ActiveX Formulars geändert, Größe des 'echten' MDIChild ändern
'resize the 'real' MDIChild if the size of the activex form is changed
Public Sub ResizeChild(hwnd As Long)
    Resize hwnd
End Sub


'die Größe des 'echten' MDIChild Fensters wird geändert --> ActiveX Form anpassen
'resize 'real' MDIChild window...
Public Sub Resize(hwnd As Long)

    Dim r As RECT

    GetWindowRect GetParent(hwnd), r            'size of parent window
    
    r.Right = r.Right - r.Left                  'calculate width and height
    r.Bottom = r.Bottom - r.Top
    r.Left = 0                              'calculate position
    r.Top = 0                         '(relative to parent window)

    ShowWindow hwnd, 5
    SetWindowPos hwnd, 0, r.Left, r.Top, r.Right, r.Bottom, SWP_NOREPOSITION Or SWP_NOZORDER Or SWP_FRAMECHANGED

End Sub

Function GetProcPointer(ByVal lWndProc As Long) As Long
    GetProcPointer = lWndProc
End Function

'WindowProc für das 'echte' MDIChild Fenster
'windowproc for the 'real' MDIChild window
Private Function MyWndProc(ByVal hwnd As Long, ByVal message As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    Dim hChild As Long

    Select Case message

    Case WM_PAINT
        hChild = GetWindow(hwnd, GW_CHILD)
        If hChild = 0 Then
            ShowWindow hwnd, 0
        End If
        
    Case WM_SIZE
        hChild = GetWindow(hwnd, GW_CHILD)
        If hChild = 0 Then
        Else
            Resize hChild
        End If

    Case WM_SETFOCUS
        hChild = GetWindow(hwnd, GW_CHILD)
        If hChild = 0 Then
        Else
            APISetFocus hChild
        End If

    Case WM_NCACTIVATE
        If wParam <> 0 Then
            'to change the menu bar...
            SendMessage GetParent(hwnd), WM_MDISETMENU, 0, ByVal 0&
        End If
        MyWndProc = DefMDIChildProc(hwnd, message, wParam, lParam)
        hChild = GetWindow(hwnd, GW_CHILD)
        If wParam <> 0 Then
            'activate activex form ans set focus
            SendMessage hChild, WM_NCACTIVATE, 1, ByVal lParam
            APISetFocus hwnd
        Else
            'deactivate activex form
            SendMessage hChild, WM_NCACTIVATE, 0, ByVal lParam
        End If
        Exit Function

    Case WM_SYSCOMMAND
        If wParam = SC_CLOSE Then
            hChild = GetWindow(hwnd, GW_CHILD)
            If hChild <> 0 Then
                'send message to the activex form
                SendMessage hChild, WM_SYSCOMMAND, wParam, ByVal lParam
                MyWndProc = 0
                'and ignore this message!
                Exit Function
            End If
        End If

    Case WM_CLOSE
        If wParam <> 255 Then
            hChild = GetWindow(hwnd, GW_CHILD)
            If hChild <> 0 Then
                'send message to the activex form
                SendMessage hChild, WM_CLOSE, wParam, ByVal lParam
                MyWndProc = 0
                'and ignore this message
                Exit Function
            End If
        End If

    Case WM_DESTROY
        'remove all Properties ans restore old hMenu
        RemoveProp hwnd, "hMDIMenu"
        RemoveProp hwnd, "hFileMenu"
        RemoveProp hwnd, "pMDIChild"
        SendMessage GetParent(hwnd), WM_MDISETMENU, g_oldhMenu, ByVal g_oldhWindow

    End Select

    'call standard MDI Window Proc!
    MyWndProc = DefMDIChildProc(hwnd, message, wParam, lParam)

End Function

'Eigene Fensterclasse registrieren
'register my window class
Public Function myRegisterClass() As Boolean
    Dim wndcls As WNDCLASS
    Dim r As Long

    wndcls.style = CS_HREDRAW + CS_VREDRAW
    wndcls.lpfnwndproc = GetProcPointer(AddressOf MyWndProc)
    wndcls.cbClsextra = 0
    wndcls.cbWndExtra2 = 0
    wndcls.hInstance = App.hInstance
    wndcls.hIcon = 0
    wndcls.hCursor = LoadCursor(0, IDC_ARROW)
    wndcls.hbrBackground = COLOR_WINDOW
    wndcls.lpszMenuName = 0
    wndcls.lpszClassName = "MDIActiveXClass"
    r = RegisterClass(wndcls)
    myRegisterClass = (r <> 0)

End Function

Public Sub myUnregisterClass()
    Call UnregisterClass("MDIActiveXClass", App.hInstance)
End Sub

'erzeuge das 'echte' MDIChild fenster
'create the 'real' MDIChild window
Public Function CreateMDIChild(hWndParent As Long, Caption As String) As Long
    Dim lWStyle As Long
    Dim hwnd As Long
    Dim hMDIClient As Long

    lWStyle = WS_CHILD Or WS_CLIPSIBLINGS Or WS_CLIPCHILDREN Or WS_VISIBLE Or WS_MAXIMIZEBOX Or WS_MINIMIZEBOX
    'old version
    hMDIClient = FindWindowEx(hWndParent, 0, "MDIClient", vbNullString)
    'since 1.0.3
    'hMDIClient = GetWindow(hWndParent, GW_CHILD)
    hwnd = CreateMDIWindow("MDIActiveXClass", Caption, lWStyle, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, hMDIClient, App.hInstance, 0)
    CreateMDIChild = hwnd

End Function

'Ermittle den Klassennamen eines Fensters
'Get classname of a window
Function WindowClass(ByVal hwnd As Long) As String
    Dim TextLen As Long
    Dim Ret As String
    Dim i As Long

    Ret = String$(255, 0)
    TextLen = GetClassName(hwnd, Ret, 256)
    WindowClass = Left$(Ret, TextLen)

End Function

'Finde das MDIFrame Fenster im gleichen Thread
'get handle to the MDIFrame window in same thread
Public Function GetMDIFrame() As Long
    Dim hwnd As Long
    Dim ProcessID As Long

    'Muß ein Child des Desktop Windows sein!
    'must be child from the desktop window
    hwnd = GetWindow(GetDesktopWindow, GW_CHILD)
    'solange ich ein Fenster gefunden habe...
    'while theres a window...
    While hwnd <> 0
        'ProcessID ermitteln
        ProcessID = GetWindowThreadProcessId(hwnd, ProcessID)
        If ProcessID = App.ThreadID Then 'same thread?
            'im richtigen Thread -> Fensterklasse vergleichen...
            'same thread --> test window class
            If Right(LCase(WindowClass(hwnd)), 7) = "mdiform" Then
                'Fenster gefunden...
                'yea!
                GetMDIFrame = hwnd
                Exit Function
            End If
        End If
        'Nächstes Fenster suchen
        'next window
        hwnd = GetWindow(hwnd, GW_HWNDNEXT)
    Wend
    'Kein MDIFrame Fenster gefunden!
    'sorry theres no MDFrame Window
    GetMDIFrame = 0

End Function

'eigene windowProc für das Formular in der ActiveX-DLL
'my windowproc for the activex form
Public Function WndProc(ByVal hwnd As Long, ByVal iMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    Dim hParent As Long
    Dim hChild As Long
    Dim Caption As String
    Dim lCaption As Long
    Dim hIcon As Long
    Dim hoIcon As Long
    Dim mywParam As Long
    Dim oldWndProc As Long

    hParent = GetParent(hwnd)
    oldWndProc = GetProp(hwnd, "MDIActiveXold")
    If oldWndProc = 0 Then Exit Function

    Select Case iMsg

    Case WM_SETTEXT, WM_STYLECHANGED
        WndProc = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)
        Caption = String(255, 0)
        lCaption = GetWindowText(hwnd, Caption, 254)
        Caption = Left(Caption, lCaption + 1)
        SetWindowText hParent, Caption

    Case WM_SETICON
        WndProc = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)
        hIcon = SendMessage(hwnd, WM_GETICON, 0, ByVal 0&)
        If hIcon <> 0 Then
            hoIcon = SendMessage(hParent, WM_SETICON, 0, ByVal hIcon)
            If hoIcon <> 0 And hoIcon <> hIcon Then
                DestroyIcon hoIcon
                Debug.Print "Destroy Icon"
            End If
        End If

    Case WM_NCLBUTTONDOWN
        If wParam = HTCAPTION Or wParam = HTBOTTOM Or _
                wParam = HTLEFT Or wParam = HTRIGHT Or _
                wParam = HTbottomright Or wParam = HTBOTTOMLEFT Or _
                wParam = HTTOPLEFT Or wParam = HTTOPRIGHT _
                Then
        ReleaseCapture
        If IsZoomed(GetParent(hwnd)) Then
            'Send message to the app window
            WndProc = SendMessage(GetParent(GetParent(GetParent(hwnd))), WM_NCLBUTTONDOWN, wParam, ByVal 0&)
        Else
            'send message to the mdi form
            WndProc = SendMessage(GetParent(hwnd), WM_NCLBUTTONDOWN, wParam, ByVal 0&)
        End If
    Else
        'send message to activex form
        WndProc = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)
    End If

Case WM_SIZE
    ResizeChild (hwnd)
    WndProc = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)

Case Else
    WndProc = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)
End Select

End Function


'Eigene WidowProc für das MDIFrame und MDIClient Fenster
'my windowproc for the MDIFrame and MDIClient window
Public Function WndProcMDI(ByVal hwnd As Long, ByVal iMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    Dim hParent As Long
    Dim hChild As Long
    Dim Caption As String
    Dim lCaption As Long
    Dim hIcon As Long
    Dim mywParam As Long
    Dim oldWndProc As Long

    hParent = GetParent(hwnd)
    oldWndProc = GetProp(hwnd, "MDIActiveXold")
    If oldWndProc = 0 Then Exit Function

    Select Case iMsg

        'MDIFrame
    Case WM_COMMAND
        hChild = SendMessage(GetWindow(hwnd, GW_CHILD), WM_MDIGETACTIVE, 0, ByVal 0&)
        If hChild <> 0 Then
            'if child has its own menu.... (wParam > 32767 --> Select window)
            If GetProp(hChild, "hMDIMenu") <> 0 And lParam = 0 And (wParam < 32768) Then
                'send message to the activex form
                hChild = GetWindow(hChild, GW_CHILD)
                WndProcMDI = SendMessage(hChild, iMsg, wParam, ByVal lParam)
            Else
                WndProcMDI = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)
            End If
        Else
            WndProcMDI = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)
        End If

        'MDIClient
    Case WM_MDISETMENU
        'find active window
        hChild = SendMessage(hwnd, WM_MDIGETACTIVE, 0, ByVal 0&)
        If hChild = 0 Then
            'save original handle to windowlist (if not done yet)
            If g_oldhWindow = 0 And lParam <> 0 Then
                g_oldhWindow = lParam
            End If
        Else
            'is there a menu handle stored
            If GetProp(hChild, "hMDIMenu") <> 0 Then
                wParam = GetProp(hChild, "hMDIMenu")
                'is there a windowlist stored
                If GetProp(hChild, "hFileMenu") <> 0 Then
                    lParam = GetProp(hChild, "hFileMEnu")
                End If
            Else
                'no --> original handles
                wParam = g_oldhMenu
                lParam = g_oldhWindow
            End If
        End If

        WndProcMDI = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)
        SendMessage hwnd, WM_MDIREFRESHMENU, 0, ByVal 0&    'refresh window list!
        DrawMenuBar GetParent(hwnd)                         'redraw menubar

    Case Else
        WndProcMDI = CallWindowProc(oldWndProc, hwnd, iMsg, wParam, lParam)

    End Select

End Function

'Starten und beenden des Subclassings

'start subclassing of the MDIFrame and MDIClient window
Public Sub SubStartMDI(hwnd As Long)
    Dim OldWindowProc As Long
    OldWindowProc = SetWindowLong(hwnd, GWL_WNDPROC, AddressOf WndProcMDI)
    SetProp hwnd, "MDIActiveXold", OldWindowProc
End Sub

'start subclassing of the activex form
Public Sub SubStart(hwnd As Long)
    Dim OldWindowProc As Long

    OldWindowProc = SetWindowLong(hwnd, GWL_WNDPROC, AddressOf WndProc)
    SetProp hwnd, "MDIActiveXold", OldWindowProc

End Sub

'stop subclassing
Public Sub SubStop(hwnd As Long)
    Dim OldWindowProc As Long

    OldWindowProc = GetProp(hwnd, "MDIActiveXold")
    Call SetWindowLong(hwnd, GWL_WNDPROC, OldWindowProc)
    RemoveProp hwnd, "MDIActiveXold"

End Sub
