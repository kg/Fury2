Attribute VB_Name = "Definitionen"
Option Explicit
'-------------------------------------------------------------'
' Copyright (c)2000 by Michael Wallner                        '
' http://wallner.homepage.com                                 '
' eMail: michael.wallner@iname.com                            '
'-------------------------------------------------------------'
' Definitionen für das MDIActiveX Control                     '
'-------------------------------------------------------------'

Public g_oldhMenu As Long           'Original hMenu
Public g_oldhWindow As Long         'Original hWindow Menu

'WindowState
Declare Function IsZoomed Lib "user32" (ByVal hwnd As Long) As Long
Declare Function IsIconic Lib "user32" (ByVal hwnd As Long) As Long
Declare Function ShowWindow Lib "user32" (ByVal hwnd As Long, ByVal nCmdShow As Long) As Long
Public Const SW_MINIMIZE = 6

'Public Declare Function LockWindowUpdate Lib "user32" (ByVal hwndLock As Long) As Long
Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal ByteLen As Long)

'Find WindowMenu
Declare Function GetMenuItemCount Lib "user32" (ByVal hMenu As Long) As Long
Declare Function GetMenuString Lib "user32" Alias "GetMenuStringA" _
        (ByVal hMenu As Long, ByVal wIDItem As Long, _
        ByVal lpString As String, ByVal nMaxCount As Long, _
        ByVal wFlag As Long) As Long
Declare Function GetSubMenu Lib "user32" (ByVal hMenu As Long, ByVal nPos As Long) As Long
Public Const MF_BYPOSITION = &H400&

'Find MDIForm
Public Declare Function GetWindowThreadProcessId Lib "user32" (ByVal hwnd As Long, lpdwProcessId As Long) As Long
Public Declare Function GetClassName Lib "user32" Alias "GetClassNameA" (ByVal hwnd As Long, ByVal lpClassName As String, ByVal nMaxCount As Long) As Long
Public Declare Function GetDesktopWindow Lib "user32" () As Long

'Resize form
Public Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Public Declare Function SetWindowPos Lib "user32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal X As Long, ByVal Y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Public Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
Public Const SM_CXDLGFRAME = 7
Public Const SM_CYDLGFRAME = 8
Public Const SM_CYCAPTION = 4
Public Enum ESetWindowPosStyles
    SWP_SHOWWINDOW = &H40
    SWP_HIDEWINDOW = &H80
    SWP_FRAMECHANGED = &H20 ' The frame changed: send WM_NCCALCSIZE
    SWP_NOACTIVATE = &H10
    SWP_NOCOPYBITS = &H100
    SWP_NOMOVE = &H2
    SWP_NOOWNERZORDER = &H200 ' Don't do owner Z ordering
    SWP_NOREDRAW = &H8
    SWP_NOREPOSITION = SWP_NOOWNERZORDER
    SWP_NOSIZE = &H1
    SWP_NOZORDER = &H4
    SWP_DRAWFRAME = SWP_FRAMECHANGED
    HWND_NOTOPMOST = -2
End Enum
Public Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

Public Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Public Declare Function PostMessage Lib "user32" Alias "PostMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long

Public Declare Function CreateMDIWindow Lib "user32" Alias "CreateMDIWindowA" (ByVal lpClassName As String, ByVal lpWindowName As String, ByVal dwStyle As Long, ByVal X As Long, ByVal Y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hWndParent As Long, ByVal hInstance As Long, ByVal lParam As Long) As Long
Public Const CW_USEDEFAULT = &H80000000

Public Declare Function GetParent Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function SetParent Lib "user32" (ByVal hWndChild As Long, ByVal hWndNewParent As Long) As Long
Public Declare Function GetWindow Lib "user32" (ByVal hwnd As Long, ByVal wCmd As Long) As Long
Public Declare Function IsWindow Lib "user32" (ByVal hwnd As Long) As Long
Public Const GW_CHILD = 5
Public Const GW_HWNDNEXT = 2

Public Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Public Const GWL_STYLE = (-16)
Public Const GWL_WNDPROC = (-4)
Public Const GWL_HWNDPARENT = (-8)
Public Const GWL_EXSTYLE = (-20)
Public Const WS_EX_MDICHILD = &H40
Public Const WS_EX_NOPARENTNOTIFY = &H4&

Public Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" (ByVal lpPrevWndFunc As Long, ByVal hwnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Public Declare Function DefMDIChildProc Lib "user32" Alias "DefMDIChildProcA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long

Public Declare Function GetWindowText Lib "user32" Alias "GetWindowTextA" (ByVal hwnd As Long, ByVal lpString As String, ByVal cch As Long) As Long
Public Declare Function SetWindowText Lib "user32" Alias "SetWindowTextA" (ByVal hwnd As Long, ByVal lpString As String) As Long
Declare Function DestroyIcon Lib "user32" (ByVal hIcon As Long) As Long
Public Const MiniIcon = 0

Public Declare Function APISetFocus Lib "user32" Alias "SetFocus" (ByVal hwnd As Long) As Long

Declare Function SetProp Lib "user32" Alias "SetPropA" (ByVal hwnd As Long, ByVal lpString As String, ByVal hData As Long) As Long
Declare Function GetProp Lib "user32" Alias "GetPropA" (ByVal hwnd As Long, ByVal lpString As String) As Long
Declare Function RemoveProp Lib "user32" Alias "RemovePropA" (ByVal hwnd As Long, ByVal lpString As String) As Long

Public Declare Function GetMenu Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function DrawMenuBar Lib "user32" (ByVal hwnd As Long) As Long

Public Declare Function ReleaseCapture Lib "user32" () As Long
Public Enum EHitTestAreas
    HTERROR = (-2)
    HTTRANSPARENT = (-1)
    HTNOWHERE = 0
    HTCLIENT = 1
    HTCAPTION = 2
    HTSYSMENU = 3
    HTGROWBOX = 4
    HTMENU = 5
    HTHSCROLL = 6
    HTVSCROLL = 7
    HTMINBUTTON = 8
    HTMAXBUTTON = 9
    HTLEFT = 10
    HTRIGHT = 11
    HTTOP = 12
    HTTOPLEFT = 13
    HTTOPRIGHT = 14
    HTBOTTOM = 15
    HTBOTTOMLEFT = 16
    HTbottomright = 17
    HTBORDER = 18
End Enum

'window messages
Public Const WM_SETICON = &H80
Public Const WM_GETICON = &H7F

Public Const WM_NCACTIVATE = &H86
Public Const WM_SETFOCUS = &H7

Public Const WM_STYLECHANGED = &H7D
Public Const WM_SETTEXT = &HC

Public Const WM_SIZE = &H5

Public Const WM_KEYDOWN = &H100

Public Const WM_CLOSE = &H10
Public Const WM_COMMAND = &H111
Public Const WM_SYSCOMMAND = &H112
Public Const WM_DESTROY = &H2

Public Const WM_MDIACTIVATE = &H222
Public Const WM_MDISETMENU = &H230
Public Const WM_MDIREFRESHMENU = &H234
Public Const WM_MDIGETACTIVE = &H229
Public Const WM_MDIRESTORE = &H223
Public Const WM_MDIMAXIMIZE = &H225
Public Const WM_MDINEXT = &H224

Public Const WM_NCLBUTTONDOWN = &HA1

Public Const WM_SYSCHAR = &H106

Public Const SC_CLOSE = &HF060
Public Const SC_NEXTWINDOW = &HF040
Public Const SC_PREVWINDOW = &HF050

' Window styles
Public Const WS_OVERLAPPED = &H0&
Public Const WS_POPUP = &H80000000
Public Const WS_CHILD = &H40000000
Public Const WS_VISIBLE = &H10000000
Public Const WS_CLIPSIBLINGS = &H4000000
Public Const WS_CLIPCHILDREN = &H2000000
Public Const WS_MAXIMIZE = &H1000000
Public Const WS_BORDER = &H800000
Public Const WS_DLGFRAME = &H400000
Public Const WS_SYSMENU = &H80000
Public Const WS_THICKFRAME = &H40000
Public Const WS_MINIMIZEBOX = &H20000
Public Const WS_MAXIMIZEBOX = &H10000

'Fensterklassen
Declare Function RegisterClass Lib "user32" Alias "RegisterClassA" (Class As WNDCLASS) As Long
Declare Function UnregisterClass Lib "user32" Alias "UnregisterClassA" (ByVal lpClassName As String, ByVal hInstance As Long) As Long
Declare Function LoadCursor Lib "user32" Alias "LoadCursorA" (ByVal hInstance As Long, ByVal lpCursorName As Any) As Long
Type WNDCLASS
    style As Long
    lpfnwndproc As Long
    cbClsextra As Long
    cbWndExtra2 As Long
    hInstance As Long
    hIcon As Long
    hCursor As Long
    hbrBackground As Long
    lpszMenuName As String
    lpszClassName As String
End Type
Public Const CS_VREDRAW = &H1
Public Const CS_HREDRAW = &H2
Public Const IDC_ARROW = 32512&
Public Const COLOR_WINDOW = 5

