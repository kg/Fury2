Attribute VB_Name = "APIDeclares"
Option Explicit

Type MINMAXINFO
    ptReserved As POINTAPI
    ptMaxSize As POINTAPI
    ptMaxPosition As POINTAPI
    ptMinTrackSize As POINTAPI
    ptMaxTrackSize As POINTAPI
End Type

Type COPYDATASTRUCT
    dwData As Long
    cbData As Long
    lpData As Long
End Type

Public Const WM_ACTIVATEAPP = &H1C
Public Const WM_CAPTURECHANGED = &H215
Public Const WM_CHAR = &H102
Public Const WM_CLOSE = &H10
Public Const WM_COMMAND = &H111
Public Const WM_COMPACTING = &H41
Public Const WM_CONTEXTMENU = &H7B
Public Const WM_COPYDATA = &H4A
Public Const WM_DEVMODECHANGE = &H1B
Public Const WM_DEVICECHANGE = &H219
Public Const WM_DISPLAYCHANGE = &H7E
Public Const WM_DROPFILES = &H233
Public Const WM_ENDSESSION = &H16
Public Const WM_ENTERMENULOOP = &H211
Public Const WM_ERASEBKGND = &H14
Public Const WM_EXITMENULOOP = &H212
Public Const WM_FONTCHANGE = &H1D
Public Const WM_GETMINMAXINFO = &H24
Public Const WM_HOTKEY = &H312
Public Const WM_HSCROLL = &H114
Public Const WM_KEYDOWN = &H100
Public Const WM_KEYUP = &H101
Public Const WM_KILLFOCUS = &H8
Public Const WM_LBUTTONDBLCLK = &H203
Public Const WM_LBUTTONDOWN = &H201
Public Const WM_LBUTTONUP = &H202
Public Const WM_MBUTTONDBLCLK = &H209
Public Const WM_MBUTTONDOWN = &H207
Public Const WM_MBUTTONUP = &H208
Public Const WM_MENUCHAR = &H120
Public Const WM_MENUSELECT = &H11F
Public Const WM_MOUSEACTIVATE = &H21
Public Const WM_MOUSEMOVE = &H200
Public Const WM_MOVE = &H3
Public Const WM_MOVING = &H216
Public Const WM_NCACTIVATE = &H86
Public Const WM_NCHITTEST = &H84
Public Const WM_NCLBUTTONDBLCLK = &HA3
Public Const WM_NCLBUTTONDOWN = &HA1
Public Const WM_NCLBUTTONUP = &HA2
Public Const WM_NCMBUTTONDBLCLK = &HA9
Public Const WM_NCMBUTTONDOWN = &HA7
Public Const WM_NCMBUTTONUP = &HA8
Public Const WM_NCMOUSEMOVE = &HA0
Public Const WM_NCPAINT = &H85
Public Const WM_NCRBUTTONDBLCLK = &HA6
Public Const WM_NCRBUTTONDOWN = &HA4
Public Const WM_NCRBUTTONUP = &HA5
Public Const WM_NOTIFY = &H4E
Public Const WM_OTHERWINDOWCREATED = &H42
Public Const WM_OTHERWINDOWDESTROYED = &H43
Public Const WM_PAINT = &HF
Public Const WM_PALETTECHANGED = &H311
Public Const WM_PALETTEISCHANGING = &H310
Public Const WM_POWER = &H48
Public Const WM_POWERBROADCAST = &H218
Public Const WM_QUERYENDSESSION = &H11
Public Const WM_QUERYNEWPALETTE = &H30F
Public Const WM_QUERYOPEN = &H13
Public Const WM_RBUTTONDBLCLK = &H206
Public Const WM_RBUTTONDOWN = &H204
Public Const WM_RBUTTONUP = &H205
Public Const WM_SETCURSOR = &H20
Public Const WM_SETFOCUS = &H7
Public Const WM_SETTINGCHANGE = &H1A
Public Const WM_SIZE = &H5
Public Const WM_SIZING = &H214
Public Const WM_SPOOLERSTATUS = &H2A
Public Const WM_SYSCOLORCHANGE = &H15
Public Const WM_SYSCOMMAND = &H112
Public Const WM_TIMECHANGE = &H1E
Public Const WM_USERCHANGED = &H54
Public Const WM_VSCROLL = &H115
Public Const WM_WININICHANGE = &H1A

' these are used in mouse messages
Public Const MK_CONTROL = &H8
Public Const MK_LBUTTON = &H1
Public Const MK_MBUTTON = &H10
Public Const MK_RBUTTON = &H2
Public Const MK_SHIFT = &H4

' these are used in menu messages
Public Const MF_APPEND = &H100&
Public Const MF_BITMAP = &H4&
Public Const MF_BYCOMMAND = &H0&
Public Const MF_BYPOSITION = &H400&
Public Const MF_CALLBACKS = &H8000000
Public Const MF_CHANGE = &H80&
Public Const MF_CHECKED = &H8&
Public Const MF_CONV = &H40000000
Public Const MF_DEFAULT = &H1000&
Public Const MF_DELETE = &H200&
Public Const MF_DISABLED = &H2&
Public Const MF_ENABLED = &H0&
Public Const MF_END = &H80
Public Const MF_ERRORS = &H10000000
Public Const MF_GRAYED = &H1&
Public Const MF_HELP = &H4000&
Public Const MF_HILITE = &H80&
Public Const MF_INSERT = &H0&
Public Const MF_LINKS = &H20000000
Public Const MF_MASK = &HFF000000
Public Const MF_MENUBARBREAK = &H20&
Public Const MF_MENUBREAK = &H40&
Public Const MF_MOUSESELECT = &H8000&
Public Const MF_OWNERDRAW = &H100&
Public Const MF_POPUP = &H10&
Public Const MF_POSTMSGS = &H4000000
Public Const MF_REMOVE = &H1000&
Public Const MF_RIGHTJUSTIFY = &H4000&
Public Const MF_SENDMSGS = &H2000000
Public Const MF_SEPARATOR = &H800&
Public Const MF_STRING = &H0&
Public Const MF_UNCHECKED = &H0&
Public Const MF_SYSMENU = &H2000&
Public Const MF_UNHILITE = &H0&
Public Const MF_USECHECKBITMAPS = &H200&

' the possible return values for WM_MOUSEACTIVATE
Public Const MA_ACTIVATE = 1
Public Const MA_ACTIVATEANDEAT = 2
Public Const MA_NOACTIVATE = 3
Public Const MA_NOACTIVATEANDEAT = 4

' these are used by the WM_MOVING and WM_SIZING messages
Public Const WMSZ_BOTTOM = 6
Public Const WMSZ_BOTTOMLEFT = 7
Public Const WMSZ_BOTTOMRIGHT = 8
Public Const WMSZ_LEFT = 1
Public Const WMSZ_RIGHT = 2
Public Const WMSZ_TOP = 3
Public Const WMSZ_TOPLEFT = 4
Public Const WMSZ_TOPRIGHT = 5

' this are the values that can be returned by a WM_NCHITTEST message
Public Const HTBORDER = 18
Public Const HTBOTTOM = 15
Public Const HTBOTTOMLEFT = 16
Public Const HTBOTTOMRIGHT = 17
Public Const HTCAPTION = 2
Public Const HTCLIENT = 1
Public Const HTCLOSE = 20
Public Const HTERROR = (-2)
Public Const HTGROWBOX = 4
Public Const HTHELP = 21
Public Const HTHSCROLL = 6
Public Const HTLEFT = 10
Public Const HTMAXBUTTON = 9
Public Const HTMENU = 5
Public Const HTMINBUTTON = 8
Public Const HTNOWHERE = 0
Public Const HTOBJECT = 19
Public Const HTREDUCE = HTMINBUTTON
Public Const HTRIGHT = 11
Public Const HTSIZE = HTGROWBOX
Public Const HTSIZEFIRST = HTLEFT
Public Const HTSYSMENU = 3
Public Const HTTOP = 12
Public Const HTTOPLEFT = 13
Public Const HTTOPRIGHT = 14
Public Const HTTRANSPARENT = (-1)
Public Const HTVSCROLL = 7
Public Const HTZOOM = HTMAXBUTTON

' these values are passed to a WM_SIZE message
Public Const SIZE_MAXHIDE = 4
Public Const SIZE_MAXIMIZED = 2
Public Const SIZE_MAXSHOW = 3
Public Const SIZE_MINIMIZED = 1
Public Const SIZE_RESTORED = 0

' this values are related to the WM_SYSCOMMAND message
Public Const SC_ARRANGE = &HF110
Public Const SC_CLOSE = &HF060
Public Const SC_CONTEXTHELP = &HF180
Public Const SC_DEFAULT = &HF160
Public Const SC_HOTKEY = &HF150
Public Const SC_HSCROLL = &HF080
Public Const SC_KEYMENU = &HF100
Public Const SC_MAXIMIZE = &HF030
Public Const SC_MINIMIZE = &HF020
Public Const SC_MONITORPOWER = &HF170
Public Const SC_MOUSEMENU = &HF090
Public Const SC_MOVE = &HF010
Public Const SC_NEXTWINDOW = &HF040
Public Const SC_PREVWINDOW = &HF050
Public Const SC_RESTORE = &HF120
Public Const SC_SCREENSAVE = &HF140
Public Const SC_SIZE = &HF000
Public Const SC_TASKLIST = &HF130
Public Const SC_VSCROLL = &HF070
Public Const SC_ZOOM = SC_MAXIMIZE

Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (dest As Any, source As Any, ByVal numBytes As Long)
Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Declare Function PostMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
' used to implement drag-and-drop recipients
Declare Sub DragAcceptFiles Lib "shell32.dll" (ByVal hwnd As Long, ByVal fAccept As Long)
Declare Function DragQueryFile Lib "shell32.dll" Alias "DragQueryFileA" (ByVal hDrop As Long, ByVal UINT As Long, ByVal lpStr As String, ByVal ch As Long) As Long
Declare Function DragQueryPoint Lib "shell32.dll" (ByVal hDrop As Long, lpPoint As POINTAPI) As Long
Declare Sub DragFinish Lib "shell32.dll" (ByVal hDrop As Long)
' used within the WM_PAINT message
Declare Function GetUpdateRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT, ByVal bErase As Long) As Long
' used for dealing with menus
Declare Function GetSystemMenu Lib "user32" (ByVal hwnd As Long, ByVal bRevert As Long) As Long
Declare Function GetMenuString Lib "user32" Alias "GetMenuStringA" (ByVal hMenu As Long, ByVal wIDItem As Long, ByVal lpString As String, ByVal nMaxCount As Long, ByVal wFlag As Long) As Long
Declare Function InsertMenu Lib "user32" Alias "InsertMenuA" (ByVal hMenu As Long, ByVal nPosition As Long, ByVal wFlags As Long, ByVal wIDNewItem As Long, ByVal lpNewItem As Any) As Long
Declare Function AppendMenu Lib "user32" Alias "AppendMenuA" (ByVal hMenu As Long, ByVal wFlags As Long, ByVal wIDNewItem As Long, ByVal lpNewItem As Any) As Long

Declare Function ScreenToClient Lib "user32" (ByVal hwnd As Long, lpPoint As POINTAPI) As Long

