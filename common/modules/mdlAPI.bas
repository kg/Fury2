Attribute VB_Name = "mdlAPI"
Option Explicit

'
'   ::fury² api declarations::
'

Public Const WS_BORDER = &H800000
Public Const WS_CAPTION = &HC00000
Public Const WS_CHILD = &H40000000
Public Const WS_CHILDWINDOW = &H40000000
Public Const WS_CLIPCHILDREN = &H2000000
Public Const WS_CLIPSIBLINGS = &H4000000
Public Const WS_DISABLED = &H8000000
Public Const WS_DLGFRAME = &H400000
Public Const WS_GROUP = &H20000
Public Const WS_HSCROLL = &H100000
Public Const WS_ICONIC = &H20000000
Public Const WS_MAXIMIZE = &H1000000
Public Const WS_MAXIMIZEBOX = &H10000
Public Const WS_MINIMIZE = &H20000000
Public Const WS_MINIMIZEBOX = &H20000
Public Const WS_OVERLAPPED = &H0
Public Const WS_OVERLAPPEDWINDOW = &HCF0000
Public Const WS_POPUP = &H80000000
Public Const WS_POPUPWINDOW = &H80880000
Public Const WS_SIZEBOX = &H40000
Public Const WS_SYSMENU = &H80000
Public Const WS_TABSTOP = &H10000
Public Const WS_THICKFRAME = &H40000
Public Const WS_TILED = &H0
Public Const WS_TILEDWINDOW = &HCF0000
Public Const WS_VISIBLE = &H10000000
Public Const WS_VSCROLL = &H200000

Public Const GMEM_FIXED = &H0
Public Const GMEM_INVALID_HANDLE = &H8000
Public Const GMEM_LOCKCOUNT = &HFF
Public Const GMEM_LOWER = &H1000
Public Const GMEM_MODIFY = &H80
Public Const GMEM_MOVEABLE = &H2
Public Const GMEM_NOCOMPACT = &H10
Public Const GMEM_NODISCARD = &H20
Public Const GMEM_NOT_BANKED = &H1000
Public Const GMEM_NOTIFY = &H4000
Public Const GMEM_SHARE = &H2000
Public Const GMEM_VALID_FLAGS = &H7F72
Public Const GMEM_ZEROINIT = &H40

Public Declare Function CoerceVarArray Lib "f2softfx" Alias "coerce" (ByRef Value As Any) As Variant()

Public Declare Function IsMMX Lib "f2softfx" Alias "_IsMMX@0" () As Long
Public Declare Function IsSSE Lib "f2softfx" Alias "_IsSSE@0" () As Long

      Public Declare Function ShellExecute Lib "Shell32.dll" Alias _
      "ShellExecuteA" (ByVal hwnd As Long, ByVal lpszOp As _
      String, ByVal lpszFile As String, ByVal lpszParams As String, _
      ByVal lpszDir As String, ByVal FsShowCmd As Long) As Long

Public Declare Function ClientToScreen Lib "user32" (ByVal hwnd As Long, lpPoint As POINTAPI) As Long

Public Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As Win32.Rect) As Long
Public Declare Function GetCurrentProcess Lib "kernel32" () As Long
Public Declare Function TerminateProcess Lib "kernel32" (ByVal hProcess As Long, ByVal uExitCode As Long) As Long
Public Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Public Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalHandle Lib "kernel32" (wMem As Any) As Long
Public Declare Function GlobalReAlloc Lib "kernel32" (ByVal hMem As Long, ByVal dwBytes As Long, ByVal wFlags As Long) As Long
Public Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalSize Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalFlags Lib "kernel32" (ByVal hMem As Long) As Long

Public Declare Sub mouse_event Lib "user32" (ByVal dwFlags As Long, ByVal dx As Long, ByVal dy As Long, ByVal cButtons As Long, ByVal dwExtraInfo As Long)

Public Declare Function SetStretchBltMode Lib "gdi32" (ByVal hdc As Long, ByVal nStretchMode As Long) As Long
Public Const COLORONCOLOR = 3
Public Declare Sub PostQuitMessage Lib "user32" (ByVal nExitCode As Long)
Public Declare Function SleepEx Lib "kernel32" (ByVal dwMilliseconds As Long, ByVal bAlertable As Long) As Long
Public Declare Function GdiGetBatchLimit Lib "gdi32" () As Long
Public Declare Function GdiSetBatchLimit Lib "gdi32" (ByVal dwLimit As Long) As Long
Public Declare Function GdiFlush Lib "gdi32" () As Long

Public Declare Function SFXSwap Lib "f2softfx" (ByRef Var1 As Any, ByRef Var2 As Any) As Long

Public Declare Function GetGlyphOutline Lib "gdi32" Alias "GetGlyphOutlineA" (ByVal hdc As Long, ByVal uChar As Long, ByVal fuFormat As Long, lpgm As GLYPHMETRICS, ByVal BufferLength As Long, ByVal lpBuffer As Long, ByVal Transform As Long) As Long

Public Declare Function SetPixelFormat Lib "gdi32" (ByVal hdc As Long, ByVal n As Long, pcPixelFormatDescriptor As PIXELFORMATDESCRIPTOR) As Long
Public Type PIXELFORMATDESCRIPTOR
    nSize As Integer
    nVersion As Integer
    dwFlags As Long
    iPixelType As Byte
    cColorBits As Byte
    cRedBits As Byte
    cRedShift As Byte
    cGreenBits As Byte
    cGreenShift As Byte
    cBlueBits As Byte
    cBlueShift As Byte
    cAlphaBits As Byte
    cAlphaShift As Byte
    cAccumBits As Byte
    cAccumRedBits As Byte
    cAccumGreenBits As Byte
    cAccumBlueBits As Byte
    cAccumAlphaBits As Byte
    cDepthBits As Byte
    cStencilBits As Byte
    cAuxBuffers As Byte
    iLayerType As Byte
    bReserved As Byte
    dwLayerMask As Long
    dwVisibleMask As Long
    dwDamageMask As Long
End Type

Public Type ColorTriplet
    Red As Byte
    Green As Byte
    Blue As Byte
End Type

Public Type DataBuffer
    Data() As Byte
End Type

Public Type ColorPalette
    Colors() As ColorTriplet
End Type

Public Type Int32
    Low As Integer
    High As Integer
End Type

Public Type Int64
    Low As Long
    High As Long
End Type

Public Type FILETIME
        dwLowDateTime As Long
        dwHighDateTime As Long
End Type

Public Type WIN32_FIND_DATA
        dwFileAttributes As Long
        ftCreationTime As FILETIME
        ftLastAccessTime As FILETIME
        ftLastWriteTime As FILETIME
        nFileSizeHigh As Long
        nFileSizeLow As Long
        dwReserved0 As Long
        dwReserved1 As Long
        cFileName As String * 260
        cAlternate As String * 14
End Type

Public Declare Function GetActiveWindow Lib "user32" () As Long
Public Declare Function GetIconInfo Lib "user32" (ByVal hIcon As Long, piconinfo As ICONINFO) As Long
Public Type ICONINFO
        fIcon As Long
        xHotspot As Long
        yHotspot As Long
        hbmMask As Long
        hbmColor As Long
End Type
Public Declare Function DrawStateUnicode Lib "user32" Alias "DrawStateW" _
    (ByVal hdc As Long, _
    ByVal hBrush As Long, _
    ByVal lpDrawStateProc As Long, _
    ByVal lParam As Long, _
    ByVal wParam As Long, _
    ByVal x As Long, _
    ByVal y As Long, _
    ByVal cX As Long, _
    ByVal cY As Long, _
    ByVal fuFlags As Long) As Long
Public Declare Function DrawState Lib "user32" Alias "DrawStateA" _
    (ByVal hdc As Long, _
    ByVal hBrush As Long, _
    ByVal lpDrawStateProc As Long, _
    ByVal lParam As Long, _
    ByVal wParam As Long, _
    ByVal x As Long, _
    ByVal y As Long, _
    ByVal cX As Long, _
    ByVal cY As Long, _
    ByVal fuFlags As Long) As Long
Public Declare Function DrawStateString Lib "user32" Alias "DrawStateA" _
    (ByVal hdc As Long, _
    ByVal hBrush As Long, _
    ByVal lpDrawStateProc As Long, _
    ByVal Text As String, _
    ByVal Length As Long, _
    ByVal x As Long, _
    ByVal y As Long, _
    ByVal cX As Long, _
    ByVal cY As Long, _
    ByVal fuFlags As Long) As Long


Public Const DST_COMPLEX = &H0
Public Const DST_TEXT = &H1
Public Const DST_PREFIXTEXT = &H2
Public Const DST_ICON = &H3
Public Const DST_BITMAP = &H4
Public Const DSS_NORMAL = &H0
Public Const DSS_UNION = &H10 ' Dither
Public Const DSS_DISABLED = &H20
Public Const DSS_MONO = &H80 ' Draw in colour of brush specified in hBrush
Public Const DSS_RIGHT = &H8000

Public Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As Win32.Rect) As Long
Public Declare Function DrawFocusRect Lib "user32" (ByVal hdc As Long, lpRect As Win32.Rect) As Long
Public Declare Function SetCapture Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function GetCapture Lib "user32" () As Long
Public Declare Function ReleaseCapture Lib "user32" () As Long
Public Declare Function IntersectRect Lib "user32" (lpDestRect As Win32.Rect, lpSrc1Rect As Win32.Rect, lpSrc2Rect As Win32.Rect) As Long
Public Declare Function LockWindowUpdate Lib "user32" (ByVal hwndLock As Long) As Long
Public Declare Function CloseWindow Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function SetForegroundWindow Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function ShowCursor Lib "user32" (ByVal bShow As Long) As Long
Public Declare Function FindFirstFile Lib "kernel32" Alias "FindFirstFileA" (ByVal lpFileName As String, lpFindFileData As WIN32_FIND_DATA) As Long
Public Declare Function FindNextFile Lib "kernel32" Alias "FindNextFileA" (ByVal hFindFile As Long, lpFindFileData As WIN32_FIND_DATA) As Long
Public Declare Function FindClose Lib "kernel32" (ByVal hFindFile As Long) As Long
Public Declare Function DrawIconEx Lib "user32" (ByVal hdc As Long, ByVal xLeft As Long, ByVal yTop As Long, ByVal hIcon As Long, ByVal cxWidth As Long, ByVal cyWidth As Long, ByVal istepIfAniCur As Long, ByVal hbrFlickerFreeDraw As Long, ByVal diFlags As Long) As Long
Public Declare Function QueryPerformanceCounter Lib "kernel32" (lpPerformanceCount As Currency) As Long
Public Declare Function QueryPerformanceFrequency Lib "kernel32" (lpFrequency As Currency) As Long
Public Declare Function DrawIcon Lib "user32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal hIcon As Long) As Long
Public Declare Function GetParent Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function SetWindowText Lib "user32" Alias "SetWindowTextA" (ByVal hwnd As Long, ByVal lpString As String) As Long
Public Declare Function GetWindowText Lib "user32" Alias "GetWindowTextA" (ByVal hwnd As Long, ByVal lpString As String, ByVal cch As Long) As Long
Public Declare Function GetWindowTextLength Lib "user32" Alias "GetWindowTextLengthA" (ByVal hwnd As Long) As Long
Public Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Public Declare Function SetWindowPos Lib "user32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cX As Long, ByVal cY As Long, ByVal wFlags As Long) As Long
Public Declare Function SetParent Lib "user32" (ByVal hWndChild As Long, ByVal hWndNewParent As Long) As Long
Public Declare Function GetLastError Lib "kernel32" () As Long
Public Declare Function GetDeviceCaps Lib "gdi32" (ByVal hdc As Long, ByVal nIndex As Long) As Long
Public Declare Function GetTabbedTextExtent Lib "user32" Alias "GetTabbedTextExtentA" (ByVal hdc As Long, ByVal lpString As String, ByVal nCount As Long, ByVal nTabPositions As Long, lpnTabStopPositions As Long) As Long
Public Declare Function TabbedTextOut Lib "user32" Alias "TabbedTextOutA" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal lpString As String, ByVal nCount As Long, ByVal nTabPositions As Long, lpnTabStopPositions As Long, ByVal nTabOrigin As Long) As Long
Public Declare Function DestroyIcon Lib "user32" (ByVal hIcon As Long) As Long
Public Declare Function LoadImage Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As String, ByVal nType As Long, ByVal ncX As Long, ByVal ncY As Long, ByVal fuLoad As Long) As Long
Public Declare Function LoadIcon Lib "user32" Alias "LoadIconA" (ByVal hInstance As Long, ByVal lpIconName As String) As Long
Public Declare Function SetCursorPos Lib "user32" (ByVal x As Long, ByVal y As Long) As Long
Public Declare Function GetTextExtentPoint32 Lib "gdi32" Alias "GetTextExtentPoint32A" (ByVal hdc As Long, ByVal lpsz As String, ByVal cbString As Long, lpSize As Size) As Long
Public Declare Function GetTextExtentPoint32B Lib "gdi32" Alias "GetTextExtentPoint32A" (ByVal hdc As Long, ByRef lpsz As Long, ByVal cbString As Long, lpSize As Size) As Long
Public Declare Function GetTickCount Lib "kernel32" () As Long
Public Declare Function OleCreatePictureIndirect Lib "olepro32" (ByRef pPictDesc As PICTDESC, ByRef riid As iid, ByVal fOwn As Boolean, ByRef ppvObj As StdPicture) As Long
Public Declare Function GetObject Lib "gdi32" Alias "GetObjectA" (ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long
Public Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Public Declare Function CreateSolidBrush Lib "gdi32" (ByVal crColor As Long) As Long
Public Declare Function GetCurrentObject Lib "gdi32" (ByVal hdc As Long, ByVal uObjectType As Long) As Long
Public Declare Function GetStockObject Lib "gdi32" (ByVal nIndex As Long) As Long
Public Declare Function CreateDIBSection Lib "gdi32" (ByVal hdc As Long, pBitmapInfo As BITMAPINFO, ByVal un As Long, lplpVoid As Long, ByVal Handle As Long, ByVal dw As Long) As Long
Public Declare Function GetDIBits Lib "gdi32" (ByVal aHDC As Long, ByVal hBitmap As Long, ByVal nStartScan As Long, ByVal nNumScans As Long, lpBits As Any, lpBI As BITMAPINFO, ByVal wUsage As Long) As Long
Public Declare Function SetDIBits Lib "gdi32" (ByVal hdc As Long, ByVal hBitmap As Long, ByVal nStartScan As Long, ByVal nNumScans As Long, lpBits As Any, lpBI As BITMAPINFO, ByVal wUsage As Long) As Long
Public Declare Function SetTextColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Public Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Public Declare Function GetTextExtentPoint Lib "gdi32" Alias "GetTextExtentPoint32A" (ByVal hdc As Long, ByVal lpsz As String, ByVal cbString As Long, lpSize As Size) As Long
Public Declare Function FillRect Lib "user32" (ByVal hdc As Long, lpRect As Win32.Rect, ByVal hBrush As Long) As Long
Public Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Public Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Public Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Public Declare Function GetDesktopWindow Lib "user32" () As Long
Public Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Public Declare Function ReleaseDC Lib "user32" (ByVal hwnd As Long, ByVal hdc As Long) As Long
Public Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function GetTextColor Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function GetBkColor Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function GDIDrawText Lib "user32" Alias "DrawTextA" (ByVal hdc As Long, ByVal lpStr As String, ByVal nCount As Long, lpRect As Win32.Rect, ByVal wFormat As Long) As Long
Public Declare Function GDIDrawTextB Lib "user32" Alias "DrawTextA" (ByVal hdc As Long, ByRef lpStr As Long, ByVal nCount As Long, lpRect As Win32.Rect, ByVal wFormat As Long) As Long
Public Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)
Public Declare Sub CopyMemoryByVal Lib "kernel32" Alias "RtlMoveMemory" (ByVal Destination As Long, ByVal Source As Long, ByVal Length As Long)
Public Declare Function StretchDIBits Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal Width As Long, ByVal Height As Long, ByVal x2 As Long, ByVal y2 As Long, ByVal sWidth As Long, ByVal sHeight As Long, ByVal PointerToBits As Long, lpBitsInfo As BITMAPINFO, ByVal wUsage As Long, ByVal dwRop As Long) As Long
Public Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
Public Declare Function GetClassInfo Lib "user32" Alias "GetClassInfoA" (ByVal hInstance As Long, ByVal lpClassName As String, lpWndClass As WNDCLASS) As Long
Public Declare Function CreateFontIndirect Lib "gdi32" Alias "CreateFontIndirectA" (lpLogFont As LOGFONT) As Long
Public Declare Function GetSysColor Lib "user32" (ByVal nIndex As APISystemColors) As Long
Public Declare Function GetPixel Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long) As Long
Public Declare Function SetPixelV Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal crColor As Long) As Long
Public Declare Function DrawTextHackedUnicode Lib "user32" Alias "DrawTextW" (ByVal hdc As Long, ByVal lpStr As Long, ByVal nCount As Long, lpRect As Win32.Rect, ByVal wFormat As Long) As Long
Public Declare Function DrawTextHacked Lib "user32" Alias "DrawTextA" (ByVal hdc As Long, ByVal lpStr As Long, ByVal nCount As Long, lpRect As Win32.Rect, ByVal wFormat As Long) As Long
Public Declare Function DrawText Lib "user32" Alias "DrawTextA" (ByVal hdc As Long, ByVal lpStr As String, ByVal nCount As Long, lpRect As Win32.Rect, ByVal wFormat As Long) As Long
'Public Declare Function DrawTextEx Lib "user32" Alias "DrawTextExW" (ByVal hDC As Long, ByVal lpsz As String, ByVal n As Long, lpRect As Win32.Rect, ByVal un As Long, lpDrawTextParams As DRAWTEXTPARAMS) As Long
Public Declare Function InflateRect Lib "user32" (lpRect As Win32.Rect, ByVal x As Long, ByVal y As Long) As Long
Public Declare Function OffsetRect Lib "user32" (lpRect As Win32.Rect, ByVal x As Long, ByVal y As Long) As Long
Public Declare Function DrawEdge Lib "user32" (ByVal hdc As Long, qrc As Win32.Rect, ByVal edge As Long, ByVal grfFlags As Long) As Long

Public Const DT_WORD_ELLIPSIS = &H40000
Public Const DT_TOP = &H0
Public Const DT_LEFT = &H0
Public Const DT_CENTER = &H1
Public Const DT_RIGHT = &H2
Public Const DT_VCENTER = &H4
Public Const DT_BOTTOM = &H8
Public Const DT_WORDBREAK = &H10
Public Const DT_SINGLELINE = &H20
Public Const DT_EXPANDTABS = &H40
Public Const DT_TABSTOP = &H80
Public Const DT_NOCLIP = &H100
Public Const DT_EXTERNALLEADING = &H200
Public Const DT_CALCRECT = &H400
Public Const DT_NOPREFIX = &H800
Public Const DT_INTERNAL = &H1000

Public Type iid
    x As Long
    s1 As Integer
    s2 As Integer
    c(0 To 7) As Byte
End Type

Public Type PICTDESC
    cbSizeOfStruct As Long
    picType As Long
    hBitmap As Long
    hPal As Long
End Type

Public Type Size
        cX As Long
        cY As Long
End Type

Public Type POINTAPI
   x As Long
   y As Long
End Type

Public Type BITMAPINFOHEADER '40 bytes
        biSize As Long
        biWidth As Long
        biHeight As Long
        biPlanes As Integer
        biBitCount As Integer
        biCompression As Long
        biSizeImage As Long
        biXPelsPerMeter As Long
        biYPelsPerMeter As Long
        biClrUsed As Long
        biClrImportant As Long
End Type

Public Type RGBQUAD
        RGBBlue As Byte
        RGBGreen As Byte
        RGBRed As Byte
        rgbReserved As Byte
End Type

Public Type RGBTRIPLE
        rgbtBlue As Byte
        rgbtGreen As Byte
        rgbtRed As Byte
End Type

Public Type BITMAPINFO
    bmiHeader As BITMAPINFOHEADER
    bmiColors(256) As Long
End Type

Public Type WNDCLASS
    Style As Long
    lpfnWndProc As Long
    cbClsExtra As Long
    cbWndExtra2 As Long
    hInstance As Long
    hIcon As Long
    hCursor As Long
    hbrBackground As Long
    lpszMenuName As String
    lpszClassName As String
End Type

Public Type WNDCLASSEX
    cbSize As Long
    Style As Long
    lpfnWndProc As Long
    cbClsExtra As Long
    cbWndExtra As Long
    hInstance As Long
    hIcon As Long
    hCursor As Long
    hbrBackground As Long
    lpszMenuName As String
    lpszClassName As String
    hIconSm As Long
End Type

Public Type MAT2
    eL11 As Integer
    eH11 As Integer
    eL12 As Integer
    eH12 As Integer
    eL21 As Integer
    eH21 As Integer
    eL22 As Integer
    eH22 As Integer
End Type

Public Type GLYPHMETRICS
        gmBlackBoxX As Long
        gmBlackBoxY As Long
        gmptGlyphOrigin As POINTAPI
        gmCellIncX As Integer
        gmCellIncY As Integer
End Type

Public Enum picType
    PicType_Uninitialized = -1
    PicType_None = 0
    PicType_Bitmap = 1
    PicType_Metafile = 2
    PicType_Icon = 3
    PICTYPE_ENHMETAFILE = 4
End Enum

Public Declare Function SendMessage Lib "user32" _
   Alias "SendMessageA" _
   (ByVal hwnd As Long, _
   ByVal wMsg As Long, _
   ByVal wParam As Long, _
   lParam As Any) _
   As Long
  
Public Const WM_GETICON = &H7F
Public Const WM_SETICON = &H80
Public Const ICON_SMALL = 0
Public Const ICON_BIG = 1

'// LoadImage() image types
Public Const IMAGE_BITMAP = 0
Public Const IMAGE_ICON = 1
Public Const IMAGE_CURSOR = 2
Public Const IMAGE_ENHMETAFILE = 3

'// LoadImage() flags
Public Const LR_DEFAULTCOLOR = &H0
Public Const LR_MONOCHROME = &H1
Public Const LR_COLOR = &H2
Public Const LR_COPYRETURNORG = &H4
Public Const LR_COPYDELETEORG = &H8
Public Const LR_LOADFROMFILE = &H10
Public Const LR_LOADTRANSPARENT = &H20
Public Const LR_DEFAULTSIZE = &H40
Public Const LR_LOADMAP3DCOLORS = &H1000
Public Const LR_CREATEDIBHeader = &H2000
Public Const LR_COPYFROMRESOURCE = &H4000
Public Const LR_SHARED = &H8000

' GetGlyphOutline flags
Public Const GGO_BITMAP = 1
Public Const GGO_GLYPH_INDEX = &H80
Public Const GGO_GRAY2_BITMAP = 4
Public Const GGO_GRAY4_BITMAP = 5
Public Const GGO_GRAY8_BITMAP = 6
Public Const GGO_METRICS = 0
Public Const GGO_NATIVE = 2

'// Text Antialiasing Flags (LOGFONT.lfQuality)
Public Const NONANTIALIASED_QUALITY = 3
Public Const ANTIALIASED_QUALITY = 4

'// Device Caps
Global Const DRIVERVERSION = 0
   Global Const TECHNOLOGY = 2
   Global Const HORZSIZE = 4
   Global Const VERTSIZE = 6
   Global Const HORZRES = 8
   Global Const VERTRES = 10
   Global Const BITSPIXEL = 12
   Global Const PLANES = 14
   Global Const NUMBRUSHES = 16
   Global Const NUMPENS = 18
   Global Const NUMMARKERS = 20
   Global Const NUMFONTS = 22
   Global Const NUMCOLORS = 24
   Global Const PDEVICESIZE = 26
   Global Const CURVECAPS = 28
   Global Const LINECAPS = 30
   Global Const POLYGONALCAPS = 32
   Global Const TEXTCAPS = 34
   Global Const CLIPCAPS = 36
   Global Const RASTERCAPS = 38
   Global Const ASPECTX = 40
   Global Const ASPECTY = 42
   Global Const ASPECTXY = 44
   Global Const LOGPIXELSX = 88
   Global Const LOGPIXELSY = 90
   Global Const SIZEPALETTE = 104
   Global Const NUMRESERVED = 106
   Global Const COLORRES = 108
   Global Const DT_PLOTTER = 0
   Global Const DT_RASDISPLAY = 1
   Global Const DT_RASPRINTER = 2
   Global Const DT_RASCAMERA = 3
   Global Const DT_CHARSTREAM = 4
   Global Const DT_METAFILE = 5
   Global Const DT_DISPFILE = 6
   Global Const CP_NONE = 0
   Global Const CP_RECTANGLE = 1
   Global Const RC_BITBLT = 1
   Global Const RC_BANDING = 2
   Global Const RC_SCALING = 4
   Global Const RC_BITMAP64 = 8
   Global Const RC_GDI20_OUTPUT = &H10
   Global Const RC_DI_BITMAP = &H80
   Global Const RC_PALETTE = &H100
   Global Const RC_DIBTODEV = &H200
   Global Const RC_BIGFONT = &H400
   Global Const RC_STRETCHBLT = &H800
   Global Const RC_FLOODFILL = &H1000
   Global Const RC_STRETCHDIB = &H2000

Public Const LF_FACESIZE = 32

Public Type LOGFONT
        lfHeight As Long
        lfWidth As Long
        lfEscapement As Long
        lfOrientation As Long
        lfWeight As Long
        lfItalic As Byte
        lfUnderline As Byte
        lfStrikeOut As Byte
        lfCharSet As Byte
        lfOutPrecision As Byte
        lfClipPrecision As Byte
        lfQuality As Byte
        lfPitchAndFamily As Byte
        lfFaceName(1 To LF_FACESIZE) As Byte
End Type

Public Const DEFAULT_CHARSET = 1

Public Const OUT_DEVICE_PRECIS = 5
Public Const OUT_RASTER_PRECIS = 6
Public Const OUT_TT_PRECIS = 4
Public Const OUT_DEFAULT_PRECIS = 0

Public Const GWL_HWNDPARENT = (-8)

Public Const MAX_PATH = 260

Global Const SWP_NOMOVE = 2
Global Const SWP_NOSIZE = 1
Global Const tmFLAGS = SWP_NOMOVE Or SWP_NOSIZE
Global Const HWND_TOPMOST = -1
Global Const HWND_NOTOPMOST = -2

Public Enum APISystemColors
    COLOR_ACTIVEBORDER = 10
    COLOR_ACTIVECAPTION = 2
    COLOR_ADJ_MAX = 100
    COLOR_ADJ_MIN = -100
    COLOR_APPWORKSPACE = 12
    COLOR_BACKGROUND = 1
    COLOR_BTNFACE = 15
    COLOR_BTNHIGHLIGHT = 20
    COLOR_BTNTEXT = 18
    COLOR_BTNSHADOW = 16
    COLOR_CAPTIONTEXT = 9
    COLOR_GRAYTEXT = 17
    COLOR_HIGHLIGHT = 13
    COLOR_HIGHLIGHTTEXT = 14
    COLOR_INACTIVEBORDER = 11
    COLOR_INACTIVECAPTION = 3
    COLOR_INACTIVECAPTIONTEXT = 19
    COLOR_MENU = 4
    COLOR_MENUTEXT = 7
    COLOR_SCROLLBAR = 0
    COLOR_WINDOWFRAME = 6
    COLOR_WINDOW = 5
    COLOR_WINDOWTEXT = 8
End Enum

Public Const DIB_RGB_COLORS = 0 '  color table in RGBs
Public Const DIB_PAL_COLORS = 1 '  color table in palette indices
Public Const DIB_PAL_INDICES = 2 '  No color table indices into surf palette
Public Const BI_RGB = 0&
Public Const OBJ_BITMAP = 7
Public Const OBJ_BRUSH = 2
Public Const OBJ_DC = 3
Public Const OBJ_PAL = 5
Public Const BLACK_BRUSH = 4
Public Const S_OK = &H0

Public Const BF_RECT = (1 Or 2 Or 4 Or 8)
Public Const BF_RIGHT = &H4
Public Const BF_TOP = &H2
Public Const BF_LEFT = &H1
Public Const BF_BOTTOM = &H8
Public Const EDGE_BUMP = (1 Or 8)
Public Const EDGE_ETCHED = (2 Or 4)
Public Const EDGE_RAISED = (1 Or 4)
Public Const EDGE_SUNKEN = (2 Or 8)
Public Const BDR_INNER = &HC
Public Const BDR_OUTER = &H3
Public Const BDR_RAISED = &H5
Public Const BDR_RAISEDINNER = &H4
Public Const BDR_RAISEDOUTER = &H1
Public Const BDR_SUNKEN = &HA
Public Const BDR_SUNKENOUTER = &H2
Public Const BDR_SUNKENINNER = &H8

Public Const MOUSEEVENTF_ABSOLUTE = &H8000 '  absolute move
Public Const MOUSEEVENTF_LEFTDOWN = &H2 '  left button down
Public Const MOUSEEVENTF_LEFTUP = &H4 '  left button up
Public Const MOUSEEVENTF_MIDDLEDOWN = &H20 '  middle button down
Public Const MOUSEEVENTF_MIDDLEUP = &H40 '  middle button up
Public Const MOUSEEVENTF_MOVE = &H1 '  mouse move
Public Const MOUSEEVENTF_RIGHTDOWN = &H8 '  right button down
Public Const MOUSEEVENTF_RIGHTUP = &H10 '  right button up

Public Type OSVERSIONINFO
    dwOSVersionInfoSize As Long
    dwMajorVersion As Long
    dwMinorVersion As Long
    dwBuildNumber As Long
    dwPlatformId As Long
    szCSDVersion As String * 128
End Type
Public Declare Function GetVersionEx Lib "kernel32" Alias "GetVersionExA" (ByRef lpVersionInformation As OSVERSIONINFO) As Long

'return True is the OS is WindowsNT3.5(1), NT4.0, 2000 or XP
Public Function IsWinNT() As Boolean
    Dim OSInfo As OSVERSIONINFO
    OSInfo.dwOSVersionInfoSize = Len(OSInfo)
    'retrieve OS version info
    GetVersionEx OSInfo
    'if we're on NT, return True
    IsWinNT = (OSInfo.dwPlatformId = 2)
End Function


