Attribute VB_Name = "mdlDesktop"
Public Declare Function SystemParametersInfo Lib "user32" Alias "SystemParametersInfoA" (ByVal uAction As Long, ByVal uParam As Long, ByVal lpvParam As Any, ByVal fuWinIni As Long) As Long
Public Const SPI_SETDESKWALLPAPER = 20
Public Enum APISystemColorConstants
    COLOR_ACTIVEBORDER = 10&
    COLOR_ACTIVECAPTION = 2&
    COLOR_ADJ_MAX = 100&
    COLOR_ADJ_MIN = -100&
    COLOR_APPWORKSPACE = 12&
    COLOR_BACKGROUND = 1&
    COLOR_BTNFACE = 15&
    COLOR_BTNHIGHLIGHT = 20&
    COLOR_BTNSHADOW = 16&
    COLOR_BTNTEXT = 18&
    COLOR_CAPTIONTEXT = 9&
    COLOR_GRADIENTACTIVECAPTION = 27&
    COLOR_GRADIENTINACTIVECAPTION = 28&
    COLOR_GRAYTEXT = 17&
    COLOR_HIGHLIGHT = 13&
    COLOR_HIGHLIGHTTEXT = 14&
    COLOR_HOTLIGHT = 26&
    COLOR_INACTIVEBORDER = 11&
    COLOR_INACTIVECAPTION = 3&
    COLOR_INACTIVECAPTIONTEXT = 19&
    COLOR_MENU = 4&
    COLOR_MENUTEXT = 7&
    COLOR_SCROLLBAR = 0&
    COLOR_WINDOW = 5&
    COLOR_WINDOWFRAME = 6&
    COLOR_WINDOWTEXT = 8&
    COLOR_3DDKSHADOW = 21&
    COLOR_3DLIGHT = 22&
    COLOR_INFOTEXT = 23&
    COLOR_INFOBK = 24&
End Enum

Public Declare Function GetSysColor Lib "user32" (ByVal nIndex As APISystemColorConstants) As Long

Function RealColor(ByVal Color As SystemColorConstants) As Long
On Error Resume Next
    Select Case Color
    Case 0
        RealColor = 0
    Case vb3DFace, vbButtonFace
        RealColor = GetSysColor(COLOR_BTNFACE)
    Case vb3DHighlight, vbButtonHighlight
        RealColor = GetSysColor(COLOR_BTNHIGHLIGHT)
    Case vb3DShadow, vbButtonShadow
        RealColor = GetSysColor(COLOR_BTNSHADOW)
    Case vbButtonText
        RealColor = GetSysColor(COLOR_BTNTEXT)
    Case vbWindowText
        RealColor = GetSysColor(COLOR_WINDOWTEXT)
    Case vbWindowBackground
        RealColor = GetSysColor(COLOR_WINDOW)
    Case vbHighlight
        RealColor = GetSysColor(COLOR_HIGHLIGHT)
    Case vbHighlightText
        RealColor = GetSysColor(COLOR_HIGHLIGHTTEXT)
    Case vb3DLight
        RealColor = GetSysColor(COLOR_HOTLIGHT)
    Case Else
        RealColor = Color
    End Select
End Function

