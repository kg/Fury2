Attribute VB_Name = "mdlUI"
Option Explicit

'
'   ::fury² user interface::
'

Public m_UI As Collection

Sub DrawBevel(Surface As Fury2Image, RECT As Fury2Rect, Optional Pushed As Boolean = False, Optional FillColor As Long = c_lngNullColor)
On Error Resume Next
Dim TopLeft As Fury2Color, BottomRight As Fury2Color
    If FillColor = c_lngNullColor Then FillColor = F2RGB(128, 128, 128, 128)
    If Pushed Then
        Set TopLeft = F2RGB(GetRed(FillColor) - c_lngBevelAmount, GetGreen(FillColor) - c_lngBevelAmount, GetBlue(FillColor) - c_lngBevelAmount, GetAlpha(FillColor))
        Set BottomRight = F2RGB(GetRed(FillColor) + c_lngBevelAmount, GetGreen(FillColor) + c_lngBevelAmount, GetBlue(FillColor) + c_lngBevelAmount, GetAlpha(FillColor))
    Else
        Set TopLeft = F2RGB(GetRed(FillColor) + c_lngBevelAmount, GetGreen(FillColor) + c_lngBevelAmount, GetBlue(FillColor) + c_lngBevelAmount, GetAlpha(FillColor))
        Set BottomRight = F2RGB(GetRed(FillColor) - c_lngBevelAmount, GetGreen(FillColor) - c_lngBevelAmount, GetBlue(FillColor) - c_lngBevelAmount, GetAlpha(FillColor))
    End If
    Surface.Fill RECT, FillColor
    Surface.DrawLine F2Rect(RECT.Left, RECT.Top, RECT.Left, RECT.Bottom - 1), TopLeft
    Surface.DrawLine F2Rect(RECT.Left, RECT.Top, RECT.Right - 1, RECT.Top), TopLeft
    Surface.DrawLine F2Rect(RECT.Right - 1, RECT.Top, RECT.Right - 1, RECT.Bottom - 1), BottomRight
    Surface.DrawLine F2Rect(RECT.Left, RECT.Bottom - 1, RECT.Right - 1, RECT.Bottom - 1), BottomRight
End Sub
