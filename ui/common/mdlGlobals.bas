Attribute VB_Name = "mdlGlobals"
Option Explicit
Global g_rfThemeFile As ngResourceFile
Global g_strThemePattern As String
Global g_strTabTheme As String
Global g_strToolbarTheme As String

Public Function ConvertSystemColor(ByVal Index As SystemColors) As Long
On Error Resume Next
    ConvertSystemColor = SetAlpha(SwapChannels(GetSystemColor(Index), Red, Blue), 255)
End Function
