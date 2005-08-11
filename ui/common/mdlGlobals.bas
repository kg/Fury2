Attribute VB_Name = "mdlGlobals"
Option Explicit
Global g_booMenuIsOpen As Boolean
Global g_fntMarlett As StdFont
Global g_rfThemeFile As ngResourceFile
Global g_strThemePattern As String
Global g_strTabTheme As String
Global g_strToolbarTheme As String
Global g_rfMenuThemeFile As ngResourceFile
Global g_strMenuThemePattern As String
Global g_colOpenMenus As New Collection
Global g_lngMenus As Long, g_lngMenuItems As Long, g_lngMenuHosts As Long

Public Function ConvertSystemColor(ByVal Index As SystemColors) As Long
On Error Resume Next
    ConvertSystemColor = SetAlpha(SwapChannels(GetSystemColor(Index), Red, Blue), 255)
End Function

Public Function GetTextWidth(ByRef Text As String, ByRef Font As StdFont) As Long
On Error Resume Next
    Set frmFont.Font = Font
    GetTextWidth = frmFont.TextWidth(Text)
End Function

Public Function GetTextHeight(ByRef Text As String, ByRef Font As StdFont) As Long
On Error Resume Next
    Set frmFont.Font = Font
    GetTextHeight = frmFont.TextHeight(Text)
End Function

Public Sub FlushForms()
On Error Resume Next
Dim l_lngIndex As Long
    Do While l_lngIndex < Forms.Count
        If Forms(l_lngIndex).Tag = "Unload" Then
            Unload Forms(l_lngIndex)
        Else
            l_lngIndex = l_lngIndex + 1
        End If
    Loop
End Sub
