Attribute VB_Name = "mdlScrollbars"
Option Explicit
Private Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
Private Const SM_CYHSCROLL = 3
Private Const SM_CXVSCROLL = 2

Function GetScrollbarSize(Scrollbar As FlatScrollBar) As Long
On Error Resume Next
    If Scrollbar.Orientation = efsoHorizontal Then
        GetScrollbarSize = GetSystemMetrics(SM_CYHSCROLL)
    ElseIf Scrollbar.Orientation = efsoVertical Then
        GetScrollbarSize = GetSystemMetrics(SM_CXVSCROLL)
    Else
        GetScrollbarSize = 0
    End If
End Function

