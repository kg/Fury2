Attribute VB_Name = "mdlGlobals"
Option Explicit

Public Function ConvertSystemColor(ByVal Index As SystemColors) As Long
On Error Resume Next
    ConvertSystemColor = SetAlpha(SwapChannels(GetSystemColor(Index), Red, Blue), 255)
End Function
