Attribute VB_Name = "mdlOverride"
Option Explicit
Declare Function GetOverrideKey Lib "SoftFX" Alias "_GetOverrideKey@4" (ByVal Index As Long) As Long
Declare Function GetOverrideIndex Lib "SoftFX" Alias "_GetOverrideIndex@4" (ByVal Name As String) As Long
Declare Function AddOverride Lib "SoftFX" Alias "_AddOverride@8" (ByVal Name As String, ByVal Ptr As Long) As Long
Declare Function GetOverrideCount Lib "SoftFX" Alias "_GetOverrideCount@4" (ByVal Name As String) As Long
Declare Function RemoveOverride Lib "SoftFX" Alias "_RemoveOverride@8" (ByVal Name As String, ByVal Ptr As Long) As Long
Declare Function KeyMatches Lib "SoftFX" Alias "_KeyMatches@8" (ByVal Ptr As Long, ByVal Match As String) As Long

