Attribute VB_Name = "mdlLibraries"
'
'   Before using the FMod type library, load it with LoadLibrary. Release it after closing, by calling FreeLibrary.
'

Public Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Public Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long

Public Function LoadFMod() As Long
    LoadFMod = LoadLibrary("FMod.dll")
End Function

Public Sub UnloadFMod(ByRef Handle As Long)
    If Handle = 0 Then Exit Sub
    FreeLibrary Handle
    Handle = 0
End Sub
