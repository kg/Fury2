Attribute VB_Name = "mdlCommonControls"
Option Explicit
Public Type CommonControlsEx
    dwSize As Long
    dwICC As Long
End Type
'Public Declare Sub InitCommonControls Lib "Comctl32.dll" ()
Public Declare Function InitCommonControlsEx Lib "Comctl32.dll" (iccex As CommonControlsEx) As Boolean
Public Const ICC_BAR_CLASSES = &H4
Public Const ICC_COOL_CLASSES = &H400
Public Const ICC_USEREX_CLASSES = &H200& '// comboex
Public Const ICC_WIN95_CLASSES = &HFF&

Public Sub InitCommonControls()
On Error Resume Next
   ' Load Common Controls:
   Dim tIccex As CommonControlsEx
   With tIccex
       .dwSize = LenB(tIccex)
       .dwICC = ICC_BAR_CLASSES
   End With
   'We need to make this call to make sure the common controls are loaded
   InitCommonControlsEx tIccex
End Sub
