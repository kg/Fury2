Attribute VB_Name = "mdlXPIcon"
'
'    Engine (Fury� Game Creation System Runtime Engine)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Private Declare Function LoadImageAsInt Lib "user32" Alias "LoadImageA" ( _
      ByVal hInst As Long, _
      ByVal Ordinal As Long, _
      ByVal uType As Long, _
      ByVal cxDesired As Long, _
      ByVal cyDesired As Long, _
      ByVal fuLoad As Long _
   ) As Long

Private Declare Function GetSystemMetrics Lib "user32" ( _
      ByVal nIndex As Long _
   ) As Long

Private Const SM_CXICON = 11
Private Const SM_CYICON = 12

Private Const SM_CXSMICON = 49
Private Const SM_CYSMICON = 50

Private Const ICON_SMALL = 0
Private Const ICON_BIG = 1

Private Declare Function GetWindow Lib "user32" ( _
   ByVal hwnd As Long, ByVal wCmd As Long) As Long
Private Const GW_OWNER = 4

Public Sub SetAppIcon(ByRef Frm As Object)
On Error Resume Next
Dim l_lngParent As Long
    l_lngParent = GetWindow(Frm.hwnd, GW_OWNER)
    With Frm
        SendMessage .hwnd, WM_SetIcon, 0, LoadResIconH(1, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON))
        SendMessage .hwnd, WM_SetIcon, 1, LoadResIconH(1, GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON))
        SendMessage l_lngParent, WM_SetIcon, 0, LoadResIconH(1, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON))
        SendMessage l_lngParent, WM_SetIcon, 1, LoadResIconH(1, GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON))
    End With
End Sub

Public Sub SetWindowIcon(ByRef Frm As Object, ByRef Icon As String)
On Error Resume Next
Dim l_lngParent As Long
    l_lngParent = GetWindow(Frm.hwnd, GW_OWNER)
    With Frm
        SendMessage .hwnd, WM_SetIcon, 0, LoadFileIcon(Icon, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON))
        SendMessage .hwnd, WM_SetIcon, 1, LoadFileIcon(Icon, GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON))
        SendMessage l_lngParent, WM_SetIcon, 0, LoadFileIcon(Icon, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON))
        SendMessage l_lngParent, WM_SetIcon, 1, LoadFileIcon(Icon, GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON))
    End With
End Sub

Public Function LoadResIconH(ByVal Handle As Long, Optional ByVal Width As Long = 32, Optional ByVal Height As Long = 32) As Long
    LoadResIconH = LoadImageAsInt(App.hInstance, Handle, Image_Icon, Width, Height, LoadImage_Shared)
End Function

Public Function LoadFileIcon(ByRef Filename As String, Optional ByVal Width As Long = 32, Optional ByVal Height As Long = 32) As Long
    LoadFileIcon = LoadImage(App.hInstance, Filename, Image_Icon, Width, Height, LoadImage_Shared Or LoadImage_LoadFromFile)
End Function


