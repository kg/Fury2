Attribute VB_Name = "mdlGDIWrappers"
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

Private m_colIcons As New Collection

Private Declare Function GetWindow Lib "user32" ( _
   ByVal hwnd As Long, ByVal wCmd As Long) As Long
Private Const GW_OWNER = 4

Private Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
Public Declare Function GetTextMetrics Lib "gdi32" Alias "GetTextMetricsA" (ByVal hdc As Long, lpMetrics As TEXTMETRIC) As Long
Public Type TEXTMETRIC
        tmHeight As Long
        tmAscent As Long
        tmDescent As Long
        tmInternalLeading As Long
        tmExternalLeading As Long
        tmAveCharWidth As Long
        tmMaxCharWidth As Long
        tmWeight As Long
        tmOverhang As Long
        tmDigitizedAspectX As Long
        tmDigitizedAspectY As Long
        tmFirstChar As Byte
        tmLastChar As Byte
        tmDefaultChar As Byte
        tmBreakChar As Byte
        tmItalic As Byte
        tmUnderlined As Byte
        tmStruckOut As Byte
        tmPitchAndFamily As Byte
        tmCharSet As Byte
End Type

Private Const ICON_SMALL = 0
Private Const ICON_LARGE = 1

Private Const SM_CXICON = 11
Private Const SM_CYICON = 12

Private Const SM_CXSMICON = 49
Private Const SM_CYSMICON = 50

Option Explicit

Public Function CreateRectangle(X As Long, Y As Long, W As Long, H As Long) As Win32.Rect
    With CreateRectangle
        .Left = X
        .Top = Y
        .Right = X + W
        .Bottom = Y + H
    End With
End Function

Public Sub SolidFill(ByVal DC As Long, ByRef Rectangle As Win32.Rect, ByVal Color As Long)
On Error Resume Next
Dim l_lngBrush As Long
    l_lngBrush = CreateSolidBrush(Color)
    FillRect DC, Rectangle, l_lngBrush
    DeleteObject l_lngBrush
End Sub

Public Function CreateDIBSection_(ByVal DC As Long, ByRef BitmapInfo As BitmapInfo, ByVal flags As Long, ByRef Pointer As Long, ByVal FileMapping As Long, ByVal FileOffset As Long) As Long
On Error Resume Next
    CreateDIBSection_ = CreateDIBSection(DC, BitmapInfo, flags, Pointer, FileMapping, FileOffset)
End Function

Public Function DeleteObject_(ByVal Object As Long) As Long
On Error Resume Next
    DeleteObject_ = DeleteObject(Object)
End Function

Public Function CreateMemoryDC() As Long
On Error Resume Next
Dim deskWnd As Long, deskDC As Long
   deskWnd = GetDesktopWindow
   deskDC = GetDC(deskWnd)
   CreateMemoryDC = CreateCompatibleDC(deskDC)
   ReleaseDC deskWnd, deskDC
End Function

Public Sub DeleteMemoryDC(hdc As Long)
On Error Resume Next
   DeleteDC hdc
End Sub

Function GetIconWidth(ByRef Icon As IPictureDisp) As Long
On Error Resume Next
Dim l_infInfo As IconInfo
Dim bmiDest As BitmapInfo
Dim DC As Long
    GetIconInfo Icon, l_infInfo
    
    DC = CreateMemoryDC
    
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
    
    ' Get header information (interested mainly in size)
    If 0 = GetDIBits(DC, l_infInfo.ColorBitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteObject l_infInfo.MaskBitmap
        DeleteObject l_infInfo.ColorBitmap
        DeleteMemoryDC DC
        Exit Function
    End If
    
    GetIconWidth = CLng(bmiDest.Header.Width)
    
    DeleteMemoryDC DC
    DeleteObject l_infInfo.MaskBitmap
    DeleteObject l_infInfo.ColorBitmap
End Function

Function GetIconMask(ByRef Icon As IPictureDisp) As IPictureDisp
On Error Resume Next
Dim l_infInfo As IconInfo
Dim bmiDest As BitmapInfo
Dim DC As Long
    GetIconInfo Icon, l_infInfo
    
    DC = CreateMemoryDC
    
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
    
    ' Get header information (interested mainly in size)
    If 0 = GetDIBits(DC, l_infInfo.ColorBitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteObject l_infInfo.MaskBitmap
        DeleteObject l_infInfo.ColorBitmap
        DeleteMemoryDC DC
        Exit Function
    End If
        
    DeleteMemoryDC DC
    DeleteObject l_infInfo.ColorBitmap
    Set GetIconMask = CreatePictureFromHandle(l_infInfo.MaskBitmap, PicType_Bitmap)
End Function

Function GetIconHeight(ByRef Icon As IPictureDisp) As Long
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
Dim l_infInfo As IconInfo
    GetIconInfo Icon, l_infInfo
    
    DC = CreateMemoryDC
    
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
    
    ' Get header information (interested mainly in size)
    If 0 = GetDIBits(DC, l_infInfo.ColorBitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteObject l_infInfo.MaskBitmap
        DeleteObject l_infInfo.ColorBitmap
        DeleteMemoryDC DC
        Exit Function
    End If
    
    GetIconHeight = CLng(bmiDest.Header.Height)
    
    DeleteMemoryDC DC
    DeleteObject l_infInfo.MaskBitmap
    DeleteObject l_infInfo.ColorBitmap
End Function

Function GetPictureWidth(ByRef Pic As IPictureDisp) As Long
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
   
    DC = CreateMemoryDC
  
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
    
    ' Get header information (interested mainly in size)
    If 0 = GetDIBits(DC, Pic.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteMemoryDC DC
        Exit Function
    End If
    
    GetPictureWidth = CLng(bmiDest.Header.Width)
    
    DeleteMemoryDC DC
    
End Function

Function GetPictureHeight(ByRef Pic As IPictureDisp) As Long
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
    
    DC = CreateMemoryDC
    
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
    
    ' Get header information (interested mainly in size)
    If 0 = GetDIBits(DC, Pic.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteMemoryDC DC
        Exit Function
    End If
    
    GetPictureHeight = CLng(bmiDest.Header.Height)
    
    DeleteMemoryDC DC
    
End Function

Function GetPictureHandleWidth(ByVal Pic As Long) As Long
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
   
    DC = CreateMemoryDC
  
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
    
    ' Get header information (interested mainly in size)
    If 0 = GetDIBits(DC, Pic, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteMemoryDC DC
        Exit Function
    End If
    
    GetPictureHandleWidth = CLng(bmiDest.Header.Width)
    
    DeleteMemoryDC DC
    
End Function

Function GetPictureHandleHeight(ByVal Pic As Long) As Long
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
    
    DC = CreateMemoryDC
    
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
    
    ' Get header information (interested mainly in size)
    If 0 = GetDIBits(DC, Pic, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteMemoryDC DC
        Exit Function
    End If
    
    GetPictureHandleHeight = CLng(bmiDest.Header.Height)
    
    DeleteMemoryDC DC
    
End Function

Public Function IID_IPictureDisp() As IId
    Dim picGuid As IId
    picGuid.X = &H7BF80981
    picGuid.S1 = &HBF32
    picGuid.S2 = &H101A
    picGuid.C(0) = &H8B
    picGuid.C(1) = &HBB
    picGuid.C(2) = &H0
    picGuid.C(3) = &HAA
    picGuid.C(4) = &H0
    picGuid.C(5) = &H30
    picGuid.C(6) = &HC
    picGuid.C(7) = &HAB
    IID_IPictureDisp = picGuid
End Function

Sub PixelsFromIcon(ByRef Icon As IPictureDisp, ByVal Pointer As Long)
On Error Resume Next
    Dim bmiDest As BitmapInfo
    Dim hdcMem As Long
    Dim deskWnd As Long, deskDC As Long
    Dim l_infInfo As IconInfo
    
    GetIconInfo Icon, l_infInfo
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    hdcMem = CreateCompatibleDC(deskDC)
    
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
   
    ' Get header information (interested mainly in size)
    If GetDIBits(hdcMem, l_infInfo.ColorBitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) = 0 Then
        DeleteObject l_infInfo.MaskBitmap
        DeleteObject l_infInfo.ColorBitmap
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        Exit Sub
    End If

    With bmiDest.Header
        .BitCount = 32
        .Compression = DIBCompression_RGB
        .Height = -(bmiDest.Header.Height)
    End With
     
    If GetDIBits(hdcMem, l_infInfo.ColorBitmap, 0, Abs(bmiDest.Header.Height), ByVal Pointer, bmiDest, DIBMode_RGB) = 0 Then
    End If
   
    DeleteDC hdcMem
    ReleaseDC deskWnd, deskDC
    DeleteObject l_infInfo.MaskBitmap
    DeleteObject l_infInfo.ColorBitmap
   
End Sub

Sub PixelsFromPicture(ByRef Pic As IPictureDisp, ByVal Pointer As Long)
On Error Resume Next
    PixelsFromPictureHandle Pic.Handle, Pointer
End Sub

Sub PixelsToPicture(ByRef Pic As IPictureDisp, ByVal Pointer As Long)
On Error Resume Next
    PixelsToPictureHandle Pic.Handle, Pointer
End Sub

Public Function CreatePictureFromHandle(ByVal Handle As Long, ByVal PicType As PicTypes) As IPictureDisp
On Error Resume Next
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim Pic As IPictureDisp
    picGuid = IID_IPictureDisp()
    picDesc.Size = Len(picDesc)
    picDesc.Bitmap = Handle
    picDesc.Type = PicType
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        Exit Function
    End If
    Set CreatePictureFromHandle = Pic
    Set Pic = Nothing
End Function

Public Function CreatePicture(ByVal Width As Long, ByVal Height As Long) As IPictureDisp
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hBmp As Long
    Dim hOldBmp As Long
    Dim Pic As IPictureDisp
    Dim rcBitmap As Rect
    Dim hdcMem As Long
    Dim deskWnd As Long, deskDC As Long
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    
    picGuid = IID_IPictureDisp
    
    picDesc.Size = Len(picDesc)
    hdcMem = CreateCompatibleDC(deskDC)
    If hdcMem = 0 Then
        Exit Function
    End If
    hBmp = CreateCompatibleBitmap(deskDC, Width, Height)
    If hBmp = 0 Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        Exit Function
    End If
    picDesc.Bitmap = hBmp
    picDesc.Palette = GetCurrentObject(deskDC, Object_Palette)
    picDesc.Type = PicType_Bitmap
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        DeleteObject hBmp
        Exit Function
    End If
    Set CreatePicture = Pic
    Set Pic = Nothing
    DeleteDC hdcMem
    ReleaseDC deskWnd, deskDC
End Function

#If fury2 = 1 Then
Public Function LoadIconObject(Filename As String, Optional SizeX As Long = 16, Optional SizeY As Long = 16) As IPictureDisp
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hdcMem As Long
    Dim Pic As IPictureDisp
    Dim deskWnd As Long, deskDC As Long
    Dim hIcon As Long
    
    If Trim(Filename) = "" Then Exit Function
    If FileLen(Filename) <= 0 Then Exit Function
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    
    picGuid = IID_IPictureDisp
     
    hIcon = LoadImage(vbNull, Filename, Image_Icon, SizeX, SizeY, LoadImage_LoadFromFile)
    If hIcon = 0 Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        Exit Function
    End If
    
    picDesc.Size = Len(picDesc)
    picDesc.Bitmap = hIcon
    picDesc.Type = PicType_Icon
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        Exit Function
    End If
    Set LoadIconObject = Pic
    Set Pic = Nothing
    DeleteDC hdcMem
    ReleaseDC deskWnd, deskDC
End Function

Sub SetFormIcon(Form As Form, Icon As IPictureDisp, Optional IconType As Long = ICON_SMALL, Optional SetGlobalIcon As Boolean = False)
On Error Resume Next
Dim l_lngHWnd As Long
    If Icon Is Nothing Then Exit Sub
    If Form Is Nothing Then Exit Sub
    If IconType = ICON_SMALL Then Set Form.Icon = Icon
    SendMessage Form.hwnd, WM_SetIcon, ByVal IconType, ByVal Icon.Handle
    If (TypeOf Form Is MDIForm) Or (SetGlobalIcon) Then
        l_lngHWnd = Form.hwnd
        Do While l_lngHWnd <> 0
            l_lngHWnd = GetWindow(l_lngHWnd, GW_OWNER)
            If l_lngHWnd <> 0 Then
                SendMessage l_lngHWnd, WM_SetIcon, ByVal IconType, ByVal Icon.Handle
            End If
        Loop
    End If
End Sub
#End If

Sub PaintPictureEx(ByVal Target As Object, ByVal Picture As Object, ByVal X As Long, ByVal Y As Long, ByVal W As Long, H As Long)
On Error Resume Next
    Select Case Picture.Type
    Case PicType_Icon
        DrawIconEx Target.hdc, X, Y, Picture.Handle, W, H, 0, 0, 3
    Case Else
        Target.PaintPicture Picture, X, Y
    End Select
End Sub

Sub PixelsFromPictureHandle(ByVal Pic As Long, ByVal Pointer As Long)
On Error Resume Next
    Dim bmiDest As BitmapInfo
    Dim hdcMem As Long
    Dim deskWnd As Long, deskDC As Long
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    hdcMem = CreateCompatibleDC(deskDC)
    
    With bmiDest.Header
        .Size = Len(bmiDest.Header)
        .Planes = 1
    End With
   
    ' Get header information (interested mainly in size)
    If GetDIBits(hdcMem, Pic, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) = 0 Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        Exit Sub
    End If

    With bmiDest.Header
        .BitCount = 32
        .Compression = DIBCompression_RGB
        .Height = -(bmiDest.Header.Height)
    End With
     
    If GetDIBits(hdcMem, Pic, 0, Abs(bmiDest.Header.Height), ByVal Pointer, bmiDest, DIBMode_RGB) = 0 Then
    End If
   
    DeleteDC hdcMem
    ReleaseDC deskWnd, deskDC
   
End Sub

Sub PixelsToPictureHandle(ByVal Pic As Long, ByVal Pointer As Long)
On Error Resume Next
    Dim bmiDest As BitmapInfo
    Dim hdcMem As Long
    Dim deskWnd As Long, deskDC As Long
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    hdcMem = CreateCompatibleDC(deskDC)
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(hdcMem, Pic, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
       .Height = -(bmiDest.Header.Height)
   End With
     
   Call SetDIBits(hdcMem, Pic, 0, Abs(bmiDest.Header.Height), ByVal Pointer, bmiDest, DIBMode_RGB)
   
   DeleteDC hdcMem
   ReleaseDC deskWnd, deskDC
   
End Sub

