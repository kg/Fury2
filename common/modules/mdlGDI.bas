Attribute VB_Name = "mdlGDI"
Option Explicit

Public Sub GDIFill(hdc As Long, Rectangle As Win32.Rect, Color As Long)
On Error Resume Next
Dim m_lngBrush As Long
    m_lngBrush = CreateSolidBrush(Color)
    FillRect hdc, Rectangle, m_lngBrush
    DeleteObject m_lngBrush
End Sub

Public Function GDIRect(x1 As Long, y1 As Long, x2 As Long, y2 As Long, Optional Absolute As Boolean = True) As Win32.Rect
On Error Resume Next
    With GDIRect
        .Left = x1
        .Top = y1
        If Absolute Then
            .Right = x2
            .Bottom = y2
        Else
            .Right = x1 + x2
            .Bottom = y1 + y2
        End If
    End With
End Function

'Public Function CreateFontSimplex(Font As IFontDisp) As Long
'Dim hFont As Long
'Dim lFont As LogicalFont
'Dim FontName As String * LF_FACESIZE
'Dim LogYPixels As Long, deskWnd As Long, deskDC As Long
'    deskWnd = GetDesktopWindow
'    deskDC = GetDC(deskWnd)
'    LogYPixels = GetDeviceCaps(deskDC, LOGPIXELSY)
'    ReleaseDC deskWnd, deskDC
'    deskWnd = 0
'    deskDC = 0
'    FontName = Font.Name
'    CopyMemory lFont.lfFaceName(1), FontName, LF_FACESIZE
'    FontName = Chr(0)
'    CopyMemory lFont.lfFaceName(32), FontName, 1
'    With lFont
'        .lfHeight = -((Font.Size * LogYPixels) / 72)
'        .lfQuality = ANTIALIASED_QUALITY
'        .lfItalic = Font.Italic
'        .lfStrikeOut = Font.Strikethrough
'        .lfUnderline = Font.Underline
'        .lfOutPrecision = OUT_DEVICE_PRECIS
'        If Font.Bold Then
'            .lfWeight = 700 ' Bold
'        Else
'            .lfWeight = 400 ' Normal
'        End If
'        .lfCharSet = DEFAULT_CHARSET
'    End With
'    hFont = CreateFontIndirect(lFont)
'    CreateFontSimplex = hFont
'End Function


Function IconFromHandle(Handle As Long) As IPictureDisp
On Error Resume Next
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim Pic As IPictureDisp
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    picDesc.Size = Len(picDesc)
    picDesc.Bitmap = Handle
    picDesc.Type = PicType_Icon
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        Exit Function
    End If
    Set IconFromHandle = Pic
    Set Pic = Nothing
End Function

Function BitmapFromHandle(Handle As Long) As IPictureDisp
On Error Resume Next
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim Pic As IPictureDisp
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    picDesc.Size = Len(picDesc)
    picDesc.Bitmap = Handle
    picDesc.Type = PicType_Bitmap
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        Exit Function
    End If
    Set BitmapFromHandle = Pic
    Set Pic = Nothing
End Function

Sub SetFormIcon(Frm As Form, Ico As IPictureDisp)
    SendMessage Frm.hWnd, WM_SetIcon, 0, ByVal Ico.Handle
End Sub

Sub ArrayToDC(ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByRef Data() As Long)
On Error Resume Next
Dim bitDesc As BitmapInfo
    With bitDesc.Header
        .Size = Len(bitDesc.Header)
        .Planes = 1
        .BitCount = 32
        .Compression = DIBCompression_RGB
        .Width = UBound(Data, 1) + 1
        .Height = -(UBound(Data, 2) + 1)
        .SizeImage = 0
        .XPixelsPerMeter = 0
        .YPixelsPerMeter = 0
        .ColorImportant = vbNull
        .ColorUsed = vbNull
    End With
    StretchDIBits hdc, x, y, UBound(Data, 1) + 1, UBound(Data, 2) + 1, 0, 0, UBound(Data, 1) + 1, UBound(Data, 2) + 1, VarPtr(Data(0, 0)), bitDesc, DIBMode_RGB, vbSrcCopy
End Sub

Sub ArrayPtrToDC(ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal Width As Long, ByVal Height As Long, ByVal Ptr As Long)
On Error Resume Next
Dim bitDesc As BitmapInfo
    With bitDesc.Header
        .Size = Len(bitDesc.Header)
        .Planes = 1
        .BitCount = 32
        .Compression = DIBCompression_RGB
        .Width = Width
        .Height = -(Height)
    End With
    StretchDIBits hdc, x, y, Width, Height, 0, 0, Width, Height, Ptr, bitDesc, DIBMode_RGB, vbSrcCopy
End Sub

Sub ArrayPtrToDCNF(ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal Width As Long, ByVal Height As Long, ByVal Ptr As Long)
On Error Resume Next
Dim bitDesc As BitmapInfo
    With bitDesc.Header
        .Size = Len(bitDesc.Header)
        .Planes = 1
        .BitCount = 32
        .Compression = DIBCompression_RGB
        .Width = Width
        .Height = (Height)
    End With
    StretchDIBits hdc, x, y, Width, Height, 0, 0, Width, Height, Ptr, bitDesc, DIBMode_RGB, vbSrcCopy
End Sub

Sub ArrayToPictureBox(ByRef PictureBox As PictureBox, ByRef ImageArray() As Long)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As RGBQuad
   MinX = LBound(ImageArray, 1)
   MinY = LBound(ImageArray, 2)
   MaxX = UBound(ImageArray, 1)
   MaxY = UBound(ImageArray, 2)
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   If PictureBox.Image Is Nothing Then
       Exit Sub
   End If
  
  
   Set PictureBox.Picture = PictureBox.Image
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(PictureBox.hdc, PictureBox.Picture.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   ' Allocate space according to retrieved size
   ReDim DIBitsDest(0 To bmiDest.Header.Width * bmiDest.Header.Height - 1)
   
   ' Now get the bits
   If 0 = GetDIBits(PictureBox.hdc, PictureBox.Picture.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If
   
   For SetY = MinY To MaxY
       For SetX = MinX To MaxX
           DestAddress = (SetX - MinX) + ((((SetY - MinY) + (YOffset))) * bmiDest.Header.Width)
           ' Copy the array data to the image
           CopyMemory DIBitsDest(DestAddress), ImageArray(SetX, (MaxY - SetY)), 4
       Next SetX
   Next SetY
   
   SelectObject PictureBox.hdc, 0
   Call SetDIBits(PictureBox.hdc, PictureBox.Picture.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB)
   SelectObject PictureBox.hdc, PictureBox.Picture.Handle
   
   PictureBox.Refresh
End Sub

Sub ArrayToForm(ByRef Form As Form, ByRef ImageArray() As Long)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As RGBQuad
   MinX = LBound(ImageArray, 1)
   MinY = LBound(ImageArray, 2)
   MaxX = UBound(ImageArray, 1)
   MaxY = UBound(ImageArray, 2)
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   If Form.Image Is Nothing Then
       Exit Sub
   End If
  
   Set Form.Picture = Form.Image
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(Form.hdc, Form.Picture.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   ' Allocate space according to retrieved size
   ReDim DIBitsDest(0 To bmiDest.Header.Width * bmiDest.Header.Height - 1)
   
   ' Now get the bits
   If 0 = GetDIBits(Form.hdc, Form.Picture.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If
   
   For SetY = MinY To MaxY
       For SetX = MinX To MaxX
           DestAddress = (SetX - MinX) + ((((SetY - MinY) + (YOffset))) * bmiDest.Header.Width)
           ' Copy the array data to the image
           CopyMemory DIBitsDest(DestAddress), ImageArray(SetX, (MaxY - SetY)), 4
       Next SetX
   Next SetY
   
   SelectObject Form.hdc, 0
   Call SetDIBits(Form.hdc, Form.Picture.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB)
   SelectObject Form.hdc, Form.Picture.Handle
   
   Form.Refresh
End Sub

Sub ArrayToPicture(ByRef Pic As IPictureDisp, ByRef ImageArray() As Long, ByVal DC As Long)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As RGBQuad
   MinX = LBound(ImageArray, 1)
   MinY = LBound(ImageArray, 2)
   MaxX = UBound(ImageArray, 1)
   MaxY = UBound(ImageArray, 2)
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Pic.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   ' Allocate space according to retrieved size
   ReDim DIBitsDest(0 To bmiDest.Header.Width * bmiDest.Header.Height - 1)
   
   ' Now get the bits
   If 0 = GetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If
   
   For SetY = MinY To MaxY
       For SetX = MinX To MaxX
           DestAddress = (SetX - MinX) + ((((SetY - MinY) + (YOffset))) * bmiDest.Header.Width)
           ' Copy the array data to the image
           CopyMemory DIBitsDest(DestAddress), ImageArray(SetX, (MaxY - SetY)), 4
       Next SetX
   Next SetY
   
   Call SetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB)
   
End Sub

Sub ArrayToPictureEx(ByRef Pic As IPictureDisp, ByRef ImageArray() As Long, ByVal DC As Long, ByVal dx As Long, ByVal dy As Long, ByVal SX As Long, ByVal SY As Long)
On Error Resume Next
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As RGBQuad
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long

    MinX = SX
    MinY = SY
    MaxX = SX + UBound(ImageArray, 1) + 1
    MaxY = SY + UBound(ImageArray, 2) + 1
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Pic.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   ' Allocate space according to retrieved size
   ReDim DIBitsDest(0 To bmiDest.Header.Width * bmiDest.Header.Height - 1)
   
   ' Now get the bits
   If 0 = GetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If
   
   For SetY = MinY To MaxY
       For SetX = MinX To MaxX
           DestAddress = ((SetX - SX) + dx) + (((((SetY - SY) + dy) + (YOffset))) * bmiDest.Header.Width)
           ' Copy the array data to the image
           CopyMemory DIBitsDest(DestAddress), ImageArray(SetX, (MaxY - SetY)), 4
       Next SetX
   Next SetY
   
   Call SetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0), bmiDest, DIBMode_RGB)
   
End Sub

Sub ByteArrayToPicture(ByRef Pic As IPictureDisp, ByRef ImageArray() As Byte, ByVal DC As Long)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As RGBQuad
   MinX = LBound(ImageArray, 1) \ 4
   MinY = LBound(ImageArray, 2)
   MaxX = UBound(ImageArray, 1) \ 4
   MaxY = UBound(ImageArray, 2)
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Pic.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
       .Height = -.Height
   End With
     
   Call SetDIBits(DC, Pic.Handle, 0, -bmiDest.Header.Height, ImageArray(0, 0), bmiDest, DIBMode_RGB)
   
End Sub

Sub BytePtrToPicture(ByRef Pic As IPictureDisp, ByVal Ptr As Long, ByVal DC As Long)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As RGBQuad
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Pic.Handle, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
       .Height = -.Height
   End With
     
   Call SetDIBits(DC, Pic.Handle, 0, -bmiDest.Header.Height, ByVal Ptr, bmiDest, DIBMode_RGB)
   
End Sub

Sub BytePtrToBitmap(ByVal Bitmap As Long, ByVal Ptr As Long)
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
    
   DC = CreateMemoryDC
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Bitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
       .Height = -.Height
   End With
     
   Call SetDIBits(DC, Bitmap, 0, -bmiDest.Header.Height, ByVal Ptr, bmiDest, DIBMode_RGB)
   
   DeleteMemoryDC DC
   
End Sub

Function GetPictureArray(ByRef Pic As IPictureDisp) As Long()
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As Long, DC As Long
    
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

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   ' Allocate space according to retrieved size
   ReDim DIBitsDest(0 To bmiDest.Header.Width - 1, 0 To bmiDest.Header.Height - 1)
   
   ' Now get the bits
   If 0 = GetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0, 0), bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Function
   End If
   
   GetPictureArray = DIBitsDest
   
   DeleteMemoryDC DC
   
End Function

Function GetPictureArrayInv(ByRef Pic As IPictureDisp) As Long()
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DIBitsDest() As Long, DC As Long
    
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

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   ' Allocate space according to retrieved size
   ReDim DIBitsDest(0 To bmiDest.Header.Width - 1, 0 To bmiDest.Header.Height - 1)
   
   bmiDest.Header.Height = -bmiDest.Header.Height
   
   ' Now get the bits
   If 0 = GetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), DIBitsDest(0, 0), bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Function
   End If
   
   bmiDest.Header.Height = -bmiDest.Header.Height
   
   GetPictureArrayInv = DIBitsDest
   
   DeleteMemoryDC DC
   
End Function

Sub GetPictureArrayByte(ByRef Pic As IPictureDisp, ByRef DestArray() As Byte)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
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
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   bmiDest.Header.Height = -bmiDest.Header.Height
   
   ' Now get the bits
   If 0 = GetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), DestArray(0, 0), bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Sub
   End If
   
   bmiDest.Header.Height = -bmiDest.Header.Height
   
   DeleteMemoryDC DC
   
End Sub

Sub GetPictureArrayPtr(ByRef Pic As IPictureDisp, ByVal Ptr As Long)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
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
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With

   bmiDest.Header.Height = -bmiDest.Header.Height
   
   ' Now get the bits
   If 0 = GetDIBits(DC, Pic.Handle, 0, Abs(bmiDest.Header.Height), ByVal Ptr, bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Sub
   End If
   
   bmiDest.Header.Height = -bmiDest.Header.Height
   
   DeleteMemoryDC DC
   
End Sub

Sub GetBitmapArrayPtr(ByVal Bitmap As Long, ByVal Ptr As Long)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DC As Long
    
   DC = CreateMemoryDC
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Bitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With

   bmiDest.Header.Height = -bmiDest.Header.Height
   
   ' Now get the bits
   If 0 = GetDIBits(DC, Bitmap, 0, Abs(bmiDest.Header.Height), ByVal Ptr, bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Sub
   End If
   
   bmiDest.Header.Height = -bmiDest.Header.Height
   
   DeleteMemoryDC DC
   
End Sub

Sub GetHBmpArrayByte(ByVal hBmp As Long, ByRef DestArray() As Byte, Optional ByVal CPDC As Long = 0)
On Error Resume Next
Dim MinX As Long, MinY As Long
Dim MaxX As Long, MaxY As Long
Dim SetX As Long, SetY As Long
Dim DestAddress As Long, YOffset As Long
Dim bmiDest As BitmapInfo
Dim DC As Long

   If CPDC = 0 Then DC = CreateMemoryDC Else DC = CPDC
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, hBmp, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Sub
   End If

   With bmiDest.Header
       .BitCount = 32
       .Compression = DIBCompression_RGB
   End With
   
   YOffset = (bmiDest.Header.Height - 1) - MaxY

   bmiDest.Header.Height = -bmiDest.Header.Height
   
   ' Now get the bits
   If 0 = GetDIBits(DC, hBmp, 0, Abs(bmiDest.Header.Height), DestArray(0, 0), bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Sub
   End If
   
   bmiDest.Header.Height = -bmiDest.Header.Height
   
   If CPDC = 0 Then DeleteMemoryDC DC
   
End Sub

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


Function GetBitmapWidth(ByVal Bitmap As Long) As Long
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
    
   DC = CreateMemoryDC
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Bitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Function
   End If
   
   GetBitmapWidth = CLng(bmiDest.Header.Width)
   
   DeleteMemoryDC DC
   
End Function

Function GetBitmapHeight(ByVal Bitmap As Long) As Long
On Error Resume Next
Dim bmiDest As BitmapInfo
Dim DC As Long
    
   DC = CreateMemoryDC
   
   With bmiDest.Header
       .Size = Len(bmiDest.Header)
       .Planes = 1
   End With
   
   ' Get header information (interested mainly in size)
   If 0 = GetDIBits(DC, Bitmap, 0, 0, ByVal 0&, bmiDest, DIBMode_RGB) Then
       DeleteMemoryDC DC
       Exit Function
   End If
   
   GetBitmapHeight = CLng(bmiDest.Header.Height)
   
   DeleteMemoryDC DC
   
End Function

Public Function ColorFromString(Text As String) As Long
On Error Resume Next
Dim cR As Long, cG As Long, cB As Long
    If Left(Text, 2) = "&H" Then
        ColorFromString = CLng(Text)
        Exit Function
    End If
    If Left(Text, 1) = "#" Then
        ColorFromString = CLng("&H" + Mid(Text, 2))
        cR = ColorFromString Mod 256
        cG = Int((ColorFromString \ 256)) Mod 256
        cB = Int(ColorFromString \ 65536)
        ColorFromString = RGB(cB, cG, cR)
        Exit Function
    End If
    If Left(Text, 1) = "." Then
        ColorFromString = CLng(Mid(Text, 2))
        Exit Function
    End If
    If (InStr(Text, ",")) Then
        cR = CLng(Left(Text, InStr(Text, ",") - 1))
        cG = CLng(Mid(Text, InStr(Text, ",") + 1, ((InStrRev(Text, ",") - 1) - (InStr(Text, ",")))))
        cB = CLng(Mid(Text, InStrRev(Text, ",") + 1))
        ColorFromString = RGB(cR, cG, cB)
        Exit Function
    End If
    Select Case Trim(LCase(Text))
    Case "aliceblue", "alice blue"
        ColorFromString = ColorFromString("#F0F8FF")
    Case "antiquewhite", "antique white"
        ColorFromString = ColorFromString("#FAEBD7")
    Case "aqua"
        ColorFromString = ColorFromString("#00FFFF")
    Case "aquamarine"
        ColorFromString = ColorFromString("#7FFFD4")
    Case "azure"
        ColorFromString = ColorFromString("#F0FFFF")
    Case "beige"
        ColorFromString = ColorFromString("#F5F5DC")
    Case "bisque"
        ColorFromString = ColorFromString("#FFE4C4")
    Case "black"
        ColorFromString = ColorFromString("#000000")
    Case "blanchedalmond", "blanched almond"
        ColorFromString = ColorFromString("#FFEBCD")
    Case "blue"
        ColorFromString = ColorFromString("#0000FF")
    Case "blueviolet", "blue-violet", "blue violet"
        ColorFromString = ColorFromString("#8A2BE2")
    Case "brown"
        ColorFromString = ColorFromString("#A52A2A")
    Case "bright ass orange", "brightassorange"
        ColorFromString = ColorFromString("#FFA500")
    Case "burlywood"
        ColorFromString = ColorFromString("#DEB887")
    Case "cadetblue", "cadet blue"
        ColorFromString = ColorFromString("#5F9EA0")
    Case "chartreuse"
        ColorFromString = ColorFromString("#7FFF00")
    Case "chocolate"
        ColorFromString = ColorFromString("#D2691E")
    Case "coral"
        ColorFromString = ColorFromString("#FF7F50")
    Case "cornflowerblue", "cornflower blue"
        ColorFromString = ColorFromString("#6495ED")
    Case "cornsilk"
        ColorFromString = ColorFromString("#FFF8DC")
    Case "crimson"
        ColorFromString = ColorFromString("#DC143C")
    Case "cyan"
        ColorFromString = ColorFromString("#00FFFF")
    Case "darkblue", "dark blue"
        ColorFromString = ColorFromString("#00008B")
    Case "darkcyan", "dark cyan"
        ColorFromString = ColorFromString("#008B8B")
    Case "darkgoldenrod", "dark goldenrod"
        ColorFromString = ColorFromString("#B8860B")
    Case "darkgray", "dark gray"
        ColorFromString = ColorFromString("#A9A9A9")
    Case "darkgreen", "dark green"
        ColorFromString = ColorFromString("#006400")
    Case "darkkhaki", "dark khaki"
        ColorFromString = ColorFromString("#BDB76B")
    Case "darkmagenta", "dark magenta"
        ColorFromString = ColorFromString("#8B008B")
    Case "darkolivegreen", "dark olive green"
        ColorFromString = ColorFromString("#556B2F")
    Case "darkorange", "dark orange"
        ColorFromString = ColorFromString("#FF8C00")
    Case "darkorchid", "dark orchid"
        ColorFromString = ColorFromString("#9932CC")
    Case "darkrealm"
        ColorFromString = ColorFromString("#202020")
    Case "darkred", "dark red"
        ColorFromString = ColorFromString("#8B0000")
    Case "darksalmon", "dark salmon"
        ColorFromString = ColorFromString("#E9967A")
    Case "darkseagreen", "dark sea green"
        ColorFromString = ColorFromString("#8FBC8F")
    Case "darkslateblue", "dark slate blue"
        ColorFromString = ColorFromString("#483D8B")
    Case "darkslategray", "dark slate gray"
        ColorFromString = ColorFromString("#2F4F4F")
    Case "darkturquoise", "dark turquoise"
        ColorFromString = ColorFromString("#00CED1")
    Case "darkviolet", "dark violet"
        ColorFromString = ColorFromString("#9400D3")
    Case "deeppink", "deep pink"
        ColorFromString = ColorFromString("#FF1493")
    Case "deepskyblue", "deep sky blue"
        ColorFromString = ColorFromString("#00BFBF")
    Case "dimgray", "dim gray"
        ColorFromString = ColorFromString("#696969")
    Case "dodgerblue", "dodger blue"
        ColorFromString = ColorFromString("#1E90FF")
    Case "firebrick"
        ColorFromString = ColorFromString("#B22222")
    Case "floralwhite", "floral white"
        ColorFromString = ColorFromString("#FFFAF0")
    Case "forestgreen", "forest green"
        ColorFromString = ColorFromString("#228B22")
    Case "fuchsia"
        ColorFromString = ColorFromString("#FF00FF")
    Case "gainsboro"
        ColorFromString = ColorFromString("#DCDCDC")
    Case "ghalieon"
        ColorFromString = ColorFromString("#380000")
    Case "ghostwhite", "ghost white"
        ColorFromString = ColorFromString("#F8F8FF")
    Case "gold"
        ColorFromString = ColorFromString("#FFD700")
    Case "goldenrod"
        ColorFromString = ColorFromString("#DAA520")
    Case "gray"
        ColorFromString = ColorFromString("#808080")
    Case "green"
        ColorFromString = ColorFromString("#008000")
    Case "greenyellow", "green-yellow", "green yellow"
        ColorFromString = ColorFromString("#ADFF2F")
    Case "honeydew"
        ColorFromString = ColorFromString("#F0FFF0")
    Case "hotpink", "hot pink"
        ColorFromString = ColorFromString("#FF69B4")
    Case "indianred", "indian red"
        ColorFromString = ColorFromString("#CD5C5C")
    Case "indigo"
        ColorFromString = ColorFromString("#4B0082")
    Case "ivory"
        ColorFromString = ColorFromString("#FFFFF0")
    Case "khaki"
        ColorFromString = ColorFromString("#F0E68C")
    Case "lavender"
        ColorFromString = ColorFromString("#E6E6FA")
    Case "lavenderblush", "lavender blush"
        ColorFromString = ColorFromString("#FFF0F5")
    Case "lawngreen", "lawn green"
        ColorFromString = ColorFromString("#7CFC00")
    Case "lemonchiffon", "lemon chiffon"
        ColorFromString = ColorFromString("#FFFACD")
    Case "lightblue", "light blue"
        ColorFromString = ColorFromString("#ADD8E6")
    Case "lightcoral", "light coral"
        ColorFromString = ColorFromString("#F08080")
    Case "lightcyan", "light cyan"
        ColorFromString = ColorFromString("#E0FFFF")
    Case "lightgoldenrodyellow", "light goldenrod yellow"
        ColorFromString = ColorFromString("#FAFAD2")
    Case "lightgreen", "light green"
        ColorFromString = ColorFromString("#90EE90")
    Case "lightgrey", "light grey"
        ColorFromString = ColorFromString("#D3D3D3")
    Case "lightpink", "light pink"
        ColorFromString = ColorFromString("#FFB6C1")
    Case "lightsalmon", "light salmon"
        ColorFromString = ColorFromString("#FFA07A")
    Case "lightseagreen", "light sea green"
        ColorFromString = ColorFromString("#20B2AA")
    Case "lightskyblue", "light sky blue"
        ColorFromString = ColorFromString("#87CEFA")
    Case "lightslategrey", "light slate grey"
        ColorFromString = ColorFromString("#778899")
    Case "lightsteelblue", "light steel blue"
        ColorFromString = ColorFromString("#B0C4DE")
    Case "lightyellow", "light yellow"
        ColorFromString = ColorFromString("#FFFFE0")
    Case "lime"
        ColorFromString = ColorFromString("#00FF00")
    Case "limegreen", "lime green"
        ColorFromString = ColorFromString("#32CD32")
    Case "linen"
        ColorFromString = ColorFromString("#FAF0E6")
    Case "magenta"
        ColorFromString = ColorFromString("#FF00FF")
    Case "maroon"
        ColorFromString = ColorFromString("#800000")
    Case "mediumaquamarine", "medium aquamarine"
        ColorFromString = ColorFromString("#66CDAA")
    Case "mediumblue", "medium blue"
        ColorFromString = ColorFromString("#0000CD")
    Case "mediumorchid", "medium orchid"
        ColorFromString = ColorFromString("#BA55D3")
    Case "mediumpurple", "medium purple"
        ColorFromString = ColorFromString("#9370DB")
    Case "mediumseagreen", "medium sea green"
        ColorFromString = ColorFromString("#3CB371")
    Case "mediumslateblue", "medium slate blue"
        ColorFromString = ColorFromString("#7B68EE")
    Case "mediumspringgreen", "medium spring green"
        ColorFromString = ColorFromString("#00FA9A")
    Case "mediumturquoise", "medium turquoise"
        ColorFromString = ColorFromString("#48D1CC")
    Case "mediumvioletred", "medium violet-red", "medium violet red"
        ColorFromString = ColorFromString("#C71585")
    Case "midnightblue", "midnight blue"
        ColorFromString = ColorFromString("#191970")
    Case "mintcream", "mint cream"
        ColorFromString = ColorFromString("#F5FFFA")
    Case "mistyrose", "misty rose"
        ColorFromString = ColorFromString("#FFE4E1")
    Case "moccasin"
        ColorFromString = ColorFromString("#FFE4B5")
    Case "navajowhite", "navajo white"
        ColorFromString = ColorFromString("#FFDEAD")
    Case "navy"
        ColorFromString = ColorFromString("#000080")
    Case "oldlace", "old lace"
        ColorFromString = ColorFromString("#FDF5E6")
    Case "olive"
        ColorFromString = ColorFromString("#808000")
    Case "olivedrab", "olive drab", "drab olive"
        ColorFromString = ColorFromString("#6B8E23")
    Case "orange"
        ColorFromString = ColorFromString("#FFA500")
    Case "orangered", "orange-red", "orange red"
        ColorFromString = ColorFromString("#FF4500")
    Case "orchid"
        ColorFromString = ColorFromString("#DA70D6")
    Case "palegoldenrod", "pale goldenrod"
        ColorFromString = ColorFromString("#EEE8AA")
    Case "palegreen", "pale green"
        ColorFromString = ColorFromString("#98FB98")
    Case "paleturquoise", "pale turquoise"
        ColorFromString = ColorFromString("#AFEEEE")
    Case "palevioletred", "pale violet red", "pale violet-red"
        ColorFromString = ColorFromString("#DB7093")
    Case "papayawhip", "papaya whip"
        ColorFromString = ColorFromString("#FFEFD5")
    Case "peachpuff"
        ColorFromString = ColorFromString("#FFDAB9")
    Case "peru"
        ColorFromString = ColorFromString("#CD853F")
    Case "pink"
        ColorFromString = ColorFromString("#FFC0CB")
    Case "plum"
        ColorFromString = ColorFromString("#DDA0DD")
    Case "powderblue", "powder blue"
        ColorFromString = ColorFromString("#B0E0E6")
    Case "purple"
        ColorFromString = ColorFromString("#800080")
    Case "rast"
        ColorFromString = ColorFromString("#B02020")
    Case "red"
        ColorFromString = ColorFromString("#FF0000")
    Case "rosybrown", "rosy brown"
        ColorFromString = ColorFromString("#BC8F8F")
    Case "royalblue", "royal blue"
        ColorFromString = ColorFromString("#4169E1")
    Case "saddlebrown", "saddle brown"
        ColorFromString = ColorFromString("#8B4513")
    Case "salmon"
        ColorFromString = ColorFromString("#FA8072")
    Case "sandybrown", "sandy brown"
        ColorFromString = ColorFromString("#F4A460")
    Case "seagreen", "sea green"
        ColorFromString = ColorFromString("#2E8B57")
    Case "seashell"
        ColorFromString = ColorFromString("#FFF5EE")
    Case "sienna"
        ColorFromString = ColorFromString("#A0522D")
    Case "silver"
        ColorFromString = ColorFromString("#C0C0C0")
    Case "skyblue", "sky blue"
        ColorFromString = ColorFromString("#87CEEB")
    Case "slateblue", "slate blue"
        ColorFromString = ColorFromString("#6A5ACD")
    Case "slategray", "slate gray"
        ColorFromString = ColorFromString("#708090")
    Case "snow"
        ColorFromString = ColorFromString("#FFFAFA")
    Case "springgreen", "spring green"
        ColorFromString = ColorFromString("#00FF7F")
    Case "steelblue", "steel blue"
        ColorFromString = ColorFromString("#4682B4")
    Case "strider"
        ColorFromString = ColorFromString("#D8D8E4")
    Case "tan"
        ColorFromString = ColorFromString("#D2B48C")
    Case "teal"
        ColorFromString = ColorFromString("#008080")
    Case "thistle"
        ColorFromString = ColorFromString("#D8BFD8")
    Case "tomato"
        ColorFromString = ColorFromString("#FF6347")
    Case "turquoise"
        ColorFromString = ColorFromString("#40E0D0")
    Case "violet"
        ColorFromString = ColorFromString("#EE82EE")
    Case "wheat"
        ColorFromString = ColorFromString("#F5DEB3")
    Case "white"
        ColorFromString = ColorFromString("#FFFFFF")
    Case "whitesmoke", "white smoke"
        ColorFromString = ColorFromString("#F5F5F5")
    Case "yellow"
        ColorFromString = ColorFromString("#FFFF00")
    Case "yellowgreen", "yellow-green", "yellow green"
        ColorFromString = ColorFromString("#9ACD32")
    Case "default", "normal", "standard", "text"
        ColorFromString = -1
    Case Else
        ColorFromString = -1
    End Select
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

Public Function CapturePicture(ByVal hdc As Long, ByVal Left As Long, ByVal Top As Long, ByVal Width As Long, ByVal Height As Long) As IPictureDisp
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hdcMem As Long
    Dim hBmp As Long
    Dim hOldBmp As Long
    Dim Pic As IPictureDisp
    Dim rcBitmap As Win32.Rect
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    
    picDesc.Size = Len(picDesc)
    hdcMem = CreateCompatibleDC(hdc)
    If hdcMem = 0 Then
        Exit Function
    End If
    hBmp = CreateCompatibleBitmap(hdc, Width, Height)
    If hBmp = 0 Then
        DeleteDC hdcMem
        Exit Function
    End If
    hOldBmp = SelectObject(hdcMem, hBmp)
    If Left >= 0 And Top >= 0 Then
        If BitBlt(hdcMem, 0, 0, Width, Height, hdc, Left, Top, vbSrcCopy) = 0 Then
            SelectObject hdcMem, hOldBmp
            DeleteDC hdcMem
            DeleteObject hBmp
            Exit Function
        End If
    Else
        With rcBitmap
           .Left = 0
           .Top = 0
           .Right = Width
           .Bottom = Height
        End With
        FillRect hdcMem, rcBitmap, GetStockObject(StockObject_Brush_Black)
    End If
    SelectObject hdcMem, hOldBmp
    picDesc.Bitmap = hBmp
    picDesc.Palette = GetCurrentObject(hdc, Object_Palette)
    picDesc.Type = PicType_Bitmap
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        DeleteDC hdcMem
        DeleteObject hBmp
        Exit Function
    End If
    Set CapturePicture = Pic
    Set Pic = Nothing
    DeleteDC hdcMem
End Function

Public Function CreatePicture(ByVal Width As Long, ByVal Height As Long) As IPictureDisp
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hdcMem As Long
    Dim hBmp As Long
    Dim hOldBmp As Long
    Dim Pic As IPictureDisp
    Dim rcBitmap As Win32.Rect
    Dim deskWnd As Long, deskDC As Long
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    
    picDesc.Size = Len(picDesc)
    hdcMem = CreateMemoryDC
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

Public Function CreateMask(ByVal Width As Long, ByVal Height As Long, ByRef SourcePic As IPictureDisp, ByVal MaskColor As Long) As IPictureDisp
On Error Resume Next
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hdcMem As Long
    Dim hBmp As Long
    Dim hOldBmp As Long
    Dim Pic As IPictureDisp
    Dim rcBitmap As Win32.Rect
    Dim deskWnd As Long, deskDC As Long
    Dim m_lngPixels() As Long
    Dim m_lngX As Long, m_lngY As Long
    
    If SourcePic Is Nothing Then Exit Function
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    
    picDesc.Size = Len(picDesc)
    hdcMem = CreateMemoryDC
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
    m_lngPixels() = GetPictureArrayInv(SourcePic)
    For m_lngY = 0 To Height - 1
        For m_lngX = 0 To Width - 1
            m_lngPixels(m_lngX, m_lngY) = IIf(m_lngPixels(m_lngX, m_lngY) = MaskColor, &HFFFFFF, &H0)
        Next m_lngX
    Next m_lngY
    ArrayToPicture Pic, m_lngPixels, hdcMem
    Set CreateMask = Pic
    Set Pic = Nothing
    DeleteDC hdcMem
    ReleaseDC deskWnd, deskDC
End Function

Public Function CreateColor(ByVal Width As Long, ByVal Height As Long, ByRef SourcePic As IPictureDisp, ByVal MaskColor As Long) As IPictureDisp
On Error Resume Next
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hdcMem As Long
    Dim hBmp As Long
    Dim hOldBmp As Long
    Dim Pic As IPictureDisp
    Dim rcBitmap As Win32.Rect
    Dim deskWnd As Long, deskDC As Long
    Dim m_lngPixels() As Long
    Dim m_lngX As Long, m_lngY As Long
    
    If SourcePic Is Nothing Then Exit Function
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    
    picDesc.Size = Len(picDesc)
    hdcMem = CreateMemoryDC
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
    m_lngPixels() = GetPictureArrayInv(SourcePic)
    For m_lngY = 0 To Height - 1
        For m_lngX = 0 To Width - 1
            m_lngPixels(m_lngX, m_lngY) = IIf(m_lngPixels(m_lngX, m_lngY) = MaskColor, &H0, m_lngPixels(m_lngX, m_lngY))
        Next m_lngX
    Next m_lngY
    ArrayToPicture Pic, m_lngPixels, hdcMem
    Set CreateColor = Pic
    Set Pic = Nothing
    DeleteDC hdcMem
    ReleaseDC deskWnd, deskDC
End Function

Public Function CreatePictureFromIcon(ByVal Width As Long, ByVal Height As Long, ByRef Icon As IPictureDisp) As IPictureDisp
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hdcMem As Long
    Dim hBmp As Long
    Dim hOldBmp As Long
    Dim Pic As IPictureDisp
    Dim rcBitmap As Win32.Rect
    Dim deskWnd As Long, deskDC As Long
    
    deskWnd = GetDesktopWindow
    deskDC = GetDC(deskWnd)
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    
    picDesc.Size = Len(picDesc)
    hdcMem = CreateMemoryDC
    If hdcMem = 0 Then
        Exit Function
    End If
    hBmp = CreateCompatibleBitmap(deskDC, Width, Height)
    If hBmp = 0 Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        Exit Function
    End If
    hOldBmp = SelectObject(hdcMem, hBmp)
    DrawIconEx hdcMem, 0, 0, Icon.Handle, Width, Height, 0, 0, 2
    SelectObject hdcMem, hOldBmp
    picDesc.Bitmap = hBmp
    picDesc.Palette = GetCurrentObject(deskDC, Object_Palette)
    picDesc.Type = PicType_Bitmap
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        DeleteDC hdcMem
        ReleaseDC deskWnd, deskDC
        DeleteObject hBmp
        Exit Function
    End If
    Set CreatePictureFromIcon = Pic
    Set Pic = Nothing
    DeleteDC hdcMem
    ReleaseDC deskWnd, deskDC
End Function

Public Function LoadResIcon(ByVal Resname As String, Optional ByVal Width As Long = 32, Optional ByVal Height As Long = 32) As IPictureDisp
    Dim picGuid As IId
    Dim picDesc As PictureDescriptor
    Dim hdcMem As Long
    Dim hIcon As Long
    Dim hOldBmp As Long
    Dim Pic As IPictureDisp
    Dim rcBitmap As Win32.Rect
    
    ' IID_IPictureDisp
    picGuid.x = &H7BF80981
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
    
    hdcMem = CreateMemoryDC
    
    hIcon = LoadIcon(App.hInstance, Resname)
    
    If hIcon = 0 Then
        DeleteMemoryDC hdcMem
        Exit Function
    End If
    
    picDesc.Size = Len(picDesc)
    picDesc.Bitmap = hIcon
    picDesc.Type = PicType_Icon
    If CreatePictureIndirect(picDesc, picGuid, True, Pic) <> 0 Then
        DeleteMemoryDC hdcMem
        Exit Function
    End If
    Set LoadResIcon = Pic
    Set Pic = Nothing
    DeleteMemoryDC hdcMem
End Function


