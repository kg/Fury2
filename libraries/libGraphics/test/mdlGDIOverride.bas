Attribute VB_Name = "mdlGDIOverride"
Option Explicit
'
'   GDIOverride
'   Creates a SoftFX image that is attached to a DC
'

Private Declare Function LoadImage Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As String, ByVal un1 As Long, ByVal n1 As Long, ByVal n2 As Long, ByVal un2 As Long) As Long
Private Const LR_DEFAULTCOLOR = &H0
Private Const LR_MONOCHROME = &H1
Private Const LR_COLOR = &H2
Private Const LR_COPYRETURNORG = &H4
Private Const LR_COPYDELETEORG = &H8
Private Const LR_LOADFROMFILE = &H10
Private Const LR_LOADTRANSPARENT = &H20
Private Const LR_DEFAULTSIZE = &H40
Private Const LR_LOADMAP3DCOLORS = &H1000
Private Const LR_CREATEDIBHeader = &H2000
Private Const LR_COPYFROMRESOURCE = &H4000
Private Const LR_SHARED = &H8000

Private m_lngAllocationBitmap As Long

Private Const IdentifierTag As Long = &H474449 ' GDI
Private Const IdentifierTagIndex As Long = 7

Private Function SFXColorToGDI(ByVal Color As Long)
On Error Resume Next
    SFXColorToGDI = RGB(SoftFX.GetRed(Color), SoftFX.GetGreen(Color), SoftFX.GetBlue(Color))
End Function

Private Function BFToLong(ByRef BlendFunc As BlendFunction) As Long
On Error Resume Next
Dim l_lngValue As Long
    CopyMemory l_lngValue, BlendFunc, 4
    BFToLong = l_lngValue
End Function

Private Function o_SetPixel(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngDC As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        If GetImageLocked(Parameters.Image) Then
            o_SetPixel = 1
            l_lngDC = GetImageTag(Parameters.Image, 1)
            SetPixelV l_lngDC, Parameters.P2, Parameters.P3, SFXColorToGDI(Parameters.P4)
        Else
        End If
    End If
End Function

Private Function o_Line(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngDC As Long
Dim l_lngBrush As Long, l_lngPen As Long
Dim l_rctParameter As Rectangle, l_lngROP As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        If GetImageLocked(Parameters.Image) Then
            o_Line = 1
            l_lngDC = GetImageTag(Parameters.Image, 1)
            DerefRectangle Parameters.Rectangle, l_rctParameter
            l_lngPen = CreatePen(0, 1, SFXColorToGDI(Parameters.P3))
            l_lngROP = SetROP2(l_lngDC, ROP_Copy)
            SelectObject l_lngDC, l_lngPen
            MoveToEx l_lngDC, l_rctParameter.Left, l_rctParameter.Top, ByVal 0
            LineTo l_lngDC, l_rctParameter.Left + l_rctParameter.Width, l_rctParameter.Top + l_rctParameter.Height
            SetROP2 l_lngDC, l_lngROP
            DeleteObject l_lngPen
        Else
        End If
    End If
End Function

Private Function o_Fill(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngDC As Long
Dim l_rctArea As Rect, l_lngBrush As Long
Dim l_rctParameter As Rectangle, l_lngROP As Long
Dim l_lngRegion As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        If GetImageLocked(Parameters.Image) Then
            o_Fill = 1
            l_lngDC = GetImageTag(Parameters.Image, 1)
            DerefRectangle Parameters.Rectangle, l_rctParameter
            With l_rctArea
                .Left = l_rctParameter.Left
                .Top = l_rctParameter.Top
                .Right = .Left + l_rctParameter.Width
                .Bottom = .Top + l_rctParameter.Height
            End With
            l_lngRegion = CreateRectRegion(l_rctArea)
            l_lngBrush = CreateSolidBrush(SFXColorToGDI(Parameters.P3))
            l_lngROP = SetROP2(l_lngDC, ROP_Copy)
            FillRegion l_lngDC, l_lngRegion, l_lngBrush
            SetROP2 l_lngDC, l_lngROP
            DeleteObject l_lngRegion
            DeleteObject l_lngBrush
        Else
        End If
    End If
End Function

Private Function o_Fill_Channel(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngDC As Long
Dim l_rctArea As Rect, l_lngBrush As Long
Dim l_rctParameter As Rectangle, l_lngROP As Long
Dim l_lngRegion As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        If GetImageLocked(Parameters.Image) Then
            o_Fill_Channel = 1
            l_lngDC = GetImageTag(Parameters.Image, 1)
            DerefRectangle Parameters.Rectangle, l_rctParameter
            With l_rctArea
                .Left = l_rctParameter.Left
                .Top = l_rctParameter.Top
                .Right = .Left + l_rctParameter.Width
                .Bottom = .Top + l_rctParameter.Height
            End With
            l_lngRegion = CreateRectRegion(l_rctArea)
            l_lngROP = SetROP2(l_lngDC, ROP_AND)
            l_lngBrush = CreateSolidBrush(RGB(IIf(Parameters.P3 = Red, 0, 255), IIf(Parameters.P3 = Green, 0, 255), IIf(Parameters.P3 = Blue, 0, 255)))
            FillRegion l_lngDC, l_lngRegion, l_lngBrush
            DeleteObject l_lngBrush
            SetROP2 l_lngDC, ROP_OR
            l_lngBrush = CreateSolidBrush(RGB(IIf(Parameters.P3 = Red, 255, 0), IIf(Parameters.P3 = Green, 255, 0), IIf(Parameters.P3 = Blue, 255, 0)))
            FillRegion l_lngDC, l_lngRegion, l_lngBrush
            DeleteObject l_lngBrush
            SetROP2 l_lngDC, l_lngROP
            DeleteObject l_lngRegion
            DeleteObject l_lngBrush
        Else
        End If
    End If
End Function

Private Function o_Invert(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngDC As Long
Dim l_rctArea As Rect, l_lngBrush As Long
Dim l_rctParameter As Rectangle, l_lngROP As Long
Dim l_lngRegion As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        If GetImageLocked(Parameters.Image) Then
            o_Invert = 1
            l_lngDC = GetImageTag(Parameters.Image, 1)
            DerefRectangle Parameters.Rectangle, l_rctParameter
            With l_rctArea
                .Left = l_rctParameter.Left
                .Top = l_rctParameter.Top
                .Right = .Left + l_rctParameter.Width
                .Bottom = .Top + l_rctParameter.Height
            End With
            If Parameters.Header.ParameterCount = 3 Then
                ' Invert_Channel
                l_lngBrush = CreateSolidBrush(RGB(IIf(Parameters.P3 = Red, 255, 0), IIf(Parameters.P3 = Green, 255, 0), IIf(Parameters.P3 = Blue, 255, 0)))
                l_lngROP = SetROP2(l_lngDC, ROP_XOR)
            ElseIf Parameters.Header.ParameterCount = 2 Then
                ' Invert or Invert_Color. Doesn't matter which; GDI doesn't care about alpha
                l_lngBrush = CreateSolidBrush(RGB(255, 255, 255))
                l_lngROP = SetROP2(l_lngDC, ROP_NOT)
            End If
            l_lngRegion = CreateRectRegion(l_rctArea)
            FillRegion l_lngDC, l_lngRegion, l_lngBrush
            SetROP2 l_lngDC, l_lngROP
            DeleteObject l_lngRegion
            DeleteObject l_lngBrush
        Else
        End If
    End If
End Function

Private Function o_Unlock(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long, l_lngHandle As Long
Dim l_bufImage As clsGDIPixelBuffer
Dim l_lngTemp As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        Debug.Print "Unlock(" & Parameters.Image & ")"
        o_Unlock = 1
        l_lngHandle = GetImageTag(Parameters.Image, 3)
        If l_lngHandle <> 0 Then
        Else
            Set l_bufImage = New clsGDIPixelBuffer
            If l_bufImage Is Nothing Then
            Else
                l_bufImage.Attach GetImageTag(Parameters.Image, 2)
            End If
            SetImageTag Parameters.Image, 3, ObjPtr(l_bufImage)
            SetImagePointer Parameters.Image, l_bufImage.Pointer
            SetImageLocked Parameters.Image, False
            CopyMemory ByVal VarPtr(l_bufImage), ByVal VarPtr(l_lngTemp), 4 ' Disassociate the object handle (dangerous, but hey what the hell)
        End If
    End If
End Function

Private Function o_Lock(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long, l_lngHandle As Long
Dim l_bufImage As clsGDIPixelBuffer
Dim l_lngTemp As Long
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        Debug.Print "Lock(" & Parameters.Image & ")"
        o_Lock = 1
        l_lngHandle = GetImageTag(Parameters.Image, 3)
        If l_lngHandle <> 0 Then
            CopyMemory ByVal VarPtr(l_bufImage), ByVal VarPtr(l_lngHandle), 4
            If l_bufImage Is Nothing Then
            Else
                l_bufImage.Detach
            End If
            SetImageTag Parameters.Image, 3, 0
            SetImagePointer Parameters.Image, 0
            SetImageLocked Parameters.Image, True
        Else
        End If
    End If
End Function

Private Function o_Allocate(ByRef Parameters As Image_Allocate_Parameters) As Long
On Error Resume Next
Dim l_lngDC As Long, l_lngBitmap As Long
Dim l_lngDesktopDC As Long, l_lngDesktopWindow As Long
    ' Allocate overrides always consume the operation
    o_Allocate = 1
    If (Parameters.Width < 1) Then Exit Function
    If (Parameters.Height < 1) Then Exit Function
    If (Parameters.Image = 0) Then Exit Function
    l_lngDesktopWindow = GetDesktopWindow()
    l_lngDesktopDC = GetDC(l_lngDesktopWindow)
    l_lngDC = CreateCompatibleDC(l_lngDesktopDC)
    ReleaseDC l_lngDesktopWindow, l_lngDesktopDC
    If m_lngAllocationBitmap = 0 Then
        l_lngBitmap = CreateCompatibleBitmap(l_lngDC, Parameters.Width, Parameters.Height)
    Else
        l_lngBitmap = m_lngAllocationBitmap
    End If
    SelectObject l_lngDC, l_lngBitmap
    SetImageTag Parameters.Image, 1, l_lngDC
    SetImageTag Parameters.Image, 2, l_lngBitmap
    SetImageTag Parameters.Image, IdentifierTagIndex, IdentifierTag
    If m_lngAllocationBitmap = 0 Then
        SetImageWidth Parameters.Image, Parameters.Width
        SetImageHeight Parameters.Image, Parameters.Height
    Else
        SetImageWidth Parameters.Image, GetBitmapWidth(m_lngAllocationBitmap)
        SetImageHeight Parameters.Image, GetBitmapHeight(m_lngAllocationBitmap)
    End If
    SetImagePitch Parameters.Image, 0
    SetImagePointer Parameters.Image, 0
    SetImageLocked Parameters.Image, True
    Debug.Print "Allocate(" & Parameters.Image & ")"
End Function

Private Function o_Deallocate(ByRef Parameters As Image_Allocate_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngDC As Long, l_lngBitmap As Long, l_lngHandle As Long
Dim l_bufTemp As clsGDIPixelBuffer
    o_Deallocate = 0
    If (Parameters.Image = 0) Then Exit Function
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        Debug.Print "Deallocate(" & Parameters.Image & ")"
        l_lngDC = GetImageTag(Parameters.Image, 1)
        l_lngBitmap = GetImageTag(Parameters.Image, 2)
        If l_lngBitmap <> 0 Then DeleteObject l_lngBitmap
        If l_lngDC <> 0 Then DeleteObject l_lngDC
        l_lngHandle = GetImageTag(Parameters.Image, 3)
        If l_lngHandle <> 0 Then
            CopyMemory l_bufTemp, l_lngHandle, 4
            If l_bufTemp Is Nothing Then
            Else
                l_bufTemp.Detach
            End If
        End If
        SetImageWidth Parameters.Image, 0
        SetImageHeight Parameters.Image, 0
        ClearImageTags Parameters.Image
        o_Deallocate = 1
    End If
End Function

Private Function o_ResampleBlit(ByRef Parameters As BlitResample_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngSourceDC As Long, l_lngDestDC As Long
Dim l_rctDest As Rectangle, l_rctSource As Rectangle
Dim l_lngROP As Long
    If (Parameters.SourceImage = 0) Then Exit Function
    If (Parameters.DestImage = 0) Then Exit Function
    l_lngTag = GetImageTag(Parameters.SourceImage, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        With Parameters.Header
            If KeyMatches(.KeyPointer, "BlitResample_Normal") Then
                l_lngROP = vbSrcCopy
            Else
                l_lngROP = 0
            End If
        End With
        l_lngSourceDC = GetImageTag(Parameters.SourceImage, 1)
        DerefRectangle Parameters.DestRectangle, l_rctDest
        DerefRectangle Parameters.SourceRectangle, l_rctSource
        If l_lngROP <> 0 Then
            If GetImageTag(Parameters.DestImage, IdentifierTagIndex) = IdentifierTag Then
                o_ResampleBlit = 1
                ' GDI to GDI
                l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
                SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
                With l_rctDest
                    StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
                End With
            ElseIf GetImageDIBHandle(Parameters.DestImage) <> 0 Then
                o_ResampleBlit = 1
                ' GDI to DIBSection
                l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
                If l_lngDestDC Then
                    SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
                    With l_rctDest
                        StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
                    End With
                End If
            End If
        End If
        If GetImageLocked(Parameters.SourceImage) Then
            o_ResampleBlit = 1
        End If
    End If
End Function

Private Function o_ResampleBlit_Opacity(ByRef Parameters As BlitResample_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngSourceDC As Long, l_lngDestDC As Long
Dim l_rctDest As Rectangle, l_rctSource As Rectangle
Dim l_bfBlend As BlendFunction
    If (Parameters.SourceImage = 0) Then Exit Function
    If (Parameters.DestImage = 0) Then Exit Function
    l_lngTag = GetImageTag(Parameters.SourceImage, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        With l_bfBlend
            .SourceConstantAlpha = Parameters.P6
            .BlendOperation = AlphaBlend_Source_Over
        End With
        l_lngSourceDC = GetImageTag(Parameters.SourceImage, 1)
        DerefRectangle Parameters.DestRectangle, l_rctDest
        DerefRectangle Parameters.SourceRectangle, l_rctSource
        If GetImageTag(Parameters.DestImage, IdentifierTagIndex) = IdentifierTag Then
            o_ResampleBlit_Opacity = 1
            ' GDI to GDI
            l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
            SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
            With l_rctDest
                'StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
                AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, BFToLong(l_bfBlend)
            End With
        ElseIf GetImageDIBHandle(Parameters.DestImage) <> 0 Then
            o_ResampleBlit_Opacity = 1
            ' GDI to DIBSection
            l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
            If l_lngDestDC Then
                SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
                With l_rctDest
                    'StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
                    AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, BFToLong(l_bfBlend)
                End With
            End If
        End If
        If GetImageLocked(Parameters.SourceImage) Then
            o_ResampleBlit_Opacity = 1
        End If
    End If
End Function

Private Function o_Blit(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngSourceDC As Long, l_lngDestDC As Long
Dim l_rctDest As Rectangle, l_lngROP As Long
    If (Parameters.Source = 0) Then Exit Function
    If (Parameters.Dest = 0) Then Exit Function
    l_lngTag = GetImageTag(Parameters.Source, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        With Parameters.Header
            If KeyMatches(.KeyPointer, "BlitSimple_Normal") Then
                l_lngROP = vbSrcCopy
            ElseIf KeyMatches(.KeyPointer, "BlitSimple_AND") Then
                l_lngROP = vbSrcAnd
            ElseIf KeyMatches(.KeyPointer, "BlitSimple_OR") Then
                l_lngROP = vbSrcPaint
            ElseIf KeyMatches(.KeyPointer, "BlitSimple_XOR") Then
                l_lngROP = vbSrcInvert
            Else
                l_lngROP = 0
            End If
        End With
        l_lngSourceDC = GetImageTag(Parameters.Source, 1)
        DerefRectangle Parameters.Rectangle, l_rctDest
        If l_lngROP <> 0 Then
            If GetImageTag(Parameters.Dest, IdentifierTagIndex) = IdentifierTag Then
                o_Blit = 1
                ' GDI to GDI
                l_lngDestDC = GetImageTag(Parameters.Dest, 1)
                With l_rctDest
                    BitBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, l_lngROP
                End With
            ElseIf GetImageDIBHandle(Parameters.Dest) <> 0 Then
                o_Blit = 1
                ' GDI to DIBSection
                l_lngDestDC = GetImageTag(Parameters.Dest, 1)
                If l_lngDestDC Then
                    With l_rctDest
                        BitBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, l_lngROP
                    End With
                End If
            End If
        End If
        If GetImageLocked(Parameters.Source) Then
            o_Blit = 1
        End If
    End If
End Function

Private Function o_Blit_Opacity(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_lngSourceDC As Long, l_lngDestDC As Long
Dim l_rctDest As Rectangle
Dim l_bfBlend As BlendFunction
    If (Parameters.Source = 0) Then Exit Function
    If (Parameters.Dest = 0) Then Exit Function
    l_lngTag = GetImageTag(Parameters.Source, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        With l_bfBlend
            .SourceConstantAlpha = Parameters.P6
            .BlendOperation = AlphaBlend_Source_Over
        End With
        l_lngSourceDC = GetImageTag(Parameters.Source, 1)
        DerefRectangle Parameters.Rectangle, l_rctDest
        If GetImageTag(Parameters.Dest, IdentifierTagIndex) = IdentifierTag Then
            o_Blit_Opacity = 1
            ' GDI to GDI
            l_lngDestDC = GetImageTag(Parameters.Dest, 1)
            With l_rctDest
                'BitBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, l_lngROP
                AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, .Width, .Height, BFToLong(l_bfBlend)
            End With
        ElseIf GetImageDIBHandle(Parameters.Dest) <> 0 Then
            o_Blit_Opacity = 1
            ' GDI to DIBSection
            l_lngDestDC = GetImageTag(Parameters.Dest, 1)
            If l_lngDestDC Then
                With l_rctDest
                    'BitBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, l_lngROP
                    AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, .Width, .Height, BFToLong(l_bfBlend)
                End With
            End If
        End If
        If GetImageLocked(Parameters.Source) Then
            o_Blit_Opacity = 1
        End If
    End If
End Function

Public Sub InitGDIOverride()
On Error Resume Next
    AddOverride "Deallocate(image)", AddressOf o_Deallocate
    AddOverride "Lock(image)", AddressOf o_Lock
    AddOverride "Unlock(image)", AddressOf o_Unlock
    AddOverride "BlitSimple_Normal", AddressOf o_Blit
    AddOverride "BlitSimple_AND", AddressOf o_Blit
    AddOverride "BlitSimple_OR", AddressOf o_Blit
    AddOverride "BlitSimple_XOR", AddressOf o_Blit
    AddOverride "BlitSimple_Normal_Opacity", AddressOf o_Blit_Opacity
    AddOverride "BlitResample_Normal", AddressOf o_ResampleBlit
    AddOverride "BlitResample_Normal_Opacity", AddressOf o_ResampleBlit_Opacity
    AddOverride "FilterSimple_Invert", AddressOf o_Invert
    AddOverride "FilterSimple_Invert_Color", AddressOf o_Invert
    AddOverride "FilterSimple_Invert_Channel", AddressOf o_Invert
    AddOverride "FilterSimple_Fill_Channel", AddressOf o_Fill_Channel
    AddOverride "FilterSimple_Fill", AddressOf o_Fill
    AddOverride "FilterSimple_Line", AddressOf o_Line
    AddOverride "SetPixel(image, x, y, value)", AddressOf o_SetPixel
End Sub

Public Function CreateGDIImage(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    ' Install the allocator
    AddOverride "Allocate(image, width, height)", AddressOf o_Allocate
    m_lngAllocationBitmap = 0
    Set CreateGDIImage = F2Image(Width, Height)
    m_lngAllocationBitmap = 0
    ' Uninstall the allocator
    RemoveOverride "Allocate(image, width, height)", AddressOf o_Allocate
End Function

Public Function CreateGDIImageFromFile(ByVal Filename As String) As Fury2Image
On Error Resume Next
    ' Install the allocator
    AddOverride "Allocate(image, width, height)", AddressOf o_Allocate
    m_lngAllocationBitmap = 0
    m_lngAllocationBitmap = LoadImage(0, Filename, Image_Bitmap, 0, 0, LR_LOADFROMFILE Or LR_DEFAULTSIZE Or LR_DEFAULTCOLOR)
    If m_lngAllocationBitmap = 0 Then
    Else
        Set CreateGDIImageFromFile = F2Image(1, 1) ' Size will be overridden by the allocator on success
    End If
    m_lngAllocationBitmap = 0
    ' Uninstall the allocator
    RemoveOverride "Allocate(image, width, height)", AddressOf o_Allocate
End Function

Public Function IsImageGDI(ByRef Image As Fury2Image) As Boolean
On Error Resume Next
    IsImageGDI = (GetImageTag(Image.Handle, IdentifierTagIndex) = IdentifierTag)
End Function
