VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Image"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements IVirtualFileSaveable

Public Dirty As Boolean
Public Tag As Variant

Private m_rctMe As Fury2Rect
Private m_rctClip As Fury2Rect
Private m_rcClipStack() As Rectangle
Private m_booNoDelete As Boolean
Private m_strName As String
Private m_booAlphaChannel As Boolean
Private m_lngImage As Long
Private m_bmdBlitMode As SFXBlitModes
Private m_smdResampleMode As SFXResampleModes
Private m_rmdRenderMode As SFXRenderModes

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Image: " & Replace(Name, vbCrLf, "") & " (" & Width & "x" & Height & ")"
End Function

Public Function Clip(ByVal Rectangle As Fury2Rect) As Boolean
Dim l_rctRect As Fury2Rect
Dim l_rcRectangle As Rectangle
    Set l_rctRect = Rectangle
    l_rcRectangle = l_rctRect.GetRectangle
    Clip = CBool(SoftFX.ClipRectangle_ImageClipRect(l_rcRectangle, m_lngImage))
    l_rctRect.SetRectangle l_rcRectangle
    If Err <> 0 Then
        Err.Clear
        Exit Function
    End If
End Function

Public Function ClipRect(ByVal Rectangle As Fury2Rect) As Boolean
Attribute ClipRect.VB_MemberFlags = "40"
    ClipRect = Clip(Rectangle)
End Function

Public Function Compare(Optional ByVal DestRectangle As Fury2Rect = Nothing, Optional ByVal SourceRectangle As Fury2Rect = Nothing, Optional ByVal Source As Fury2Image = Nothing) As Long
On Error Resume Next
Dim l_rctDest As Fury2Rect, l_rctSource As Fury2Rect
Dim l_imgSource As Fury2Image
    Set l_rctDest = DestRectangle
    Set l_rctSource = SourceRectangle
    Set l_imgSource = Source
    If Err <> 0 Then
        Err.Clear
        Exit Function
    End If
    If l_imgSource Is Nothing Then Exit Function
    If l_rctDest Is Nothing Then Set l_rctDest = Me.Rectangle
    If l_rctSource Is Nothing Then Set l_rctSource = l_imgSource.Rectangle
    Compare = SoftFX.Blit_Compare(m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_rctSource.Left, l_rctSource.Top)
End Function

Public Sub Blur(ByVal XRadius As Long, ByVal YRadius As Long, Optional ByVal Rectangle As Fury2Rect = Nothing)
On Error Resume Next
    If Rectangle Is Nothing Then Set Rectangle = Me.Rectangle
    SoftFX.Filter_Blur m_lngImage, Rectangle.GetRectangle, XRadius, YRadius
End Sub

Public Function Draw(ByVal Surface As Fury2Image, ByVal X As Single, ByVal Y As Single, Optional ByVal Alpha As Single = 1, Optional ByVal ScalePercentage As Single = 1, Optional ByVal RotationAngle As Single = 0, Optional ByVal Mode As SFXBlitModes = BlitMode_Default, Optional ByVal Color As Long = &HFFFFFFFF, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default, Optional ByVal AntiAlias As Boolean = False) As Fury2Rect
On Error Resume Next
Dim l_sngX As Single, l_sngY As Single
Dim l_sngAlpha As Single, l_sngScale As Single, l_sngAngle As Single
Dim l_imgSurface As Fury2Image, l_rctRect As Fury2Rect
Dim l_lngMode As Long, l_arrPoints As Variant
    Set l_imgSurface = Surface
    l_sngX = CSng(X)
    l_sngY = CSng(Y)
    l_sngAlpha = CSng(Alpha)
    l_sngScale = CSng(ScalePercentage)
    l_sngAngle = CSng(RotationAngle)
    If (Abs(l_sngAngle) < 0.1) And (AntiAlias = False) Then
        If l_sngScale = 1 Then
            Set l_rctRect = F2Rect(Floor(X - (Width / 2)), Floor(Y - (Height / 2)), Width, Height, False)
        Else
            Set l_rctRect = F2Rect(Floor(X - ((Width * l_sngScale) / 2)), Floor(Y - ((Height * l_sngScale) / 2)), Ceil(X + ((Width * l_sngScale) / 2)), Ceil(Y + ((Height * l_sngScale) / 2)), True)
        End If
        Set Draw = l_rctRect
        l_imgSurface.Blit l_rctRect, Rectangle, Me, l_sngAlpha, Mode, Color, ResampleMode
    Else
        Dim l_sngHalfX As Single, l_sngHalfY As Single
        l_sngHalfX = Me.Width / 2
        l_sngHalfY = Me.Height / 2
        ReDim l_arrPoints(0 To 3)
        l_arrPoints(0) = RotatePoint(-l_sngHalfX, -l_sngHalfY, l_sngAngle)
        l_arrPoints(1) = RotatePoint(l_sngHalfX, -l_sngHalfY, l_sngAngle)
        l_arrPoints(2) = RotatePoint(l_sngHalfX, l_sngHalfY, l_sngAngle)
        l_arrPoints(3) = RotatePoint(-l_sngHalfX, l_sngHalfY, l_sngAngle)
        
        l_arrPoints(0)(0) = (l_arrPoints(0)(0) * l_sngScale) + l_sngX
        l_arrPoints(0)(1) = (l_arrPoints(0)(1) * l_sngScale) + l_sngY
        l_arrPoints(1)(0) = (l_arrPoints(1)(0) * l_sngScale) + l_sngX
        l_arrPoints(1)(1) = (l_arrPoints(1)(1) * l_sngScale) + l_sngY
        l_arrPoints(2)(0) = (l_arrPoints(2)(0) * l_sngScale) + l_sngX
        l_arrPoints(2)(1) = (l_arrPoints(2)(1) * l_sngScale) + l_sngY
        l_arrPoints(3)(0) = (l_arrPoints(3)(0) * l_sngScale) + l_sngX
        l_arrPoints(3)(1) = (l_arrPoints(3)(1) * l_sngScale) + l_sngY
        If Mode = BlitMode_Default Then Mode = m_bmdBlitMode
        Select Case Mode
        Case BlitMode_SourceAlpha
            l_lngMode = SFXRenderModes.RenderMode_SourceAlpha
        Case BlitMode_Additive
            l_lngMode = SFXRenderModes.RenderMode_Additive
        Case BlitMode_Subtractive
            l_lngMode = SFXRenderModes.RenderMode_Subtractive
        Case BlitMode_Screen
            l_lngMode = SFXRenderModes.RenderMode_Screen
        Case BlitMode_Merge
            l_lngMode = SFXRenderModes.RenderMode_Merge
        Case Else
            l_lngMode = SFXRenderModes.RenderMode_Normal
        End Select
        If AntiAlias Then
            l_imgSurface.AntiAliasTexturedConvexPolygon l_arrPoints, Me, l_lngMode, ResampleMode
        Else
            l_imgSurface.TexturedConvexPolygon l_arrPoints, Me, l_lngMode, ResampleMode
        End If
    End If
End Function

Public Function GetPixel(ByVal X As Long, ByVal Y As Long) As Long
On Error Resume Next
    If Initialized Then GetPixel = SoftFX.GetPixel(m_lngImage, X, Y)
End Function

Public Function GetPixelAA(ByVal X As Single, ByVal Y As Single) As Long
On Error Resume Next
    If Initialized Then GetPixelAA = SoftFX.GetPixelAA(m_lngImage, X, Y)
End Function

Private Function GetReferenceCount(ByVal Obj As Object) As Long
On Error Resume Next
Dim m_objObj As IShellFolderEx_TLB.IUnknown
    Set m_objObj = Obj
    If Err <> 0 Then Exit Function
    m_objObj.AddRef
    GetReferenceCount = m_objObj.Release
End Function

Friend Function GetRenderer(ByVal RenderMode As SFXRenderModes) As Long
On Error Resume Next
    Select Case RenderMode
    Case SFXRenderModes.RenderMode_SourceAlpha
        GetRenderer = GetSourceAlphaRenderer()
    Case SFXRenderModes.RenderMode_Additive
        GetRenderer = GetAdditiveRenderer()
    Case SFXRenderModes.RenderMode_Subtractive
        GetRenderer = GetSubtractiveRenderer()
    Case SFXRenderModes.RenderMode_Merge
        GetRenderer = GetMergeRenderer()
    Case SFXRenderModes.RenderMode_Screen
        GetRenderer = GetScreenRenderer()
    Case SFXRenderModes.RenderMode_Shadow
        GetRenderer = GetShadowRenderer()
    Case SFXRenderModes.RenderMode_Additive_SourceAlpha
        GetRenderer = GetAdditiveSourceAlphaRenderer()
    Case SFXRenderModes.RenderMode_Subtractive_SourceAlpha
        GetRenderer = GetSubtractiveSourceAlphaRenderer()
    Case SFXRenderModes.RenderMode_Font_SourceAlpha
        GetRenderer = GetFontSourceAlphaRenderer()
    Case SFXRenderModes.RenderMode_Unerase
        GetRenderer = GetUneraseRenderer()
    Case SFXRenderModes.RenderMode_Erase
        GetRenderer = GetEraseRenderer()
    Case SFXRenderModes.RenderMode_SourceAlpha_Premultiplied
        GetRenderer = GetPremultipliedSourceAlphaRenderer()
    End Select
End Function

Friend Function GetScaler(ByVal ResampleMode As SFXResampleModes) As Long
On Error Resume Next
    Select Case ResampleMode
    Case SFXResampleModes.ResampleMode_Bilinear, SFXResampleModes.ResampleMode_Bilinear_High_Quality
        GetScaler = GetBilinearScaler
    Case SFXResampleModes.ResampleMode_Bilinear_Wrap
        GetScaler = GetBilinearWrapScaler
    Case SFXResampleModes.ResampleMode_Linear_Wrap
        GetScaler = GetLinearWrapScaler
    Case SFXResampleModes.ResampleMode_Bilinear_Clamp
        GetScaler = GetBilinearClampScaler
    Case SFXResampleModes.ResampleMode_Linear_Clamp
        GetScaler = GetLinearClampScaler
    Case SFXResampleModes.ResampleMode_Linear
        GetScaler = GetLinearScaler
    Case Else
    End Select
End Function

Public Function PopClipRectangle() As Boolean
On Error Resume Next
    If UBound(m_rcClipStack) > 0 Then
        SoftFX.SetImageClipRectangle m_lngImage, m_rcClipStack(UBound(m_rcClipStack))
        ReDim Preserve m_rcClipStack(0 To UBound(m_rcClipStack) - 1)
        PopClipRectangle = True
    End If
End Function

Public Function Resample(ByVal Width As Long, ByVal Height As Long, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default) As Fury2Image
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_sngXRatio As Single, l_sngYRatio As Single
Dim l_imgImage As Fury2Image, l_imgTemp As Fury2Image
    If Not Initialized Then Exit Function
    If VarType(Width) = vbSingle Or VarType(Height) = vbSingle Then
        l_lngWidth = Me.Width * CSng(Width)
        l_lngHeight = Me.Height * CSng(Height)
    Else
        l_lngWidth = CLng(Width)
        l_lngHeight = CLng(Height)
    End If
    If ResampleMode = ResampleMode_Default Then ResampleMode = m_smdResampleMode
    If Err <> 0 Then Exit Function
    Set l_imgImage = New Fury2Image
    If ResampleMode = ResampleMode_Bilinear_High_Quality Then
        Set l_imgTemp = Me
        ResampleMode = ResampleMode_Bilinear
        l_sngXRatio = Width / Me.Width
        l_sngYRatio = Height / Me.Height
        If (l_sngXRatio) < 0.5 Or (l_sngYRatio < 0.5) Then
            Do Until (l_sngXRatio >= 0.5) And (l_sngYRatio >= 0.5)
                Call ResampleImage(l_imgImage.Handle, l_imgTemp.Handle, l_imgTemp.Width / 2, l_imgTemp.Height / 2, ResampleMode_Bilinear)
                l_sngXRatio = l_sngXRatio * 2
                l_sngYRatio = l_sngYRatio * 2
                Set l_imgTemp = l_imgImage
                Set l_imgImage = New Fury2Image
            Loop
            Call ResampleImage(l_imgImage.Handle, l_imgTemp.Handle, l_lngWidth, l_lngHeight, ResampleMode_Bilinear)
        Else
            Call ResampleImage(l_imgImage.Handle, Handle, l_lngWidth, l_lngHeight, ResampleMode)
        End If
    Else
        Call ResampleImage(l_imgImage.Handle, Handle, l_lngWidth, l_lngHeight, ResampleMode)
    End If
    Set Resample = l_imgImage
End Function

Private Function RotatePoint(X As Single, Y As Single, Angle As Single) As Variant
On Error Resume Next
Dim l_sngX As Single, l_sngY As Single
Dim l_sngTheta As Single, l_sngR As Single
    l_sngR = Sqr((X * X) + (Y * Y))
    If (X > 0) Then
        l_sngTheta = Atn(Y / X)
    ElseIf (X < 0) Then
        l_sngTheta = Atn(Y / X) + c_dblPi
    Else
        l_sngTheta = c_dblPi / 2
    End If
    l_sngTheta = l_sngTheta + CSng(Angle * c_dblRadian)
    l_sngX = l_sngR * Cos(l_sngTheta)
    l_sngY = l_sngR * Sin(l_sngTheta)
    RotatePoint = Array(Round(l_sngX * 1000) / 1000, Round(l_sngY * 1000) / 1000)
End Function

Public Function SavePNG(ByVal Filename As String) As Boolean
On Error Resume Next
Dim l_strFilename As String
    If Not Initialized Then Exit Function
    Err.Clear
    l_strFilename = CStr(Filename)
    If Err <> 0 Then Exit Function
    SavePNG = (SoftFX.SaveImageToPNG(m_lngImage, l_strFilename) = 1)
    Err.Clear
End Function

Public Function SaveTGA(ByVal Filename As String) As Boolean
On Error Resume Next
Dim l_strFilename As String
    If Not Initialized Then Exit Function
    Err.Clear
    l_strFilename = CStr(Filename)
    If Err <> 0 Then Exit Function
    SaveTGA = (SoftFX.SaveImageToTGA(m_lngImage, l_strFilename) = 1)
    Err.Clear
End Function

Public Function Split(ByVal Width As Long, ByVal Height As Long) As Fury2Image()
On Error Resume Next
Dim l_imgImages() As Fury2Image
Dim l_lngRowSize As Long, l_lngColSize As Long
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngX As Long, l_lngY As Long
    l_lngWidth = CLng(Width)
    l_lngHeight = CLng(Height)
    If Err <> 0 Then Exit Function
    l_lngRowSize = Ceil(Me.Width / l_lngWidth)
    l_lngColSize = Ceil(Me.Height / l_lngHeight)
    ReDim l_imgImages(0 To (l_lngRowSize * l_lngColSize) - 1)
    For l_lngY = 0 To l_lngColSize - 1
        For l_lngX = 0 To l_lngRowSize - 1
            Set l_imgImages(l_lngX + (l_lngY * l_lngRowSize)) = Me.Subimage(F2Rect(l_lngX * l_lngWidth, l_lngY * l_lngHeight, l_lngWidth, l_lngHeight, False))
        Next l_lngX
    Next l_lngY
    Split = l_imgImages
End Function

Public Function Subimage(ByVal Rectangle As Fury2Rect) As Fury2Image
On Error Resume Next
Dim l_rctRect As Fury2Rect
    Set l_rctRect = Rectangle
    If Err <> 0 Then
        Err.Clear
        Exit Function
    End If
    Set Subimage = F2Image(l_rctRect.Width, l_rctRect.Height)
    Subimage.AlphaChannel = Me.AlphaChannel
    SoftFX.Blit_Normal Subimage.Handle, m_lngImage, Subimage.Rectangle.GetRectangle, l_rctRect.Left, l_rctRect.Top
    Subimage.Optimize
End Function

Public Property Get AlphaChannel() As Boolean
    AlphaChannel = m_booAlphaChannel
End Property

Public Property Get BlitMode() As SFXBlitModes
    BlitMode = m_bmdBlitMode
End Property

Public Property Get ClipRectangle() As Fury2Rect
On Error Resume Next
Dim l_rctRect As Rectangle
Dim l_lngRefCount As Long
    SoftFX.GetImageClipRectangle m_lngImage, l_rctRect
    If m_rctClip Is Nothing Then
        Set m_rctClip = F2RectFromStruct(l_rctRect)
    Else
        ' If we already have a rectangle object for ourselves, see if someone is holding a reference to it
        l_lngRefCount = GetReferenceCount(m_rctClip)
        If (l_lngRefCount > 4) Then
            ' Someone's got a reference, so we make ourselves a new copy instead of screwing with theirs
            Set m_rctClip = F2RectFromStruct(l_rctRect)
        Else
            ' Nobody has a reference to our rectangle, so we can just update it (in case someone modified it) and return it
            m_rctClip.SetRectangle l_rctRect
        End If
    End If
    Set ClipRectangle = m_rctClip
End Property

Public Property Get DIBHandle() As Long
On Error Resume Next
    DIBHandle = GetImageDIBHandle(m_lngImage)
End Property

Public Property Get Duplicate() As Fury2Image
Attribute Duplicate.VB_Description = "*"
Attribute Duplicate.VB_MemberFlags = "400"
On Error Resume Next
    Set Duplicate = New Fury2Image
    Duplicate.Copy Me
End Property

Public Property Get Handle() As Long
    Handle = m_lngImage
End Property

Public Property Get Height() As Long
    If Initialized Then Height = SoftFX.GetImageHeight(m_lngImage)
End Property

Public Property Get Initialized() As Boolean
On Error Resume Next
    Initialized = (SoftFX.GetInitialized())
End Property

Public Property Get Locked() As Boolean
On Error Resume Next
    Locked = (SoftFX.GetImageLocked(m_lngImage) <> 0)
End Property

Public Property Get MaskColor() As Long
Attribute MaskColor.VB_MemberFlags = "40"
    MaskColor = MatteColor
End Property

Public Property Get MatteColor() As Long
    MatteColor = GetImageMatteColor(m_lngImage)
End Property

Public Property Get Name() As String
    Name = m_strName
End Property

Public Property Get NumericTags(ByVal Index As Long) As Long
On Error Resume Next
    NumericTags = GetImageTag(m_lngImage, ClipValue(Index, 0, 7))
End Property

Public Property Get Picture() As StdPicture
On Error Resume Next
Dim l_imgTemp As Fury2Image
    Set l_imgTemp = Me.Duplicate
    l_imgTemp.Composite F2RGB(255, 255, 255, 0)
    Set Picture = CreatePicture(Width, Height)
    PixelsToPicture Picture, l_imgTemp.Pointer(0, 0)
    l_imgTemp.Deallocate
    Set l_imgTemp = Nothing
End Property

Public Property Get Pitch() As Long
    If Initialized Then Pitch = SoftFX.GetImagePitch(m_lngImage)
End Property

Public Property Get Pointer(ByVal X As Long, ByVal Y As Long) As Long
    If Initialized Then Pointer = SoftFX.GetImagePointer(m_lngImage, X, Y)
End Property

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
Dim l_rctRect As Rectangle
Dim l_lngRefCount As Long
    SoftFX.GetImageRectangle m_lngImage, l_rctRect
    If m_rctMe Is Nothing Then
        Set m_rctMe = F2RectFromStruct(l_rctRect)
    Else
        ' If we already have a rectangle object for ourselves, see if someone is holding a reference to it
        l_lngRefCount = GetReferenceCount(m_rctMe)
        If (l_lngRefCount > 4) Then
            ' Someone's got a reference, so we make ourselves a new copy instead of screwing with theirs
            Set m_rctMe = F2RectFromStruct(l_rctRect)
        Else
            ' Nobody has a reference to our rectangle, so we can just update it (in case someone modified it) and return it
            m_rctMe.SetRectangle l_rctRect
        End If
    End If
    Set Rectangle = m_rctMe
End Property

Public Property Get RenderMode() As SFXRenderModes
    RenderMode = m_rmdRenderMode
End Property

Public Property Get ResampleMode() As SFXResampleModes
    ResampleMode = m_smdResampleMode
End Property

Public Property Get Width() As Long
    If Initialized Then Width = SoftFX.GetImageWidth(m_lngImage)
End Property

Public Property Let AlphaChannel(ByVal NewValue As Boolean)
    m_booAlphaChannel = NewValue
End Property

Public Property Let BlitMode(ByVal NewMode As SFXBlitModes)
    If NewMode <> BlitMode_Default Then m_bmdBlitMode = NewMode
End Property

Public Property Let Locked(NewState As Boolean)
On Error Resume Next
Dim l_lngState As Long
    l_lngState = SoftFX.GetImageLocked(m_lngImage)
    If (l_lngState <> 0) = (NewState <> False) Then
    Else
        If NewState Then
            SoftFX.LockImage m_lngImage
        Else
            SoftFX.UnlockImage m_lngImage
        End If
    End If
End Property

Public Property Let MaskColor(ByVal NewColor As Long)
    MatteColor = NewColor
End Property

Public Property Let MatteColor(ByVal NewColor As Long)
    SetImageMatteColor m_lngImage, NewColor
End Property

Public Property Let Name(ByVal NewName As String)
    m_strName = NewName
End Property

Public Property Let NumericTags(ByVal Index As Long, ByVal Value As Long)
On Error Resume Next
    Call SetImageTag(m_lngImage, ClipValue(Index, 0, 7), Value)
End Property

Public Property Let RenderMode(ByVal NewMode As SFXRenderModes)
    If NewMode <> RenderMode_Default Then m_rmdRenderMode = NewMode
End Property

Public Property Let ResampleMode(ByVal NewMode As SFXResampleModes)
    If NewMode <> ResampleMode_Default Then m_smdResampleMode = NewMode
End Property

Public Property Set ClipRectangle(ByRef NewRect As Fury2Rect)
On Error Resume Next
    SoftFX.SetImageClipRectangle m_lngImage, Rectangle.GetRectangle
    ClippedSetClipRectangle NewRect
End Property

Public Property Set Picture(ByVal NewPicture As StdPicture)
On Error Resume Next
Dim l_imgAlpha As Fury2Image
    If NewPicture Is Nothing Then Exit Property
    If NewPicture.Type = PicType_Icon Then
        Resize GetIconWidth(NewPicture), GetIconHeight(NewPicture), False
        PixelsFromIcon NewPicture, Pointer(0, 0)
        Set l_imgAlpha = New Fury2Image
        Set l_imgAlpha.Picture = GetIconMask(NewPicture)
        l_imgAlpha.Invert
        CopyChannel l_imgAlpha, Alpha, Red
    ElseIf NewPicture.Type = PicType_Bitmap Then
        Resize GetPictureWidth(NewPicture), GetPictureHeight(NewPicture), False
        PixelsFromPicture NewPicture, Pointer(0, 0)
        FillChannel Me.Rectangle, Alpha, 255
    End If
    AlphaChannel = True
End Property

Public Sub Adjust(ByVal Amount As Long, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Adjust m_lngImage, Area.GetRectangle, Amount
End Sub

Public Sub AdjustChannel(ByVal Channel As Long, ByVal Amount As Long, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Adjust_Channel m_lngImage, Area.GetRectangle, Channel, Amount
End Sub

Public Sub AdjustChannelGamma(ByVal Channel As Long, ByVal Gamma As Single, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Gamma_Channel m_lngImage, Area.GetRectangle, Channel, Gamma
End Sub

Public Sub AdjustGamma(ByVal Gamma As Single, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Gamma m_lngImage, Area.GetRectangle, Gamma
End Sub

Public Sub AdjustHSV(ByVal HueAmount As Single, ByVal SaturationAmount As Single, ByVal ValueAmount As Single, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Adjust_HSV m_lngImage, Area.GetRectangle, Floor(HueAmount * 10), Floor(SaturationAmount * 2550), Floor(ValueAmount * 2550)
End Sub

Public Sub AdjustRGB(ByVal RedAmount As Long, ByVal GreenAmount As Long, ByVal BlueAmount As Long, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Adjust_RGB m_lngImage, Area.GetRectangle, RedAmount, GreenAmount, BlueAmount
End Sub

Public Sub AdjustRGBGamma(ByVal RedGamma As Single, ByVal GreenGamma As Single, ByVal BlueGamma As Single, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Gamma_RGB m_lngImage, Area.GetRectangle, RedGamma, GreenGamma, BlueGamma
End Sub

Public Sub Allocate()
On Error Resume Next
    Deallocate
    m_lngImage = SoftFX.AllocateEmptyImage
'    HeapAdd m_lngImage
End Sub

Public Sub AllocateDIBSection(ByVal Width As Long, ByVal Height As Long, ByVal DC As Long)
On Error Resume Next
    Deallocate
    m_lngImage = SoftFX.AllocateDIBSection(Width, Height, DC)
'    HeapAdd m_lngImage
End Sub

Public Sub AllocateShared(ByVal Width As Long, ByVal Height As Long, ByVal ID As String)
On Error Resume Next
    Deallocate
    m_lngImage = SoftFX.AllocateSharedImage(Width, Height, ID)
'    HeapAdd m_lngImages
End Sub

Public Sub AntiAliasArc(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Single, ByVal YRadius As Single, ByVal StartAngle As Single, ByVal EndAngle As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Accuracy As Single = 0.66, Optional ByVal Thickness As Single = 1, Optional ByVal Softness As Single = 0.75)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Arc_AntiAlias Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), XRadius, YRadius, StartAngle, EndAngle, Color, GetRenderer(Mode), Thickness, Softness, Accuracy
End Sub

Public Sub AntiAliasConvexPolygon(ByVal Points, ByVal Color As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_ptPoints() As FPoint
Dim l_lngColor As Long
Dim l_lngPoints As Long
Dim l_plyPoly As GenericPolygon
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If (VarType(Points) And vbArray) <> vbArray Then Exit Sub
    Err.Clear
    ReDim l_ptPoints(LBound(Points) To UBound(Points))
    For l_lngPoints = LBound(Points) To UBound(Points)
        If (VarType(Points(l_lngPoints)) And vbArray) <> vbArray Then Exit Sub
        l_ptPoints(l_lngPoints).X = CSng(Points(l_lngPoints)(0))
        l_ptPoints(l_lngPoints).Y = CSng(Points(l_lngPoints)(1))
    Next l_lngPoints
    l_lngColor = Color
    If Err <> 0 Then Exit Sub
    l_plyPoly.VertexPointer = VarPtr(l_ptPoints(0))
    l_plyPoly.VertexCount = (UBound(Points) - LBound(Points)) + 1
    Primitive_ConvexPolygon_AntiAlias Handle, l_plyPoly, l_lngColor, GetRenderer(Mode), 0
End Sub

Public Sub AntiAliasEllipse(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Single, ByVal YRadius As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Accuracy As Single = 0.66, Optional ByVal Thickness As Single = 1, Optional ByVal Softness As Single = 0.75)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Circle_AntiAlias Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), XRadius, YRadius, Color, GetRenderer(Mode), Thickness, Softness, Accuracy
End Sub

Public Sub AntiAliasFilledArc(ByVal Coordinates, ByVal Color As Long, ByVal R1 As Single, ByVal R2 As Single, ByVal StartAngle As Single, ByVal EndAngle As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Step As Single = 10)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Arc_Filled_AntiAlias Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), R1, R2, StartAngle, EndAngle, Color, GetRenderer(Mode), Step
End Sub

Public Sub AntiAliasFilledEllipse(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Single, ByVal YRadius As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Accuracy As Single = 0.66)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Circle_Filled_AntiAlias Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), XRadius, YRadius, Color, GetRenderer(Mode), Accuracy
End Sub

Public Sub AntiAliasFilledPie(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Single, ByVal YRadius As Single, ByVal StartAngle As Single, ByVal EndAngle As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Accuracy As Single = 0.66)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Pie_Filled_AntiAlias Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), XRadius, YRadius, StartAngle, EndAngle, Color, GetRenderer(Mode), Accuracy
End Sub

Public Sub AntiAliasGradientLine(ByVal Coordinates, ByVal StartColor As Long, ByVal EndColor As Long)
On Error Resume Next
Dim l_rctArea As Fury2Rect
Dim l_sngX1 As Single, l_sngY1 As Single, l_sngX2 As Single, l_sngY2 As Single
    If Not Initialized Then Exit Sub
    If VarType(Coordinates) = vbObject Then
        Set l_rctArea = Coordinates
        l_sngX1 = l_rctArea.Left
        l_sngY1 = l_rctArea.Top
        l_sngX2 = l_rctArea.Right
        l_sngY2 = l_rctArea.Bottom
    ElseIf (VarType(Coordinates) And vbArray) = vbArray Then
        If VarType(Coordinates(0) And vbArray) = vbArray Then
            l_sngX1 = CSng(Coordinates(0)(0))
            l_sngY1 = CSng(Coordinates(0)(1))
            l_sngX2 = CSng(Coordinates(1)(0))
            l_sngY2 = CSng(Coordinates(1)(1))
        Else
            l_sngX1 = CSng(Coordinates(0))
            l_sngY1 = CSng(Coordinates(1))
            l_sngX2 = CSng(Coordinates(2))
            l_sngY2 = CSng(Coordinates(3))
        End If
    End If
    If Err <> 0 Then Exit Sub
    Primitive_Line_Gradient_AA Handle, l_sngX1, l_sngY1, l_sngX2, l_sngY2, StartColor, EndColor
End Sub

Public Sub AntiAliasLine(ByVal Coordinates, ByVal Color As Long)
On Error Resume Next
Dim l_rctArea As Fury2Rect
Dim l_sngX1 As Single, l_sngY1 As Single, l_sngX2 As Single, l_sngY2 As Single
    If Not Initialized Then Exit Sub
    If VarType(Coordinates) = vbObject Then
        Set l_rctArea = Coordinates
        l_sngX1 = l_rctArea.Left
        l_sngY1 = l_rctArea.Top
        l_sngX2 = l_rctArea.Right
        l_sngY2 = l_rctArea.Bottom
    ElseIf (VarType(Coordinates) And vbArray) = vbArray Then
        If VarType(Coordinates(0) And vbArray) = vbArray Then
            l_sngX1 = CSng(Coordinates(0)(0))
            l_sngY1 = CSng(Coordinates(0)(1))
            l_sngX2 = CSng(Coordinates(1)(0))
            l_sngY2 = CSng(Coordinates(1)(1))
        Else
            l_sngX1 = CSng(Coordinates(0))
            l_sngY1 = CSng(Coordinates(1))
            l_sngX2 = CSng(Coordinates(2))
            l_sngY2 = CSng(Coordinates(3))
        End If
    End If
    If Err <> 0 Then Exit Sub
    Primitive_Line_AA Handle, l_sngX1, l_sngY1, l_sngX2, l_sngY2, Color
End Sub

Public Sub AntiAliasPie(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Single, ByVal YRadius As Single, ByVal StartAngle As Single, ByVal EndAngle As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Accuracy As Single = 0.66, Optional ByVal Thickness As Single = 1, Optional ByVal Softness As Single = 0.75)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Pie_AntiAlias Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), XRadius, YRadius, StartAngle, EndAngle, Color, GetRenderer(Mode), Thickness, Softness, Accuracy
End Sub

Public Sub AntiAliasTexturedConvexPolygon(ByVal Points, ByVal Texture As Fury2Image, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default, Optional ByVal Color As Long = 0)
On Error Resume Next
Dim l_ptPoints() As TexturedVertex
Dim l_lngColor As Long
Dim l_imgTexture As Fury2Image
Dim l_lngPoints As Long
Dim l_plyPoly As GenericPolygon
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If (VarType(Points) And vbArray) <> vbArray Then Exit Sub
    Err.Clear
    ReDim l_ptPoints(LBound(Points) To UBound(Points))
    Set l_imgTexture = Texture
    For l_lngPoints = LBound(Points) To UBound(Points)
        With l_ptPoints(l_lngPoints)
            If (VarType(Points(l_lngPoints)) And vbArray) <> vbArray Then Exit Sub
            .X = CSng(Points(l_lngPoints)(0))
            .Y = CSng(Points(l_lngPoints)(1))
            If UBound(Points(l_lngPoints)) > 1 Then
                .U = CSng(Points(l_lngPoints)(2))
                .V = CSng(Points(l_lngPoints)(3))
            Else
                Select Case l_lngPoints
                Case 0
                    .U = 0
                    .V = 0
                Case 1
                    .U = 1
                    .V = 0
                Case 2
                    .U = 1
                    .V = 1
                Case 3
                    .U = 0
                    .V = 1
                Case Else ' wtf dude i am not a doctor
                End Select
            End If
        End With
    Next l_lngPoints
    For l_lngPoints = LBound(Points) To UBound(Points)
        With l_ptPoints(l_lngPoints)
            .U = .U * (l_imgTexture.Width - 1)
            .V = .V * (l_imgTexture.Height - 1)
        End With
    Next l_lngPoints
    If Err <> 0 Then Exit Sub
    If ResampleMode = ResampleMode_Default Then ResampleMode = m_smdResampleMode
    l_plyPoly.VertexPointer = VarPtr(l_ptPoints(0))
    l_plyPoly.VertexCount = (UBound(Points) - LBound(Points)) + 1
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_ConvexPolygon_Textured_AntiAlias Handle, l_imgTexture.Handle, l_plyPoly, GetScaler(ResampleMode), GetRenderer(Mode), Color
End Sub

Public Sub BezierStroke(ByVal Points, Optional ByVal DefaultColor As Long = -1, Optional ByVal DefaultThickness As Single = 1, Optional ByVal Softness As Single = 0.75, Optional ByVal Looping As Boolean = False, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Accuracy As Single = 0.33)
On Error Resume Next
Dim l_strStroke As Stroke
Dim l_ptPoints() As StrokePoint
Dim l_lngPoints As Long
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If (VarType(Points) And vbArray) <> vbArray Then Exit Sub
    Err.Clear
    ReDim l_ptPoints(LBound(Points) To UBound(Points))
    For l_lngPoints = LBound(Points) To UBound(Points)
        If (VarType(Points(l_lngPoints)) And vbArray) <> vbArray Then Exit Sub
        l_ptPoints(l_lngPoints).X = CSng(Points(l_lngPoints)(0))
        l_ptPoints(l_lngPoints).Y = CSng(Points(l_lngPoints)(1))
        If UBound(Points(l_lngPoints)) >= 2 Then
            l_ptPoints(l_lngPoints).Color = CLng(Points(l_lngPoints)(2))
        Else
            l_ptPoints(l_lngPoints).Color = DefaultColor
        End If
        If UBound(Points(l_lngPoints)) = 3 Then
            l_ptPoints(l_lngPoints).Thickness = CSng(Points(l_lngPoints)(3))
        Else
            l_ptPoints(l_lngPoints).Thickness = DefaultThickness
        End If
    Next l_lngPoints
    If Err <> 0 Then Exit Sub
    l_strStroke.Points = VarPtr(l_ptPoints(0))
    l_strStroke.Loop = Abs(CLng(Looping))
    l_strStroke.PointCount = UBound(Points) - LBound(Points) + 1
    l_strStroke.Softness = Softness
    Primitive_Stroke_Bezier Handle, l_strStroke, GetRenderer(Mode), 0, Accuracy
End Sub

Public Sub NormalMapBlit(Optional ByVal DestRectangle As Fury2Rect = Nothing, Optional ByVal SourceRectangle As Fury2Rect = Nothing, Optional ByVal Source As Fury2Image = Nothing, Optional ByVal LightVector As Variant = vbEmpty, Optional ByVal LightColor As Long = -1, Optional ByVal Mode As SFXBlitModes = BlitMode_Default, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default)
On Error Resume Next
Dim l_fp3Light As FPoint3
Dim l_lngScaler As Long
Dim l_vtType As VbVarType
    l_vtType = VarType(LightVector)
    If (l_vtType And vbArray) = vbArray Then
        l_fp3Light.X = LightVector(0)
        l_fp3Light.Y = LightVector(1)
        l_fp3Light.Z = LightVector(2)
    ElseIf (l_vtType = vbUserDefinedType) Or (l_vtType = vbObject) Then
        l_fp3Light.X = LightVector.X
        l_fp3Light.Y = LightVector.Y
        l_fp3Light.Z = LightVector.Z
    Else
        Exit Sub
    End If
    If Source Is Nothing Then Exit Sub
    If DestRectangle Is Nothing Then Set DestRectangle = Me.Rectangle
    If SourceRectangle Is Nothing Then Set SourceRectangle = Source.Rectangle
    If Mode = BlitMode_Default Then Mode = m_bmdBlitMode
    If ResampleMode = ResampleMode_Default Then ResampleMode = m_smdResampleMode
    If ((SourceRectangle.Width <> DestRectangle.Width) Or (SourceRectangle.Height <> DestRectangle.Height)) Then
        l_lngScaler = GetScaler(ResampleMode)
    End If
    If l_lngScaler Then
    Else
        Select Case Mode
        Case SFXBlitModes.BlitMode_Normal
            SoftFX.Blit_NormalMap m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_fp3Light, LightColor
        Case SFXBlitModes.BlitMode_SourceAlpha
            SoftFX.Blit_NormalMap_SourceAlpha m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_fp3Light, LightColor
        Case SFXBlitModes.BlitMode_Additive
            SoftFX.Blit_NormalMap_Additive m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_fp3Light, LightColor
        Case SFXBlitModes.BlitMode_Additive_SourceAlpha
            SoftFX.Blit_NormalMap_Additive_SourceAlpha m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_fp3Light, LightColor
        Case Else
        End Select
    End If
End Sub

Public Sub Blit(Optional ByVal DestRectangle As Fury2Rect = Nothing, Optional ByVal SourceRectangle As Fury2Rect = Nothing, Optional ByVal Source As Fury2Image = Nothing, Optional ByVal Alpha As Single = 1, Optional ByVal Mode As SFXBlitModes = BlitMode_Default, Optional ByVal Color As Long = &HFFFFFFFF, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default)
On Error Resume Next
Dim l_lngAlpha As Long
Dim l_lngScaler As Long
    l_lngAlpha = Alpha * 255
    If Source Is Nothing Then Exit Sub
    If DestRectangle Is Nothing Then Set DestRectangle = Me.Rectangle
    If SourceRectangle Is Nothing Then Set SourceRectangle = Source.Rectangle
    If Mode = BlitMode_Default Then Mode = m_bmdBlitMode
    If ResampleMode = ResampleMode_Default Then ResampleMode = m_smdResampleMode
    If ((SourceRectangle.Width <> DestRectangle.Width) Or (SourceRectangle.Height <> DestRectangle.Height)) Then
        l_lngScaler = GetScaler(ResampleMode)
    End If
    If l_lngScaler Then
        Select Case Mode
        Case SFXBlitModes.BlitMode_Normal, SFXBlitModes.BlitMode_Matte
            SoftFX.Blit_Resample_Normal_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler, l_lngAlpha
        Case SFXBlitModes.BlitMode_SourceAlpha
            SoftFX.Blit_Resample_SourceAlpha_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler, l_lngAlpha
        Case SFXBlitModes.BlitMode_SourceAlpha_Tint
            SoftFX.Blit_Resample_SourceAlpha_Tint_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler, Color, l_lngAlpha
        Case SFXBlitModes.BlitMode_Merge
            SoftFX.Blit_Resample_Merge_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler, l_lngAlpha
        Case SFXBlitModes.BlitMode_Additive
            SoftFX.Blit_Resample_Additive_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler, l_lngAlpha
        Case SFXBlitModes.BlitMode_Subtractive
            SoftFX.Blit_Resample_Subtractive_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler, l_lngAlpha
        Case SFXBlitModes.BlitMode_Multiply
            SoftFX.Blit_Resample_Multiply_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler, l_lngAlpha
        Case SFXBlitModes.BlitMode_Lightmap
            SoftFX.Blit_Resample_Lightmap m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler
        Case SFXBlitModes.BlitMode_Lightmap_RGB
            SoftFX.Blit_Resample_Lightmap_RGB m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.GetRectangle, l_lngScaler
        Case Else
        End Select
    Else
        Select Case Mode
        Case SFXBlitModes.BlitMode_Normal
            SoftFX.Blit_Normal_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Matte
            SoftFX.Blit_Automatic_Matte_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_SourceAlpha
            SoftFX.Blit_Automatic_SourceAlpha_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_SourceAlpha_Premultiplied
            SoftFX.Blit_SourceAlpha_Premultiplied_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Additive
            SoftFX.Blit_Additive_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Subtractive
            SoftFX.Blit_Subtractive_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Additive_SourceAlpha
            SoftFX.Blit_Additive_SourceAlpha_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Subtractive_SourceAlpha
            SoftFX.Blit_Subtractive_SourceAlpha_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Matte_Tint
            SoftFX.Blit_Matte_Tint_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, Color, l_lngAlpha
        Case SFXBlitModes.BlitMode_SourceAlpha_Tint
            SoftFX.Blit_SourceAlpha_Tint_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, Color, l_lngAlpha
        Case SFXBlitModes.BlitMode_Normal_Tint
            SoftFX.Blit_Normal_Tint_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, Color, l_lngAlpha
        Case SFXBlitModes.BlitMode_Font
            SoftFX.Blit_Font_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, Color, l_lngAlpha
        Case SFXBlitModes.BlitMode_Font_SourceAlpha
            SoftFX.Blit_Font_SourceAlpha_RGB_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, Color, l_lngAlpha
        Case SFXBlitModes.BlitMode_Font_Merge
            SoftFX.Blit_Font_Merge_RGB_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, Color, l_lngAlpha
        Case SFXBlitModes.BlitMode_Dodge
            SoftFX.Blit_Dodge_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Burn
            SoftFX.Blit_Burn_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Behind
            SoftFX.Blit_Behind_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_AND
            SoftFX.Blit_AND m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top
        Case SFXBlitModes.BlitMode_OR
            SoftFX.Blit_OR m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top
        Case SFXBlitModes.BlitMode_XOR
            SoftFX.Blit_XOR m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top
        Case SFXBlitModes.BlitMode_Difference
            SoftFX.Blit_Difference m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top
        Case SFXBlitModes.BlitMode_Dither
            SoftFX.Blit_Dither m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top
        Case SFXBlitModes.BlitMode_Lightmap
            SoftFX.Blit_Lightmap_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Lightmap_RGB
            SoftFX.Blit_Lightmap_RGB_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Screen
            SoftFX.Blit_Screen_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Multiply
            SoftFX.Blit_Multiply_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Merge
            SoftFX.Blit_Merge_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Erase
            SoftFX.Blit_Erase_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_Unerase
            SoftFX.Blit_Unerase_Opacity m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, l_lngAlpha
        Case SFXBlitModes.BlitMode_SourceAlpha_ColorMask
            SoftFX.Blit_SourceAlpha_ColorMask m_lngImage, Source.Handle, DestRectangle.GetRectangle, SourceRectangle.Left, SourceRectangle.Top, SetAlpha(Color, GetAlpha(Color) * Alpha)
        Case Else
        End Select
    End If
End Sub

Public Sub Box(ByVal Area As Fury2Rect, ByVal Color As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctArea As Fury2Rect
Dim l_lngColor As Long
    If Not Initialized Then Exit Sub
    Set l_rctArea = Area
    l_lngColor = Color
    If Err <> 0 Then Exit Sub
    If l_rctArea Is Nothing Then Exit Sub
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Primitive_Box m_lngImage, l_rctArea.GetRectangle, l_lngColor
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Primitive_Box_SourceAlpha m_lngImage, l_rctArea.GetRectangle, l_lngColor
    Case Else
    End Select
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    g_lngImageCount = g_lngImageCount + 1
    If Initialized Then Allocate
    m_booAlphaChannel = False
    ResetClipRectangleStack
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Deallocate
    g_lngImageCount = g_lngImageCount - 1
End Sub

Public Sub Clear(Optional ByVal Color As Long = 0)
On Error Resume Next
Dim l_lngColor As Long
    l_lngColor = CLng(Color)
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If l_lngColor = 0 Then
        SoftFX.ClearImage m_lngImage
    Else
        SoftFX.Filter_Fill_Pointer m_lngImage, 0, l_lngColor
    End If
End Sub

Public Sub ClippedSetClipRectangle(ByVal NewRect As Fury2Rect)
On Error Resume Next
Dim l_rcRect As Rectangle
    l_rcRect = NewRect.GetRectangle
    If SoftFX.ClipRectangle_ImageClipRect(l_rcRect, m_lngImage) Then
        SoftFX.SetImageClipRectangle m_lngImage, l_rcRect
    Else
        l_rcRect.Left = 0
        l_rcRect.Top = 0
        l_rcRect.Width = 0
        l_rcRect.Height = 0
        SoftFX.SetImageClipRectangle m_lngImage, l_rcRect
    End If
End Sub

Public Property Get Premultiplied() As Boolean
On Error Resume Next
    Premultiplied = SoftFX.GetImagePremultiplied(m_lngImage)
End Property

Public Property Let Premultiplied(ByVal NewValue As Boolean)
On Error Resume Next
    SoftFX.SetImagePremultiplied m_lngImage, NewValue
End Property

Public Property Get NormalsPrepared() As Boolean
On Error Resume Next
    NormalsPrepared = SoftFX.GetImageNormalsPrepared(m_lngImage)
End Property

Public Property Let NormalsPrepared(ByVal NewValue As Boolean)
On Error Resume Next
    SoftFX.SetImageNormalsPrepared m_lngImage, NewValue
End Property

Public Sub Premultiply()
On Error Resume Next
    If Premultiplied Then Exit Sub
    SoftFX.Filter_Premultiply m_lngImage, Rectangle.GetRectangle
End Sub

Public Sub PrepareNormals()
On Error Resume Next
'    If Premultiplied Then Exit Sub
    SoftFX.Filter_PrepareNormals m_lngImage, Rectangle.GetRectangle
End Sub

Public Sub Composite(ByVal Color As Long)
On Error Resume Next
    SoftFX.Filter_Composite m_lngImage, ClipRectangle.GetRectangle, Color
End Sub

Public Sub ConvexPolygon(ByVal Points, ByVal Color As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_ptPoints() As FPoint
Dim l_lngColor As Long
Dim l_lngPoints As Long
Dim l_plyPoly As GenericPolygon
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If (VarType(Points) And vbArray) <> vbArray Then Exit Sub
    Err.Clear
    ReDim l_ptPoints(LBound(Points) To UBound(Points))
    For l_lngPoints = LBound(Points) To UBound(Points)
        If (VarType(Points(l_lngPoints)) And vbArray) <> vbArray Then Exit Sub
        l_ptPoints(l_lngPoints).X = CSng(Points(l_lngPoints)(0))
        l_ptPoints(l_lngPoints).Y = CSng(Points(l_lngPoints)(1))
    Next l_lngPoints
    l_lngColor = Color
    If Err <> 0 Then Exit Sub
    l_plyPoly.VertexPointer = VarPtr(l_ptPoints(0))
    l_plyPoly.VertexCount = (UBound(Points) - LBound(Points)) + 1
    Primitive_ConvexPolygon Handle, l_plyPoly, l_lngColor, GetRenderer(Mode), 0
End Sub

Public Sub Copy(ByVal Image As Fury2Image)
On Error Resume Next
    If TypeOf Image Is Fury2Image Then Else Exit Sub
    If Initialized Then SoftFX.CopyImage m_lngImage, Image.Handle
    Me.Name = Image.Name
    Me.AlphaChannel = Image.AlphaChannel
    Me.BlitMode = Image.BlitMode
    Me.RenderMode = Image.RenderMode
    Set Me.ClipRectangle = Image.ClipRectangle
    Me.MaskColor = Image.MaskColor
    Me.ResampleMode = Image.ResampleMode
    ResetClipRectangleStack
End Sub

Public Sub CopyChannel(ByRef SourceImage, ByVal DestChannel As Long, ByVal SourceChannel As Long)
On Error Resume Next
Dim l_imgSource As Fury2Image
Dim l_lngSourceChannel As Long, l_lngDestChannel As Long
    Set l_imgSource = SourceImage
    l_lngSourceChannel = SourceChannel
    l_lngDestChannel = DestChannel
    If Err <> 0 Then Exit Sub
    Err.Clear
    SoftFX.Blit_Channel m_lngImage, l_imgSource.Handle, Rectangle.GetRectangle, 0, 0, l_lngDestChannel, l_lngSourceChannel
End Sub

Public Sub Deallocate()
On Error Resume Next
    If m_lngImage <> 0 Then
        If Not m_booNoDelete Then
            SoftFX.DeallocateImage m_lngImage
    '        HeapRemove m_lngImage
        End If
        m_lngImage = 0
    End If
End Sub

Public Sub Decay(ByVal Rectangle As Fury2Rect, Optional ByVal Alpha As Single = 1)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_lngAlpha As Long
    Set l_rctRect = Rectangle
    l_lngAlpha = Alpha * 255
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    SoftFX.Filter_Decay m_lngImage, l_rctRect.GetRectangle, l_lngAlpha
End Sub

Public Sub DeformBlit(Optional ByVal DestRectangle As Fury2Rect = Nothing, Optional ByVal SourceRectangle As Fury2Rect = Nothing, Optional ByVal Source As Fury2Image = Nothing, Optional ByVal Mesh As Fury2DeformationMesh = Nothing, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default, Optional ByVal Color As Long = 0)
On Error Resume Next
Dim l_rctDest As Fury2Rect, l_rctSource As Fury2Rect
Dim l_imgSource As Fury2Image
    Set l_rctDest = DestRectangle
    Set l_rctSource = SourceRectangle
    Set l_imgSource = Source
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mesh Is Nothing Then Exit Sub
    If l_imgSource Is Nothing Then Exit Sub
    If l_rctDest Is Nothing Then Set l_rctDest = Me.Rectangle
    If l_rctSource Is Nothing Then Set l_rctSource = l_imgSource.Rectangle
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If ResampleMode = ResampleMode_Default Then ResampleMode = m_smdResampleMode
    SoftFX.Blit_Deform m_lngImage, l_imgSource.Handle, Mesh.GetParam, l_rctDest.GetRectangle, l_rctSource.GetRectangle, Color, GetRenderer(Mode), GetScaler(ResampleMode)
End Sub

Public Sub Ellipse(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Long, ByVal YRadius As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    If Not Initialized Then Exit Sub
    Err.Clear
    l_lngX = CLng(Coordinates(0))
    l_lngY = CLng(Coordinates(1))
    If Err <> 0 Then Exit Sub
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Primitive_Circle m_lngImage, l_lngX, l_lngY, XRadius, Color
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Primitive_Circle_SourceAlpha m_lngImage, l_lngX, l_lngY, XRadius, Color
    Case Else
    End Select
End Sub

Public Sub EnsureSize(ByVal Width As Long, ByVal Height As Long)
On Error Resume Next
    If Me.Width < Width Or Me.Height < Height Then
        Resize Width, Height
    End If
End Sub

Public Sub Fill(ByVal Rectangle As Fury2Rect, ByVal Color As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_lngColor As Long
    Set l_rctRect = Rectangle
    l_lngColor = Color
    If l_rctRect Is Nothing Then Set l_rctRect = Me.ClipRectangle
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Filter_Fill m_lngImage, l_rctRect.GetRectangle, l_lngColor
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Filter_Fill_SourceAlpha m_lngImage, l_rctRect.GetRectangle, l_lngColor
    Case SFXRenderModes.RenderMode_Additive
        SoftFX.Filter_Fill_Additive m_lngImage, l_rctRect.GetRectangle, l_lngColor
    Case SFXRenderModes.RenderMode_Subtractive
        SoftFX.Filter_Fill_Subtractive m_lngImage, l_rctRect.GetRectangle, l_lngColor
    Case Else
    End Select
End Sub

Public Sub FillChannel(ByVal Rectangle As Fury2Rect, ByVal Channel As Long, ByVal Value As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_lngValue As Long, l_lngChannel As Long
    Set l_rctRect = Rectangle
    l_lngValue = Value
    l_lngChannel = Channel
    If l_rctRect Is Nothing Then Set l_rctRect = Me.ClipRectangle
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Filter_Fill_Channel m_lngImage, l_rctRect.GetRectangle, l_lngChannel, l_lngValue
    Case Else
    End Select
End Sub

Public Sub FilledArc(ByVal Coordinates, ByVal Color As Long, ByVal R1 As Single, ByVal R2 As Single, ByVal StartAngle As Single, ByVal EndAngle As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Step As Single = 10)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Arc_Filled Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), R1, R2, StartAngle, EndAngle, Color, GetRenderer(Mode), Step
End Sub

Public Sub FilledEllipse(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Long, ByVal YRadius As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    If Not Initialized Then Exit Sub
    Err.Clear
    l_lngX = CLng(Coordinates(0))
    l_lngY = CLng(Coordinates(1))
    If Err <> 0 Then Exit Sub
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Primitive_Circle_Filled m_lngImage, l_lngX, l_lngY, XRadius, Color
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Primitive_Circle_Filled_SourceAlpha m_lngImage, l_lngX, l_lngY, XRadius, Color
    Case Else
    End Select
End Sub

Public Sub FilledPie(ByVal Coordinates, ByVal Color As Long, ByVal XRadius As Single, ByVal YRadius As Single, ByVal StartAngle As Single, ByVal EndAngle As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Accuracy As Single = 0.66)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Pie_Filled Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), XRadius, YRadius, StartAngle, EndAngle, Color, GetRenderer(Mode), Accuracy
End Sub

Public Sub Flip(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.Rectangle
    SoftFX.Filter_Flip m_lngImage, Area.GetRectangle
End Sub

Public Sub GenerateMask()
On Error Resume Next
Dim l_rctImage As Rectangle
    If Not Initialized Then Exit Sub
    SoftFX.GetImageClipRectangle m_lngImage, l_rctImage
    SoftFX.Filter_Fill_Channel m_lngImage, l_rctImage, 3, 255
    SoftFX.Filter_Replace m_lngImage, l_rctImage, SetAlpha(SoftFX.GetImageMatteColor(m_lngImage), 255), 0
    SoftFX.SetImageMatteColor m_lngImage, 0
    m_booAlphaChannel = True
End Sub

Public Sub GradientConvexPolygon(ByVal Points, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_ptPoints() As GradientVertex
Dim l_lngColor As Long
Dim l_lngPoints As Long
Dim l_plyPoly As GenericPolygon
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If (VarType(Points) And vbArray) <> vbArray Then Exit Sub
    Err.Clear
    ReDim l_ptPoints(LBound(Points) To UBound(Points))
    For l_lngPoints = LBound(Points) To UBound(Points)
        With l_ptPoints(l_lngPoints)
            If (VarType(Points(l_lngPoints)) And vbArray) <> vbArray Then Exit Sub
            .X = CSng(Points(l_lngPoints)(0))
            .Y = CSng(Points(l_lngPoints)(1))
            If UBound(Points(l_lngPoints)) = 2 Then
                .Color = CLng(Points(l_lngPoints)(2))
            Else
                .Color = F2Black
            End If
        End With
    Next l_lngPoints
    If Err <> 0 Then Exit Sub
    l_plyPoly.VertexPointer = VarPtr(l_ptPoints(0))
    l_plyPoly.VertexCount = (UBound(Points) - LBound(Points)) + 1
    Primitive_ConvexPolygon_Gradient Handle, l_plyPoly, GetRenderer(Mode), 0
End Sub

Public Sub GradientFill(ByVal Rectangle As Fury2Rect, ByVal Colors, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctRect As Fury2Rect
    Set l_rctRect = Rectangle
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If UBound(Colors) = 2 Then
        Select Case LCase(Trim(CStr(Colors(2))))
        Case "h"
            Colors = Array(Colors(0), Colors(1), Colors(0), Colors(1))
        Case "v"
            Colors = Array(Colors(0), Colors(0), Colors(1), Colors(1))
        Case Else
        End Select
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Filter_Gradient_4Point m_lngImage, l_rctRect.GetRectangle, CLng(Colors(0)), CLng(Colors(1)), CLng(Colors(2)), CLng(Colors(3))
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Filter_Gradient_4Point_SourceAlpha m_lngImage, l_rctRect.GetRectangle, CLng(Colors(0)), CLng(Colors(1)), CLng(Colors(2)), CLng(Colors(3))
'    Case SFXRenderModes.RenderMode_Additive
'        SoftFX.Filter_Gradient_4Point_Additive m_lngImage, l_rctRect.GetRectangle, CLng(Colors(0)), CLng(Colors(1)), CLng(Colors(2)), CLng(Colors(3))
'    Case SFXRenderModes.RenderMode_Subtractive
'        SoftFX.Filter_Gradient_4Point_Subtractive m_lngImage, l_rctRect.GetRectangle, CLng(Colors(0)), CLng(Colors(1)), CLng(Colors(2)), CLng(Colors(3))
    Case Else
    End Select
End Sub

Public Sub GradientFilledArc(ByVal Coordinates, ByVal StartColor As Long, ByVal EndColor As Long, ByVal R1 As Single, ByVal R2 As Single, ByVal StartAngle As Single, ByVal EndAngle As Single, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal Step As Single = 10)
On Error Resume Next
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_Arc_Filled_Gradient Handle, CSng(Coordinates(0)), CSng(Coordinates(1)), R1, R2, StartAngle, EndAngle, StartColor, EndColor, GetRenderer(Mode), Step
End Sub

Public Sub GradientLine(ByVal Coordinates, ByVal StartColor As Long, ByVal EndColor As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctArea As Fury2Rect
Dim l_lngColors(0 To 1) As Long
    If Not Initialized Then Exit Sub
    If VarType(Coordinates) = vbObject Then
        Set l_rctArea = Coordinates
    ElseIf (VarType(Coordinates) And vbArray) = vbArray Then
        Set l_rctArea = F2Rect(CLng(Coordinates(0)), CLng(Coordinates(1)), CLng(Coordinates(2)), CLng(Coordinates(3)), True)
    End If
    l_lngColors(0) = StartColor
    l_lngColors(1) = EndColor
    If Err <> 0 Then Exit Sub
    If l_rctArea Is Nothing Then Exit Sub
    If l_lngColors(0) = l_lngColors(1) Then
        Me.[Line] Coordinates, StartColor, Mode
        Exit Sub
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Primitive_Line_Gradient m_lngImage, l_rctArea.GetRectangle, l_lngColors(0), l_lngColors(1)
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Primitive_Line_Gradient_SourceAlpha m_lngImage, l_rctArea.GetRectangle, l_lngColors(0), l_lngColors(1)
    Case Else
    End Select
End Sub

Public Sub GradientQuad(ByVal Points, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
    GradientConvexPolygon Points, Mode
End Sub

Public Sub Grayscale(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Grayscale m_lngImage, Area.GetRectangle
End Sub

Public Sub GrayscaleNoiseFill(ByVal Rectangle As Fury2Rect, Optional ByVal Alpha As Single = 1, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_lngAlpha As Long
    Set l_rctRect = Rectangle
    l_lngAlpha = Alpha * 255
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Filter_Noise_Grayscale_Opacity m_lngImage, l_rctRect.GetRectangle, l_lngAlpha
    Case SFXRenderModes.RenderMode_Subtractive
        SoftFX.Filter_Noise_Grayscale_Subtractive_Opacity m_lngImage, l_rctRect.GetRectangle, l_lngAlpha
    Case Else
    End Select
End Sub

Public Sub Invert(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Invert m_lngImage, Area.GetRectangle
End Sub

Public Sub InvertChannel(ByVal Channel As Long, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Invert_Channel m_lngImage, Area.GetRectangle, Channel
End Sub

Public Sub InvertRGB(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Invert_Color m_lngImage, Area.GetRectangle
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As VirtualFile)
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long
    File.ReadSegment_Begin
        File.Load l_lngWidth
        File.Load l_lngHeight
        File.Load m_booAlphaChannel
        Resize l_lngWidth, l_lngHeight, False
        File.RawLoad Pointer(0, 0), Width * Height * 4
    File.ReadSegment_End
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As VirtualFile)
On Error Resume Next
    File.WriteSegment_Begin
        File.Save Width
        File.Save Height
        File.Save m_booAlphaChannel
        File.RawSave Pointer(0, 0), Width * Height * 4
    File.WriteSegment_End
End Sub

Public Sub Line(ByVal Coordinates, ByVal Color As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctArea As Fury2Rect
    If Not Initialized Then Exit Sub
    If VarType(Coordinates) = vbObject Then
        Set l_rctArea = Coordinates
    ElseIf (VarType(Coordinates) And vbArray) = vbArray Then
        If VarType(Coordinates(0) And vbArray) = vbArray Then
            Set l_rctArea = F2Rect(CLng(Coordinates(0)(0)), CLng(Coordinates(0)(1)), CLng(Coordinates(1)(0)), CLng(Coordinates(1)(1)), True)
        Else
            Set l_rctArea = F2Rect(CLng(Coordinates(0)), CLng(Coordinates(1)), CLng(Coordinates(2)), CLng(Coordinates(3)), True)
        End If
    End If
    If Err <> 0 Then Exit Sub
    If l_rctArea Is Nothing Then Exit Sub
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Primitive_Line m_lngImage, l_rctArea.GetRectangle, Color
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Primitive_Line_SourceAlpha m_lngImage, l_rctArea.GetRectangle, Color
    Case SFXRenderModes.RenderMode_Additive
        SoftFX.Primitive_Line_Additive m_lngImage, l_rctArea.GetRectangle, Color
    Case SFXRenderModes.RenderMode_Subtractive
        SoftFX.Primitive_Line_Subtractive m_lngImage, l_rctArea.GetRectangle, Color
    Case Else
    End Select
End Sub

Public Sub LoadBytes(ByRef Data() As Byte)
On Error Resume Next
    Deallocate
    m_lngImage = SoftFX.AllocateImageFromFileBuffer(VarPtr(Data(LBound(Data))), UBound(Data) - LBound(Data) + 1)
    If m_lngImage = 0 Then
    Else
        m_booAlphaChannel = True
    End If
    UnlockImage m_lngImage
End Sub

Public Sub LoadFile(ByVal Filename As String)
On Error Resume Next
    Deallocate
    m_lngImage = SoftFX.AllocateImageFromFile(Filename)
    If m_lngImage = 0 Then
    Else
        m_strName = Filename
        m_booAlphaChannel = True
    End If
    UnlockImage m_lngImage
End Sub

Public Sub LoadString(ByRef Data As String)
On Error Resume Next
    Deallocate
    m_lngImage = SoftFX.AllocateImageFromFileBuffer(StrPtr(Data), LenB(Data))
    If m_lngImage = 0 Then
    Else
        m_booAlphaChannel = True
    End If
    UnlockImage m_lngImage
End Sub

Public Sub MaskBlit(Optional ByVal DestRectangle As Fury2Rect = Nothing, Optional ByVal SourceRectangle As Fury2Rect = Nothing, Optional ByVal MaskRectangle As Fury2Rect = Nothing, Optional ByVal Source As Fury2Image = Nothing, Optional ByVal Mask As Fury2Image = Nothing, Optional ByVal Alpha As Single = 1, Optional ByVal Mode As SFXBlitModes = BlitMode_Default)
On Error Resume Next
Dim l_rctDest As Fury2Rect, l_rctSource As Fury2Rect, l_rctMask As Fury2Rect
Dim l_imgSource As Fury2Image, l_imgMask As Fury2Image
Dim l_sngAlpha As Single
Dim l_lngAlpha As Long
    Set l_rctDest = DestRectangle
    Set l_rctSource = SourceRectangle
    Set l_rctMask = MaskRectangle
    Set l_imgSource = Source
    Set l_imgMask = Mask
    If l_imgSource Is Nothing Then Exit Sub
    If l_imgMask Is Nothing Then Exit Sub
    If l_rctDest Is Nothing Then Set l_rctDest = Me.Rectangle
    If l_rctSource Is Nothing Then Set l_rctSource = l_imgSource.Rectangle
    If l_rctMask Is Nothing Then Set l_rctMask = l_imgMask.Rectangle
    l_sngAlpha = Alpha
    l_lngAlpha = l_sngAlpha * 255
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mode = BlitMode_Default Then Mode = m_bmdBlitMode
    Select Case Mode
    Case SFXBlitModes.BlitMode_Normal
        SoftFX.Blit_Mask_Normal_Opacity m_lngImage, l_imgSource.Handle, l_imgMask.Handle, l_rctDest.GetRectangle, l_rctSource.Left, l_rctSource.Top, l_rctMask.Left, l_rctMask.Top, l_lngAlpha
    Case SFXBlitModes.BlitMode_SourceAlpha
        SoftFX.Blit_Mask_SourceAlpha_Opacity m_lngImage, l_imgSource.Handle, l_imgMask.Handle, l_rctDest.GetRectangle, l_rctSource.Left, l_rctSource.Top, l_rctMask.Left, l_rctMask.Top, l_lngAlpha
    Case SFXBlitModes.BlitMode_Merge
        SoftFX.Blit_Mask_Merge_Opacity m_lngImage, l_imgSource.Handle, l_imgMask.Handle, l_rctDest.GetRectangle, l_rctSource.Left, l_rctSource.Top, l_rctMask.Left, l_rctMask.Top, l_lngAlpha
    Case Else
    End Select
End Sub

Public Sub MaskDeformBlit(Optional ByVal DestRectangle As Fury2Rect = Nothing, Optional ByVal SourceRectangle As Fury2Rect = Nothing, Optional ByVal MaskRectangle As Fury2Rect = Nothing, Optional ByVal Source As Fury2Image = Nothing, Optional ByVal Mask As Fury2Image = Nothing, Optional ByVal Mesh As Fury2DeformationMesh = Nothing, Optional ByVal Alpha As Single = 1#, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default, Optional ByVal Color As Long = 0)
On Error Resume Next
Dim l_rctDest As Fury2Rect, l_rctSource As Fury2Rect
Dim l_imgSource As Fury2Image
    Set l_rctDest = DestRectangle
    Set l_rctSource = SourceRectangle
    Set l_imgSource = Source
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mesh Is Nothing Then Exit Sub
    If l_imgSource Is Nothing Then Exit Sub
    If Mask Is Nothing Then Exit Sub
    If l_rctDest Is Nothing Then Set l_rctDest = Me.Rectangle
    If l_rctSource Is Nothing Then Set l_rctSource = l_imgSource.Rectangle
    If MaskRectangle Is Nothing Then Set MaskRectangle = Mask.Rectangle
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If ResampleMode = ResampleMode_Default Then ResampleMode = m_smdResampleMode
    SoftFX.Blit_Mask_Deform m_lngImage, l_imgSource.Handle, Mask.Handle, Mesh.GetParam, l_rctDest.GetRectangle, l_rctSource.GetRectangle, MaskRectangle.GetRectangle, Color, CLng(Alpha * 255), GetRenderer(Mode), GetScaler(ResampleMode)
End Sub

Public Sub Mirror(Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Mirror m_lngImage, Area.GetRectangle
End Sub

Public Sub Multiply(ByVal Factor As Single, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Multiply m_lngImage, Area.GetRectangle, Factor
End Sub

Public Sub MultiplyChannel(ByVal Channel As Long, ByVal Factor As Single, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Multiply_Channel m_lngImage, Area.GetRectangle, Channel, Factor
End Sub

Public Sub MultiplyRGB(ByVal RedFactor As Single, ByVal GreenFactor As Single, ByVal BlueFactor As Single, Optional ByVal Area As Fury2Rect = Nothing)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Area Is Nothing Then Set Area = Me.ClipRectangle
    SoftFX.Filter_Multiply_RGB m_lngImage, Area.GetRectangle, RedFactor, GreenFactor, BlueFactor
End Sub

Public Sub NoiseFill(ByVal Rectangle As Fury2Rect, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctRect As Fury2Rect
    Set l_rctRect = Rectangle
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Filter_Noise m_lngImage, l_rctRect.GetRectangle
    Case Else
    End Select
End Sub

Public Sub NoiseFillChannel(ByVal Rectangle As Fury2Rect, ByVal Channel As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_lngChannel As Long
    l_lngChannel = Channel
    Set l_rctRect = Rectangle
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Filter_Noise_Channel m_lngImage, l_rctRect.GetRectangle, l_lngChannel
    Case Else
    End Select
End Sub

Public Sub Optimize()
On Error Resume Next
    OptimizeImage m_lngImage
End Sub

Public Sub PushClipRectangle(ByVal NewRect As Fury2Rect)
On Error Resume Next
Dim l_rcRect As Rectangle
    ReDim Preserve m_rcClipStack(0 To UBound(m_rcClipStack) + 1)
    SoftFX.GetImageClipRectangle m_lngImage, m_rcClipStack(UBound(m_rcClipStack))
    l_rcRect = NewRect.GetRectangle
    If SoftFX.ClipRectangle_ImageClipRect(l_rcRect, m_lngImage) Then
        SoftFX.SetImageClipRectangle m_lngImage, l_rcRect
    Else
        l_rcRect.Left = 0
        l_rcRect.Top = 0
        l_rcRect.Width = 0
        l_rcRect.Height = 0
        SoftFX.SetImageClipRectangle m_lngImage, l_rcRect
    End If
End Sub

Public Sub Quad(ByVal Points, ByVal Color As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
    ConvexPolygon Points, Color, Mode
End Sub

Public Sub RadialGradientFill(ByVal Rectangle As Fury2Rect, ByVal Colors, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional Offsets = Nothing)
On Error Resume Next
Dim l_rctRect As Fury2Rect
    Set l_rctRect = Rectangle
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Dim l_sngOffsets(0 To 1) As Single
    If (VarType(Offsets) = vbObject) Then
    Else
        l_sngOffsets(0) = CSng(Offsets(0))
        l_sngOffsets(1) = CSng(Offsets(1))
    End If
    Select Case Mode
    Case SFXRenderModes.RenderMode_Normal
        SoftFX.Filter_Gradient_Radial_Ex m_lngImage, l_rctRect.GetRectangle, CLng(Colors(0)), CLng(Colors(1)), l_sngOffsets(0), l_sngOffsets(1)
    Case SFXRenderModes.RenderMode_SourceAlpha
        SoftFX.Filter_Gradient_Radial_SourceAlpha_Ex m_lngImage, l_rctRect.GetRectangle, CLng(Colors(0)), CLng(Colors(1)), l_sngOffsets(0), l_sngOffsets(1)
    Case Else
    End Select
End Sub

Public Sub Reallocate()
On Error Resume Next
    Deallocate
    m_lngImage = SoftFX.AllocateEmptyImage
End Sub

Public Sub ReplaceColor(ByVal FindColor As Long, ByVal ReplaceColor As Long, Optional Rectangle = Nothing)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_lngColor As Long, l_lngColor2 As Long
    If Rectangle Is Nothing Then
        Set l_rctRect = Me.Rectangle
    Else
        Set l_rctRect = Rectangle
    End If
    l_lngColor = FindColor
    l_lngColor2 = ReplaceColor
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    SoftFX.Filter_Replace m_lngImage, l_rctRect.GetRectangle, l_lngColor, l_lngColor2
End Sub

Public Sub ResetClipRectangle()
On Error Resume Next
Dim l_rctRect As Rectangle
    SoftFX.GetImageRectangle m_lngImage, l_rctRect
    SoftFX.SetImageClipRectangle m_lngImage, l_rctRect
End Sub

Public Sub ResetClipRectangleStack()
On Error Resume Next
    ReDim m_rcClipStack(0 To 0)
    SoftFX.GetImageRectangle m_lngImage, m_rcClipStack(0)
End Sub

Public Sub Resize(ByVal Width As Long, ByVal Height As Long, Optional ByVal PreserveData = False)
On Error Resume Next
    If m_booNoDelete Then Exit Sub
    If CBool(PreserveData) Then
        If Initialized Then SoftFX.ResizeImage m_lngImage, Width, Height
    Else
        If Initialized Then SoftFX.ReAllocateImage m_lngImage, Width, Height
    End If
    ResetClipRectangle
    ResetClipRectangleStack
End Sub

Public Sub Rotate(ByVal Angle As Single)
On Error Resume Next
    If Not Initialized Then Exit Sub
    If Abs(90 - Angle) < 0.5 Then
        SoftFX.Filter_Rotate90 m_lngImage
        Mirror
    ElseIf Abs(180 - Angle) < 0.5 Then
        Flip
        Mirror
    ElseIf Abs(270 - Angle) < 0.5 Then
        SoftFX.Filter_Rotate90 m_lngImage
        Flip
    ElseIf Abs(0 - Angle) < 0.5 Then
        Exit Sub
    Else
        SoftFX.RotateImage m_lngImage, Angle
    End If
End Sub

Public Sub SetHandle(Handle As Long)
On Error Resume Next
    m_lngImage = Handle
    m_booNoDelete = True
End Sub

Public Sub SetPixel(ByVal X As Long, ByVal Y As Long, ByVal Color As Long)
On Error Resume Next
    If Initialized Then SoftFX.SetPixel m_lngImage, X, Y, Color
End Sub

Public Sub SetPixelAA(ByVal X As Single, ByVal Y As Single, ByVal Color As Long)
On Error Resume Next
    If Initialized Then SoftFX.SetPixelAA m_lngImage, X, Y, Color
End Sub

Public Sub Slide(ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    If Initialized Then SoftFX.SlideImage m_lngImage, X, Y
End Sub

Public Sub Solarize(ByVal Rectangle As Fury2Rect, Optional ByVal Alpha As Single = 1)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_lngAlpha As Long
    Set l_rctRect = Rectangle
    l_lngAlpha = Alpha * 255
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    SoftFX.Filter_Solarize m_lngImage, l_rctRect.GetRectangle, l_lngAlpha
End Sub

Public Sub Stroke(ByVal Points, Optional ByVal DefaultColor As Long = -1, Optional ByVal DefaultThickness As Single = 1, Optional ByVal Softness As Single = 0.75, Optional ByVal Looping As Boolean = False, Optional ByVal Mode As SFXRenderModes = RenderMode_Default)
On Error Resume Next
Dim l_strStroke As Stroke
Dim l_ptPoints() As StrokePoint
Dim l_lngPoints As Long
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If (VarType(Points) And vbArray) <> vbArray Then Exit Sub
    Err.Clear
    ReDim l_ptPoints(LBound(Points) To UBound(Points))
    For l_lngPoints = LBound(Points) To UBound(Points)
        If (VarType(Points(l_lngPoints)) And vbArray) <> vbArray Then Exit Sub
        l_ptPoints(l_lngPoints).X = CSng(Points(l_lngPoints)(0))
        l_ptPoints(l_lngPoints).Y = CSng(Points(l_lngPoints)(1))
        If UBound(Points(l_lngPoints)) >= 2 Then
            l_ptPoints(l_lngPoints).Color = CLng(Points(l_lngPoints)(2))
        Else
            l_ptPoints(l_lngPoints).Color = DefaultColor
        End If
        If UBound(Points(l_lngPoints)) = 3 Then
            l_ptPoints(l_lngPoints).Thickness = CSng(Points(l_lngPoints)(3))
        Else
            l_ptPoints(l_lngPoints).Thickness = DefaultThickness
        End If
    Next l_lngPoints
    If Err <> 0 Then Exit Sub
    l_strStroke.Points = VarPtr(l_ptPoints(0))
    l_strStroke.Loop = Abs(CLng(Looping))
    l_strStroke.PointCount = UBound(Points) - LBound(Points) + 1
    l_strStroke.Softness = Softness
    Primitive_Stroke Handle, l_strStroke, GetRenderer(Mode), 0
End Sub

Public Sub SwapChannels(ByVal Channel1 As Long, ByVal Channel2 As Long)
On Error Resume Next
    SoftFX.Filter_Swap_Channels m_lngImage, ClipRectangle.GetRectangle, Channel1, Channel2
End Sub

Public Sub TexturedConvexPolygon(ByVal Points, ByVal Texture As Fury2Image, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default, Optional ByVal Color As Long = 0)
On Error Resume Next
Dim l_ptPoints() As TexturedVertex
Dim l_lngColor As Long
Dim l_imgTexture As Fury2Image
Dim l_lngPoints As Long
Dim l_plyPoly As GenericPolygon
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    If (VarType(Points) And vbArray) <> vbArray Then Exit Sub
    Err.Clear
    ReDim l_ptPoints(LBound(Points) To UBound(Points))
    Set l_imgTexture = Texture
    For l_lngPoints = LBound(Points) To UBound(Points)
        With l_ptPoints(l_lngPoints)
            If (VarType(Points(l_lngPoints)) And vbArray) <> vbArray Then Exit Sub
            .X = CSng(Points(l_lngPoints)(0))
            .Y = CSng(Points(l_lngPoints)(1))
            If UBound(Points(l_lngPoints)) > 1 Then
                .U = CSng(Points(l_lngPoints)(2))
                .V = CSng(Points(l_lngPoints)(3))
            Else
                Select Case l_lngPoints
                Case 0
                    .U = 0
                    .V = 0
                Case 1
                    .U = 1
                    .V = 0
                Case 2
                    .U = 1
                    .V = 1
                Case 3
                    .U = 0
                    .V = 1
                Case Else ' wtf dude i am not a doctor
                End Select
            End If
        End With
    Next l_lngPoints
    For l_lngPoints = LBound(Points) To UBound(Points)
        With l_ptPoints(l_lngPoints)
            .U = .U * (l_imgTexture.Width - 1)
            .V = .V * (l_imgTexture.Height - 1)
        End With
    Next l_lngPoints
    If Err <> 0 Then Exit Sub
    If ResampleMode = ResampleMode_Default Then ResampleMode = m_smdResampleMode
    l_plyPoly.VertexPointer = VarPtr(l_ptPoints(0))
    l_plyPoly.VertexCount = (UBound(Points) - LBound(Points)) + 1
    If Mode = RenderMode_Default Then Mode = m_rmdRenderMode
    Primitive_ConvexPolygon_Textured Handle, l_imgTexture.Handle, l_plyPoly, GetScaler(ResampleMode), GetRenderer(Mode), Color
End Sub

Public Sub TexturedQuad(ByVal Points, ByRef Texture, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal ResampleMode As SFXResampleModes = ResampleMode_Default)
On Error Resume Next
    TexturedConvexPolygon Points, Texture, Mode, ResampleMode
End Sub

Public Sub TileBlit(Optional ByVal DestRectangle As Fury2Rect = Nothing, Optional ByVal Source As Fury2Image = Nothing, Optional ByVal Alpha As Single = 1, Optional ByVal Mode As SFXBlitModes = BlitMode_Default, Optional ByVal Color As Long = &HFFFFFFFF)
On Error Resume Next
Dim l_rctDest As Fury2Rect, l_rctSource As Fury2Rect
Dim l_imgSource As Fury2Image
Dim l_lngColor As Long
Dim l_sngAlpha As Single
Dim l_lngAlpha As Long
Dim l_rctClip As Fury2Rect
    Set l_rctDest = DestRectangle
    Set l_imgSource = Source
    l_lngColor = Color
    l_sngAlpha = Alpha
    l_lngAlpha = l_sngAlpha * 255
    If Err <> 0 Then
        Err.Clear
        Exit Sub
    End If
    If l_imgSource Is Nothing Then Exit Sub
    If l_rctDest Is Nothing Then Set l_rctDest = Me.Rectangle
    If Mode = BlitMode_Default Then Mode = m_bmdBlitMode
    Select Case Mode
    Case SFXBlitModes.BlitMode_Normal
        SoftFX.Blit_Tile_Normal_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngAlpha
    Case SFXBlitModes.BlitMode_Matte
        SoftFX.Blit_Tile_Matte_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngAlpha
    Case SFXBlitModes.BlitMode_SourceAlpha
        SoftFX.Blit_Tile_SourceAlpha_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngAlpha
    Case SFXBlitModes.BlitMode_Additive
        SoftFX.Blit_Tile_Additive_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngAlpha
    Case SFXBlitModes.BlitMode_Subtractive
        SoftFX.Blit_Tile_Subtractive_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngAlpha
    Case SFXBlitModes.BlitMode_Multiply
        SoftFX.Blit_Tile_Multiply_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngAlpha
    Case SFXBlitModes.BlitMode_Matte_Tint
        SoftFX.Blit_Tile_Matte_Tint_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngColor, l_lngAlpha
    Case SFXBlitModes.BlitMode_SourceAlpha_Tint
        SoftFX.Blit_Tile_SourceAlpha_Tint_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngColor, l_lngAlpha
    Case SFXBlitModes.BlitMode_Merge
        SoftFX.Blit_Tile_Merge_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngAlpha
    Case SFXBlitModes.BlitMode_Font
        SoftFX.Blit_Tile_Font_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngColor, l_lngAlpha
    Case SFXBlitModes.BlitMode_Font_SourceAlpha
        SoftFX.Blit_Tile_Font_SourceAlpha_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngColor, l_lngAlpha
    Case SFXBlitModes.BlitMode_Font_Merge
        SoftFX.Blit_Tile_Font_Merge_Opacity m_lngImage, l_imgSource.Handle, l_rctDest.GetRectangle, l_lngColor, l_lngAlpha
    Case Else
    End Select
End Sub

Public Sub Unsize()
    If m_booNoDelete Then Exit Sub
    If Initialized Then SoftFX.UnsizeImage m_lngImage
End Sub

Public Sub UpdateClipRect()
    Set ClipRectangle = Rectangle
End Sub

