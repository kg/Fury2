VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2GEGlobal"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Public Enum ResamplingMethods
    [Linear] = 0
    [BiLinear] = 1
End Enum

Private CPUFreq As Currency
Private Const Black As Long = &HFF000000, White As Long = &HFFFFFFFF

Public Function PointInPolygon(ByVal PolyPoints, ByVal Point) As Boolean
Dim l_ptPoints() As FPoint
Dim l_plyPoly As GenericPolygon
Dim l_lngPoints As Long
Dim l_ptPoint As FPoint
    If (VarType(PolyPoints) And vbArray) <> vbArray Then Exit Function
    If (VarType(Point) And vbArray) <> vbArray Then Exit Function
    Err.Clear
    ReDim l_ptPoints(LBound(PolyPoints) To UBound(PolyPoints))
    For l_lngPoints = LBound(PolyPoints) To UBound(PolyPoints)
        If (VarType(PolyPoints(l_lngPoints)) And vbArray) <> vbArray Then Exit Function
        l_ptPoints(l_lngPoints).X = CSng(PolyPoints(l_lngPoints)(0))
        l_ptPoints(l_lngPoints).Y = CSng(PolyPoints(l_lngPoints)(1))
    Next l_lngPoints
    l_ptPoint.X = CSng(Point(0))
    l_ptPoint.Y = CSng(Point(1))
    l_plyPoly.VertexPointer = VarPtr(l_ptPoints(0))
    l_plyPoly.VertexCount = (UBound(PolyPoints) - LBound(PolyPoints)) + 1
    If SoftFX.PointInsidePolygon(l_plyPoly, l_ptPoint) = 1 Then
        PointInPolygon = True
    End If
End Function

Public Function F2SharpenFilter(Optional ByVal Strength As Long = 8) As Fury2ConvolutionFilter
On Error Resume Next
Dim l_lngCenterWeight As Long
    If Strength > 10 Then Strength = 10
    l_lngCenterWeight = 9 + (10 - Strength)
    Set F2SharpenFilter = New Fury2ConvolutionFilter
    F2SharpenFilter.SetValues Array( _
    0, -2, 0, _
    -2, l_lngCenterWeight, -2, _
    0, -2, 0)
    F2SharpenFilter.Divisor = -8 + l_lngCenterWeight
End Function

Public Function F2GaussianBlurFilter(Optional ByVal Radius As Single = 1) As Fury2ConvolutionFilter
On Error Resume Next
Dim l_sngX As Single, l_sngY As Single
Dim l_sngW As Single, l_sngXO As Single, l_sngYO As Single, l_sngM As Single
Dim l_sngXW As Single, l_sngYW As Single, l_sngD As Single, l_sngSum As Single
Dim l_sngWMul As Single
    Set F2GaussianBlurFilter = New Fury2ConvolutionFilter
    Call F2GaussianBlurFilter.Resize(Ceil(Radius) * 2 + 1, Ceil(Radius) * 2 + 1)
    l_sngM = F2GaussianBlurFilter.Width - 1
    l_sngXO = ((F2GaussianBlurFilter.Width - 1) / 2)
    l_sngYO = ((F2GaussianBlurFilter.Height - 1) / 2)
    l_sngWMul = 1 / (Sqr(2))
    For l_sngY = 0 To l_sngM
        l_sngYW = (l_sngY) / Radius
        If l_sngYW < 0 Then l_sngYW = 0
        If l_sngYW > 1 Then l_sngYW = 1 - (l_sngYW - 1)
        l_sngYW = l_sngYW * l_sngYW
        For l_sngX = 0 To l_sngM
            l_sngXW = (l_sngX) / Radius
            If l_sngXW < 0 Then l_sngXW = 0
            If l_sngXW > 1 Then l_sngXW = 1 - (l_sngXW - 1)
            l_sngXW = l_sngXW * l_sngXW
            l_sngW = Sqr(l_sngXW + l_sngYW) * l_sngWMul
            F2GaussianBlurFilter.Weight(CLng(l_sngX), CLng(l_sngY)) = l_sngW
            l_sngSum = l_sngSum + l_sngW
        Next l_sngX
    Next l_sngY
    F2GaussianBlurFilter.Divisor = l_sngSum
End Function

' P.S.: I'm a retard
Public Function F2BoxFilter(Optional ByVal Size As Long = 3) As Fury2ConvolutionFilter
    Set F2BoxFilter = F2AverageFilter
End Function

Public Function F2AverageFilter(Optional ByVal Size As Long = 3) As Fury2ConvolutionFilter
On Error Resume Next
    Set F2AverageFilter = New Fury2ConvolutionFilter
    F2AverageFilter.Resize Size, Size
    F2AverageFilter.SetAll 1
    F2AverageFilter.Divisor = Size ^ 2
End Function

Public Function F2BlurFilter(Optional ByVal Strength As Long = 6) As Fury2ConvolutionFilter
On Error Resume Next
Dim l_lngCenterWeight As Long
    If Strength > 10 Then Strength = 10
    l_lngCenterWeight = (10 - Strength)
    Set F2BlurFilter = New Fury2ConvolutionFilter
    F2BlurFilter.SetValues Array( _
    1, 2, 1, _
    2, l_lngCenterWeight, 2, _
    1, 2, 1)
    F2BlurFilter.Divisor = 12 + l_lngCenterWeight
End Function

Public Function GetGlyphBitmap(ByVal Font As StdFont, ByVal Character As Long) As Fury2Image
Attribute GetGlyphBitmap.VB_Description = "Returns an image generated of a character from a truetype font. Stores the following attributes in the image's Tag property: (GlyphOriginX, GlyphOriginY, BlackBoxX, BlackBoxY, CellIncX, CellIncY, MaximumCharWidth, MaximumCharHeight)"
On Error Resume Next
Dim m_lngPalette(0 To 255) As Long
Dim m_lngColors As Long
Dim m_matTransform As Mat2
Dim m_gmMetrics As GlyphMetrics
Dim m_bytData() As Byte
Dim m_imgChar As Fury2Image
Dim m_lngSize As Long, m_lngWidth As Long
Dim m_lngDC As Long
Dim m_tmMetric As TEXTMETRIC
Dim m_frmText As frmTextRender
    Set m_frmText = New frmTextRender
    Load m_frmText
    Set m_frmText.Font = Font
    For m_lngColors = 0 To 66
        m_lngPalette(m_lngColors) = F2RGB(255, 255, 255, (m_lngColors * 255) \ 65)
    Next m_lngColors
    m_matTransform.H11 = 1
    m_matTransform.H22 = 1
    m_lngDC = m_frmText.hdc
    m_lngSize = GetGlyphOutline(m_lngDC, Character, GGO_Metrics, m_gmMetrics, Len(m_gmMetrics), 0, VarPtr(m_matTransform))
    m_lngSize = GetGlyphOutline(m_lngDC, Character, GGO_Gray8_Bitmap, m_gmMetrics, 0, 0, VarPtr(m_matTransform))
    Call GetTextMetrics(m_lngDC, m_tmMetric)
    If m_lngSize <= 1 Then
        Set m_imgChar = F2Image(0, 0)
    Else
        ReDim m_bytData(0 To (m_lngSize) - 1)
        Call GetGlyphOutline(m_lngDC, Character, GGO_Gray8_Bitmap, m_gmMetrics, m_lngSize, VarPtr(m_bytData(0)), VarPtr(m_matTransform))
        m_lngWidth = ((m_gmMetrics.BlackBoxX + 3) \ 4) * 4
        If m_lngWidth <= 0 Then Exit Function
        Set m_imgChar = F2Image(m_lngWidth, (m_lngSize \ m_lngWidth))
        SoftFX.Filter_Depalletize m_imgChar.Handle, m_imgChar.Rectangle.GetRectangle, VarPtr(m_bytData(0)), VarPtr(m_lngPalette(0)), ClipValue(m_lngSize, 0, m_imgChar.Width * m_imgChar.Height)
    End If
    With m_gmMetrics
        m_imgChar.Tag = Array(.GlyphOrigin.X, .GlyphOrigin.Y, .BlackBoxX, .BlackBoxY, .CellIncX, .CellIncY, m_tmMetric.tmMaxCharWidth, m_tmMetric.tmHeight)
    End With
    m_imgChar.Name = ChrW(Character)
    Unload m_frmText
    Set m_frmText = Nothing
    Set GetGlyphBitmap = m_imgChar
End Function

Private Function CheckExt(ByRef Fn As String, ByVal Ext As String) As Boolean
On Error Resume Next
Dim m_lngLength As Long
    Err.Clear
    m_lngLength = FileLen(Fn + "." + Ext)
    If (Err = 0) And (m_lngLength > 0) Then
        Fn = Fn + "." + Ext
        CheckExt = True
    Else
        CheckExt = False
    End If
End Function

Public Function Floor(ByVal Value As Single) As Long
On Error Resume Next
    Floor = SoftFX.Floor(Value)
End Function

Public Function Ceil(ByVal Value As Single) As Long
On Error Resume Next
    Ceil = SoftFX.Ceil(Value)
End Function

Public Function ClipNumber(Value As Long, Min As Long, Max As Long) As Long
    ClipNumber = ClipValue(Value, Min, Max)
End Function

Public Function F2File(Optional Filename As String = "") As VirtualFile
On Error Resume Next
    Set F2File = New VirtualFile
    If Filename <> "" Then F2File.LoadFile Filename
End Function

Public Function F2FileFromString(ByRef Data As String) As VirtualFile
On Error Resume Next
    Set F2FileFromString = New VirtualFile
    F2FileFromString.LoadFromString Data
End Function

Public Sub F2UnRGB(Color, ByRef RedVariable, ByRef GreenVariable, ByRef BlueVariable, Optional ByRef AlphaVariable)
On Error Resume Next
Dim m_lngColor As Long
    RedVariable = SoftFX.GetRed(Color)
    GreenVariable = SoftFX.GetGreen(Color)
    BlueVariable = SoftFX.GetBlue(Color)
    If IsMissing(AlphaVariable) Then
    Else
        AlphaVariable = SoftFX.GetAlpha(Color)
    End If
End Sub

Public Sub F2UnHSB(Color, ByRef HueVariable, ByRef SaturationVariable, ByRef BrightnessVariable, Optional ByRef AlphaVariable)
'On Error Resume Next
'Dim m_lngColor As Long
'Dim m_lngHue As Long, m_lngSaturation As Long, m_lngBrightness As Long
'Dim m_bytRed As Byte, m_bytGreen As Byte, m_bytBlue As Byte
'    m_bytRed = GetRed(Color)
'    m_bytGreen = GetGreen(Color)
'    m_bytBlue = GetBlue(Color)
'    RGBtoHSB m_bytRed, m_bytGreen, m_bytBlue, m_lngHue, m_lngSaturation, m_lngBrightness
'    HueVariable = CSng(m_lngHue) / 4096!
'    SaturationVariable = (CSng(m_lngSaturation) / 4096!) * 360!
'    BrightnessVariable = (CSng(m_lngBrightness) / 4096!) * 360!
'    If IsMissing(AlphaVariable) Then
'    Else
'        AlphaVariable = GetAlpha(Color)
'    End If
End Sub

Public Function F2MixColors(ByVal Dest As Long, ByVal Source As Long, Optional ByVal Alpha As Single = 0.5) As Long
On Error Resume Next
    F2MixColors = SoftFX.BlendColors(Dest, Source, Alpha * 255)
End Function

Public Function F2MultiplyColor(ByVal Color As Long, ByVal Mul As Long, Optional ByVal Alpha As Single = 1) As Long
On Error Resume Next
    F2MultiplyColor = SoftFX.MultiplyColor(Color, Mul, Alpha * 255)
End Function

Public Sub F2SwapRedBlue(ByRef Color As Long)
    Color = SoftFX.SwapChannels(Color, 0, 2)
End Sub

Public Sub F2Invert(ByRef Color As Long, Optional ByVal WithAlpha As Boolean)
'    If WithAlpha Then
'        Color = Invert(Color)
'    Else
'        Color = InvertRGB(Color)
'    End If
End Sub

Public Function F2FromGDIColor(ByVal Color As Long) As Long
    F2FromGDIColor = SoftFX.SetAlpha(SoftFX.SwapChannels(Color, 0, 2), 255)
End Function

Public Sub F2SetRed(ByRef Color, ByVal NewValue As Long)
    Color = SoftFX.SetRed(Color, NewValue)
End Sub

Public Sub F2SetGreen(ByRef Color, ByVal NewValue As Long)
    Color = SoftFX.SetGreen(Color, NewValue)
End Sub

Public Sub F2SetBlue(ByRef Color, ByVal NewValue As Long)
    Color = SoftFX.SetBlue(Color, NewValue)
End Sub

Public Sub F2SetAlpha(ByRef Color, ByVal NewValue As Long)
    Color = SoftFX.SetAlpha(Color, NewValue)
End Sub

Public Sub F2SetGray(ByRef Color, ByVal NewValue As Long)
    Color = SoftFX.SetGray(Color, NewValue)
End Sub

Public Function F2GetRed(Color) As Long
    F2GetRed = SoftFX.GetRed(CLng(Color))
End Function

Public Function F2GetGreen(Color) As Long
    F2GetGreen = SoftFX.GetGreen(CLng(Color))
End Function

Public Function F2GetBlue(Color) As Long
    F2GetBlue = SoftFX.GetBlue(CLng(Color))
End Function

Public Function F2GetAlpha(Color) As Long
    F2GetAlpha = SoftFX.GetAlpha(CLng(Color))
End Function

Public Function F2GetGray(Color) As Long
    F2GetGray = SoftFX.GetGray(CLng(Color))
End Function

' Deprecated
Public Function GetColorAlpha(Color) As Long
Attribute GetColorAlpha.VB_MemberFlags = "40"
    GetColorAlpha = SoftFX.GetAlpha(CLng(Color))
End Function
Public Function GetColorRed(Color) As Long
Attribute GetColorRed.VB_MemberFlags = "40"
    GetColorRed = SoftFX.GetRed(CLng(Color))
End Function
Public Function GetColorGreen(Color) As Long
Attribute GetColorGreen.VB_MemberFlags = "40"
    GetColorGreen = SoftFX.GetGreen(CLng(Color))
End Function
Public Function GetColorBlue(Color) As Long
Attribute GetColorBlue.VB_MemberFlags = "40"
    GetColorBlue = SoftFX.GetBlue(CLng(Color))
End Function

Public Function GraphicsFormatSupported(Format As String) As Boolean
On Error Resume Next
    Select Case Trim(LCase(Format))
    Case "bmp", "gif", "jpg", "jpeg", "png", "ico", "cur", "wmf", "emf", "tga", "pcx", "pict", "tif", "tiff", "pgm", "ppm", "eps", "chr", "tst", "fnt", "f2g", "rfn", "rws", "tle", "dib", "rle", "xyz", "vsp", "rts"
        GraphicsFormatSupported = True
    Case Else
    End Select
End Function

Public Function SupportedGraphicsFormats() As String
    SupportedGraphicsFormats = "*.bmp;*.gif;*.jpg;*.jpeg;*.png;*.tga;*.pcx;*.ppm;*.chr;*.tst;*.fnt;*.f2g;*.rfn;*.rws;*.tle;*.dib;*.rle;*.xyz;*.vsp;*.rts"
End Function

Public Function DesktopDC() As Long
    DesktopDC = GetDC(0)
End Function

Public Function HiResTimer() As Double
Dim PCounter As Currency
    Call QueryPerformanceCounter(PCounter)
    HiResTimer = CDbl(PCounter) / CPUFreq
End Function

Public Function F2Init()
On Error Resume Next
Dim cFreq As Currency
    Call QueryPerformanceFrequency(cFreq)
    CPUFreq = CDbl(cFreq)
    Load frmTextRender
    SoftFX.Initialize
    SoftFX.SetCallbacks AddressOf CreateDIBSection_, AddressOf DeleteObject_
    F2Init = True
End Function

Public Function F2Shutdown()
On Error Resume Next
    SoftFX.UnInitialize
    Unload frmTextRender
    F2Shutdown = True
End Function

Public Function F2Initialized() As Boolean
On Error Resume Next
    F2Initialized = CBool(SoftFX.GetInitialized)
End Function

Public Function StripAlpha(Color As Long) As Long
    StripAlpha = SoftFX.SetAlpha(Color, 0)
End Function

Public Function AddAlpha(Color As Long) As Long
    AddAlpha = SoftFX.SetAlpha(Color, 255)
End Function

Public Function F2Rect(x1, y1, x2, y2, Optional Absolute = True) As Fury2Rect
On Error Resume Next
Dim NewRect As Fury2Rect
    Set NewRect = New Fury2Rect
    If Absolute Then
        NewRect.SetValues CLng(x1), CLng(y1), CLng(x2), CLng(y2)
    Else
        NewRect.SetValues CLng(x1), CLng(y1), CLng(x1 + x2), CLng(y1 + y2)
    End If
    Set F2Rect = NewRect
    Set NewRect = Nothing
End Function
'
'Public Function F2LoadFNT(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadFNT CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadFNT = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadGPH(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadGPH CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadGPH = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadTST(Filename, Optional RowWidth As Long = 9) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadTST CStr(Filename), RowWidth
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadTST = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadPPM(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadPPM CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadPPM = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadCHR(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadCHR CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadCHR = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadICO(Filename, Optional Size As Long = 32) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    Set newImage.Picture = LoadIconObject(CStr(Filename), Size)
'    newImage.Name = Filename
'    Set F2LoadICO = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadPNG(Filename) As Fury2Image
'On Error Resume Next
'    Set F2LoadPNG = F2LoadPicture(Filename)
'End Function
'
'Public Function F2LoadVSP(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadVSP CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadVSP = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadXYZ(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadXYZ CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadXYZ = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadRFN(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadRFN CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadRFN = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadRWS(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadRWS CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadRWS = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadRTS(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadRTS CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadRTS = newImage
'    Set newImage = Nothing
'End Function
'
'Public Function F2LoadF2G(Filename) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadF2G CStr(Filename)
'    newImage.Name = Filename
'    If newImage.Width <= 0 Or newImage.Height <= 0 Then Set newImage = Nothing
'    Set F2LoadF2G = newImage
'    Set newImage = Nothing
'End Function

Public Function AutoSelectExtension(Filename) As String
On Error Resume Next
Dim m_strFn As String
    m_strFn = CStr(Filename)
    If InStr(m_strFn, ".") Then AutoSelectExtension = m_strFn: Exit Function
    If CheckExt(m_strFn, "png") Then AutoSelectExtension = m_strFn: Exit Function
    If CheckExt(m_strFn, "f2g") Then AutoSelectExtension = m_strFn: Exit Function
    If CheckExt(m_strFn, "tga") Then AutoSelectExtension = m_strFn: Exit Function
    If CheckExt(m_strFn, "bmp") Then AutoSelectExtension = m_strFn: Exit Function
    If CheckExt(m_strFn, "jpg") Then AutoSelectExtension = m_strFn: Exit Function
    If CheckExt(m_strFn, "gif") Then AutoSelectExtension = m_strFn: Exit Function
    AutoSelectExtension = m_strFn
End Function

Public Property Get F2LockingMode() As SFXLockingModes
On Error Resume Next
    F2LockingMode = SoftFX.GetLockingMode
End Property

Public Property Let F2LockingMode(ByVal NewMode As SFXLockingModes)
On Error Resume Next
    SoftFX.SetLockingMode CLng(NewMode)
End Property

Public Function F2LoadImage(Filename) As Fury2Image
On Error Resume Next
Dim Extension As String
Dim m_strFilename As String
Dim m_varParameters As Variant
Dim m_varTokens As Variant
Dim m_lngParameter As Long
Dim l_imgLayer As Fury2Image
    m_strFilename = Replace(Filename, "/", "\")
    Err.Clear
    If InStr(m_strFilename, "%") Then
        m_varParameters = Split(Mid(m_strFilename, InStrRev(m_strFilename, "%") + 1), ";")
        m_strFilename = Left(m_strFilename, InStrRev(m_strFilename, "%") - 1)
    Else
        m_varParameters = Array("")
    End If
    If InStr(m_strFilename, ".") Then
    Else
        If FileLen(m_strFilename) <= 0 Then m_strFilename = AutoSelectExtension(m_strFilename)
        If FileLen(m_strFilename) <= 0 Then Exit Function
    End If
    If InStr(m_strFilename, ":") Then
    Else
        m_strFilename = CurDir + IIf(Right(CurDir, 1) = "\", "", "\") + IIf(Left(m_strFilename, 1) = "\", Mid(m_strFilename, 2), m_strFilename)
    End If
    Extension = Mid(CStr(m_strFilename), InStrRev(CStr(m_strFilename), ".") + 1)
    Select Case LCase(Trim(Extension))
    Case "pcx", "dib", "rle", "bmp", "jpg", "gif", "jpeg"
        Set F2LoadImage = F2LoadPicture(m_strFilename)
        F2LoadImage.AlphaChannel = False
    Case "png", "tga"
        Set F2LoadImage = F2LoadPicture(m_strFilename)
    End Select
    For m_lngParameter = LBound(m_varParameters) To UBound(m_varParameters)
        If InStr(m_varParameters(m_lngParameter), "(") Then
            m_varTokens = Split(m_varParameters(m_lngParameter), "(")
            If Right(m_varTokens(1), 1) = ")" Then m_varTokens(1) = Left(m_varTokens(1), Len(m_varTokens(1)) - 1)
            Select Case LCase(Trim(m_varTokens(0)))
            Case "layer"
                Set l_imgLayer = F2LoadImage(m_varTokens(1))
                If l_imgLayer Is Nothing Then
                Else
                    If l_imgLayer.AlphaChannel Then
                        F2LoadImage.Blit , , l_imgLayer, , BlitMode_SourceAlpha
                    Else
                        F2LoadImage.Blit , , l_imgLayer, , BlitMode_Normal
                    End If
                End If
            Case "invert"
                If CBool(m_varTokens(1)) Then
                    F2LoadImage.Invert
                Else
                    F2LoadImage.InvertRGB
                End If
            Case "grayscale", "greyscale", "gray", "grey"
                F2LoadImage.Grayscale
            Case "generatealpha"
                F2LoadImage.GenerateMask
            Case "adjust"
                F2LoadImage.Adjust CLng(m_varTokens(1)), F2LoadImage.Rectangle
            Case "maskcolor", "mattecolor", "mask", "matte"
                F2LoadImage.MatteColor = CLng(m_varTokens(1))
            Case "replacecolor", "replace"
                F2LoadImage.ReplaceColor CLng(m_varTokens(1)), CLng(m_varTokens(2)), F2LoadImage.Rectangle
            Case "composite"
                F2LoadImage.Composite CLng(m_varTokens(1))
            Case "gamma"
                F2LoadImage.AdjustGamma CLng(m_varTokens(1))
            Case "flip"
                F2LoadImage.Flip
            Case "mirror"
                F2LoadImage.Mirror
            Case "rotate"
                F2LoadImage.Rotate CSng(m_varTokens(1))
            Case Else
            End Select
        End If
    Next m_lngParameter
        If F2LoadImage Is Nothing Then
        Else
                F2LoadImage.Name = m_strFilename
        End If
End Function
'
'Public Function F2LoadPalettedImage(filename) As Fury2PalettedImage
'On Error Resume Next
'Dim Extension As String
'Dim m_strFilename As String
'Dim m_varParameters As Variant
'Dim m_varTokens As Variant
'Dim m_lngParameter As Long
'    m_strFilename = Replace(filename, "/", "\")
'    Err.Clear
'    If InStr(m_strFilename, "%") Then
'        m_varParameters = Split(Mid(m_strFilename, InStrRev(m_strFilename, "%") + 1), ";")
'        m_strFilename = left(m_strFilename, InStrRev(m_strFilename, "%") - 1)
'    Else
'        m_varParameters = Array("")
'    End If
'    If InStr(m_strFilename, ".") Then
'    Else
'        If FileLen(m_strFilename) <= 0 Then m_strFilename = AutoSelectExtension(m_strFilename)
'        If FileLen(m_strFilename) <= 0 Then Exit Function
'    End If
'    If InStr(m_strFilename, ":") Then
'    Else
'        m_strFilename = CurDir + IIf(Right(CurDir, 1) = "\", "", "\") + IIf(left(m_strFilename, 1) = "\", Mid(m_strFilename, 2), m_strFilename)
'    End If
'    Extension = Mid(CStr(m_strFilename), InStrRev(CStr(m_strFilename), ".") + 1)
'    Select Case LCase(Trim(Extension))
'    Case "pcx", "bmp", "jpg", "gif", "jpeg", "png", "tga"
'        Set F2LoadPalettedImage = New Fury2PalettedImage
'        F2LoadPalettedImage.LoadBitmap m_strFilename
'    Case Else
'    End Select
'    F2LoadPalettedImage.Name = m_strFilename
'End Function

Public Function F2LoadPicture(Filename) As Fury2Image
On Error Resume Next
Dim newImage As Fury2Image
    Err.Clear
    If FileLen(Filename) <= 0 Then Exit Function
    If Err <> 0 Then Exit Function
    Set newImage = New Fury2Image
    newImage.LoadFile Filename
    Set F2LoadPicture = newImage
    Set newImage = Nothing
    Err.Clear
End Function

Public Function F2Image(ByVal X As Long, ByVal Y As Long) As Fury2Image
On Error Resume Next
Dim newImage As Fury2Image
    Set newImage = New Fury2Image
    newImage.Resize X, Y
    Set F2Image = newImage
    Set newImage = Nothing
End Function

Public Function F2DIBSection(ByVal X As Long, ByVal Y As Long, ByVal DC As Long) As Fury2Image
On Error Resume Next
Dim newImage As Fury2Image
    Set newImage = New Fury2Image
    newImage.AllocateDIBSection X, Y, DC
    Set F2DIBSection = newImage
    Set newImage = Nothing
End Function

'Public Function F2PalettedImage(ByVal x As Long, ByVal y As Long) As Fury2PalettedImage
'On Error Resume Next
'Dim newImage As Fury2PalettedImage
'    Set newImage = New Fury2PalettedImage
'    newImage.Resize x, y
'    Set F2PalettedImage = newImage
'    Set newImage = Nothing
'End Function

Public Function F2RGB(ByVal Red, ByVal Green, ByVal Blue, Optional ByVal Alpha = 255) As Long
On Error Resume Next
    F2RGB = BGRA(CLng(Red), CLng(Green), CLng(Blue), CLng(Alpha))
End Function

'Public Function F2HSB(ByVal Hue, ByVal Saturation, ByVal Brightness, Optional ByVal Alpha = 255) As Fury2Color
'On Error Resume Next
'Dim NewColor As Fury2Color
'Dim m_lngHue As Long, m_lngSaturation As Long, m_lngBrightness As Long
'Dim m_bytRed As Byte, m_bytGreen As Byte, m_bytBlue As Byte
'    Set NewColor = New Fury2Color
'    Set F2HSB = NewColor
'    m_lngHue = CSng(Hue) * 4096!
'    m_lngSaturation = (CSng(Saturation) / 360!) * 4096!
'    m_lngBrightness = (CSng(Brightness) / 360!) * 4096!
'    HSBtoRGB m_lngHue, m_lngSaturation, m_lngBrightness, m_bytRed, m_bytGreen, m_bytBlue
'    NewColor.BGRAColor = BGRA(CLng(m_bytRed), CLng(m_bytGreen), CLng(m_bytBlue), CLng(Alpha))
'    Set NewColor = Nothing
'End Function

'Public Function F2ImageFromDC(ByVal hdc As Long, SourceRect As Fury2Rect) As Fury2Image
'On Error Resume Next
'Dim newImage As Fury2Image, newPic As IPictureDisp
'    Set newImage = New Fury2Image
'    newImage.Resize SourceRect.Width, SourceRect.Height
'    Set newPic = CapturePicture(CLng(hdc), CLng(SourceRect.left), CLng(SourceRect.top), CLng(SourceRect.Width), CLng(SourceRect.Height))
'    Set newImage.Picture = newPic
'    Set newPic = Nothing
'    Set F2ImageFromDC = newImage
'    Set newImage = Nothing
'End Function

Public Function F2ImageFromPicture(ByRef Picture As IPictureDisp) As Fury2Image
On Error Resume Next
Dim newImage As Fury2Image
    Set newImage = New Fury2Image
    Set newImage.Picture = Picture
    Set F2ImageFromPicture = newImage
    Set newImage = Nothing
End Function

Public Sub CopyImageToDC(ByVal hdc As Long, DestRect As Fury2Rect, SourceImage As Fury2Image)
On Error Resume Next
Dim bitDesc As BitmapInfo
    If SourceImage.Initialized Then Else Exit Sub
    With bitDesc.Header
        .Size = Len(bitDesc.Header)
        .Planes = 1
        .BitCount = 32
        .Width = SourceImage.Width
        .Height = -(SourceImage.Height)
    End With
    StretchDIBits hdc, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, 0, 0, DestRect.Width, DestRect.Height, SourceImage.Pointer(0, 0), bitDesc, 0, vbSrcCopy
End Sub

'Public Sub CopyPalettedImageToDC(ByVal hdc As Long, DestRect As Fury2Rect, SourceImage As Fury2PalettedImage)
'On Error Resume Next
'    Err.Raise 12345, "CopyPalettedImageToDC", "Unsupported, sorry"
'End Sub

Public Sub CopyImageToDCAlphaComposite(ByVal hdc As Long, DestRect As Fury2Rect, SourceImage As Fury2Image, Optional ByVal BackColor As Long = -12558208)
On Error Resume Next
Dim m_imgBuffer As Fury2Image
    Set m_imgBuffer = F2Image(SourceImage.Width, SourceImage.Height)
    m_imgBuffer.Clear BackColor
    m_imgBuffer.Blit m_imgBuffer.Rectangle, SourceImage.Rectangle, SourceImage, 1, BlitMode_SourceAlpha
    CopyImageToDC hdc, DestRect, m_imgBuffer
    Set m_imgBuffer = Nothing
End Sub

Public Sub DrawImageToDC(ByVal hdc As Long, DestRect As Fury2Rect, SourceRect As Fury2Rect, SourceImage As Fury2Image)
On Error Resume Next
Dim bitDesc As BitmapInfo
    If SourceImage.Initialized Then Else Exit Sub
    With bitDesc.Header
        .Size = Len(bitDesc.Header)
        .Planes = 1
        .BitCount = 32
        .Width = SourceImage.Width
        .Height = (SourceImage.Height)
    End With
    StretchDIBits hdc, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, SourceRect.Left, SourceRect.Bottom + 1, SourceRect.Width, -SourceRect.Height, SourceImage.Pointer(0, 0), bitDesc, 0, vbSrcCopy
End Sub

Public Sub DrawImageToDCAlphaComposite(ByVal hdc As Long, DestRect As Fury2Rect, SourceRect As Fury2Rect, SourceImage As Fury2Image, Optional ByVal BackColor As Long = -12558208)
On Error Resume Next
Dim m_imgBuffer As Fury2Image
    Set m_imgBuffer = F2Image(SourceRect.Width, SourceRect.Height)
    m_imgBuffer.Clear BackColor
    m_imgBuffer.Blit m_imgBuffer.Rectangle, SourceRect, SourceImage, 1, BlitMode_SourceAlpha
    DrawImageToDC hdc, DestRect, SourceRect, m_imgBuffer
    Set m_imgBuffer = Nothing
End Sub

'Public Sub DrawImageToDCBP(ByVal hdc As Long, DestRect As Fury2Rect, SourceRect As Fury2Rect, SourceImage As Fury2Image, BPP As Long)
'On Error Resume Next
'Dim bitDesc As BitmapInfo
'    If SourceImage.Initialized Then Else Exit Sub
'    With bitDesc.Header
'        .Size = Len(bitDesc.Header)
'        .Planes = 1
'        .BitCount = BPP
'        .Compression = BI_RGB
'        .Width = SourceImage.Width
'        .Height = (SourceImage.Height)
'    End With
'    StretchDIBits hdc, DestRect.left, DestRect.top, DestRect.Width, DestRect.Height, SourceRect.left, SourceRect.Bottom + 1, SourceRect.Width, -SourceRect.Height, SourceImage.Pointer(0, 0), bitDesc, DIB_RGB_COLORS, vbSrcCopy
'End Sub

Public Sub DrawImageToDCEx(ByVal hdc As Long, DestRect As Fury2Rect, SourceRect As Fury2Rect, SourceImage As Fury2Image, RasterOp As Long)
On Error Resume Next
Dim bitDesc As BitmapInfo
    If SourceImage.Initialized Then Else Exit Sub
    With bitDesc.Header
        .Size = Len(bitDesc.Header)
        .Planes = 1
        .BitCount = 32
        .Compression = 0
        .Width = SourceImage.Width
        .Height = (SourceImage.Height)
    End With
    StretchDIBits hdc, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, SourceRect.Left, SourceRect.Bottom + 1, SourceRect.Width, -SourceRect.Height, SourceImage.Pointer(0, 0), bitDesc, 0, RasterOp
End Sub

Public Function F2White() As Long
    F2White = White
End Function

Public Function F2Black() As Long
    F2Black = Black
End Function

Public Function RealColor(Color As Long) As Long
On Error Resume Next
    If Color And &H80000000 = &H80000000 Then
        RealColor = SoftFX.SwapChannels(GetSystemColor(Color And (Not &H80000000)), Red, Blue)
    Else
        RealColor = SoftFX.SwapChannels(Color, Red, Blue)
    End If
End Function

