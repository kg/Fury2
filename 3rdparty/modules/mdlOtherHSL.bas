Attribute VB_Name = "ModHSL"
'Comes from PSC - a submission called ColorLab
'sorry dont remember the author

Public Const HSLMAX As Integer = 240
Const RGBMAX As Integer = 255
Const UNDEFINED As Integer = (HSLMAX * 2 / 3)
Public Type HSLCol
    Hue As Integer
    Sat As Integer
    Lum As Integer
End Type
Public Function RGBtoHSL(RGBCol As Long) As HSLCol
Dim R As Integer, G As Integer, b As Integer
Dim cMax As Integer, cMin As Integer
Dim RDelta As Double, GDelta As Double, _
    BDelta As Double
Dim H As Double, s As Double, L As Double
Dim cMinus As Long, cPlus As Long
    R = RGBRed(RGBCol)
    G = RGBGreen(RGBCol)
    b = RGBBlue(RGBCol)
    cMax = iMax(iMax(R, G), b)
    cMin = iMin(iMin(R, G), b)
    cMinus = cMax - cMin
    cPlus = cMax + cMin
    L = ((cPlus * HSLMAX) + RGBMAX) / (2 * RGBMAX)
    If cMax = cMin Then
        s = 0
        H = 0
    Else
        If L <= (HSLMAX / 2) Then
            s = ((cMinus * HSLMAX) + 0.5) / cPlus
        Else
            s = ((cMinus * HSLMAX) + 0.5) / (2 * RGBMAX - cPlus)
        End If
        RDelta = (((cMax - R) * (HSLMAX / 6)) + 0.5) / cMinus
        GDelta = (((cMax - G) * (HSLMAX / 6)) + 0.5) / cMinus
        BDelta = (((cMax - b) * (HSLMAX / 6)) + 0.5) / cMinus
        Select Case cMax
            Case CLng(R)
                H = BDelta - GDelta
            Case CLng(G)
                H = (HSLMAX / 3) + RDelta - BDelta
            Case CLng(b)
                H = ((2 * HSLMAX) / 3) + GDelta - RDelta
        End Select
        If H < 0 Then H = H + HSLMAX
    End If
    RGBtoHSL.Hue = CInt(H)
    RGBtoHSL.Lum = CInt(L)
    RGBtoHSL.Sat = CInt(s)
End Function
Public Function HSLtoRGB(HueLumSat As HSLCol) As Long
    Dim R As Double, G As Double, b As Double
    Dim H As Double, L As Double, s As Double
    Dim Magic1 As Double, Magic2 As Double
    H = HueLumSat.Hue
    L = HueLumSat.Lum
    s = HueLumSat.Sat
    If CInt(s) = 0 Then
        R = (L * RGBMAX) / HSLMAX
        G = R
        b = R
        If CInt(H) <> UNDEFINED Then
        End If
    Else
        If L <= HSLMAX / 2 Then
            Magic2 = (L * (HSLMAX + s) + 0.5) / HSLMAX
        Else
            Magic2 = L + s - ((L * s) + 0.5) / HSLMAX
        End If
        Magic1 = 2 * L - Magic2
        R = (HuetoRGB(Magic1, Magic2, H + (HSLMAX / 3)) _
            * RGBMAX + 0.5) / HSLMAX
        G = (HuetoRGB(Magic1, Magic2, H) * RGBMAX + 0.5) / HSLMAX
        b = (HuetoRGB(Magic1, Magic2, H - (HSLMAX / 3)) _
            * RGBMAX + 0.5) / HSLMAX
    End If
    If R < 0 Then R = 0
    If G < 0 Then G = 0
    If b < 0 Then b = 0
    If R > 255 Then R = 255
    If G > 255 Then G = 255
    If b > 255 Then b = 255
    HSLtoRGB = RGB(CInt(R), CInt(G), CInt(b))
End Function
Private Function HuetoRGB(mag1 As Double, mag2 As Double, _
    ByVal Hue As Double) As Double
    If Hue < 0 Then
        Hue = Hue + HSLMAX
    ElseIf Hue > HSLMAX Then
        Hue = Hue - HSLMAX
    End If
    Select Case Hue
        Case Is < (HSLMAX / 6)
            HuetoRGB = (mag1 + (((mag2 - mag1) * Hue + _
                (HSLMAX / 12)) / (HSLMAX / 6)))
        Case Is < (HSLMAX / 2)
            HuetoRGB = mag2
        Case Is < (HSLMAX * 2 / 3)
            HuetoRGB = (mag1 + (((mag2 - mag1) * _
                ((HSLMAX * 2 / 3) - Hue) + _
                (HSLMAX / 12)) / (HSLMAX / 6)))
        Case Else
            HuetoRGB = mag1
    End Select
End Function
Public Function RGBRed(RGBCol As Long) As Integer
    RGBRed = RGBCol And &HFF
End Function
Public Function RGBGreen(RGBCol As Long) As Integer
    RGBGreen = ((RGBCol And &H100FF00) / &H100)
End Function
Public Function RGBBlue(RGBCol As Long) As Integer
    RGBBlue = (RGBCol And &HFF0000) / &H10000
End Function
Private Function iMax(a As Integer, b As Integer) _
    As Integer
    iMax = IIf(a > b, a, b)
End Function
Private Function iMin(a As Integer, b As Integer) _
    As Integer
    iMin = IIf(a < b, a, b)
End Function
Public Function ContrastingColor(RGBCol As Long) As Long
Dim HSL As HSLCol
    HSL = RGBtoHSL(RGBCol)
    If HSL.Lum > HSLMAX / 2 Then ContrastingColor = 0 _
        Else: ContrastingColor = &HFFFFFF
End Function

