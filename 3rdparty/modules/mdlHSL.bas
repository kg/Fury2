Attribute VB_Name = "mdlHSL"
'**************************************
'Windows API/Global Declarations for :HS
'     L<->RGB and Color Manipulation Rou
'     tines
'**************************************
Option Explicit
'
' modHSL.bas
' HSL/RGB + Color Manipulation routines
'
'Portions of this code marked with *** a
'     re converted from
'C/C++ routines for RGB/HSL conversion f
'     ound in the
'Microsoft Knowledge Base (PD sample cod
'     e):
'http://support.microsoft.com/support/kb
'     /articles/Q29/2/40.asp
'In addition to the language conversion,
'     some internal
'calculations have been modified and con
'     verted to FP math to
'reduce rounding errors.
'Conversion to VB and original code by
'Dan Redding (bwsoft@revealed.net)
'http://home.revealed.net/bwsoft
'Free to use, please give proper credit
Const HSLMAX As Integer = 240 '***
'H, S and L values can be 0 - HSLMAX
'240 matches what is used by MS Win;
'any number less than 1 byte is OK;
'works best if it is evenly divisible by
'     6
Const RGBMAX As Integer = 255 '***
'R, G, and B value can be 0 - RGBMAX
Const UNDEFINED As Integer = (HSLMAX * 2 / 3) '***
'Hue is undefined if Saturation = 0 (gre
'     yscale)

Public Type HSLCol 'Datatype used To pass HSL Color values
    Hue As Integer
    Sat As Integer
    Lum As Integer
End Type


Public Function RGBRed(RGBCol As Long) As Integer
    'Return the Red component from an RGB Co
    '     lor
    RGBRed = RGBCol And &HFF
End Function


Public Function RGBGreen(RGBCol As Long) As Integer
    'Return the Green component from an RGB
    '     Color
    RGBGreen = ((RGBCol And &H100FF00) / &H100)
End Function


Public Function RGBBlue(RGBCol As Long) As Integer
    'Return the Blue component from an RGB C
    '     olor
    RGBBlue = (RGBCol And &HFF0000) / &H10000
End Function


Private Function iMax(a As Integer, B As Integer) As Integer
    'Return the Larger of two values
    iMax = IIf(a > B, a, B)
End Function


Private Function iMin(a As Integer, B As Integer) As Integer
    'Return the smaller of two values
    iMin = IIf(a < B, a, B)
End Function


Public Function RGBtoHSL(RGBCol As Long) As HSLCol '***
    'Returns an HSLCol datatype containing H
    '     ue, Luminescence
    'and Saturation; given an RGB Color valu
    '     e
    Dim R As Integer, G As Integer, B As Integer
    Dim cMax As Integer, cMin As Integer
    Dim RDelta As Double, GDelta As Double, _
    BDelta As Double
    Dim H As Double, S As Double, L As Double
    Dim cMinus As Long, cPlus As Long
    
    R = RGBRed(RGBCol)
    G = RGBGreen(RGBCol)
    B = RGBBlue(RGBCol)
    
    cMax = iMax(iMax(R, G), B) 'Highest and lowest
    cMin = iMin(iMin(R, G), B) 'color values
    
    cMinus = cMax - cMin 'Used To simplify the
    cPlus = cMax + cMin 'calculations somewhat.
    
    'Calculate luminescence (lightness)
    L = ((cPlus * HSLMAX) + RGBMAX) / (2 * RGBMAX)
    


    If cMax = cMin Then 'achromatic (r=g=b, greyscale)
        S = 0 'Saturation 0 For greyscale
        H = UNDEFINED 'Hue undefined For greyscale
    Else
        'Calculate color saturation


        If L <= (HSLMAX / 2) Then
            S = ((cMinus * HSLMAX) + 0.5) / cPlus
        Else
            S = ((cMinus * HSLMAX) + 0.5) / (2 * RGBMAX - cPlus)
        End If
        
        'Calculate hue
        RDelta = (((cMax - R) * (HSLMAX / 6)) + 0.5) / cMinus
        GDelta = (((cMax - G) * (HSLMAX / 6)) + 0.5) / cMinus
        BDelta = (((cMax - B) * (HSLMAX / 6)) + 0.5) / cMinus
        


        Select Case cMax
            Case CLng(R)
            H = BDelta - GDelta
            Case CLng(G)
            H = (HSLMAX / 3) + RDelta - BDelta
            Case CLng(B)
            H = ((2 * HSLMAX) / 3) + GDelta - RDelta
        End Select
    
    If H < 0 Then H = H + HSLMAX
End If

RGBtoHSL.Hue = CInt(H)
RGBtoHSL.Lum = CInt(L)
RGBtoHSL.Sat = CInt(S)
End Function


Public Function HSLtoRGB(HueLumSat As HSLCol) As Long '***
    Dim R As Long, G As Long, B As Long
    Dim H As Long, L As Long, S As Long
    Dim Magic1 As Integer, Magic2 As Integer
    H = HueLumSat.Hue
    L = HueLumSat.Lum
    S = HueLumSat.Sat


    If S = 0 Then 'Greyscale
        R = (L * RGBMAX) / HSLMAX 'luminescence,
        'converted to the proper range
        G = R 'All RGB values same In greyscale
        B = R


        If H <> UNDEFINED Then
            'This is technically an error.
            'The RGBtoHSL routine will always return
            '
            'Hue = UNDEFINED (in this case 160)
            'when Sat = 0.
            'if you are writing a color mixer and
            'letting the user input color values,
            'you may want to set Hue = UNDEFINED
            'in this case.
        End If
    Else
        'Get the "Magic Numbers"


        If L <= HSLMAX / 2 Then
            Magic2 = (L * (HSLMAX + S) + _
            (HSLMAX / 2)) / HSLMAX
        Else
            Magic2 = L + S - ((L * S) + _
            (HSLMAX / 2)) / HSLMAX
        End If
        Magic1 = 2 * L - Magic2
        'get R, G, B; change units from HSLMAX r
        '     ange
        'to RGBMAX range
        R = (HuetoRGB(Magic1, Magic2, H + (HSLMAX / 3)) _
        * RGBMAX + (HSLMAX / 2)) / HSLMAX
        G = (HuetoRGB(Magic1, Magic2, H) _
        * RGBMAX + (HSLMAX / 2)) / HSLMAX
        B = (HuetoRGB(Magic1, Magic2, H - (HSLMAX / 3)) _
        * RGBMAX + (HSLMAX / 2)) / HSLMAX
    End If
    HSLtoRGB = RGB(CInt(R), CInt(G), CInt(B))
End Function


Private Function HuetoRGB(mag1 As Integer, mag2 As Integer, _
    Hue As Long) As Long '***
    'Utility function for HSLtoRGB
    'Range check


    If Hue < 0 Then
        Hue = Hue + HSLMAX
    ElseIf Hue > HSLMAX Then
        Hue = Hue - HSLMAX
    End If
    'Return r, g, or b value from parameters
    '


    Select Case Hue 'Values Get progressively larger.
        'Only the first true condition will exec
        '     ute
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
'
' The following are individual functions
'
' that use the HSL/RGB routines
' This is not intended to be a comprehen
'     sive library,
' just a sampling to demonstrate how to
'     use the routines
' and what kind of things are possible
'

Public Function ContrastingColor(RGBCol As Long) As Long
    'Returns Black or White, whichever will
    '     show up better
    'on the specified color
    'Useful for setting label forecolors wit
    '     h transparent
    'backgrounds (send it the form backcolor
    '     - RGB value, not
    'system value!)
    Dim HSL As HSLCol
    HSL = RGBtoHSL(RGBCol)
    If HSL.Lum > HSLMAX / 2 Then ContrastingColor = 0 _
Else: ContrastingColor = &HFFFFFF
End Function
