VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Math"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Function Floor(ByVal Value As Single) As Single
Attribute Floor.VB_Description = "Returns the floor value of a number."
On Error Resume Next
    Floor = SoftFX.FloorF(Value)
End Function

Public Function Ceil(ByVal Value As Single) As Single
Attribute Ceil.VB_Description = "Returns the ceiling value of a number."
On Error Resume Next
    Ceil = SoftFX.CeilF(Value)
End Function

Public Function RotatePoint(ByVal X As Single, ByVal Y As Single, ByVal Angle As Single)
Attribute RotatePoint.VB_Description = "Rotates a point around 0,0 by a given angle (in degrees)."
On Error Resume Next
Dim l_sngTheta As Single, l_sngR As Single
    l_sngR = Sqr((X * X) + (Y * Y))
    If (X > 0) Then
        l_sngTheta = Atn(Y / X)
    ElseIf (X < 0) Then
        l_sngTheta = Atn(Y / X) + c_dblPi
    Else
        l_sngTheta = c_dblPi / 2
    End If
    l_sngTheta = l_sngTheta + Angle * c_dblRadian
    RotatePoint = Array(l_sngR * Sin(l_sngTheta), l_sngR * -Cos(l_sngTheta))
End Function

Public Function AngleBetween(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single) As Single
Attribute AngleBetween.VB_Description = "Returns the angle between two given points (in degrees)."
Dim Rx As Single, Ry As Single
On Error Resume Next
    Rx = (x2 - x1)
    Ry = (y2 - y1)
    If Rx = 0 And Ry = 0 Then
        AngleBetween = 0
        Exit Function
    ElseIf Ry = 0 Then
        If Rx > 0 Then
            AngleBetween = 90
        Else
            AngleBetween = 270
        End If
        Exit Function
    ElseIf Rx = 0 Then
        If Ry > 0 Then
            AngleBetween = 180
        Else
            AngleBetween = 0
        End If
        Exit Function
    End If
    If Ry < 0 And Rx < 0 Then
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (270)
    ElseIf Ry >= 0 And Rx < 0 Then
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (270)
    ElseIf Ry < 0 And Rx >= 0 Then
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (90)
    Else
        AngleBetween = (Atn(Ry / Rx) / c_dblRadian) + (90)
    End If
    AngleBetween = NormalizeAngle(AngleBetween)
End Function

Public Function AngleDistance(ByVal Angle1 As Single, ByVal Angle2 As Single) As Single
Attribute AngleDistance.VB_Description = "Returns the distance between two given angles (in degrees)"
On Error Resume Next
Dim l_sngDelta As Single
    Angle1 = NormalizeAngle(Angle1)
    Angle2 = NormalizeAngle(Angle2)
    l_sngDelta = NormalizeAngle(Angle2 - Angle1)
    AngleDistance = l_sngDelta
End Function

Public Function AngleInRange(ByVal Angle As Single, ByVal Minimum As Single, ByVal Maximum As Single, Optional ByVal Inclusive As Boolean = True) As Boolean
Attribute AngleInRange.VB_Description = "Checks to see if an angle is between two given angles."
On Error Resume Next
    Angle = NormalizeAngle(Angle)
    Minimum = NormalizeAngle(Minimum)
    Maximum = NormalizeAngle(Maximum)
    If Not Inclusive Then
        If Angle = Minimum Then Exit Function
        If Angle = Maximum Then Exit Function
    End If
    AngleInRange = SoftFX.AngleInRange(Minimum, Maximum, Angle) <> 0
End Function

Public Function ClipInteger(ByVal Number As Long, ByVal Min As Long, ByVal Max As Long) As Long
Attribute ClipInteger.VB_Description = "Clamps the value of an integer to (Min - Max)."
On Error Resume Next
    ClipInteger = ClipValue(Number, Min, Max)
End Function

Public Function ClipNumber(ByVal Number As Single, ByVal Min As Single, ByVal Max As Single) As Single
Attribute ClipNumber.VB_Description = "Clamps the value of a number to (Min - Max)."
On Error Resume Next
Dim l_dblMin As Single, l_dblMax As Single
    ClipNumber = Number
    If ClipNumber < Min Then ClipNumber = Min
    If ClipNumber > Max Then ClipNumber = Max
End Function

Public Function Clip(ByVal Number As Single, ByVal Min As Single, ByVal Max As Single) As Single
Attribute Clip.VB_Description = "Clamps the value of a number to (Min - Max)."
On Error Resume Next
Dim l_dblMin As Single, l_dblMax As Single
    Clip = Number
    If Number < Min Then Clip = Min
    If Number > Max Then Clip = Max
End Function

Public Function WrapInteger(Number, Min, Max) As Long
Attribute WrapInteger.VB_Description = "Wraps the value of an integer into (Min - Max)."
On Error Resume Next
    WrapInteger = WrapValue(CLng(Number), CLng(Min), CLng(Max))
End Function

Public Function Wrap(ByVal Number As Single, ByVal Min As Single, ByVal Max As Single) As Single
Attribute Wrap.VB_Description = "Wraps the value of a number into (Min - Max). THIS DOES NOT HAVE THE SAME SEMANTICS AS WRAPINTEGER."
On Error Resume Next
Dim D As Single, N As Single, I As Single, R As Single
    D = Max - Min
' Hack
'    WrapNumber = WrapValue(CLng(Number), CLng(Min), CLng(Max))
    If (Number < Min) Then
        N = Abs(Min - Number)
        I = N / D
        R = I - Floor(I)
        Wrap = Min + ((1 - R) * D)
    ElseIf (Number > Max) Then
        N = Abs(Number - Max)
        I = N / D
        R = I - Floor(I)
        Wrap = Min + (R * D)
    Else
        Wrap = Number
    End If
End Function

Public Function WrapNumber(ByVal Number As Single, ByVal Min As Single, ByVal Max As Single) As Single
Attribute WrapNumber.VB_Description = "Wraps the value of a number into (Min - Max). THIS DOES NOT HAVE THE SAME SEMANTICS AS WRAPINTEGER."
On Error Resume Next
Dim D As Single, N As Single, I As Single, R As Single
    D = Max - Min
' Hack
'    WrapNumber = WrapValue(CLng(Number), CLng(Min), CLng(Max))
    If (Number < Min) Then
        N = Abs(Min - Number)
        I = N / D
        R = I - Floor(I)
        WrapNumber = Min + ((1 - R) * D)
    ElseIf (Number > Max) Then
        N = Abs(Number - Max)
        I = N / D
        R = I - Floor(I)
        WrapNumber = Min + (R * D)
    Else
        WrapNumber = Number
    End If
End Function

Public Function NormalizeAngle(ByVal Angle As Single) As Single
Attribute NormalizeAngle.VB_Description = "Normalizes an angle into the range 0-359.999."
Dim N As Single
    N = Angle / 360
    NormalizeAngle = Angle - (SoftFX.FloorF(N) * 360)
End Function

Public Function Random(Optional ByVal Minimum As Single = 0, Optional ByVal Maximum As Single = 1, Optional ByVal FloatingPoint As Boolean = True)
Attribute Random.VB_Description = "Generates a random integer or number within the range (Minimum - Maximum), or (0-1) if no range is specified."
On Error Resume Next
    If CBool(FloatingPoint) Then
        Random = (Rnd * (Maximum - Minimum)) + Minimum
    Else
        Random = CLng(FRound((Rnd * (FRound(Maximum) - FRound(Minimum))) + FRound(Minimum)))
    End If
End Function

Public Function Ray(ByVal Angle As Single, ByVal Length As Single) As Variant
Attribute Ray.VB_Description = "Generates a ray of a given length in a given angle."
On Error Resume Next
    Angle = NormalizeAngle(Angle) * c_dblRadian
    Ray = Array(Sin(Angle) * Length, -Cos(Angle) * Length)
End Function

Public Function PathTarget(ByVal x1 As Single, ByVal y1 As Single, ByVal Angle As Single, ByVal Distance As Single, Optional ByRef x2, Optional ByRef y2)
Attribute PathTarget.VB_Description = "Moves a given point in a given direction by a given amount. Stores the new value in x2/y2 and returns it as an array."
On Error Resume Next
Dim m_sngX As Single, m_sngY As Single
    Angle = NormalizeAngle(Angle) * c_dblRadian
    m_sngX = x1 + (Sin(Angle) * Distance)
    m_sngY = y1 + -(Cos(Angle) * Distance)
    PathTarget = Array(m_sngX, m_sngY)
    x2 = m_sngX
    y2 = m_sngY
End Function

Public Function BaseNToBaseN(ByVal Value As String, ByVal FromDigits As String, ByVal ToDigits As String) As String
Attribute BaseNToBaseN.VB_Description = "Converts a numeric value from one base to another."
On Error Resume Next
    BaseNToBaseN = IntegerToBaseN(BaseNToInteger(Value, FromDigits), ToDigits)
End Function

Public Function BaseNToInteger(ByVal Value As String, ByVal Digits As String) As Long
Attribute BaseNToInteger.VB_Description = "Converts a numeric value from a given base into an integer."
On Error Resume Next
Dim l_bytDigits() As Byte
Dim l_bytInput() As Byte
Dim l_lngValue As Long
Dim l_lngBase As Long
Dim l_lngChar As Long, l_lngFind As Long
Dim l_lngNegative As Long
    l_bytDigits = StrConv(Digits, vbFromUnicode)
    l_bytInput = StrConv(Value, vbFromUnicode)
    l_lngBase = UBound(l_bytDigits) - LBound(l_bytDigits) + 1
    l_lngNegative = 1
    For l_lngChar = LBound(l_bytInput) To UBound(l_bytInput)
        If l_bytInput(l_lngChar) = 45 Then
            l_lngNegative = -1
        Else
            For l_lngFind = LBound(l_bytDigits) To UBound(l_bytDigits)
                If l_bytInput(l_lngChar) = l_bytDigits(l_lngFind) Then
                    l_lngValue = l_lngValue * l_lngBase
                    l_lngValue = l_lngValue Or l_lngFind
                    Exit For
                End If
            Next l_lngFind
        End If
    Next l_lngChar
    BaseNToInteger = l_lngValue * l_lngNegative
End Function

Public Function IntegerToBaseN(ByVal Value As Long, ByVal Digits As String) As String
Attribute IntegerToBaseN.VB_Description = "Converts an integer into a numeric value in a given base."
On Error Resume Next
Dim l_booNegative As Boolean
Dim l_bytDigits() As Byte
Dim l_strOutput As String
Dim l_lngBase As Long
    l_booNegative = (Value < 0)
    Value = Abs(Value)
    l_bytDigits = StrConv(Digits, vbFromUnicode)
    l_lngBase = UBound(l_bytDigits) - LBound(l_bytDigits) + 1
    Do While Value > 0
        l_strOutput = Chr(l_bytDigits(Value Mod l_lngBase)) & l_strOutput
        Value = Value \ l_lngBase
    Loop
    IntegerToBaseN = IIf(l_booNegative, "-", "") & l_strOutput
End Function

Public Function FormatNumber(ByVal Value As Single, ByVal Mask As String)
Attribute FormatNumber.VB_Description = "Formats a number into a string using a formatting mask."
On Error Resume Next
    FormatNumber = Format(Value, Mask)
End Function

Public Function Maximum(ParamArray Values() As Variant) As Single
Attribute Maximum.VB_Description = "Returns the largest of the given values."
On Error Resume Next
Dim l_lngItems As Long, l_varBiggest As Variant
    l_varBiggest = Empty
    For l_lngItems = LBound(Values) To UBound(Values)
        If (l_varBiggest = Empty) Or (l_varBiggest < Values(l_lngItems)) Then
            l_varBiggest = Values(l_lngItems)
        End If
    Next l_lngItems
    Maximum = l_varBiggest
End Function

Public Function Minimum(ParamArray Values() As Variant) As Single
Attribute Minimum.VB_Description = "Returns the smallest of the given values."
On Error Resume Next
Dim l_lngItems As Long, l_varSmallest As Variant
    l_varSmallest = Empty
    For l_lngItems = LBound(Values) To UBound(Values)
        If (l_varSmallest = Empty) Or (l_varSmallest > Values(l_lngItems)) Then
            l_varSmallest = Values(l_lngItems)
        End If
    Next l_lngItems
    Minimum = l_varSmallest
End Function

Public Function Atan(ByVal Value As Single) As Single
Attribute Atan.VB_Description = "Returns the arc-tangent of value."
On Error Resume Next
    Atan = Atn(Value)
End Function

Public Function Atan2(ByVal X As Single, ByVal Y As Single) As Single
Attribute Atan2.VB_Description = "Returns the 2D arc-tangent of x/y."
On Error Resume Next
    If (X = 0) Then
        Atan2 = 0
    Else
        Atan2 = Atn(Y / X)
    End If
End Function

Public Function Interpolate(ByVal A As Single, ByVal B As Single, ByVal X As Single) As Single
Attribute Interpolate.VB_Description = "Interpolates linearly between A and B, using X as the interpolation factor."
On Error Resume Next
    Interpolate = SoftFX.Interpolate(A, B, X)
End Function

Public Function CosineInterpolate(ByVal A As Single, ByVal B As Single, ByVal X As Single) As Single
Attribute CosineInterpolate.VB_Description = "Interpolates cosinely between A and B, using X as the interpolation factor."
On Error Resume Next
    CosineInterpolate = SoftFX.CosineInterpolate(A, B, X)
End Function

Public Function CubicInterpolate(ByVal A As Single, ByVal B As Single, ByVal C As Single, ByVal D As Single, ByVal X As Single) As Single
Attribute CubicInterpolate.VB_Description = "Performs cubic interpolation on (A, B, C, D) using X as the interpolation factor."
On Error Resume Next
    CubicInterpolate = SoftFX.CubicInterpolate(A, B, C, D, X)
End Function


