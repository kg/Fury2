Attribute VB_Name = "mdlUtility"
'
'   ::fury2 utility functions::
'
'Public Declare Function DX8Init Lib "DX8FX" Alias "_DX8Init@4" (ByVal Window As Long) As Long

Public Function StripParens(ByRef Text As String) As Boolean
On Error Resume Next
    If InStr(Text, "(") >= 0 And InStr(Text, ")") >= 0 Then
        Text = Trim(Text)
        If Left(Text, 1) = "(" And Right(Text, 1) = ")" Then
            Text = Mid(Text, 2, Len(Text) - 2)
            StripParens = True
        End If
    End If
End Function

Public Sub brSwap(ByRef v1 As Long, ByRef v2 As Long)
Dim v3 As Long
    v3 = v1
    v1 = v2
    v2 = v3
End Sub

Public Function F2Initialized() As Boolean
    F2Initialized = CBool(SoftFX.GetInitialized)
End Function

Public Function Bitmask(StartBit As Long, Length As Long) As Long
'Dim Value As Long, Bits As Long
'    For Bits = StartBit To StartBit + (Length - 1)
'        SetBit Value, Bits
'    Next Bits
'    Bitmask = Value
End Function

Public Function ConvertRect(Rect) As Fury2Rect
On Error Resume Next
    Set ConvertRect = F2Rect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom)
End Function

Public Function vbMax(ParamArray Values() As Variant)
On Error Resume Next
Dim biggestValue, biggestIndex As Long, checkAll As Long
    biggestValue = 0
    For checkAll = LBound(Values) To UBound(Values)
        If Values(checkAll) > biggestValue Then biggestIndex = checkAll: biggestValue = Values(checkAll)
    Next checkAll
    vbMax = biggestValue
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

Public Function F2RectFromStruct(Rect As SoftFX.Rectangle) As Fury2Rect
On Error Resume Next
Dim NewRect As Fury2Rect
    Set NewRect = New Fury2Rect
    NewRect.SetRectangle Rect
    Set F2RectFromStruct = NewRect
    Set NewRect = Nothing
End Function
'
'Public Function F2LoadF2G(ByVal filename As String) As Fury2Image
'Dim newImage As Fury2Image
'    Set newImage = New Fury2Image
'    newImage.LoadF2G CStr(filename)
'    Set F2LoadF2G = newImage
'    Set newImage = Nothing
'End Function

Public Function F2LoadPicture(ByVal Filename As String) As Fury2Image
Dim newImage As Fury2Image
    Set newImage = New Fury2Image
    newImage.LoadFile CStr(Filename)
    Set F2LoadPicture = newImage
    Set newImage = Nothing
End Function

Public Function F2Image(ByVal X As Long, ByVal Y As Long) As Fury2Image
Dim newImage As Fury2Image
    Set newImage = New Fury2Image
    newImage.Resize X, Y
    Set F2Image = newImage
    Set newImage = Nothing
End Function

Public Function F2RGB(ByVal Red, ByVal Green, ByVal Blue, Optional ByVal Alpha = 255) As Long
On Error Resume Next
    F2RGB = BGRA(CLng(Red), CLng(Green), CLng(Blue), CLng(Alpha))
End Function

Public Function F2White() As Long
    F2White = White
End Function

Public Function F2Black() As Long
    F2Black = Black
End Function

Public Sub SetFormSize(ByRef Frm As Form, X As Long, Y As Long, Optional ByVal Center As Boolean = False)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    If Center Then
        Frm.Move (Screen.Width - (CLng(X * Screen.TwipsPerPixelX) + (BW * 2))) \ 2, (Screen.Height - (CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2))) \ 2, CLng(X * Screen.TwipsPerPixelX) + (BW * 2), CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    Else
        Frm.Move Frm.Left, Frm.Top, CLng(X * Screen.TwipsPerPixelX) + (BW * 2), CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    End If
    Frm.ScaleMode = OldMode
End Sub

