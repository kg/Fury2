VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2PalettedImage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_Image As Long
Private m_Iaeron As Long
Private m_Palette() As Long
Private m_Width As Long, m_Height As Long
Private m_Initialized As Boolean
Private m_booIaeron As Boolean
Private m_MyRect As Fury2Rect
Public Name As String

Sub SavePNG(filename As String)
On Error Resume Next
    If Not Initialized Then Exit Sub
    Call mdlIaeron.iaSaveImageP(filename, m_Image, VarPtr(m_Palette(0)), m_Width, m_Height)
End Sub

Public Function GetRect() As Fury2Rect
On Error Resume Next
    If m_MyRect Is Nothing Then
        Set m_MyRect = F2Rect(0, 0, m_Width, m_Height)
        Set GetRect = m_MyRect.Copy
    Else
        Set GetRect = m_MyRect.Copy
    End If
End Function

Public Sub LoadBitmap(filename As String)
On Error Resume Next
Dim imgHeight As Long, imgWidth As Long
Dim m_lngHandle As Long
Dim m_lngPaletteSize As Long
Dim m_strExt As String
    If Not F2Initialized Then Exit Sub
    m_Initialized = True
    m_lngHandle = iaLoadImageP(filename)
    If m_lngHandle = 0 Or Err <> 0 Then Exit Sub
    If m_lngHandle <> 0 Then
        m_strExt = Mid(filename, InStrRev(filename, ".") + 1)
        Free
        m_Width = iaGetWidth(m_lngHandle)
        m_Height = iaGetHeight(m_lngHandle)
        If m_Width <= 0 Or m_Height <= 0 Then Exit Sub
        m_Image = iaGetPointer(m_lngHandle)
        m_lngPaletteSize = iaGetPaletteLength(m_lngHandle)
        ReDim m_Palette(0 To m_lngPaletteSize - 1)
        Call iaGetPalette(m_lngHandle, VarPtr(m_Palette(0)))
        m_Iaeron = m_lngHandle
        m_booIaeron = True
        m_Initialized = True
    End If
End Sub

Friend Sub SetData(ImagePointer As Long, Palette() As Long, Width As Long, Height As Long)
On Error Resume Next
    m_Image = ImagePointer
    m_Palette = Palette
    m_Width = Width
    m_Height = Height
    m_Initialized = True
End Sub

Public Function ConvertToImage() As Fury2Image
On Error Resume Next
    Set ConvertToImage = F2Image(m_Width, m_Height)
    With ConvertToImage
        SoftFX.Depalettize .Pointer(0, 0), m_Image, VarPtr(m_Palette(0)), m_Width * m_Height
    End With
End Function

Public Sub ConvertToExistingImage(ByRef Image As Fury2Image)
On Error Resume Next
    With Image
        If .Width <> m_Width Or .Height <> m_Height Then .Resize m_Width, m_Height
        SoftFX.Depalettize .Pointer(0, 0), m_Image, VarPtr(m_Palette(0)), m_Width * m_Height
    End With
End Sub

Public Function GetPixel(ByVal x As Long, ByVal y As Long) As Byte
On Error Resume Next
Dim m_bytValue As Byte
    If Not Initialized Then Exit Function
    If (x < 0) Or (y < 0) Or (x >= m_Width) Or (y >= m_Height) Then Err.Raise 10241, "GetPixel", "Out of bounds": Exit Function
    CopyMemory m_bytValue, ByVal m_Image + (x) + (y * m_Width), 1
    GetPixel = m_bytValue
End Function

Public Sub SetPixel(ByVal x As Long, ByVal y As Long, ByRef Color)
On Error Resume Next
Dim m_bytColor As Byte
    If Not Initialized Then Exit Sub
    m_bytColor = CByte(Color)
    If (x < 0) Or (y < 0) Or (x >= m_Width) Or (y >= m_Height) Then Exit Sub
    CopyMemory ByVal m_Image + (x) + (y * m_Width), m_bytColor, 1
End Sub

Public Function GetPaletteEntry(ByVal i) As Long
On Error Resume Next
    If Not Initialized Then Exit Function
    If (i < 0) Or (i > UBound(m_Palette)) Then Err.Raise 10241, "GetPaletteEntry", "Out of bounds": Exit Function
    GetPaletteEntry = m_Palette(i)
End Function

Public Sub SetPaletteEntry(ByVal i As Long, ByRef Color)
On Error Resume Next
Dim m_lngColor As Byte
    If Not Initialized Then Exit Sub
    m_lngColor = CLng(Color)
    If (i < 0) Or (i > UBound(m_Palette)) Then Err.Raise 10241, "GetPaletteEntry", "Out of bounds": Exit Sub
    m_Palette(i) = m_lngColor
End Sub

Public Function GetPalette() As Long()
On Error Resume Next
    If Not Initialized Then Exit Function
    GetPalette = m_Palette
End Function

Public Sub SetPalette(Palette() As Long)
On Error Resume Next
    If Not Initialized Then Exit Sub
    m_Palette = Palette()
    If UBound(m_Palette) < 255 Then ReDim Preserve m_Palette(0 To 255)
End Sub

Public Function GetByteArray() As Byte()
On Error Resume Next
Dim m_bytArray() As Byte
    If Not Initialized Then Exit Function
    ReDim m_bytArray(0 To m_Width - 1, 0 To m_Height - 1)
    CopyMemory m_bytArray(0, 0), ByVal m_Image, m_Width * m_Height
    GetByteArray = m_bytArray
End Function

Public Sub SetByteArray(NewArray() As Byte)
    If Not F2Initialized Then Exit Sub
    m_Width = UBound(NewArray, 1) + 1
    m_Height = UBound(NewArray, 2) + 1
    m_Initialized = True
    Resize UBound(NewArray, 1) + 1, UBound(NewArray, 2) + 1
    CopyMemory ByVal m_Image, NewArray(0, 0), m_Width * m_Height
End Sub

Public Property Get Width() As Long
On Error Resume Next
    Width = m_Width
End Property

Public Property Get Height() As Long
On Error Resume Next
    Height = m_Height
End Property

Public Function Pointer(ByVal x As Long, ByVal y As Long) As Long
On Error Resume Next
    Pointer = 0
    If Not Initialized Then Exit Function
    Pointer = m_Image + (x) + (y * m_Width)
End Function

Public Function PalettePointer(ByVal i As Long) As Long
On Error Resume Next
    PalettePointer = 0
    If Not Initialized Then Exit Function
    PalettePointer = VarPtr(m_Palette(i))
End Function

Public Property Get Initialized()
On Error Resume Next
    Err.Clear
    Initialized = False
    If m_Width > -1 Then
        If m_Height > -1 Then
            If m_Image <> 0 Then
                If UBound(m_Palette) >= 255 Then
                    If F2Initialized Then
                        If Err = 0 Then
                            Initialized = m_Initialized
                        Else
                        End If
                    End If
                End If
            End If
        End If
    End If
End Property

Private Sub FreeBuffer()
On Error Resume Next
    If m_booIaeron Then
        iaFreeImage m_Iaeron
        m_Iaeron = 0
        m_booIaeron = False
        m_Image = 0
    Else
        If m_Image <> 0 Then Call GlobalFree(m_Image)
        m_Image = 0
    End If
End Sub

Sub Free()
On Error Resume Next
    If Not Initialized Then Exit Sub
    FreeBuffer
    Set m_MyRect = Nothing
    m_Width = -1
    m_Height = -1
    m_Initialized = False
End Sub

Sub Resize(ByVal x As Long, ByVal y As Long)
On Error Resume Next
    If x <= 0 Or y <= 0 Then Free: Exit Sub
    If m_Image <> 0 Then Free
    ReDim m_Palette(0 To 255)
    Set m_MyRect = Nothing
    m_Image = GlobalAlloc(GlobalAlloc_ZeroInitialize Or GlobalAlloc_Fixed, x * y)
    m_Width = x
    m_Height = y
    m_Initialized = True
End Sub

Private Sub Class_Initialize()
    ReDim m_Palette(0 To 0)
End Sub
