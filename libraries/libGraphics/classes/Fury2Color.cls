VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Color"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Private m_Color As Long

Public Property Let ARGBColor(ByVal vData As Long)
Dim m_lngRed As Long, m_lngGreen As Long, m_lngBlue As Long, m_lngAlpha As Long
Dim m_lngColor As Long
    m_lngColor = vData
    m_lngRed = GetRed(m_lngColor)
    m_lngGreen = GetGreen(m_lngColor)
    m_lngBlue = GetBlue(m_lngColor)
    m_lngAlpha = GetAlpha(m_lngColor)
    m_Color = SoftFX.BGRA(m_lngGreen, m_lngRed, m_lngAlpha, m_lngBlue)
End Property

Public Property Get ARGBColor() As Long
End Property

Public Property Let ABGRColor(ByVal vData As Long)
Dim m_lngRed As Long, m_lngGreen As Long, m_lngBlue As Long, m_lngAlpha As Long
Dim m_lngColor As Long
    m_lngColor = vData
    m_lngRed = GetRed(m_lngColor)
    m_lngGreen = GetGreen(m_lngColor)
    m_lngBlue = GetBlue(m_lngColor)
    m_lngAlpha = GetAlpha(m_lngColor)
    m_Color = SoftFX.BGRA(m_lngGreen, m_lngBlue, m_lngAlpha, m_lngRed)
End Property

Public Property Get ABGRColor() As Long
End Property

Public Property Let BGRAColor(ByVal vData As Long)
    m_Color = vData
End Property

Public Property Get BGRAColor() As Long
Attribute BGRAColor.VB_ProcData.VB_Invoke_Property = "StandardColor"
Attribute BGRAColor.VB_UserMemId = 0
Attribute BGRAColor.VB_MemberFlags = "200"
    BGRAColor = m_Color
End Property

Public Property Let RGBAColor(ByVal vData As Long)
End Property

Public Property Get RGBAColor() As Long
End Property

Public Property Let Alpha(ByVal vData As Long)
    m_Color = SoftFX.SetAlpha(m_Color, vData)
End Property

Public Property Get Alpha() As Long
    Alpha = SoftFX.GetAlpha(m_Color)
End Property

Public Property Let Lightness(ByVal vData As Long)
End Property

Public Property Get Lightness() As Long
End Property

Public Property Let Saturation(ByVal vData As Long)
End Property

Public Property Get Saturation() As Long
End Property

Public Property Let Hue(ByVal vData As Long)
End Property

Public Property Get Hue() As Long
End Property

Public Property Let Blue(ByVal vData As Long)
    m_Color = SetBlue(m_Color, vData)
End Property

Public Property Get Blue() As Long
    Blue = GetBlue(m_Color)
End Property

Public Property Let Green(ByVal vData As Long)
    m_Color = SetRed(m_Color, vData)
End Property

Public Property Get Green() As Long
    Green = GetGreen(m_Color)
End Property

Public Property Let Red(ByVal vData As Long)
    m_Color = SetRed(m_Color, vData)
End Property

Public Property Get Red() As Long
    Red = GetRed(m_Color)
End Property

Public Property Get Grayscale() As Fury2Color
Dim cNewColor As Fury2Color
    Set cNewColor = New Fury2Color
'    cNewColor.BGRAColor = SoftFX.Grayscale(m_Color)
    Set Grayscale = cNewColor
    Set cNewColor = Nothing
End Property
