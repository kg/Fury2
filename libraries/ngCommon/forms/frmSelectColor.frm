VERSION 5.00
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Begin VB.Form frmSelectColor 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Select Color"
   ClientHeight    =   4065
   ClientLeft      =   45
   ClientTop       =   360
   ClientWidth     =   5865
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmSelectColor.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   271
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   391
   StartUpPosition =   1  'CenterOwner
   Begin VB.CommandButton cmdEffects 
      Caption         =   "&Effects"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   4335
      TabIndex        =   14
      Top             =   1800
      Width           =   1500
   End
   Begin VB.CommandButton cmdDropper 
      Caption         =   "&Dropper"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   4335
      TabIndex        =   13
      Top             =   1350
      Width           =   1500
   End
   Begin VB.Frame fraOldColor 
      Caption         =   "Old Color"
      Height          =   900
      Left            =   4335
      TabIndex        =   45
      Top             =   3135
      Width           =   1500
      Begin VB.PictureBox picOldColor 
         AutoRedraw      =   -1  'True
         BackColor       =   &H00000000&
         BorderStyle     =   0  'None
         Height          =   615
         Left            =   60
         ScaleHeight     =   41
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   91
         TabIndex        =   46
         Top             =   210
         Width           =   1365
      End
   End
   Begin VB.Frame fraNewColor 
      Caption         =   "New Color"
      Height          =   900
      Left            =   4335
      TabIndex        =   15
      Top             =   2220
      Width           =   1500
      Begin VB.PictureBox picNewColor 
         AutoRedraw      =   -1  'True
         BackColor       =   &H00000000&
         BorderStyle     =   0  'None
         Height          =   615
         Left            =   60
         ScaleHeight     =   41
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   91
         TabIndex        =   16
         Top             =   210
         Width           =   1365
      End
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Default         =   -1  'True
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   4335
      TabIndex        =   11
      Top             =   30
      Width           =   1500
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   4335
      TabIndex        =   12
      Top             =   480
      Width           =   1500
   End
   Begin vbalDTab6.vbalDTabControl dtVisual 
      Height          =   3030
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4260
      _ExtentX        =   7514
      _ExtentY        =   5345
      AllowScroll     =   0   'False
      TabAlign        =   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty SelectedFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShowCloseButton =   0   'False
      MoveableTabs    =   0   'False
      Begin VB.PictureBox picVisual 
         BorderStyle     =   0  'None
         Height          =   2640
         Index           =   1
         Left            =   30
         ScaleHeight     =   176
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   280
         TabIndex        =   6
         Top             =   360
         Visible         =   0   'False
         Width           =   4200
         Begin VB.CommandButton cmdSavePalette 
            Caption         =   "&Save Palette..."
            Height          =   360
            Left            =   1260
            TabIndex        =   9
            Top             =   2250
            Width           =   1440
         End
         Begin VB.CommandButton cmdLoadPalette 
            Caption         =   "&Load Palette..."
            Height          =   360
            Left            =   2730
            TabIndex        =   10
            Top             =   2250
            Width           =   1440
         End
         Begin VB.PictureBox Picture1 
            Height          =   2205
            Left            =   30
            ScaleHeight     =   2145
            ScaleWidth      =   4080
            TabIndex        =   7
            Top             =   30
            Width           =   4140
            Begin cFScroll.FlatScrollBar vsPalette 
               Height          =   2145
               Left            =   3810
               TabIndex        =   8
               Top             =   0
               Width           =   270
               _ExtentX        =   476
               _ExtentY        =   3784
               Orientation     =   1
               Max             =   100
               Style           =   -1
            End
         End
      End
      Begin VB.PictureBox picVisual 
         BorderStyle     =   0  'None
         Height          =   1875
         Index           =   0
         Left            =   30
         ScaleHeight     =   125
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   280
         TabIndex        =   1
         Top             =   360
         Visible         =   0   'False
         Width           =   4200
         Begin VB.PictureBox picAlphaSlider 
            AutoRedraw      =   -1  'True
            BackColor       =   &H00000000&
            BorderStyle     =   0  'None
            Height          =   435
            Left            =   30
            ScaleHeight     =   29
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   276
            TabIndex        =   5
            Top             =   1410
            Width           =   4140
         End
         Begin VB.PictureBox picBlueSlider 
            AutoRedraw      =   -1  'True
            BackColor       =   &H00000000&
            BorderStyle     =   0  'None
            Height          =   435
            Left            =   30
            ScaleHeight     =   29
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   276
            TabIndex        =   4
            Top             =   945
            Width           =   4140
         End
         Begin VB.PictureBox picGreenSlider 
            AutoRedraw      =   -1  'True
            BackColor       =   &H00000000&
            BorderStyle     =   0  'None
            Height          =   435
            Left            =   30
            ScaleHeight     =   29
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   276
            TabIndex        =   3
            Top             =   480
            Width           =   4140
         End
         Begin VB.PictureBox picRedSlider 
            AutoRedraw      =   -1  'True
            BackColor       =   &H00000000&
            BorderStyle     =   0  'None
            Height          =   435
            Left            =   30
            ScaleHeight     =   29
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   276
            TabIndex        =   2
            Top             =   15
            Width           =   4140
         End
      End
   End
   Begin vbalDTab6.vbalDTabControl dtTextual 
      Height          =   945
      Left            =   30
      TabIndex        =   17
      Top             =   3090
      Width           =   4260
      _ExtentX        =   7514
      _ExtentY        =   1667
      AllowScroll     =   0   'False
      TabAlign        =   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty SelectedFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShowCloseButton =   0   'False
      MoveableTabs    =   0   'False
      Begin VB.PictureBox picTextual 
         BorderStyle     =   0  'None
         Height          =   540
         Index           =   1
         Left            =   30
         ScaleHeight     =   36
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   280
         TabIndex        =   27
         Top             =   360
         Visible         =   0   'False
         Width           =   4200
         Begin VB.TextBox txtHSLAlpha 
            Height          =   300
            Left            =   3135
            Locked          =   -1  'True
            TabIndex        =   31
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtLuminance 
            Height          =   300
            Left            =   2115
            Locked          =   -1  'True
            TabIndex        =   30
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtSaturation 
            Height          =   300
            Left            =   1095
            Locked          =   -1  'True
            TabIndex        =   29
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtHue 
            Height          =   300
            Left            =   75
            Locked          =   -1  'True
            TabIndex        =   28
            Top             =   0
            Width           =   990
         End
         Begin VB.Label lblAlpha 
            Alignment       =   2  'Center
            Caption         =   "Alpha"
            Height          =   195
            Index           =   1
            Left            =   3135
            TabIndex        =   35
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblSat 
            Alignment       =   2  'Center
            Caption         =   "Luminance"
            Height          =   195
            Left            =   2115
            TabIndex        =   34
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblLum 
            Alignment       =   2  'Center
            Caption         =   "Saturation"
            Height          =   195
            Left            =   1095
            TabIndex        =   33
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblHue 
            Alignment       =   2  'Center
            Caption         =   "Hue"
            Height          =   195
            Left            =   75
            TabIndex        =   32
            Top             =   330
            Width           =   990
         End
      End
      Begin VB.PictureBox picTextual 
         BorderStyle     =   0  'None
         Height          =   540
         Index           =   2
         Left            =   30
         ScaleHeight     =   36
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   280
         TabIndex        =   36
         Top             =   360
         Visible         =   0   'False
         Width           =   4200
         Begin VB.TextBox txtRedHex 
            Height          =   300
            Left            =   75
            TabIndex        =   37
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtGreenHex 
            Height          =   300
            Left            =   1095
            TabIndex        =   38
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtBlueHex 
            Height          =   300
            Left            =   2115
            TabIndex        =   39
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtAlphaHex 
            Height          =   300
            Left            =   3135
            TabIndex        =   40
            Top             =   0
            Width           =   990
         End
         Begin VB.Label lblRed 
            Alignment       =   2  'Center
            Caption         =   "Red"
            Height          =   195
            Index           =   1
            Left            =   75
            TabIndex        =   41
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblGreen 
            Alignment       =   2  'Center
            Caption         =   "Green"
            Height          =   195
            Index           =   1
            Left            =   1095
            TabIndex        =   42
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblBlue 
            Alignment       =   2  'Center
            Caption         =   "Blue"
            Height          =   195
            Index           =   1
            Left            =   2115
            TabIndex        =   43
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblAlpha 
            Alignment       =   2  'Center
            Caption         =   "Alpha"
            Height          =   195
            Index           =   0
            Left            =   3135
            TabIndex        =   44
            Top             =   330
            Width           =   990
         End
      End
      Begin VB.PictureBox picTextual 
         BorderStyle     =   0  'None
         Height          =   555
         Index           =   0
         Left            =   30
         ScaleHeight     =   37
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   280
         TabIndex        =   18
         Top             =   360
         Visible         =   0   'False
         Width           =   4200
         Begin VB.TextBox txtAlpha 
            Height          =   300
            Left            =   3135
            TabIndex        =   22
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtBlue 
            Height          =   300
            Left            =   2115
            TabIndex        =   21
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtGreen 
            Height          =   300
            Left            =   1095
            TabIndex        =   20
            Top             =   0
            Width           =   990
         End
         Begin VB.TextBox txtRed 
            Height          =   300
            Left            =   75
            TabIndex        =   19
            Top             =   0
            Width           =   990
         End
         Begin VB.Label lblAlpha 
            Alignment       =   2  'Center
            Caption         =   "Alpha"
            Height          =   195
            Index           =   2
            Left            =   3135
            TabIndex        =   26
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblBlue 
            Alignment       =   2  'Center
            Caption         =   "Blue"
            Height          =   195
            Index           =   0
            Left            =   2115
            TabIndex        =   25
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblGreen 
            Alignment       =   2  'Center
            Caption         =   "Green"
            Height          =   195
            Index           =   0
            Left            =   1095
            TabIndex        =   24
            Top             =   330
            Width           =   990
         End
         Begin VB.Label lblRed 
            Alignment       =   2  'Center
            Caption         =   "Red"
            Height          =   195
            Index           =   0
            Left            =   75
            TabIndex        =   23
            Top             =   330
            Width           =   990
         End
      End
   End
   Begin VB.Menu popEffects 
      Caption         =   "Effects Popup"
      Visible         =   0   'False
      Begin VB.Menu mnuInvert 
         Caption         =   "Invert"
         Begin VB.Menu mnuInvertAll 
            Caption         =   "All Channels"
         End
         Begin VB.Menu mnuInvertRGB 
            Caption         =   "RGB"
         End
         Begin VB.Menu mnuInvertRed 
            Caption         =   "Red"
         End
         Begin VB.Menu mnuInvertGreen 
            Caption         =   "Green"
         End
         Begin VB.Menu mnuInvertBlue 
            Caption         =   "Blue"
         End
         Begin VB.Menu mnuInvertAlpha 
            Caption         =   "Alpha"
         End
      End
      Begin VB.Menu mnuGrayscale 
         Caption         =   "Grayscale"
      End
   End
End
Attribute VB_Name = "frmSelectColor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Done As Boolean, Cancelled As Boolean
Public OldColor As Long, NewColor As Long
Public PickerImage As Fury2Image

Private m_imgColorBuffer As Fury2Image
Private m_imgColorBufferBG As Fury2Image
Private m_imgSliderBuffer As Fury2Image

Private Sub RecalcHSL()
On Error Resume Next
Dim l_lngR As Long, l_lngG As Long, l_lngB As Long
    Call HSLToRGB(CSng(Replace(txtHue.Text, "°", "")), CSng(Replace(txtSaturation.Text, "%", "")) / 100, CSng(Replace(txtLuminance.Text, "%", "")) / 100, l_lngR, l_lngG, l_lngB)
    NewColor = BGRA(l_lngR, l_lngG, l_lngB, CLng(CSng(Replace(txtHSLAlpha.Text, "%", "")) / 100 * 255))
    RefreshAll False
End Sub

Private Sub DrawBevel(ByRef Surface As Fury2Image, Optional ByRef Rectangle As Fury2Rect = Nothing, Optional ByVal Pressed As Boolean = False, Optional ByRef Colors As Variant, Optional ByVal FillColor As Long = 0)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_varTemp As Variant
    If Rectangle Is Nothing Then Set Rectangle = Surface.Rectangle
    If (VarType(Colors) And vbArray) <> vbArray Then
        Colors = Array(SwapChannels(GetSystemColor(SystemColor_Button_Highlight), Red, Blue), SwapChannels(GetSystemColor(SystemColor_Button_Shadow), Red, Blue))
    End If
    If Pressed Then
        l_varTemp = Colors(0)
        Colors(0) = Colors(1)
        Colors(1) = l_varTemp
    End If
    Set l_rctRect = Rectangle.Copy
    With l_rctRect
        Surface.Fill l_rctRect, FillColor, RenderMode_SourceAlpha
        Surface.[Line] Array(.Left, .Top, .Right - 2, .Top), SetAlpha(Colors(0), 255), RenderMode_SourceAlpha
        Surface.[Line] Array(.Left, .Top + 1, .Left, .Bottom - 2), SetAlpha(Colors(0), 255), RenderMode_SourceAlpha
        Surface.[Line] Array(.Right - 1, .Top, .Right - 1, .Bottom - 1), SetAlpha(Colors(1), 255), RenderMode_SourceAlpha
        Surface.[Line] Array(.Left, .Bottom - 1, .Right - 2, .Bottom - 1), SetAlpha(Colors(1), 255), RenderMode_SourceAlpha
        .Adjust -1, -1
        Surface.[Line] Array(.Left, .Top, .Right - 2, .Top), SetAlpha(Colors(0), 127), RenderMode_SourceAlpha
        Surface.[Line] Array(.Left, .Top + 1, .Left, .Bottom - 2), SetAlpha(Colors(0), 127), RenderMode_SourceAlpha
        Surface.[Line] Array(.Right - 1, .Top, .Right - 1, .Bottom - 1), SetAlpha(Colors(1), 127), RenderMode_SourceAlpha
        Surface.[Line] Array(.Left, .Bottom - 1, .Right - 2, .Bottom - 1), SetAlpha(Colors(1), 127), RenderMode_SourceAlpha
        .Adjust -2, -2
    End With
End Sub

Public Sub RefreshTextual(Optional Override As Boolean = True)
On Error Resume Next
Dim l_lngTabs As Long
Dim l_sngHue As Single, l_sngLum As Single, l_sngSat As Single
    For l_lngTabs = picTextual.LBound To picTextual.UBound
        picTextual(l_lngTabs).Visible = (dtTextual.SelectedTab.Index = (l_lngTabs + 1))
    Next l_lngTabs
    If (dtTextual.SelectedTab.Index <> 1) Or Override Then
        txtRed.Text = GetRed(NewColor)
        txtGreen.Text = GetGreen(NewColor)
        txtBlue.Text = GetBlue(NewColor)
        txtAlpha.Text = GetAlpha(NewColor)
    End If
    If (dtTextual.SelectedTab.Index <> 2) Or Override Then
        RGBToHSL GetRed(NewColor), GetGreen(NewColor), GetBlue(NewColor), l_sngHue, l_sngSat, l_sngLum
        txtHue.Text = CStr(CLng(l_sngHue)) & "°"
        txtLuminance.Text = CStr(CLng(l_sngLum * 100)) & "%"
        txtSaturation.Text = CStr(CLng(l_sngSat * 100)) & "%"
        txtHSLAlpha.Text = CStr(CLng((GetAlpha(NewColor) / 255) * 100)) & "%"
    End If
    If (dtTextual.SelectedTab.Index <> 3) Or Override Then
        txtRedHex.Text = Hex(GetRed(NewColor))
        txtGreenHex.Text = Hex(GetGreen(NewColor))
        txtBlueHex.Text = Hex(GetBlue(NewColor))
        txtAlphaHex.Text = Hex(GetAlpha(NewColor))
    End If
End Sub

Public Sub RefreshVisual()
On Error Resume Next
Dim l_lngTabs As Long
Dim l_sngMul As Single
    If m_imgSliderBuffer Is Nothing Then
        Set m_imgSliderBuffer = F2Image(picRedSlider.ScaleWidth, picRedSlider.ScaleHeight)
    End If
    For l_lngTabs = picVisual.LBound To picVisual.UBound
        picVisual(l_lngTabs).Visible = (dtVisual.SelectedTab.Index = (l_lngTabs + 1))
    Next l_lngTabs
    Select Case dtVisual.SelectedTab.Index
    Case 1
        l_sngMul = (m_imgSliderBuffer.Width - 9) / 255
        m_imgSliderBuffer.TileBlit , m_imgColorBufferBG
        m_imgSliderBuffer.GradientFill m_imgSliderBuffer.Rectangle, _
        Array(SetRed(NewColor, 0), SetRed(NewColor, 255), "H"), RenderMode_SourceAlpha
        DrawBevel m_imgSliderBuffer, F2Rect(GetRed(NewColor) * l_sngMul + 1, 1, 7, m_imgSliderBuffer.Height - 2, False), True, , SetAlpha(SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue), 127)
        DrawBevel m_imgSliderBuffer
        CopyImageToDC picRedSlider.hDC, m_imgSliderBuffer.Rectangle, m_imgSliderBuffer
        picRedSlider.Refresh
        m_imgSliderBuffer.TileBlit , m_imgColorBufferBG
        m_imgSliderBuffer.GradientFill m_imgSliderBuffer.Rectangle, _
        Array(SetGreen(NewColor, 0), SetGreen(NewColor, 255), "H"), RenderMode_SourceAlpha
        DrawBevel m_imgSliderBuffer, F2Rect(GetGreen(NewColor) * l_sngMul + 1, 1, 7, m_imgSliderBuffer.Height - 2, False), True, , SetAlpha(SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue), 127)
        DrawBevel m_imgSliderBuffer
        CopyImageToDC picGreenSlider.hDC, m_imgSliderBuffer.Rectangle, m_imgSliderBuffer
        picGreenSlider.Refresh
        m_imgSliderBuffer.TileBlit , m_imgColorBufferBG
        m_imgSliderBuffer.GradientFill m_imgSliderBuffer.Rectangle, _
        Array(SetBlue(NewColor, 0), SetBlue(NewColor, 255), "H"), RenderMode_SourceAlpha
        DrawBevel m_imgSliderBuffer, F2Rect(GetBlue(NewColor) * l_sngMul + 1, 1, 7, m_imgSliderBuffer.Height - 2, False), True, , SetAlpha(SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue), 127)
        DrawBevel m_imgSliderBuffer
        CopyImageToDC picBlueSlider.hDC, m_imgSliderBuffer.Rectangle, m_imgSliderBuffer
        picBlueSlider.Refresh
        m_imgSliderBuffer.TileBlit , m_imgColorBufferBG
        m_imgSliderBuffer.GradientFill m_imgSliderBuffer.Rectangle, _
        Array(SetAlpha(NewColor, 0), SetAlpha(NewColor, 255), "H"), RenderMode_SourceAlpha
        DrawBevel m_imgSliderBuffer, F2Rect(GetAlpha(NewColor) * l_sngMul + 1, 1, 7, m_imgSliderBuffer.Height - 2, False), True, , SetAlpha(SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue), 127)
        DrawBevel m_imgSliderBuffer
        CopyImageToDC picAlphaSlider.hDC, m_imgSliderBuffer.Rectangle, m_imgSliderBuffer
        picAlphaSlider.Refresh
    Case Else
    End Select
End Sub

Public Sub RefreshColors()
On Error Resume Next
    If m_imgColorBufferBG Is Nothing Then
        Set m_imgColorBufferBG = F2Image(2, 2)
        m_imgColorBufferBG.SetPixel 0, 0, F2RGB(220, 220, 220, 255)
        m_imgColorBufferBG.SetPixel 1, 1, F2RGB(220, 220, 220, 255)
        m_imgColorBufferBG.SetPixel 1, 0, F2RGB(96, 96, 96, 255)
        m_imgColorBufferBG.SetPixel 0, 1, F2RGB(96, 96, 96, 255)
        Set m_imgColorBufferBG = m_imgColorBufferBG.Resample(16, 16, ResampleMode_Linear)
    End If
    If m_imgColorBuffer Is Nothing Then
        Set m_imgColorBuffer = F2Image(picNewColor.ScaleWidth, picNewColor.ScaleHeight)
    End If
    m_imgColorBuffer.TileBlit , m_imgColorBufferBG
    m_imgColorBuffer.Fill m_imgColorBuffer.Rectangle, OldColor, RenderMode_SourceAlpha
    DrawBevel m_imgColorBuffer, , True
    CopyImageToDC picOldColor.hDC, m_imgColorBuffer.Rectangle, m_imgColorBuffer
    picOldColor.Refresh
    m_imgColorBuffer.TileBlit , m_imgColorBufferBG
    m_imgColorBuffer.Fill m_imgColorBuffer.Rectangle, NewColor, RenderMode_SourceAlpha
    DrawBevel m_imgColorBuffer, , True
    CopyImageToDC picNewColor.hDC, m_imgColorBuffer.Rectangle, m_imgColorBuffer
    picNewColor.Refresh
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Me.Hide
    Cancelled = True
    Done = True
End Sub

Private Sub cmdEffects_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Me.PopupMenu popEffects, , cmdEffects.Left, cmdEffects.Top + cmdEffects.Height
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
    Me.Hide
    Cancelled = False
    Done = True
End Sub

Private Sub dtTextual_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    RefreshTextual
End Sub

Private Sub dtVisual_TabSelected(theTab As vbalDTab6.cTab)
    RefreshVisual
End Sub

Private Sub Form_Activate()
On Error Resume Next
    RefreshAll
End Sub

Private Sub Form_Load()
On Error Resume Next
    dtVisual.Tabs.Add "Sliders", , "Sliders"
    dtVisual.Tabs.Add "Palette", , "Palette"
    dtVisual.Tabs.Add "Image", , "Image"
    dtTextual.Tabs.Add "RGB", , "RGB"
    dtTextual.Tabs.Add "HSL", , "HSL"
    dtTextual.Tabs.Add "Hex", , "Hex"
    RefreshAll
End Sub

Public Sub RefreshAll(Optional Override As Boolean = True)
On Error Resume Next
    RefreshTextual Override
    RefreshColors
    RefreshVisual
End Sub

Private Sub mnuGrayscale_Click()
On Error Resume Next
    NewColor = ColorToGrayscale(NewColor)
    RefreshAll
End Sub

Private Sub mnuInvertAll_Click()
On Error Resume Next
    NewColor = InvertColor(NewColor)
    RefreshAll
End Sub

Private Sub mnuInvertAlpha_Click()
On Error Resume Next
    NewColor = InvertChannel(NewColor, 3)
    RefreshAll
End Sub

Private Sub mnuInvertBlue_Click()
On Error Resume Next
    NewColor = InvertChannel(NewColor, 0)
    RefreshAll
End Sub

Private Sub mnuInvertGreen_Click()
On Error Resume Next
    NewColor = InvertChannel(NewColor, 1)
    RefreshAll
End Sub

Private Sub mnuInvertRed_Click()
On Error Resume Next
    NewColor = InvertChannel(NewColor, 2)
    RefreshAll
End Sub

Private Sub mnuInvertRGB_Click()
On Error Resume Next
    NewColor = InvertColorRGB(NewColor)
    RefreshAll
End Sub

Private Sub picRedSlider_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sngMul As Single
    l_sngMul = (m_imgSliderBuffer.Width - 9) / 255
    NewColor = SetRed(NewColor, (X - 4) / l_sngMul)
    RefreshAll
    DoEvents
End Sub

Private Sub picRedSlider_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button <> 0 Then picRedSlider_MouseDown Button, Shift, X, Y
End Sub

Private Sub picGreenSlider_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sngMul As Single
    l_sngMul = (m_imgSliderBuffer.Width - 9) / 255
    NewColor = SetGreen(NewColor, (X - 4) / l_sngMul)
    RefreshAll
    DoEvents
End Sub

Private Sub picGreenSlider_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button <> 0 Then picGreenSlider_MouseDown Button, Shift, X, Y
End Sub

Private Sub picBlueSlider_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sngMul As Single
    l_sngMul = (m_imgSliderBuffer.Width - 9) / 255
    NewColor = SetBlue(NewColor, (X - 4) / l_sngMul)
    RefreshAll
    DoEvents
End Sub

Private Sub picBlueSlider_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button <> 0 Then picBlueSlider_MouseDown Button, Shift, X, Y
End Sub

Private Sub picAlphaSlider_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sngMul As Single
    l_sngMul = (m_imgSliderBuffer.Width - 9) / 255
    NewColor = SetAlpha(NewColor, (X - 4) / l_sngMul)
    RefreshAll
    DoEvents
End Sub

Private Sub picAlphaSlider_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button <> 0 Then picAlphaSlider_MouseDown Button, Shift, X, Y
End Sub

Private Sub txtAlpha_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 1 Then Exit Sub
    NewColor = SetAlpha(NewColor, CLng(txtAlpha.Text))
    RefreshAll False
End Sub

Private Sub txtBlue_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 1 Then Exit Sub
    NewColor = SetBlue(NewColor, CLng(txtBlue.Text))
    RefreshAll False
End Sub

Private Sub txtGreen_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 1 Then Exit Sub
    NewColor = SetGreen(NewColor, CLng(txtGreen.Text))
    RefreshAll False
End Sub

Private Sub txtHSLAlpha_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 2 Then Exit Sub
    If txtHSLAlpha.Locked Then Exit Sub
    NewColor = SetAlpha(NewColor, CSng(Replace(txtHSLAlpha.Text, "%", "")) * 2.55)
    RefreshAll False
End Sub

Private Sub txtHSLAlpha_GotFocus()
On Error Resume Next
    txtHSLAlpha.Locked = False
End Sub

Private Sub txtHSLAlpha_LostFocus()
On Error Resume Next
    txtHSLAlpha.Locked = True
End Sub

Private Sub txtHue_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 2 Then Exit Sub
    If txtHue.Locked Then Exit Sub
    RecalcHSL
End Sub

Private Sub txtHue_GotFocus()
On Error Resume Next
    txtHue.Locked = False
End Sub

Private Sub txtHue_LostFocus()
On Error Resume Next
    txtHue.Locked = True
End Sub

Private Sub txtLuminance_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 2 Then Exit Sub
    If txtLuminance.Locked Then Exit Sub
    RecalcHSL
End Sub

Private Sub txtLuminance_GotFocus()
On Error Resume Next
    txtLuminance.Locked = False
End Sub

Private Sub txtLuminance_LostFocus()
On Error Resume Next
    txtLuminance.Locked = True
End Sub

Private Sub txtRed_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 1 Then Exit Sub
    NewColor = SetRed(NewColor, CLng(txtRed.Text))
    RefreshAll False
End Sub

Private Sub txtAlphaHex_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 3 Then Exit Sub
    NewColor = SetAlpha(NewColor, CLng("&H" & txtAlphaHex.Text))
    RefreshAll False
End Sub

Private Sub txtBlueHex_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 3 Then Exit Sub
    NewColor = SetBlue(NewColor, CLng("&H" & txtBlueHex.Text))
    RefreshAll False
End Sub

Private Sub txtGreenHex_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 3 Then Exit Sub
    NewColor = SetGreen(NewColor, CLng("&H" & txtGreenHex.Text))
    RefreshAll False
End Sub

Private Sub txtRedHex_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 3 Then Exit Sub
    NewColor = SetRed(NewColor, CLng("&H" & txtRedHex.Text))
    RefreshAll False
End Sub

Private Sub txtSaturation_Change()
On Error Resume Next
    If dtTextual.SelectedTab.Index <> 2 Then Exit Sub
    If txtSaturation.Locked Then Exit Sub
    RecalcHSL
End Sub

Private Sub txtSaturation_GotFocus()
On Error Resume Next
    txtSaturation.Locked = False
End Sub

Private Sub txtSaturation_LostFocus()
On Error Resume Next
    txtSaturation.Locked = True
End Sub

