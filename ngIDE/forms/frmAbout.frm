VERSION 5.00
Begin VB.Form frmAbout 
   AutoRedraw      =   -1  'True
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "About Fury²"
   ClientHeight    =   4500
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   6000
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   14.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmAbout.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Picture         =   "frmAbout.frx":000C
   ScaleHeight     =   300
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   400
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrRedraw 
      Interval        =   50
      Left            =   2400
      Top             =   2010
   End
   Begin VB.Image imgOverlay 
      Height          =   2250
      Left            =   2985
      Picture         =   "frmAbout.frx":1103B
      Stretch         =   -1  'True
      Top             =   15
      Visible         =   0   'False
      Width           =   3000
   End
End
Attribute VB_Name = "frmAbout"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private m_imgBuffer As Fury2Image
Private m_imgBackground As Fury2Image
Private m_imgOverlay As Fury2Image
Private m_imgText As Fury2Image
Private m_imgTopFade As Fury2Image
Private m_imgBottomFade As Fury2Image
Private m_fntFont As Fury2Font
Private m_lngYOffset As Long
Private m_lngYOffset2 As Long
Private m_lngCreditsY As Long
'Private m_filShadow As Fury2ConvolutionFilter
Private m_strText() As String

Private Sub Form_Load()
Dim l_vfFile As VirtualFile
    Screen.MousePointer = 11
    m_lngCreditsY = 300
    F2Init
    Set m_imgBuffer = F2Image(400, 300)
    Set m_imgBackground = F2ImageFromPicture(Me.Picture)
    Set m_imgOverlay = F2ImageFromPicture(imgOverlay.Picture)
    Set m_imgText = F2Image(360, 260)
    Set m_imgTopFade = F2Image(360, 20)
    m_imgTopFade.GradientFill m_imgTopFade.Rectangle, Array(F2White, F2White, F2Black, F2Black)
    Set m_imgBottomFade = m_imgTopFade.Duplicate
    m_imgBottomFade.Flip
    Set Me.Picture = Nothing
    Set imgOverlay.Picture = Nothing
    Set m_fntFont = g_engEngine.F2Font
    m_fntFont.ImportTTF Me.Font
    m_fntFont.Color = F2White
    m_fntFont.AddOutline F2Black, 1
    m_fntFont.ShadowColor = F2RGB(0, 0, 0, 192)
    m_fntFont.Merge = True
'    Set m_filShadow = F2GaussianBlurFilter(2)
    m_strText = Split(StrConv(LoadResData("CREDITS", "TEXT"), vbUnicode), vbCrLf)
    Screen.MousePointer = 0
End Sub

Private Sub tmrRedraw_Timer()
On Error Resume Next
Dim l_lngLines As Long, l_lngY As Long
    m_lngYOffset = (m_lngYOffset + 2) Mod 300
    m_lngYOffset2 = (m_lngYOffset2 + 1) Mod 300
    m_lngCreditsY = m_lngCreditsY - 1
    m_imgText.Clear 0
    m_imgBuffer.Copy m_imgBackground
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset2, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Burn
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset2 + 300, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Burn
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Dodge
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset + 300, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Dodge
    l_lngY = m_lngCreditsY
    For l_lngLines = LBound(m_strText) To UBound(m_strText)
        If Len(Trim(m_strText(l_lngLines))) > 0 Then
            m_fntFont.DrawCentered m_imgText, m_strText(l_lngLines), F2Rect(0, l_lngY, m_imgText.Width, m_fntFont.FullHeight, False), F2White
        End If
        l_lngY = l_lngY + m_fntFont.FullHeight
    Next l_lngLines
    m_imgText.Blit F2Rect(0, 0, 360, 20, False), , m_imgTopFade, , BlitMode_Erase
    m_imgText.Blit F2Rect(0, 260 - 20, 360, 20, False), , m_imgBottomFade, , BlitMode_Erase
'    m_filShadow.Filter m_imgBuffer, m_imgText, F2Rect(20, 127, 360, 155, False), m_imgText.Rectangle, RenderMode_Shadow, F2Black
    m_imgBuffer.Blit F2Rect(20, 20, 360, 260, False), , m_imgText, 1, BlitMode_SourceAlpha
    CopyImageToDC Me.hdc, m_imgBuffer.Rectangle, m_imgBuffer
    Me.Refresh
End Sub
