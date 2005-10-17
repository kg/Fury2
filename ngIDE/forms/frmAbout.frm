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
      Size            =   12
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
   MousePointer    =   7  'Size N S
   ScaleHeight     =   300
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   400
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrRedraw 
      Interval        =   50
      Left            =   2400
      Top             =   2010
   End
End
Attribute VB_Name = "frmAbout"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngIDE (Fury² Game Creation System Next-Generation Editor)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Private m_imgBuffer As Fury2Image
Private m_imgBackground As Fury2Image
Private m_imgOverlay As Fury2Image
Private m_imgLogo As Fury2Image
Private m_imgText As Fury2Image
Private m_imgTopFade As Fury2Image
Private m_imgBottomFade As Fury2Image
Private m_fntFont As Fury2RasterFont
Private m_lngYOffset As Long
Private m_lngYOffset2 As Long
Private m_lngHeight As Long
Private m_lngCreditsY As Long
Private m_strText As String
Private m_booAutoScroll As Boolean
Private m_lngDragStartY As Long, m_lngDragStartOffset As Long

Private Sub Form_Load()
On Error Resume Next
Dim l_vfFile As VirtualFile
    Screen.MousePointer = 11
    m_lngCreditsY = 260
    F2Init
    Set m_imgBuffer = F2Image(400, 300)
    Set m_imgBackground = g_edEditor.Resources.ItemData("about\background.jpg")
    Set m_imgOverlay = g_edEditor.Resources.ItemData("about\overlay.jpg")
    Set m_imgLogo = g_edEditor.Resources.ItemData("about\logo.png")
    Set m_imgText = F2Image(360, 260)
    Set m_imgTopFade = F2Image(360, 20)
    m_imgTopFade.GradientFill m_imgTopFade.Rectangle, Array(F2White, F2White, F2Black, F2Black)
    Set m_imgBottomFade = m_imgTopFade.Duplicate
    m_imgBottomFade.Flip
    Set m_fntFont = New Fury2RasterFont
    m_fntFont.ImportTTF Me.Font
    m_fntFont.Color = F2White
    m_fntFont.AddOutline F2Black, 1
    m_fntFont.ShadowColor = F2RGB(0, 0, 0, 192)
    m_fntFont.Merge = True
    m_strText = CStr(g_edEditor.Resources.ItemData("about\credits.txt"))
    Screen.MousePointer = 0
    m_booAutoScroll = True
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button = 1 Then
        m_booAutoScroll = False
        m_lngDragStartY = Y
        m_lngDragStartOffset = m_lngCreditsY
    End If
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button = 1 Then
        m_lngCreditsY = ((Y - m_lngDragStartY)) + m_lngDragStartOffset
        Render
        DoEvents
    End If
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_booAutoScroll = True
End Sub

Private Sub tmrRedraw_Timer()
On Error Resume Next
    m_lngYOffset = (m_lngYOffset + 2) Mod 300
    m_lngYOffset2 = (m_lngYOffset2 + 1) Mod 300
    If m_booAutoScroll Then
        m_lngCreditsY = m_lngCreditsY - 1
    End If
    If (m_lngCreditsY > 260) Then m_lngCreditsY = 260
    If (m_lngCreditsY < -m_lngHeight) Then m_lngCreditsY = 260
    Render
End Sub

Private Sub Render()
On Error Resume Next
Static l_booHere As Boolean
Dim l_lngY As Long
    If l_booHere Then Exit Sub
    l_booHere = True
    m_imgText.Clear 0
    m_imgBuffer.Copy m_imgBackground
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset2, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Burn
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset2 + 300, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Burn
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Dodge
    m_imgBuffer.Blit F2Rect(0, -m_lngYOffset + 300, 400, 300, False), , m_imgOverlay, 0.33, BlitMode_Dodge
    l_lngY = m_lngCreditsY
    m_fntFont.Draw m_imgText, m_strText, F2Rect(0, 0, m_imgText.Width, m_imgText.Height), F2White, 1, m_fntFont.Options(0, l_lngY), m_lngHeight
    m_imgText.Blit F2Rect(0, 0, 360, 20, False), , m_imgTopFade, , BlitMode_Erase
    m_imgText.Blit F2Rect(0, 260 - 20, 360, 20, False), , m_imgBottomFade, , BlitMode_Erase
    m_imgBuffer.Blit F2Rect(20, 20, 360, 260, False), , m_imgText, 1, BlitMode_SourceAlpha
    m_imgBuffer.Blit F2Rect(m_imgBuffer.Width - m_imgLogo.Width - 5, m_imgBuffer.Height - m_imgLogo.Height, m_imgLogo.Width, m_imgLogo.Height, False), , m_imgLogo, , BlitMode_SourceAlpha
    CopyImageToDC Me.hdc, m_imgBuffer.Rectangle, m_imgBuffer
    Me.Refresh
    l_booHere = False
End Sub
