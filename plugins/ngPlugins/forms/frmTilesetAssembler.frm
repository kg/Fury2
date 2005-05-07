VERSION 5.00
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Begin VB.Form frmTilesetAssembler 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Tileset Assembler"
   ClientHeight    =   3990
   ClientLeft      =   45
   ClientTop       =   345
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
   Icon            =   "frmTilesetAssembler.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   266
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   391
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.PictureBox picTab 
      BorderStyle     =   0  'None
      Height          =   3540
      Index           =   0
      Left            =   60
      ScaleHeight     =   236
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   280
      TabIndex        =   2
      Top             =   390
      Visible         =   0   'False
      Width           =   4200
      Begin VB.PictureBox picTiles 
         AutoRedraw      =   -1  'True
         Height          =   3105
         Left            =   15
         ScaleHeight     =   203
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   274
         TabIndex        =   3
         Top             =   15
         Width           =   4170
         Begin cFScroll.FlatScrollBar vsTiles 
            Height          =   3045
            Left            =   3855
            TabIndex        =   4
            Top             =   0
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   5371
            Orientation     =   1
            Max             =   100
            Style           =   -1
         End
      End
      Begin VB.CommandButton cmdAddTiles 
         Caption         =   "&Add Tiles..."
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
         Left            =   15
         TabIndex        =   5
         Top             =   3135
         Width           =   1950
      End
      Begin VB.CommandButton cmdRemove 
         Caption         =   "&Remove Selected"
         Enabled         =   0   'False
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
         Left            =   2235
         TabIndex        =   6
         Top             =   3135
         Width           =   1950
      End
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
      TabIndex        =   7
      Top             =   480
      Width           =   1500
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "Assemble"
      Enabled         =   0   'False
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
      TabIndex        =   1
      Top             =   30
      Width           =   1500
   End
   Begin vbalDTab6.vbalDTabControl dtTabs 
      Height          =   3930
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4260
      _ExtentX        =   7514
      _ExtentY        =   6932
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
   End
End
Attribute VB_Name = "frmTilesetAssembler"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
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
Public Editor As Object
Private m_colTiles As New Engine.Fury2Collection
Private m_booSelection() As Boolean

Public Sub RedrawTiles()
On Error Resume Next
Dim l_lngY As Long
Dim l_lngHeight As Long
Dim l_lngTile As Long
Dim l_imgTile As Fury2Image
Dim l_rctText As Win32.RECT
Dim l_lngMax As Long
    If m_colTiles.Count > 0 Then
        ReDim Preserve m_booSelection(1 To m_colTiles.Count)
    Else
        Erase m_booSelection
    End If
    picTiles.Cls
    l_lngY = -vsTiles.Value
    If m_colTiles.Count > 0 Then
        For l_lngTile = 1 To m_colTiles.Count
            Set l_imgTile = Nothing
            Set l_imgTile = m_colTiles(l_lngTile).Duplicate
            If m_booSelection(l_lngTile) Then
                If l_imgTile.AlphaChannel Then l_imgTile.Composite BlendColors(SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue), SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127)
                picTiles.Line (0, l_lngY)-(picTiles.ScaleWidth, l_lngY + (l_imgTile.Height + 4)), GetSystemColor(SystemColor_Highlight), BF
            Else
                If l_imgTile.AlphaChannel Then l_imgTile.Composite SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
            End If
            With l_imgTile
                CopyImageToDC picTiles.hdc, F2Rect(2, l_lngY + 2, .Width, .Height, False), l_imgTile
                
                picTiles.Font.Bold = True
                With l_rctText
                    .Left = l_imgTile.Width + 4
                    .Top = l_lngY + 2
                    .Right = picTiles.ScaleWidth - (IIf(vsTiles.Visible, vsTiles.Width, 0) + 2)
                    .Bottom = .Top + l_imgTile.Height
                End With
                Win32.DrawText picTiles.hdc, .Name, Len(.Name), l_rctText, DrawText_Align_Top Or DrawText_Align_Left Or DrawText_Wrap_WordBreak
                picTiles.Font.Bold = False
            
                l_lngY = l_lngY + .Height + 4
                l_lngHeight = l_lngHeight + .Height + 4
            End With
        Next l_lngTile
        l_lngMax = l_lngHeight - picTiles.ScaleHeight
        If (l_lngMax > 0) Then
            vsTiles.Enabled = True
            vsTiles.Max = l_lngMax
        Else
            vsTiles.Enabled = False
            vsTiles.Value = 0
        End If
    End If
    picTiles.Refresh
End Sub

Private Sub cmdAddTiles_Click()
On Error Resume Next
Dim l_varFilenames As Variant
Dim l_lngImages As Long
Dim l_imgImage As Fury2Image
Dim l_strErrors As String
    l_varFilenames = SelectFiles("Images|" + libGraphics.SupportedGraphicsFormats, "Select Tiles...", True)
    For l_lngImages = LBound(l_varFilenames) To UBound(l_varFilenames)
        Set l_imgImage = Nothing
        Set l_imgImage = F2LoadImage(l_varFilenames(l_lngImages))
        If (l_imgImage Is Nothing) Or (l_imgImage.Width < 1) Or (l_imgImage.Height < 1) Then
            l_strErrors = l_strErrors + "Unable to load """ + l_varFilenames(l_lngImages) + """." + vbCrLf
        End If
        m_colTiles.Add l_imgImage, l_imgImage.Name
    Next l_lngImages
    If Len(l_strErrors) > 0 Then
        MsgBox "Errors occurred while loading images:" & vbCrLf & l_strErrors, vbExclamation, "Error"
    End If
    RedrawTiles
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Me.Hide
    Unload Me
End Sub

Private Sub dtTabs_TabSelected(TheTab As vbalDTab6.cTab)
On Error Resume Next
Dim l_lngTabs As Long
    For l_lngTabs = picTab.LBound To picTab.UBound
        If l_lngTabs = TheTab.Index - 1 Then
            picTab(l_lngTabs).Visible = True
        Else
            picTab(l_lngTabs).Visible = False
        End If
    Next l_lngTabs
End Sub

Private Sub Form_Load()
On Error Resume Next
    dtTabs.Tabs.Add "Tiles", , "Tiles"
    dtTabs.Tabs.Add "Options", , "Options"
End Sub

Private Sub vsTiles_Change()
    RedrawTiles
End Sub
