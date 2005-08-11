VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#12.7#0"; "ngUI.ocx"
Begin VB.Form frmSpriteImporter 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Sprite Importer"
   ClientHeight    =   5250
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   7500
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmSpriteImporter.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   350
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   500
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.PictureBox picPreview 
      Height          =   4785
      Left            =   75
      ScaleHeight     =   315
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   384
      TabIndex        =   3
      Top             =   390
      Width           =   5820
   End
   Begin ngPlugins.ObjectInspector insSettings 
      Height          =   4800
      Left            =   75
      TabIndex        =   2
      Top             =   375
      Width           =   5820
      _ExtentX        =   10266
      _ExtentY        =   8467
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
      Left            =   5970
      TabIndex        =   1
      Top             =   480
      Width           =   1500
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
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
      Left            =   5970
      TabIndex        =   0
      Top             =   30
      Width           =   1500
   End
   Begin ngUI.ngTabStrip tsViews 
      Height          =   5190
      Left            =   30
      TabIndex        =   4
      Top             =   30
      Width           =   5910
      _ExtentX        =   10425
      _ExtentY        =   9155
   End
End
Attribute VB_Name = "frmSpriteImporter"
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
Private m_imgPreview As Fury2Image
Private m_imgSource As Fury2Image
Public Options As SpriteImporterOptions

Public Sub RefreshPreview()
On Error Resume Next
Dim l_rctFrame As Fury2Rect
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngCellsX As Long, l_lngCellsY As Long
Dim l_lngX As Long, l_lngY As Long
    Set m_imgPreview = F2Image(picPreview.ScaleWidth, picPreview.ScaleHeight)
    m_imgPreview.Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
    With Options
        If m_imgSource Is Nothing Then
            Set m_imgSource = DefaultEngine.LoadImage(Options.Image, True)
        End If
        l_lngWidth = m_imgSource.Width - .MarginLeft - .MarginRight
        l_lngHeight = m_imgSource.Height - .MarginTop - .MarginBottom
        l_lngCellsX = ClipValue(l_lngWidth \ (.CellWidth + .GridWidth), 0, .MaxFrames)
        l_lngCellsY = ClipValue(l_lngHeight \ (.CellHeight + .GridHeight), 0, .MaxPoses)
        For l_lngY = 0 To l_lngCellsY - 1
            For l_lngX = 0 To l_lngCellsX - 1
                Set l_rctFrame = F2Rect(l_lngX * (.CellWidth + .GridWidth) + .MarginLeft, l_lngY * (.CellHeight + .GridHeight) + .MarginTop, .CellWidth, .CellHeight, False)
                m_imgPreview.Blit F2Rect(l_lngX * (.CellWidth + 1), l_lngY * (.CellHeight + 1), .CellWidth, .CellHeight, False), l_rctFrame, m_imgSource
            Next l_lngX
        Next l_lngY
    End With
    picPreview_Paint
End Sub

Public Sub RefreshSettings()
On Error Resume Next
    insSettings.Inspect Options, "Settings", True
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Set Options = Nothing
    Me.Hide
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
    Me.Hide
End Sub

Private Sub tsViews_TabSelected(TheTab As ngTab)
On Error Resume Next
    Select Case TheTab.Index
    Case 1
        picPreview.Visible = False
        insSettings.Visible = True
        Set m_imgSource = Nothing
        RefreshSettings
    Case 2
        picPreview.Visible = True
        insSettings.Visible = False
        RefreshPreview
    Case Else
    End Select
End Sub

Private Sub Form_Load()
On Error Resume Next
    Set Options = New SpriteImporterOptions
    tsViews.Tabs.AddNew "Options"
    tsViews.Tabs.AddNew "Preview"
    picPreview.Move tsViews.Left + 2, tsViews.Top + tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - tsViews.IdealHeight - 3
    insSettings.Move tsViews.Left + 2, tsViews.Top + tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - tsViews.IdealHeight - 3
End Sub

Private Sub picPreview_Paint()
On Error Resume Next
    CopyImageToDC picPreview.hdc, m_imgPreview.Rectangle, m_imgPreview
End Sub
