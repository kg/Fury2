VERSION 5.00
Begin VB.Form frmConfigure 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Configure Fury�"
   ClientHeight    =   1815
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4680
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmConfigure.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   121
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
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
      Height          =   405
      Left            =   3450
      TabIndex        =   6
      Top             =   1380
      Width           =   1200
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
      Height          =   405
      Left            =   30
      TabIndex        =   5
      Top             =   1380
      Width           =   1200
   End
   Begin VB.Frame fraSound 
      Caption         =   "Sound"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   270
      Left            =   30
      TabIndex        =   3
      Top             =   1080
      Width           =   4620
      Begin VB.PictureBox picEnableSound 
         BorderStyle     =   0  'None
         Height          =   195
         Left            =   3765
         ScaleHeight     =   195
         ScaleWidth      =   780
         TabIndex        =   9
         Top             =   0
         Width           =   780
         Begin VB.CheckBox chkEnableSound 
            Alignment       =   1  'Right Justify
            Caption         =   "Enable"
            Height          =   195
            Left            =   0
            TabIndex        =   4
            Top             =   0
            Value           =   1  'Checked
            Width           =   780
         End
      End
   End
   Begin VB.Frame fraGraphics 
      Caption         =   "Graphics"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1050
      Left            =   30
      TabIndex        =   0
      Top             =   0
      Width           =   4620
      Begin VB.ComboBox cmbDisplayMode 
         Enabled         =   0   'False
         Height          =   315
         ItemData        =   "frmConfigure.frx":492A
         Left            =   1470
         List            =   "frmConfigure.frx":4949
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   615
         Width           =   3045
      End
      Begin VB.ComboBox cmbGraphicsPlugin 
         Height          =   315
         ItemData        =   "frmConfigure.frx":499A
         Left            =   1470
         List            =   "frmConfigure.frx":49A1
         Style           =   2  'Dropdown List
         TabIndex        =   1
         Top             =   255
         Width           =   3045
      End
      Begin VB.Label lblDisplayMode 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Display Mode:"
         Enabled         =   0   'False
         Height          =   195
         Left            =   435
         TabIndex        =   8
         Top             =   675
         Width           =   1005
      End
      Begin VB.Label lblOutputPlugin 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Output Plugin:"
         Height          =   195
         Left            =   405
         TabIndex        =   7
         Top             =   315
         Width           =   1035
      End
   End
End
Attribute VB_Name = "frmConfigure"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    Engine (Fury� Game Creation System Runtime Engine)
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

Public Cancelled As Boolean
Public Engine As Fury2Engine

Sub LoadSettings()
On Error Resume Next
Dim l_strPlugin As String
    With Engine
        cmbGraphicsPlugin.Clear
        If m_booIDE Then
            l_strPlugin = Dir(App.Path & "\..\sys\video_*.dll")
        Else
            l_strPlugin = Dir(App.Path & "\video_*.dll")
        End If
        Do While l_strPlugin <> ""
            cmbGraphicsPlugin.AddItem Replace(Replace(l_strPlugin, "Video_", "", , , vbTextCompare), ".dll", "", , , vbTextCompare)
            l_strPlugin = Dir
        Loop
        If m_booIDE Then
            cmbGraphicsPlugin.Text = "GDI"
        Else
            cmbGraphicsPlugin.Text = .OutputPlugin
        End If
        Err.Clear
        chkEnableSound.Value = Abs(CInt(Not CBool(.DisableSound)))
        cmbDisplayMode.Text = "Default"
        cmbBitDepth.Text = "Default"
    End With
End Sub

Sub SaveSettings()
On Error Resume Next
    With Engine
        .OutputPlugin = cmbGraphicsPlugin.Text
        If InStr(cmbDisplayMode.Text, "Windowed") Then
            .Fullscreen = False
            .ScreenScaleRatio = CSng(Replace(Replace(cmbDisplayMode.Text, "x", ""), "Windowed", ""))
        ElseIf cmbDisplayMode.Text = "Default" Then
        Else
            .Fullscreen = True
            .ScreenScaleRatio = CSng(Replace(cmbDisplayMode.Text, "x", ""))
        End If
        .DisableSound = Not CBool(chkEnableSound.Value)
    End With
End Sub

Private Sub cmbGraphicsPlugin_Change()
    If LCase(Trim(cmbGraphicsPlugin.Text)) = "GDI" Then
        cmbDisplayMode.Enabled = False
        lblDisplayMode.Enabled = False
    Else
        cmbDisplayMode.Enabled = True
        lblDisplayMode.Enabled = True
    End If
End Sub

Private Sub cmbGraphicsPlugin_Click()
    cmbGraphicsPlugin_Change
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Cancelled = True
    Me.Hide
End Sub

Private Sub cmdOK_Click()
    SaveSettings
    Me.Hide
End Sub

Private Sub Form_Load()
    Cancelled = False
    SetAppIcon Me
'    LoadSettings
End Sub
