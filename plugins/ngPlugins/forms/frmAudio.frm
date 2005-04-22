VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Begin VB.Form frmAudio 
   BorderStyle     =   0  'None
   Caption         =   "Loading"
   ClientHeight    =   3180
   ClientLeft      =   0
   ClientTop       =   -15
   ClientWidth     =   4665
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
   Icon            =   "frmAudio.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   212
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   311
   ShowInTaskbar   =   0   'False
   Begin VB.Frame fraPlayback 
      Caption         =   "Playback"
      Height          =   615
      Left            =   30
      TabIndex        =   5
      Top             =   975
      Width           =   4590
      Begin VB.PictureBox picPlaybackButtons 
         BorderStyle     =   0  'None
         Height          =   330
         Left            =   60
         ScaleHeight     =   330
         ScaleWidth      =   3045
         TabIndex        =   6
         Top             =   210
         Width           =   3045
         Begin VB.CommandButton cmdPlay 
            Caption         =   "&Play"
            Height          =   330
            Left            =   0
            TabIndex        =   7
            Top             =   0
            Width           =   1000
         End
         Begin VB.CommandButton cmdPause 
            Caption         =   "Pa&use"
            Height          =   330
            Left            =   1020
            TabIndex        =   8
            Top             =   0
            Width           =   1000
         End
         Begin VB.CommandButton cmdRewind 
            Caption         =   "&Rewind"
            Height          =   330
            Left            =   2040
            TabIndex        =   9
            Top             =   0
            Width           =   1000
         End
      End
   End
   Begin VB.Frame fraGeneral 
      Caption         =   "General"
      Height          =   930
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4605
      Begin VB.TextBox txtFileName 
         BackColor       =   &H8000000F&
         Height          =   300
         Left            =   1005
         Locked          =   -1  'True
         TabIndex        =   2
         Top             =   210
         Width           =   3495
      End
      Begin VB.TextBox txtFileType 
         BackColor       =   &H8000000F&
         Height          =   300
         Left            =   1005
         Locked          =   -1  'True
         TabIndex        =   4
         Top             =   525
         Width           =   3495
      End
      Begin VB.Label lblName 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Name:"
         Height          =   195
         Left            =   480
         TabIndex        =   1
         Top             =   255
         Width           =   465
      End
      Begin VB.Label lblType 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Type:"
         Height          =   195
         Left            =   525
         TabIndex        =   3
         Top             =   570
         Width           =   420
      End
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   15
      Top             =   2670
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmAudio"
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
Implements iExtendedForm
Implements iDocument

Private m_hndHandle As SoundHandle
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
End Property

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Public Sub RefreshInfo()
On Error Resume Next
Dim l_sngLength As Single
    txtFileName.Text = GetTitle(m_hndHandle.Name)
    With txtFileType
        Select Case LCase(Trim(GetExtension(m_hndHandle.Name)))
        Case "wav"
            .Text = "RIFF Wave Audio"
        Case "aif", "aiff"
            .Text = "AIFF Audio"
        Case "mp3", "mp2"
            .Text = "MPEG Layer 3 Audio"
        Case "ogg"
            .Text = "Ogg Vorbis Audio"
        Case "wma"
            .Text = "Windows Media Audio"
        Case "xm"
            .Text = "XM Module"
        Case "oxm"
            .Text = "XM Module with Ogg Vorbis Samples"
        Case "s3m"
            .Text = "ScreamTracker 3 Module"
        Case "it"
            .Text = "Impulse Tracker Module"
        Case "mod"
            .Text = "Module"
        Case "mid", "midi", "rmi"
            .Text = "MIDI File"
        Case Else
            .Text = "Unknown Type"
        End Select
    End With
End Sub

Private Sub cmdPause_Click()
On Error Resume Next
    m_hndHandle.Paused = Not m_hndHandle.Paused
End Sub

Private Sub cmdPlay_Click()
On Error Resume Next
    If m_hndHandle.Playing Then
        cmdPlay.Caption = "&Play"
        m_hndHandle.Kill
    Else
        cmdPlay.Caption = "&Stop"
        m_hndHandle.Play
    End If
End Sub

Private Sub cmdRewind_Click()
On Error Resume Next
    m_hndHandle.Kill
    m_hndHandle.Play
End Sub

Private Sub Form_Activate()
On Error Resume Next
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    m_hndHandle.Free
End Sub

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = m_fpgPlugin
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
    Set m_fpgPlugin = RHS
End Property

Friend Sub SetHandle(Handle As SoundHandle)
    Set m_hndHandle = Handle
    RefreshInfo
End Sub

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = GetTitle(Name)
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
End Sub

Private Sub Form_Resize()
On Error Resume Next
    fraGeneral.Move 2, 5, Me.ScaleWidth - 4, fraGeneral.Height
    fraPlayback.Move 2, fraGeneral.Top + fraGeneral.Height + 2, Me.ScaleWidth - 4, fraPlayback.Height
    txtFileName.Width = (fraGeneral.Width * Screen.TwipsPerPixelX) - (txtFileName.Left + (7 * Screen.TwipsPerPixelX))
    txtFileType.Width = txtFileName.Width
End Sub

Private Property Get iDocument_CanSave() As Boolean
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = m_strFilename
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Audio File"
End Property

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = False
End Property

