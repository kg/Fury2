VERSION 5.00
Begin VB.Form frmTrace 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Trace"
   ClientHeight    =   3270
   ClientLeft      =   60
   ClientTop       =   330
   ClientWidth     =   4425
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmTrace.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   218
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   295
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Visible         =   0   'False
   Begin VB.PictureBox picButtons 
      Align           =   2  'Align Bottom
      BorderStyle     =   0  'None
      Height          =   315
      Left            =   0
      ScaleHeight     =   315
      ScaleWidth      =   4425
      TabIndex        =   1
      Top             =   2955
      Width           =   4425
      Begin VB.CheckBox chkTraceContext 
         Caption         =   "Trace Context"
         Height          =   315
         Left            =   3030
         TabIndex        =   6
         Top             =   0
         Width           =   1335
      End
      Begin VB.CommandButton cmdGo 
         Caption         =   "&Go"
         Enabled         =   0   'False
         Height          =   315
         Left            =   2250
         TabIndex        =   5
         Top             =   0
         Width           =   750
      End
      Begin VB.CommandButton cmdStop 
         Caption         =   "S&top"
         Height          =   315
         Left            =   1500
         TabIndex        =   4
         Top             =   0
         Width           =   750
      End
      Begin VB.CommandButton cmdSave 
         Caption         =   "&Save"
         Enabled         =   0   'False
         Height          =   315
         Left            =   750
         TabIndex        =   3
         Top             =   0
         Width           =   750
      End
      Begin VB.CommandButton cmdClear 
         Caption         =   "&Clear"
         Height          =   315
         Left            =   0
         TabIndex        =   2
         Top             =   0
         Width           =   750
      End
   End
   Begin VB.TextBox txtTrace 
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2940
      Left            =   0
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   0
      Width           =   4440
   End
End
Attribute VB_Name = "frmTrace"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    Engine (Fury² Game Creation System Runtime Engine)
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

Private Sub chkTraceContext_Click()
    m_booTrace = CBool(chkTraceContext.Value)
End Sub

Private Sub cmdClear_Click()
    txtTrace.Text = ""
End Sub

Private Sub cmdGo_Click()
On Error Resume Next
    m_Engine.Paused = False
    cmdGo.Enabled = False
    cmdStop.Enabled = True
End Sub

Private Sub cmdStop_Click()
On Error Resume Next
    m_Engine.Paused = True
    cmdGo.Enabled = True
    cmdStop.Enabled = False
End Sub

Private Sub Form_Resize()
    txtTrace.Move 0, 0, Me.ScaleWidth, Me.ScaleHeight - picButtons.Height
End Sub
