VERSION 5.00
Begin VB.Form frmCriticalError 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Critical Error!"
   ClientHeight    =   3000
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4500
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
   Icon            =   "frmCriticalError.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   200
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   300
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame fraError 
      Caption         =   "Error Info:"
      Height          =   2070
      Left            =   30
      TabIndex        =   2
      Top             =   450
      Width           =   4440
      Begin VB.Label lblInfo 
         Height          =   1785
         Left            =   60
         TabIndex        =   3
         Top             =   210
         Width           =   4305
         WordWrap        =   -1  'True
      End
   End
   Begin VB.CommandButton cmdExitNow 
      Cancel          =   -1  'True
      Caption         =   "Shut Down"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   30
      TabIndex        =   1
      Top             =   2550
      Width           =   4440
   End
   Begin VB.Label lblCritError 
      AutoSize        =   -1  'True
      Caption         =   "A critical error has occurred. Please review the error information and then click the 'shut down' button."
      Height          =   390
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4440
      WordWrap        =   -1  'True
   End
End
Attribute VB_Name = "frmCriticalError"
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

Private Sub cmdExitNow_Click()
On Error Resume Next
    m_booCritical = False
    Shutdown
End Sub

Private Sub Form_Load()
On Error Resume Next
End Sub
