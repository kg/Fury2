VERSION 5.00
Begin VB.Form frmImportTTF 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Import TrueType Font"
   ClientHeight    =   3990
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   5865
   Icon            =   "frmImportTTF.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   266
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   391
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
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
      Left            =   4335
      TabIndex        =   2
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
      TabIndex        =   1
      Top             =   480
      Width           =   1500
   End
   Begin ngPlugins.ObjectInspector insSettings 
      Height          =   3930
      Left            =   15
      TabIndex        =   0
      Top             =   30
      Width           =   4275
      _ExtentX        =   7541
      _ExtentY        =   6932
   End
End
Attribute VB_Name = "frmImportTTF"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngPlugins (Fury� Game Creation System Next-Generation Editor Standard Plugin Set)
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
Public Options As ImportTTFOptions

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

Private Sub Form_Load()
On Error Resume Next
    Set Options = New ImportTTFOptions
End Sub

