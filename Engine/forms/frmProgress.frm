VERSION 5.00
Begin VB.Form frmProgress 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Processing In Progress"
   ClientHeight    =   615
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4230
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
   Icon            =   "frmProgress.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   41
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   282
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picProgress 
      AutoRedraw      =   -1  'True
      FillColor       =   &H8000000D&
      Height          =   330
      Left            =   30
      ScaleHeight     =   18
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   275
      TabIndex        =   1
      Top             =   270
      Width           =   4185
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Please Wait"
      Height          =   195
      Left            =   45
      TabIndex        =   0
      Top             =   45
      Width           =   840
   End
End
Attribute VB_Name = "frmProgress"
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

Sub SetProgress(Amount As Single)
On Error Resume Next
    picProgress.Cls
    picProgress.Line (0, 0)-(picProgress.ScaleWidth * (Amount / 100), picProgress.ScaleHeight), picProgress.FillColor, BF
    picProgress.Refresh
    DoEvents
End Sub
