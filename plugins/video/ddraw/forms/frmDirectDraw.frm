VERSION 5.00
Begin VB.Form frmDirectDraw 
   BackColor       =   &H00000000&
   ClientHeight    =   3600
   ClientLeft      =   60
   ClientTop       =   60
   ClientWidth     =   4800
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
   Icon            =   "frmDirectDraw.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.PictureBox picBackbuffer 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   3600
      Left            =   0
      ScaleHeight     =   240
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   320
      TabIndex        =   0
      Top             =   0
      Width           =   4800
   End
End
Attribute VB_Name = "frmDirectDraw"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
