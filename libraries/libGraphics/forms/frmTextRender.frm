VERSION 5.00
Begin VB.Form frmTextRender 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Offscreen Text Renderer (GDI SUCKS!)"
   ClientHeight    =   90
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   90
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   FontTransparent =   0   'False
   ForeColor       =   &H00FFFFFF&
   LinkTopic       =   "Form1"
   ScaleHeight     =   6
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   6
   ShowInTaskbar   =   0   'False
   Visible         =   0   'False
End
Attribute VB_Name = "frmTextRender"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
