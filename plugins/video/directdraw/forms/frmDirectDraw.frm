VERSION 5.00
Begin VB.Form frmDirectDraw 
   BackColor       =   &H00000000&
   ClientHeight    =   3600
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   4800
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
   MouseIcon       =   "frmDirectDraw.frx":492A
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.PictureBox picBuffer 
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   495
      Left            =   1800
      ScaleHeight     =   33
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   81
      TabIndex        =   0
      Top             =   1560
      Width           =   1215
   End
End
Attribute VB_Name = "frmDirectDraw"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
    SetAppIcon Me
End Sub

Private Sub Form_Resize()
On Error Resume Next
    picBuffer.Move 0, 0, Me.ScaleWidth, Me.ScaleHeight
End Sub
