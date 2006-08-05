VERSION 5.00
Begin VB.Form frmLoading 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Loading..."
   ClientHeight    =   495
   ClientLeft      =   45
   ClientTop       =   375
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
   Icon            =   "frmUnpacking.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   33
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picProgress 
      FillColor       =   &H8000000D&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   450
      Left            =   30
      ScaleHeight     =   26
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   304
      TabIndex        =   0
      Top             =   30
      Width           =   4620
   End
End
Attribute VB_Name = "frmLoading"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Progress As Single

Public Sub SetProgress(ByVal NewProgress As Single)
On Error Resume Next
    Progress = NewProgress
    picProgress_Paint
    DoEvents
End Sub

Private Sub picProgress_Paint()
On Error Resume Next
    picProgress.Line (0, 0)-(picProgress.ScaleWidth * Progress, picProgress.ScaleHeight), picProgress.FillColor, BF
    picProgress.Line (picProgress.ScaleWidth * Progress, 0)-(picProgress.ScaleWidth, picProgress.ScaleHeight), picProgress.BackColor, BF
End Sub
