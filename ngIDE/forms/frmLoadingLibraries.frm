VERSION 5.00
Begin VB.Form frmLoadingLibraries 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Loading..."
   ClientHeight    =   1065
   ClientLeft      =   45
   ClientTop       =   465
   ClientWidth     =   4680
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   9
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmLoadingLibraries.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   71
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picProgress 
      FillColor       =   &H8000000D&
      Height          =   450
      Left            =   30
      ScaleHeight     =   26
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   304
      TabIndex        =   2
      Top             =   585
      Width           =   4620
   End
   Begin VB.Label lblFilename 
      Height          =   240
      Left            =   30
      TabIndex        =   1
      Top             =   315
      Width           =   4620
   End
   Begin VB.Label lblCaption 
      Caption         =   "Loading libraries..."
      Height          =   225
      Left            =   30
      TabIndex        =   0
      Top             =   45
      Width           =   4620
   End
End
Attribute VB_Name = "frmLoadingLibraries"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Progress As Single

Public Sub SetCaption(ByRef Caption As String)
On Error Resume Next
    lblCaption.Caption = Caption
End Sub

Public Sub SetText(ByRef Text As String)
On Error Resume Next
    lblFilename.Caption = Text
End Sub

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
