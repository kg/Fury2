VERSION 5.00
Begin VB.Form frmChangelog 
   Caption         =   "Changelog"
   ClientHeight    =   6000
   ClientLeft      =   60
   ClientTop       =   390
   ClientWidth     =   8250
   Icon            =   "frmChangelog.frx":0000
   LinkTopic       =   "Form1"
   MinButton       =   0   'False
   ScaleHeight     =   400
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   550
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtText 
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3135
      Left            =   0
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   0
      Width           =   4665
   End
End
Attribute VB_Name = "frmChangelog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Resize()
On Error Resume Next
    txtText.Move 0, 0, Me.ScaleWidth, Me.ScaleHeight
End Sub
