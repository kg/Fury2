VERSION 5.00
Begin VB.Form MDIChild 
   Caption         =   "Normales MDIChild"
   ClientHeight    =   1425
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4410
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   1425
   ScaleWidth      =   4410
   Begin VB.CommandButton Command2 
      Caption         =   "Caption ändern..."
      Height          =   330
      Left            =   2250
      TabIndex        =   3
      Top             =   810
      Width           =   1905
   End
   Begin VB.PictureBox Picture1 
      Height          =   330
      Left            =   3510
      Picture         =   "MDIChild.frx":0000
      ScaleHeight     =   270
      ScaleWidth      =   270
      TabIndex        =   2
      Top             =   225
      Visible         =   0   'False
      Width           =   330
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Icon ändern..."
      Height          =   330
      Left            =   180
      TabIndex        =   1
      Top             =   810
      Width           =   1905
   End
   Begin VB.Label Label1 
      Caption         =   "Eine normale MDIChild Form mit VB Mitteln erstellt! Diese Form hat kein eigenes Menü!"
      Height          =   555
      Left            =   180
      TabIndex        =   0
      Top             =   90
      Width           =   2805
   End
End
Attribute VB_Name = "MDIChild"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
  Me.Icon = Me.Picture1.Picture
End Sub

Private Sub Command2_Click()
  Me.Caption = "Es ist der " & Now
End Sub
