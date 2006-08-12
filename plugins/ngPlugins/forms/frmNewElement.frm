VERSION 5.00
Begin VB.Form frmNewElement 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "New Element"
   ClientHeight    =   3090
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   5865
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmNewElement.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   206
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   391
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame fraProperties 
      Caption         =   "Element Properties"
      Height          =   3030
      Left            =   30
      TabIndex        =   2
      Top             =   30
      Width           =   4275
      Begin ngPlugins.Script scShow 
         Height          =   1000
         Left            =   1080
         TabIndex        =   7
         Top             =   870
         Width           =   3075
         _ExtentX        =   5424
         _ExtentY        =   1773
      End
      Begin VB.TextBox txtExpression 
         Height          =   285
         Left            =   1080
         TabIndex        =   5
         Top             =   555
         Width           =   3075
      End
      Begin VB.TextBox txtID 
         Height          =   285
         Left            =   1080
         TabIndex        =   3
         Text            =   "NewElement"
         Top             =   240
         Width           =   3075
      End
      Begin ngPlugins.Script scHide 
         Height          =   1005
         Left            =   1080
         TabIndex        =   9
         Top             =   1905
         Width           =   3075
         _ExtentX        =   5424
         _ExtentY        =   1773
      End
      Begin VB.Label lblHideScript 
         AutoSize        =   -1  'True
         Caption         =   "&Hide Code:"
         Height          =   195
         Left            =   135
         TabIndex        =   10
         Top             =   1935
         Width           =   795
      End
      Begin VB.Label lblShowScript 
         AutoSize        =   -1  'True
         Caption         =   "&Show Code:"
         Height          =   195
         Left            =   135
         TabIndex        =   8
         Top             =   900
         Width           =   870
      End
      Begin VB.Label lblExpression 
         AutoSize        =   -1  'True
         Caption         =   "&Expression:"
         Height          =   195
         Left            =   135
         TabIndex        =   6
         Top             =   600
         Width           =   840
      End
      Begin VB.Label lblID 
         AutoSize        =   -1  'True
         Caption         =   "&ID:"
         Height          =   195
         Left            =   135
         TabIndex        =   4
         Top             =   285
         Width           =   225
      End
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
      TabIndex        =   0
      Top             =   30
      Width           =   1500
   End
End
Attribute VB_Name = "frmNewElement"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub lblHideScript_Click()

End Sub
