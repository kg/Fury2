VERSION 5.00
Begin VB.Form frmSpriteImporter 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Sprite Importer"
   ClientHeight    =   3990
   ClientLeft      =   45
   ClientTop       =   345
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
   Icon            =   "frmSpriteImporter.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   266
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   391
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Visible         =   0   'False
   Begin ngPlugins.ObjectInspector insSettings 
      Height          =   3930
      Left            =   30
      TabIndex        =   2
      Top             =   30
      Width           =   4275
      _ExtentX        =   7541
      _ExtentY        =   6932
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
Attribute VB_Name = "frmSpriteImporter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Options As SpriteImporterOptions

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
    Set Options = New SpriteImporterOptions
End Sub
