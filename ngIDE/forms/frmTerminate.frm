VERSION 5.00
Begin VB.Form frmTerminate 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Shutting Down..."
   ClientHeight    =   345
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   4680
   ControlBox      =   0   'False
   Icon            =   "frmTerminate.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   23
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrKill 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   0
      Top             =   0
   End
   Begin VB.Label lblPleaseWait 
      Alignment       =   2  'Center
      Caption         =   "Shutting down, please wait..."
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   45
      TabIndex        =   0
      Top             =   45
      Width           =   4590
   End
End
Attribute VB_Name = "frmTerminate"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub tmrKill_Timer()
On Error Resume Next
    tmrKill.Enabled = False
    TerminateProgram
End Sub
