VERSION 5.00
Begin VB.Form frmFont 
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   975
   ClientLeft      =   -6405
   ClientTop       =   -6405
   ClientWidth     =   885
   LinkTopic       =   "Form1"
   ScaleHeight     =   65
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   59
   ShowInTaskbar   =   0   'False
   Visible         =   0   'False
   Begin VB.Timer tmrFlushForms 
      Interval        =   10000
      Left            =   180
      Top             =   315
   End
End
Attribute VB_Name = "frmFont"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Initialize()
    g_lngFontForms = g_lngFontForms + 1
End Sub

Private Sub Form_Load()

End Sub

Private Sub Form_Terminate()
    g_lngFontForms = g_lngFontForms - 1
End Sub

Private Sub tmrFlushForms_Timer()
    FlushForms
End Sub
