VERSION 5.00
Begin VB.Form frmProfile 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Profiler"
   ClientHeight    =   3180
   ClientLeft      =   60
   ClientTop       =   315
   ClientWidth     =   4680
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmProfile.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   212
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   StartUpPosition =   3  'Windows Default
   Begin VB.VScrollBar vsScroll 
      Height          =   3180
      LargeChange     =   16
      Left            =   4425
      Max             =   1
      SmallChange     =   4
      TabIndex        =   0
      Top             =   0
      Width           =   255
   End
End
Attribute VB_Name = "frmProfile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public ProfileTextHeight As Long

Public Sub Form_Resize()
On Error Resume Next
    vsScroll.Move Me.ScaleWidth - vsScroll.Width, 0, vsScroll.Width, Me.ScaleHeight
    If ProfileTextHeight > Me.ScaleHeight Then
        vsScroll.Max = ProfileTextHeight - Me.ScaleHeight
        vsScroll.Enabled = True
    Else
        vsScroll.Enabled = False
        If vsScroll.Value <> 0 Then vsScroll.Value = 0
    End If
End Sub
