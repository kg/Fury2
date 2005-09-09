VERSION 5.00
Begin VB.Form frmSharedTest 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Shared Memory Test"
   ClientHeight    =   4035
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4890
   Icon            =   "frmSharedTest.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   269
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   326
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtTag 
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   330
      Left            =   15
      TabIndex        =   1
      Text            =   "0"
      Top             =   3690
      Width           =   4845
   End
   Begin VB.Timer tmrRedraw 
      Interval        =   25
      Left            =   1845
      Top             =   1605
   End
   Begin VB.PictureBox picDisplay 
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3660
      Left            =   15
      ScaleHeight     =   240
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   320
      TabIndex        =   0
      Top             =   0
      Width           =   4860
   End
End
Attribute VB_Name = "frmSharedTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim m_imgPicture As Fury2Image

Private Sub Form_Load()
On Error Resume Next
    F2Init
    Set m_imgPicture = F2SharedImage(320, 240, "SharedTest")
End Sub

Private Sub picDisplay_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_imgPicture.AntiAliasFilledEllipse Array(X, Y), F2White, 5, 5, RenderMode_SourceAlpha, 0.33
End Sub

Private Sub picDisplay_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        picDisplay_MouseDown Button, Shift, X, Y
    End If
End Sub

Private Sub tmrRedraw_Timer()
On Error Resume Next
    Debug.Print m_imgPicture.Width & " x "; m_imgPicture.Height
    Debug.Print m_imgPicture.Pointer(0, 0)
    CopyImageToDC picDisplay.hDC, m_imgPicture.Rectangle, m_imgPicture
    If Me.ActiveControl Is txtTag Then
    Else
        txtTag.Text = CStr(m_imgPicture.NumericTags(0))
    End If
End Sub

Private Sub txtTag_Change()
On Error Resume Next
    m_imgPicture.NumericTags(0) = CLng(txtTag.Text)
End Sub
