VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#3.2#0"; "ngUI.ocx"
Begin VB.Form frmTest 
   Caption         =   "ngToolbar Test"
   ClientHeight    =   3150
   ClientLeft      =   60
   ClientTop       =   390
   ClientWidth     =   4500
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   ScaleHeight     =   210
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   300
   StartUpPosition =   3  'Windows Default
   Begin ngUI.ngToolbar tbrTest 
      Align           =   1  'Align Top
      Height          =   60
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4500
      _ExtentX        =   7938
      _ExtentY        =   106
   End
End
Attribute VB_Name = "frmTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
Dim l_imgTest As Fury2Image
    Set l_imgTest = F2LoadImage("C:\Documents and Settings\Kevin\My Documents\Projects\chimaera\res\graphic\cave\mail_new.png")
    tbrTest.Buttons.AddNew "Left", "Test1", l_imgTest, , btaLeft
    tbrTest.Buttons.AddNew "Right", "Test2", l_imgTest, , btaRight
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Top", "Test3", l_imgTest, , btaTop
    tbrTest.Buttons.AddNew "Bottom", "Test4", l_imgTest, , btaBottom
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Text Only", "Test5"
    tbrTest.Buttons.AddNew , "Test6", l_imgTest
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Disabled", "Test7", l_imgTest, , , , False
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Toggle", "Test8", , , bsyCheck
    tbrTest.AutoSize
End Sub

