VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#7.2#0"; "ngUI.ocx"
Begin VB.Form frmTest 
   Caption         =   "ngToolbar Test"
   ClientHeight    =   3150
   ClientLeft      =   60
   ClientTop       =   390
   ClientWidth     =   4500
   BeginProperty Font 
      Name            =   "Tempus Sans ITC"
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
      Align           =   3  'Align Left
      Height          =   3150
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   720
      _ExtentX        =   1270
      _ExtentY        =   5556
   End
End
Attribute VB_Name = "frmTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
On Error Resume Next
Dim l_imgTest As Fury2Image
Dim l_rfFile As ngResourceFile
    Set l_rfFile = LoadResourceFile("J:\development\binary\sys\resources.zip")
    Set Me.Icon = l_rfFile.ItemData("editor.ico")
    Set tbrTest.ResourceFile = l_rfFile
    Set l_imgTest = Nothing
    tbrTest.ResourcePattern = "*.png"
    Set tbrTest.Buttons.AddNew("Left", "Test1", "tileset", , , btaLeft).Font = Me.Font
    tbrTest.Buttons.AddNew "Right", "Test2", l_imgTest, , , btaRight
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Top", "Test3", l_imgTest, , , btaTop
    tbrTest.Buttons.AddNew "Bottom", "Test4", l_imgTest, , , btaBottom
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Text Only", "Test5"
    tbrTest.Buttons.AddNew , "Test6", l_imgTest
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Disabled", "Test7", l_imgTest, , , , False
    tbrTest.Buttons.AddNew "-"
    tbrTest.Buttons.AddNew "Toggle", "Test8", F2ImageFromPicture(Me.Icon), , bsyCheck
    tbrTest.AutoSize
End Sub

