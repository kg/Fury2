VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#8.5#0"; "ngUI.ocx"
Begin VB.Form frmTest 
   Caption         =   "ngToolbar Test"
   ClientHeight    =   5490
   ClientLeft      =   60
   ClientTop       =   390
   ClientWidth     =   6285
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
   ScaleHeight     =   366
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   419
   StartUpPosition =   3  'Windows Default
   Begin ngUI.ngListBox lbxTest 
      Height          =   3600
      Left            =   735
      TabIndex        =   1
      Top             =   945
      Width           =   4800
      _ExtentX        =   8467
      _ExtentY        =   6350
   End
   Begin ngUI.ngToolbar tbrTest 
      Align           =   1  'Align Top
      Height          =   195
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   6285
      _ExtentX        =   11086
      _ExtentY        =   344
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
Dim l_lngIndex As Long
    Set l_rfFile = LoadResourceFile("J:\development\binary\sys\resources\common.zip")
    Set tbrTest.ResourceFile = l_rfFile
    Set l_imgTest = Nothing
    tbrTest.Orientation = tboHorizontal
    tbrTest.ResourcePattern = "*.png"
    tbrTest.LoadTheme "theme\toolbar\"
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
    lbxTest.AllowReorder = True
    For l_lngIndex = 0 To 99
        lbxTest.ListItems.AddNew "Item &" & l_lngIndex
    Next l_lngIndex
End Sub

