VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#10.11#0"; "ngUI.ocx"
Begin VB.Form frmTest 
   BackColor       =   &H000000FF&
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
   FontTransparent =   0   'False
   LinkTopic       =   "Form1"
   ScaleHeight     =   366
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   419
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdMenu 
      Caption         =   "Show Menu"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   630
      Left            =   4215
      TabIndex        =   3
      Top             =   4755
      Width           =   1995
   End
   Begin ngUI.ngTabStrip tsTabs 
      Height          =   765
      Left            =   60
      TabIndex        =   2
      Top             =   4680
      Width           =   3435
      _ExtentX        =   6059
      _ExtentY        =   1349
   End
   Begin ngUI.ngListBox lbxTest 
      Height          =   3600
      Left            =   330
      TabIndex        =   1
      Top             =   855
      Width           =   4800
      _ExtentX        =   8467
      _ExtentY        =   6350
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tempus Sans ITC"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      AllowReorder    =   0   'False
      AllowMultiSelect=   0   'False
      AllowNullSelection=   0   'False
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
Dim m_mnuTest As ngMenu
Dim m_mnuChild As ngMenu
Dim m_mnuSubChild As ngMenu

Public Sub ItemClicked(Item)
    Debug.Print "ItemClicked(" & Item.Key & ")"
End Sub

Private Sub cmdMenu_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_ptPoint As PointAPI
    ClientToScreen cmdMenu.hWnd, l_ptPoint
    With m_mnuTest.Show(l_ptPoint.X, l_ptPoint.Y + (cmdMenu.Height))
        Me.Caption = "Selected " & .FullKey
    End With
End Sub

Private Sub Form_Load()
On Error Resume Next
Dim l_imgTest As Fury2Image
Dim l_rfFile As ngResourceFile
Dim l_lngIndex As Long
    Set l_rfFile = LoadResourceFile("J:\development\binary\sys\resources\common.zip")
'    Set tbrTest.ResourceFile = l_rfFile
    Set l_imgTest = Nothing
    SetTheme l_rfFile, "*.png"
    SetToolbarTheme "theme\toolbar\"
    SetTabTheme "theme\tabstrip\"
    tbrTest.Orientation = tboHorizontal
'    tbrTest.ResourcePattern = "*.png"
'    tbrTest.LoadTheme "theme\toolbar\"
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
'    Set tsTabs.ResourceFile = l_rfFile
'    tsTabs.ResourcePattern = "*.png"
'    tsTabs.LoadTheme "theme\tabstrip\"
    tsTabs.ShowCloseButtons = True
    tsTabs.Tabs.AddNew "Tab 1", , F2ImageFromPicture(Me.Icon)
    tsTabs.Tabs.AddNew "Tab 2", , F2ImageFromPicture(Me.Icon)
    tsTabs.Tabs.AddNew "Tab 3", , F2ImageFromPicture(Me.Icon)
    tsTabs.Tabs.AddNew "Tab 4"
    tsTabs.Tabs.AddNew "Tab 5"
    tsTabs.Tabs.AddNew "Tab 6"
    tsTabs.Tabs.AddNew "Tab 7"
    tsTabs.Tabs.AddNew "Tab 8"
    tsTabs.Tabs.AddNew "Tab 9"
    tsTabs.Tabs.AddNew "Tab 10"
    Set m_mnuTest = CreateMenu()
    m_mnuTest.Items.AddNew "&Item 1", "Alt+F4", "Item1", F2ImageFromPicture(Me.Icon)
    m_mnuTest.Items.AddNew "I&tem 2", "None", "Item2", F2Image(4, 4), , , False
    m_mnuTest.Items.AddNew "-"
    m_mnuTest.Items.AddNew "Item &Number Three", "Ctrl+Alt+Del", "Item3"
    Set m_mnuTest.SelectEvent = BindEvent(Me, "ItemClicked")
    Set m_mnuChild = CreateMenu()
    m_mnuChild.Items.AddNew "Item 1", , "Item1"
    m_mnuChild.Items.AddNew "Item 2", , "Item2"
    m_mnuChild.Items.AddNew "-"
    m_mnuChild.Items.AddNew "Item 3", , "Item3"
    m_mnuChild.Items.AddNew "Item 4", , "Item4"
    Set m_mnuTest.Items(1).ChildMenu = m_mnuChild
    Set m_mnuSubChild = CreateMenu()
    m_mnuSubChild.Items.AddNew "Item 1", , "Item1"
    m_mnuSubChild.Items.AddNew "Item 2", , "Item2"
    Set m_mnuChild.Items(2).ChildMenu = m_mnuSubChild
End Sub

