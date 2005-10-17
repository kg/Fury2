VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#12.12#0"; "ngUI.ocx"
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
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   105
      TabIndex        =   2
      Top             =   4110
      Width           =   1215
   End
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
      TabIndex        =   0
      Top             =   4755
      Width           =   1995
   End
   Begin ngUI.ngTabStrip tsTabs 
      Height          =   765
      Left            =   60
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
Private Declare Function MsgWaitForMultipleObjects Lib "user32" (ByVal nCount As Long, pHandles As Long, ByVal fWaitAll As Long, ByVal dwMilliseconds As Long, ByVal dwWakeMask As Long) As Long

Dim m_mnuTest As ngMenu
Dim m_mnuChild As ngMenu
Dim m_mnuSubChild As ngMenu

Public Sub MenuShow()
On Error Resume Next
    m_mnuTest.Items.RemoveByKeys "*"
    m_mnuTest.Items.addnew "&Item 1", "Alt+F4", "Item1", F2ImageFromPicture(Me.Icon)
    m_mnuTest.Items.addnew "I&tem 2", "None", "Item2", F2Image(4, 4), , , False
    m_mnuTest.Items.addnew "-"
    m_mnuTest.Items.addnew "Item &Number Three", "Ctrl+Alt+Del", "Item3"
End Sub

Public Sub ItemClicked(Item)
    Debug.Print "ItemClicked(" & Item.Key & ")"
End Sub

Private Sub cmdMenu_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_ptPoint As PointAPI
    ClientToScreen cmdMenu.hWnd, l_ptPoint
    Me.Caption = "Show"
    With m_mnuTest.Show(l_ptPoint.X, l_ptPoint.Y + (cmdMenu.Height))
        Me.Caption = "Selected " & .FullKey
    End With
    Me.Caption = "Shown"
End Sub

Private Sub Command1_Click()
On Error Resume Next
    tbrTest.EnableTheme = False
    tbrTest.DrawBorder = True
    If tsTabs.tabs.Count > 0 Then
        tsTabs.tabs.Clear
    Else
        tsTabs.tabs.addnew "Test"
    End If
    tsTabs.AutoSize
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
    tbrTest.EnableTheme = False
    tbrTest.Orientation = tboHorizontal
'    tbrTest.ResourcePattern = "*.png"
'    tbrTest.LoadTheme "theme\toolbar\"
    Set tbrTest.Buttons.addnew("Left", "Test1", "tileset", , , btaLeft).Font = Me.Font
    tbrTest.Buttons.addnew "Right", "Test2", l_imgTest, , , btaRight
    tbrTest.Buttons.addnew "-"
    tbrTest.Buttons.addnew "Top", "Test3", l_imgTest, , , btaTop
    tbrTest.Buttons.addnew "Bottom", "Test4", l_imgTest, , , btaBottom
    tbrTest.Buttons.addnew "-"
    tbrTest.Buttons.addnew "Text Only", "Test5"
    tbrTest.Buttons.addnew , "Test6", l_imgTest
    tbrTest.Buttons.addnew "-"
    tbrTest.Buttons.addnew "Disabled", "Test7", l_imgTest, , , , False
    tbrTest.Buttons.addnew "-"
    tbrTest.Buttons.addnew "Toggle", "Test8", F2ImageFromPicture(Me.Icon), , bsyCheck
    tbrTest.AutoSize
    lbxTest.AllowReorder = True
    For l_lngIndex = 0 To 99
        lbxTest.ListItems.addnew "Item &" & l_lngIndex
    Next l_lngIndex
'    Set tsTabs.ResourceFile = l_rfFile
'    tsTabs.ResourcePattern = "*.png"
'    tsTabs.LoadTheme "theme\tabstrip\"
    tsTabs.ShowCloseButtons = True
    tsTabs.tabs.addnew "Tab 1", , F2ImageFromPicture(Me.Icon)
    tsTabs.tabs.addnew "Tab 2", , F2ImageFromPicture(Me.Icon)
    tsTabs.tabs.addnew "Tab 3", , F2ImageFromPicture(Me.Icon)
    tsTabs.tabs.addnew "Tab 4"
    tsTabs.tabs.addnew "Tab 5"
    tsTabs.tabs.addnew "Tab 6"
    tsTabs.tabs.addnew "Tab 7"
    tsTabs.tabs.addnew "Tab 8"
    tsTabs.tabs.addnew "Tab 9"
    tsTabs.tabs.addnew "Tab 10"
    Set m_mnuTest = CreateMenu()
    m_mnuTest.Items.addnew "&Item 1", "Alt+F4", "Item1", F2ImageFromPicture(Me.Icon)
    m_mnuTest.Items.addnew "I&tem 2", "None", "Item2", F2Image(4, 4), , , False
    m_mnuTest.Items.addnew "-"
    m_mnuTest.Items.addnew "Item &Number Three", "Ctrl+Alt+Del", "Item3"
    Set m_mnuTest.SelectEvent = BindEvent(Me, "ItemClicked")
    Set m_mnuChild = CreateMenu()
    m_mnuChild.Items.addnew "Item 1", , "Item1"
    m_mnuChild.Items.addnew "Item 2", , "Item2"
    m_mnuChild.Items.addnew "-"
    m_mnuChild.Items.addnew "Item 3", , "Item3"
    m_mnuChild.Items.addnew "Item 4", , "Item4"
    Set m_mnuTest.Items(1).ChildMenu = m_mnuChild
    Set m_mnuSubChild = CreateMenu()
    m_mnuSubChild.Items.addnew "Item 1", , "Item1"
    m_mnuSubChild.Items.addnew "Item 2", , "Item2"
    Set m_mnuTest.ShowEvent = BindEvent(Me, "MenuShow")
    Set m_mnuChild.Items(2).ChildMenu = m_mnuSubChild
End Sub

