VERSION 5.00
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Object = "{462EF1F4-16AF-444F-9DEE-F41BEBEC2FD8}#1.1#0"; "vbalodcl6.ocx"
Begin VB.Form frmPluginManager 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Plugin Manager"
   ClientHeight    =   4845
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   5880
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmPluginManager.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   323
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   392
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.CommandButton cmdRemoveSelected 
      Cancel          =   -1  'True
      Caption         =   "&Remove"
      Enabled         =   0   'False
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
      TabIndex        =   6
      Top             =   1860
      Width           =   1500
   End
   Begin vbalIml6.vbalImageList ilPlugins 
      Left            =   5295
      Top             =   1860
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   24
   End
   Begin VB.CommandButton cmdConfigureSelected 
      Caption         =   "&Configure..."
      Enabled         =   0   'False
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
      TabIndex        =   5
      Top             =   1440
      Width           =   1500
   End
   Begin VB.CommandButton cmdInstallNewPlugin 
      Caption         =   "&Install New..."
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
      TabIndex        =   4
      Top             =   1020
      Width           =   1500
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
      TabIndex        =   3
      Top             =   540
      Width           =   1500
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Default         =   -1  'True
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
      TabIndex        =   2
      Top             =   120
      Width           =   1500
   End
   Begin VB.Frame fraPlugins 
      Caption         =   "Available Plugins"
      Height          =   2535
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4275
      Begin ODCboLst6.OwnerDrawComboList lstPlugins 
         Height          =   2220
         Left            =   75
         TabIndex        =   1
         Top             =   225
         Width           =   4110
         _ExtentX        =   7250
         _ExtentY        =   3916
         ExtendedUI      =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   -2147483630
         Style           =   4
         FullRowSelect   =   -1  'True
         MaxLength       =   0
         NoDimWhenOutOfFocus=   -1  'True
      End
   End
   Begin vbalDTab6.vbalDTabControl dtSelected 
      Height          =   2220
      Left            =   30
      TabIndex        =   7
      Top             =   2595
      Width           =   5805
      _ExtentX        =   10239
      _ExtentY        =   3916
      AllowScroll     =   0   'False
      TabAlign        =   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty SelectedFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShowCloseButton =   0   'False
      MoveableTabs    =   0   'False
      Begin VB.PictureBox picPluginOptions 
         BorderStyle     =   0  'None
         Height          =   1035
         Left            =   30
         ScaleHeight     =   69
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   383
         TabIndex        =   8
         Top             =   360
         Visible         =   0   'False
         Width           =   5745
         Begin VB.CheckBox chkShowInOpenDialog 
            Caption         =   "Show in Open Dialog"
            Enabled         =   0   'False
            Height          =   255
            Left            =   975
            TabIndex        =   12
            Top             =   765
            Width           =   4635
         End
         Begin VB.CheckBox chkShowNewMenu 
            Caption         =   "Show option on New File Menu"
            Enabled         =   0   'False
            Height          =   255
            Left            =   975
            TabIndex        =   11
            Top             =   510
            Width           =   4635
         End
         Begin VB.CheckBox chkShowPluginButton 
            Caption         =   "Show button on Plugin Toolbar"
            Enabled         =   0   'False
            Height          =   255
            Left            =   975
            TabIndex        =   9
            Top             =   0
            Width           =   4635
         End
         Begin VB.CheckBox chkShowPluginMenu 
            Caption         =   "Show option on Tools Menu"
            Enabled         =   0   'False
            Height          =   255
            Left            =   975
            TabIndex        =   10
            Top             =   255
            Width           =   4635
         End
      End
      Begin VB.PictureBox picPluginInfo 
         BorderStyle     =   0  'None
         Height          =   1830
         Left            =   30
         ScaleHeight     =   122
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   383
         TabIndex        =   13
         Top             =   360
         Visible         =   0   'False
         Width           =   5745
         Begin VB.TextBox txtPluginDescription 
            BackColor       =   &H8000000F&
            Height          =   870
            Left            =   975
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   2  'Vertical
            TabIndex        =   21
            Top             =   945
            Width           =   4635
         End
         Begin VB.TextBox txtPluginPath 
            BackColor       =   &H8000000F&
            Height          =   300
            Left            =   975
            Locked          =   -1  'True
            TabIndex        =   19
            Top             =   630
            Width           =   4635
         End
         Begin VB.TextBox txtPluginType 
            BackColor       =   &H8000000F&
            Height          =   300
            Left            =   975
            Locked          =   -1  'True
            TabIndex        =   17
            Top             =   315
            Width           =   4635
         End
         Begin VB.TextBox txtPluginName 
            BackColor       =   &H8000000F&
            Height          =   300
            Left            =   975
            Locked          =   -1  'True
            TabIndex        =   15
            Top             =   0
            Width           =   4635
         End
         Begin VB.Label Label1 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "Description:"
            Height          =   195
            Left            =   60
            TabIndex        =   20
            Top             =   990
            Width           =   855
         End
         Begin VB.Label lblPath 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "Path:"
            Height          =   195
            Left            =   525
            TabIndex        =   18
            Top             =   675
            Width           =   390
         End
         Begin VB.Label lblType 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "Type:"
            Height          =   195
            Left            =   495
            TabIndex        =   16
            Top             =   360
            Width           =   420
         End
         Begin VB.Label lblName 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "Name:"
            Height          =   195
            Left            =   450
            TabIndex        =   14
            Top             =   45
            Width           =   465
         End
      End
   End
End
Attribute VB_Name = "frmPluginManager"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public Sub RefreshPluginProperties()
On Error Resume Next
Dim l_plgPlugin As iPlugin
Dim l_fpgPlugin As iFileTypePlugin
    cmdRemoveSelected.Enabled = (lstPlugins.ListCount > 0) And (lstPlugins.ListIndex >= 0)
    Set l_plgPlugin = g_colPlugins(lstPlugins.ItemData(lstPlugins.ListIndex))
    If l_plgPlugin Is Nothing Then
    Else
        With l_plgPlugin
            txtPluginName.Text = .PluginName
            If TypeOf l_plgPlugin Is iFileTypePlugin Then
                txtPluginType.Text = "File Type Plugin"
                Set l_fpgPlugin = l_plgPlugin
                chkShowNewMenu.Enabled = l_fpgPlugin.ShowInNewMenu
                chkShowInOpenDialog.Enabled = True
            Else
                txtPluginType.Text = "Tool Plugin"
                chkShowNewMenu.Enabled = False
                chkShowInOpenDialog.Enabled = False
            End If
            txtPluginPath.Text = .PluginPath
            txtPluginDescription.Text = .PluginDescription
            chkShowPluginButton.Enabled = .ShowInPluginMenu
            chkShowPluginMenu.Enabled = .ShowInPluginMenu
            If chkShowPluginButton.Enabled Then
                chkShowPluginButton.Value = CLng(ReadRegSetting("Plugins\Show In Toolbar\" & TypeName(l_plgPlugin), 2))
            Else
                chkShowPluginButton.Value = 0
            End If
            If chkShowPluginMenu.Enabled Then
                chkShowPluginMenu.Value = CLng(ReadRegSetting("Plugins\Show In Menu\" & TypeName(l_plgPlugin), 2))
            Else
                chkShowPluginMenu.Value = 0
            End If
            If chkShowNewMenu.Enabled Then
                chkShowNewMenu.Value = CLng(ReadRegSetting("Plugins\Show In New Menu\" & TypeName(l_plgPlugin), 2))
            Else
                chkShowNewMenu.Value = 0
            End If
            If chkShowInOpenDialog.Enabled Then
                chkShowInOpenDialog.Value = CLng(ReadRegSetting("Plugins\Show In Open Dialog\" & TypeName(l_plgPlugin), 2))
            Else
                chkShowInOpenDialog.Value = 0
            End If
            cmdConfigureSelected.Enabled = .CanConfigure
        End With
    End If
End Sub

Public Sub RefreshPluginList()
On Error Resume Next
Dim l_plgPlugin As iPlugin, l_lngPluginIndex As Long, l_lngIcon As Long
Dim l_icnIcon As IPictureDisp
    ilPlugins.Clear
    ilPlugins.AddFromHandle Me.Icon.Handle, Image_Icon, "DEFAULT"
    With lstPlugins
        .ImageList = ilPlugins.hIml
        .Clear
        l_lngPluginIndex = 1
        For Each l_plgPlugin In g_colPlugins
            With l_plgPlugin
                Set l_icnIcon = Nothing
                Set l_icnIcon = .Icon
                If l_icnIcon Is Nothing Then
                    l_lngIcon = 0
                Else
                    ilPlugins.AddFromHandle l_icnIcon.Handle, Image_Icon, "ICON_" & l_icnIcon.Handle
                    l_lngIcon = ilPlugins.ItemIndex("ICON_" & l_icnIcon.Handle) - 1
                End If
                lstPlugins.AddItemAndData " " & .PluginName, l_lngIcon, 2, , , l_lngPluginIndex, , 18, eixLeft, eixVCentre
            End With
            l_lngPluginIndex = l_lngPluginIndex + 1
        Next l_plgPlugin
    End With
End Sub

Private Sub chkShowInOpenDialog_Click()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
    If chkShowInOpenDialog.Value = 2 Then Exit Sub
    Set l_plgPlugin = g_colPlugins(lstPlugins.ItemData(lstPlugins.ListIndex))
    If l_plgPlugin Is Nothing Then
    Else
        WriteRegSetting "Plugins\Show In Open Dialog\" & TypeName(l_plgPlugin), CLng(chkShowInOpenDialog.Value)
    End If
End Sub

Private Sub chkShowNewMenu_Click()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
    If chkShowNewMenu.Value = 2 Then Exit Sub
    Set l_plgPlugin = g_colPlugins(lstPlugins.ItemData(lstPlugins.ListIndex))
    If l_plgPlugin Is Nothing Then
    ElseIf l_plgPlugin.ShowInNewMenu Then
        WriteRegSetting "Plugins\Show In New Menu\" & TypeName(l_plgPlugin), CLng(chkShowNewMenu.Value)
    End If
End Sub

Private Sub chkShowPluginButton_Click()
On Error Resume Next
Dim l_plgPlugin As iPlugin
    If chkShowPluginButton.Value = 2 Then Exit Sub
    Set l_plgPlugin = g_colPlugins(lstPlugins.ItemData(lstPlugins.ListIndex))
    If l_plgPlugin Is Nothing Then
    ElseIf l_plgPlugin.ShowInPluginMenu Then
        WriteRegSetting "Plugins\Show In Toolbar\" & TypeName(l_plgPlugin), CLng(chkShowPluginButton.Value)
    End If
End Sub

Private Sub chkShowPluginMenu_Click()
On Error Resume Next
Dim l_plgPlugin As iPlugin
    If chkShowPluginMenu.Value = 2 Then Exit Sub
    Set l_plgPlugin = g_colPlugins(lstPlugins.ItemData(lstPlugins.ListIndex))
    If l_plgPlugin Is Nothing Then
    ElseIf l_plgPlugin.ShowInPluginMenu Then
        WriteRegSetting "Plugins\Show In Menu\" & TypeName(l_plgPlugin), CLng(chkShowPluginMenu.Value)
    End If
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Me.Hide
    Unload Me
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
    Me.Hide
    Unload Me
    'SavePluginChanges
    ShutdownPlugins
    InitPlugins
    LoadPlugins
End Sub

Private Sub dtSelected_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    Select Case LCase(Trim(theTab.key))
    Case "information"
        picPluginInfo.Visible = True
        picPluginOptions.Visible = False
    Case "options"
        picPluginInfo.Visible = False
        picPluginOptions.Visible = True
    Case Else
    End Select
End Sub

Private Sub Form_Activate()
On Error Resume Next
    dtSelected_TabSelected dtSelected.SelectedTab
End Sub

Private Sub Form_Load()
On Error Resume Next
    dtSelected.Tabs.Add "Information", , "Information"
    dtSelected.Tabs.Add "Options", , "Options"
    RefreshPluginList
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    lstPlugins.Clear
    lstPlugins.ImageList = 0
    ilPlugins.Destroy
End Sub

Private Sub lstPlugins_Change()
On Error Resume Next
    RefreshPluginProperties
End Sub
