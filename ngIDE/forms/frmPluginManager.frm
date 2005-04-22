VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#8.10#0"; "ngUI.ocx"
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
   StartUpPosition =   2  'CenterScreen
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
      TabIndex        =   5
      Top             =   1860
      Width           =   1500
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
      TabIndex        =   4
      Top             =   1440
      Width           =   1500
   End
   Begin VB.CommandButton cmdInstallNewPlugin 
      Caption         =   "&Install New..."
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
      TabIndex        =   3
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
      TabIndex        =   2
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
      TabIndex        =   1
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
      Begin ngUI.ngListBox lstPlugins 
         Height          =   2220
         Left            =   75
         TabIndex        =   6
         Top             =   225
         Width           =   4110
         _ExtentX        =   7250
         _ExtentY        =   3916
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Tahoma"
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
   End
   Begin VB.PictureBox picPluginOptions 
      BackColor       =   &H80000014&
      BorderStyle     =   0  'None
      Height          =   1035
      Left            =   75
      ScaleHeight     =   69
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   383
      TabIndex        =   17
      Top             =   2940
      Visible         =   0   'False
      Width           =   5745
      Begin VB.CheckBox chkShowInOpenDialog 
         BackColor       =   &H80000014&
         Caption         =   "Show in Open Dialog"
         Enabled         =   0   'False
         Height          =   255
         Left            =   975
         TabIndex        =   21
         Top             =   765
         Width           =   4770
      End
      Begin VB.CheckBox chkShowNewMenu 
         BackColor       =   &H80000014&
         Caption         =   "Show option on New File Menu"
         Enabled         =   0   'False
         Height          =   255
         Left            =   975
         TabIndex        =   20
         Top             =   510
         Width           =   4770
      End
      Begin VB.CheckBox chkShowPluginButton 
         BackColor       =   &H80000014&
         Caption         =   "Show button on Plugin Toolbar"
         Enabled         =   0   'False
         Height          =   255
         Left            =   975
         TabIndex        =   19
         Top             =   0
         Width           =   4770
      End
      Begin VB.CheckBox chkShowPluginMenu 
         BackColor       =   &H80000014&
         Caption         =   "Show option on Tools Menu"
         Enabled         =   0   'False
         Height          =   255
         Left            =   975
         TabIndex        =   18
         Top             =   255
         Width           =   4770
      End
   End
   Begin VB.PictureBox picPluginInfo 
      BackColor       =   &H80000014&
      BorderStyle     =   0  'None
      Height          =   1830
      Left            =   75
      ScaleHeight     =   122
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   383
      TabIndex        =   8
      Top             =   2940
      Visible         =   0   'False
      Width           =   5745
      Begin VB.TextBox txtPluginDescription 
         BackColor       =   &H80000014&
         Height          =   870
         Left            =   975
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   2  'Vertical
         TabIndex        =   12
         Top             =   945
         Width           =   4770
      End
      Begin VB.TextBox txtPluginPath 
         BackColor       =   &H80000014&
         Height          =   300
         Left            =   975
         Locked          =   -1  'True
         TabIndex        =   11
         Top             =   630
         Width           =   4770
      End
      Begin VB.TextBox txtPluginType 
         BackColor       =   &H80000014&
         Height          =   300
         Left            =   975
         Locked          =   -1  'True
         TabIndex        =   10
         Top             =   315
         Width           =   4770
      End
      Begin VB.TextBox txtPluginName 
         BackColor       =   &H80000014&
         Height          =   300
         Left            =   975
         Locked          =   -1  'True
         TabIndex        =   9
         Top             =   0
         Width           =   4770
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         BackColor       =   &H80000014&
         Caption         =   "Description:"
         Height          =   195
         Left            =   60
         TabIndex        =   16
         Top             =   990
         Width           =   855
      End
      Begin VB.Label lblPath 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         BackColor       =   &H80000014&
         Caption         =   "Path:"
         Height          =   195
         Left            =   525
         TabIndex        =   15
         Top             =   675
         Width           =   390
      End
      Begin VB.Label lblType 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         BackColor       =   &H80000014&
         Caption         =   "Type:"
         Height          =   195
         Left            =   495
         TabIndex        =   14
         Top             =   360
         Width           =   420
      End
      Begin VB.Label lblName 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         BackColor       =   &H80000014&
         Caption         =   "Name:"
         Height          =   195
         Left            =   450
         TabIndex        =   13
         Top             =   45
         Width           =   465
      End
   End
   Begin ngUI.ngTabStrip tsSelected 
      Height          =   2220
      Left            =   30
      TabIndex        =   7
      Top             =   2595
      Width           =   5805
      _ExtentX        =   10239
      _ExtentY        =   3916
   End
End
Attribute VB_Name = "frmPluginManager"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngIDE (Fury² Game Creation System Next-Generation Editor)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit

Public Sub RefreshPluginProperties()
On Error Resume Next
Dim l_plgPlugin As iPlugin
Dim l_fpgPlugin As iFileTypePlugin
    'cmdRemoveSelected.Enabled = (lstPlugins.ListCount > 0) And (lstPlugins.ListIndex >= 0)
    Set l_plgPlugin = lstPlugins.FirstSelectedItem.Tag
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
Dim l_imgIcon As Fury2Image, l_imgDefault As Fury2Image
    Set l_imgDefault = F2ImageFromPicture(Me.Icon).Resample(16, 16, ResampleMode_Bilinear)
    With lstPlugins
        .ListItems.Clear
        For Each l_plgPlugin In g_colPlugins
            With l_plgPlugin
                Set l_icnIcon = Nothing
                Set l_imgIcon = Nothing
                Set l_imgIcon = .ToolbarIcon
                If l_imgIcon Is Nothing Then
                    Set l_icnIcon = .Icon
                    If l_icnIcon Is Nothing Then
                        Set l_imgIcon = l_imgDefault
                    Else
                        Set l_imgIcon = F2ImageFromPicture(l_icnIcon).Resample(16, 16, ResampleMode_Bilinear)
                    End If
                End If
                Set lstPlugins.ListItems.AddNew(.PluginName, , l_imgIcon).Tag = l_plgPlugin
            End With
        Next l_plgPlugin
    End With
End Sub

Private Sub chkShowInOpenDialog_Click()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
    If chkShowInOpenDialog.Value = 2 Then Exit Sub
    Set l_plgPlugin = lstPlugins.FirstSelectedItem.Tag
    If l_plgPlugin Is Nothing Then
    Else
        WriteRegSetting "Plugins\Show In Open Dialog\" & TypeName(l_plgPlugin), CLng(chkShowInOpenDialog.Value)
    End If
End Sub

Private Sub chkShowNewMenu_Click()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
    If chkShowNewMenu.Value = 2 Then Exit Sub
    Set l_plgPlugin = lstPlugins.FirstSelectedItem.Tag
    If l_plgPlugin Is Nothing Then
    ElseIf l_plgPlugin.ShowInNewMenu Then
        WriteRegSetting "Plugins\Show In New Menu\" & TypeName(l_plgPlugin), CLng(chkShowNewMenu.Value)
    End If
End Sub

Private Sub chkShowPluginButton_Click()
On Error Resume Next
Dim l_plgPlugin As iPlugin
    If chkShowPluginButton.Value = 2 Then Exit Sub
    Set l_plgPlugin = lstPlugins.FirstSelectedItem.Tag
    If l_plgPlugin Is Nothing Then
    ElseIf l_plgPlugin.ShowInPluginMenu Then
        WriteRegSetting "Plugins\Show In Toolbar\" & TypeName(l_plgPlugin), CLng(chkShowPluginButton.Value)
    End If
End Sub

Private Sub chkShowPluginMenu_Click()
On Error Resume Next
Dim l_plgPlugin As iPlugin
    If chkShowPluginMenu.Value = 2 Then Exit Sub
    Set l_plgPlugin = lstPlugins.FirstSelectedItem.Tag
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

Private Sub tsSelected_Resize()
On Error Resume Next
    picPluginInfo.Move tsSelected.Left + 2, tsSelected.Top + tsSelected.IdealHeight + 1, tsSelected.Width - 4, tsSelected.Height - (tsSelected.IdealHeight + 3)
    picPluginOptions.Move picPluginInfo.Left, picPluginInfo.Top, picPluginInfo.Width, picPluginInfo.Height
End Sub

Private Sub tsSelected_TabSelected(theTab As ngTab)
On Error Resume Next
    Select Case LCase(Trim(theTab.Text))
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
    tsSelected_Resize
    tsSelected.SelectTab 1
End Sub

Private Sub Form_Load()
On Error Resume Next
    tsSelected.Tabs.AddNew "Information"
    tsSelected.Tabs.AddNew "Options"
    RefreshPluginList
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
End Sub

Private Sub lstPlugins_SelectionChange()
On Error Resume Next
    RefreshPluginProperties
End Sub

