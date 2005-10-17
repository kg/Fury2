VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#12.13#0"; "ngUI.ocx"
Begin VB.Form frmInsertSprite 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Insert Sprite"
   ClientHeight    =   5520
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   7500
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmInsertSprite.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   368
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   500
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.PictureBox picProgress 
      AutoRedraw      =   -1  'True
      FillColor       =   &H8000000D&
      Height          =   270
      Left            =   2880
      ScaleHeight     =   1
      ScaleMode       =   0  'User
      ScaleWidth      =   1
      TabIndex        =   4
      Top             =   5235
      Width           =   3060
   End
   Begin VB.CheckBox chkTemplate 
      Caption         =   "Use as Template"
      Height          =   240
      Left            =   30
      TabIndex        =   2
      Top             =   5250
      Width           =   5910
   End
   Begin ngUI.ngListBox lstSprites 
      Height          =   4890
      Left            =   60
      TabIndex        =   3
      Top             =   300
      Width           =   5850
      _ExtentX        =   10319
      _ExtentY        =   8625
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
      Left            =   5970
      TabIndex        =   1
      Top             =   480
      Width           =   1500
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
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
      Left            =   5970
      TabIndex        =   0
      Top             =   30
      Width           =   1500
   End
   Begin ngUI.ngTabStrip tsViews 
      Height          =   5190
      Left            =   30
      Top             =   30
      Width           =   5910
      _ExtentX        =   10425
      _ExtentY        =   9155
   End
End
Attribute VB_Name = "frmInsertSprite"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
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
Public Engine As Fury2Engine
Public Cancelled As Boolean
Public Sprite As Fury2Sprite

Friend Sub SetProgress(ByVal Progress As Single)
On Error Resume Next
    picProgress.Cls
    picProgress.Line (0, 0)-(Progress, 1), picProgress.FillColor, BF
    picProgress.Refresh
End Sub

Friend Sub ClearList()
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_sprSprite As Fury2Sprite
    Debug.Print "ClearList"
    lstSprites.DisableUpdates = True
    For Each l_liItem In lstSprites.ListItems
        Set l_sprSprite = Nothing
        Set l_sprSprite = l_liItem.Tag
        If l_sprSprite Is Nothing Then
        Else
            l_sprSprite.Free
        End If
        Set l_liItem.Tag = Nothing
        Set l_liItem.Image = Nothing
    Next l_liItem
    lstSprites.DisableUpdates = False
    lstSprites.ListItems.Clear
End Sub

Public Sub RefreshSpriteList()
On Error Resume Next
Static l_booHere As Boolean
Dim l_flsFiles As Fury2Files
Dim l_scSprites As Fury2Sprites
Dim l_sprSprite As Fury2Sprite
Dim l_imgSprite As Fury2Image
Dim l_rctSprite As Fury2Rect
Dim l_lngFiles As Long, l_lngSprites As Long
    If l_booHere Then Exit Sub
    l_booHere = True
    Debug.Print "RefreshSpriteList"
    Set l_flsFiles = Engine.FileSystem.EnumFiles(, "*.f2sprites", True)
    ClearList
    If l_flsFiles Is Nothing Then
    Else
        If l_flsFiles.Count > 0 Then
            SetProgress 0
            For l_lngFiles = 1 To l_flsFiles.Count
                With l_flsFiles.File(l_lngFiles)
                    SetProgress (l_lngFiles - 1) / l_flsFiles.Count
                    Set l_scSprites = Engine.LoadSprites(.Name)
                    lstSprites.ListItems.AddNew(.Title).Enabled = False
                    l_lngSprites = 1
                    For Each l_sprSprite In l_scSprites
                        SetProgress ((l_lngFiles - 1 + (l_lngSprites / (l_scSprites.Count))) / l_flsFiles.Count)
                        With l_sprSprite
                            .Initialize
                            .Load
                            .Template = l_flsFiles.File(l_lngFiles).Name & ":" & .Name
                            Set l_rctSprite = .Rectangle(True)
                            Set l_imgSprite = F2Image(l_rctSprite.Width, l_rctSprite.Height)
                            l_sprSprite.Render l_imgSprite, 0, 0
                            Set lstSprites.ListItems.AddNew(.Name, , l_imgSprite).Tag = l_sprSprite
                        End With
                        l_lngSprites = l_lngSprites + 1
                        DoEvents
                    Next l_sprSprite
                    l_scSprites.Clear
                End With
            Next l_lngFiles
            SetProgress 100
        End If
    End If
    cmdOK.Enabled = lstSprites.SelectedItemCount > 0
    Debug.Print "RefreshSpriteList Done"
    lstSprites.Reflow
    l_booHere = False
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Cancelled = True
    ClearList
    Me.Hide
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
    If chkTemplate.Value = 0 Then
        Sprite.Template = ""
    Else
    End If
    If Sprite Is Nothing Then Cancelled = True Else Cancelled = False
    ClearList
    Me.Hide
End Sub

Private Sub Form_Activate()
On Error Resume Next
    Debug.Print "frmInsertSprite_Activate"
    lstSprites.ListItems.Clear
    RefreshSpriteList
End Sub

Private Sub lstSprites_ItemSelect(Item As ngUI.ngListItem)
On Error Resume Next
    Set Sprite = Item.Tag
    cmdOK.Enabled = Not (Sprite Is Nothing)
End Sub

Private Sub lstSprites_SelectionChange()
On Error Resume Next
    Set Sprite = Nothing
    Set Sprite = lstSprites.SelectedItemCollection(1).Tag
    cmdOK.Enabled = Not (Sprite Is Nothing)
End Sub

Private Sub tsViews_TabSelected(TheTab As ngTab)
On Error Resume Next
    Debug.Print "tsViews_TabSelected"
    Set Sprite = Nothing
    cmdOK.Enabled = False
    Select Case TheTab.Index
    Case 1
        lstSprites.Visible = True
        RefreshSpriteList
    Case Else
    End Select
End Sub

Private Sub Form_Load()
On Error Resume Next
    Debug.Print "frmInsertSprite_Load"
    ClearList
    tsViews.Tabs.AddNew "Sprite Collections"
    lstSprites.Move tsViews.Left + 2, tsViews.Top + tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - tsViews.IdealHeight - 3
    lstSprites.Colors(lbcBackground) = ConvertSystemColor(SystemColor_Button_Highlight)
    tsViews_TabSelected tsViews.Tabs(1)
End Sub
