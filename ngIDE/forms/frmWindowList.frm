VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#8.5#0"; "ngUI.ocx"
Begin VB.Form frmWindowList 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Windows"
   ClientHeight    =   2970
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   5340
   Icon            =   "frmWindowList.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   198
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   356
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame fraWindows 
      Caption         =   "Open Windows"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2910
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   3750
      Begin ngUI.ngListBox lstWindows 
         Height          =   2610
         Left            =   75
         TabIndex        =   3
         Top             =   225
         Width           =   3600
         _ExtentX        =   6350
         _ExtentY        =   4604
      End
   End
   Begin VB.CommandButton cmdSwitch 
      Caption         =   "&Switch To"
      Enabled         =   0   'False
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
      Left            =   3810
      TabIndex        =   1
      Top             =   120
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
      Left            =   3810
      TabIndex        =   2
      Top             =   540
      Width           =   1500
   End
End
Attribute VB_Name = "frmWindowList"
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

Public Sub RefreshWindowList()
On Error Resume Next
Dim l_docDocument As cChildManager
Dim l_lngIcon As Long, l_lngWindow As Long
Dim l_icnIcon As IPictureDisp
    l_lngWindow = 1
    For Each l_docDocument In frmMain.Documents
        l_lngIcon = -1
        Set l_icnIcon = Nothing
        Set l_icnIcon = l_docDocument.Form.Icon
        With l_docDocument
            Set lstWindows.ListItems.AddNew(IIf(Trim(.Document.Filename) = "", .Form.Caption, GetTitle(.Document.Filename)), , F2ImageFromPicture(l_icnIcon).Resample(16, 16, ResampleMode_Bilinear), ltaLeft).Tag = l_docDocument
        End With
    Next l_docDocument
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Me.Hide
    Unload Me
End Sub

Private Sub cmdSwitch_Click()
On Error Resume Next
Dim l_docDocument As cChildManager
    If lstWindows.SelectedItemCount < 1 Then Beep: Exit Sub
    Me.Hide
    Set l_docDocument = lstWindows.FirstSelectedItem.Tag
    l_docDocument.Activate
    Unload Me
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    Select Case KeyCode
    Case vbKeySpace, vbKeyReturn
        cmdSwitch_Click
    Case Else
    End Select
End Sub

Private Sub Form_Load()
On Error Resume Next
    RefreshWindowList
    lstWindows.SetFocus
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
End Sub

Private Sub lstWindows_Click()
On Error Resume Next
End Sub

Private Sub lstWindows_DblClick()
On Error Resume Next
    cmdSwitch_Click
End Sub

Private Sub lstWindows_SelectionChange()
On Error Resume Next
    cmdSwitch.Enabled = (lstWindows.SelectedItemCount >= 1)
End Sub
