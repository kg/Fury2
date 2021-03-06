VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#10.9#0"; "ngUI.ocx"
Begin VB.Form frmSaveOpenDocuments 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Confirm Close"
   ClientHeight    =   3060
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
   Icon            =   "frmSaveOpenDocuments.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   204
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   392
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
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
      Top             =   960
      Width           =   1500
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "Don't Save"
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
      Caption         =   "Save Selected"
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
   Begin VB.Frame fraDocuments 
      Caption         =   "Currently Open Documents"
      Height          =   3000
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4275
      Begin ngUI.ngListBox lstDocuments 
         Height          =   2700
         Left            =   75
         TabIndex        =   4
         Top             =   225
         Width           =   4125
         _ExtentX        =   7276
         _ExtentY        =   4763
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
End
Attribute VB_Name = "frmSaveOpenDocuments"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngIDE (Fury� Game Creation System Next-Generation Editor)
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
Public Cancelled As Boolean

Public Sub RefreshDocumentList()
On Error Resume Next
Dim l_docDocument As cChildManager, l_lngDocumentIndex As Long
Dim l_plgPlugin As iPlugin, l_lngIcon As Long
Dim l_icnIcon As IPictureDisp, l_imgIcon As Fury2Image
    With lstDocuments
        .ListItems.Clear
        l_lngDocumentIndex = 1
        For Each l_docDocument In frmMain.Documents
            With l_docDocument
                If .Document.CanSave Then
                    Set l_icnIcon = Nothing
                    Set l_plgPlugin = Nothing
                    Set l_plgPlugin = .Document.Plugin
                    Set l_imgIcon = l_plgPlugin.Icon
                    Set lstDocuments.ListItems.AddNew(IIf(Trim(.Document.Filename) = "", .Form.Caption, GetTitle(.Document.Filename)), , l_imgIcon.Resample(16, 16, ResampleMode_Bilinear), ltaLeft).Tag = l_docDocument
                End If
            End With
        Next l_docDocument
        lstDocuments.SelectAll
        If lstDocuments.ListItems.Count = 0 Then
            Cancelled = False
            Me.Hide
        End If
    End With
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Cancelled = True
    Me.Hide
End Sub

Private Sub cmdClose_Click()
On Error Resume Next
    Cancelled = False
    Me.Hide
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
Dim l_docDocument As cChildManager
Dim l_liItem As ngListItem
    Cancelled = False
    For Each l_liItem In lstDocuments.ListItems
        If l_liItem.Selected Then
            Set l_docDocument = l_liItem.Tag
            g_edEditor.File_Save l_docDocument
        End If
    Next l_liItem
    Me.Hide
End Sub

Private Sub Form_Activate()
On Error Resume Next
End Sub

Private Sub Form_Load()
On Error Resume Next
    RefreshDocumentList
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
End Sub

Private Sub lstDocuments_Change()
On Error Resume Next
End Sub
