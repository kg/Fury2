VERSION 5.00
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Begin VB.Form frmOptions 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Options"
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
   Icon            =   "frmOptions.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   323
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   392
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdApply 
      Caption         =   "&Apply"
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
      Top             =   450
      Width           =   1500
   End
   Begin VB.ListBox lstFileTypes 
      Height          =   4380
      IntegralHeight  =   0   'False
      Left            =   75
      Style           =   1  'Checkbox
      TabIndex        =   4
      Top             =   390
      Visible         =   0   'False
      Width           =   4185
   End
   Begin ngIDE.ObjectInspector insOptions 
      Height          =   4380
      Left            =   75
      TabIndex        =   3
      Top             =   390
      Visible         =   0   'False
      Width           =   4185
      _ExtentX        =   7382
      _ExtentY        =   7726
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
      TabIndex        =   1
      Top             =   870
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
      TabIndex        =   0
      Top             =   30
      Width           =   1500
   End
   Begin vbalDTab6.vbalDTabControl dtSelected 
      Height          =   4785
      Left            =   30
      TabIndex        =   2
      Top             =   30
      Width           =   4275
      _ExtentX        =   7541
      _ExtentY        =   8440
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
   End
End
Attribute VB_Name = "frmOptions"
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
Dim m_colPlugins As Collection
Dim m_optOptions As EditorOptions

Private Sub cmdApply_Click()
On Error Resume Next
    Set g_edEditor.Options = m_optOptions
    AssociateFileTypes
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Me.Hide
    Unload Me
End Sub

Public Sub RefreshFileTypes()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
Dim l_strExt As Variant
Dim l_lngIndex As Long
Dim l_regRegistry As cRegistry
    Set l_regRegistry = New cRegistry
    l_regRegistry.ClassKey = HKEY_CLASSES_ROOT
    lstFileTypes.Clear
    Set m_colPlugins = New Collection
    For Each l_plgPlugin In g_colFileTypePlugins
        For Each l_strExt In l_plgPlugin.SupportedExtensions
            l_regRegistry.SectionKey = "." & CStr(l_strExt)
            l_regRegistry.ValueKey = ""
            lstFileTypes.AddItem CStr(l_strExt)
            If Left(Trim(LCase(l_regRegistry.Value)), 2) = "ng" Then
                lstFileTypes.Selected(lstFileTypes.ListCount - 1) = True
            End If
            m_colPlugins.Add l_plgPlugin
        Next l_strExt
    Next l_plgPlugin
End Sub

Public Sub AssociateFileTypes()
On Error Resume Next
Dim l_lngItems As Long
Dim l_plgPlugin As iFileTypePlugin
    For l_lngItems = 0 To lstFileTypes.ListCount - 1
        Set l_plgPlugin = m_colPlugins(l_lngItems + 1)
        If lstFileTypes.Selected(l_lngItems) Then
            g_edEditor.AssociateFileType lstFileTypes.List(l_lngItems), TypeName(l_plgPlugin), l_plgPlugin.FileTypeName, l_plgPlugin.FileTypeName & ".ico"
        End If
    Next l_lngItems
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
    Set g_edEditor.Options = m_optOptions
    AssociateFileTypes
    Me.Hide
    Unload Me
End Sub

Private Sub dtSelected_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    insOptions.Visible = False
    lstFileTypes.Visible = False
    Select Case LCase(Trim(theTab.key))
    Case "general"
        insOptions.Visible = True
        insOptions.Inspect m_optOptions
    Case "associations"
        lstFileTypes.Visible = True
        RefreshFileTypes
    Case "editor scripts"
    Case Else
    End Select
End Sub

Private Sub Form_Activate()
On Error Resume Next
    dtSelected_TabSelected dtSelected.SelectedTab
End Sub

Private Sub Form_Load()
On Error Resume Next
    Set m_optOptions = g_edEditor.Options.Duplicate
    dtSelected.Tabs.Add "General", , "General"
    dtSelected.Tabs.Add "Associations", , "File Associations"
    dtSelected.Tabs.Add "Editor Scripts", , "Editor Scripts"
    insOptions.ShowHierarchy = False
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
End Sub
