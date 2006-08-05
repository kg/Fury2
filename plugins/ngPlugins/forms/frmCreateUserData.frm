VERSION 5.00
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#13.2#0"; "ngUI.ocx"
Begin VB.Form frmCreateUserData 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Create User Data"
   ClientHeight    =   5250
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
   Icon            =   "frmCreateUserData.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   350
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   500
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin ngPlugins.Script scExpression 
      Height          =   3600
      Left            =   675
      TabIndex        =   3
      Top             =   1065
      Width           =   4800
      _ExtentX        =   8467
      _ExtentY        =   6350
   End
   Begin ngUI.ngListBox lstClasses 
      Height          =   4890
      Left            =   30
      TabIndex        =   2
      Top             =   285
      Width           =   5835
      _ExtentX        =   10292
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
Attribute VB_Name = "frmCreateUserData"
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
Public Expression As String

Public Sub RefreshClassList()
On Error Resume Next
Dim l_clsClass As ScriptClass
Dim l_fnFunction As ScriptFunction
    lstClasses.ListItems.Clear
    For Each l_clsClass In Engine.ScriptEngine.State.Classes
        If Left(l_clsClass.Name, 2) = "SO" Then
            ' Exclude script objects
        Else
            lstClasses.ListItems.AddNew l_clsClass.Name, l_clsClass.Name
        End If
    Next l_clsClass
    cmdOK.Enabled = lstClasses.SelectedItemCount > 0
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Cancelled = True
    Me.Hide
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
    If Len(Expression) < 1 Then Cancelled = True Else Cancelled = False
    Me.Hide
End Sub

Private Sub lstClasses_ItemSelect(Item As ngUI.ngListItem)
On Error Resume Next
    cmdOK.Enabled = True
    Expression = Engine.ScriptEngine.Language.GenerateInstantiation(Item.key)
End Sub

Private Sub scExpression_Change()
On Error Resume Next
    cmdOK.Enabled = True
    Expression = scExpression.Text
End Sub

Private Sub tsViews_TabSelected(TheTab As ngTab)
On Error Resume Next
    Expression = ""
    cmdOK.Enabled = False
    Select Case TheTab.Index
    Case 1
        lstClasses.Visible = True
        scExpression.Visible = False
        RefreshClassList
    Case 2
        scExpression.Visible = True
        lstClasses.Visible = False
        scExpression_Change
    Case Else
    End Select
End Sub

Private Sub Form_Load()
On Error Resume Next
    tsViews.Tabs.AddNew "Existing Class"
    tsViews.Tabs.AddNew "Expression"
    lstClasses.Move tsViews.Left + 2, tsViews.Top + tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - tsViews.IdealHeight - 3
    scExpression.Move lstClasses.Left, lstClasses.Top, lstClasses.Width, lstClasses.Height
    lstClasses.Colors(lbcBackground) = ConvertSystemColor(SystemColor_Button_Highlight)
    tsViews_TabSelected tsViews.Tabs(1)
End Sub
