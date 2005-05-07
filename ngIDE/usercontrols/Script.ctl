VERSION 5.00
Object = "{665BF2B8-F41F-4EF4-A8D0-303FBFFC475E}#2.0#0"; "cmcs21.ocx"
Begin VB.UserControl Script 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForwardFocus    =   -1  'True
   KeyPreview      =   -1  'True
   ScaleHeight     =   3600
   ScaleWidth      =   4800
   Begin CodeSenseCtl.CodeSense csScript 
      Height          =   3450
      Left            =   0
      OleObjectBlob   =   "Script.ctx":0000
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   0
      Width           =   4680
   End
End
Attribute VB_Name = "Script"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
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
Public Event Change()
Public Event SelectionChange()

Public Property Get Text() As String
    Text = csScript.Text
End Property

Public Property Let Text(ByRef NewText As String)
    csScript.Text = NewText
End Property

Public Property Get Control() As CodeSense
Attribute Control.VB_UserMemId = 0
Attribute Control.VB_MemberFlags = "600"
On Error Resume Next
    Set Control = csScript
End Property

Private Sub csScript_Change(ByVal Control As CodeSenseCtl.ICodeSense)
    RaiseEvent Change
End Sub

Private Sub csScript_SelChange(ByVal Control As CodeSenseCtl.ICodeSense)
    RaiseEvent SelectionChange
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
    csScript.Move 0, 0, UserControl.ScaleWidth, UserControl.ScaleHeight
End Sub

Private Sub UserControl_Show()
On Error Resume Next
    InitializeF2Script
    csScript.Language = "F2Script"
    With csScript
        .SetColor cmClrNumber, RGB(0, 64, 96)
        .SetColor cmClrKeyword, RGB(0, 0, 160)
        .SetColor cmClrOperator, RGB(160, 0, 0)
    End With
End Sub
