VERSION 5.00
Begin VB.UserControl Script 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ControlContainer=   -1  'True
   EditAtDesignTime=   -1  'True
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
   Begin VB.TextBox txtScript 
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   11.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3600
      Left            =   0
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   0
      Top             =   0
      Width           =   4800
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

Public Property Get CanUndo() As Boolean
On Error Resume Next
    CanUndo = False
End Property

Public Property Get CanRedo() As Boolean
On Error Resume Next
    CanRedo = False
End Property

Public Property Get CanCut() As Boolean
On Error Resume Next
    CanCut = txtScript.SelLength > 0
End Property

Public Property Get CanCopy() As Boolean
On Error Resume Next
    CanCopy = txtScript.SelLength > 0
End Property

Public Property Get CanPaste() As Boolean
On Error Resume Next
    CanPaste = Clipboard.GetFormat(vbCFText)
End Property

Public Property Get CanDelete() As Boolean
On Error Resume Next
    CanDelete = txtScript.SelLength > 0
End Property

Public Sub Cut()
On Error Resume Next
    Copy
    Delete
End Sub

Public Sub Copy()
On Error Resume Next
    Clipboard.Clear
    Clipboard.SetText txtScript.SelText
End Sub

Public Sub Paste()
On Error Resume Next
    txtScript.SelText = Clipboard.GetText()
End Sub

Public Sub Delete()
On Error Resume Next
    txtScript.SelText = ""
End Sub

Public Sub Undo()
On Error Resume Next
End Sub

Public Sub Redo()
On Error Resume Next
End Sub

Public Property Get hwnd() As Long
On Error Resume Next
    hwnd = UserControl.hwnd
End Property

Public Sub OpenFile(ByVal Filename As String)
On Error Resume Next
    txtScript.Text = ReadTextFile(Filename)
End Sub

Public Property Get Text() As String
On Error Resume Next
    Text = txtScript.Text
End Property

Public Property Let Text(ByRef NewText As String)
On Error Resume Next
    txtScript.Text = NewText
End Property

Public Property Get Control() As TextBox
Attribute Control.VB_UserMemId = 0
Attribute Control.VB_MemberFlags = "600"
On Error Resume Next
    Set Control = txtScript
End Property

Private Sub txtScript_Change()
On Error Resume Next
    RaiseEvent Change
End Sub

Private Sub txtScript_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    RaiseEvent SelectionChange
End Sub

Private Sub txtScript_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    RaiseEvent SelectionChange
End Sub

Private Sub txtScript_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button <> 0 Then
        RaiseEvent SelectionChange
    End If
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
'    edScript.Resize 0, 0, UserControl.ScaleWidth, UserControl.ScaleHeight
    txtScript.Move 0, 0, UserControl.ScaleWidth, UserControl.ScaleHeight
End Sub

Private Sub UserControl_Show()
On Error Resume Next
'    tmrInit.Enabled = True
 '    InitializeF2Script
'    edScript.Language = "F2Script"
'    With edScript
'        .SetColor cmClrNumber, RGB(0, 64, 96)
'        .SetColor cmClrKeyword, RGB(0, 0, 160)
'        .SetColor cmClrOperator, RGB(160, 0, 0)
'    End With
End Sub
