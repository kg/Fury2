VERSION 5.00
Begin VB.Form frmLog 
   BackColor       =   &H00000000&
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Log"
   ClientHeight    =   1650
   ClientLeft      =   60
   ClientTop       =   330
   ClientWidth     =   4770
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   9
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmLog.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   110
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   318
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.PictureBox picBuffer 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   1740
      ScaleHeight     =   29
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   77
      TabIndex        =   2
      Top             =   255
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.Timer tmrStart 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   1740
      Top             =   255
   End
   Begin VB.TextBox txtCommand 
      BackColor       =   &H00303030&
      BorderStyle     =   0  'None
      ForeColor       =   &H00E0E0E0&
      Height          =   255
      Left            =   45
      TabIndex        =   1
      Top             =   1350
      Width           =   4590
   End
   Begin VB.ListBox lstLog 
      Appearance      =   0  'Flat
      BackColor       =   &H00303030&
      ForeColor       =   &H00E0E0E0&
      Height          =   1290
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4620
   End
End
Attribute VB_Name = "frmLog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    Engine (Fury² Game Creation System Runtime Engine)
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

Public m_Icon As IPictureDisp

Private Sub Form_Activate()
On Error Resume Next
    #If DebugFeatures = 1 Then
        If m_Engine Is Nothing Then
        Else
            If m_Engine.HideLog Then Me.Visible = False
        End If
    #Else
        Me.Visible = False
    #End If
End Sub

Sub Form_Load()
On Error Resume Next
    #If DebugFeatures = 1 Then
        If m_Engine Is Nothing Then
        Else
            If m_Engine.HideLog Then Me.Visible = False
        End If
    #Else
        Me.Visible = False
    #End If
End Sub

Private Sub Form_Resize()
On Error Resume Next
    lstLog.Move 2, 2, Me.ScaleWidth - 4, Me.ScaleHeight - (6 + txtCommand.Height)
    txtCommand.Move 2, lstLog.top + lstLog.Height + 2, lstLog.Width, txtCommand.Height
    If (lstLog.Height / TextHeight("AaBbYyZz")) > 0 Then
        Do While lstLog.ListCount >= (lstLog.Height / TextHeight("AaBbYyZz"))
            lstLog.RemoveItem 0
        Loop
    End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
On Error Resume Next
    Unload frmTrace
    Set m_Icon = Nothing
End Sub

Private Sub lstLog_Click()
On Error Resume Next
    lstLog.ListIndex = -1
End Sub

Private Sub lstLog_GotFocus()
On Error Resume Next
    txtCommand.SetFocus
End Sub

Private Sub tmrStart_Timer()
On Error Resume Next
End Sub

Private Sub txtCommand_KeyPress(KeyAscii As Integer)
On Error Resume Next
Dim m_strOldContext As String
    If KeyAscii = 13 Then
        txtCommand.Locked = True
        KeyAscii = 0
        Err.Clear
        If left(Trim(txtCommand.Text), 1) = "?" Then
            txtCommand.Text = m_Engine.ScriptEngine.Language.GenerateFunctionCall("TextOut", Mid(Trim(txtCommand.Text), 3)) + m_Engine.ScriptEngine.Language.lineterminator
        End If
        Err.Clear
        m_strOldContext = m_Engine.ScriptContext
        m_Engine.ScriptContext = "Console"
        m_Engine.ScriptEngine.AddCode txtCommand.Text
        m_Engine.ScriptContext = m_strOldContext
        If (Err.Number = 0) And (m_Engine.ScriptEngine.Error.Number = 0) Then txtCommand.Text = ""
        txtCommand.Locked = False
    End If
End Sub
