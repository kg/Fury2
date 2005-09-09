VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cChildManager"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
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

' Handles notifying the main form of child changes

Option Explicit
Private Declare Function SetFocus Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function SetForegroundWindow Lib "user32" (ByVal hwnd As Long) As Long
Private WithEvents m_frmForm As Form
Attribute m_frmForm.VB_VarHelpID = -1

Public Property Let Visible(NewValue As Boolean)
On Error Resume Next
    With m_frmForm
        .Visible = NewValue
    End With
    With extender
        .Visible = NewValue
    End With
End Property

Public Sub Activate()
On Error Resume Next
Dim l_objForm As Object
    Err.Clear
    With extender
        .SetFocus
    End With
    SetForegroundWindow m_frmForm.hwnd
    SetFocus m_frmForm.hwnd
    m_frmForm.Form_Activate
    g_edEditor.Event_DocumentActivate Me
    If g_booMainWindowLoaded Then frmMain.RefreshWindows
End Sub

Public Sub Hide()
On Error Resume Next
    If frmMain.Documents.Find(Me) < 1 Then Exit Sub
    If g_edEditor.Event_DocumentClose(Me) Then Exit Sub
    SetBusyState True
    If Not (Form Is Nothing) Then
        Unload Form
    End If
    Detach
    SetBusyState False
End Sub

Public Property Get extender() As MDIActiveX
On Error Resume Next
Dim l_objForm As iExtendedForm
    Set l_objForm = m_frmForm
    Set extender = l_objForm.extender
    Err.Clear
End Property

Public Property Get Menus() As iCustomMenus
On Error Resume Next
    Set Menus = m_frmForm
End Property

Public Property Get Accelerators() As iCustomAccelerators
On Error Resume Next
    Set Accelerators = m_frmForm
End Property

Public Property Get Toolbar() As iToolbar
On Error Resume Next
    Set Toolbar = m_frmForm
End Property

Public Property Get Document() As iDocument
On Error Resume Next
    Set Document = m_frmForm
End Property

Public Property Get Form() As Form
On Error Resume Next
    Set Form = m_frmForm
End Property

Public Sub Attach(ByRef NewForm As Form)
On Error Resume Next
    Set m_frmForm = NewForm
End Sub

Public Sub Detach()
On Error Resume Next
    Set m_frmForm = Nothing
    If g_booMainWindowLoaded Then frmMain.DereferenceChildManager Me
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Debug.Print "cChildManager_Initialize"
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Debug.Print "cChildManager_Terminate"
End Sub

Private Sub m_frmForm_Activate()
On Error Resume Next
    If m_frmForm.WindowState = 0 Then
        m_frmForm.WindowState = 2
    End If
End Sub

Private Sub m_frmForm_Unload(Cancel As Integer)
On Error Resume Next
    Detach
    If g_booMainWindowLoaded Then frmMain.RefreshWindows
End Sub
