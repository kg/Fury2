VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2InputControl"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    Engine (Fury� Game Creation System Runtime Engine)
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
Public ActivateEvent As String
Public DeactivateEvent As String
Public Repeat As Boolean
Public Enabled As Boolean
Public Consumed As Boolean
Public Handled As Boolean

Private m_sngLastActivate As Single
Private m_booReset As Boolean
Private m_lngState As Long
Private m_strName As String

Public Sub Consume()
    Consumed = True
End Sub

Public Property Get Name() As String
    Name = m_strName
End Property

Public Property Let Name(NewName As String)
    m_strName = NewName
End Property

Public Property Get State() As Boolean
    State = (m_lngState > 0) And (m_booReset = False) And (Enabled)
End Property

Public Sub ClearState()
    m_lngState = 0
End Sub

Public Sub Reset()
    m_booReset = True
End Sub

Public Sub Trigger()
    Activate
    Deactivate
End Sub

Public Sub TriggerRepeat()
On Error Resume Next
Dim l_sngNow As Single
    If (Repeat) Then
        If (m_lngState > 0) Then
            l_sngNow = m_Engine.HiTimer
            If ((l_sngNow - m_sngLastActivate) * 1000) > m_Engine.RepeatDelay Then
                m_sngLastActivate = m_Engine.HiTimer
                m_booReset = False
                Handled = False
                m_Engine.ControlActivate Me
            End If
        End If
    End If
End Sub

Public Sub Activate()
On Error Resume Next
Dim l_booEvent As Boolean
    Handled = False
    If m_lngState = 0 Then l_booEvent = True
    m_lngState = m_lngState + 1
    m_booReset = False
    If l_booEvent = True Then
        m_sngLastActivate = m_Engine.HiTimer
        m_Engine.ControlActivate Me
    End If
End Sub

Public Sub Deactivate()
On Error Resume Next
Dim l_booEvent As Boolean
    Handled = False
    If m_lngState > 0 Then l_booEvent = True
    m_lngState = m_lngState - 1
    If m_lngState < 0 Then m_lngState = 0
    If l_booEvent Then m_Engine.ControlDeactivate Me
End Sub

Private Sub Class_Initialize()
    Enabled = True
End Sub