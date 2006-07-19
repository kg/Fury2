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
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
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

Option Explicit
Public ActivateEvent As Fury2Event
Public DeactivateEvent As Fury2Event
Public Repeat As Boolean
Public Enabled As Boolean
Public Consumed As Boolean
Public Handled As Boolean
Public RepeatDelay As Single

Private m_dblLastActivate As Single
Private m_booReset As Boolean
Private m_lngState As Long
Private m_strName As String
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

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

Friend Sub ResetRepeat()
On Error Resume Next
    m_dblLastActivate = SoftFX.GetTime
End Sub

Friend Sub TriggerRepeat()
On Error Resume Next
Dim l_dblNow As Single, l_dblElapsed As Double, l_dblDelay As Double
Dim l_lngElapsed As Long, l_lngTriggers As Long
    If (Repeat) Then
        If (m_lngState > 0) Then
            l_dblNow = SoftFX.GetTime()
            l_dblDelay = IIf(RepeatDelay = -1, Engine.RepeatDelay, RepeatDelay)
            l_dblElapsed = (l_dblNow - m_dblLastActivate) / l_dblDelay
            l_lngElapsed = Floor(l_dblElapsed)
            If l_lngElapsed > 0 Then
                m_dblLastActivate = m_dblLastActivate + (l_dblDelay * l_lngElapsed)
                For l_lngTriggers = 1 To l_lngElapsed
                    m_booReset = False
                    Handled = False
                    Engine.ControlActivate Me
                    If Not (ActivateEvent Is Nothing) Then ActivateEvent.Invoke
                Next l_lngTriggers
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
        m_dblLastActivate = SoftFX.GetTime()
        Engine.ControlActivate Me
        If Not (ActivateEvent Is Nothing) Then ActivateEvent.Invoke
    End If
End Sub

Public Sub Deactivate()
On Error Resume Next
Dim l_booEvent As Boolean
    Handled = False
    If m_lngState > 0 Then l_booEvent = True
    m_lngState = m_lngState - 1
    If m_lngState < 0 Then m_lngState = 0
    If l_booEvent Then
        Engine.ControlDeactivate Me
        If Not (DeactivateEvent Is Nothing) Then DeactivateEvent.Invoke
    End If
End Sub

Private Sub Class_Initialize()
    Enabled = True
    RepeatDelay = -1
End Sub
