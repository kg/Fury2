VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Keyboard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Integer

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

Implements Fury2Object
Private m_engEngine As Fury2Engine
Private m_booEnabled As Boolean
Private m_objBindings(0 To c_lngKeyCount) As Object
Private m_booState(0 To c_lngKeyCount) As Boolean
Private m_booActive(0 To c_lngKeyCount) As Boolean

Public Property Get Enabled() As Boolean
    Enabled = m_booEnabled
End Property

Public Property Let Enabled(ByVal NewValue As Boolean)
    m_booEnabled = NewValue
End Property

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Property Get Binding(ByVal Key As Long) As Object
On Error Resume Next
    If Key < 0 Or Key > c_lngKeyCount Then Exit Property
    Set Binding = m_objBindings(Key)
End Property

Public Property Set Binding(ByVal Key As Long, ByVal NewValue As Object)
On Error Resume Next
Dim l_ctlControl As Fury2InputControl
Dim l_evtEvent As Fury2Event
    If Key < 0 Or Key > c_lngKeyCount Then Exit Property
    If TypeOf NewValue Is Fury2InputControl Then
        Set l_ctlControl = NewValue
        Set m_objBindings(Key) = l_ctlControl
    ElseIf TypeOf NewValue Is Fury2Event Then
        Set l_evtEvent = NewValue
        Set m_objBindings(Key) = l_evtEvent
    End If
End Property

Public Property Get State(ByVal Key As Long) As Boolean
On Error Resume Next
    If Key < 0 Or Key > c_lngKeyCount Then Exit Property
    State = m_booState(Key)
End Property

Public Property Let State(ByVal Key As Long, ByVal NewValue As Boolean)
On Error Resume Next
Dim l_evtBinding As Fury2Event
Dim l_ctlBinding As Fury2InputControl
    If Key < 0 Or Key > c_lngKeyCount Then Exit Property
    If m_booState(Key) = NewValue Then Exit Property
    Set l_evtBinding = m_objBindings(Key)
    Set l_ctlBinding = m_objBindings(Key)
    If Not (l_evtBinding Is Nothing) Then
        If NewValue Then
            l_evtBinding.Invoke
        End If
    ElseIf Not (l_ctlBinding Is Nothing) Then
        If NewValue Then
            l_ctlBinding.Activate
        Else
            l_ctlBinding.Deactivate
        End If
    End If
    m_booState(Key) = NewValue
End Property

Friend Sub KeyDown(ByVal Key As Long, Optional ByVal EnableRepeat As Boolean = False)
On Error Resume Next
Dim l_evtBinding As Fury2Event
Dim l_ctlBinding As Fury2InputControl
    If Key < 0 Or Key > c_lngKeyCount Then Exit Sub
    If m_booState(Key) Then
        Exit Sub
    End If
    Set l_evtBinding = m_objBindings(Key)
    Set l_ctlBinding = m_objBindings(Key)
    m_booState(Key) = True
'    If m_booActive(Key) Then Exit Sub
'    m_booActive(Key) = True
    If Not (l_evtBinding Is Nothing) Then
        l_evtBinding.Invoke
    ElseIf Not (l_ctlBinding Is Nothing) Then
        If l_ctlBinding.State Then
        Else
            l_ctlBinding.Activate
        End If
    End If
'    m_booActive(Key) = False
End Sub

Friend Sub KeyUp(ByVal Key As Long)
On Error Resume Next
Dim l_evtBinding As Fury2Event
Dim l_ctlBinding As Fury2InputControl
    If Key < 0 Or Key > c_lngKeyCount Then Exit Sub
    If Not m_booState(Key) Then Exit Sub
    m_booState(Key) = False
    Set l_evtBinding = m_objBindings(Key)
    Set l_ctlBinding = m_objBindings(Key)
    If Not (l_evtBinding Is Nothing) Then
    ElseIf Not (l_ctlBinding Is Nothing) Then
        l_ctlBinding.Deactivate
    End If
End Sub

Public Sub Reset()
On Error Resume Next
Dim l_lngKey As Long
    For l_lngKey = 0 To c_lngKeyCount
        State(l_lngKey) = False
    Next l_lngKey
End Sub

Public Sub Update()
On Error Resume Next
Dim l_lngKeys As Long
Dim l_booState As Boolean
    For l_lngKeys = 32 To 127
        l_booState = (GetAsyncKeyState(l_lngKeys) < 0)
        If l_booState <> m_booState(l_lngKeys) Then
            If l_booState = False Then
                KeyUp l_lngKeys
'                Engine.QueueEvent 1, , , CLng(l_lngKeys), 0, 0
            End If
        End If
    Next l_lngKeys
End Sub

Private Sub Class_Initialize()
    m_booEnabled = True
End Sub
