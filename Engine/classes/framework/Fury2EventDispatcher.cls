VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2EventDispatcher"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
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
Implements Fury2Plugin

Public Enum DispatcherEvents
    DispEvt_ControlActivate
    DispEvt_ControlDeactivate
    DispEvt_Init
    DispEvt_KeyDown
    DispEvt_KeyPress
    DispEvt_KeyUp
    DispEvt_MapLoad
    DispEvt_MouseDown
    DispEvt_MouseMove
    DispEvt_MouseUp
    DispEvt_Redraw
    DispEvt_Shutdown
    DispEvt_Startup
    DispEvt_SyncUpdate
    DispEvt_Unload
    DispEvt_Update
    DispEvt_Max_Value
End Enum

Public Dispatchee As Object
Attribute Dispatchee.VB_VarDescription = "The object that events should be dispatched to."
Public Enabled As Boolean
Attribute Enabled.VB_VarDescription = "If True, this dispatcher is enabled."
Public DrawBehind As Boolean
Attribute DrawBehind.VB_VarDescription = "If True, redraw events are recieved before picture slots have been drawn, otherwise redraw events are recieved after picture slots have been drawn."
Public Backbuffer As Fury2Image
Private m_booEventsEnabled(DispEvt_Max_Value) As Boolean
Private m_engEngine As Fury2Engine

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Private Function CanDispatch(ByVal Evt As DispatcherEvents) As Boolean
On Error Resume Next
    If Me.Enabled = False Then Exit Function
    If Me.Dispatchee Is Nothing Then Exit Function
    CanDispatch = m_booEventsEnabled(Evt)
End Function

Public Function ResolveEvent(Index) As DispatcherEvents
On Error Resume Next
    If VarType(Index) = vbString Then
        Select Case LCase(Trim(CStr(Index)))
        Case "controlactivate"
            ResolveEvent = DispEvt_ControlActivate
        Case "controldeactivate"
            ResolveEvent = DispEvt_ControlDeactivate
        Case "init"
            ResolveEvent = DispEvt_Init
        Case "keydown"
            ResolveEvent = DispEvt_KeyDown
        Case "keypress"
            ResolveEvent = DispEvt_KeyPress
        Case "keyup"
            ResolveEvent = DispEvt_KeyUp
        Case "mapload"
            ResolveEvent = DispEvt_MapLoad
        Case "mousedown"
            ResolveEvent = DispEvt_MouseDown
        Case "mousemove"
            ResolveEvent = DispEvt_MouseMove
        Case "mouseup"
            ResolveEvent = DispEvt_MouseUp
        Case "redraw"
            ResolveEvent = DispEvt_Redraw
        Case "shutdown"
            ResolveEvent = DispEvt_Shutdown
        Case "startup"
            ResolveEvent = DispEvt_Startup
        Case "syncupdate"
            ResolveEvent = DispEvt_SyncUpdate
        Case "unload"
            ResolveEvent = DispEvt_Unload
        Case "update"
            ResolveEvent = DispEvt_Update
        Case Else
            ResolveEvent = DispEvt_Max_Value
        End Select
    Else
        ResolveEvent = CLng(Index)
    End If
End Function

Public Property Get EventEnabled(Index) As Boolean
On Error Resume Next
    EventEnabled = m_booEventsEnabled(ResolveEvent(Index))
End Property

Public Property Let EventEnabled(Index, ByVal Value As Boolean)
Attribute EventEnabled.VB_Description = "Enables dispatching of a given event to the dispatchee."
On Error Resume Next
    m_booEventsEnabled(ResolveEvent(Index)) = Value
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Enabled = True
    Set Backbuffer = Engine.Backbuffer
End Sub

Private Sub Fury2Plugin_ControlActivate(Control As Engine.Fury2InputControl)
On Error Resume Next
    If CanDispatch(DispEvt_ControlActivate) Then
        Dispatchee.Event_ControlActivate Nothing, Control
    End If
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Engine.Fury2InputControl)
On Error Resume Next
    If CanDispatch(DispEvt_ControlDeactivate) Then
        Dispatchee.Event_ControlDeactivate Nothing, Control
    End If
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
    Fury2Plugin_DrawBehind = Me.DrawBehind
End Property

Private Property Set Fury2Plugin_Engine(ByVal RHS As Engine.Fury2Engine)
    Set m_engEngine = RHS
End Property

Private Property Get Fury2Plugin_Engine() As Engine.Fury2Engine
    Set Fury2Plugin_Engine = m_engEngine
End Property

Private Sub Fury2Plugin_Init()
On Error Resume Next
    If CanDispatch(DispEvt_Init) Then
        Dispatchee.Event_Init Nothing
    End If
End Sub

Private Sub Fury2Plugin_KeyDown(KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    If CanDispatch(DispEvt_KeyDown) Then
        Dispatchee.Event_KeyDown Nothing, KeyCode, Shift
    End If
End Sub

Private Sub Fury2Plugin_KeyPress(KeyAscii As Long)
On Error Resume Next
    If CanDispatch(DispEvt_KeyPress) Then
        Dispatchee.Event_KeyPress Nothing, KeyAscii
    End If
End Sub

Private Sub Fury2Plugin_KeyUp(KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    If CanDispatch(DispEvt_KeyUp) Then
        Dispatchee.Event_KeyUp Nothing, KeyCode, Shift
    End If
End Sub

Private Sub Fury2Plugin_MapLoad(Filename As String)
On Error Resume Next
    If CanDispatch(DispEvt_MapLoad) Then
        Dispatchee.Event_MapLoad Nothing, Filename
    End If
End Sub

Private Sub Fury2Plugin_MouseDown(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If CanDispatch(DispEvt_MouseDown) Then
        Dispatchee.Event_MouseDown Nothing, Button, Shift, X, Y
    End If
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If CanDispatch(DispEvt_MouseMove) Then
        Dispatchee.Event_MouseMove Nothing, Button, Shift, X, Y
    End If
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If CanDispatch(DispEvt_MouseUp) Then
        Dispatchee.Event_MouseUp Nothing, Button, Shift, X, Y
    End If
End Sub

Private Sub Fury2Plugin_Redraw()
On Error Resume Next
    If CanDispatch(DispEvt_Redraw) Then
        Dispatchee.Event_Redraw Nothing, Me.Backbuffer, 0, 0
    End If
End Sub

Private Sub Fury2Plugin_Shutdown()
On Error Resume Next
    If CanDispatch(DispEvt_Shutdown) Then
        Dispatchee.Event_Shutdown Nothing
    End If
End Sub

Private Sub Fury2Plugin_Startup()
On Error Resume Next
    If CanDispatch(DispEvt_Startup) Then
        Dispatchee.Event_Startup Nothing
    End If
End Sub

Private Sub Fury2Plugin_SyncUpdate()
On Error Resume Next
    If CanDispatch(DispEvt_SyncUpdate) Then
        Dispatchee.Event_SyncUpdate Nothing
    End If
End Sub

Private Sub Fury2Plugin_Unload()
On Error Resume Next
    If CanDispatch(DispEvt_Unload) Then
        Dispatchee.Event_Unload Nothing
    End If
End Sub

Private Sub Fury2Plugin_Update()
On Error Resume Next
    If CanDispatch(DispEvt_Update) Then
        Dispatchee.Event_Update Nothing
    End If
End Sub

Private Sub Fury2Plugin_OutputPluginChanged()
End Sub


