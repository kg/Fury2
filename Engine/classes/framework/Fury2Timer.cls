VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Timer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Timer Object"
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
Implements Fury2Object

Private mvarFireCode As String
Private mvarDelay As Long
Private mvarBuildup As Long
Private mvarEnabled As Boolean
Private mvarLastFiring As Long
Private mvarRealtime As Boolean
Private mvarGameOnly As Boolean
Private mvarOneShot As Boolean
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Property Let OneShot(ByVal vData As Boolean)
    mvarOneShot = vData
End Property

Public Property Get OneShot() As Boolean
    OneShot = mvarOneShot
End Property

Public Property Let Realtime(ByVal vData As Boolean)
    mvarRealtime = Realtime
End Property

Public Property Get Realtime() As Boolean
    Realtime = mvarRealtime
End Property

Public Property Let GameOnly(ByVal vData As Boolean)
    mvarGameOnly = GameOnly
End Property

Public Property Get GameOnly() As Boolean
    GameOnly = mvarGameOnly
End Property

Public Property Let Enabled(ByVal vData As Boolean)
    mvarEnabled = vData
    mvarLastFiring = CLng(Engine.HiTimer * 1000)
End Property

Public Property Get Enabled() As Boolean
    Enabled = mvarEnabled
End Property

Public Property Let Delay(ByVal vData As Long)
    mvarDelay = vData
End Property

Public Property Get Delay() As Long
    Delay = mvarDelay
End Property

Public Property Let FireCode(ByVal vData As String)
    mvarFireCode = vData
End Property

Public Property Get FireCode() As String
    FireCode = mvarFireCode
End Property

Public Sub Update()
On Error Resume Next
Dim m_lngTime As Long
Dim m_lngUpdatesDue As Long
Dim m_lngUpdates As Long
    If Not mvarEnabled Then Exit Sub
    Err.Clear
    m_lngTime = CLng(Engine.HiTimer * 1000)
    If mvarRealtime Then
        mvarBuildup = mvarBuildup + (m_lngTime - mvarLastFiring)
        mvarLastFiring = m_lngTime
        m_lngUpdatesDue = mvarBuildup \ mvarDelay
        mvarBuildup = mvarBuildup Mod mvarDelay
        If m_lngUpdatesDue <= 0 Then Err.Clear: Exit Sub
        If m_lngUpdatesDue > c_lngMaxTimerUpdates Then m_lngUpdatesDue = c_lngMaxTimerUpdates
        For m_lngUpdates = 1 To m_lngUpdatesDue
            If (Engine.Paused = False) Or (mvarGameOnly = False) Then
                If mvarOneShot Then mvarEnabled = False
                Engine.ContextLevelAdd "Timer_Fire"
                Err.Clear
                Engine.ScriptEngine.AddCode mvarFireCode
                If Err <> 0 Then mvarEnabled = False
                Err.Clear
                Engine.ContextLevelRemove
            End If
        Next m_lngUpdates
        Err.Clear
    Else
        If (m_lngTime) >= (mvarLastFiring + mvarDelay) Then
            If (Engine.Paused = False) Or (mvarGameOnly = False) Then
                If mvarOneShot Then mvarEnabled = False
                mvarLastFiring = m_lngTime
                Engine.ContextLevelAdd "Timer_Fire"
                Err.Clear
                Engine.ScriptEngine.AddCode mvarFireCode
                If Err <> 0 Then mvarEnabled = False
                Err.Clear
                Engine.ContextLevelRemove
            End If
        End If
    End If
    Err.Clear
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    mvarLastFiring = Engine.HiTimer * 1000
End Sub

Private Sub Class_Terminate()
On Error Resume Next
End Sub
