VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2TimerEvent"
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
Private m_mapMap As Fury2Map
Private m_strScript As String, m_objScript As Object, m_booScriptFailed As Boolean
Private m_lngType As Long
Private m_lngBeginTime As Long, m_lngEndTime As Long
Private m_booRepeats As Boolean, m_lngRepeatTiming As Long
Private m_booTriggered As Boolean, m_booEnded As Boolean
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Sub Free()
On Error Resume Next
    Set m_mapMap = Nothing
    Set m_objScript = Nothing
End Sub

Public Property Get ScriptSource() As String
    ScriptSource = m_strScript
End Property
Public Property Let ScriptSource(NewValue As String)
    m_strScript = NewValue
End Property

Public Property Get ActivationType() As Long
    ActivationType = m_lngType
End Property
Public Property Let ActivationType(NewValue As Long)
    m_lngType = NewValue
End Property

Public Property Get BeginTime() As Long
    BeginTime = m_lngBeginTime
End Property
Public Property Let BeginTime(NewValue As Long)
    m_lngBeginTime = NewValue
End Property

Public Property Get EndTime() As Long
    EndTime = m_lngEndTime
End Property
Public Property Let EndTime(NewValue As Long)
    m_lngEndTime = NewValue
End Property

Public Property Get Length() As Long
    Length = m_lngEndTime - m_lngBeginTime
End Property
Public Property Let Length(NewValue As Long)
    m_lngEndTime = m_lngBeginTime + NewValue
End Property

Public Property Get Repeat() As Boolean
    Repeat = m_booRepeats
End Property
Public Property Let Repeat(NewValue As Boolean)
    m_booRepeats = NewValue
End Property

Public Property Get RepeatDelay() As Long
    RepeatDelay = m_lngRepeatTiming
End Property
Public Property Let RepeatDelay(NewValue As Long)
    m_lngRepeatTiming = NewValue
End Property

Friend Sub SetParent(Map As Fury2Map)
On Error Resume Next
    Set m_mapMap = Map
End Sub

Friend Sub Began()
On Error Resume Next
    m_booTriggered = True
    m_booEnded = False
    Script.Began
End Sub

Friend Sub AlreadyBegan()
On Error Resume Next
    m_booTriggered = True
    m_booEnded = False
    Script.AlreadyBegan
End Sub

Friend Sub Ended()
On Error Resume Next
Dim l_lngLength As Long
    m_booEnded = True
    If m_booRepeats Then
        m_booTriggered = False
        l_lngLength = m_lngEndTime - m_lngBeginTime
        m_lngBeginTime = Engine.GameTime + m_lngRepeatTiming
        m_lngEndTime = Engine.GameTime + m_lngRepeatTiming + l_lngLength
    End If
    Script.Ended
End Sub

Public Property Get Active() As Boolean
On Error Resume Next
    Select Case m_lngType
    Case Fury2TimerEventTypes.F2TE_Normal
        Active = (Engine.GameTime >= m_lngBeginTime) And (Engine.GameTime <= m_lngEndTime)
    Case Fury2TimerEventTypes.F2TE_Exact
        Active = ((Engine.GameTime >= m_lngBeginTime) And (Engine.GameTime <= m_lngEndTime)) And (m_booTriggered)
    Case Else
    End Select
End Property

Public Sub Update()
On Error Resume Next
Dim l_lngMissed As Long, l_lngLength As Long

    If m_booRepeats Then
        If Engine.GameTime > m_lngEndTime Then
            l_lngMissed = Ceil((Engine.GameTime - m_lngEndTime) / m_lngRepeatTiming)
            l_lngLength = m_lngEndTime - m_lngBeginTime
            m_lngBeginTime = m_lngEndTime + (m_lngRepeatTiming * l_lngMissed)
            m_lngEndTime = m_lngBeginTime + l_lngLength
        End If
    End If

    If m_booTriggered Then
    Else
        If m_lngType = Fury2TimerEventTypes.F2TE_Exact Then
            If Engine.GameTime = m_lngBeginTime Then
                Began
            End If
        Else
            If Active Then
                If Engine.GameTime = m_lngBeginTime Then
                    Began
                Else
                    AlreadyBegan
                End If
            End If
        End If
    End If
    
    If (m_booTriggered And (Not Active)) And (Not m_booEnded) Then
        Ended
    End If
End Sub

Public Property Get Script() As Object
On Error Resume Next
Dim l_strScript As String
    Err.Clear
    If Trim(m_strScript) = "" Then Exit Property
    If m_objScript Is Nothing Then
        If (m_booScriptFailed = False) Then
            Err.Clear
            With Engine.ScriptEngine.Language
                l_strScript = .GenerateClassVariable("Object") & .LineTerminator & _
                .GenerateClassVariable("Map") & .LineTerminator & _
                .GenerateClassMethod("F2OInit", Array("newThis", "newMap"), _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "newThis", True) & .LineTerminator & _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "newMap", True) _
                ) & .LineTerminator & ScriptSource
                Set m_objScript = Engine.GenerateScriptObject(l_strScript)
            End With
            Set Engine.IObjThis = Me
            m_objScript.F2OInit Me, m_mapMap
            m_objScript.Initialize
            If (Err.Number <> 0) Or (m_objScript Is Nothing) Then m_booScriptFailed = True
        End If
    End If
    Set Script = m_objScript
    Err.Clear
End Property

