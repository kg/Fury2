VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Counter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
Implements IVirtualFileSaveable
Implements IInspectable
Implements Fury2Object
Public Offset As Double
Public StartTime As Double
Public Interval As Double
Public Engine As Fury2Engine
Private m_booPaused As Boolean
Private m_dblPausedSince As Double

Public Sub Restart()
On Error Resume Next
    StartTime = Engine.HiTimer
    If m_booPaused Then
        m_dblPausedSince = StartTime
    End If
End Sub

Public Sub Reset()
On Error Resume Next
    StartTime = Engine.HiTimer
    m_booPaused = True
    m_dblPausedSince = StartTime
End Sub

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Counter: " & Value
End Function

Public Property Get Paused() As Boolean
On Error Resume Next
    Paused = m_booPaused
End Property

Public Property Let Paused(ByVal NewValue As Boolean)
On Error Resume Next
Dim l_dblPausedTime As Double
    If NewValue = m_booPaused Then Exit Property
    If NewValue = True Then
        m_dblPausedSince = Engine.HiTimer
    Else
        l_dblPausedTime = Engine.HiTimer - m_dblPausedSince
        StartTime = StartTime + l_dblPausedTime
    End If
    m_booPaused = NewValue
End Property

Public Property Get Value() As Double
On Error Resume Next
    If m_booPaused Then
        Value = ((m_dblPausedSince - StartTime) / (Interval)) + Offset
    Else
        Value = ((Engine.HiTimer - StartTime) / (Interval)) + Offset
    End If
End Property

Public Sub Start(ByVal Interval As Double)
On Error Resume Next
    m_booPaused = False
    StartTime = Engine.HiTimer
    Me.Interval = Interval
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Interval = 1
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    File.ReadSegment_Begin
        File.Load StartTime
        File.Load Interval
    File.ReadSegment_End
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    File.WriteSegment_Begin
        File.Save StartTime
        File.Save Interval
    File.WriteSegment_End
End Sub

