VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Slider"
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
Implements IInspectable
Implements IVirtualFileSaveable
Implements Fury2Object
Public StartTime As Double
Attribute StartTime.VB_VarDescription = "The time when this slider was started."
Public EndTime As Double
Attribute EndTime.VB_VarDescription = "The time when this slider will end."
Private m_booPaused As Boolean
Private m_dblPausedSince As Double
Public Engine As Fury2Engine

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Slider: " & Progress
End Function

Public Property Get Paused() As Boolean
Attribute Paused.VB_Description = "If True, the slider is paused and its progress will not increase until it is unpaused."
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
        EndTime = EndTime + l_dblPausedTime
    End If
    m_booPaused = NewValue
End Property

Public Property Get TimeLeft() As Double
Attribute TimeLeft.VB_Description = "The amount of time left (in seconds) until this slider is finished."
On Error Resume Next
    If m_booPaused Then
        TimeLeft = EndTime - m_dblPausedSince
    Else
        TimeLeft = EndTime - Engine.HiTimer
    End If
    If TimeLeft < 0 Then TimeLeft = 0
End Property

Public Property Get Finished() As Boolean
Attribute Finished.VB_Description = "If True, this slider has finished progressing."
On Error Resume Next
    If m_booPaused Then
        Finished = False
    Else
        Finished = Engine.HiTimer >= EndTime
    End If
End Property

Public Property Get Progress() As Double
Attribute Progress.VB_Description = "The progress (0.0 - 1.0) of this slider."
On Error Resume Next
    If m_booPaused Then
        Progress = (m_dblPausedSince - StartTime) / (EndTime - StartTime)
    Else
        Progress = (Engine.HiTimer - StartTime) / (EndTime - StartTime)
    End If
    If Progress < 0 Then Progress = 0
    If Progress > 1 Then Progress = 1
End Property

Public Sub Start(ByVal Length As Double)
Attribute Start.VB_Description = "Starts the slider progressing for the next Length seconds."
On Error Resume Next
    m_booPaused = False
    StartTime = Engine.HiTimer
    EndTime = StartTime + Length
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    File.ReadSegment_Begin
        File.Load StartTime
        File.Load EndTime
    File.ReadSegment_End
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    File.WriteSegment_Begin
        File.Save StartTime
        File.Save EndTime
    File.WriteSegment_End
End Sub

