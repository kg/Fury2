VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SoundObject"
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
Implements Fury2Object
Implements IVirtualFileSaveable
Implements Fury2MapObject
Implements IInspectable
Public X As Single
Public Y As Single
Public FalloffOffset As Single
Public FalloffDistance As Single
Public Volume As Single
Public Sound As String
Public Repeat As Boolean
Public Enabled As Boolean
Public Handle As SoundHandle
Public Engine As Fury2Engine
Public Map As Fury2Map
Public JustLoaded As Boolean
Private Image As Fury2Image
Private mvarName As String

Public Property Get Visible() As Boolean
Attribute Visible.VB_Description = "*"
Attribute Visible.VB_MemberFlags = "40"
    Visible = Enabled
End Property

Public Property Let Visible(ByRef Value As Boolean)
    Enabled = Value
End Property

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Let Name(ByRef Value As String)
    mvarName = Value
End Property

Public Function CalculateVolume() As Long
On Error Resume Next
Dim l_sngDistance As Single
Dim l_sngVolume As Single
    l_sngDistance = Engine.DistanceBetween(Engine.SoundListener.X, Engine.SoundListener.Y, X, Y)
    If (l_sngDistance < FalloffOffset) Then
        l_sngVolume = 1
    Else
        l_sngVolume = 1 - ((l_sngDistance - FalloffOffset) / FalloffDistance)
        If (l_sngVolume < 0) Then l_sngVolume = 0
        If (l_sngVolume > 1) Then l_sngVolume = 1
    End If
    CalculateVolume = l_sngVolume * Volume
End Function

Public Sub Trigger()
On Error Resume Next
    If Not Enabled Then Exit Sub
    Handle.PlayEx CalculateVolume
End Sub

Private Sub Class_Initialize()
    Volume = 1#
    Enabled = True
    Name = "Sound"
End Sub

Private Sub Fury2MapObject_Load(Engine As Fury2Engine, ByRef Map As Fury2Map)
On Error Resume Next
Dim l_lngHandle As Long
    l_lngHandle = Engine.SoundEngine.Load(Sound, Repeat)
    If l_lngHandle <> 0 Then
        Set Handle = Engine.SoundEngine.Handles(l_lngHandle)
    Else
    End If
    Set Me.Engine = Engine
    Set Me.Map = Map
    JustLoaded = True
End Sub

Private Sub Fury2MapObject_Render(Surface As libGraphics.Fury2Image, ByVal XOffset As Long, ByVal YOffset As Long)
End Sub

Private Sub Fury2MapObject_Update()
On Error Resume Next
    If Handle.Playing Then
        Handle.Volume = CalculateVolume()
    ElseIf JustLoaded Then
        JustLoaded = False
        Trigger
    ElseIf Repeat Then
        Trigger
    End If
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
            .Load X
            .Load Y
            .Load FalloffOffset
            .Load FalloffDistance
            .Load Volume
            .Load Sound
            .Load Repeat
            .Load Enabled
            .Load Name
        .ReadSegment_End
    End With
    JustLoaded = False
    Set Engine = Nothing
    Set Handle = Nothing
    Set Map = Nothing
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save X
            .Save Y
            .Save FalloffOffset
            .Save FalloffDistance
            .Save Volume
            .Save Sound
            .Save Repeat
            .Save Enabled
            .Save Name
        .WriteSegment_End
    End With
End Sub
