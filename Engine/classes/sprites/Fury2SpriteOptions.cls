VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SpriteOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Sprite Parameter List"
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
Public Obj As Fury2Sprite

Public Property Get Alpha() As Single
    Alpha = Obj.Alpha
End Property

Public Property Let Alpha(NewAlpha As Single)
    If Obj.Alpha <> NewAlpha Then Obj.Alpha = NewAlpha
End Property

Public Property Get Angle() As Single
    Angle = Obj.Angle
End Property

Public Property Let Angle(NewAngle As Single)
    If Obj.Angle <> NewAngle Then Obj.Angle = NewAngle
End Property

Public Property Get BlitMode() As Long
    BlitMode = Obj.BlitMode
End Property

Public Property Let BlitMode(NewBlitMode As Long)
    If Obj.BlitMode <> NewBlitMode Then Obj.BlitMode = NewBlitMode
End Property

Public Function BlitMode_Options() As Variant
    BlitMode_Options = Array("Masked", "Source Alpha", "Additive", "Subtractive", "Adjusted", "AND", "OR", "XOR", "Lightsource")
End Function

Public Property Get Frame() As Long
    Frame = Obj.Frame
End Property

Public Property Let Frame(NewFrame As Long)
    If Obj.Frame <> NewFrame Then Obj.Frame = NewFrame
End Property

Public Property Get Name() As String
    Name = Obj.Name
End Property

Public Property Let Name(NewName As String)
    If Obj.Name <> NewName Then Obj.Name = NewName
End Property

Public Property Get Pose() As Long
    Pose = Obj.Pose
End Property

Public Property Let Pose(NewPose As Long)
    If Obj.Pose <> NewPose Then Obj.Pose = NewPose
End Property

Public Property Get X() As Single
    X = Obj.X
End Property

Public Property Let X(NewX As Single)
    If Obj.X <> NewX Then Obj.X = NewX
End Property

Public Property Get Y() As Single
    Y = Obj.Y
End Property

Public Property Let Y(NewY As Single)
    If Obj.Y <> NewY Then Obj.Y = NewY
End Property

Public Function MemberList() As Variant
    MemberList = Array("Alpha", "Angle", "BlitMode", "Frame", "Name", "Pose", _
    "X", "Y")
End Function

