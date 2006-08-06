VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2State"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Sprite State"
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

Implements IInspectable
Implements Fury2Object
Implements IVirtualFileSaveable

Private mvarMaxVelocity As Single
Private mvarDirectionPoses As Long
Private mvarPoseOffset As Long
Private mvarName As String

Public Function Duplicate() As Fury2State
Attribute Duplicate.VB_Description = "Creates a copy of this state."
On Error Resume Next
    Set Duplicate = New Fury2State
    With Duplicate
        .Name = mvarName
        .PoseOffset = mvarPoseOffset
        .DirectionPoses = mvarDirectionPoses
        .MaxVelocity = mvarMaxVelocity
    End With
End Function

Public Property Let Name(ByVal vData As String)
Attribute Name.VB_Description = "The name of this state."
    mvarName = vData
End Property

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Let PoseOffset(ByVal vData As Long)
Attribute PoseOffset.VB_Description = "The index of the first pose used by this state."
    mvarPoseOffset = vData
End Property

Public Property Get PoseOffset() As Long
    PoseOffset = mvarPoseOffset
End Property

Public Property Let MaxVelocity(ByVal vData As Single)
Attribute MaxVelocity.VB_Description = "The maximum movement velocity of a sprite in this state."
    mvarMaxVelocity = vData
End Property

Public Property Get MaxVelocity() As Single
    MaxVelocity = mvarMaxVelocity
End Property

Public Property Let DirectionPoses(ByVal vData As Long)
Attribute DirectionPoses.VB_Description = "The number of poses used by this state."
    mvarDirectionPoses = vData
End Property

Public Property Get DirectionPoses() As Long
    DirectionPoses = mvarDirectionPoses
End Property

Private Sub Class_Initialize()
    mvarDirectionPoses = 1
    mvarMaxVelocity = 1
    mvarName = "New State"
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
            .Load mvarName
            .Load mvarPoseOffset
            .Load mvarDirectionPoses
            .Load mvarMaxVelocity
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarName
            .Save mvarPoseOffset
            .Save mvarDirectionPoses
            .Save mvarMaxVelocity
        .WriteSegment_End
    End With
End Sub


