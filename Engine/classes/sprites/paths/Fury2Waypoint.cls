VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Waypoint"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Path Waypoint Object"
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

Implements Fury2Object
Implements IVirtualFileSaveable
Implements IInspectable
Private mvarX As Single
Private mvarY As Single
Private mvarWait As Long
Public ReachedEvent As String
Public FailEvent As String
Public WaitRemainder As Long

Public Function Duplicate() As Fury2Waypoint
On Error Resume Next
Dim l_wpNew As Fury2Waypoint
    Set l_wpNew = New Fury2Waypoint
    l_wpNew.X = X
    l_wpNew.Y = Y
    l_wpNew.Wait = Wait
    l_wpNew.WaitRemainder = WaitRemainder
    l_wpNew.ReachedEvent = ReachedEvent
    l_wpNew.FailEvent = FailEvent
End Function

Public Property Let Wait(ByVal vData As Long)
On Error Resume Next
    mvarWait = vData
    WaitRemainder = vData
End Property

Public Property Get Wait() As Long
    Wait = mvarWait
End Property

Public Property Let Y(ByVal vData As Single)
On Error Resume Next
    mvarY = vData
End Property

Public Property Get Y() As Single
On Error Resume Next
    Y = mvarY
End Property

Public Property Let X(ByVal vData As Single)
On Error Resume Next
    mvarX = vData
End Property

Public Property Get X() As Single
On Error Resume Next
    X = mvarX
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
            .Load mvarX
            .Load mvarY
            .Load mvarWait
            WaitRemainder = mvarWait
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarX
            .Save mvarY
            .Save mvarWait
        .WriteSegment_End
    End With
End Sub

