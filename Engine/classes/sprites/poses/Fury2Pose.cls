VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Pose"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
'Private mvarParent As Fury2Sprite
Private mvarParent As WeakReference
Private mvarLoopMode As Fury2LoopModes
Private mvarZLeft As Single
Private mvarZRight As Single
Private mvarFrames As Fury2PoseFrames
Private mvarBlocking As Fury2Blocking
Private mvarName As String
Private mvarIndex As Long

Friend Sub SetIndex(ByVal Index As Long)
    mvarIndex = Index
End Sub

Public Property Get Index() As Long
Attribute Index.VB_Description = "The index of this state."
    Index = mvarIndex
End Property

Public Property Get Parent() As Fury2Sprite
Attribute Parent.VB_Description = "The sprite this pose belongs to."
On Error Resume Next
    Set Parent = WRGetSprite(mvarParent)
End Property

Public Property Get EndScript() As String
Attribute EndScript.VB_Description = "*"
Attribute EndScript.VB_MemberFlags = "40"
On Error Resume Next
    If mvarFrames.Count < 1 Then Exit Property
    EndScript = mvarFrames(mvarFrames.Count).Script
End Property

Public Property Let EndScript(RHS As String)
On Error Resume Next
    If mvarFrames.Count < 1 Then Exit Property
    mvarFrames(mvarFrames.Count).Script = RHS
End Property

Sub Free(ByVal Object As Fury2Sprite)
Attribute Free.VB_MemberFlags = "40"
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame
    WRFree mvarParent
    If mvarFrames Is Nothing Then Exit Sub
    mvarFrames.SetParent Nothing
    If mvarFrames.Count > 0 Then
        For Each l_fraFrame In mvarFrames
            If l_fraFrame Is Nothing Then
            Else
                l_fraFrame.Free
            End If
        Next l_fraFrame
    End If
End Sub

Public Property Get LoopMode() As Fury2LoopModes
Attribute LoopMode.VB_Description = "The looping mode for the pose. Valid poses are One-shot, Normal, and Bi-directional (aka Ping-Pong)."
    LoopMode = mvarLoopMode
End Property

Public Property Let LoopMode(ByVal NewMode As Fury2LoopModes)
    mvarLoopMode = NewMode
End Property

Public Sub ModifyGraphics(ByVal FunctionName As String, Optional ByRef Parameters As Variant)
On Error Resume Next
    mvarFrames.ModifyGraphics FunctionName, Parameters
End Sub

Sub LoadGraphics()
Attribute LoadGraphics.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngFrames As Long
    mvarFrames.LoadGraphics
End Sub

Sub RunEndScript()
Attribute RunEndScript.VB_Description = "Runs the pose's end script."
On Error Resume Next
    mvarFrames(mvarFrames.Count).RunScript
End Sub

Sub Play()
Attribute Play.VB_Description = "Sets the sprite's current pose to this pose and plays it from the beginning."
On Error Resume Next
    With WRGetSprite(mvarParent)
        .SetPose Me
        .Animate = True
        .Frame = 1
        .AnimationDirection = 1
    End With
    Err.Clear
End Sub

Sub PlayBackwards()
Attribute PlayBackwards.VB_Description = "Sets the sprite's current pose to this pose and plays it from the end, backwards."
On Error Resume Next
    With WRGetSprite(mvarParent)
        .SetPose Me
        .Animate = True
        .Frame = mvarFrames.Count
        .AnimationDirection = -1
    End With
    Err.Clear
End Sub

Friend Sub SetParent(Parent As Fury2Sprite)
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame
    WRSet mvarParent, Parent
    If mvarFrames.Count = 0 Then Exit Sub
    For Each l_fraFrame In mvarFrames
        l_fraFrame.SetParent Me
    Next l_fraFrame
End Sub

Public Property Set Frames(NewFrames As Fury2PoseFrames)
Attribute Frames.VB_Description = "~The pose's animation frames. Frame #1 is always the stop frame, following frames are animation frames."
    Set mvarFrames = NewFrames
End Property

Public Property Get Frames() As Fury2PoseFrames
    Set Frames = mvarFrames
End Property

Public Function Duplicate() As Fury2Pose
Attribute Duplicate.VB_Description = "Creates a copy of the pose."
On Error Resume Next
    Set Duplicate = New Fury2Pose
    With Duplicate
        .Name = mvarName
        .Blocking.SetValues mvarBlocking.Width, mvarBlocking.Height
        Set .Blocking.Polygon = mvarBlocking.Polygon.Duplicate()
        .LoopMode = mvarLoopMode
        .ZLeft = mvarZLeft
        .ZRight = mvarZRight
        Set .Frames = mvarFrames.Duplicate
    End With
End Function

Public Property Get Blocking() As Fury2Blocking
Attribute Blocking.VB_Description = "~The collision data for the pose."
    Set Blocking = mvarBlocking
End Property

Public Property Set Blocking(vData As Fury2Blocking)
    Set mvarBlocking = vData
End Property

Public Property Get BlockingPolygon() As Fury2Polygon
Attribute BlockingPolygon.VB_Description = "The blocking polygon for the pose."
On Error Resume Next
    Set BlockingPolygon = mvarBlocking.Polygon
End Property

Public Property Let ZLeft(ByVal vData As Single)
Attribute ZLeft.VB_Description = "The sorting value for the left side of the sprite."
    mvarZLeft = vData
End Property

Public Property Get ZLeft() As Single
    ZLeft = mvarZLeft
End Property

Public Property Let ZRight(ByVal vData As Single)
Attribute ZRight.VB_Description = "The sorting value for the right side of the sprite."
    mvarZRight = vData
End Property

Public Property Get ZRight() As Single
    ZRight = mvarZRight
End Property

Public Property Get Name() As String
Attribute Name.VB_Description = "The Pose's name."
    Name = mvarName
End Property

Public Property Let Name(NewName As String)
On Error Resume Next
    mvarName = NewName
    If WRIsNull(mvarParent) Then Exit Property
    LoadGraphics
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set mvarBlocking = New Fury2Blocking
    Set mvarFrames = New Fury2PoseFrames
    mvarFrames.SetParent Me
    mvarName = "New Pose"
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set mvarFrames = Nothing
    Set mvarBlocking = Nothing
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_strTemp As String
    With File
        .ReadSegment_Begin
            .Load mvarName
            .ReadSegment_Begin
                .Skip 8
                .Load mvarLoopMode
            .ReadSegment_End
            .Load mvarBlocking
            Set mvarFrames = New Fury2PoseFrames
            mvarFrames.SetParent Me
            .Load mvarFrames
            .Load l_strTemp
            If Len(l_strTemp) > 0 Then
                mvarFrames(mvarFrames.Count).Script = l_strTemp
            End If
            .Load mvarZLeft
            .Load mvarZRight
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarName
            .WriteSegment_Begin
                .Save CSng(0)
                .Save CSng(0)
                .Save mvarLoopMode
            .WriteSegment_End
            .Save mvarBlocking
            .Save mvarFrames
            .Save ""
            .Save mvarZLeft
            .Save mvarZRight
        .WriteSegment_End
    End With
End Sub


