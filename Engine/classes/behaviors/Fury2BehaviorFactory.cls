VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2BehaviorFactory"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Behavior Manager"
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
Implements IVirtualFileSaveable
Private mvarType As Long
Public TrueObject As Fury2Behavior

Public Sub Instantiate()
On Error Resume Next
    Select Case mvarType
    Case F2BH_None
        Set TrueObject = New Fury2Behavior
    Case F2BH_Fade
        Set TrueObject = New Fury2FadeBehavior
    Case F2BH_Translate
        Set TrueObject = New Fury2TranslateBehavior
    Case F2BH_Script
        Set TrueObject = New Fury2ScriptBehavior
    Case Else
    End Select
    Set TrueObject.Engine = m_Engine
End Sub

Public Sub UpdateType()
On Error Resume Next
    If TypeOf TrueObject Is Fury2BehaviorFactory Then
        ' WTF@USER
    ElseIf TypeOf TrueObject Is Fury2FadeBehavior Then
        mvarType = Fury2BehaviorTypes.F2BH_Fade
    ElseIf TypeOf TrueObject Is Fury2TranslateBehavior Then
        mvarType = Fury2BehaviorTypes.F2BH_Translate
    ElseIf TypeOf TrueObject Is Fury2ScriptBehavior Then
        mvarType = Fury2BehaviorTypes.F2BH_Script
    Else
        mvarType = Fury2BehaviorTypes.F2BH_None
    End If
End Sub

Public Property Get BehaviorType() As Fury2BehaviorTypes
On Error Resume Next
    BehaviorType = mvarType
End Property

Public Property Let BehaviorType(NewType As Fury2BehaviorTypes)
On Error Resume Next
    mvarType = NewType
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
        .Load mvarType
        Instantiate
        .Load TrueObject
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    UpdateType
    With File
        .WriteSegment_Begin
        .Save mvarType
        .Save TrueObject
        .WriteSegment_End
    End With
End Sub
