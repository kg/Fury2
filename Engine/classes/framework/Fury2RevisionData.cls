VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2RevisionData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² File Version Data Footer"
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

Private mvarRevision As Long
Private mvarVersion As Single
Private mvarCreator As String
Private mvarModifier As String

Public Property Let Modifier(ByVal vData As String)
    mvarModifier = vData
End Property

Public Property Get Modifier() As String
    Modifier = mvarModifier
End Property

Public Property Let Creator(ByVal vData As String)
    mvarCreator = vData
End Property

Public Property Get Creator() As String
    Creator = mvarCreator
End Property

Public Property Let Version(ByVal vData As Single)
    mvarVersion = vData
End Property

Public Property Get Version() As Single
    Version = mvarVersion
End Property

Public Property Let Revision(ByVal vData As Long)
    mvarRevision = vData
End Property

Public Property Get Revision() As Long
    Revision = mvarRevision
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
    With File
        .ReadSegment_Begin
            .Load mvarVersion
            .Load mvarRevision
            .Load mvarCreator
            .Load mvarModifier
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
    With File
        .WriteSegment_Begin
            .Save mvarVersion
            .Save mvarRevision
            .Save mvarCreator
            .Save mvarModifier
        .WriteSegment_End
    End With
End Sub
