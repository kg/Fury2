VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Blocking"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Blocking Data Object"
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
Implements IInspectorType

Private mvarWidth As Long
Private mvarHeight As Long

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Let Width(NewWidth As Long)
    mvarWidth = NewWidth
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Public Property Let Height(NewHeight As Long)
    mvarHeight = NewHeight
End Property

Public Sub SetValues(Width, Height)
On Error Resume Next
    mvarWidth = Width
    mvarHeight = Height
End Sub

Private Sub IInspectorType_FromString(ByVal Data As String)
On Error Resume Next
Dim l_varParts As Variant
    l_varParts = Split(Data, "x")
    mvarWidth = CLng(l_varParts(0))
    mvarHeight = CLng(l_varParts(1))
End Sub

Private Function IInspectorType_ToString() As Variant
    IInspectorType_ToString = mvarWidth & "x" & mvarHeight
End Function

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
            .Load mvarWidth
            .Load mvarHeight
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarWidth
            .Save mvarHeight
        .WriteSegment_End
    End With
End Sub

