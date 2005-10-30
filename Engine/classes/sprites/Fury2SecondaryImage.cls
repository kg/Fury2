VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SecondaryImage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
Implements IInspectable
Implements Fury2Object
Implements IVirtualFileSaveable

Private mvarFilename As String
Private mvarImage As Fury2Image
Private mvarParent As Fury2PoseFrame
Private mvarType As SecondaryImageTypes
Private mvarIndex As Long

Friend Sub SetIndex(ByVal index As Long)
On Error Resume Next
    mvarIndex = index
End Sub

Public Property Get index() As Long
    index = mvarIndex
End Property

Public Property Get Parent() As Fury2PoseFrame
    Set Parent = mvarParent
End Property

Friend Sub SetParent(ByRef Parent As Fury2PoseFrame)
On Error Resume Next
    Set mvarParent = Parent
End Sub

Public Sub LoadGraphics()
On Error Resume Next
Dim l_varVariables As Variant
Dim l_strFilename As String
    l_varVariables = Array(Array("i", mvarIndex), Array("index", mvarIndex), _
        Array("f", mvarParent.index), Array("frame", mvarParent.index), _
        Array("p", mvarParent.Parent.index), Array("pose", mvarParent.Parent.index), _
        Array("pn", mvarParent.Parent.Name), Array("posename", mvarParent.Parent.Name), _
        Array("n", mvarParent.Parent.Parent.Name), Array("name", mvarParent.Parent.Parent.Name))
    l_strFilename = mvarParent.Parent.Parent.Engine.ParseString(mvarFilename, l_varVariables)
    If Not mvarImage Is Nothing Then
        If mvarImage.Name <> Filename Then
            Set mvarImage = Nothing
        End If
    End If
    If mvarImage Is Nothing Then
        Set mvarImage = mvarParent.Parent.Parent.Engine.LoadImage(l_strFilename)
    End If
    If mvarImage Is Nothing Then
        mvarParent.Parent.Parent.Engine.CriticalError "Fury2SecondaryImage.LoadGraphics", "Unable to load secondary image: " + l_strFilename
    End If
End Sub

Sub Free()
On Error Resume Next
    Set mvarParent = Nothing
    Set mvarImage = Nothing
End Sub

Public Function Duplicate() As Fury2SecondaryImage
On Error Resume Next
    Set Duplicate = New Fury2SecondaryImage
    With Duplicate
        Set .Image = mvarImage
        .SetFilename mvarFilename
        .ImageType = Me.ImageType
    End With
End Function

Friend Sub SetImage(Img As Fury2Image)
    Set mvarImage = Img
End Sub

Public Property Get ImageType() As SecondaryImageTypes
On Error Resume Next
    ImageType = mvarType
End Property

Public Property Let ImageType(ByVal NewValue As SecondaryImageTypes)
On Error Resume Next
    mvarType = NewValue
End Property

Public Property Set Image(RHS As Fury2Image)
    Set mvarImage = RHS
End Property

Public Property Get Image() As Fury2Image
    Set Image = mvarImage
End Property

Friend Sub SetFilename(ByRef Filename As String)
On Error Resume Next
    mvarFilename = Filename
End Sub

Public Property Let Filename(ByVal vData As String)
On Error Resume Next
    If mvarFilename = vData Then Exit Property
    mvarFilename = vData
    Set mvarImage = Nothing
    LoadGraphics
End Property

Public Property Get Filename() As String
    Filename = mvarFilename
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_bytValue As Byte
    With File
        .ReadSegment_Begin
            .Load mvarFilename
            .Load l_bytValue
            mvarType = l_bytValue
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarFilename
            .Save CByte(mvarType)
        .WriteSegment_End
    End With
End Sub


