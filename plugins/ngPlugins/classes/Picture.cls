VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Picture"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
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

Implements IVirtualFileSaveable
Implements IInspectable

Private m_strFilename As String
Public Image As Fury2Image
Attribute Image.VB_VarDescription = "~"
Public Rectangle As Fury2Rect
Attribute Rectangle.VB_VarDescription = "~"
Public SourceRectangle As Fury2Rect
Attribute SourceRectangle.VB_VarDescription = "~"
Public BlitMode As SFXBlitModes
Attribute BlitMode.VB_VarDescription = "The image's rendering effect."
Public ResampleMode As SFXResampleModes
Attribute ResampleMode.VB_VarDescription = "The image's resampling mode."
Public Color As Long
Attribute Color.VB_VarDescription = "The image's tint color.{Color}"
Public Opacity As Single
Attribute Opacity.VB_VarDescription = "The image's opacity."

Private Sub Class_Initialize()
On Error Resume Next
    BlitMode = BlitMode_Normal
    ResampleMode = ResampleMode_Linear
    Opacity = 1
    Color = 0
End Sub

Public Sub Draw(ByRef Surface As Fury2Image)
On Error Resume Next
Dim l_rctDest As Fury2Rect, l_rctSource As Fury2Rect
    If Rectangle Is Nothing Then
        Set l_rctDest = Image.Rectangle
    Else
        Set l_rctDest = Rectangle
    End If
    If SourceRectangle Is Nothing Then
        Set l_rctSource = Image.Rectangle
    Else
        Set l_rctSource = SourceRectangle
    End If
    Surface.Blit l_rctDest, l_rctSource, Image, Opacity, BlitMode, Color, ResampleMode
End Sub

Friend Sub ReloadImage()
On Error Resume Next
    Set Image = DefaultEngine.LoadImage(m_strFilename, True)
End Sub

Public Property Get Filename() As String
Attribute Filename.VB_Description = "The path to the image.{ImagePath}"
On Error Resume Next
    Filename = m_strFilename
End Property

Public Property Let Filename(ByRef NewFilename As String)
On Error Resume Next
    m_strFilename = NewFilename
    ReloadImage
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngValue As Long
    Set Rectangle = New Fury2Rect
    Set SourceRectangle = New Fury2Rect
    With File
        .ReadSegment_Begin
            .Load m_strFilename
            .ReadSegment_Begin
                .Load Rectangle
            .ReadSegment_End
            .ReadSegment_Begin
                .Load SourceRectangle
            .ReadSegment_End
            .Load l_lngValue
            BlitMode = l_lngValue
            .Load l_lngValue
            ResampleMode = l_lngValue
            .Load Color
            .Load Opacity
        .ReadSegment_End
    End With
    ReloadImage
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save m_strFilename
            .WriteSegment_Begin
                .Save Rectangle
            .WriteSegment_End
            .WriteSegment_Begin
                .Save SourceRectangle
            .WriteSegment_End
            .Save CLng(BlitMode)
            .Save CLng(ResampleMode)
            .Save Color
            .Save Opacity
        .WriteSegment_End
    End With
End Sub
