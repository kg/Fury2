VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Graphic"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    Engine (Fury� Game Creation System Runtime Engine)
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
Implements IVirtualFileSaveable

Public Name As String
Public ImageFilenames As Fury2Collection
Public Images As Fury2Collection
Public XCenter As Single, YCenter As Single
Public LoopMode As Fury2LoopModes
Public BlitMode As Long
Public Alpha As Single
Public FadeSpeed As Single
Public AnimationDelay As Long
Public AnimationFrame As Long
Private m_parParam As AnimatedGraphicParam
Private m_lngImages() As Long
Private AnimationDelayBuildup As Long

Public Property Get Handle() As Long
On Error Resume Next
    Handle = VarPtr(m_parParam)
End Property

Public Sub Refresh()
On Error Resume Next
Dim l_lngImages As Long
    ReDim m_lngImages(0 To Images.Count - 1)
    For l_lngImages = LBound(m_lngImages) To UBound(m_lngImages)
        m_lngImages(l_lngImages) = CLng(Images(l_lngImages + 1).Handle)
    Next l_lngImages
    With m_parParam
        .FrameCount = Images.Count
        .Frame = AnimationFrame
        .BlitMode = BlitMode
        .Alpha = ClipByte(Alpha * 255)
        .XCenter = XCenter
        .YCenter = YCenter
        .pFrames = VarPtr(m_lngImages(0))
    End With
End Sub

Public Property Get CurrentImage() As Fury2Image
On Error Resume Next
    AnimationFrame = WrapValue(AnimationFrame, 1, Images.Count)
    Set CurrentImage = Images(AnimationFrame)
    Err.Clear
End Property

Public Property Get Width() As Long
On Error Resume Next
    Width = Images(AnimationFrame).Width
End Property

Public Property Get Height() As Long
On Error Resume Next
    Height = Images(AnimationFrame).Height
End Property

Function Show(ByVal X, ByVal Y, Optional ByVal Alpha = 1, Optional ByVal Slot = -1) As Long
On Error Resume Next
    If Slot = -1 Then Slot = m_Engine.ActivePictureSlots
    m_Engine.ShowPicture Slot, Images(AnimationFrame), Floor(CSng(X) - XCenter), Floor(CSng(Y) - YCenter), , , , , BlitMode, Alpha
    Show = Slot
End Function

Sub Draw(ByVal Surface, ByVal X, ByVal Y, Optional ByVal Alpha = 1)
On Error Resume Next
Dim l_imgSurface As Fury2Image
Dim l_imgFrame As Fury2Image
    If Images.Count = 0 Then Exit Sub
    Set l_imgSurface = Surface
    If l_imgSurface Is Nothing Then Exit Sub
    AnimationFrame = WrapValue(AnimationFrame, 1, Images.Count)
    Set l_imgFrame = Images(AnimationFrame)
    If l_imgFrame Is Nothing Then Exit Sub
    With l_imgFrame
        l_imgSurface.Blit F2Rect(Floor(CSng(X) - XCenter), Floor(CSng(Y) - YCenter), .Width, .Height, False), .Rectangle, l_imgFrame, Alpha * Me.Alpha, BlitMode
    End With
End Sub

Sub Update()
On Error Resume Next
    If Images.Count > 1 Then
        AnimationDelayBuildup = AnimationDelayBuildup + 1
        If AnimationDelayBuildup >= AnimationDelay Then
            AnimationDelayBuildup = AnimationDelayBuildup - AnimationDelay
            AnimationFrame = WrapValue(AnimationFrame + 1, 1, Images.Count)
        End If
    End If
    Alpha = Alpha + FadeSpeed
    If Alpha > 1 Then Alpha = 1
    If Alpha < 0 Then Alpha = 0
    Refresh
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set Images = New Fury2Collection
    Set ImageFilenames = New Fury2Collection
    BlitMode = BlitMode_SourceAlpha
    Alpha = 1
    AnimationDelay = 5
    AnimationFrame = 1
    Err.Clear
End Sub

Public Sub ReloadImages()
On Error Resume Next
Dim l_lngImage As Long, l_imgFile As Fury2Image
    Set Images = New Fury2Collection
    If ImageFilenames.Count > 0 Then
        For l_lngImage = 1 To ImageFilenames.Count
            Set l_imgFile = Nothing
            Set l_imgFile = m_Engine.LoadImage(CStr(ImageFilenames(l_lngImage)))
            Images.Add l_imgFile
            Err.Clear
        Next
    End If
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngFilenames As Long, l_lngCount As Long
Dim l_strFilename As String, l_imgFile As Fury2Image
    With File
        .WriteSegment_Begin
            .Load l_lngCount
            Set ImageFilenames = New Fury2Collection
            If l_lngCount > 0 Then
                For l_lngFilenames = 1 To l_lngCount
                    l_strFilename = ""
                    .Load l_strFilename
                    ImageFilenames.Add l_strFilename
                Next l_lngFilenames
                ReloadImages
            End If
        .WriteSegment_End
        .Load Alpha
        .Load AnimationDelay
        .Load BlitMode
        .Load XCenter
        .Load YCenter
        .Load Name
    End With
    Err.Clear
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngFilenames As Long
    With File
        .WriteSegment_Begin
            .Save CLng(ImageFilenames.Count)
            For l_lngFilenames = 1 To ImageFilenames.Count
                .Save CStr(ImageFilenames(l_lngFilenames))
            Next l_lngFilenames
        .WriteSegment_End
        .Save Alpha
        .Save AnimationDelay
        .Save BlitMode
        .Save XCenter
        .Save YCenter
        .Save Name
    End With
    Err.Clear
End Sub