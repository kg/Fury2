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
Attribute VB_Creatable = False
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

Option Explicit
Implements IVirtualFileSaveable

Public Name As String
Public ImageFilenames As Fury2Collection
Public Images As Fury2Collection
Public XCenter As Single, YCenter As Single
Public LoopMode As Fury2LoopModes
Public BlitMode As SFXBlitModes
Public Alpha As Single
Public FadeSpeed As Double
Public AnimationDelay As Single
Public AnimationFrame As Long
Public AnimationDirection As Long
Private m_parParam As AnimatedGraphicParam
Private m_lngImages() As Long
Private m_sngLastFrame As Double
Private m_engEngine As Fury2Engine

Friend Function GetParam() As AnimatedGraphicParam
On Error Resume Next
    GetParam = m_parParam
End Function

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
    Set Images = Engine.F2Collection()
    Set ImageFilenames = Engine.F2Collection()
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Function Duplicate() As Fury2Graphic
On Error Resume Next
    Set Duplicate = New Fury2Graphic
    With Duplicate
        Duplicate.SetEngine Engine
        Set .Images = Images.Duplicate()
        Set .ImageFilenames = ImageFilenames.Duplicate()
        .Name = Name
        .XCenter = XCenter
        .YCenter = YCenter
        .LoopMode = LoopMode
        .BlitMode = BlitMode
        .Alpha = Alpha
        .FadeSpeed = FadeSpeed
        .AnimationDelay = AnimationDelay
        .AnimationFrame = AnimationFrame
        .Refresh
    End With
End Function

Public Property Get Handle() As Long
On Error Resume Next
    Handle = VarPtr(m_parParam)
End Property

Public Sub Refresh()
On Error Resume Next
Dim l_lngImages As Long
    ReDim m_lngImages(0 To Images.Count - 1)
    If Images.Count > 0 Then
        For l_lngImages = LBound(m_lngImages) To UBound(m_lngImages)
            m_lngImages(l_lngImages) = CLng(Images(l_lngImages + 1).Handle)
        Next l_lngImages
    End If
    With m_parParam
        .FrameCount = Images.Count
        .Frame = AnimationFrame
        .BlitMode = BlitMode
        .MatteColor = Images(AnimationFrame).MatteColor
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

Function Show(ByVal X As Single, ByVal Y As Single, Optional ByVal Alpha As Single = 1, Optional ByVal Slot As Long = -1) As Long
On Error Resume Next
    If Slot = -1 Then Slot = Engine.ActivePictureSlots
    Engine.ShowPicture Slot, Images(AnimationFrame), Floor(X - XCenter), Floor(Y - YCenter), , , , , BlitMode, Alpha
    Show = Slot
End Function

Sub Draw(ByVal Surface As Fury2Image, ByVal X As Single, ByVal Y As Single, Optional ByVal Alpha As Single = 1)
On Error Resume Next
Dim l_imgFrame As Fury2Image
    If Images.Count = 0 Then Exit Sub
    If Surface Is Nothing Then Exit Sub
    AnimationFrame = WrapValue(AnimationFrame, 1, Images.Count)
    Set l_imgFrame = Images(AnimationFrame)
    If l_imgFrame Is Nothing Then Exit Sub
    With l_imgFrame
        Surface.Blit F2Rect(Floor(X - XCenter), Floor(Y - YCenter), .Width, .Height, False), .Rectangle, l_imgFrame, Alpha * Me.Alpha, BlitMode
    End With
End Sub

Sub Update()
On Error Resume Next
Dim l_dblElapsed As Double
    If Images.Count > 1 Then
        l_dblElapsed = (Engine.HiTimer - m_sngLastFrame) / ((1 / Engine.DesiredFramerate) * AnimationDelay)
        If (l_dblElapsed >= 1) Then
            m_sngLastFrame = Engine.HiTimer - ((l_dblElapsed - Floor(l_dblElapsed)) * (1 / Engine.DesiredFramerate) * AnimationDelay)
            Select Case LoopMode
            Case F2LM_Restart
                AnimationFrame = WrapValue(AnimationFrame + (AnimationDirection * Floor(l_dblElapsed)), 1, Images.Count)
            Case F2LM_PingPong
                AnimationFrame = AnimationFrame + (AnimationDirection * Floor(l_dblElapsed))
                If (AnimationFrame < 1) Then
                    AnimationFrame = 2
                    AnimationDirection = 1
                ElseIf (AnimationFrame > Images.Count) Then
                    AnimationFrame = Images.Count - 1
                    AnimationDirection = -1
                End If
            Case F2LM_OneShot
                AnimationFrame = ClipValue(AnimationFrame + (AnimationDirection * Floor(l_dblElapsed)), 1, Images.Count)
            End Select
        End If
    End If
    Alpha = Alpha + FadeSpeed
    If Alpha > 1 Then Alpha = 1
    If Alpha < 0 Then Alpha = 0
    Refresh
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    BlitMode = BlitMode_SourceAlpha
    Alpha = 1
    AnimationDelay = 5
    AnimationFrame = 1
    AnimationDirection = 1
    Err.Clear
End Sub

Public Sub ReloadImages()
On Error Resume Next
Dim l_lngImage As Long, l_imgFile As Fury2Image
    Set Images = Engine.F2Collection()
    If ImageFilenames.Count > 0 Then
        For l_lngImage = 1 To ImageFilenames.Count
            Set l_imgFile = Nothing
            Set l_imgFile = Engine.LoadImage(CStr(ImageFilenames(l_lngImage)))
            Images.Add l_imgFile
            Err.Clear
        Next
    End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    If Engine Is Nothing Then
    Else
        Engine.GraphicDestroyed Me
    End If
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngFilenames As Long, l_lngCount As Long
Dim l_strFilename As String, l_imgFile As Fury2Image
    With File
        .WriteSegment_Begin
            .Load l_lngCount
            Set ImageFilenames = Engine.F2Collection()
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
