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
Attribute Name.VB_VarDescription = "The name of this graphic."
Public Images As Fury2Collection
Attribute Images.VB_VarDescription = "The images in this graphic."
Public XCenter As Single, YCenter As Single
Attribute XCenter.VB_VarDescription = "The X centerpoint of this graphic."
Attribute YCenter.VB_VarDescription = "The Y centerpoint of this graphic."
Public LoopMode As Fury2LoopModes
Attribute LoopMode.VB_VarDescription = "Determines the graphic's looping behavior."
Public BlitMode As SFXBlitModes
Attribute BlitMode.VB_VarDescription = "The blit mode used when drawing this graphic."
Public Alpha As Single
Attribute Alpha.VB_VarDescription = "The opacity of this graphic."
Public FadeSpeed As Double
Attribute FadeSpeed.VB_VarDescription = "The speed at which this graphic should fade."
Public AnimationDelay As Single
Attribute AnimationDelay.VB_VarDescription = "The number of seconds between animation frames."
Public AnimationFrame As Long
Attribute AnimationFrame.VB_VarDescription = "The current animation frame."
Public AnimationDirection As Long
Attribute AnimationDirection.VB_VarDescription = "The direction in which animation will go."
Public OnFinished As Fury2Event
Private m_booAnimate As Boolean
Private m_parParam As AnimatedGraphicParam
Private m_lngImages() As Long
Private m_sngLastFrame As Double
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
    Set Images = Engine.F2Collection()
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Friend Function GetParam() As AnimatedGraphicParam
On Error Resume Next
    GetParam = m_parParam
End Function

Public Function Duplicate() As Fury2Graphic
Attribute Duplicate.VB_Description = "Creates a copy of this graphic."
On Error Resume Next
    Set Duplicate = New Fury2Graphic
    With Duplicate
        Duplicate.SetEngine Engine
        Set .Images = Images.Duplicate()
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
Attribute CurrentImage.VB_Description = "The current frame of the graphic."
On Error Resume Next
    AnimationFrame = WrapValue(AnimationFrame, 1, Images.Count)
    Set CurrentImage = Images(AnimationFrame)
    Err.Clear
End Property

Public Property Get Width() As Long
Attribute Width.VB_Description = "The width of the graphic."
On Error Resume Next
    Width = Images(AnimationFrame).Width
End Property

Public Property Get Height() As Long
Attribute Height.VB_Description = "The height of the graphic."
On Error Resume Next
    Height = Images(AnimationFrame).Height
End Property

Function Show(ByVal X As Single, ByVal Y As Single, Optional ByVal Alpha As Single = 1, Optional ByVal Slot As Long = -1) As Long
Attribute Show.VB_Description = "Shows the graphic in a given picture slot at a given location."
On Error Resume Next
    If Slot = -1 Then Slot = Engine.ActivePictureSlots
    Engine.ShowPicture Slot, Images(AnimationFrame), Floor(X - XCenter), Floor(Y - YCenter), , , , , BlitMode, Alpha
    Show = Slot
End Function

Sub Draw(ByVal Surface As Fury2Image, ByVal X As Single, ByVal Y As Single, Optional ByVal Alpha As Single = 1, Optional ByVal ScalePercentage As Single = 1, Optional ByVal RotationAngle As Single = 0)
Attribute Draw.VB_Description = "Draws the graphic to an image at a given location."
On Error Resume Next
Dim l_imgFrame As Fury2Image
Dim l_dblFrameLength As Double
Dim l_dblElapsed As Double
Dim l_dblNow As Double
    If Images.Count = 0 Then Exit Sub
    If Surface Is Nothing Then Exit Sub
    l_dblNow = Engine.HiTimer
    If m_sngLastFrame <= 0 Then m_sngLastFrame = l_dblNow
    If Images.Count > 1 Then
        l_dblFrameLength = ((1 / Engine.DesiredFramerate) * AnimationDelay)
        l_dblElapsed = (l_dblNow - m_sngLastFrame)
        l_dblElapsed = l_dblElapsed / l_dblFrameLength
        If (l_dblElapsed >= 1) Then
            Alpha = Alpha + (FadeSpeed * l_dblElapsed)
            If Alpha > 1 Then Alpha = 1
            If Alpha < 0 Then Alpha = 0
            m_sngLastFrame = m_sngLastFrame + (Floor(l_dblElapsed) * l_dblFrameLength)
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
                AnimationFrame = AnimationFrame + (AnimationDirection * Floor(l_dblElapsed))
                If (AnimationFrame > Images.Count) Or (AnimationFrame < 1) Then
                    If Not (OnFinished Is Nothing) Then OnFinished.Invoke
                End If
                AnimationFrame = ClipValue(AnimationFrame, 1, Images.Count)
            End Select
            Refresh
        End If
    End If
    AnimationFrame = WrapValue(AnimationFrame, 1, Images.Count)
    Set l_imgFrame = Images(AnimationFrame)
    If l_imgFrame Is Nothing Then Exit Sub
    With l_imgFrame
        .Draw Surface, X + XCenter, Y + YCenter, Alpha * Me.Alpha, ScalePercentage, RotationAngle, BlitMode, , IIf(m_engEngine.SmoothScaling, ResampleMode_Bilinear, ResampleMode_Linear)
'        Surface.Blit F2Rect(Floor(X - XCenter), Floor(Y - YCenter), .Width, .Height, False), .Rectangle, l_imgFrame, Alpha * Me.Alpha, BlitMode
    End With
End Sub

Sub Play(Optional ByVal OnFinish As Engine.Fury2Event = Nothing)
On Error Resume Next
    AnimationFrame = 1
    AnimationDirection = 1
    Animate = True
    LoopMode = F2LM_OneShot
    Set OnFinished = OnFinish
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

Public Property Get Animate() As Boolean
On Error Resume Next
    Animate = m_booAnimate
End Property

Public Property Let Animate(ByVal NewValue As Boolean)
On Error Resume Next
    m_booAnimate = NewValue
    m_sngLastFrame = 0
End Property

Private Sub Class_Terminate()
On Error Resume Next
    If Engine Is Nothing Then
    Else
        Engine.GraphicDestroyed Me
    End If
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngImages As Long, l_lngCount As Long
Dim l_imgImage As Fury2Image
    With File
        .ReadSegment_Begin
            .Load l_lngCount
            If l_lngCount > 0 Then
                For l_lngImages = 1 To l_lngCount
                    Set l_imgImage = New Fury2Image
                    .Load l_imgImage
                    Images.Add l_imgImage
                Next l_lngImages
            End If
        .ReadSegment_End
        .Load Alpha
        .Load AnimationDelay
        .Load BlitMode
        .Load XCenter
        .Load YCenter
        .Load Name
        .Load LoopMode
    End With
    Err.Clear
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngImages As Long
    With File
        .WriteSegment_Begin
            .Save CLng(Images.Count)
            For l_lngImages = 1 To Images.Count
                .Save Images(l_lngImages)
            Next l_lngImages
        .WriteSegment_End
        .Save Alpha
        .Save AnimationDelay
        .Save BlitMode
        .Save XCenter
        .Save YCenter
        .Save Name
        .Save CLng(LoopMode)
    End With
    Err.Clear
End Sub

Function Load(ByVal Filename) As Boolean
On Error Resume Next
    Load = Engine.LoadFromFile(Me, Engine.Filesystem.File(Filename))
End Function

Function Save(ByVal Filename) As Boolean
On Error Resume Next
    Save = Engine.SaveToFile(Me, Engine.Filesystem.CreateFile(Filename))
End Function

