VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Transitions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Transition Module"
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
Public m_booTransitioning As Boolean

Sub FadeOut(Optional ByVal Length As Single = 1, Optional BackColor = 0, Optional WithMusic As Boolean = False)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_dblStart As Double, m_dblNow As Double, m_dblEnd As Double, m_dblLength As Double
Dim m_lngBackColor As Long
Dim m_lngVolume As Long
    If Length = -1 Then Length = 1
    If Length <= 0 Then Exit Sub
    If m_Engine.SpecialFX = False Then
        If WithMusic Then m_Engine.BGMVolume = 0
        Exit Sub
    End If
    If m_booTransitioning Then Exit Sub
    ProfileStart "Transitions.FadeOut"
    m_Engine.FrameBuildup = 0
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.FadeOut"
    m_lngBackColor = CLng(BackColor)
    Call F2SetAlpha(m_lngBackColor, 255)
    If WithMusic Then m_lngVolume = m_Engine.BGMVolume
    m_dblStart = HiResTimer
    m_dblLength = CDbl(Length)
    m_dblEnd = m_dblStart + m_dblLength
    m_dblNow = m_dblStart
    Do Until m_dblNow >= m_dblEnd
        m_dblNow = HiResTimer
        m_sngAlpha = (m_dblNow - m_dblStart) / m_dblLength
        m_Engine.UpdateBackbuffer
        Call F2SetAlpha(m_lngBackColor, m_sngAlpha * 255)
        If WithMusic Then m_Engine.BGMVolume = (1 - m_sngAlpha) * m_lngVolume
        m_Backbuffer.Fill m_Backbuffer.Rectangle, m_lngBackColor, RenderMode_SourceAlpha
        m_Engine.FlipScreen
        m_Engine.Yield
    Loop
    If WithMusic Then m_Engine.BGMVolume = 0
    Call F2SetAlpha(m_lngBackColor, 0)
    m_Backbuffer.Clear m_lngBackColor
    m_Engine.FlipScreen
    m_Engine.Yield
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
    ProfileStop "Transitions.FadeOut"
End Sub

Sub FadeIn(Optional ByVal Length As Single = 1, Optional BackColor = 0, Optional WithMusic As Boolean = False)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_dblStart As Double, m_dblNow As Double, m_dblEnd As Double, m_dblLength As Double
Dim m_lngBackColor As Long
    If Length = -1 Then Length = 1
    If Length <= 0 Then Exit Sub
    If m_Engine.SpecialFX = False Then
        If WithMusic Then m_Engine.BGMVolume = m_Engine.DefaultBGMVolume
        Exit Sub
    End If
    If m_booTransitioning Then Exit Sub
    ProfileStart "Transitions.FadeIn"
    m_Engine.FrameBuildup = 0
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.FadeIn"
    m_lngBackColor = CLng(BackColor)
    Call F2SetAlpha(m_lngBackColor, 0)
    m_Backbuffer.Clear m_lngBackColor
    m_Engine.FlipScreen
    m_Engine.Yield
    m_dblStart = HiResTimer
    m_dblLength = CDbl(Length)
    m_dblEnd = m_dblStart + m_dblLength
    m_dblNow = m_dblStart
    Do Until m_dblNow >= m_dblEnd
        m_dblNow = HiResTimer
        m_sngAlpha = 1 - ((m_dblNow - m_dblStart) / m_dblLength)
        m_Engine.UpdateBackbuffer
        Call F2SetAlpha(m_lngBackColor, m_sngAlpha * 255)
        If WithMusic Then m_Engine.BGMVolume = (1 - m_sngAlpha) * m_Engine.DefaultBGMVolume
        m_Backbuffer.Fill m_Backbuffer.Rectangle, m_lngBackColor, RenderMode_SourceAlpha
        m_Engine.FlipScreen
        m_Engine.Yield
    Loop
    Call F2SetAlpha(m_lngBackColor, 255)
    If WithMusic Then m_Engine.BGMVolume = m_Engine.DefaultBGMVolume
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
    ProfileStop "Transitions.FadeIn"
End Sub

Sub Crossfade(Optional ByVal Length As Single = 1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_dblStart As Double, m_dblNow As Double, m_dblEnd As Double, m_dblLength As Double
Dim m_imgOld As Fury2Image
    If Length = -1 Then Length = 1
    If Length <= 0 Then Exit Sub
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    ProfileStart "Transitions.Crossfade"
    m_Engine.FrameBuildup = 0
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.Crossfade"
    m_dblStart = HiResTimer
    m_dblLength = CDbl(Length)
    m_dblEnd = m_dblStart + m_dblLength
    m_dblNow = m_dblStart
    If OldImage Is Nothing Then
        Set m_imgOld = m_Backbuffer.Duplicate
    Else
        Set m_imgOld = OldImage
    End If
    m_Engine.Sync
    Do Until m_dblNow >= m_dblEnd
        m_dblNow = HiResTimer
        m_sngAlpha = 1 - ((m_dblNow - m_dblStart) / m_dblLength)
        m_Engine.UpdateBackbuffer
        m_Backbuffer.Blit m_Backbuffer.Rectangle, m_imgOld.Rectangle, m_imgOld, m_sngAlpha, BlitMode_Normal
        m_Engine.FlipScreen
        m_Engine.Yield
    Loop
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    ProfileStop "Transitions.Crossfade"
    Err.Clear
End Sub

#If EnableAllTransitions Then
Sub CircleIn(Optional Length = 1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_dblStart As Double, m_dblNow As Double, m_dblEnd As Double, m_dblLength As Double
Dim m_imgBackbuffer As Fury2Image
Dim m_imgMask As Fury2Image
    If Length = -1 Then Length = 1
    If Length <= 0 Then Exit Sub
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_Engine.FrameBuildup = 0
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.CircleIn"
    Err.Clear
    m_dblStart = HiResTimer
    m_dblLength = CDbl(Length)
    m_dblEnd = m_dblStart + m_dblLength
    m_dblNow = m_dblStart
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    Set m_imgMask = F2Image(m_Backbuffer.Width, m_Backbuffer.Height)
    m_Engine.Update
    m_Engine.Redraw
    Do Until m_dblNow >= m_dblEnd
        m_dblNow = HiResTimer
        m_sngAlpha = ((m_dblNow - m_dblStart) / m_dblLength)
        m_Engine.UpdateBackbuffer
        m_imgMask.Copy m_imgBackbuffer
        m_imgMask.DrawCircleFilledFast m_imgMask.Width \ 2, m_imgMask.Height \ 2, (vbMax(m_imgBackbuffer.Width, m_imgBackbuffer.Height) * 0.6) * m_sngAlpha, c_lngNullColor + 1
        m_imgMask.MaskColor = c_lngNullColor + 1
        m_Backbuffer.Blit m_Backbuffer.Rectangle, m_imgBackbuffer.Rectangle, m_imgMask
        m_Engine.FlipScreen
        m_Engine.Yield
    Loop
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SmoothFadeUp(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_imgMask As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.SmoothFadeUp"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 50
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    Set m_imgMask = F2Image(m_Backbuffer.Width, m_Backbuffer.Height)
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_imgMask.Clear F2Black
        m_imgMask.GradientFill F2Rect(0, 0, m_imgMask.Width, m_imgMask.Height, False), F2RGB(255, 255, 255, (m_sngAlpha) * 512), F2RGB(255, 255, 255, (m_sngAlpha - 0.5) * 512), f2gVertical
        m_imgBackbuffer.LoadChannel m_imgMask, f2aAlpha, f2aRed
        m_Backbuffer.Blit m_Backbuffer.Rectangle, m_imgBackbuffer.Rectangle, m_imgBackbuffer, 1, BlitMode_SourceAlpha
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SmoothFadeDown(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_imgMask As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.SmoothFadeDown"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 50
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    Set m_imgMask = F2Image(m_Backbuffer.Width, m_Backbuffer.Height)
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_imgMask.Clear F2Black
        m_imgMask.GradientFill F2Rect(0, 0, m_imgMask.Width, m_imgMask.Height, False), F2RGB(255, 255, 255, (m_sngAlpha - 0.5) * 512), F2RGB(255, 255, 255, (m_sngAlpha) * 512), f2gVertical
        m_imgBackbuffer.LoadChannel m_imgMask, f2aAlpha, f2aRed
        m_Backbuffer.Blit m_Backbuffer.Rectangle, m_imgBackbuffer.Rectangle, m_imgBackbuffer, 1, BlitMode_SourceAlpha
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SmoothFadeLeft(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_imgMask As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.SmoothFadeLeft"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 50
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    Set m_imgMask = F2Image(m_Backbuffer.Width, m_Backbuffer.Height)
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_imgMask.Clear F2Black
        m_imgMask.GradientFill F2Rect(0, 0, m_imgMask.Width, m_imgMask.Height, False), F2RGB(255, 255, 255, (m_sngAlpha) * 512), F2RGB(255, 255, 255, (m_sngAlpha - 0.5) * 512), f2gHorizontal
        m_imgBackbuffer.LoadChannel m_imgMask, f2aAlpha, f2aRed
        m_Backbuffer.Blit m_Backbuffer.Rectangle, m_imgBackbuffer.Rectangle, m_imgBackbuffer, 1, BlitMode_SourceAlpha
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SmoothFadeRight(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_imgMask As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.ContextLevelAdd "Transitions.SmoothFadeRight"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 50
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    Set m_imgMask = F2Image(m_Backbuffer.Width, m_Backbuffer.Height)
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_imgMask.Clear F2Black
        m_imgMask.GradientFill F2Rect(0, 0, m_imgMask.Width, m_imgMask.Height, False), F2RGB(255, 255, 255, (m_sngAlpha - 0.5) * 512), F2RGB(255, 255, 255, (m_sngAlpha) * 512), f2gHorizontal
        m_imgBackbuffer.LoadChannel m_imgMask, f2aAlpha, f2aRed
        m_Backbuffer.Blit m_Backbuffer.Rectangle, m_imgBackbuffer.Rectangle, m_imgBackbuffer, 1, BlitMode_SourceAlpha
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SlideLeft(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.FrameBuildup = 0
    m_Engine.ContextLevelAdd "Transitions.SlideLeft"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 80
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_Backbuffer.Slide (m_sngAlpha * m_Backbuffer.Width), 0
        m_Backbuffer.Blit F2Rect(-(m_Backbuffer.Width - (m_sngAlpha * m_Backbuffer.Width)), 0, m_Backbuffer.Width, m_Backbuffer.Height, False), m_imgBackbuffer.Rectangle, m_imgBackbuffer
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SlideRight(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.FrameBuildup = 0
    m_Engine.ContextLevelAdd "Transitions.SlideRight"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 80
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_Backbuffer.Slide -(m_sngAlpha * m_Backbuffer.Width), 0
        m_Backbuffer.Blit F2Rect((m_Backbuffer.Width - (m_sngAlpha * m_Backbuffer.Width)), 0, m_Backbuffer.Width, m_Backbuffer.Height, False), m_imgBackbuffer.Rectangle, m_imgBackbuffer
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SlideUp(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.FrameBuildup = 0
    m_Engine.ContextLevelAdd "Transitions.SlideUp"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 80
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_Backbuffer.Slide 0, (m_sngAlpha * m_Backbuffer.Height)
        m_Backbuffer.Blit F2Rect(0, -(m_Backbuffer.Height - (m_sngAlpha * m_Backbuffer.Height)), m_Backbuffer.Width, m_Backbuffer.Height, False), m_imgBackbuffer.Rectangle, m_imgBackbuffer
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub

Sub SlideDown(Optional Speed = -1, Optional OldImage = Nothing)
On Error Resume Next
Dim m_lngFade As Long
Dim m_sngAlpha As Single
Dim m_imgBackbuffer As Fury2Image
Dim m_lngSpeed As Long
    If m_Engine.SpecialFX = False Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    m_Engine.FrameBuildup = 0
    m_Engine.ContextLevelAdd "Transitions.SlideDown"
    m_lngSpeed = Speed
    If m_lngSpeed = -1 Then m_lngSpeed = 80
    Err.Clear
    If m_lngSpeed <= 2 Then Exit Sub
    If OldImage Is Nothing Then
        Set m_imgBackbuffer = m_Backbuffer.Duplicate
    Else
        Set m_imgBackbuffer = OldImage
    End If
    m_Engine.Update
    m_Engine.Redraw
    For m_lngFade = 0 To (m_lngSpeed) - 1
        m_sngAlpha = 1 - (CSng(m_lngFade) / CSng(m_lngSpeed))
        m_Engine.UpdateBackbuffer
        m_Backbuffer.Slide 0, -(m_sngAlpha * m_Backbuffer.Height)
        m_Backbuffer.Blit F2Rect(0, (m_Backbuffer.Height - (m_sngAlpha * m_Backbuffer.Height)), m_Backbuffer.Width, m_Backbuffer.Height, False), m_imgBackbuffer.Rectangle, m_imgBackbuffer
        m_Engine.FlipScreen
        m_Engine.Yield
    Next m_lngFade
    m_Engine.UpdateBackbuffer
    m_Engine.FlipScreen
    m_Engine.Yield
    Set m_imgBackbuffer = Nothing
    m_Engine.ContextLevelRemove
    m_booTransitioning = False
    Err.Clear
End Sub
#End If
