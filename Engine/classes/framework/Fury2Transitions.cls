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
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Sub DoFade(ByVal Length As Double, ByVal Mode As Long)
On Error Resume Next
    If Mode = 0 Then
        FadeOut Length / 2
    ElseIf Mode = 1 Then
        FadeIn Length / 2
    End If
End Sub

Sub DoCrossfade(ByVal Length As Double, ByVal Mode As Long)
On Error Resume Next
Static s_imgOldFrame As Fury2Image
    If Mode = 0 Then
        Set s_imgOldFrame = Engine.GrabScreen(False)
    ElseIf Mode = 1 Then
        Crossfade Length, s_imgOldFrame
        Set s_imgOldFrame = Nothing
    End If
End Sub

Sub DoWipe(ByVal Length As Double, ByVal Mode As Long, ByVal Direction As String)
On Error Resume Next
Static s_imgOldFrame As Fury2Image
    If Mode = 0 Then
        Set s_imgOldFrame = Engine.GrabScreen(False)
    ElseIf Mode = 1 Then
        Wipe Direction, Length, False, s_imgOldFrame
        Set s_imgOldFrame = Nothing
    End If
End Sub

Sub DoSmoothWipe(ByVal Length As Double, ByVal Mode As Long, ByVal Direction As String, Optional ByVal Size = 1 / 20)
On Error Resume Next
Static s_imgOldFrame As Fury2Image
    If Mode = 0 Then
        Set s_imgOldFrame = Engine.GrabScreen(False)
    ElseIf Mode = 1 Then
        SmoothWipe Direction, Length, CSng(Size), s_imgOldFrame
        Set s_imgOldFrame = Nothing
    End If
End Sub

Sub DoSlide(ByVal Length As Double, ByVal Mode As Long, ByVal Direction As String)
On Error Resume Next
Static s_imgOldFrame As Fury2Image
    If Mode = 0 Then
        Set s_imgOldFrame = Engine.GrabScreen(False)
    ElseIf Mode = 1 Then
        Slide Direction, Length, False, s_imgOldFrame
        Set s_imgOldFrame = Nothing
    End If
End Sub

Sub Wipe(ByVal Direction As String, Optional ByVal Length As Double = 1, Optional ByVal Reverse As Boolean = False, Optional ByVal OldImage As Fury2Image = Nothing)
Attribute Wipe.VB_Description = "Performs a wipe transition. Direction is a string specifying one of four directions (up, right, down, left)"
On Error Resume Next
Dim m_imgOld As Fury2Image
    If OldImage Is Nothing Then
        Set m_imgOld = Engine.GrabScreen(False)
    Else
        Set m_imgOld = OldImage
    End If
    Select Case LCase(Trim(Direction))
    Case "left"
        Transition Engine.BindEvent("WipeHorizontal_Render", Me, Array(m_imgOld, -1#)), Length, Reverse
    Case "right"
        Transition Engine.BindEvent("WipeHorizontal_Render", Me, Array(m_imgOld, 1#)), Length, Reverse
    Case "up"
        Transition Engine.BindEvent("WipeVertical_Render", Me, Array(m_imgOld, -1#)), Length, Reverse
    Case "down"
        Transition Engine.BindEvent("WipeVertical_Render", Me, Array(m_imgOld, 1#)), Length, Reverse
    Case Else
    End Select
End Sub

Sub Slide(ByVal Direction As String, Optional ByVal Length As Double = 1, Optional ByVal Reverse As Boolean = False, Optional ByVal OldImage As Fury2Image = Nothing)
Attribute Slide.VB_Description = "Performs a slide transition. Direction is a string specifying one of four directions (up, right, down, left)"
On Error Resume Next
Dim m_imgOld As Fury2Image
    If OldImage Is Nothing Then
        Set m_imgOld = Engine.GrabScreen(False)
    Else
        Set m_imgOld = OldImage
    End If
    Select Case LCase(Trim(Direction))
    Case "left"
        Transition Engine.BindEvent("Slide_Render", Me, Array(m_imgOld, -1#, 0#)), Length, Reverse
    Case "right"
        Transition Engine.BindEvent("Slide_Render", Me, Array(m_imgOld, 1#, 0#)), Length, Reverse
    Case "up"
        Transition Engine.BindEvent("Slide_Render", Me, Array(m_imgOld, 0#, -1#)), Length, Reverse
    Case "down"
        Transition Engine.BindEvent("Slide_Render", Me, Array(m_imgOld, 0#, 1#)), Length, Reverse
    Case Else
    End Select
End Sub

Sub SmoothWipe(ByVal Direction As String, Optional ByVal Length As Double = 1, Optional ByVal Size As Single = 1 / 20, Optional ByVal OldImage As Fury2Image = Nothing)
Attribute SmoothWipe.VB_Description = "Performs a smooth wipe transition. Direction is a string specifying one of four directions (up, right, down, left)"
On Error Resume Next
Dim m_imgOld As Fury2Image
Dim l_imgMask As Fury2Image
    If Size > 0.5 Then Size = 0.5
    If Size < 0.01 Then Size = 0.01
    If OldImage Is Nothing Then
        Set m_imgOld = Engine.GrabScreen(False)
    Else
        Set m_imgOld = OldImage
    End If
    Select Case LCase(Trim(Direction))
    Case "left"
        Set l_imgMask = F2Image(Engine.ScreenWidth * Size, Engine.ScreenHeight)
        l_imgMask.GradientFill l_imgMask.Rectangle, Array(F2RGB(255, 255, 255, 255), F2RGB(255, 255, 255, 0), "H")
        Transition Engine.BindEvent("SmoothWipeHorizontal_Render", Me, Array(m_imgOld, l_imgMask, -1#)), Length, False
    Case "right"
        Set l_imgMask = F2Image(Engine.ScreenWidth * Size, Engine.ScreenHeight)
        l_imgMask.GradientFill l_imgMask.Rectangle, Array(F2RGB(255, 255, 255, 0), F2RGB(255, 255, 255, 255), "H")
        Transition Engine.BindEvent("SmoothWipeHorizontal_Render", Me, Array(m_imgOld, l_imgMask, 1#)), Length, False
    Case "up"
        Set l_imgMask = F2Image(Engine.ScreenWidth, Engine.ScreenHeight * Size)
        l_imgMask.GradientFill l_imgMask.Rectangle, Array(F2RGB(255, 255, 255, 255), F2RGB(255, 255, 255, 0), "V")
        Transition Engine.BindEvent("SmoothWipeVertical_Render", Me, Array(m_imgOld, l_imgMask, -1#)), Length, False
    Case "down"
        Set l_imgMask = F2Image(Engine.ScreenWidth, Engine.ScreenHeight * Size)
        l_imgMask.GradientFill l_imgMask.Rectangle, Array(F2RGB(255, 255, 255, 0), F2RGB(255, 255, 255, 255), "V")
        Transition Engine.BindEvent("SmoothWipeVertical_Render", Me, Array(m_imgOld, l_imgMask, 1#)), Length, False
    Case Else
    End Select
End Sub

Sub FadeOut(Optional ByVal Length As Double = 1, Optional ByVal BackColor As Long = 0)
Attribute FadeOut.VB_Description = "Performs a fade-out transition."
On Error Resume Next
    Transition Engine.BindEvent("Fade_Render", Me, Array(BackColor)), Length, False
End Sub

Sub FadeIn(Optional ByVal Length As Double = 1, Optional ByVal BackColor As Long = 0)
Attribute FadeIn.VB_Description = "Performs a fade-in transition."
On Error Resume Next
    Transition Engine.BindEvent("Fade_Render", Me, Array(BackColor)), Length, True
End Sub

Sub Crossfade(Optional ByVal Length As Double = 1, Optional ByVal OldImage As Fury2Image = Nothing)
Attribute Crossfade.VB_Description = "Performs a crossfade transition."
On Error Resume Next
Dim m_imgOld As Fury2Image
    If OldImage Is Nothing Then
        Set m_imgOld = Engine.GrabScreen(False)
    Else
        Set m_imgOld = OldImage
    End If
    Transition Engine.BindEvent("Crossfade_Render", Me, Array(m_imgOld)), Length, True
End Sub

Sub Transition(ByVal Proc As Fury2Event, Optional ByVal Length As Double = 1, Optional ByVal Reverse As Boolean)
Attribute Transition.VB_Description = "Performs a generic transition for Length seconds, calling Proc to render it."
On Error Resume Next
Dim m_sldTransition As Fury2Slider
    If Length = -1 Then Length = 1
    If Length <= 0 Then Exit Sub
    If m_booTransitioning Then Exit Sub
    m_booTransitioning = True
    Engine.ContextLevelAdd "Transition"
    If Reverse Then
        Proc.Invoke Engine.Backbuffer, 1
    Else
        Proc.Invoke Engine.Backbuffer, 0
    End If
    Set m_sldTransition = Engine.F2Slider()
    m_sldTransition.Start Length
    Do Until m_sldTransition.Finished
        Engine.UpdateBackbuffer
        If Reverse Then
            Proc.Invoke Engine.Backbuffer, 1 - m_sldTransition.Progress
        Else
            Proc.Invoke Engine.Backbuffer, m_sldTransition.Progress
        End If
        Engine.FlipScreen
        Engine.Yield True
    Loop
    Engine.UpdateBackbuffer
    If Reverse Then
        Proc.Invoke Engine.Backbuffer, 0
    Else
        Proc.Invoke Engine.Backbuffer, 1
    End If
    Engine.FlipScreen
    Engine.Yield True
    Engine.ContextLevelRemove
    Engine.ResetTiming
    m_booTransitioning = False
    Err.Clear
End Sub

Sub Fade_Render(ByVal Surface As Fury2Image, ByVal Progress As Double, ByVal BackColor As Long)
Attribute Fade_Render.VB_Description = "The renderer for fade transitions."
On Error Resume Next
    Surface.Fill Surface.Rectangle, SetAlpha(BackColor, Progress * 255), RenderMode_SourceAlpha
End Sub

Sub Crossfade_Render(ByVal Surface As Fury2Image, ByVal Progress As Double, ByVal OldImage As Fury2Image)
Attribute Crossfade_Render.VB_Description = "The renderer for crossfade transitions."
On Error Resume Next
    Surface.Blit , , OldImage, Progress, BlitMode_Normal
End Sub

Sub WipeHorizontal_Render(ByVal Surface As Fury2Image, ByVal Progress As Double, ByVal OldImage As Fury2Image, ByVal Direction As Single)
Attribute WipeHorizontal_Render.VB_Description = "The renderer for horizontal wipe transitions."
On Error Resume Next
Dim l_sngX As Single
Dim l_rctRect As Fury2Rect
    l_sngX = Surface.Width * IIf(Direction < 0, 1 - Progress, Progress)
    If Direction < 0 Then
        Set l_rctRect = F2Rect(0, 0, l_sngX, Surface.Height)
    Else
        Set l_rctRect = F2Rect(l_sngX, 0, Surface.Width, Surface.Height)
    End If
    Surface.Blit l_rctRect, l_rctRect, OldImage, 1, BlitMode_Normal
End Sub

Sub WipeVertical_Render(ByVal Surface As Fury2Image, ByVal Progress As Double, ByVal OldImage As Fury2Image, ByVal Direction As Single)
Attribute WipeVertical_Render.VB_Description = "The renderer for vertical wipe transitions."
On Error Resume Next
Dim l_sngY As Single
Dim l_rctRect As Fury2Rect
    l_sngY = Surface.Height * IIf(Direction < 0, 1 - Progress, Progress)
    If Direction < 0 Then
        Set l_rctRect = F2Rect(0, 0, Surface.Width, l_sngY)
    Else
        Set l_rctRect = F2Rect(0, l_sngY, Surface.Width, Surface.Height)
    End If
    Surface.Blit l_rctRect, l_rctRect, OldImage, 1, BlitMode_Normal
End Sub

Sub SmoothWipeHorizontal_Render(ByVal Surface As Fury2Image, ByVal Progress As Double, ByVal OldImage As Fury2Image, ByVal Mask As Fury2Image, ByVal Direction As Single)
Attribute SmoothWipeHorizontal_Render.VB_Description = "The renderer for horizontal smooth wipe transitions."
On Error Resume Next
Dim l_sngX As Single
Dim l_rctRectMask As Fury2Rect, l_rctRect As Fury2Rect
    l_sngX = (Surface.Width) * IIf(Direction < 0, 1 - Progress, Progress)
    If Direction < 0 Then
        Set l_rctRectMask = F2Rect(l_sngX, 0, Mask.Width, Mask.Height, False)
        Set l_rctRect = F2Rect(0, 0, l_sngX, Surface.Height)
    Else
        Set l_rctRectMask = F2Rect(l_sngX, 0, Mask.Width, Mask.Height, False)
        Set l_rctRect = F2Rect(l_sngX + Mask.Width, 0, Surface.Width, Surface.Height)
    End If
    Surface.MaskBlit l_rctRectMask, l_rctRectMask, Mask.Rectangle, OldImage, Mask, 1, BlitMode_Normal
    Surface.Blit l_rctRect, l_rctRect, OldImage, 1, BlitMode_Normal
End Sub

Sub SmoothWipeVertical_Render(ByVal Surface As Fury2Image, ByVal Progress As Double, ByVal OldImage As Fury2Image, ByVal Mask As Fury2Image, ByVal Direction As Single)
Attribute SmoothWipeVertical_Render.VB_Description = "The renderer for vertical smooth wipe transitions."
On Error Resume Next
Dim l_sngY As Single
Dim l_rctRectMask As Fury2Rect, l_rctRect As Fury2Rect
    l_sngY = (Surface.Height) * IIf(Direction < 0, 1 - Progress, Progress)
    If Direction < 0 Then
        Set l_rctRectMask = F2Rect(0, l_sngY, Mask.Width, Mask.Height, False)
        Set l_rctRect = F2Rect(0, 0, Surface.Width, l_sngY)
    Else
        Set l_rctRectMask = F2Rect(0, l_sngY, Mask.Width, Mask.Height, False)
        Set l_rctRect = F2Rect(0, l_sngY + Mask.Height, Surface.Width, Surface.Height)
    End If
    Surface.MaskBlit l_rctRectMask, l_rctRectMask, Mask.Rectangle, OldImage, Mask, 1, BlitMode_Normal
    Surface.Blit l_rctRect, l_rctRect, OldImage, 1, BlitMode_Normal
End Sub

Sub Slide_Render(ByVal Surface As Fury2Image, ByVal Progress As Double, ByVal OldImage As Fury2Image, ByVal XDirection As Single, ByVal YDirection As Single)
Attribute Slide_Render.VB_Description = "The renderer for slide transitions."
On Error Resume Next
Dim l_sngX As Single, l_sngY As Single
Dim l_rctRect As Fury2Rect
    l_sngX = XDirection * Progress * Surface.Width
    l_sngY = YDirection * Progress * Surface.Height
    Set l_rctRect = F2Rect(l_sngX, l_sngY, Surface.Width, Surface.Height, False)
    Surface.Blit l_rctRect, , OldImage, 1, BlitMode_Normal
End Sub

