VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2MessageWindow"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Generic Message Box Plugin"
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
Implements Fury2Plugin
Implements Fury2Object
Implements IVirtualFileSaveable

Public Active As Boolean
Public Alpha As Single
Public AnimationDelay As Long
#If EnableBehaviors = 1 Then
Public Behaviors As Fury2Behaviors
#End If
Public ClickEvent As Fury2Event
Public CloseEvent As Fury2Event
Public DrawBehind As Boolean
Public DisableClose As Boolean
Public Engine As Fury2Engine
Public Enabled As Boolean
Public Rectangle As Fury2Rect
Public TextColor As Long
Public Text As String
Public Font As Fury2Font
Public Image As Object
Public MiddleImage As Object
Public Queued As Boolean
Public ShadowText As Boolean
Public ScrollY As Long
Public ScrollSpeed As Long
Public ScrollEnd As Long
Public TotalHeight As Long
Public WindowSkin As Object
Public WrapMode As Long
Public First As Boolean, Last As Boolean
Private mvarOldDisable As Boolean

Public Sub CloseWindow()
On Error Resume Next
    CloseEvent.Invoke
    Err.Clear
    If DisableClose Then Exit Sub
    If ((Queued = False) Or (Queued And Last)) And Engine.EnableObjectEvents Then Engine.ScriptEngine.Exec "MessageWindow_Destroy", Me
    If Err.Number <> 0 Or ((Queued = True)) Then
        If Me.Active Then Engine.UnHookEvents Me
        If (Queued = True) And (Engine.MessageQueue.Count > 0) Then
            Engine.MessageQueue.Remove 1
            If Engine.MessageQueue.Count > 0 Then
                Engine.HookEvents Engine.MessageQueue(1)
            End If
        End If
    Else
        If Me.Active Then Engine.UnHookEvents Me
    End If
End Sub

Public Sub Startup()
On Error Resume Next
    Me.Active = True
    Engine.DisableMovement = True
    If ((Queued And First) Or (Not Queued)) And Engine.EnableObjectEvents Then Engine.ScriptEngine.Exec "MessageWindow_Create", Me
    Err.Clear
End Sub

Public Sub Unload()
Dim m_lngMWins As Long
    Engine.DisableMovement = False
    Me.Active = False
    If Engine.MessageWindows.Count >= 1 Then
        For m_lngMWins = Engine.MessageWindows.Count To 1 Step -1
            If Engine.MessageWindows(m_lngMWins).Active = False Then
                Engine.MessageWindows.Remove m_lngMWins
            End If
        Next m_lngMWins
    End If
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set Rectangle = New Fury2Rect
#If EnableBehaviors = 1 Then
    Set Behaviors = New Fury2Behaviors
    Behaviors.SetParent Me
#End If
    Alpha = 1
    WrapMode = 2
    Enabled = True
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Fury2InputControl)
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
    Fury2Plugin_DrawBehind = DrawBehind
End Property

Private Property Set Fury2Plugin_Engine(ByVal RHS As Fury2Engine)
    Set Engine = RHS
End Property

Private Property Get Fury2Plugin_Engine() As Fury2Engine
    Set Fury2Plugin_Engine = Engine
End Property

Private Sub Fury2Plugin_Init()
End Sub

Private Sub Fury2Plugin_KeyDown(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_KeyPress(KeyAscii As Long)
End Sub

Private Sub Fury2Plugin_KeyUp(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_MapLoad(Filename As String)
End Sub

Private Sub Fury2Plugin_MouseDown(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If Me.Active Then Else Exit Sub
    If (Engine.Mouse.Visible = False) And (Engine.MouseCursor Is Nothing) Then Exit Sub
    If (X >= Rectangle.Left) And (Y >= Rectangle.Top) And (X < Rectangle.Right) And (Y < Rectangle.Bottom) Then
        X = -32767
        Y = -32767
    End If
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If Me.Active Then Else Exit Sub
    If (Engine.Mouse.Visible = False) And (Engine.MouseCursor Is Nothing) Then Exit Sub
    If (X >= Rectangle.Left) And (Y >= Rectangle.Top) And (X < Rectangle.Right) And (Y < Rectangle.Bottom) Then
        X = -32767
        Y = -32767
    End If
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If Me.Active Then Else Exit Sub
    If Not Enabled Then
        X = -32767
        Y = -32767
        Exit Sub
    End If
    If (Engine.Mouse.Visible = False) And (Engine.MouseCursor Is Nothing) Then Exit Sub
    If (X >= Rectangle.Left) And (Y >= Rectangle.Top) And (X < Rectangle.Right) And (Y < Rectangle.Bottom) Then
        X = -32767
        Y = -32767
        If Button = 2 Then
            CloseWindow
        ElseIf Button = 1 Then
            If (TotalHeight + ScrollY) > (Rectangle.Height - 4) Then
                ScrollEnd = ScrollY - (Rectangle.Height - 4)
                ScrollSpeed = -1
            Else
                ClickEvent.Invoke
                Err.Clear
                If Not DisableClose Then
                    CloseWindow
                End If
            End If
        End If
    End If
End Sub

Private Sub Fury2Plugin_Redraw()
On Error Resume Next
Static Here As Boolean
Dim m_fntOldFont As StdFont
Dim m_rctNewRectangle As Fury2Rect, m_rctText As Fury2Rect
Dim m_lngAlpha As Long, m_lngSpace As Long, m_lngSpace2 As Long
Dim m_strText As String
Dim oImage As Object
    Set m_rctNewRectangle = Me.Rectangle.Copy
    m_strText = Text
    If InStr(m_strText, vbCrLf) Then
    Else
        m_strText = m_strText + vbCrLf
    End If
    If InStr(m_strText, "<%=") Then
        m_strText = Engine.ParseString(m_strText)
    End If
    With Engine.Backbuffer
        .UpdateClipRect
        m_lngSpace = 2
        If WindowSkin.EdgeOverlap Then
            WindowSkin.Draw Engine.Backbuffer, Rectangle, Alpha, SF_Middle
        Else
            WindowSkin.Draw Engine.Backbuffer, Rectangle, Alpha
        End If
        If Image Is Nothing Then
            Set oImage = Nothing
        Else
            Set oImage = Image
        End If
        If (Image Is Nothing) Or (oImage.Width <= 0) Or (oImage.Height <= 0) Then
            Set m_rctText = F2Rect(m_rctNewRectangle.Left + m_lngSpace, m_rctNewRectangle.Top + m_lngSpace, m_rctNewRectangle.Right - m_lngSpace, m_rctNewRectangle.Bottom - m_lngSpace)
            Call Font.Draw(Engine.Backbuffer, m_strText, m_rctText, TextColor, Alpha, Font.Options(, ScrollY), TotalHeight)
        Else
            m_lngSpace2 = m_lngSpace
            m_lngSpace = (m_rctNewRectangle.Height - Image.Height) \ 2
            Set m_rctText = Rectangle.Copy
            m_rctText.Adjust -m_lngSpace2, -m_lngSpace2
            Set .ClipRectangle = m_rctText
            If Image.AlphaChannel Then
                .Blit F2Rect(m_rctNewRectangle.Left + m_lngSpace2, m_rctNewRectangle.Top + m_lngSpace, Image.Width, Image.Height, False), Image.Rectangle, Image, Alpha, BlitMode_SourceAlpha
            Else
                .Blit F2Rect(m_rctNewRectangle.Left + m_lngSpace2, m_rctNewRectangle.Top + m_lngSpace, Image.Width, Image.Height, False), Image.Rectangle, Image, Alpha
            End If
            Set m_rctText = F2Rect(Image.Width + m_rctNewRectangle.Left + m_lngSpace2, m_rctNewRectangle.Top + m_lngSpace2, m_rctNewRectangle.Right - m_lngSpace2, m_rctNewRectangle.Bottom - m_lngSpace2)
            Call Font.Draw(Engine.Backbuffer, m_strText, m_rctText, TextColor, Alpha, Font.Options(, ScrollY), TotalHeight)
            .UpdateClipRect
        End If
        If MiddleImage Is Nothing Then
        Else
            If MiddleImage.AlphaChannel Then
                .Blit F2Rect(Me.Rectangle.Left + ((Me.Rectangle.Width - MiddleImage.Width) \ 2), Me.Rectangle.Top + ((Me.Rectangle.Height - MiddleImage.Height) \ 2), MiddleImage.Width, MiddleImage.Height, False), MiddleImage.Rectangle, MiddleImage, 1, BlitMode_SourceAlpha
            Else
                .Blit F2Rect(Me.Rectangle.Left + ((Me.Rectangle.Width - MiddleImage.Width) \ 2), Me.Rectangle.Top + ((Me.Rectangle.Height - MiddleImage.Height) \ 2), MiddleImage.Width, MiddleImage.Height, False), MiddleImage.Rectangle, MiddleImage
            End If
        End If
        If WindowSkin.EdgeOverlap Then
            WindowSkin.Draw Engine.Backbuffer, Rectangle, Alpha, SF_All Xor SF_Middle
        End If
    End With
End Sub

Private Sub Fury2Plugin_Shutdown()
End Sub

Private Sub Fury2Plugin_Startup()
    Startup
End Sub

Private Sub Fury2Plugin_SyncUpdate()
End Sub

Private Sub Fury2Plugin_ControlActivate(Control As Fury2InputControl)
On Error Resume Next
    If Me.Active Then Else Exit Sub
    If Enabled Then
    Else
        Control.Consume
        Exit Sub
    End If
    If (Control.Name = "Cancel") Or (Control.Name = "Close") Or (Control.Name = "Secondary") Then
        CloseWindow
    ElseIf (Control.Name = "OK") Or (Control.Name = "Select") Or (Control.Name = "Primary") Then
        If (TotalHeight + ScrollY) > (Rectangle.Height) Then
            ScrollEnd = ScrollY - (Rectangle.Height)
            ScrollSpeed = -1
        Else
            ClickEvent.Invoke
            Err.Clear
            If Not DisableClose Then
                CloseWindow
            End If
        End If
    End If
    Control.Consume
    Control.Reset
End Sub

Private Sub Fury2Plugin_Unload()
    Unload
End Sub

Private Sub Fury2Plugin_Update()
On Error Resume Next
#If EnableBehaviors = 1 Then
    If Engine.EnableObjectEvents Then
    Else
        Behaviors.Reset
    End If
    Behaviors.SetParent Me
    Behaviors.Update
#End If
    ScrollY = ScrollY + ScrollSpeed
    If ScrollSpeed < 0 Then
        If ScrollY <= ScrollEnd Then ScrollSpeed = 0
    ElseIf ScrollSpeed > 1 Then
        If ScrollY >= ScrollEnd Then ScrollSpeed = 0
    End If
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
        .WriteSegment_End
    End With
End Sub

Private Sub Fury2Plugin_OutputPluginChanged()
End Sub

