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
Attribute VB_Description = "Fury� Generic Message Box Plugin"
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
Implements Fury2Plugin
Implements Fury2Object
Implements IVirtualFileSaveable

Public Active As Boolean
Attribute Active.VB_VarDescription = "If True, the message window will respond to input. If False, the message window is queued and waiting to be opened."
Public Alpha As Single
Attribute Alpha.VB_VarDescription = "The opacity of the window."
Public AnimationDelay As Long
Public ClickEvent As Fury2Event
Attribute ClickEvent.VB_VarDescription = "This event will be invoked when the user clicks the window or presses the activate button."
Public CloseEvent As Fury2Event
Attribute CloseEvent.VB_VarDescription = "This event will be invoked when the window is closed."
Public DrawBehind As Boolean
Attribute DrawBehind.VB_VarDescription = "If True, the window will draw behind picture slots."
Public DisableClose As Boolean
Attribute DisableClose.VB_VarDescription = "If True, the user will be unable to close the window with input."
Public Engine As Fury2Engine
Public Enabled As Boolean
Attribute Enabled.VB_VarDescription = "If True, the window will respond to and consume input."
Public Rectangle As Fury2Rect
Attribute Rectangle.VB_VarDescription = "The rectangle occupied by the window."
Public TextColor As Long
Attribute TextColor.VB_VarDescription = "The color of the window's text."
Public Text As String
Attribute Text.VB_VarDescription = "The text to display in the window. Text that does not fit will scroll vertically and be wrapped."
Public Font As Fury2Font
Attribute Font.VB_VarDescription = "The font to draw text with."
Public Image As Fury2Image
Attribute Image.VB_VarDescription = "An image to display on the left side of the window."
Public MiddleImage As Fury2Image
Attribute MiddleImage.VB_VarDescription = "An image to display in the center of the window."
Public Queued As Boolean
Attribute Queued.VB_VarDescription = "If True, this message window is queued and waiting to be shown."
Public ScrollY As Long
Attribute ScrollY.VB_VarDescription = "The vertical scroll position of the message's text."
Public ScrollSpeed As Long
Attribute ScrollSpeed.VB_VarDescription = "The speed at which scrolling is occurring."
Public ScrollEnd As Long
Attribute ScrollEnd.VB_VarDescription = "The point at which scrolling will stop."
Public TotalHeight As Long
Attribute TotalHeight.VB_VarDescription = "The total height of the text in the window."
Public WindowSkin As Fury2WindowSkin
Attribute WindowSkin.VB_VarDescription = "The windowskin to draw the window with."
Public WrapMode As Long
Attribute WrapMode.VB_VarDescription = "Determines how text will be wrapped."
Public First As Boolean, Last As Boolean
Attribute First.VB_VarDescription = "If True, the window is the first in the queue"
Attribute Last.VB_VarDescription = "If True, the window is the last in the queue"
Public TabStops As Variant
Private mvarOldDisable As Boolean

Public Sub CloseWindow()
Attribute CloseWindow.VB_Description = "Closes the window."
On Error Resume Next
    If Not (CloseEvent Is Nothing) Then CloseEvent.Invoke
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
    TabStops = vbEmpty
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
    If (X >= Rectangle.left) And (Y >= Rectangle.top) And (X < Rectangle.right) And (Y < Rectangle.bottom) Then
        X = -32767
        Y = -32767
    End If
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If Me.Active Then Else Exit Sub
    If (Engine.Mouse.Visible = False) And (Engine.MouseCursor Is Nothing) Then Exit Sub
    If (X >= Rectangle.left) And (Y >= Rectangle.top) And (X < Rectangle.right) And (Y < Rectangle.bottom) Then
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
    If (X >= Rectangle.left) And (Y >= Rectangle.top) And (X < Rectangle.right) And (Y < Rectangle.bottom) Then
        X = -32767
        Y = -32767
        If Button = 2 Then
            CloseWindow
        ElseIf Button = 1 Then
            If (TotalHeight + ScrollY) > (Rectangle.Height - 4) Then
                ScrollEnd = ScrollY - (Rectangle.Height - 4)
                ScrollSpeed = -1
            Else
                If Not (ClickEvent Is Nothing) Then ClickEvent.Invoke
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
Dim l_varStops As Variant
Dim oImage As Object
    If (TabStops <> vbEmpty) Then
        l_varStops = Font.TabStops
        Font.TabStops = TabStops
    End If
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
            Set m_rctText = F2Rect(m_rctNewRectangle.left + m_lngSpace, m_rctNewRectangle.top + m_lngSpace, m_rctNewRectangle.right - m_lngSpace, m_rctNewRectangle.bottom - m_lngSpace)
            Call Font.Draw(Engine.Backbuffer, m_strText, m_rctText, TextColor, Alpha, Font.Options(, ScrollY), TotalHeight)
        Else
            m_lngSpace2 = m_lngSpace
            m_lngSpace = (m_rctNewRectangle.Height - Image.Height) \ 2
            Set m_rctText = Rectangle.Copy
            m_rctText.Adjust -m_lngSpace2, -m_lngSpace2
            Set .ClipRectangle = m_rctText
            If Image.AlphaChannel Then
                .Blit F2Rect(m_rctNewRectangle.left + m_lngSpace2, m_rctNewRectangle.top + m_lngSpace, Image.Width, Image.Height, False), Image.Rectangle, Image, Alpha, BlitMode_SourceAlpha
            Else
                .Blit F2Rect(m_rctNewRectangle.left + m_lngSpace2, m_rctNewRectangle.top + m_lngSpace, Image.Width, Image.Height, False), Image.Rectangle, Image, Alpha
            End If
            Set m_rctText = F2Rect(Image.Width + m_rctNewRectangle.left + m_lngSpace2, m_rctNewRectangle.top + m_lngSpace2, m_rctNewRectangle.right - m_lngSpace2, m_rctNewRectangle.bottom - m_lngSpace2)
            Call Font.Draw(Engine.Backbuffer, m_strText, m_rctText, TextColor, Alpha, Font.Options(, ScrollY), TotalHeight)
            .UpdateClipRect
        End If
        If MiddleImage Is Nothing Then
        Else
            If MiddleImage.AlphaChannel Then
                .Blit F2Rect(Me.Rectangle.left + ((Me.Rectangle.Width - MiddleImage.Width) \ 2), Me.Rectangle.top + ((Me.Rectangle.Height - MiddleImage.Height) \ 2), MiddleImage.Width, MiddleImage.Height, False), MiddleImage.Rectangle, MiddleImage, 1, BlitMode_SourceAlpha
            Else
                .Blit F2Rect(Me.Rectangle.left + ((Me.Rectangle.Width - MiddleImage.Width) \ 2), Me.Rectangle.top + ((Me.Rectangle.Height - MiddleImage.Height) \ 2), MiddleImage.Width, MiddleImage.Height, False), MiddleImage.Rectangle, MiddleImage
            End If
        End If
        If WindowSkin.EdgeOverlap Then
            WindowSkin.Draw Engine.Backbuffer, Rectangle, Alpha, SF_All Xor SF_Middle
        End If
    End With
    If (TabStops <> vbEmpty) Then
        Font.TabStops = l_varStops
    End If
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
        Control.Consume
        CloseWindow
    ElseIf (Control.Name = "OK") Or (Control.Name = "Select") Or (Control.Name = "Primary") Then
        Control.Consume
        If (TotalHeight + ScrollY) > (Rectangle.Height) Then
            ScrollEnd = ScrollY - (Rectangle.Height)
            ScrollSpeed = -1
        Else
            If Not (ClickEvent Is Nothing) Then ClickEvent.Invoke
            Err.Clear
            If Not DisableClose Then
                CloseWindow
            End If
        End If
    End If
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



