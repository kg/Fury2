VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2TextElement"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Generic Onscreen Text Plugin"
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
Implements Fury2Object
Implements Fury2Plugin
Implements IVirtualFileSaveable
Public Engine As Fury2Engine
Private m_strText As String
Public Font As Fury2Font
Attribute Font.VB_VarDescription = "The font to draw text with."
Public Alpha As Single
Attribute Alpha.VB_VarDescription = "The opacity of the text."
Public ShadowColor As Long
Attribute ShadowColor.VB_VarDescription = "The color to draw the text's shadow with."
Public FadeSpeed As Double
Attribute FadeSpeed.VB_VarDescription = "The speed at which to fade the text."
Public Rectangle As Fury2Rect
Attribute Rectangle.VB_VarDescription = "The rectangle in which to draw the text."
Public Color As Long
Attribute Color.VB_VarDescription = "The color with which to draw the text."
Public Realtime As Boolean
Attribute Realtime.VB_VarDescription = "If True, any expressions in the text will be reparsed every frame."
Public WrapMode As Long
Attribute WrapMode.VB_VarDescription = "Determines the wrapping mode of the text."
Public Center As Boolean
Attribute Center.VB_VarDescription = "If True, text will be centered within the rectangle."

Public Property Get DrawBehind() As Boolean
Attribute DrawBehind.VB_Description = "If True, the text will be drawn behind picture slots."
    DrawBehind = False
End Property

Public Property Let Text(NewText As String)
Attribute Text.VB_Description = "The text to draw."
On Error Resume Next
    m_strText = NewText
    If Realtime Then
    Else
        If InStr(m_strText, "<%=") Then
            m_strText = Engine.ParseString(m_strText)
        End If
    End If
End Property

Public Property Get Text() As String
On Error Resume Next
    Text = m_strText
End Property

Public Sub Redraw()
On Error Resume Next
Dim m_strText2 As String, m_rctText As Fury2Rect
Dim m_lngXOffset As Long, m_lngYOffset As Long
    If Realtime Then
        If InStr(m_strText, "<%=") Then
            Err.Clear
            m_strText2 = Engine.ParseString(m_strText)
            If Err <> 0 Then
                Realtime = False
                Err.Clear
                m_strText = m_strText2
            End If
        Else
            m_strText2 = m_strText
        End If
    Else
        m_strText2 = m_strText
    End If
    Set m_rctText = Rectangle.Copy
    With Engine.Backbuffer
        If Center Then
            m_lngXOffset = (Rectangle.Width - Font.TextWidth(m_strText2)) \ 2
            m_lngYOffset = (Rectangle.Height - Font.TextHeight(m_strText2)) \ 2
            Set m_rctText = F2Rect(Rectangle.left + m_lngXOffset, Rectangle.top + m_lngYOffset, Rectangle.right, Rectangle.bottom, True)
        End If
        Font.Draw Engine.Backbuffer, m_strText2, m_rctText
    End With
End Sub

Sub Update()
    If FadeSpeed > 0 Then
        Alpha = Alpha + FadeSpeed
        If Alpha > 1 Then Alpha = 1: FadeSpeed = 0
        If Alpha < 0 Then Alpha = 0
    ElseIf FadeSpeed < 0 Then
        Alpha = Alpha + FadeSpeed
        If Alpha > 1 Then Alpha = 1
        If Alpha < 0 Then Alpha = 0: FadeSpeed = 0: Engine.UnHookEvents Me
    End If
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Alpha = 1
    Set Rectangle = New Fury2Rect
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
End Property

Private Property Set Fury2Plugin_Engine(ByVal RHS As Fury2Engine)
    Set Engine = RHS
End Property

Private Property Get Fury2Plugin_Engine() As Fury2Engine
    Set Fury2Plugin_Engine = Engine
End Property

Private Sub Fury2Plugin_ControlActivate(Control As Fury2InputControl)
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Fury2InputControl)
End Sub

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
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Private Sub Fury2Plugin_Redraw()
    Redraw
End Sub

Private Sub Fury2Plugin_Shutdown()
End Sub

Private Sub Fury2Plugin_Startup()
End Sub

Private Sub Fury2Plugin_SyncUpdate()
End Sub

Private Sub Fury2Plugin_Unload()
End Sub

Private Sub Fury2Plugin_Update()
    Update
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


