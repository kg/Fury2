VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Mouse"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Mouse Object"
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

Implements Fury2Object
Private m_lngXDelta As Long, m_lngYDelta As Long
Private m_lngLastX As Long, m_lngLastY As Long
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Sub Update()
On Error Resume Next
    m_lngXDelta = g_lngMouseX - m_lngLastX
    m_lngYDelta = g_lngMouseY - m_lngLastY
    m_lngLastX = g_lngMouseX
    m_lngLastY = g_lngMouseY
End Sub

Public Property Get XDelta() As Long
    XDelta = m_lngXDelta
End Property

Public Property Get YDelta() As Long
    YDelta = m_lngYDelta
End Property

Public Property Get Cursor() As Fury2Image
    Set Cursor = Engine.MouseCursor
End Property

Public Property Set Cursor(RHS As Fury2Image)
    Set Engine.MouseCursor = RHS
End Property

Public Property Let Visible(ByVal vData As Boolean)
    If (m_booIDE) Or (Engine.EditMode) Or (Not (Engine.Debugger Is Nothing)) Then
        If m_booMouseVisible <> True Then
            ShowCursor True
            m_booMouseVisible = True
        End If
    Else
        If vData <> m_booMouseVisible Then
            ShowCursor vData
'            m_objOutputPlugin.Form.MousePointer = IIf(vData, 0, 99)
            m_booMouseVisible = vData
        End If
    End If
End Property

Public Property Get Visible() As Boolean
    If (m_booIDE) Or (Engine.EditMode) Or (Not (Engine.Debugger Is Nothing)) Then
        Visible = True
    Else
        Visible = m_booMouseVisible
    End If
End Property

Public Property Let Y(ByVal vData As Single)
On Error Resume Next
    Engine.DisableMoveQueue = True
    Engine.SetMouseLocation g_lngMouseX, vData
    g_lngMouseY = vData
    Engine.DisableMoveQueue = False
End Property

Public Property Get Y() As Single
    Y = g_lngMouseY
End Property

Public Property Get Buttons() As Long
    Buttons = g_lngMouseButtons
End Property

Public Property Let X(ByVal vData As Single)
On Error Resume Next
    Engine.DisableMoveQueue = True
    Engine.SetMouseLocation vData, g_lngMouseY
    g_lngMouseX = vData
    Engine.DisableMoveQueue = False
End Property

Public Property Get X() As Single
    X = g_lngMouseX
End Property

Sub Move(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    If ((X = g_lngMouseX) And (Y = g_lngMouseY)) Then Exit Sub
    Engine.DisableMoveQueue = True
    Engine.SetMouseLocation CLng(X), CLng(Y)
    Engine.DisableMoveQueue = False
    g_lngMouseX = X
    g_lngMouseY = Y
    m_lngLastX = g_lngMouseX
    m_lngLastY = g_lngMouseY
    m_lngXDelta = 0
    m_lngYDelta = 0
End Sub
