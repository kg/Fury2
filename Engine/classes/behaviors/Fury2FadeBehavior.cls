VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2FadeBehavior"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² 'Fade' Object Behavior"
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
Implements Fury2Behavior
Implements Fury2Object
Implements IVirtualFileSaveable
Public Engine As Fury2Engine
Public Obj As Object
Public FadeSpeed As Single
Public EndAlpha As Single
Public EndEvent As String
Public EndScript As String

Private Sub Fury2Behavior_Attach(Obj As Object)
    Behavior_Attach Me, Obj
End Sub

Private Sub Fury2Behavior_Detach()
    Behavior_Detach Me
End Sub

Private Property Set Fury2Behavior_Engine(ByVal RHS As Fury2Engine)
    Set Engine = RHS
End Property

Private Property Get Fury2Behavior_Engine() As Fury2Engine
    Set Fury2Behavior_Engine = Engine
End Property

Private Property Get Fury2Behavior_O() As Object
    Set Fury2Behavior_O = Me
End Property

Private Property Set Fury2Behavior_Obj(RHS As Object)
    Set Obj = RHS
End Property

Private Property Get Fury2Behavior_Obj() As Object
    Set Fury2Behavior_Obj = Obj
End Property

Private Sub Fury2Behavior_Update()
On Error Resume Next
    If FadeSpeed = 0 Then Exit Sub
    If Obj Is Nothing Then Exit Sub
    With Obj
        If .Alpha < EndAlpha Then
            .Alpha = .Alpha + Abs(FadeSpeed)
            If .Alpha >= EndAlpha Then
                .Alpha = EndAlpha
                FadeSpeed = 0
                If EndEvent <> "" Then
                    Set m_Engine.IObjThis = Obj
                    m_Engine.SetIObjThis
                    m_Engine.ScriptEngine.AddCode EndEvent
                End If
            End If
        Else
            .Alpha = .Alpha - Abs(FadeSpeed)
            If .Alpha <= EndAlpha Then
                .Alpha = EndAlpha
                FadeSpeed = 0
                If EndEvent <> "" Then
                    Set m_Engine.IObjThis = Obj
                    m_Engine.SetIObjThis
                    m_Engine.ScriptEngine.AddCode EndEvent
                End If
            End If
        End If
    End With
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save FadeSpeed
        .Save EndAlpha
        .Save EndEvent
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
    With File
        .Load FadeSpeed
        .Load EndAlpha
        .Load EndEvent
    End With
End Sub
