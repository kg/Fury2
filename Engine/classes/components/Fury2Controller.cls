VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Controller"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Keyboard Controller Plugin"
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
Implements Fury2Plugin
Implements IVirtualFileSaveable
Public Engine As Fury2Engine
Public Obj As Object
Public Camera As Object
Public ScrollSpeed As Long
Public MoveCamera As Boolean
Public CameraXOffset As Long, CameraYOffset As Long
Public Enabled As Boolean
Public Style As Long
Public JumpVelocity As Single
Public TurnSpeed As Single

Private InputState(0 To 3) As Boolean

Public LeftBinding As String, UpBinding As String, RightBinding As String, DownBinding As String, ActivateBinding As String

Public Sub Update()
On Error Resume Next
    If Obj Is Nothing Then Exit Sub
    If TypeOf Obj Is Fury2Sprite Then
        If (MoveCamera = True) And (Not (Camera Is Nothing)) Then
            Camera.CenterOn CLng(Obj.X), CLng(Obj.YCenter - Obj.Z)
        End If
        Obj.AnimateWhileMoving = True
        If Enabled Then
            With Engine
            If Obj.Path.Count <= 0 Then
                    If .DisableMovement Then
                        Obj.Velocity = Obj.Velocity - .VelocityDecay
                        If Obj.Velocity <= 0.05 Then
                            Obj.Velocity = 0
                        Else
                        End If
                    Else
                        Select Case Style
                        Case CS_Normal
                            If InputState(3) = False And InputState(0) = False And _
                            InputState(1) = True And InputState(2) = False Then
                                Obj.Angle = 90 ' Right
                            ElseIf InputState(3) = False And InputState(0) = False And _
                            InputState(1) = True And InputState(2) = True Then
                                Obj.Angle = 135 ' Down-Right
                            ElseIf InputState(3) = False And InputState(0) = False And _
                            InputState(1) = False And InputState(2) = True Then
                                Obj.Angle = 180 ' Down
                            ElseIf InputState(3) = True And InputState(0) = False And _
                            InputState(1) = False And InputState(2) = True Then
                                Obj.Angle = 225 ' Down-Left
                            ElseIf InputState(3) = True And InputState(0) = False And _
                            InputState(1) = False And InputState(2) = False Then
                                Obj.Angle = 270 ' Left
                            ElseIf InputState(3) = True And InputState(0) = True And _
                            InputState(1) = False And InputState(2) = False Then
                                Obj.Angle = 315 ' Up-Left
                            ElseIf InputState(3) = False And InputState(0) = True And _
                            InputState(1) = False And InputState(2) = False Then
                                Obj.Angle = 0 ' Up
                            ElseIf InputState(3) = False And InputState(0) = True And _
                            InputState(1) = True And InputState(2) = False Then
                                Obj.Angle = 45 ' Up-Right
                            End If
                            If InputState(3) Or InputState(1) Or InputState(0) Or InputState(2) Then
                                If (Obj.State = Obj.IdleState) Then Obj.State = Obj.WalkState
                                Obj.Velocity = Obj.Velocity + .VelocityDecay
                                If Obj.Velocity > Obj.WalkSpeed Then Obj.Velocity = Obj.WalkSpeed
                            Else
                                Obj.Velocity = Obj.Velocity - .VelocityDecay
                                If Obj.Velocity < .VelocityDecay Then
                                    If (Obj.State = Obj.WalkState) Then Obj.State = Obj.IdleState
                                    Obj.Velocity = 0
                                End If
                            End If
                        Case CS_ResidentEvil
                            If InputState(3) And InputState(1) Then
                            ElseIf InputState(3) Then
                                Obj.Angle = Obj.Angle - TurnSpeed
                            ElseIf InputState(1) Then
                                Obj.Angle = Obj.Angle + TurnSpeed
                            End If
                            If .EnableGravity Then
                                If JumpControl.State Then
                                    If (Obj.ZVelocity = 0 And (Obj.Poses(Obj.TruePose).YVelocity * Obj.Velocity) = 0) And (Int(Obj.Z) = 0) Then
                                        Obj.ZVelocity = JumpVelocity
                                    End If
                                End If
                            End If
                            If InputState(0) Then
                                Obj.State = Obj.WalkState
                                Obj.Velocity = Obj.Velocity + .VelocityDecay
                                If Obj.Velocity > Obj.WalkSpeed Then Obj.Velocity = Obj.WalkSpeed
                            ElseIf InputState(2) Then
                                Obj.State = Obj.WalkState
                                Obj.Velocity = Obj.Velocity - .VelocityDecay
                                If Obj.Velocity < -Obj.WalkSpeed Then
                                    Obj.Velocity = -Obj.WalkSpeed
                                End If
                            Else
                                Obj.Velocity = Obj.Velocity - .VelocityDecay
                                If Obj.Velocity < .VelocityDecay Then
                                    Obj.State = Obj.IdleState
                                    Obj.Velocity = 0
                                End If
                            End If
                        Case CS_VerticalShooter
                            If InputState(3) = False And InputState(1) = True Then
                                Obj.State = Obj.WalkState
                                Obj.XVelocity = Obj.WalkSpeed
                            ElseIf InputState(3) = True And InputState(1) = False Then
                                Obj.State = Obj.WalkState
                                Obj.XVelocity = -Obj.WalkSpeed
                            Else
                                Obj.State = Obj.IdleState
                                Obj.XVelocity = 0
                            End If
                            Obj.YVelocity = Obj.WalkSpeed
                        Case CS_HorizontalShooter
                            If InputState(0) = False And InputState(2) = True Then
                                Obj.State = Obj.WalkState
                                Obj.YVelocity = Obj.WalkSpeed
                            ElseIf InputState(0) = True And InputState(2) = False Then
                                Obj.State = Obj.WalkState
                                Obj.YVelocity = -Obj.WalkSpeed
                            Else
                                Obj.State = Obj.IdleState
                                Obj.YVelocity = 0
                            End If
                            Obj.XVelocity = Obj.WalkSpeed
                        Case CS_Platformer
                            If InputState(3) = False And InputState(1) = True Then
                                Obj.Angle = 90 ' Right
                            ElseIf InputState(3) = True And InputState(1) = False Then
                                Obj.Angle = 270 ' Left
                            End If
                            If .EnableGravity Then
                                If InputState(0) Then
                                    If (Obj.ZVelocity = 0 And (Obj.Poses(Obj.TruePose).YVelocity * Obj.Velocity) = 0) And (Int(Obj.Z) = 0) Then
                                        Obj.ZVelocity = JumpVelocity
                                    End If
                                End If
                            End If
                            If InputState(3) Or InputState(1) Then
                                Obj.State = Obj.WalkState
                                Obj.Velocity = Obj.Velocity + .VelocityDecay
                                If Obj.Velocity > Obj.WalkSpeed Then Obj.Velocity = Obj.WalkSpeed
                            Else
                                Obj.Velocity = Obj.Velocity - .VelocityDecay
                            If Obj.Velocity < .VelocityDecay Then
                                Obj.State = Obj.IdleState
                                Obj.Velocity = 0
                            End If
                            End If
                        Case Else
                        End Select
                    End If
            End If
            End With
        Else
            InputState(0) = False
            InputState(1) = False
            InputState(2) = False
            InputState(3) = False
            Obj.Velocity = Obj.Velocity - Engine.VelocityDecay
            If Obj.Velocity < Engine.VelocityDecay Then
                Obj.State = Obj.IdleState
                Obj.Velocity = 0
            End If
        End If
        If (MoveCamera = True) And (Not (Camera Is Nothing)) Then
            Camera.CenterOn CLng(Obj.X) + CameraXOffset, CLng(Obj.Y - Obj.Z) + CameraYOffset
            If Engine.SmartMessageBoxPositioning Then
                Engine.DefaultMessageTop = IIf((Obj.Y - Obj.Z) < (Engine.ScreenHeight * 0.49), 1, 0)
            End If
        End If
    ElseIf TypeOf Obj Is Fury2Camera Then
        If Enabled Then
            If InputState(3) And InputState(1) Then
            ElseIf InputState(3) Then
                Obj.ViewportX = Obj.ViewportX - ScrollSpeed
            ElseIf InputState(1) Then
                Obj.ViewportX = Obj.ViewportX + ScrollSpeed
            End If
            If InputState(0) And InputState(2) Then
            ElseIf InputState(0) Then
                Obj.ViewportY = Obj.ViewportY - ScrollSpeed
            ElseIf InputState(2) Then
                Obj.ViewportY = Obj.ViewportY + ScrollSpeed
            End If
        Else
            InputState(0) = False
            InputState(1) = False
            InputState(2) = False
            InputState(3) = False
        End If
    End If
End Sub

Private Sub Class_Initialize()
    ScrollSpeed = 1
    MoveCamera = True
    Enabled = True
    LeftBinding = "Left"
    UpBinding = "Up"
    RightBinding = "Right"
    DownBinding = "Down"
    ActivateBinding = "OK"
End Sub

Private Sub Fury2Plugin_ControlActivate(Control As Fury2InputControl)
On Error Resume Next
    If Enabled = False Then Exit Sub
    If Control.Name = ActivateBinding Then
        If Obj.ActionAhead Then Control.Consume
    ElseIf Control.Name = UpBinding Then
        InputState(0) = True
        Control.Consume
    ElseIf Control.Name = RightBinding Then
        InputState(1) = True
        Control.Consume
    ElseIf Control.Name = DownBinding Then
        InputState(2) = True
        Control.Consume
    ElseIf Control.Name = LeftBinding Then
        InputState(3) = True
        Control.Consume
    End If
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Fury2InputControl)
On Error Resume Next
    If Enabled = False Then Exit Sub
    If Control.Name = UpBinding Then
        InputState(0) = False
        Control.Consume
    ElseIf Control.Name = RightBinding Then
        InputState(1) = False
        Control.Consume
    ElseIf Control.Name = DownBinding Then
        InputState(2) = False
        Control.Consume
    ElseIf Control.Name = LeftBinding Then
        InputState(3) = False
        Control.Consume
    End If
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
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
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
End Sub

Private Sub Fury2Plugin_Redraw()
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

