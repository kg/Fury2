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
Public TurnSpeed As Double
Public TileBased As Boolean

Private BlockAccumulator As Long
Private InputState(0 To 3) As Boolean

Public LeftBinding As String, UpBinding As String, RightBinding As String, DownBinding As String, ActivateBinding As String

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
        Control.Consume
        Obj.ActionAhead
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
On Error Resume Next
Dim l_sprObj As Fury2Sprite
Dim l_camObj As Fury2Camera
Dim l_wpNode As Fury2Waypoint
Dim l_sngX As Single, l_sngY As Single
Dim l_booBlocked As Boolean
    If Obj Is Nothing Then Exit Sub
    If TypeOf Obj Is Fury2Sprite Then
        Set l_sprObj = Obj
        l_sprObj.AnimateWhileMoving = True
        If Enabled Then
            With l_sprObj
                If TileBased Then
                    If Engine.DisableMovement Then
                        .Path.Clear
                    Else
                        If .Path.count <= 0 Then
                            Set l_wpNode = .Path.Add(.X, .Y)
                        Else
                            Set l_wpNode = .Path(1)
                        End If
                        l_booBlocked = .Blocked(l_wpNode.X, l_wpNode.Y)
                        If l_booBlocked Then
                            .Path.index = 2
                        Else
                            If .Path.index > 1 Then
                                .Path.index = 1
                            End If
                        End If
                        l_sngX = .TileX
                        l_sngY = .TileY
                        If InputState(3) = False And InputState(0) = False And _
                        InputState(1) = True And InputState(2) = False Then
                            l_sngX = l_sngX + 1 ' Right
                        ElseIf InputState(3) = False And InputState(0) = False And _
                        InputState(1) = True And InputState(2) = True Then
                            l_sngX = l_sngX + 1 ' Down-Right
                            l_sngY = l_sngY + 1
                        ElseIf InputState(3) = False And InputState(0) = False And _
                        InputState(1) = False And InputState(2) = True Then
                            l_sngY = l_sngY + 1 ' Down
                        ElseIf InputState(3) = True And InputState(0) = False And _
                        InputState(1) = False And InputState(2) = True Then
                            l_sngX = l_sngX - 1 ' Down-Left
                            l_sngY = l_sngY + 1
                        ElseIf InputState(3) = True And InputState(0) = False And _
                        InputState(1) = False And InputState(2) = False Then
                            l_sngX = l_sngX - 1 ' Left
                        ElseIf InputState(3) = True And InputState(0) = True And _
                        InputState(1) = False And InputState(2) = False Then
                            l_sngX = l_sngX - 1 ' Up-Left
                            l_sngY = l_sngY - 1
                        ElseIf InputState(3) = False And InputState(0) = True And _
                        InputState(1) = False And InputState(2) = False Then
                            l_sngY = l_sngY - 1 ' Up
                        ElseIf InputState(3) = False And InputState(0) = True And _
                        InputState(1) = True And InputState(2) = False Then
                            l_sngX = l_sngX + 1 ' Up-Right
                            l_sngY = l_sngY - 1
                        Else
                            Set l_wpNode = Nothing
                        End If
                        With .Map.Layers(.Layer)
                            l_sngX = (Floor(l_sngX) * .Tileset.TileWidth) + (.Tileset.TileWidth / 2)
                            l_sngY = (Floor(l_sngY) * .Tileset.TileHeight) + (.Tileset.TileHeight - 1)
                            If l_sprObj.Blocked(l_sngX, l_sngY) Then
                                l_sprObj.Face Array(l_sngX, l_sngY)
                            Else
                                l_wpNode.X = l_sngX
                                l_wpNode.Y = l_sngY
                            End If
                        End With
                    End If
                Else
                    If .Path.count <= 0 Then
                        If Engine.DisableMovement Then
                            .Velocity = .Velocity - Engine.VelocityDecay
                            If .Velocity <= 0.05 Then
                                .Velocity = 0
                            Else
                            End If
                        Else
                            If InputState(3) = False And InputState(0) = False And _
                            InputState(1) = True And InputState(2) = False Then
                                .Angle = 90 ' Right
                            ElseIf InputState(3) = False And InputState(0) = False And _
                            InputState(1) = True And InputState(2) = True Then
                                .Angle = 135 ' Down-Right
                            ElseIf InputState(3) = False And InputState(0) = False And _
                            InputState(1) = False And InputState(2) = True Then
                                .Angle = 180 ' Down
                            ElseIf InputState(3) = True And InputState(0) = False And _
                            InputState(1) = False And InputState(2) = True Then
                                .Angle = 225 ' Down-Left
                            ElseIf InputState(3) = True And InputState(0) = False And _
                            InputState(1) = False And InputState(2) = False Then
                                .Angle = 270 ' Left
                            ElseIf InputState(3) = True And InputState(0) = True And _
                            InputState(1) = False And InputState(2) = False Then
                                .Angle = 315 ' Up-Left
                            ElseIf InputState(3) = False And InputState(0) = True And _
                            InputState(1) = False And InputState(2) = False Then
                                .Angle = 0 ' Up
                            ElseIf InputState(3) = False And InputState(0) = True And _
                            InputState(1) = True And InputState(2) = False Then
                                .Angle = 45 ' Up-Right
                            End If
                            If InputState(3) Or InputState(1) Or InputState(0) Or InputState(2) Then
                                If (.State = .IdleState) Then .State = .WalkState
                                .Velocity = .Velocity + Engine.VelocityDecay
                                If .Velocity > .WalkSpeed Then .Velocity = .WalkSpeed
                            Else
                                .Velocity = .Velocity - Engine.VelocityDecay
                                If .Velocity < Engine.VelocityDecay Then
                                    If (.State = .WalkState) Then .State = .IdleState
                                    .Velocity = 0
                                End If
                            End If
                        End If
                    End If
                End If
            End With
        Else
            InputState(0) = False
            InputState(1) = False
            InputState(2) = False
            InputState(3) = False
            l_sprObj.Velocity = l_sprObj.Velocity - Engine.VelocityDecay
            If l_sprObj.Velocity < Engine.VelocityDecay Then
                l_sprObj.State = l_sprObj.IdleState
                l_sprObj.Velocity = 0
            End If
        End If
        If (MoveCamera = True) And (Not (Camera Is Nothing)) Then
            Camera.CenterOn CLng(l_sprObj.X) + CameraXOffset, CLng(l_sprObj.Y) + CameraYOffset
            If Engine.SmartMessageBoxPositioning Then
                Engine.DefaultMessageTop = IIf((l_sprObj.Y - l_sprObj.Z) < (Engine.ScreenHeight * 0.49), 1, 0)
            End If
        End If
    ElseIf TypeOf Obj Is Fury2Camera Then
        Set l_camObj = Obj
        If Enabled Then
            With l_camObj
                If InputState(3) And InputState(1) Then
                ElseIf InputState(3) Then
                    .ViewportX = .ViewportX - ScrollSpeed
                ElseIf InputState(1) Then
                    .ViewportX = .ViewportX + ScrollSpeed
                End If
                If InputState(0) And InputState(2) Then
                ElseIf InputState(0) Then
                    .ViewportY = .ViewportY - ScrollSpeed
                ElseIf InputState(2) Then
                    .ViewportY = .ViewportY + ScrollSpeed
                End If
            End With
        Else
            InputState(0) = False
            InputState(1) = False
            InputState(2) = False
            InputState(3) = False
        End If
    End If
End Sub

Private Sub Fury2Plugin_OutputPluginChanged()
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

