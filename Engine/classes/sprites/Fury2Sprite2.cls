VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Sprite"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
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
Implements IInspectable
Implements IVirtualFileSaveable
Implements Fury2Object

Private m_sprAttached As Fury2Sprite
Private m_sngAttachX As Single, m_sngAttachY As Single

Private m_lngLayer As Long
Private m_mapMap As Fury2Map
Private m_sprSprites As Fury2Sprites

Private m_strName As String
Private m_strScript As String
Private m_strTemplate As String

Private m_lngState As Long, m_staState As Fury2State
Private m_lngPose As Long, m_posPose As Fury2Pose
Private m_lngFrame As Long, m_fraFrame As Fury2PoseFrame
Private m_lngAIDelay As Long, m_lngAIUpdateRate As Long

Private m_booDestroyed As Boolean

Private m_lngDecayTime As Long

Private m_sngAnimationDelay As Single
Private m_lngAnimationDirection As Long
Private m_booAnimateWhileStopped As Boolean, m_booAnimateWhileMoving As Boolean

Private m_booDepthmapOverride As Boolean, m_booPoseOverride As Boolean, m_booScriptOverride As Boolean

Private m_booTrueMovement As Boolean, m_booRelativeAnimation As Boolean
Private m_booDisplayRotation As Boolean

Private m_booMoved As Boolean

Private m_lngProcessType As Long
Private m_booCull As Boolean

Private m_posPoses As Fury2Poses
Private m_staStates As Fury2States
Private m_pthPath As Fury2Path

Private m_spParam As SpriteParam
Private m_spOldParam As SpriteParam

Private m_objScript As Object
Private m_booScriptLoadFailed As Boolean

Private m_booScript_Update As Boolean, m_booScript_Hit As Boolean
Private m_booScript_HitMap As Boolean, m_booScript_Decay As Boolean
Private m_booScript_AIUpdate As Boolean, m_booScript_Activate As Boolean
Private m_booScript_Touched As Boolean, m_booScript_Load As Boolean
Private m_booScript_Died As Boolean, m_booScript_Step As Boolean
Private m_booScript_FadeOut As Boolean, m_booScript_Collided As Boolean
    
Private m_booAreasUpdated As Boolean

Private m_lngWalkState As Long, m_lngIdleState As Long

Public SavedPaths As Fury2Collection
Attribute SavedPaths.VB_VarDescription = "*"
Public AttachedGraphic As Fury2Graphic
Attribute AttachedGraphic.VB_VarDescription = "*"
Public Tag As String
Attribute Tag.VB_VarDescription = "An additional piece of data."
Public AnimationMultiplier As Single
Public Ignore As Boolean

Public Property Get DiffuseLight() As Boolean
    DiffuseLight = m_spParam.Params.DiffuseLight <> 0
End Property

Public Property Let DiffuseLight(ByVal NewValue As Boolean)
    m_spParam.Params.DiffuseLight = CByte(Abs(CLng(NewValue)))
End Property

Public Property Get VelocityMultiplier() As Single
    VelocityMultiplier = m_spParam.Velocity.VM
End Property

Public Property Let VelocityMultiplier(ByVal NewValue As Single)
    m_spParam.Velocity.VM = NewValue
End Property

Public Property Get Map() As Fury2Map
Attribute Map.VB_Description = "*The map containing the sprite."
    Set Map = m_mapMap
End Property

Public Property Get Color() As Long
Attribute Color.VB_Description = "The sprite's tint color.{Color}"
    Color = m_spParam.Params.Color
End Property

Public Property Let Color(ByVal NewColor As Long)
    m_spParam.Params.Color = NewColor
End Property

Public Function SightCheck(ByVal X As Single, ByVal Y As Single, Optional ByRef IgnoreSprite = Nothing) As Boolean
On Error Resume Next
    SightCheck = m_mapMap.Layers(m_lngLayer).Lighting.SightCheck(m_spParam.Position.X, m_spParam.Position.Y, X, Y, Me, IgnoreSprite)
End Function

Public Property Get AIUpdateRate() As Long
Attribute AIUpdateRate.VB_Description = "The number of frames between each time the AIUpdate event is triggered."
    AIUpdateRate = m_lngAIUpdateRate
End Property

Public Property Let AIUpdateRate(ByVal NewValue As Long)
On Error Resume Next
    m_lngAIUpdateRate = Abs(NewValue)
End Property

Public Property Get Attached() As Fury2Sprite
Attribute Attached.VB_Description = "*"
    Set Attached = m_sprAttached
End Property

Public Sub Attach(ByRef Sprite, Optional ByVal XOffset As Single = 0, Optional ByVal YOffset As Single = 0)
On Error Resume Next
    Set m_sprAttached = Sprite
    m_sngAttachX = XOffset
    m_sngAttachY = YOffset
End Sub

Public Sub AttachGraphic(Graphic)
On Error Resume Next
Dim l_gphGraphic As Fury2Graphic
    Set l_gphGraphic = Graphic
    Set AttachedGraphic = l_gphGraphic
    RefreshAttachedGraphic
    Err.Clear
End Sub

Sub RefreshAttachedGraphic()
On Error Resume Next
    With m_spParam
        If AttachedGraphic Is Nothing Then
'            .AttachedGraphic = 0
        Else
            AttachedGraphic.Refresh
'            .AttachedGraphic = AttachedGraphic.Handle
        End If
    End With
End Sub

Public Sub Refresh()
On Error Resume Next
    UpdateGraphic
End Sub

Public Property Get WalkState() As Long
Attribute WalkState.VB_Description = "The state used while the sprite is in motion."
    WalkState = m_lngWalkState
End Property

Public Property Let WalkState(ByVal NewState As Long)
    m_lngWalkState = NewState
End Property

Public Property Get IdleState() As Long
Attribute IdleState.VB_Description = "The state used when the sprite is not in motion."
    IdleState = m_lngIdleState
End Property

Public Property Let IdleState(ByVal NewState As Long)
    m_lngIdleState = NewState
End Property

Public Property Get IlluminationLevel() As Long
Attribute IlluminationLevel.VB_Description = "*If the sprite is on a layer with lighting enabled, this property contains the light currently being recieved by the sprite."
    IlluminationLevel = m_spParam.Params.IlluminationLevel
End Property

Public Property Get CastShadow() As Boolean
Attribute CastShadow.VB_Description = "If True, the sprite will cast a shadow when on a layer with lighting enabled."
    CastShadow = (m_spParam.Params.SpecialFX And 4) = 4
End Property

Public Property Let CastShadow(ByVal NewValue As Boolean)
    m_spParam.Params.SpecialFX = (m_spParam.Params.SpecialFX And (Not 4)) Or (IIf(NewValue, 4, 0))
End Property

Public Property Get DisplayRotation() As Boolean
Attribute DisplayRotation.VB_Description = "If True, the sprite will be rotated onscreen according to its Angle."
    DisplayRotation = m_booDisplayRotation
End Property

Public Property Let DisplayRotation(ByVal Value As Boolean)
    m_booDisplayRotation = Value
End Property

Public Property Get Destroyed() As Boolean
Attribute Destroyed.VB_Description = "*This value is true if the sprite has been destroyed by a Die command."
    Destroyed = m_booDestroyed
End Property

Private Sub Dirty()
On Error Resume Next
'    m_Engine.Cameras.Dirty
End Sub

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Sprite: " & m_strName
End Function

Function SupportsAction(ByVal Name As String) As Boolean
On Error Resume Next
    With m_Engine.ScriptEngine.Language
        Select Case LCase(Trim(Name))
        Case "update"
            SupportsAction = m_booScript_Update
        Case "hit"
            SupportsAction = m_booScript_Hit
        Case "hitmap"
            SupportsAction = m_booScript_HitMap
        Case "decay"
            SupportsAction = m_booScript_Decay
        Case "aiupdate"
            SupportsAction = m_booScript_AIUpdate
        Case "activate"
            SupportsAction = m_booScript_Activate
        Case "touched"
            SupportsAction = m_booScript_Touched
        Case "died"
            SupportsAction = m_booScript_Died
        Case "step"
            SupportsAction = m_booScript_Step
        Case "fadeout"
            SupportsAction = m_booScript_FadeOut
        Case "collided"
            SupportsAction = m_booScript_Collided
        Case Else
            SupportsAction = .ImplementsMethod(m_strScript, Name)
        End Select
    End With
    Err.Clear
End Function

Function Action(Optional ByVal Activator As Object, Optional ByVal ActionName As String = "Activate") As Boolean
On Error Resume Next
    Action = True
    m_Engine.ContextLevelAdd "Sprite_" + ActionName
    If m_objScript Is Nothing Then Action = False
    If m_objScript Is Nothing Then Else CallByName m_objScript, ActionName, VbMethod, Activator
    If Err.Number <> 0 Then
        Err.Clear
        If m_objScript Is Nothing Then Else CallByName m_objScript, ActionName, VbMethod
        If Err.Number <> 0 Then Action = False
    End If
    m_Engine.ContextLevelRemove
End Function

Function ActionAhead(Optional ByVal Distance As Single = -1, Optional ByVal ActionName As String = "Activate") As Boolean
On Error Resume Next
Dim l_objTarget As Object
Dim l_sprTarget As Fury2Sprite, l_araTarget As Fury2Area
    If Distance = -1 Then
        Distance = m_Engine.ActivationDistance
    End If
    Set l_objTarget = Me.ObjectCollisionCheck(Sin(m_spParam.Velocity.B * c_dblRadian) * Distance, -Cos(m_spParam.Velocity.B * c_dblRadian) * Distance)
    If l_objTarget Is Nothing Then
    Else
        If TypeOf l_objTarget Is Fury2Sprite Then
            Set l_sprTarget = l_objTarget
            ActionAhead = l_sprTarget.Action(Me, ActionName)
        Else
            Set l_araTarget = l_objTarget
            ActionAhead = l_araTarget.Action(Me, ActionName)
        End If
    End If
End Function

Public Property Get Alpha() As Single
Attribute Alpha.VB_Description = "The opacity of the sprite."
    Alpha = m_spParam.Params.Alpha
End Property

Public Property Let Alpha(ByVal NewValue As Single)
    m_spParam.Params.Alpha = NewValue
End Property

Public Property Get Angle() As Single
Attribute Angle.VB_Description = "The direction the sprite is facing. (Also affects Pose unless PoseOverride is True)"
    Angle = m_spParam.Velocity.B
End Property

Public Property Let Angle(ByVal NewValue As Single)
    m_spParam.Velocity.B = NewValue
    ReselectPose
End Property

Public Property Let Animate(ByVal NewValue As Boolean)
Attribute Animate.VB_Description = "If True, the sprite will animate, both while moving and while stopped."
    m_booAnimateWhileStopped = NewValue
    m_booAnimateWhileMoving = NewValue
End Property

Public Property Get AnimateWhileMoving() As Boolean
Attribute AnimateWhileMoving.VB_Description = "If True, the sprite will animate while moving."
    AnimateWhileMoving = m_booAnimateWhileMoving
End Property

Public Property Let AnimateWhileMoving(ByVal NewValue As Boolean)
    m_booAnimateWhileMoving = NewValue
End Property

Public Property Get AnimateWhileStopped() As Boolean
Attribute AnimateWhileStopped.VB_Description = "If True, the sprite will animate while stopped."
    AnimateWhileStopped = m_booAnimateWhileStopped
End Property

Public Property Let AnimateWhileStopped(ByVal NewValue As Boolean)
    m_booAnimateWhileStopped = NewValue
End Property

Public Property Get AnimationDelay() As Single
Attribute AnimationDelay.VB_Description = "*The delay accumulator for animations."
    AnimationDelay = m_sngAnimationDelay
End Property

Public Property Let AnimationDelay(ByVal NewValue As Single)
    m_sngAnimationDelay = NewValue
End Property

Public Property Get AnimationDirection() As Long
Attribute AnimationDirection.VB_Description = "*The direction animations are currently being played in."
    AnimationDirection = m_lngAnimationDirection
End Property

Public Property Let AnimationDirection(ByVal NewValue As Long)
    If (NewValue = 0) Then Exit Property
    m_lngAnimationDirection = NewValue
End Property

Public Property Let Class(ByVal NewValue As Fury2SpriteClasses)
Attribute Class.VB_Description = "The sprite's class. Used for behaviors."
    m_spParam.Type = NewValue
End Property

Public Property Get Class() As Fury2SpriteClasses
    Class = m_spParam.Type
End Property

Public Sub PushPath()
On Error Resume Next
    SavedPaths.Add m_pthPath
    Set m_pthPath = New Fury2Path
End Sub

Public Sub PopPath()
On Error Resume Next
    Set m_pthPath = SavedPaths.Item(SavedPaths.Count)
    SavedPaths.Remove SavedPaths.Count
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set SavedPaths = New Fury2Collection
    Set m_posPoses = New Fury2Poses
    Set m_staStates = New Fury2States
    Set m_pthPath = New Fury2Path
    m_lngAnimationDirection = 1
    m_lngState = 1
    m_lngPose = 1
    m_lngFrame = 1
    Alpha = 1
    ScaleLevel = 1
    Visible = True
    Name = "Sprite"
    SpriteType = F2ST_Entity
    VelocityMultiplier = 1
    AnimationMultiplier = 1
    m_spParam.Obj = ObjPtr(Me)
End Sub

Public Sub CopyTo(ByVal OtherSprite, Optional ByVal AsTemplate As Boolean = False)
On Error Resume Next
Dim l_sprOther As Fury2Sprite
Dim l_spOld As SpriteParam
    Set l_sprOther = OtherSprite
    If l_sprOther Is Nothing Then Exit Sub
    With l_sprOther
        Set .States = Me.States.Duplicate
        Set .Poses = Me.Poses.Duplicate
    
        l_spOld = .GetParam
        .SetParam m_spParam
        If AsTemplate Then
            If Trim(.Name) = "" Then
                .Name = m_strName
            End If
        Else
            .Name = m_strName
        End If
        .DepthmapOverride = Me.DepthmapOverride
        .PoseOverride = Me.PoseOverride
        
        .Alpha = Me.Alpha
        .Angle = Me.Angle
        .AnimateWhileMoving = Me.AnimateWhileMoving
        .AnimateWhileStopped = Me.AnimateWhileStopped
        .AnimationDelay = Me.AnimationDelay
        .AnimationDirection = Me.AnimationDirection
        .AnimationMultiplier = Me.AnimationMultiplier
        .Class = Me.Class
        .Effect = Me.Effect
        .FadeTarget = Me.FadeTarget
        .FadeVelocity = Me.FadeVelocity
        .CastShadow = Me.CastShadow
        .Color = Me.Color
        .DiffuseLight = Me.DiffuseLight
        
        .State = Me.State
        .Pose = Me.Pose
        .Frame = Me.Frame
        
        If AsTemplate Then
            .Position(0) = l_spOld.Position.X
            .Position(1) = l_spOld.Position.Y
            .Position(2) = l_spOld.Position.Z
        Else
        End If
        
        .RotationAngle = Me.RotationAngle
        .ScaleLevel = Me.ScaleLevel
        .Solid = Me.Solid
        .SpinTarget = Me.SpinTarget
        .SpinVelocity = Me.SpinVelocity
        .SpriteType = Me.SpriteType
        .Velocity = Me.Velocity
        .VelocityMultiplier = Me.VelocityMultiplier
        .Weight = Me.Weight
        .XVelocity = Me.XVelocity
        .YVelocity = Me.YVelocity
        .ZVelocity = Me.ZVelocity
        
        .IdleState = Me.IdleState
        .WalkState = Me.WalkState
        
        .AIUpdateRate = Me.AIUpdateRate
       
        If .ScriptOverride And AsTemplate Then
        Else
            .ScriptSource = Me.ScriptSource
        End If
        
        If AsTemplate Then
        Else
            .Template = m_strTemplate
        End If
    End With
End Sub

Public Property Get DecayTime() As Long
Attribute DecayTime.VB_Description = "The amount of time until the sprite is destroyed, in frames. Used by projectiles."
    DecayTime = m_lngDecayTime
End Property

Public Property Let DecayTime(ByVal NewValue As Long)
    m_lngDecayTime = NewValue
End Property

Public Property Get DepthmapOverride() As Boolean
Attribute DepthmapOverride.VB_Description = "*If True, the sprite's scale will not be read from a map's depthmap."
    DepthmapOverride = m_booDepthmapOverride
End Property

Public Property Let DepthmapOverride(ByVal NewValue As Boolean)
    m_booDepthmapOverride = NewValue
End Property

Public Sub Die()
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    Dirty
    m_booDestroyed = True
    m_Engine.ContextLevelAdd Name + " : Die"
    m_objScript.Die
    For Each l_lyrLayer In m_mapMap.Layers
        l_lyrLayer.Sprites.Remove Me
    Next l_lyrLayer
    m_mapMap.Sprites.Remove Me
    m_Engine.ContextLevelRemove
End Sub

Public Function Duplicate() As Fury2Sprite
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Set l_sprSprite = New Fury2Sprite
    CopyTo l_sprSprite, False
    Set Duplicate = l_sprSprite
End Function

Public Property Let Effect(ByVal NewValue As Fury2SpriteEffects)
Attribute Effect.VB_Description = "The rendering effect used to draw the sprite."
    m_spParam.Params.Effect = NewValue
End Property

Public Property Get Effect() As Fury2SpriteEffects
    Effect = m_spParam.Params.Effect
End Property

Public Sub Face(ByRef OtherSprite)
On Error Resume Next
    Angle = m_Engine.AngleBetween(X, Y, OtherSprite.X, OtherSprite.Y)
End Sub

Public Sub Fade(Optional ByVal FromAlpha = -1, Optional ByVal ToAlpha = -1, Optional ByVal Speed = 1 / 60)
On Error Resume Next
Dim l_sngAlpha As Single
    If ToAlpha = -1 Then
        l_sngAlpha = m_spParam.Params.Alpha
    Else
        l_sngAlpha = CSng(ToAlpha)
    End If
    If FromAlpha = -1 Then
    Else
        m_spParam.Params.Alpha = CSng(FromAlpha)
    End If
    m_spParam.Velocity.AT = l_sngAlpha
    If (m_spParam.Params.Alpha < m_spParam.Velocity.AT) Then
        m_spParam.Velocity.A = Abs(CSng(Speed))
    Else
        m_spParam.Velocity.A = -Abs(CSng(Speed))
    End If
End Sub

Public Property Get FadeTarget() As Single
Attribute FadeTarget.VB_Description = "*The target opacity for a fade."
    FadeTarget = m_spParam.Velocity.AT
End Property

Public Property Let FadeTarget(ByVal NewValue As Single)
    m_spParam.Velocity.AT = NewValue
End Property

Public Property Get FadeVelocity() As Single
Attribute FadeVelocity.VB_Description = "*The speed at which a fade should occur."
    FadeVelocity = m_spParam.Velocity.A
End Property

Public Property Let FadeVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.A = NewValue
End Property

Public Property Let Frame(ByVal NewValue As Long)
Attribute Frame.VB_Description = "The current frame being displayed."
    m_lngFrame = NewValue
    UpdateFrame
End Property

Public Property Get Frame() As Long
    Frame = m_lngFrame
End Property

Public Sub Free()
On Error Resume Next
Dim l_posPose As Fury2Pose
    Set m_staState = Nothing
    Set m_posPose = Nothing
    Set m_fraFrame = Nothing
    For Each l_posPose In m_posPoses
        l_posPose.Free
    Next l_posPose
    m_pthPath.Clear
    m_posPoses.Clear
    m_staStates.Clear
    Set SavedPaths = Nothing
    Set AttachedGraphic = Nothing
    Set m_posPoses = Nothing
    Set m_staStates = Nothing
    Set m_pthPath = Nothing
    Set m_objScript = Nothing
    Set m_sprAttached = Nothing
    Set m_sprSprites = Nothing
    Set m_mapMap = Nothing
End Sub

Friend Property Let Index(ByVal NewValue As Long)
    m_spParam.Index = NewValue
End Property

Public Property Get Index() As Long
Attribute Index.VB_Description = "*The numeric index of the sprite within its parent (if any)."
    Index = m_spParam.Index
End Property

Public Sub Initialize()
On Error Resume Next
Dim l_objScript As Object
Dim l_posPose As Fury2Pose
    LoadTemplate
    Set l_objScript = Script

    l_objScript.Init

    l_objScript.Initialize

    If m_booDestroyed Then Exit Sub
    For Each l_posPose In m_posPoses
        l_posPose.SetParent Me
    Next l_posPose
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_booValue As Boolean, l_sngValue As Single, l_lngValue As Long, l_bytValue As Byte
Dim l_sngStateVelocity As Single, l_staState As Fury2State
    With File
        .ReadSegment_Begin
            .ReadSegment_Begin
                .Load m_strName
                .RawLoad VarPtr(m_spParam.Position.X), 4 * 3
                .RawLoad VarPtr(m_spParam.Velocity.B), 4
                .Load m_strTemplate
                .Load Tag
            .ReadSegment_End
            .ReadSegment_Begin
                .RawLoad VarPtr(m_spParam.Velocity.V), 4
                .RawLoad VarPtr(m_spParam.Velocity.X), 4 * 3
                .RawLoad VarPtr(m_spParam.Velocity.BR), 4
                .Load l_sngValue
                If l_sngValue <> 0 Then
                    If l_sngValue > 0 Then
                        If l_sngValue < 256 Then
                            l_sngStateVelocity = l_sngValue
                        End If
                    End If
                End If
                .RawLoad VarPtr(m_spParam.Velocity.A), 4
                .RawLoad VarPtr(m_spParam.Velocity.BRT), 4
                .RawLoad VarPtr(m_spParam.Velocity.AT), 4
            .ReadSegment_End
            .ReadSegment_Begin
                .Load m_lngPose
                .Load m_lngFrame
                .Load m_lngState
                .Load m_sngAnimationDelay
            .ReadSegment_End
            .ReadSegment_Begin
                .Load m_booAnimateWhileStopped
                .Load m_booAnimateWhileMoving
                .Skip 4
                .Load m_booTrueMovement
                .Load m_booRelativeAnimation
                .Load m_lngIdleState
                .Load m_lngWalkState
            .ReadSegment_End
            .ReadSegment_Begin
                .Load l_booValue
                m_spParam.Stats.Solid = CByte(Abs(l_booValue))
                .Load l_sngValue
                m_spParam.Stats.Weight = CSng(l_sngValue)
                .Load l_booValue
                m_spParam.Stats.Pushable = CByte(Abs(l_booValue))
                .Load l_booValue
                .Load l_booValue
                .Load l_booValue
                m_spParam.Stats.Platform = CByte(Abs(l_booValue))
            .ReadSegment_End
            .ReadSegment_Begin
                .Load l_lngValue
                m_spParam.Params.Effect = l_lngValue
                .Load l_sngValue
                m_spParam.Params.Alpha = CSng(l_sngValue)
                .Load l_sngValue
                m_spParam.Params.ScaleLevel = CSng(l_sngValue)
                .Load l_sngValue
                m_spParam.Params.RotationAngle = CSng(l_sngValue)
                .Load m_booDepthmapOverride
                .Load m_booPoseOverride
                .Load l_booValue
                CastShadow = l_booValue
                .Load l_lngValue
                m_spParam.Params.Color = l_lngValue
                l_booValue = True
                If Not .EOS Then .Load l_booValue
                DiffuseLight = l_booValue
            .ReadSegment_End
            .ReadSegment_Begin
                .Load m_strScript
                .Load m_booScriptOverride
            .ReadSegment_End
            
            ' ai settings
            .ReadSegment_Begin
            .ReadSegment_End
            
            .ReadSegment_Begin
                .Load l_bytValue
                m_spParam.Type = l_bytValue
                .Load l_booValue
                m_spParam.Stats.Cull = CByte(Abs(l_booValue))
                .Load l_booValue
                If l_booValue Then
                    Me.SpriteType = F2ST_Projectile
                    .Load l_booValue
                Else
                    .Load l_booValue
                    If l_booValue Then
                        Me.SpriteType = F2ST_Entity
                    Else
                        Me.SpriteType = F2ST_Object
                    End If
                End If
                ' Projectile options
                .ReadSegment_Begin
                    .Load m_lngDecayTime
                .ReadSegment_End
                ' AI Options
                .ReadSegment_Begin
                    .Load m_lngAIUpdateRate
                .ReadSegment_End
                .Load Ignore
            .ReadSegment_End
            
            ' state mappings
            .ReadSegment_Begin
            .ReadSegment_End
            
            .Load m_posPoses
            
            ' path
            .Load m_pthPath
            
            ' ai
            .ReadSegment_Begin
            .ReadSegment_End
            
            .Load m_staStates
        .ReadSegment_End
        VelocityMultiplier = 1
        AnimationMultiplier = 1
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .WriteSegment_Begin
                .Save m_strName
                .RawSave VarPtr(m_spParam.Position.X), 4 * 3
                .RawSave VarPtr(m_spParam.Velocity.B), 4
                .Save m_strTemplate
                .Save Tag
            .WriteSegment_End
            .WriteSegment_Begin
                .RawSave VarPtr(m_spParam.Velocity.V), 4
                .RawSave VarPtr(m_spParam.Velocity.X), 4 * 3
                .RawSave VarPtr(m_spParam.Velocity.BR), 4
                .Save CSng(0)
                .RawSave VarPtr(m_spParam.Velocity.A), 4
                .RawSave VarPtr(m_spParam.Velocity.BRT), 4
                .RawSave VarPtr(m_spParam.Velocity.AT), 4
            .WriteSegment_End
            .WriteSegment_Begin
                .Save m_lngPose
                .Save m_lngFrame
                .Save m_lngState
                .Save m_sngAnimationDelay
            .WriteSegment_End
            .WriteSegment_Begin
                .Save m_booAnimateWhileStopped
                .Save m_booAnimateWhileMoving
                .Skip 4
                .Save m_booTrueMovement
                .Save m_booRelativeAnimation
                .Save m_lngIdleState
                .Save m_lngWalkState
            .WriteSegment_End
            .WriteSegment_Begin
                .Save CBool(m_spParam.Stats.Solid)
                .Save m_spParam.Stats.Weight
                .Save CBool(m_spParam.Stats.Pushable)
                .Skip 2
                .Skip 2
                .Save CBool(m_spParam.Stats.Platform)
            .WriteSegment_End
            .WriteSegment_Begin
                .Save CLng(m_spParam.Params.Effect)
                .Save m_spParam.Params.Alpha
                .Save m_spParam.Params.ScaleLevel
                .Save m_spParam.Params.RotationAngle
                .Save m_booDepthmapOverride
                .Save m_booPoseOverride
                .Save CastShadow
                .Save m_spParam.Params.Color
                .Save DiffuseLight
            .WriteSegment_End
            .WriteSegment_Begin
                .Save m_strScript
                .Save m_booScriptOverride
            .WriteSegment_End
            
            .WriteSegment_Begin
            .WriteSegment_End
            
            .WriteSegment_Begin
                .Save CByte(m_spParam.Type)
                .Save CBool(m_spParam.Stats.Cull)
                .Save CBool(m_spParam.Type = F2ST_Projectile)
                .Save CBool(m_spParam.Type = F2ST_Entity)
                .WriteSegment_Begin
                    .Save m_lngDecayTime
                .WriteSegment_End
                ' AI Options
                .WriteSegment_Begin
                    .Save m_lngAIUpdateRate
                .WriteSegment_End
                .Save Ignore
            .WriteSegment_End
            
            .WriteSegment_Begin
            .WriteSegment_End
            
            .Save m_posPoses
            
            .Save m_pthPath
            
            .WriteSegment_Begin
            .WriteSegment_End
            
            .Save m_staStates
        
        .WriteSegment_End
    End With
End Sub

Friend Property Let Layer(ByVal NewLayer As Long)
    m_lngLayer = NewLayer
End Property

Public Property Get Layer() As Long
Attribute Layer.VB_Description = "*The numeric index of the layer containing the sprite (if any)."
    Layer = m_lngLayer
End Property

Public Sub Load()
On Error Resume Next
    If m_booEditor Then
    Else
        Script.Load
    End If
    If m_booDestroyed Then Exit Sub
    m_posPoses.LoadGraphics
    UpdateState True
    
    InitAreas
'    UpdatePose True
'    UpdateFrame True
End Sub

Public Sub LoadTemplate()
On Error Resume Next
Dim l_sprTemplate As Fury2Sprite, l_strTemplateSprite As String, l_strTemplateIndex As String
    If Trim(m_strTemplate) = "" Then
    Else
        l_strTemplateSprite = Left(m_strTemplate, InStrRev(m_strTemplate, ":") - 1)
        l_strTemplateIndex = Mid(m_strTemplate, InStrRev(m_strTemplate, ":") + 1)
        Set l_sprTemplate = m_Engine.LoadSprite(l_strTemplateSprite, l_strTemplateIndex)
        If l_sprTemplate Is Nothing Then
        Else
            l_sprTemplate.CopyTo Me, True
        End If
    End If
End Sub

Public Property Let Name(NewName As String)
Attribute Name.VB_Description = "The sprite's name."
    m_strName = NewName
End Property

Public Property Get Name() As String
    Name = m_strName
End Property

Public Function ObjectCollisionCheck(Optional ByVal XOffset As Single = 0, Optional ByVal YOffset As Single = 0) As Object
On Error Resume Next
Dim l_rctThis As Fury2Rect, l_rctThat As Fury2Rect
Dim l_sprSprite As Fury2Sprite, l_araArea As Fury2Area
    Set l_rctThis = Me.Rectangle(False)
    l_rctThis.Translate CLng(XOffset), CLng(YOffset)
    
    For Each l_sprSprite In m_sprSprites
        If l_sprSprite Is Me Then
        Else
            Set l_rctThat = l_sprSprite.Rectangle(False)
            If l_rctThat.Intersect(l_rctThis) Then
                Set ObjectCollisionCheck = l_sprSprite
                Err.Clear
                Exit Function
            End If
        End If
    Next l_sprSprite
    
    For Each l_araArea In m_mapMap.Areas
        Set l_rctThat = l_araArea.Rectangle
        If l_rctThat.Intersect(l_rctThis) Then
            Set ObjectCollisionCheck = l_araArea
            Err.Clear
            Exit Function
        End If
    Next l_araArea
    Err.Clear
End Function

Public Property Get Parent() As Fury2Map
Attribute Parent.VB_Description = "*The sprite's parent (if any)."
    Set Parent = m_mapMap
End Property

Public Function PerformAction(Optional ByVal Activator = Nothing, Optional ByRef Action = "Activate") As Boolean
On Error Resume Next
    m_Engine.ContextLevelAdd Name + " : PerformAction(" + Action + ")"
    Err.Clear
    CallByName m_objScript, Action, VbMethod, Activator
    If Err.Number <> 0 Then
        Err.Clear
        CallByName m_objScript, Action, VbMethod
        If Err.Number <> 0 Then PerformAction = False
    End If
    Err.Clear
    m_Engine.ContextLevelRemove
End Function

Public Property Get Pointer() As Long
Attribute Pointer.VB_Description = "*A numeric pointer to the sprite's internal representation."
On Error Resume Next
    Pointer = VarPtr(m_spParam)
End Property

Public Property Get Path() As Fury2Path
Attribute Path.VB_Description = "The sprite's path."
    Set Path = m_pthPath
End Property

Public Property Set Path(ByVal NewPath As Fury2Path)
    Set m_pthPath = NewPath
End Property

Public Property Let Pose(ByVal NewValue As Long)
Attribute Pose.VB_Description = "The current pose."
    m_lngPose = NewValue
    UpdatePose
End Property

Public Property Get Pose() As Long
    Pose = m_lngPose
End Property

Public Property Get PoseOverride() As Boolean
Attribute PoseOverride.VB_Description = "If True, Pose is not altered by changes to Angle."
    PoseOverride = m_booPoseOverride
End Property

Public Property Let PoseOverride(ByVal NewValue As Boolean)
    m_booPoseOverride = NewValue
End Property

Friend Property Set Poses(ByVal NewValue As Fury2Poses)
    Set m_posPoses = NewValue
    UpdatePose True
End Property

Public Property Get Poses() As Fury2Poses
Attribute Poses.VB_Description = "~The sprite's defined poses."
    Set Poses = m_posPoses
End Property

Public Property Let Position(Index, NewValue As Single)
Attribute Position.VB_Description = "*"
On Error Resume Next
    If VarType(Index) = vbString Then
        Select Case LCase(Trim(Index))
        Case "x"
            m_spParam.Position.X = NewValue
        Case "y"
            m_spParam.Position.Y = NewValue
        Case "z"
            m_spParam.Position.Z = NewValue
        Case Else
        End Select
    Else
        Select Case CLng(Index)
        Case 0
            m_spParam.Position.X = NewValue
        Case 1
            m_spParam.Position.Y = NewValue
        Case 2
            m_spParam.Position.Z = NewValue
        Case Else
        End Select
    End If
End Property

Public Property Get Position(Index) As Single
On Error Resume Next
    If VarType(Index) = vbString Then
        Select Case LCase(Trim(Index))
        Case "x"
            Position = m_spParam.Position.X
        Case "y"
            Position = m_spParam.Position.Y
        Case "z"
            Position = m_spParam.Position.Z
        Case Else
        End Select
    Else
        Select Case CLng(Index)
        Case 0
            Position = m_spParam.Position.X
        Case 1
            Position = m_spParam.Position.Y
        Case 2
            Position = m_spParam.Position.Z
        Case Else
        End Select
    End If
End Property

Public Function PointInside(ByVal X As Long, ByVal Y As Long, Optional ByVal Onscreen As Boolean = True) As Boolean
On Error Resume Next
    If Onscreen Then
        If X < Floor(m_spParam.Position.X - (m_spParam.Graphic.Rectangle.Width / 2)) Then Exit Function
        If Y < Floor(m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height)) Then Exit Function
        If X >= Floor(m_spParam.Position.X + (m_spParam.Graphic.Rectangle.Width / 2)) Then Exit Function
        If Y >= Floor(m_spParam.Position.Y) Then Exit Function
    Else
        If X < Floor(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2)) Then Exit Function
        If Y < Floor(m_spParam.Position.Y - (m_spParam.Obstruction.Height)) Then Exit Function
        If X >= Floor(m_spParam.Position.X + (m_spParam.Obstruction.Width / 2)) Then Exit Function
        If Y >= Floor(m_spParam.Position.Y) Then Exit Function
    End If
    PointInside = True
End Function

Public Property Get Rectangle(Optional ByVal Onscreen As Boolean = True) As Fury2Rect
Attribute Rectangle.VB_Description = "*Returns a rectangle enclosing the sprite."
    If Onscreen Then
        Set Rectangle = F2Rect(Floor(m_spParam.Position.X - (m_spParam.Graphic.Rectangle.Width / 2)), Floor(m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height)), m_spParam.Graphic.Rectangle.Width, m_spParam.Graphic.Rectangle.Height, False)
    Else
        Set Rectangle = F2Rect(Floor(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2)), Floor(m_spParam.Position.Y - (m_spParam.Obstruction.Height)), m_spParam.Obstruction.Width, m_spParam.Obstruction.Height, False)
    End If
End Property

Public Sub Render(ByVal Surface As Fury2Image, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
Dim m_cpCamera As CameraParam
Dim m_spSprite As SpriteParam
    m_spSprite = m_spParam
    m_cpCamera.Rectangle = Surface.Rectangle.GetRectangle
    m_cpCamera.pImage = Surface.Handle
    m_cpCamera.ViewportX = 0
    m_cpCamera.ViewportY = 0
    m_cpCamera.Alpha = 255
    m_spSprite.Position.X = X + (m_spSprite.Graphic.Rectangle.Width / 2)
    m_spSprite.Position.Y = Y + (m_spSprite.Graphic.Rectangle.Height)
    m_spSprite.SortedNext = 0
    m_spSprite.Next = 0
    RenderSprites VarPtr(m_spSprite), m_cpCamera
End Sub

Private Sub ReselectPose()
On Error Resume Next
Dim l_lngHeading As Long
Dim l_lngOldPose As Long
    If m_booPoseOverride Then Exit Sub
    l_lngHeading = CLng((((Angle * CSng(m_staState.DirectionPoses)) / 360!) + 0!)) Mod m_staState.DirectionPoses
    l_lngOldPose = m_lngPose
    Pose = (l_lngHeading) + 1
    UpdatePose
End Sub

Public Property Get RotationAngle() As Single
Attribute RotationAngle.VB_Description = "The angle to rotate the sprite's onscreen display by."
    RotationAngle = m_spParam.Params.RotationAngle
End Property

Public Property Let RotationAngle(ByVal NewValue As Single)
    m_spParam.Params.RotationAngle = NewValue
End Property

Public Property Get ScaleLevel() As Single
Attribute ScaleLevel.VB_Description = "The amount to scale the sprite's onscreen display by."
    ScaleLevel = m_spParam.Params.ScaleLevel
End Property

Public Property Let ScaleLevel(ByVal NewValue As Single)
    m_spParam.Params.ScaleLevel = NewValue
End Property

Public Property Get Script() As Object
Attribute Script.VB_Description = "*Returns the sprite's script object (if any)."
On Error Resume Next
Dim l_strScript As String
Dim l_strName As String
    Err.Clear
    If Trim(m_strScript) = "" Then Exit Property
    If m_objScript Is Nothing Then
        If (m_booScriptLoadFailed = False) Then
            Err.Clear
            m_Engine.SpriteObjectCount = m_Engine.SpriteObjectCount + 1
            If Left(Trim(m_strScript), 2) = "<@" Then
                l_strScript = Trim(m_strScript)
                l_strName = Mid(l_strScript, 3, Len(l_strScript) - 3)
                Set m_objScript = m_Engine.ScriptEngine.Eval(l_strName)
                m_booScript_Update = m_objScript.ImplementsMethod("Update")
                m_booScript_Hit = m_objScript.ImplementsMethod("Hit")
                m_booScript_HitMap = m_objScript.ImplementsMethod("HitMap")
                m_booScript_Decay = m_objScript.ImplementsMethod("Decay")
                m_booScript_AIUpdate = m_objScript.ImplementsMethod("AIUpdate")
                m_booScript_Activate = m_objScript.ImplementsMethod("Activate")
                m_booScript_Touched = m_objScript.ImplementsMethod("Touched")
                m_booScript_Load = m_objScript.ImplementsMethod("Load")
                m_booScript_Died = m_objScript.ImplementsMethod("Died")
                m_booScript_Step = m_objScript.ImplementsMethod("Step")
                m_booScript_FadeOut = m_objScript.ImplementsMethod("FadeOut")
            Else
                l_strName = m_Engine.VariableName("SS" & m_Engine.SpriteObjectCount)
                With m_Engine.ScriptEngine.Language
                    l_strScript = .GenerateClassVariable("Object") & .LineTerminator & _
                    .GenerateClassVariable("Map") & .LineTerminator & _
                    .GenerateClassMethod("F2OInit", Array("T", "M"), _
                    .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "T", True) & .LineTerminator & _
                    .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "M", True) _
                    ) & .LineTerminator & m_strScript
                    m_booScript_Update = .ImplementsMethod(l_strScript, "Update")
                    m_booScript_Hit = .ImplementsMethod(l_strScript, "Hit")
                    m_booScript_HitMap = .ImplementsMethod(l_strScript, "HitMap")
                    m_booScript_Decay = .ImplementsMethod(l_strScript, "Decay")
                    m_booScript_AIUpdate = .ImplementsMethod(l_strScript, "AIUpdate")
                    m_booScript_Activate = .ImplementsMethod(l_strScript, "Activate")
                    m_booScript_Touched = .ImplementsMethod(l_strScript, "Touched")
                    m_booScript_Load = .ImplementsMethod(l_strScript, "Load")
                    m_booScript_Died = .ImplementsMethod(l_strScript, "Died")
                    m_booScript_Step = .ImplementsMethod(l_strScript, "Step")
                    m_booScript_FadeOut = .ImplementsMethod(l_strScript, "FadeOut")
                    m_Engine.CacheScript l_strName, .GenerateClass(l_strName, l_strScript)
                    m_Engine.ScriptEngine.AddCode .GenerateAssignment("New" & l_strName, .GenerateInstantiation(l_strName), True)
                    Set m_objScript = m_Engine.ScriptEngine.Eval("New" & l_strName)
                End With
            End If
            Set m_Engine.IObjThis = Me
        
            m_objScript.F2OInit Me, m_mapMap
        
            If (Err.Number <> 0) Or (m_objScript Is Nothing) Then m_booScriptLoadFailed = True
        End If
    End If
    Set Script = m_objScript
    Err.Clear
End Property

Public Property Get ScriptOverride() As Boolean
Attribute ScriptOverride.VB_Description = "If True, the sprite's Template will not override its Script."
    ScriptOverride = m_booScriptOverride
End Property

Public Property Let ScriptOverride(ByVal NewValue As Boolean)
    m_booScriptOverride = NewValue
End Property

Public Property Get ScriptSource() As String
Attribute ScriptSource.VB_Description = "*The source code of the sprite's script (if any)."
    ScriptSource = m_strScript
End Property

Public Property Let ScriptSource(ByRef NewValue As String)
    m_strScript = NewValue
End Property

Friend Sub SetAttributes(ByVal Index As Long, ByVal Layer As Long, ByVal Sprites As Fury2Sprites, ByVal Map As Fury2Map)
On Error Resume Next
    m_spParam.Index = Index
    m_lngLayer = Layer
    Set m_sprSprites = Sprites
    Set m_mapMap = Map
End Sub

Friend Sub SetListAttributes(ByVal NextEntry As Long)
On Error Resume Next
    m_spParam.Next = NextEntry
    m_spParam.SortedNext = NextEntry
End Sub

Friend Sub SetParam(ByRef Param As SpriteParam)
On Error Resume Next
    m_spParam = Param
    m_spParam.Index = 0
    m_spParam.Next = 0
    m_spParam.SortedNext = 0
End Sub

Friend Function GetParam() As SpriteParam
On Error Resume Next
    GetParam = m_spParam
End Function

Friend Sub SetPose(ByVal Pose As Fury2Pose)
On Error Resume Next
Dim l_lngPose As Long
    l_lngPose = m_posPoses.Find(Pose)
    If l_lngPose >= 1 Then Pose = l_lngPose
End Sub

Public Property Get Solid() As Boolean
Attribute Solid.VB_Description = "If True, the sprite can be obstructed by map blocking and other Solid sprites."
    Solid = m_spParam.Stats.Solid
End Property

Public Property Let Solid(ByVal NewValue As Boolean)
    m_spParam.Stats.Solid = CByte(Abs(NewValue))
End Property

Public Property Get ZHeight() As Single
    ZHeight = m_spParam.ZHeight
End Property

Public Property Let ZHeight(ByVal NewValue As Single)
    m_spParam.ZHeight = NewValue
End Property

Public Property Get SpinTarget() As Single
Attribute SpinTarget.VB_Description = "*The target angle for a spin."
    SpinTarget = m_spParam.Velocity.BRT
End Property

Public Property Let SpinTarget(ByVal NewValue As Single)
    m_spParam.Velocity.BRT = NewValue
End Property

Public Property Get SpinVelocity() As Single
Attribute SpinVelocity.VB_Description = "*The speed at which a spin should occur."
    SpinVelocity = m_spParam.Velocity.BR
End Property

Public Property Let SpinVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.BR = NewValue
End Property

Public Property Get SpriteType() As Long
Attribute SpriteType.VB_Description = "The sprite's type."
    SpriteType = m_spParam.ProcessType
End Property

Public Property Let SpriteType(ByVal NewValue As Long)
    m_spParam.ProcessType = NewValue
End Property

Public Property Let State(ByVal NewValue As Long)
Attribute State.VB_Description = "The current state."
On Error Resume Next
    If NewValue = m_lngState Then Exit Property
    m_lngState = NewValue
    UpdateState
End Property

Public Property Get State() As Long
    State = m_lngState
End Property

Friend Property Set States(ByVal NewValue As Fury2States)
    Set m_staStates = NewValue
    UpdateState True
End Property

Public Property Get States() As Fury2States
Attribute States.VB_Description = "~The sprite's defined states."
    Set States = m_staStates
End Property

Public Sub Teleport(ByVal X, ByVal Y, Optional ByVal Z = 0, Optional ByVal Layer = -1)
On Error Resume Next
    If Layer <> -1 Then
        If Layer <> m_lngLayer Then
            m_mapMap.Layers(m_lngLayer).Sprites.MoveTo m_spParam.Index, CLng(Layer)
        End If
    End If
    m_spParam.Position.X = CSng(X)
    m_spParam.Position.Y = CSng(Y)
End Sub

Public Property Let Template(NewTemplate As String)
Attribute Template.VB_Description = "{Path}The template to sync the sprite with."
    m_strTemplate = NewTemplate
End Property

Public Property Get Template() As String
    Template = m_strTemplate
End Property

Public Sub Update()
On Error Resume Next
Dim l_sprCollided As Fury2Sprite
    If Ignore Then Exit Sub
    m_lngAIDelay = m_lngAIDelay + 1
    If m_lngAIDelay > m_lngAIUpdateRate Then
        m_lngAIDelay = 0
        If m_booScript_AIUpdate Then
            m_Engine.ContextLevelAdd "Sprite_AIUpdate"
            m_objScript.AIUpdate
            m_Engine.ContextLevelRemove
        End If
    End If

    If AttachedGraphic Is Nothing Then
    Else
        AttachedGraphic.Update
    End If
    
    'm_spParam.Params.SpecialFX = fxSoftShadow
    If m_booDisplayRotation Then
        m_spParam.Params.RotationAngle = m_spParam.Velocity.B
    End If
    
    If m_spParam.Events.Changed Or m_spParam.Events.Moved Then
        Dirty
    End If
    
    If m_spParam.Events.Moved Then
        UpdateAreas
    End If
    
    If SpinVelocity <> 0 Then
        ReselectPose
    End If
    
    UpdateAnimation
    
    UpdatePath
       
    ProfileStart "Sprite Scripts"
    If (CLng(m_spParam.Position.X) <> CLng(m_spOldParam.Position.X)) Or (CLng(m_spParam.Position.Y) <> CLng(m_spOldParam.Position.Y)) Then
        If m_booScript_Step Then
            m_Engine.ContextLevelAdd "Sprite_Step"
            Err.Clear
        
            m_objScript.Step
        
            Err.Clear
            m_Engine.ContextLevelRemove
        End If
    End If
    
    If m_spParam.Events.FadedOut Then
        m_Engine.ContextLevelAdd "Sprite_FadeOut"
        Err.Clear
    
        m_objScript.FadeOut
    
        Err.Clear
        m_Engine.ContextLevelRemove
    End If
    
    If m_spParam.Events.CollidedWith <> 0 Then
        m_Engine.ContextLevelAdd "Sprite_Hit"
        Err.Clear
        Set l_sprCollided = m_mapMap.Layers(m_lngLayer).Sprites(m_spParam.Events.CollidedWith)
    
        m_objScript.Hit l_sprCollided
    
        If Err <> 0 Then
            Err.Clear
        
            m_objScript.Hit
        
        End If
        m_Engine.ContextLevelRemove
    ElseIf m_spParam.Events.CollidedWithMap Then
        m_Engine.ContextLevelAdd "Sprite_HitMap"
        Err.Clear
    
        m_objScript.HitMap
    
        Err.Clear
        m_Engine.ContextLevelRemove
    End If
    ProfileStop "Sprite Scripts"
    
    ProfileStart "Attached Update / State Copy"
    If m_sprAttached Is Nothing Then
    Else
        With m_sprAttached
            m_spParam.Position.X = .X + m_sngAttachX
            m_spParam.Position.Y = .Y + m_sngAttachY
            Me.Angle = .Angle
            Me.State = .State
            Me.Velocity = .Velocity
            Me.Pose = .Pose
            Me.Frame = .Frame
            Me.Alpha = .Alpha
        End With
    End If
    
    m_spOldParam = m_spParam
    ProfileStop "Attached Update / State Copy"
End Sub

Private Sub UpdatePath()
On Error Resume Next
Dim l_sngPathDistance As Single
Dim l_lngRCX As Long, l_lngRCY As Long, l_lngRPX As Long, l_lngRPY As Long
    If m_pthPath.Count > 0 Then
        If State = IdleState Then State = WalkState
        ProfileStart "Sprite.UpdatePath"
        With m_pthPath(1)
            l_sngPathDistance = m_Engine.DistanceBetween(m_spParam.Position.X, m_spParam.Position.Y, .X, .Y)
            l_lngRCX = CLng(m_spParam.Position.X)
            l_lngRCY = CLng(m_spParam.Position.Y)
            l_lngRPX = CLng(m_spOldParam.Position.X)
            l_lngRPY = CLng(m_spOldParam.Position.X)
            If (l_lngRPX = l_lngRCX) And (l_lngRPY = l_lngRCY) And (CLng(.X) = l_lngRCX) And (CLng(.Y) = l_lngRCY) Then
                m_spParam.Position.X = .X
                m_spParam.Position.Y = .Y
                m_pthPath.Completed
                If m_pthPath.Count = 0 Then
                    If State = WalkState Then State = IdleState
                    Velocity = 0
                End If
            Else
                If (Abs(l_sngPathDistance) >= (Velocity)) Then
                    Angle = m_Engine.AngleBetween(m_spParam.Position.X, m_spParam.Position.Y, .X, .Y)
                    Velocity = Velocity + m_Engine.VelocityDecay
                    If Velocity < 0 Then Velocity = 0
                    If Velocity > WalkSpeed Then Velocity = WalkSpeed
                    If Velocity > l_sngPathDistance Then Velocity = l_sngPathDistance
                Else
                    m_spParam.Position.X = .X
                    m_spParam.Position.Y = .Y
                    m_pthPath.Completed
                    If m_pthPath.Count = 0 Then
                        If State = WalkState Then State = IdleState
                        Velocity = 0
                    End If
                End If
            End If
        End With
        ProfileStop "Sprite.UpdatePath"
    Else
    End If
End Sub

Private Sub UpdateAnimation()
On Error Resume Next
Dim l_lngOldFrame As Long
Dim l_lngPrePose As Long, l_lngPreFrame As Long, l_lngPreState As Long
    ProfileStart "Sprite.UpdateAnimation"
    l_lngOldFrame = m_lngFrame
    If m_fraFrame Is Nothing Then
    Else
        If (m_spParam.Events.Moved And m_booAnimateWhileMoving) Or ((Not m_spParam.Events.Moved) And m_booAnimateWhileStopped) Then
            m_sngAnimationDelay = m_sngAnimationDelay + (1 * AnimationMultiplier)
            If m_posPose.Frames.Count < 2 Then
                m_sngAnimationDelay = 0
            End If
            If m_sngAnimationDelay >= m_fraFrame.Delay Then
                m_sngAnimationDelay = m_sngAnimationDelay - m_fraFrame.Delay
                l_lngPreFrame = m_lngFrame
                l_lngPrePose = m_lngPose
                l_lngPreState = m_lngState
                m_posPose.Frames(m_lngFrame).RunScript Me
                If (l_lngPreFrame <> m_lngFrame) Or (l_lngPrePose <> m_lngPose) Or (l_lngPreState <> m_lngState) Then
                    m_Engine.Cameras.Dirty
                    ProfileStop "Sprite.UpdateAnimation"
                    Exit Sub
                End If
                m_lngFrame = m_lngFrame + m_lngAnimationDirection
                If m_posPose.Frames.Count = 1 Then
                    m_lngFrame = 1
                Else
                    Select Case m_posPose.LoopMode
                    Case F2LM_OneShot
                        If m_lngFrame > m_posPose.Frames.Count Then
                            m_lngFrame = m_posPose.Frames.Count
                        ElseIf m_lngFrame < 2 Then
                            m_lngFrame = 2
                        End If
                    Case F2LM_Restart
                        If m_lngFrame > m_posPose.Frames.Count Then
                            m_lngFrame = 2
                        ElseIf m_lngFrame < 2 Then
                            m_lngFrame = m_posPose.Frames.Count
                        End If
                    Case F2LM_PingPong
                        If m_lngFrame > m_posPose.Frames.Count Then
                            m_lngFrame = m_posPose.Frames.Count - 1
                            m_lngAnimationDirection = -m_lngAnimationDirection
                        ElseIf m_lngFrame < 2 Then
                            m_lngFrame = 3
                            m_lngAnimationDirection = -m_lngAnimationDirection
                        End If
                    Case Else
                    End Select
                End If
                UpdateFrame
            End If
        Else
            m_sngAnimationDelay = 0
            If Frame <> 1 Then
                Frame = 1
            End If
        End If
    End If
    If m_lngFrame <> l_lngOldFrame Then
        m_Engine.Cameras.Dirty
    End If
    ProfileStop "Sprite.UpdateAnimation"
End Sub

Private Sub UpdateAreas()
On Error Resume Next
Dim l_rctCurrent As Rectangle, l_rctOld As Rectangle
Dim l_araArea As Fury2Area, l_booWasIn As Boolean, l_booIsIn As Boolean
    If m_booAreasUpdated Then Exit Sub
    m_booAreasUpdated = True
    ProfileStart "Sprite.UpdateAreas"
    With l_rctCurrent
        .Left = CLng(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2))
        .Top = CLng(m_spParam.Position.Y - (m_spParam.Obstruction.Height))
        .Width = m_spParam.Obstruction.Width
        .Height = m_spParam.Obstruction.Height
    End With
    With l_rctOld
        .Left = CLng(m_spOldParam.Position.X - (m_spOldParam.Obstruction.Width / 2))
        .Top = CLng(m_spOldParam.Position.Y - (m_spOldParam.Obstruction.Height))
        .Width = m_spOldParam.Obstruction.Width
        .Height = m_spOldParam.Obstruction.Height
    End With
    For Each l_araArea In m_mapMap.Areas
        l_araArea.DualIntersect l_rctOld, l_rctCurrent, l_booWasIn, l_booIsIn
        
        If (Not l_booWasIn) And (l_booIsIn) Then
            ' Enter
            ProfileStart "Area Scripts"
            l_araArea.Action Me, "Enter"
            ProfileStop "Area Scripts"
        ElseIf (l_booWasIn) And (l_booIsIn) Then
            ' Move
            ProfileStart "Area Scripts"
            l_araArea.Action Me, "Move"
            ProfileStop "Area Scripts"
        ElseIf (l_booWasIn) And (Not l_booIsIn) Then
            ' Leave
            ProfileStart "Area Scripts"
            l_araArea.Action Me, "Leave"
            ProfileStop "Area Scripts"
        End If
        
        Err.Clear
    Next l_araArea
    m_booAreasUpdated = False
    ProfileStop "Sprite.UpdateAreas"
End Sub

Private Sub InitAreas()
On Error Resume Next
Dim l_rctCurrent As Rectangle
Dim l_araArea As Fury2Area
Dim l_booInside As Boolean
    With l_rctCurrent
        .Left = CLng(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2))
        .Top = CLng(m_spParam.Position.Y - (m_spParam.Obstruction.Height))
        .Width = m_spParam.Obstruction.Width
        .Height = m_spParam.Obstruction.Height
    End With
    For Each l_araArea In m_mapMap.Areas
        l_araArea.DualIntersect l_rctCurrent, l_rctCurrent, l_booInside, l_booInside
        
        If (l_booInside) Then
            ' Enter
            ProfileStart "Area Scripts"
            l_araArea.Action Me, "Enter"
            ProfileStop "Area Scripts"
        End If
        
        Err.Clear
    Next l_araArea
End Sub

Private Sub UpdateFrame(Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame
    If m_posPose Is Nothing Then
        Set l_fraFrame = Nothing
    Else
        m_lngFrame = ClipValue(m_lngFrame, 1, m_posPose.Frames.Count)
        Set l_fraFrame = m_posPose.Frames(m_lngFrame)
    End If
    If (l_fraFrame Is m_fraFrame) And (Not Force) Then
    Else
        Set m_fraFrame = l_fraFrame
        UpdateGraphic
        Dirty
    End If
End Sub

Private Sub UpdateGraphic()
On Error Resume Next
    If m_fraFrame Is Nothing Then Exit Sub
    With m_spParam.Graphic
        If m_fraFrame.Rectangle Is Nothing Then
        Else
            .Rectangle = m_fraFrame.Rectangle.GetRectangle
        End If
        .MaskColor = m_fraFrame.MaskColor
        .XCenter = m_fraFrame.XCenter
        .YCenter = m_fraFrame.YCenter
        If m_fraFrame.Image Is Nothing Then
        Else
            .pImage = m_fraFrame.Image.Handle
        End If
    End With
End Sub

Private Sub UpdateObstruction()
On Error Resume Next
    If m_posPose Is Nothing Then Exit Sub
    With m_spParam.Obstruction
        .Width = m_posPose.Blocking.Width
        .Height = m_posPose.Blocking.Height
    End With
End Sub

Private Sub UpdatePose(Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim l_posPose As Fury2Pose
    m_lngPose = ClipValue(m_lngPose, 1, m_posPoses.Count - m_staState.PoseOffset)
    Set l_posPose = m_posPoses(m_staState.PoseOffset + m_lngPose)
    If (l_posPose Is m_posPose) And (Not Force) Then
    Else
        Set m_posPose = l_posPose
        UpdateObstruction
        UpdateFrame Force
        Dirty
    End If
End Sub

Private Sub UpdateState(Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim l_staState As Fury2State
    m_lngState = ClipValue(m_lngState, 1, m_staStates.Count)
    Set l_staState = m_staStates(m_lngState)
    If (l_staState Is m_staState) And (Not Force) Then
    Else
        Set m_staState = l_staState
        UpdatePose Force
        Dirty
    End If
End Sub

Public Property Get Visible() As Boolean
Attribute Visible.VB_Description = "The sprite's visibility."
    Visible = m_spParam.Visible
End Property

Public Property Let Visible(NewVisible As Boolean)
    m_spParam.Visible = CByte(Abs(CLng(NewVisible)))
End Property

Public Property Get Velocity() As Single
Attribute Velocity.VB_Description = "*The sprite's current movement speed."
    Velocity = m_spParam.Velocity.V
End Property

Public Property Let Velocity(ByVal NewValue As Single)
    If m_spParam.Velocity.V > 0 Then
'    If NewValue < (m_spParam.Velocity.V / 2) Then Stop
    End If
    m_spParam.Velocity.V = NewValue
End Property

Public Property Get WalkSpeed() As Single
Attribute WalkSpeed.VB_Description = "The maximum speed for the sprite's movement. (This is an alias for Poses(Pose).WalkSpeed.)"
    WalkSpeed = m_staState.MaxVelocity
End Property

Public Property Let WalkSpeed(ByVal NewValue As Single)
    m_staState.MaxVelocity = NewValue
End Property

Public Property Get Weight() As Single
Attribute Weight.VB_Description = "The sprite's weight. Used if Pushable is True."
    Weight = m_spParam.Stats.Weight
End Property

Public Property Let Weight(ByVal NewValue As Single)
    m_spParam.Stats.Weight = NewValue
End Property

Public Property Get X() As Single
    X = m_spParam.Position.X
End Property

Public Property Let X(ByVal NewValue As Single)
    m_spParam.Position.X = NewValue
End Property

Public Property Get XLeft() As Single
    XLeft = m_spParam.Position.X - (m_spParam.Graphic.Rectangle.Width / 2)
End Property

Public Property Get XRight() As Single
    XRight = m_spParam.Position.X + (m_spParam.Graphic.Rectangle.Width / 2)
End Property

Public Property Get XCenter() As Single
    XCenter = m_spParam.Position.X
End Property

Public Property Get XVelocity() As Single
    XVelocity = m_spParam.Velocity.X
End Property

Public Property Let XVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.X = NewValue
End Property

Public Property Get Y() As Single
    Y = m_spParam.Position.Y
End Property

Public Property Let Y(ByVal NewValue As Single)
    m_spParam.Position.Y = NewValue
End Property

Public Property Get YCenter() As Single
    YCenter = m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height \ 2)
End Property

Public Property Get YTop() As Single
    YTop = m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height)
End Property

Public Property Get YVelocity() As Single
    YVelocity = m_spParam.Velocity.Y
End Property

Public Property Let YVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.Y = NewValue
End Property

Public Property Get Z() As Single
    Z = m_spParam.Position.Z
End Property

Public Property Let Z(ByVal NewValue As Single)
    m_spParam.Position.Z = NewValue
End Property

Public Property Get ZVelocity() As Single
    ZVelocity = m_spParam.Velocity.Z
End Property

Public Property Let ZVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.Z = NewValue
End Property
