VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Sprite_"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Description = "Fury² Sprite Object"
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

' Disable this class
#If Undefined Then

Implements Fury2Object
Implements IVirtualFileSaveable
Option Explicit

Public Dynamic As Boolean

Public AttachedGraphic As Fury2Graphic

Public Index As Long

Public Moved As Boolean

Public ScriptOverride As Boolean
Public Template As String

Public AnimationMultiplier As Single
Public VelocityMultiplier As Single

Public OverridePose As Boolean
Public OverrideDepth As Boolean

Public PauseWhileOffscreen As Boolean
Public RelativeAnimation As Boolean
Public RelativeVelocity As Boolean

Public TrueMovement As Boolean

Public Animating As Boolean
Public AnimateWhileMoving As Boolean

Public Entity As Boolean
Public TrackAsProjectile As Boolean
Public CanHitProjectiles As Boolean

Public Pushable As Boolean
Public Weight As Single

Public Velocity As Single
Public XVelocity As Single
Public YVelocity As Single
Public ZVelocity As Single
Public RotationVelocity As Single
Public AlphaVelocity As Single

Private mvarRotation As Single
Private mvarScale As Single
Private mvarAlpha As Single
Private mvarEffect As Long

Private mvarLoaded As Boolean
Private mvarParam As SpriteParam
Private mvarAttached As Fury2Sprite
Private mvarAttachX As Single, mvarAttachY As Single
Private mvarProjectileLastTouched As Fury2Sprite
Private mvarAI As Fury2AIBehaviors
Private mvarTouched As Long
Private mvarState As Long
Private mvarStates As Fury2States
Private mvarDelay As Single
Private mvarAngle As Single
Private mvarPose As Long
Private mvarFrame As Long
Private mvarFrameInc As Long
Private mvarWalkSpeed As Single
Private mvarX As Single
Private mvarY As Single
Private mvarOldX As Single, mvarOldY As Single, mvarOldZ As Single
Private mvarNewOldX As Single, mvarNewOldY As Single, mvarNewOldZ As Single
Private mvarPoses As Fury2Poses
Private mvarPath As Fury2Path
Private mvarLayer As Long
Private mvarZ As Single
Private mvarADest As Single
Private mvarSolid As Boolean
Private mvarName As String
Private mvarScript As String
Private mvarMap As Fury2Map
Private mvarObject As Object
Private mvarTrueMovement As Boolean
Private mvarScriptFailed As Boolean
Private mvarStopped As Boolean
Private mvarFollowPath As Boolean
Private mvarPathDelay As Long
Private mvarFollowing As Object
Private mvarPreviousX() As Long, mvarPreviousY() As Long
Private mvarPreviousA() As Single, mvarPreviousV() As Single
Private mvarHistoryLength As Long
Private mvarAIUpdateDelay As Long
Private mvarAIAccum As Long
Private mvarClass As Fury2SpriteClasses
Private mvarDecayTime As Single
Private mvarPathStartDistance As Single
Public IdleState As String
Public WalkState As String
Public RunState As String
Public JumpState As String

Private mvarScript_AIUpdate As Boolean
Private mvarScript_Hit As Boolean
Private mvarScript_HitMap As Boolean
Private mvarScript_Decay As Boolean
Private mvarScript_Update As Boolean
Private mvarScript_Activate As Boolean
Private mvarScript_Touched As Boolean
Private mvarScript_Load As Boolean
Private mvarScript_Die As Boolean
Private mvarScript_Step As Boolean
Private mvarScript_FadeOut As Boolean

Dim m_booTriggerTileEvent, m_booTriggerSpriteEvent As Boolean, m_sprEvent As Fury2Sprite, m_lngEdge As Long

Public Sub Clone(Sprite)
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Err.Clear
    Set l_sprSprite = Sprite
    If l_sprSprite Is Nothing Or Err <> 0 Then Exit Sub
    l_sprSprite.CopyTo Me
End Sub

Friend Sub CopyTo(Target As Fury2Sprite, Optional Template As Boolean)
On Error Resume Next
Dim m_objScript As Object
    With Target
        .Alpha = Me.Alpha
        .Effect = Me.Effect
        .Solid = Me.Solid
        .Class = Me.Class
        .TrackAsProjectile = Me.TrackAsProjectile
        .DecayTime = Me.DecayTime
        .Animating = Me.Animating
        .AnimateWhileMoving = Me.AnimateWhileMoving
        .RelativeAnimation = Me.RelativeAnimation
        .WalkSpeed = Me.WalkSpeed
        .AnimateDirection = Me.AnimateDirection
        If Template Then
            If Trim(.Name) = "" Then
                .Name = Me.Name
            End If
        Else
            .Name = Me.Name
        End If
        .Pushable = Me.Pushable
        .RotationVelocity = Me.RotationVelocity
        .ScaleLevel = Me.ScaleLevel
        .ScriptSource = Me.ScriptSource
        .ScaleVelocity = Me.ScaleVelocity
        .OverrideDepth = Me.OverrideDepth
        .OverridePose = Me.OverridePose
        .TrueMovement = Me.TrueMovement
        .Velocity = Me.Velocity
        .AIUpdateDelay = Me.AIUpdateDelay
        If Not Template Then
            .X = Me.X
            .Y = Me.Y
            .Z = Me.Z
        End If
        .XVelocity = Me.XVelocity
        .YVelocity = Me.YVelocity
        .ZVelocity = Me.ZVelocity
        .RotationVelocity = Me.RotationVelocity
        .AlphaVelocity = Me.AlphaVelocity
        Set .Poses = Me.Poses.Duplicate
        Set .States = Me.States.Duplicate
        If Not Template Then
            .State = Me.State
            .Angle = Me.Angle
            .Pose = Me.Pose
            .Frame = Me.Frame
        End If
        Set .AI = Me.AI.Duplicate
        .WalkState = Me.WalkState
        .IdleState = Me.IdleState
        .Entity = Me.Entity
        .Color = Me.Color
        If Not Template Then
            .ScriptOverride = Me.ScriptOverride
            .Template = Me.Template
        End If
        .Refresh
    End With
End Sub

Private Function EditMode() As Boolean
On Error Resume Next
    If mvarMap Is Nothing Then Exit Function
    EditMode = mvarMap.EditMode
    Err.Clear
End Function

Public Property Get Color() As Long
On Error Resume Next
    Color = mvarParam.Params.Color
End Property

Public Property Let Color(ByVal NewColor As Long)
On Error Resume Next
    mvarParam.Params.Color = NewColor
End Property

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
    Err.Clear
    Set Rectangle = F2Rect(mvarX - (Width / 2), mvarY - (Height), mvarX + (Width / 2), mvarY, True)
    If Err <> 0 Then
        Set Rectangle = F2Rect(0, 0, 0, 0)
    End If
End Property

Public Property Get ScreenRectangle() As Fury2Rect
On Error Resume Next
    With mvarPoses(mvarPose).Frames(mvarFrame)
        Err.Clear
        Set ScreenRectangle = F2Rect(mvarX - Ceil(.Rectangle.Width / 2), mvarY - Ceil(.Rectangle.Height), mvarX + Ceil(.Rectangle.Width / 2), mvarY)
        If Err <> 0 Then
            Set ScreenRectangle = F2Rect(0, 0, 0, 0)
        End If
    End With
End Property

Public Sub AttachGraphic(Graphic)
On Error Resume Next
Dim l_gphGraphic As Fury2Graphic
    Set l_gphGraphic = Graphic
    Set AttachedGraphic = l_gphGraphic
    RefreshAttachedGraphic
    Err.Clear
End Sub

Public Sub ReplaceScript(NewObject, Optional ImplementedEvents As String = "Activate Touched Load Die")
On Error Resume Next
Dim l_varEvents As Variant, l_lngEvents As Long
    Set mvarObject = NewObject
    l_varEvents = Split(ImplementedEvents)
    mvarScript_AIUpdate = False
    mvarScript_Hit = False
    mvarScript_HitMap = False
    mvarScript_Decay = False
    mvarScript_Update = False
    mvarScript_Activate = False
    mvarScript_Touched = False
    mvarScript_Load = False
    mvarScript_Die = False
    mvarScript_Step = False
    mvarScript_FadeOut = False
    For l_lngEvents = LBound(l_varEvents) To UBound(l_varEvents)
        Select Case LCase(Trim(l_varEvents(l_lngEvents)))
        Case "aiupdate"
            mvarScript_AIUpdate = True
        Case "hit"
            mvarScript_Hit = True
        Case "HitMap"
            mvarScript_HitMap = True
        Case "decay"
            mvarScript_Decay = True
        Case "update"
            mvarScript_Update = True
        Case "activate"
            mvarScript_Activate = True
        Case "touched"
            mvarScript_Touched = True
        Case "load"
            mvarScript_Load = True
        Case "die"
            mvarScript_Die = True
        Case "step"
            mvarScript_Step = True
        Case "fadeout"
            mvarScript_FadeOut = True
        Case Else
        End Select
    Next l_lngEvents
End Sub

Public Sub SetState(Name)
On Error Resume Next
Dim m_lngStates As Long
    If Trim(Name) = "" Then Exit Sub
    If mvarStates.Count <= 0 Then Exit Sub
    For m_lngStates = 1 To mvarStates.Count
        If LCase(Trim(mvarStates(m_lngStates).Name)) = LCase(Trim(Name)) Then
            If m_lngStates = mvarState Then Exit Sub
            Me.State = m_lngStates
            Exit For
        End If
    Next m_lngStates
End Sub

Public Property Get Attachees() As Fury2Sprites
Attribute Attachees.VB_MemberFlags = "40"
End Property

Public Property Set Attachees(RHS As Fury2Sprites)
End Property

Function ParamPointer() As Long
On Error Resume Next
    ParamPointer = VarPtr(mvarParam)
End Function

Friend Sub SetAttributes(Layer As Long, Index As Long)
On Error Resume Next
    If Layer = -1 Then
    Else
        Me.Layer = Layer
    End If
    Me.Index = Index
    mvarParam.Index = Index
End Sub

Friend Sub SetNextListEntry(ByVal pNext As Long)
On Error Resume Next
    mvarParam.Next = pNext
End Sub

Sub Refresh()
On Error Resume Next
    If Not mvarLoaded Then LoadGFX
    With mvarParam
        RefreshPosition
        RefreshOptions
        RefreshObstruction
        RefreshGraphic
        RefreshAttachedGraphic
    End With
    Dirty
    Err.Clear
End Sub

Sub RefreshAttachedGraphic()
On Error Resume Next
    With mvarParam
        If AttachedGraphic Is Nothing Then
            .AttachedGraphic = 0
        Else
            AttachedGraphic.Refresh
            .AttachedGraphic = AttachedGraphic.Handle
        End If
    End With
End Sub

Sub RefreshOptions()
On Error Resume Next
    With mvarParam
        .Stats.Solid = CByte(Abs(mvarSolid))
        With .Params
            .Alpha = mvarAlpha
            .ScaleLevel = mvarScale
            .BlitMode = mvarEffect
            .SpecialFX = CByte(Abs(m_Engine.SmoothScaling))
        End With
        .Index = Index
        .Type = CByte(mvarClass)
    End With
    Dirty
End Sub

Sub RefreshPosition()
On Error Resume Next
    With mvarParam
        With .Position
            .X = mvarX
            .Y = mvarY - mvarZ
        End With
    End With
    Dirty
    Err.Clear
End Sub

Sub RefreshObstruction()
On Error Resume Next
Dim l_blkBlocking As Fury2Blocking
    If mvarPoses Is Nothing Then Exit Sub
    If mvarPose < 1 Then Exit Sub
    Set l_blkBlocking = mvarPoses(mvarPose).Blocking
    With mvarParam
        With .Obstruction
            .Width = l_blkBlocking.Width * IIf(m_Engine.ScaleBlocking, mvarScale, 1)
            .Height = l_blkBlocking.Height * IIf(m_Engine.ScaleBlocking, mvarScale, 1)
        End With
    End With
    Err.Clear
End Sub

Sub RefreshGraphic()
On Error Resume Next
Dim m_lngX1 As Long, m_lngY1 As Long, m_lngX2 As Long, m_lngY2 As Long
    If mvarPoses Is Nothing Then Exit Sub
    If mvarPose < 1 Then Exit Sub
    If mvarPoses(mvarPose).Frames Is Nothing Then Exit Sub
    If mvarFrame < 1 Then Exit Sub
    With mvarPoses(mvarPose).Frames(mvarFrame)
        mvarParam.Graphic.XCenter = .XCenter
        mvarParam.Graphic.YCenter = .YCenter
        mvarParam.Graphic.MaskColor = .MaskColor
        If .Image Is Nothing Then
            mvarParam.Graphic.pImage = 0
        Else
            mvarParam.Graphic.pImage = .Image.Handle
        End If
        mvarParam.Graphic.Rectangle = .Rectangle.GetRectangle
    End With
    Dirty
    Err.Clear
End Sub

Sub Free()
Attribute Free.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngPoses As Long
    If mvarPoses.Count > 0 Then
        For m_lngPoses = 1 To mvarPoses.Count
            With mvarPoses(m_lngPoses)
                .SetParent Me
                .Free
            End With
        Next m_lngPoses
    End If
    Erase mvarPreviousX()
    Erase mvarPreviousY()
    Erase mvarPreviousA()
    Erase mvarPreviousV()
    Set mvarProjectileLastTouched = Nothing
    Set mvarAttached = Nothing
    Set mvarStates = New Fury2States
    mvarAI.Reset
End Sub

Sub Face(Obj)
Attribute Face.VB_Description = "Makes the sprite face another sprite."
On Error Resume Next
    If TypeOf Obj Is Fury2Sprite Then
        mvarAngle = m_Engine.AngleBetween(mvarX, mvarY, Obj.X, Obj.Y)
        Dirty
    End If
End Sub


Public Function Class_Options() As Variant
Attribute Class_Options.VB_MemberFlags = "40"
    Class_Options = Array("Normal", "Party Member", "NPC", "Monster", "Object", "Projectile", "Effect", "Item", "Sign")
End Function

Public Property Get Attached() As Fury2Sprite
Attribute Attached.VB_Description = "The sprite this sprite is attached to (if any)."
    Set Attached = mvarAttached
End Property

Sub Attach(Obj)
Attribute Attach.VB_Description = "Attaches the sprite to another sprite."
On Error Resume Next
    Set mvarAttached = Obj
End Sub

Sub Detach()
Attribute Detach.VB_Description = "If the sprite is attached to another sprite, this command detaches it."
On Error Resume Next
    Set mvarAttached = Nothing
End Sub

Sub Render(Surface As Fury2Image, XOffset As Long, YOffset As Long)
Attribute Render.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngOld As Long
Dim m_camParam As CameraParam
    Refresh
    m_lngOld = mvarParam.Next
    mvarParam.Next = 0
    With m_camParam
        .Rectangle = Surface.Rectangle.GetRectangle
        .pImage = Surface.Handle
        .ViewportX = XOffset
        .ViewportY = YOffset
        .Alpha = 255
    End With
    SoftFX.RenderSprites VarPtr(mvarParam), m_camParam
    mvarParam.Next = m_lngOld
End Sub

Public Property Get DecayTime() As Long
Attribute DecayTime.VB_Description = "The amount of updates before the Decay script event is fired (for projectiles)."
On Error Resume Next
    DecayTime = mvarDecayTime
End Property

Public Property Let DecayTime(NewDecayTime As Long)
On Error Resume Next
    mvarDecayTime = NewDecayTime
End Property

Public Property Get ScaleVelocity() As Boolean
On Error Resume Next
    ScaleVelocity = RelativeVelocity
End Property

Public Property Let ScaleVelocity(NewValue As Boolean)
    RelativeVelocity = NewValue
End Property

Public Property Get AnimateDirection() As Long
Attribute AnimateDirection.VB_MemberFlags = "40"
On Error Resume Next
    AnimateDirection = mvarFrameInc
End Property

Public Property Let AnimateDirection(NewDirection As Long)
On Error Resume Next
    mvarFrameInc = NewDirection
End Property

Public Function Width() As Single
Attribute Width.VB_Description = "The (virtual) width of the sprite."
On Error Resume Next
    Width = mvarPoses(mvarPose).Blocking.Width * IIf(m_Engine.ScaleBlocking, mvarScale, 1)
End Function

Public Function Height() As Single
Attribute Height.VB_Description = "The (virtual) height of the sprite."
On Error Resume Next
    Height = mvarPoses(mvarPose).Blocking.Height * IIf(m_Engine.ScaleBlocking, mvarScale, 1)
End Function

Public Property Get Class() As Fury2SpriteClasses
Attribute Class.VB_Description = "The 'class' or type of the sprite."
    Class = mvarClass
End Property

Public Property Let Class(NewClass As Fury2SpriteClasses)
    mvarClass = NewClass
End Property

Public Property Get AI() As Fury2AIBehaviors
Attribute AI.VB_Description = "The sprite's AI Behavior chain."
    Set AI = mvarAI
End Property

Public Property Set AI(NewAI As Fury2AIBehaviors)
    Set mvarAI = NewAI
End Property

Friend Sub Touched(Toucher As Fury2Sprite)
On Error Resume Next
    If mvarScript_Touched Then
        If mvarTouched > 0 Then Exit Sub
        mvarTouched = mvarTouched + m_Engine.TouchDelay
        m_Engine.ContextLevelAdd "Sprite_Touched"
        Err.Clear
        Script.Touched Toucher
        If Err.Number <> 0 Then
            Err.Clear
            Script.Touched
        End If
        m_Engine.ContextLevelRemove
        Err.Clear
    End If
End Sub

Public Property Get Map() As Fury2Map
    Set Map = mvarMap
End Property

Public Sub Dirty()
Attribute Dirty.VB_MemberFlags = "40"
On Error Resume Next
Dim m_rctOnscreen As Win32.RECT
    m_Engine.Cameras.Dirty
'    m_rctOnscreen = GetRectOnscreen
'    m_Engine.Cameras.AddDirtyRect CLng(mvarX + m_rctOnscreen.left), CLng(mvarY + m_rctOnscreen.top), CLng(mvarX + m_rctOnscreen.right), CLng(mvarY + m_rctOnscreen.bottom), True
End Sub

Public Sub SetParent(Parent As Fury2Map)
On Error Resume Next
    If Parent Is Nothing Then Exit Sub
    Set mvarMap = Parent
    If Err.Number <> 0 Then Err.Clear
End Sub

Public Property Get WalkSpeed() As Single
Attribute WalkSpeed.VB_Description = "The maximum speed of the sprite."
On Error Resume Next
    WalkSpeed = mvarWalkSpeed
End Property

Public Property Let WalkSpeed(NewSpeed As Single)
On Error Resume Next
    mvarWalkSpeed = NewSpeed
End Property

Public Sub Fade(FromAlpha, ToAlpha, Optional Speed As Single = 3.33333333333333E-02)
Attribute Fade.VB_Description = "Makes the sprite fade in or out."
On Error Resume Next
    mvarAlpha = CSng(FromAlpha)
    mvarADest = CSng(ToAlpha)
    If mvarAlpha > 1 Then mvarAlpha = 1
    If mvarAlpha < -1 Then mvarAlpha = -1
    If mvarADest > 1 Then mvarADest = 1
    If mvarADest < -1 Then mvarADest = -1
    If mvarADest < mvarAlpha Then
        AlphaVelocity = -Speed
    Else
        AlphaVelocity = Speed
    End If
    Dirty
End Sub

Public Function TruePose() As Long
Attribute TruePose.VB_MemberFlags = "40"
On Error Resume Next
    If mvarState <> 0 Then
        TruePose = mvarPose
    Else
        TruePose = mvarPose
    End If
End Function

Public Property Get Leader() As Object
On Error Resume Next
    Set Leader = mvarFollowing
End Property

Public Property Set Leader(NewLeader As Object)
On Error Resume Next
    Set mvarFollowing = NewLeader
End Property

Public Sub Die()
Attribute Die.VB_Description = "Removes the sprite from the map, and destroys it."
On Error Resume Next
Dim m_lngLayers As Long
Dim m_lngSprites As Long
    Dirty
    m_Engine.ContextLevelAdd "Die"
    Script.Die
    If mvarMap.Layers.Count > 0 Then
        For m_lngLayers = 1 To mvarMap.Layers.Count
            mvarMap.Layers(m_lngLayers).Sprites.Remove Me
        Next
    End If
    m_Engine.ContextLevelRemove
    Err.Clear
End Sub

Public Property Get FollowPath() As Boolean
Attribute FollowPath.VB_Description = "This flag controls whether the sprite automatically follows path waypoints."
    FollowPath = mvarFollowPath
End Property

Public Property Let FollowPath(NewFollow As Boolean)
    If mvarFollowPath = True And NewFollow = False Then Velocity = 0
    mvarFollowPath = NewFollow
End Property

Public Property Get IsOnscreen() As Boolean
Attribute IsOnscreen.VB_Description = "Returns true if the sprite is currently visible (Not scrolled offscreen)."
Dim m_lngCameras As Long
    IsOnscreen = True
    If m_Engine.Cameras.Count > 0 Then
        For m_lngCameras = 1 To m_Engine.Cameras.Count
            With m_Engine.Cameras(m_lngCameras)
                IsOnscreen = IsOnscreen And ((mvarX >= .ViewportX) And (mvarY >= .ViewportY) And (mvarX < (.ViewportX + .Width)) And (mvarY < (.ViewportY + .Height)))
            End With
        Next m_lngCameras
    End If
End Property

Public Property Get ScaleLevel() As Single
Attribute ScaleLevel.VB_Description = "The scale (size) of the sprite. (~0.0 - ~3.0)"
    ScaleLevel = mvarScale
End Property

Public Property Let ScaleLevel(NewScaleLevel As Single)
    Dirty
    mvarScale = NewScaleLevel
    If mvarScale < 0.01 Then mvarScale = 0.01
    If mvarScale > 25 Then mvarScale = 25
    Dirty
End Property

Public Function Duplicate() As Fury2Sprite
Attribute Duplicate.VB_Description = "Creates a copy of the sprite."
On Error Resume Next
    Set Duplicate = New Fury2Sprite
    CopyTo Duplicate
    Err.Clear
End Function

Public Function XAhead(Distance) As Single
Attribute XAhead.VB_MemberFlags = "40"
On Error Resume Next
Dim m_sngAngle As Single, m_sngXVel As Single
        If mvarTrueMovement = True Then
            m_sngAngle = mvarAngle * c_dblRadian
            m_sngXVel = Sin(m_sngAngle) * Distance
        Else
            m_sngXVel = mvarPoses(TruePose).XVelocity * Distance
        End If
        XAhead = mvarX + m_sngXVel
End Function

Public Function YAhead(Distance) As Single
Attribute YAhead.VB_MemberFlags = "40"
On Error Resume Next
Dim m_sngAngle As Single, m_sngYVel As Single
        If mvarTrueMovement = True Then
            m_sngAngle = mvarAngle * c_dblRadian
            m_sngYVel = -Cos(m_sngAngle) * Distance
        Else
            m_sngYVel = mvarPoses(TruePose).YVelocity * Distance
        End If
        YAhead = mvarY + m_sngYVel
End Function

Friend Property Get Script_Update() As Boolean
On Error Resume Next
    Script_Update = mvarScript_Update
End Property

Friend Property Get Script_Touched() As Boolean
On Error Resume Next
    Script_Touched = mvarScript_Touched
End Property

Public Property Get YCenter() As Single
Attribute YCenter.VB_MemberFlags = "40"
On Error Resume Next
    YCenter = mvarY
End Property

Public Property Get Script() As Object
Attribute Script.VB_Description = "The sprite's script."
On Error Resume Next
Dim SpriteName As String
Dim l_strScript As String
    Err.Clear
    If Trim(mvarScript) = "" Then Exit Property
    If mvarObject Is Nothing Then
        If (mvarScriptFailed = False) Then
            Err.Clear
            m_Engine.SpriteObjectCount = m_Engine.SpriteObjectCount + 1
            If Dynamic Then
                SpriteName = m_Engine.VariableName(Name + "_S" & m_Engine.SpriteObjectCount)
            Else
                SpriteName = m_Engine.VariableName(Name + "_S" & Index & Layer)
            End If
            With m_Engine.ScriptEngine.Language
                l_strScript = .GenerateClassVariable("Object") & .LineTerminator & _
                .GenerateClassVariable("Map") & .LineTerminator & _
                .GenerateClassMethod("F2OInit", Array("T", "M"), _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "T", True) & .LineTerminator & _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "M", True) _
                ) & .LineTerminator & mvarScript
                mvarScript_Update = .ImplementsMethod(l_strScript, "Update")
                mvarScript_Hit = .ImplementsMethod(l_strScript, "Hit")
                mvarScript_HitMap = .ImplementsMethod(l_strScript, "HitMap")
                mvarScript_Decay = .ImplementsMethod(l_strScript, "Decay")
                mvarScript_AIUpdate = .ImplementsMethod(l_strScript, "AIUpdate")
                mvarScript_Activate = .ImplementsMethod(l_strScript, "Activate")
                mvarScript_Touched = .ImplementsMethod(l_strScript, "Touched")
                mvarScript_Load = .ImplementsMethod(l_strScript, "Load")
                mvarScript_Die = .ImplementsMethod(l_strScript, "Die")
                mvarScript_Step = .ImplementsMethod(l_strScript, "Step")
                mvarScript_FadeOut = .ImplementsMethod(l_strScript, "FadeOut")
                m_Engine.CacheScript SpriteName, .GenerateClass(SpriteName, l_strScript)
                m_Engine.ScriptEngine.AddCode .GenerateAssignment("New" & SpriteName, .GenerateInstantiation(SpriteName), True)
                Set mvarObject = m_Engine.ScriptEngine.Eval("New" & SpriteName)
            End With
            Set m_Engine.IObjThis = Me
            mvarObject.F2OInit Me, mvarMap
            mvarObject.Initialize
            mvarObject.Init
            If (Err.Number <> 0) Or (mvarObject Is Nothing) Then mvarScriptFailed = True
        End If
    End If
    Set Script = mvarObject
    Err.Clear
End Property

Public Property Get ScriptSource() As String
Attribute ScriptSource.VB_Description = "The source code to the sprite's script."
    ScriptSource = mvarScript
End Property

Public Property Let ScriptSource(NewScript As String)
    mvarScript = NewScript
End Property

Friend Property Get GetRect() As Win32.RECT
On Error Resume Next
    With GetRect
        .Left = -(Width / 2)
        .Right = (Width / 2)
        .Top = -(Height - 1)
        .Bottom = 0
    End With
End Property

Friend Property Get GetFRect() As FRect
On Error Resume Next
Dim l_sngW As Single
    l_sngW = (Width / 2)
    With GetFRect
        .x1 = mvarX - l_sngW
        .x2 = mvarX + l_sngW
        .y1 = mvarY - Height
        .y2 = mvarY
    End With
End Property

Friend Property Get GetRectOnscreen() As Win32.RECT
On Error Resume Next
    With mvarPoses(mvarPose).Frames(mvarFrame)
        GetRectOnscreen.Left = (-(((.Rectangle.Width / 2)) * mvarScale)) - (1!)
        GetRectOnscreen.Right = ((((.Rectangle.Width / 2)) * mvarScale)) + (1!)
        GetRectOnscreen.Top = ((-(.Rectangle.Height) * mvarScale) - 1!) + (.Rectangle.Height - .YCenter)
        GetRectOnscreen.Bottom = 1! + (.Rectangle.Height - .YCenter)
    End With
End Property

Public Property Get IsBlockedByTiles() As Boolean
Attribute IsBlockedByTiles.VB_Description = "Returns the tile blocking state of the sprite."
On Error Resume Next
Dim m_rctRect As Win32.RECT
    If mvarLayer < 1 Then Exit Property
    If Not mvarSolid Then IsBlockedByTiles = False
    Select Case mvarMap.CollisionType
    Case 0
        ' no collision checks
        IsBlockedByTiles = False
    Case 2
        ' vector collision checks
        m_rctRect = GetRect
        IsBlockedByTiles = mvarMap.Layers(mvarLayer).Sprites.LineCollisionCheck(Index, mvarMap.Layers(mvarLayer))
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Left), (mvarY - mvarZ) + CSng(m_rctRect.Bottom), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Right), (mvarY - mvarZ) + CSng(m_rctRect.Bottom), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Left), (mvarY - mvarZ) + CSng(m_rctRect.Top), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Right), (mvarY - mvarZ) + CSng(m_rctRect.Top), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX, mvarY, mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
    Case Else
        ' simple tile collision checks
        m_rctRect = GetRect
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX, mvarY, mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Left), (mvarY - mvarZ) + CSng(m_rctRect.Bottom), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Right), (mvarY - mvarZ) + CSng(m_rctRect.Bottom), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Left), (mvarY - mvarZ) + CSng(m_rctRect.Top), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
        IsBlockedByTiles = mvarMap.Collider.Blocked(mvarX + CSng(m_rctRect.Right), (mvarY - mvarZ) + CSng(m_rctRect.Top), mvarLayer)
        If IsBlockedByTiles = True Then Exit Property
    End Select
End Property

Public Property Get IsBlocked() As Boolean
Attribute IsBlocked.VB_Description = "Returns the blocking state of the sprite."
On Error Resume Next
Dim m_sprBlocked As Fury2Sprite
    If mvarLayer < 1 Then Exit Property
    If Not mvarSolid Then IsBlocked = False: Exit Function
    Set m_sprBlocked = IsBlockedBySprite
    If (m_sprBlocked Is Nothing) Then Else IsBlocked = True
    If IsBlocked = True Then Exit Property
    IsBlocked = IsBlockedByTiles
    If IsBlocked = True Then Exit Property
End Property

Function Action(Optional ByRef Activator As Fury2Sprite, Optional ByVal ActionName As String = "Activate") As Boolean
Attribute Action.VB_Description = "Tells the sprite to perform an action."
On Error Resume Next
    Action = True
    m_Engine.ContextLevelAdd "Sprite_" + ActionName
    If Script Is Nothing Then Else CallByName Script, ActionName, VbMethod, Activator
    If Err.Number <> 0 Then
        Err.Clear
        If Script Is Nothing Then Else CallByName Script, ActionName, VbMethod
        If Err.Number <> 0 Then Action = False
    End If
    m_Engine.ContextLevelRemove
End Function

Function Activate(Optional ByRef Activator As Fury2Sprite) As Boolean
Attribute Activate.VB_Description = "Activates the sprite."
On Error Resume Next
    If mvarScript_Activate = True Then
        Activate = True
        m_Engine.ContextLevelAdd "Sprite_Activate"
        If Script Is Nothing Then Else Script.Activate Activator
        If Err.Number <> 0 Then
            Err.Clear
            If Script Is Nothing Then Else Script.Activate
            If Err.Number <> 0 Then Activate = False
        End If
        m_Engine.ContextLevelRemove
    End If
End Function

Sub ActivateAhead(Optional Distance = -32767)
Attribute ActivateAhead.VB_Description = "Activates the sprite ahead of this one (if any)."
On Error Resume Next
Dim m_rctSource As Win32.RECT
Dim m_rctDest As Win32.RECT
Dim m_rctIntersect As Win32.RECT
Dim m_sngX As Single, m_sngY As Single
Dim m_sprSprite As Fury2Sprite
Dim m_araArea As Fury2Area
    Err.Clear
    If Distance = -32767 Then Distance = m_Engine.ActivationDistance
    Distance = CSng(Distance)
    m_rctSource = GetRect
    m_Engine.ContextLevelAdd "ActivateAhead"
    m_sngX = mvarX
    m_sngY = mvarY
    mvarX = XAhead(Distance)
    mvarY = YAhead(Distance)
    RefreshPosition
    With m_rctSource
        .Left = (XAhead(Distance)) - (Me.Width / 2)
        .Right = (XAhead(Distance)) + (Me.Width / 2)
        .Top = (YAhead(Distance)) - Me.Height
        .Bottom = (YAhead(Distance))
    End With
    Set m_sprSprite = Me.IsBlockedBySprite
    If m_sprSprite Is Nothing Then
    Else
        Err.Clear
        If m_sprSprite.Activate(Me) Then
            mvarX = m_sngX
            mvarY = m_sngY
            RefreshPosition
            Exit Sub
        End If
    End If
    For Each m_araArea In mvarMap.Areas
        With m_araArea
            m_rctDest = .Rectangle.GetRect
            If IntersectRect(m_rctIntersect, m_rctSource, m_rctDest) <> 0 Then
                If m_araArea.Activate(Me) Then
                    mvarX = m_sngX
                    mvarY = m_sngY
                    RefreshPosition
                    Exit Sub
                End If
            End If
        End With
    Next m_araArea
    m_Engine.ContextLevelRemove
    mvarX = m_sngX
    mvarY = m_sngY
    RefreshPosition
    Set m_sprSprite = Nothing
    Set m_araArea = Nothing
End Sub

Sub ActionAhead(Distance, Action)
Attribute ActionAhead.VB_Description = "Tells the sprite ahead of this one (if any) to perform an action."
On Error Resume Next
Dim m_rctSource As Win32.RECT
Dim m_rctDest As Win32.RECT
Dim m_rctIntersect As Win32.RECT
Dim m_sngX As Single, m_sngY As Single
Dim m_sprSprite As Fury2Sprite
Dim m_araArea As Fury2Area
    Err.Clear
    If Distance = -32767 Then Distance = m_Engine.ActivationDistance
    Distance = CSng(Distance)
    m_rctSource = GetRect
    m_Engine.ContextLevelAdd "ActivateAhead"
    m_sngX = mvarX
    m_sngY = mvarY
    mvarX = XAhead(Distance)
    mvarY = YAhead(Distance)
    RefreshPosition
    With m_rctSource
        .Left = (XAhead(Distance)) - (Me.Width / 2)
        .Right = (XAhead(Distance)) + (Me.Width / 2)
        .Top = (YAhead(Distance)) - Me.Height
        .Bottom = (YAhead(Distance))
    End With
    Set m_sprSprite = Me.IsBlockedBySprite
    If m_sprSprite Is Nothing Then
    Else
        Err.Clear
        If m_sprSprite.Action(Me, Action) Then
            mvarX = m_sngX
            mvarY = m_sngY
            RefreshPosition
            Exit Sub
        End If
    End If
    For Each m_araArea In mvarMap.Areas
        With m_araArea
            m_rctDest = .Rectangle.GetRect
            If IntersectRect(m_rctIntersect, m_rctSource, m_rctDest) <> 0 Then
                If m_araArea.Action(Me, Action) Then
                    mvarX = m_sngX
                    mvarY = m_sngY
                    RefreshPosition
                    Exit Sub
                End If
            End If
        End With
    Next m_araArea
    m_Engine.ContextLevelRemove
    mvarX = m_sngX
    mvarY = m_sngY
    RefreshPosition
    Set m_sprSprite = Nothing
    Set m_araArea = Nothing
End Sub

Public Property Get IsBlockedBySprite() As Fury2Sprite
Attribute IsBlockedBySprite.VB_Description = "If the sprite is blocked by another sprite, returns the sprite in the way."
On Error Resume Next
    If mvarLayer < 1 Then Exit Property
    Set IsBlockedBySprite = mvarMap.Layers(mvarLayer).Sprites.CollisionCheck(Index, True)
    If IsBlockedBySprite Is Me Then Set IsBlockedBySprite = Nothing
End Property

Public Property Get IsTouchingSprite(Optional ExcludeClass As Fury2SpriteClasses = -1) As Fury2Sprite
Attribute IsTouchingSprite.VB_Description = "If the sprite is touching another sprite, returns the sprite being touched."
On Error Resume Next
    If mvarLayer < 1 Then Exit Property
    Set IsTouchingSprite = mvarMap.Layers(mvarLayer).Sprites.CollisionCheck(Index, False, -1, ExcludeClass)
End Property

Public Property Get IsNearSprite(Distance As Single, Optional ExcludeClass As Fury2SpriteClasses = -1) As Fury2Sprite
Attribute IsNearSprite.VB_Description = "Calculates the distance between one sprite and one or more other sprites, and returns the nearest."
On Error Resume Next
Dim m_sprSprite As Fury2Sprite
Dim m_sngDistance As Single
    Set m_sprSprite = mvarMap.Layers(mvarLayer).Sprites.GetClosest(Index, False, -1, ExcludeClass, m_sngDistance)
    If m_sngDistance <= Distance Then Set IsNearSprite = m_sprSprite
'Dim m_sngDist As Single
'Dim m_lngChecked As Long
'Dim m_sngXDist As Single, m_sngYDist As Single
'Dim m_sngWidth As Single, m_sngHeight As Single
'    For Each m_sprSprite In mvarMap.Layers(mvarLayer).Sprites
'        If m_sprSprite Is Me Then
'        Else
'            With m_sprSprite
'                If .Class <> ExcludeClass Then
'                    m_sngXDist = Abs(mvarX - .X)
'                    m_sngYDist = Abs(mvarY - .Y)
'                    m_sngDist = IIf(m_sngXDist > m_sngYDist, m_sngXDist, m_sngYDist)
'                    If (m_sngDist <= Distance) Then
'                        Set IsNearSprite = m_sprSprite
'                        Set m_sprSprite = Nothing
'                    End If
'                End If
'            End With
'        End If
'    Next m_sprSprite
'    Set m_sprSprite = Nothing
End Property

Public Property Get Layer() As Long
Attribute Layer.VB_Description = "The index of the sprite's layer."
    Layer = mvarLayer
End Property

Public Property Let Layer(vData As Long)
On Error Resume Next
Dim m_lngLayers As Long
Dim m_lngSprites As Long
    If vData <> mvarLayer Then
        If mvarMap.Layers(mvarLayer) Is Nothing Then
        Else
            If mvarMap.Layers(mvarLayer).Sprites Is Nothing Then
            Else
                mvarMap.Layers(mvarLayer).Sprites.MoveTo Index, vData
            End If
        End If
        Dirty
    End If
    mvarLayer = CLng(vData)
End Property

Public Property Let Z(ByVal vData As Single)
Attribute Z.VB_Description = "The sprite's Z coordinate."
On Error Resume Next
    Dirty
    mvarZ = vData
    RefreshObstruction
    Dirty
End Property

Public Property Get Z() As Single
    Z = mvarZ
End Property

Public Property Get FollowX() As Single
On Error Resume Next
    If mvarHistoryLength < 0 Then
        FollowX = mvarX
    Else
        FollowX = mvarPreviousX(mvarHistoryLength)
    End If
End Property

Public Property Get FollowY() As Single
On Error Resume Next
    If mvarHistoryLength < 0 Then
        FollowY = mvarY
    Else
        FollowY = mvarPreviousY(mvarHistoryLength)
    End If
End Property

Public Sub AIUpdate()
Attribute AIUpdate.VB_MemberFlags = "40"
On Error Resume Next
    mvarAI.Update Me
    Err.Clear
End Sub

Friend Sub UpdateProjectile()
On Error Resume Next
Dim m_sprBlock As Fury2Sprite
    If PauseWhileOffscreen Then
        If Not CBool(mvarParam.Onscreen) Then
            Exit Sub
        End If
    End If
    If TrackAsProjectile Then
        If m_booTriggerSpriteEvent Then m_booTriggerTileEvent = False
        If mvarScript_HitMap Then
            If IsBlockedByTiles Or m_booTriggerTileEvent Then
                m_Engine.ContextLevelAdd "Sprite_HitMap"
                Err.Clear
                Script.HitMap
                Err.Clear
                m_Engine.ContextLevelRemove
            End If
        End If
        If mvarScript_Hit Then
            Set m_sprBlock = Nothing
            If m_Engine.RangedProjectileChecks Then
                If CanHitProjectiles Then
                    Set m_sprBlock = IsNearSprite((Width + Height) / 2)
                Else
                    Set m_sprBlock = IsNearSprite((Width + Height) / 2, F2ST_Projectile)
                End If
            Else
                If CanHitProjectiles Then
                    Set m_sprBlock = IsTouchingSprite()
                Else
                    Set m_sprBlock = IsTouchingSprite(F2ST_Projectile)
                End If
            End If
            If m_sprBlock Is Nothing And m_booTriggerSpriteEvent Then
                Set m_sprBlock = m_sprEvent
                Set m_sprEvent = Nothing
            End If
            If m_sprBlock Is Nothing Then
            ElseIf m_sprBlock Is mvarProjectileLastTouched Then
            Else
                m_Engine.ContextLevelAdd "Sprite_Hit"
                Err.Clear
                Script.Hit m_sprBlock
                If Err <> 0 Then
                    Err.Clear
                    Script.Hit
                End If
                m_Engine.ContextLevelRemove
            End If
            Set mvarProjectileLastTouched = m_sprBlock
            Err.Clear
        End If
        If mvarDecayTime > 0 Then
            mvarDecayTime = mvarDecayTime - (1 * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier))
            If mvarDecayTime <= 0 Then
                m_Engine.ContextLevelAdd "Sprite_Decay"
                Err.Clear
                Script.Decay
                Err.Clear
                m_Engine.ContextLevelRemove
            End If
        End If
    End If
    m_booTriggerTileEvent = False
    m_booTriggerSpriteEvent = False
End Sub

Friend Sub UpdateEntity()
On Error Resume Next
    UpdateLogic
    UpdatePath
    UpdatePushing
    UpdateMovement
    UpdateProjectile
    UpdateAnimation
    UpdateAreas
    UpdateMisc
End Sub

Friend Sub UpdateObject()
On Error Resume Next
    UpdatePushing
    UpdateMovement
    UpdateAnimation
    UpdateAreas
    UpdateMisc
End Sub

Friend Sub UpdateLogic()
On Error Resume Next
    If PauseWhileOffscreen Then
        If Not CBool(mvarParam.Onscreen) Then
            Exit Sub
        End If
    End If
    If mvarScript_Update = True Then
        m_Engine.ContextLevelAdd "Sprite_Update"
        If Script Is Nothing Then Else Script.Update
        m_Engine.ContextLevelRemove
    End If
    If (mvarAIUpdateDelay > 0) Then
        mvarAIAccum = mvarAIAccum + 1
        If mvarAIAccum >= mvarAIUpdateDelay Then
            mvarAIAccum = 0
            AIUpdate
        End If
    Else
        AIUpdate
    End If
End Sub

Friend Sub UpdateAreas()
On Error Resume Next
Dim m_lngAreas As Long
    If Moved Then
        If (Map.Areas.Count > 0) Then
            For m_lngAreas = 1 To Map.Areas.Count
                If Map.Areas(m_lngAreas) Is Nothing Then
                Else
                    With Map.Areas(m_lngAreas)
                        If ((mvarX) >= .X) And ((mvarX) <= (.X + .Width)) And ((mvarY - mvarZ) >= .Y) And ((mvarY - mvarZ) <= (.Y + .Height)) Then
                            If ((mvarOldX) >= .X) And ((mvarOldX) <= (.X + .Width)) And ((mvarOldY - mvarOldZ) >= .Y) And ((mvarOldY - mvarOldZ) <= (.Y + .Height)) Then
                                With Map.Areas(m_lngAreas)
                                    Err.Clear
                                    If .Script_Move Then
                                        m_Engine.ContextLevelAdd "Map.Areas(" + CStr(m_lngAreas) + ").Move"
                                        Err.Clear
                                        .Script.Move Me
                                        If (Err.Number <> 0) Then
                                            Err.Clear
                                            .Script.Move
                                        End If
                                        m_Engine.ContextLevelRemove
                                    End If
                                End With
                            Else
                                With Map.Areas(m_lngAreas)
                                    Err.Clear
                                    If .Script_Enter Then
                                        m_Engine.ContextLevelAdd "Map.Areas(" + CStr(m_lngAreas) + ").Enter"
                                        .Script.Enter Me
                                        If (Err.Number <> 0) Then
                                            Err.Clear
                                            .Script.Enter
                                        End If
                                        m_Engine.ContextLevelRemove
                                    End If
                                End With
                            End If
                        Else
                            If ((mvarOldX) >= .X) And ((mvarOldX) <= (.X + .Width)) And ((mvarOldY - mvarOldZ) >= .Y) And ((mvarOldY - mvarOldZ) <= (.Y + .Height)) Then
                                With Map.Areas(m_lngAreas)
                                    Err.Clear
                                    If .Script_Leave Then
                                        m_Engine.ContextLevelAdd "Map.Areas(" + CStr(m_lngAreas) + ").Leave"
                                        .Script.Leave Me
                                        If (Err.Number <> 0) Then
                                            Err.Clear
                                            .Script.Leave
                                        End If
                                        m_Engine.ContextLevelRemove
                                    End If
                                End With
                            End If
                        End If
                    End With
                End If
            Next m_lngAreas
        End If
    End If
End Sub

Friend Sub UpdateAnimation()
On Error Resume Next
    If PauseWhileOffscreen Then
        If Not CBool(mvarParam.Onscreen) Then
            Exit Sub
        End If
    End If
    If (Animating = True) Or ((AnimateWhileMoving = True) And (Moved = True)) Then
        If (AnimateWhileMoving) Then
            If ((Animating) And (Abs(TotalVelocity) > 0)) Or (Not Animating) Then
                If RelativeAnimation Then
                    mvarDelay = (mvarDelay + ((1 * (TotalVelocity / mvarWalkSpeed)) * IIf(AnimationMultiplier = -1, m_Engine.AnimationMultiplier, AnimationMultiplier)))
                Else
                    mvarDelay = (mvarDelay + (1 * IIf(AnimationMultiplier = -1, m_Engine.AnimationMultiplier, AnimationMultiplier)))
                End If
            Else
                mvarDelay = (mvarDelay + (1 * IIf(AnimationMultiplier = -1, m_Engine.AnimationMultiplier, AnimationMultiplier)))
            End If
        Else
            mvarDelay = (mvarDelay + (1 * IIf(AnimationMultiplier = -1, m_Engine.AnimationMultiplier, AnimationMultiplier)))
        End If
        If mvarDelay >= mvarPoses(TruePose).Frames(mvarFrame).Delay Then
            mvarDelay = 0
            If mvarPoses(TruePose).Frames.Count <= 1 Then
                If mvarFrame <> 1 Then
                    mvarFrame = 1
                    RefreshGraphic
                    Dirty
                End If
            ElseIf mvarPoses(TruePose).Frames.Count <= 2 Then
                If mvarFrame <> IIf(Velocity, 2, 1) Then
                    mvarFrame = IIf(Velocity, 2, 1)
                    RefreshGraphic
                    Dirty
                End If
            Else
                If mvarFrameInc <> 0 Then
                    mvarFrame = (mvarFrame + mvarFrameInc)
                    If mvarFrame > mvarPoses(TruePose).Frames.Count Then
                        Select Case mvarPoses(TruePose).LoopMode
                        Case F2LM_OneShot
                            mvarFrame = mvarPoses(TruePose).Frames.Count
                            RefreshGraphic
                            mvarPoses(TruePose).RunEndScript
                        Case F2LM_Restart
                            mvarFrame = 2
                            RefreshGraphic
                            Dirty
                        Case F2LM_PingPong
                            mvarFrame = mvarPoses(TruePose).Frames.Count - 1
                            mvarFrameInc = -1
                            RefreshGraphic
                            Dirty
                        Case Else
                            mvarFrame = 2
                            RefreshGraphic
                            Dirty
                        End Select
                    ElseIf mvarFrame < 2 Then
                        Select Case mvarPoses(TruePose).LoopMode
                        Case F2LM_OneShot
                            mvarFrame = 2
                            RefreshGraphic
                            mvarPoses(TruePose).RunEndScript
                        Case F2LM_Restart
                            mvarFrame = mvarPoses(TruePose).Frames.Count
                            RefreshGraphic
                            Dirty
                        Case F2LM_PingPong
                            mvarFrame = 3
                            mvarFrameInc = 1
                            RefreshGraphic
                            Dirty
                        Case Else
                            mvarFrame = mvarPoses(TruePose).Frames.Count
                            RefreshGraphic
                            Dirty
                        End Select
                    Else
                        RefreshGraphic
                        Dirty
                    End If
                End If
            End If
        End If
    Else
        If mvarFrame = 1 Then
        Else
            mvarFrame = 1
            RefreshGraphic
            Dirty
        End If
    End If
End Sub

Friend Sub UpdateMovement()
On Error Resume Next
Static m_sngOlderX As Single, m_sngOlderY As Single
Dim m_sngVel As Single
Dim m_sngXVel As Single, m_sngYVel As Single
Dim m_lngPreX As Long, m_lngPreY As Long, m_lngPreZ As Long
Dim m_sngAngle As Single
Dim m_sprBlock As Fury2Sprite
Dim m_booBlocked As Boolean
Static m_booWasBlocked As Boolean
Dim m_lngDetail As Long
    Moved = False
    If PauseWhileOffscreen Then
        If Not CBool(mvarParam.Onscreen) Then
            Exit Sub
        End If
    End If
    If mvarAttached Is Nothing Then
    Else
        With mvarAttached
            mvarX = .X + mvarAttachX
            mvarY = .Y + mvarAttachY
            mvarZ = .Z
            Velocity = .Velocity
            mvarAngle = .Angle
        End With
        If ((mvarX = mvarOldX) = True) And ((mvarY = mvarOldY) = True) And ((mvarZ = mvarOldZ) = True) Then
            Moved = False
        Else
            Moved = True
        End If
        Exit Sub
        Err.Clear
    End If
    m_sngOlderX = mvarX
    m_sngOlderY = mvarY
    Angle = Angle + RotationVelocity
    m_sngVel = IIf(RelativeVelocity, Velocity * mvarScale, Velocity)
    If (m_sngVel <> 0) Then
        If mvarTrueMovement Then
            If m_Engine.AccurateRotation Then
                m_sngAngle = mvarAngle * c_dblRadian
                m_sngXVel = Sin(m_sngAngle) * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
                m_sngYVel = -Cos(m_sngAngle) * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
            Else
                m_sngAngle = mvarAngle * c_dblRadian
                m_sngXVel = Sin(m_sngAngle)
                m_sngYVel = -Cos(m_sngAngle)
                If m_sngXVel < -0.4 Then
                    m_sngXVel = -1 * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
                ElseIf m_sngXVel > 0.4 Then
                    m_sngXVel = 1 * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
                Else
                    m_sngXVel = 0
                End If
                If m_sngYVel < -0.4 Then
                    m_sngYVel = -1 * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
                ElseIf m_sngYVel > 0.4 Then
                    m_sngYVel = 1 * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
                Else
                    m_sngYVel = 0
                End If
            End If
        Else
            m_sngXVel = mvarPoses(mvarPose).XVelocity * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
            m_sngYVel = mvarPoses(mvarPose).YVelocity * IIf(VelocityMultiplier = -1, m_Engine.VelocityMultiplier, VelocityMultiplier)
        End If
    End If
    If mvarSolid Then
        If (m_sngVel <> 0) Or (XVelocity <> 0) Or (YVelocity <> 0) Then
            m_sngVel = IIf(RelativeVelocity, Velocity * mvarScale, Velocity)
            mvarX = mvarX + ((m_sngXVel * m_sngVel) + XVelocity)
            RefreshPosition
            If IsBlocked Then
                If Not m_booWasBlocked Then m_booTriggerTileEvent = True
                Set m_sprBlock = IsBlockedBySprite
                If m_sprBlock Is Me Then
                Else
                    If m_sprBlock Is Nothing Then
                    Else
                        If m_booTriggerSpriteEvent Then
                        Else
                            m_booTriggerSpriteEvent = True
                            Set m_sprEvent = m_sprBlock
                        End If
                        With m_sprBlock
                            Touched m_sprBlock
                            .Touched Me
                            Err.Clear
                            If .Pushable Then
                                If Weight >= .Weight Then
                                    m_lngPreX = .X
                                    .X = .X + ((m_sngXVel * m_sngVel) + XVelocity)
                                    If .IsBlockedByTiles Then
                                        .X = m_lngPreX
                                    End If
                                Else
                                    mvarX = mvarX + (.XVelocity + (.Velocity * .Poses(.TruePose).XVelocity))
                                    RefreshPosition
                                End If
                            End If
                        End With
                    End If
                End If
                Set m_sprBlock = Nothing
                m_sngVel = IIf(RelativeVelocity, Velocity * mvarScale, Velocity)
                m_lngDetail = 0
                Do While ((IsBlocked) And (Floor(m_sngVel) > 0))
                    mvarX = mvarOldX
                    m_sngVel = m_sngVel - 1
                    mvarX = mvarX + ((m_sngXVel * m_sngVel) + XVelocity)
                    m_lngDetail = m_lngDetail + 1
                    If m_lngDetail >= 3 Then Exit Do
                    RefreshPosition
                Loop
                If (IsBlocked) Then
                    mvarX = Int(mvarX)
                    RefreshPosition
                End If
                If (IsBlocked) Then
                    mvarX = mvarOldX
                    RefreshPosition
                End If
            End If
            If Int(mvarZ) = 0 Then
                m_sngVel = IIf(RelativeVelocity, Velocity * mvarScale, Velocity)
                mvarY = mvarY + ((m_sngYVel * m_sngVel) + YVelocity)
                RefreshPosition
                If IsBlocked Then
                    If Not m_booWasBlocked Then m_booTriggerTileEvent = True
                    Set m_sprBlock = IsBlockedBySprite
                    If m_sprBlock Is Me Then
                    Else
                        If m_sprBlock Is Nothing Then
                        Else
                            If m_booTriggerSpriteEvent Then
                            Else
                                m_booTriggerSpriteEvent = True
                                Set m_sprEvent = m_sprBlock
                            End If
                            With m_sprBlock
                                Touched m_sprBlock
                                .Touched Me
                                Err.Clear
                                If .Pushable Then
                                    If Weight >= .Weight Then
                                        m_lngPreY = .Y
                                        .Y = .Y + ((m_sngYVel * m_sngVel) + YVelocity)
                                        If .IsBlockedByTiles Then
                                            .Y = m_lngPreY
                                        End If
                                        .ZVelocity = 0
                                    Else
                                        mvarY = mvarY + (.YVelocity + (.Velocity * .Poses(.TruePose).YVelocity))
                                        .ZVelocity = 0
                                        RefreshPosition
                                    End If
                                End If
                            End With
                        End If
                    End If
                    Set m_sprBlock = Nothing
                    m_sngVel = IIf(RelativeVelocity, Velocity * mvarScale, Velocity)
                    m_lngDetail = 0
                    Do While ((IsBlocked) And (Floor(m_sngVel) > 0))
                        mvarY = mvarOldY
                        m_sngVel = m_sngVel - 1
                        mvarY = mvarY + ((m_sngYVel * m_sngVel) + YVelocity)
                        m_lngDetail = m_lngDetail + 1
                        If m_lngDetail >= 3 Then Exit Do
                        RefreshPosition
                    Loop
                    If (IsBlocked) Then
                        mvarY = Int(mvarY)
                        RefreshPosition
                    End If
                    If (IsBlocked) Then
                        mvarY = mvarOldY
                        RefreshPosition
                    End If
                End If
            End If
        End If
        If ZVelocity <> 0 Then
            If ZVelocity > 0 Then
                ZVelocity = ZVelocity - m_Engine.VelocityDecay
            Else
                If (mvarZ > 0) Or (m_Engine.EnableGravity) Then
                    ZVelocity = ZVelocity - m_Engine.VelocityDecay
                    If ZVelocity < -(m_Engine.GravityStrength) Then ZVelocity = -(m_Engine.GravityStrength)
                Else
                    If m_Engine.EnableGravity Then
                        mvarY = mvarY - mvarZ
                        mvarZ = 0
                        RefreshPosition
                    Else
                        ZVelocity = 0
                        mvarZ = 0
                        RefreshPosition
                    End If
                End If
            End If
            m_sngVel = ZVelocity
            mvarZ = mvarZ + (m_sngVel)
            If mvarZ < 0 Then mvarZ = 0
            RefreshPosition
            If IsBlocked Then
                m_sngVel = ZVelocity
                m_lngDetail = 0
                mvarZ = Int(mvarZ)
                RefreshPosition
                ZVelocity = m_sngVel
                If (IsBlocked) Then
                    mvarZ = mvarOldZ
                    mvarY = mvarY - mvarZ
                    mvarZ = 0
                    ZVelocity = 0
                    RefreshPosition
                End If
            End If
        ElseIf m_Engine.EnableGravity Then
            ZVelocity = ZVelocity - m_Engine.VelocityDecay
            If ZVelocity < -(m_Engine.GravityStrength) Then ZVelocity = -(m_Engine.GravityStrength)
        End If
    Else
        If (m_sngVel <> 0) Or (XVelocity <> 0) Or (YVelocity <> 0) Then
            m_sngVel = IIf(RelativeVelocity, Velocity * mvarScale, Velocity)
            mvarX = mvarX + (m_sngXVel * m_sngVel) + XVelocity
            mvarY = mvarY + (m_sngYVel * m_sngVel) + YVelocity
            If mvarX < 0 Then mvarX = 0
            If mvarY < 0 Then mvarY = 0
            If mvarX > Map.MaxX Then mvarX = Map.MaxX
            If mvarY > Map.MaxY Then mvarY = Map.MaxY
            RefreshPosition
        End If
        If ZVelocity <> 0 Then
            If ZVelocity > 0 Then
                ZVelocity = ZVelocity - m_Engine.VelocityDecay
            Else
                If (mvarZ > 0) Or (m_Engine.EnableGravity) Then
                    ZVelocity = ZVelocity - m_Engine.VelocityDecay
                    If ZVelocity < -(m_Engine.GravityStrength) Then ZVelocity = -(m_Engine.GravityStrength)
                Else
                    If m_Engine.EnableGravity Then
                        mvarY = mvarY - mvarZ
                        mvarZ = 0
                        RefreshPosition
                    Else
                        ZVelocity = 0
                        mvarZ = 0
                        RefreshPosition
                    End If
                End If
            End If
            mvarZ = mvarZ + (ZVelocity)
            RefreshPosition
        End If
    End If
    If mvarMap.Layers(mvarLayer).WrapX Then
        If mvarX < 0 Then mvarX = mvarX + CSng(mvarMap.Width * mvarMap.Layers(1).Tileset.TileWidth): RefreshPosition
        If mvarX > mvarMap.Width * mvarMap.Layers(1).Tileset.TileWidth Then mvarX = mvarX - CSng(mvarMap.Width * mvarMap.Layers(1).Tileset.TileWidth): RefreshPosition
    End If
    If mvarMap.Layers(mvarLayer).WrapY Then
        If mvarY < 0 Then mvarY = mvarY + CSng(mvarMap.Height * mvarMap.Layers(1).Tileset.TileHeight): RefreshPosition
        If mvarY > mvarMap.Height * mvarMap.Layers(1).Tileset.TileHeight Then mvarY = mvarY - CSng(mvarMap.Height * mvarMap.Layers(1).Tileset.TileHeight): RefreshPosition
    End If
    If (((Abs(Velocity)) <= 0.015) = True) And (((Abs(ZVelocity)) <= 0.015) = True) And (((Abs(XVelocity)) <= 0.015) = True) And (((Abs(YVelocity)) <= 0.015) = True) Then
        Moved = False
    Else
        If ((mvarX = mvarOldX) = True) And ((mvarY = mvarOldY) = True) And ((mvarZ = mvarOldZ) = True) Then
            Moved = False
        Else
            Moved = True
        End If
    End If
    m_booWasBlocked = IsBlocked
End Sub

Friend Sub UpdatePushing()
On Error Resume Next
Static m_sngOlderX As Single, m_sngOlderY As Single
Dim m_sngVel As Single
Dim m_sngXVel As Single, m_sngYVel As Single
Dim m_lngPreX As Long, m_lngPreY As Long, m_lngPreZ As Long
Dim m_sngAngle As Single
Dim m_sprBlock As Fury2Sprite
Dim m_booBlocked As Boolean
Dim m_lngDetail As Long
    If PauseWhileOffscreen Then
        If Not CBool(mvarParam.Onscreen) Then
            Exit Sub
        End If
    End If
    If mvarAttached Is Nothing Then
    Else
        With mvarAttached
            mvarX = .X + mvarAttachX
            mvarY = .Y + mvarAttachY
            mvarZ = .Z
            Velocity = .Velocity
            mvarAngle = .Angle
        End With
        If ((mvarX = mvarOldX) = True) And ((mvarY = mvarOldY) = True) And ((mvarZ = mvarOldZ) = True) Then
            Moved = False
        Else
            Moved = True
        End If
        Exit Sub
        Err.Clear
    End If
    ' New push-compensation code.
    ' Should work way better. :)
    If Pushable Then
        If mvarSolid Then
            Set m_sprBlock = IsBlockedBySprite
            If Not (m_sprBlock Is Nothing) Then
                If m_sprBlock Is Me Then
                Else
                    If m_sprBlock Is Nothing Then
                    Else
                        With m_sprBlock
                            Touched m_sprBlock
                            .Touched Me
                            If Weight > .Weight Then
                            Else
                                m_lngPreX = mvarX
                                mvarX = mvarX + ((.XVelocity) + (.Velocity * .Poses(.TruePose).XVelocity))
                                RefreshPosition
                                If IsBlocked Then
                                    mvarX = m_lngPreX
                                End If
                                m_lngPreY = mvarY
                                mvarY = mvarY + ((.YVelocity) + (.Velocity * .Poses(.TruePose).YVelocity))
                                RefreshPosition
                                If IsBlocked Then
                                    mvarY = m_lngPreY
                                End If
                                mvarY = mvarY - mvarZ
                                mvarZ = 0
                                ZVelocity = 0
                                RefreshPosition
                            End If
                        End With
                    End If
                End If
                Set m_sprBlock = Nothing
            End If
        End If
    End If
End Sub

Friend Sub UpdatePath()
On Error Resume Next
Static m_sngLastDistance As Single
Dim m_pthNode As Fury2Waypoint
Dim m_sngPathDistance As Single, m_sngPathAngle As Single, m_booPathComplete As Boolean
    If PauseWhileOffscreen Then
        If Not CBool(mvarParam.Onscreen) Then
            Exit Sub
        End If
    End If
    If (mvarFollowing Is Nothing) Then
    Else
        Err.Clear
        mvarPath.Remove 1
        mvarPath.Add mvarFollowing.FollowX, mvarFollowing.FollowY
        Err.Clear
    End If
    If (mvarFollowPath) Or (Not (mvarFollowing Is Nothing)) Then
        Set m_Engine.IObjThis = Me
        m_Engine.SetIObjThis
        m_booPathComplete = False
        If mvarPath.Count > 0 Then
            With mvarPath(1)
                m_sngPathDistance = m_Engine.DistanceBetween(mvarX, mvarY, .X, .Y)
                If ((m_sngLastDistance <= m_sngPathDistance) And (Abs(Velocity) > 0.1)) Then
                    If (Round(mvarPreviousX(1)) = Round(mvarX)) And (Round(mvarPreviousY(1)) = Round(mvarY)) And (Round(mvarPreviousX(0)) <> Round(mvarX)) And (Round(mvarPreviousY(0)) <> Round(mvarY)) Then
                        mvarX = .X
                        mvarY = .Y
                        RefreshPosition
                        m_booPathComplete = True
                    Else
                        If (Abs(m_sngPathDistance) >= (Velocity)) Then
                            m_sngPathAngle = m_Engine.AngleBetween(mvarX, mvarY, .X, .Y)
                            Angle = m_sngPathAngle
                            Velocity = Velocity + m_Engine.VelocityDecay
                            If Velocity > mvarWalkSpeed Then Velocity = mvarWalkSpeed
                            If Velocity > m_sngPathDistance Then Velocity = m_sngPathDistance
                        ElseIf (Abs(mvarPathStartDistance) >= 1) Then
                            If Velocity > mvarWalkSpeed Then Velocity = mvarWalkSpeed
                            If Velocity > m_sngPathDistance Then Velocity = m_sngPathDistance
                            m_booPathComplete = True
                        Else
                            mvarX = .X
                            mvarY = .Y
                            RefreshPosition
                            m_booPathComplete = True
                        End If
                    End If
                    mvarPathDelay = mvarPathDelay + 1
                    If mvarPathDelay >= IIf(.WaitOverride >= 0, .WaitOverride, m_Engine.MaxPathWait) Then
                        mvarPathDelay = 0
                        Set m_pthNode = mvarPath(1)
                        With m_pthNode
                            If mvarPath.Looping Then
                                mvarPath.Add .X, .Y
                                mvarPath.Remove 1
                                With mvarPath(1)
                                    mvarPathStartDistance = m_Engine.DistanceBetween(mvarX, mvarY, .X, .Y)
                                End With
                            Else
                                mvarPath.Remove 1
                                If mvarPath.Count < 1 Then
                                    Velocity = 0
                                Else
                                    With mvarPath(1)
                                        mvarPathStartDistance = m_Engine.DistanceBetween(mvarX, mvarY, .X, .Y)
                                    End With
                                End If
                            End If
                            If Trim(.FailEvent) = "" Then
                                If Trim(.ReachedEvent) = "" Then
                                Else
                                    Err.Clear
                                    CallByName Script, .ReachedEvent, VbMethod
                                    If Err <> 0 Then
                                        Err.Clear
                                        m_Engine.ScriptEngine.AddCode .ReachedEvent
                                    End If
                                    Err.Clear
                                End If
                            Else
                                Err.Clear
                                CallByName Script, .FailEvent, VbMethod
                                If Err <> 0 Then
                                    Err.Clear
                                    m_Engine.ScriptEngine.AddCode .FailEvent
                                End If
                                Err.Clear
                            End If
                        End With
                    End If
                Else
                    mvarPathDelay = ClipValue(mvarPathDelay - 1, 0, 9999)
                    If mvarPathStartDistance = 0 Then mvarPathStartDistance = m_sngPathDistance
                    If (Abs(m_sngPathDistance) >= (Velocity)) Then
                        m_sngPathAngle = m_Engine.AngleBetween(mvarX, mvarY, .X, .Y)
                        Angle = m_sngPathAngle
                        Velocity = Velocity + m_Engine.VelocityDecay
                        If Velocity > mvarWalkSpeed Then Velocity = mvarWalkSpeed
                        If Velocity > m_sngPathDistance Then Velocity = m_sngPathDistance
                    ElseIf (Abs(mvarPathStartDistance) >= 1) Then
                        If Velocity > mvarWalkSpeed Then Velocity = mvarWalkSpeed
                        If Velocity > m_sngPathDistance Then Velocity = m_sngPathDistance
                        m_booPathComplete = True
                    Else
                        mvarX = .X
                        mvarY = .Y
                        RefreshPosition
                        m_booPathComplete = True
                    End If
                End If
            End With
            If m_booPathComplete Then
                m_booPathComplete = False
                Set m_pthNode = mvarPath(1)
                With m_pthNode
                    If mvarPath.Looping Then
                        mvarPath.Add .X, .Y
                        mvarPath.Remove 1
                        With mvarPath(1)
                            mvarPathStartDistance = m_Engine.DistanceBetween(mvarX, mvarY, .X, .Y)
                        End With
                    Else
                        mvarPath.Remove 1
                        If mvarPath.Count < 1 Then
                            Velocity = 0
                        Else
                            With mvarPath(1)
                                mvarPathStartDistance = m_Engine.DistanceBetween(mvarX, mvarY, .X, .Y)
                            End With
                        End If
                    End If
                    If Trim(.ReachedEvent) = "" Then
                    Else
                        Err.Clear
                        CallByName Script, .ReachedEvent, VbMethod
                        If Err <> 0 Then
                            Err.Clear
                            m_Engine.ScriptEngine.AddCode .ReachedEvent
                        End If
                        Err.Clear
                    End If
                End With
            End If
            m_sngLastDistance = m_sngPathDistance
        Else
            Velocity = 0
        End If
    End If
End Sub

Friend Sub UpdateMisc()
On Error Resume Next
    If PauseWhileOffscreen Then
        If Not CBool(mvarParam.Onscreen) Then
            Exit Sub
        End If
    End If
    
    If mvarPose < 1 Then
        mvarPose = 1
        RefreshGraphic
    End If
    
    If mvarState < 1 Then
        mvarState = 1
    End If
    
    If AttachedGraphic Is Nothing Then
    Else
        AttachedGraphic.Update
    End If
    
    If (AlphaVelocity <> 0) Then
        Alpha = Alpha + AlphaVelocity
        If AlphaVelocity < 0 Then
            If Alpha <= mvarADest Then AlphaVelocity = 0: Script.FadeOut
        Else
            If Alpha >= mvarADest Then AlphaVelocity = 0: Script.FadeOut
        End If
    End If
    
    If OverrideDepth Then
    Else
        If mvarLayer > 0 Then
            With Map.Layers(mvarLayer)
                If .HasDepthmap Then
                    mvarScale = .DepthmapRead(mvarX, mvarY)
                End If
            End With
        End If
    End If
    
End Sub

Public Sub Update(Map As Fury2Map, Optional CLayer As Long = 1)
Attribute Update.VB_MemberFlags = "40"
On Error Resume Next
Dim Moved As Boolean
Dim m_lngXSub As Long, m_lngYSub As Long
Dim m_sngNewX As Single, m_sngNewY As Single, m_sngNewZ As Single
Dim m_lngMoveBack As Long
    mvarTouched = ClipValue(mvarTouched - 1, 0, 100)
    mvarLayer = CLayer
    Set mvarMap = Map
    If Entity Then
        UpdateEntity
    Else
        UpdateObject
    End If
    If Moved Then
        For m_lngMoveBack = c_lngMaxHistoryLength To 1 Step -1
            mvarPreviousX(m_lngMoveBack) = mvarPreviousX(m_lngMoveBack - 1)
            mvarPreviousY(m_lngMoveBack) = mvarPreviousY(m_lngMoveBack - 1)
            mvarPreviousA(m_lngMoveBack) = mvarPreviousA(m_lngMoveBack - 1)
            mvarPreviousV(m_lngMoveBack) = mvarPreviousV(m_lngMoveBack - 1)
        Next m_lngMoveBack
        mvarPreviousX(0) = Floor(mvarX)
        mvarPreviousY(0) = Floor(mvarY)
        mvarPreviousA(0) = mvarAngle
        mvarPreviousV(0) = Velocity
        mvarHistoryLength = ClipValue(mvarHistoryLength + 1, 0, c_lngMaxHistoryLength)
        If mvarScript_Step Then
            m_Engine.ContextLevelAdd "Sprite_Step"
            Script.Step
            m_Engine.ContextLevelRemove
        End If
        RefreshPosition
        Dirty
    End If
    mvarOldX = mvarX
    mvarOldY = mvarY
    mvarOldZ = mvarZ
    Set m_sprEvent = Nothing
End Sub

Public Function PreviousX(Index) As Long
    PreviousX = mvarPreviousX(CLng(Index))
End Function

Public Function PreviousY(Index) As Long
    PreviousY = mvarPreviousY(CLng(Index))
End Function

Public Function PreviousAngle(Index) As Single
    PreviousAngle = mvarPreviousA(CLng(Index))
End Function

Public Function PreviousVelocity(Index) As Single
    PreviousVelocity = mvarPreviousV(CLng(Index))
End Function

Public Function PoseOffset() As Long
On Error Resume Next
    Err.Clear
    If mvarStates Is Nothing Then Exit Function
    If mvarStates.Count < 1 Then Exit Function
    If mvarState < 1 Or mvarState > mvarStates.Count Then Exit Function
    PoseOffset = mvarStates(mvarState).PoseOffset
End Function

Public Sub UpdatePose()
Attribute UpdatePose.VB_MemberFlags = "40"
On Error Resume Next
Dim m_posPose As Fury2Pose, m_lngPoses As Long
Dim m_lngHeading As Long, m_lngOldPose As Long
    If OverridePose Then Exit Sub
        If mvarStates.Count < 1 Then Exit Sub
        If mvarStates(mvarState) Is Nothing Then Exit Sub
    m_lngHeading = CLng((((mvarAngle * CSng(mvarStates(mvarState).DirectionPoses)) / 360!) + 0!)) Mod mvarStates(mvarState).DirectionPoses
    m_lngOldPose = mvarPose
    Pose = PoseOffset + (m_lngHeading) + 1
End Sub

Public Property Get Name() As String
Attribute Name.VB_Description = "The sprite's name."
Attribute Name.VB_MemberFlags = "200"
On Error Resume Next
    Name = mvarName
End Property

Public Property Let Name(NewName As String)
On Error Resume Next
    mvarName = NewName
End Property

Public Function TotalVelocity() As Single
On Error Resume Next
    TotalVelocity = Abs(Velocity) + Abs(XVelocity) + Abs(YVelocity)
End Function

Public Property Let Effect(ByVal vData As Fury2SpriteEffects)
    mvarEffect = Abs(vData)
    Dirty
End Property

Public Property Get Effect() As Fury2SpriteEffects
Attribute Effect.VB_Description = "The sprite's display mode."
    Effect = mvarEffect
End Property

Public Property Get AIUpdateDelay() As Long
Attribute AIUpdateDelay.VB_Description = "The delay between individual AI updates."
    AIUpdateDelay = mvarAIUpdateDelay
End Property

Public Property Let AIUpdateDelay(NewDelay As Long)
    mvarAIUpdateDelay = CLng(NewDelay)
End Property

Public Property Let Alpha(ByVal vData As Single)
Attribute Alpha.VB_Description = "The sprite's opacity."
    If mvarAlpha <> vData Then
        mvarAlpha = vData
        If mvarAlpha > 1 Then mvarAlpha = 1
        If mvarAlpha < -1 Then mvarAlpha = -1
        Dirty
    End If
    RefreshOptions
End Property

Public Property Get Alpha() As Single
    Alpha = mvarAlpha
End Property

Public Property Set Path(vData As Fury2Path)
Attribute Path.VB_Description = "The sprite's path waypoints."
    Set mvarPath = vData
End Property

Public Property Get Path() As Fury2Path
    Set Path = mvarPath
End Property

Friend Sub SetPose(Obj As Fury2Pose)
On Error Resume Next
Dim m_lngPoses As Long
    If mvarPoses.Count > 0 Then
        For m_lngPoses = 1 To mvarPoses.Count
            If mvarPoses(m_lngPoses) Is Obj Then
                mvarPose = m_lngPoses
                If mvarPoses(m_lngPoses).Frames.Count >= 2 Then
                    mvarFrame = 2
                    RefreshGraphic
                Else
                    mvarFrame = 1
                    RefreshGraphic
                End If
                mvarDelay = 0
                Dirty
                Exit Sub
            End If
        Next m_lngPoses
    End If
End Sub

Sub LoadGFX()
Attribute LoadGFX.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngPoses As Long
    mvarLoaded = True
    If mvarPoses.Count > 0 Then
        For m_lngPoses = 1 To mvarPoses.Count
            With mvarPoses(m_lngPoses)
                .SetParent Me
                .LoadFrames
                If mvarEffect = 1 Then
                    .EnsureAlpha
                End If
            End With
        Next m_lngPoses
    End If
    RefreshGraphic
End Sub

Public Property Set Poses(vData As Fury2Poses)
Attribute Poses.VB_Description = "All the sprite's defined poses."
On Error Resume Next
    Set mvarPoses = vData
End Property

Public Property Get Poses() As Fury2Poses
On Error Resume Next
    Set Poses = mvarPoses
End Property

Public Property Set States(vData As Fury2States)
On Error Resume Next
    Set mvarStates = vData
End Property

Public Property Get States() As Fury2States
On Error Resume Next
    Set States = mvarStates
End Property

Public Property Let AttachY(ByVal vData As Single)
On Error Resume Next
    mvarAttachY = vData
End Property

Public Property Get AttachY() As Single
    AttachY = mvarAttachY
End Property

Public Property Let AttachX(ByVal vData As Single)
On Error Resume Next
    mvarAttachX = vData
End Property

Public Property Get AttachX() As Single
    AttachX = mvarAttachX
End Property

Public Property Let Y(ByVal vData As Single)
Attribute Y.VB_Description = "The sprite's Y coordinate."
On Error Resume Next
    Dirty
    mvarY = vData
    RefreshPosition
    Dirty
End Property

Public Property Get Y() As Single
    Y = mvarY
End Property

Public Property Let X(ByVal vData As Single)
Attribute X.VB_Description = "The sprite's X coordinate."
On Error Resume Next
    Dirty
    mvarX = vData
    RefreshPosition
    Dirty
End Property

Public Property Get X() As Single
    X = mvarX
End Property

Public Property Get OldX() As Single
Attribute OldX.VB_MemberFlags = "40"
    OldX = mvarOldX
End Property

Public Property Get OldY() As Single
Attribute OldY.VB_MemberFlags = "40"
    OldY = mvarOldY
End Property

Public Property Get OldZ() As Single
Attribute OldZ.VB_MemberFlags = "40"
    OldZ = mvarOldZ
End Property

Public Function Parent() As Fury2Map
    Set Parent = mvarMap
End Function

Public Property Let Solid(ByVal vData As Boolean)
Attribute Solid.VB_Description = "This flag controls whether the sprite can collide with objects and tiles, and interact with objects."
    mvarSolid = vData
    RefreshOptions
End Property

Public Property Get Solid() As Boolean
    Solid = mvarSolid
End Property

Public Property Let Frame(ByVal vData As Long)
Attribute Frame.VB_Description = "The current animation frame."
On Error Resume Next
    If vData > 1 And vData <= mvarPoses(mvarPose).Frames.Count Then
        mvarFrame = vData
    Else
        mvarFrame = 1
    End If
    RefreshGraphic
    Dirty
End Property

Public Property Get Frame() As Long
    Frame = mvarFrame
End Property

Public Property Let Pose(ByVal vData As Long)
Attribute Pose.VB_Description = "The sprite's current pose."
On Error Resume Next
Dim m_lngLoop As Long
    If vData = mvarPose Then Exit Property
    If vData < 1 Then
        m_lngLoop = 0
        Do Until (vData >= 1) Or (m_lngLoop > 9)
            vData = vData + mvarPoses.Count
            m_lngLoop = m_lngLoop + 1
        Loop
    End If
    If vData > mvarPoses.Count Then
        m_lngLoop = 0
        Do Until (vData <= mvarPoses.Count) Or (m_lngLoop > 9)
            vData = vData - mvarPoses.Count
            m_lngLoop = m_lngLoop + 1
        Loop
    End If
    mvarPose = vData
    If mvarFrameInc = 0 Then
        mvarFrameInc = 1
    End If
    RefreshGraphic
    RefreshObstruction
    Dirty
End Property

Public Property Get Pose() As Long
    If mvarPose < 1 Then mvarPose = 1
    Pose = mvarPose
End Property

Public Property Get State() As Long
    If mvarState < 1 Then mvarState = 1
    State = mvarState
End Property

Public Property Let State(NewState As Long)
On Error Resume Next
Dim m_lngOldPose As Long, m_lngNewPose As Long
Dim m_lngOldState As Long
    If NewState < 1 Then Exit Property
    If NewState > mvarStates.Count Then Exit Property
    m_lngOldState = mvarState
    m_lngOldPose = mvarPose
    UpdatePose
    m_lngNewPose = mvarPose
    mvarPose = m_lngOldPose
    mvarState = NewState
    If m_lngOldPose = m_lngNewPose Then
                If mvarStates(m_lngOldState) Is Nothing Then
                ElseIf mvarStates(mvarState) Is Nothing Then
                Else
                        mvarPose = mvarStates(mvarState).PoseOffset + (m_lngNewPose - mvarStates(m_lngOldState).PoseOffset)
                End If
    End If
    RefreshObstruction
    RefreshGraphic
End Property

Public Property Let Angle(ByVal vData As Single)
Attribute Angle.VB_Description = "The sprite's angle (0-360)."
On Error Resume Next
    If vData < 360! Then
    Else
        Do Until vData < 360!
            vData = vData - 360!
        Loop
    End If
    If vData >= 0! Then
    Else
        Do Until vData >= 0!
            vData = vData + 360!
        Loop
    End If
    mvarAngle = vData
    UpdatePose
End Property

Public Property Get Angle() As Single
    Angle = mvarAngle
End Property

Public Sub LoadTemplate()
On Error Resume Next
Dim l_sprTemplate As Fury2Sprites
Dim l_sprSprite As Fury2Sprite
    If Trim(Template) = "" Then Exit Sub
    Set l_sprTemplate = m_Engine.LoadSprites(Left(Template, InStrRev(Template, ":") - 1))
    If l_sprTemplate Is Nothing Then Exit Sub
    Set l_sprSprite = l_sprTemplate(Mid(Template, InStrRev(Template, ":") + 1))
    l_sprSprite.CopyTo Me, True
    Set mvarObject = Nothing
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    mvarHistoryLength = -1
    AnimationMultiplier = -1
    VelocityMultiplier = -1
    mvarTrueMovement = True
    WalkSpeed = 1
    Set mvarStates = New Fury2States
    mvarStates.Add New Fury2State
    With mvarStates(1)
        .Name = "Normal"
        .PoseOffset = 0
    End With
    Set mvarPoses = New Fury2Poses
    Set mvarPath = New Fury2Path
    Set mvarAI = New Fury2AIBehaviors
    ReDim mvarPreviousX(0 To c_lngMaxHistoryLength)
    ReDim mvarPreviousY(0 To c_lngMaxHistoryLength)
    ReDim mvarPreviousA(0 To c_lngMaxHistoryLength)
    ReDim mvarPreviousV(0 To c_lngMaxHistoryLength)
    Entity = True
    mvarAlpha = 1
    mvarFrame = 1
    mvarFrameInc = 1
    mvarScale = 1
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngDirectionPoses As Long
Dim l_staState As Fury2State
Dim l_strScript As String
    With File
        .ReadSegment_Begin
            .ReadSegment_Begin
                .Load mvarName
                .Load mvarX
                .Load mvarY
                .Load mvarZ
                .Load mvarAngle
                .Load Template
            .ReadSegment_End
            .ReadSegment_Begin
                .Load Velocity
                .Load XVelocity
                .Load YVelocity
                .Load ZVelocity
                .Load RotationVelocity
                .Load mvarWalkSpeed
            .ReadSegment_End
            .ReadSegment_Begin
                .Load mvarPose
                .Load mvarFrame
                .Load mvarState
            .ReadSegment_End
            .ReadSegment_Begin
                .Load Animating
                .Load AnimateWhileMoving
                .Load l_lngDirectionPoses
                .Load mvarTrueMovement
                .Load RelativeAnimation
            .ReadSegment_End
            .ReadSegment_Begin
                .Load mvarSolid
                .Load Weight
                .Load Pushable
                .Load mvarFollowPath
                .Load RelativeVelocity
            .ReadSegment_End
            .ReadSegment_Begin
                .Load mvarEffect
                .Load mvarAlpha
                .Load mvarScale
                .Load mvarRotation
                .Load OverrideDepth
                .Load OverridePose
            .ReadSegment_End
            .ReadSegment_Begin
                .Load mvarScript
                .Load ScriptOverride
                l_strScript = mvarScript
            .ReadSegment_End
            .ReadSegment_Begin
                .Load mvarAIUpdateDelay
            .ReadSegment_End
            .ReadSegment_Begin
                .Load mvarClass
                .Load PauseWhileOffscreen
                .Load TrackAsProjectile
                If .EOS Then
                    Entity = True
                Else
                    .Load Entity
                End If
            .ReadSegment_End
            .ReadSegment_Begin
                .Load IdleState
                .Load WalkState
                .Load RunState
                .Load JumpState
            .ReadSegment_End
            .Load mvarPoses
            .Load mvarPath
            .Load mvarAI
            .Load mvarStates
            If l_lngDirectionPoses = -1 Then
            Else
                For Each l_staState In mvarStates
                    l_staState.DirectionPoses = l_lngDirectionPoses
                Next l_staState
            End If
        .ReadSegment_End
        LoadTemplate
        If ScriptOverride Or (m_booEditor) Then
            mvarScript = l_strScript
        End If
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .WriteSegment_Begin
                .Save mvarName
                .Save mvarX
                .Save mvarY
                .Save mvarZ
                .Save mvarAngle
                If EditMode Then
                    .Save Template
                End If
            .WriteSegment_End
            .WriteSegment_Begin
                .Save Velocity
                .Save XVelocity
                .Save YVelocity
                .Save ZVelocity
                .Save RotationVelocity
                .Save mvarWalkSpeed
            .WriteSegment_End
            .WriteSegment_Begin
                .Save mvarPose
                .Save mvarFrame
                .Save mvarState
            .WriteSegment_End
            .WriteSegment_Begin
                .Save Animating
                .Save AnimateWhileMoving
                .Save CLng(-1)
                .Save mvarTrueMovement
                .Save RelativeAnimation
            .WriteSegment_End
            .WriteSegment_Begin
                .Save mvarSolid
                .Save Weight
                .Save Pushable
                .Save mvarFollowPath
                .Save RelativeVelocity
            .WriteSegment_End
            .WriteSegment_Begin
                .Save mvarEffect
                .Save mvarAlpha
                .Save mvarScale
                .Save mvarRotation
                .Save OverrideDepth
                .Save OverridePose
            .WriteSegment_End
            .WriteSegment_Begin
                .Save mvarScript
                .Save ScriptOverride
            .WriteSegment_End
            .WriteSegment_Begin
                .Save mvarAIUpdateDelay
            .WriteSegment_End
            .WriteSegment_Begin
                .Save mvarClass
                .Save PauseWhileOffscreen
                .Save TrackAsProjectile
                .Save Entity
            .WriteSegment_End
            .WriteSegment_Begin
                .Save IdleState
                .Save WalkState
                .Save RunState
                .Save JumpState
            .WriteSegment_End
            .Save mvarPoses
            .Save mvarPath
            .Save mvarAI
            .Save mvarStates
        .WriteSegment_End
    End With
End Sub

Sub Teleport(Optional NewX, Optional NewY)
Attribute Teleport.VB_Description = "Teleports the sprite to new coordinates."
On Error Resume Next
    Dirty
    If IsMissing(NewX) Then
    ElseIf NewX = vbNull Then
    Else
        mvarX = CSng(NewX)
        mvarOldX = mvarX
    End If
    If IsMissing(NewY) Then
    ElseIf NewY = vbNull Then
    Else
        mvarY = CSng(NewY)
        mvarOldY = mvarY
    End If
    mvarHistoryLength = -1
    RefreshPosition
    Dirty
End Sub

#End If
