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

'Private SpriteID_ As Long

Private m_sngVelocityDecay As Single
Private m_sngVelocityAccel As Single

Private m_sprAttached As Fury2Sprite
Private m_sngAttachX As Single, m_sngAttachY As Single, m_sngAttachH As Single, m_sngAttachV As Single

Private m_lngLayer As Long
Private m_mapMap As Fury2Map
'Private m_sprSprites As WeakReference
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

Private m_sngRotationOffset As Single
Private m_booTrueMovement As Boolean, m_booRelativeAnimation As Boolean
Private m_booDisplayRotation As Boolean, m_booRelativeRotation As Boolean

Private m_booMoved As Boolean
Private m_booLoaded As Boolean

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
Private m_booScript_Load As Boolean
Private m_booScript_Die As Boolean, m_booScript_Step As Boolean
Private m_booScript_FadeOut As Boolean, m_booScript_Collided As Boolean
Private m_booScript_WaypointReached As Boolean

Private m_booAreasUpdated As Boolean

Private m_lngWalkState As Long, m_lngIdleState As Long
Private m_lngBlockingType As Long

Public SaveScriptData As Boolean
Attribute SaveScriptData.VB_VarDescription = "If True, the data in the sprite's Script will be saved along with the sprite."

Public LinkState As Boolean, LinkPose As Boolean, LinkFrame As Boolean, LinkProperties As Boolean, LinkBearing As Boolean
Attribute LinkState.VB_VarDescription = "If True, while Attached to another sprite, this sprite will share that sprite's state."
Attribute LinkPose.VB_VarDescription = "If True, while Attached to another sprite, this sprite will share that sprite's pose."
Attribute LinkFrame.VB_VarDescription = "If True, while Attached to another sprite, this sprite will share that sprite's frame."
Attribute LinkProperties.VB_VarDescription = "If True, while Attached to another sprite, this sprite will share that sprite's properties (Alpha, etc.)."
Attribute LinkBearing.VB_VarDescription = "If True, while Attached to another sprite, this sprite will share that sprite's bearing (angle)."

Private WithEvents m_colPaths As Fury2Collection
Attribute m_colPaths.VB_VarHelpID = -1
Public PathIndex As Long
Attribute PathIndex.VB_VarDescription = "*"
Public SavedPaths As Fury2Collection
Attribute SavedPaths.VB_VarDescription = "*"
Public AttachedGraphic As Fury2Graphic
Attribute AttachedGraphic.VB_VarDescription = "*"
Public Tag As String
Attribute Tag.VB_VarDescription = "An additional piece of data."
Public AnimationMultiplier As Single
Attribute AnimationMultiplier.VB_VarDescription = "*"
Public Ignore As Boolean
Attribute Ignore.VB_VarDescription = "If True, this sprite's AI, pathing, and events will not be processed."
Private mvarEngine As Fury2Engine

Private m_lngID_Activate As Long
Private m_lngID_AIUpdate As Long
Private m_lngID_Collided As Long
Private m_lngID_Decay As Long
Private m_lngID_Die As Long
Private m_lngID_FadeOut As Long
Private m_lngID_Hit As Long
Private m_lngID_HitMap As Long
Private m_lngID_Load As Long
Private m_lngID_Step As Long
Private m_lngID_Update As Long
Private m_lngID_WaypointReached As Long

Friend Sub BindScript()
On Error Resume Next
    m_lngID_Activate = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Activate")
    m_lngID_AIUpdate = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "AIUpdate")
    m_lngID_Collided = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Collided")
    m_lngID_Decay = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Decay")
    m_lngID_Die = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Die")
    m_lngID_FadeOut = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "FadeOut")
    m_lngID_Hit = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Hit")
    m_lngID_HitMap = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "HitMap")
    m_lngID_Load = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Load")
    m_lngID_Step = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Step")
    m_lngID_Update = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "Update")
    m_lngID_WaypointReached = mvarEngine.ScriptEngine.ResolveMember(m_objScript, "WaypointReached")
End Sub

Friend Sub FreeScript()
On Error Resume Next
Dim l_scrOld As Object
    Set l_scrOld = m_objScript
    Set m_objScript = Nothing
    Set Engine.IObjThis = Nothing
    l_scrOld.F2OInit Nothing, Nothing
End Sub

Public Sub ReloadScript()
Attribute ReloadScript.VB_Description = "Reloads the sprite's script."
On Error Resume Next
Dim l_vfTemp As VirtualFile
Dim l_scrOld As Object
    Set l_scrOld = m_objScript
    Set m_objScript = Nothing
    If SaveScriptData Then
        Set l_vfTemp = New VirtualFile
        l_vfTemp.Save l_scrOld
    End If
    m_booScriptLoadFailed = False
    If Script Is Nothing Then
        Set m_objScript = l_scrOld
        Engine.DebugOut "Script reload failed for sprite " & Name
    Else
        l_scrOld.F2OInit Nothing, Nothing
        Set l_scrOld = Nothing
        Script.Init
        Script.Initialize
        If m_booLoaded Then
            Script.Load
        End If
        If SaveScriptData Then
            l_vfTemp.CursorPos = 0
            l_vfTemp.Load Script
        End If
    End If
End Sub

Public Property Get RenderTarget() As Long
Attribute RenderTarget.VB_Description = "Sets this sprite to draw to an alternative rendering target."
On Error Resume Next
    RenderTarget = m_spParam.Params.RenderTarget + 1
End Property

Public Property Let RenderTarget(ByVal Index As Long)
On Error Resume Next
    m_spParam.Params.RenderTarget = Index - 1
End Property

Friend Sub SetEngine(NewEngine As Fury2Engine)
On Error Resume Next
    Set mvarEngine = NewEngine
    Set SavedPaths.Engine = NewEngine
End Sub

Friend Property Get Engine() As Fury2Engine
    Set Engine = mvarEngine
End Property

Public Property Get DiffuseLight() As Boolean
Attribute DiffuseLight.VB_Description = "If True, this sprite will emit diffuse light into a lightmap."
    DiffuseLight = m_spParam.Params.DiffuseLight <> 0
End Property

Public Property Let DiffuseLight(ByVal NewValue As Boolean)
    m_spParam.Params.DiffuseLight = CByte(Abs(CLng(NewValue)))
End Property

Public Property Get Pushable() As Boolean
Attribute Pushable.VB_Description = "If True, other sprites can apply force to this sprite by colliding with it."
    Pushable = m_spParam.Stats.Pushable <> 0
End Property

Public Property Let Pushable(ByVal NewValue As Boolean)
    m_spParam.Stats.Pushable = CByte(Abs(CLng(NewValue)))
End Property

Public Property Get CanPush() As Boolean
Attribute CanPush.VB_Description = "If True, this sprite can apply force to other sprites by colliding with them."
    CanPush = m_spParam.Stats.CanPush <> 0
End Property

Public Property Let CanPush(ByVal NewValue As Boolean)
    m_spParam.Stats.CanPush = CByte(Abs(CLng(NewValue)))
End Property

Public Property Get VelocityMultiplier() As Single
Attribute VelocityMultiplier.VB_Description = "*"
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

Public Property Get VelocityDecay() As Single
Attribute VelocityDecay.VB_Description = "Controls the rate at which this object decelerates when under the control of a path or controller. Set to -1 to use value of Engine.VelocityDecay."
On Error Resume Next
    If m_sngVelocityDecay <> -1 Then
        VelocityDecay = m_sngVelocityDecay
    Else
        If (Engine Is Nothing) Then
            VelocityDecay = m_sngVelocityDecay
        Else
            VelocityDecay = Engine.VelocityDecay
        End If
    End If
End Property

Public Property Let VelocityDecay(ByVal NewValue As Single)
On Error Resume Next
    m_sngVelocityDecay = NewValue
End Property

Public Property Get VelocityAccel() As Single
Attribute VelocityAccel.VB_Description = "Controls the rate at which this object accelerates when under the control of a path or controller. Set to -1 to use value of Engine.VelocityAccel."
On Error Resume Next
    If m_sngVelocityAccel <> -1 Then
        VelocityAccel = m_sngVelocityAccel
    Else
        VelocityAccel = VelocityDecay
    End If
End Property

Public Property Let VelocityAccel(ByVal NewValue As Single)
On Error Resume Next
    m_sngVelocityAccel = NewValue
End Property

Public Function SightCheck(ByVal X As Single, ByVal Y As Single, Optional ByRef IgnoreSprite = Nothing) As Boolean
Attribute SightCheck.VB_Description = "Performs a sight check from this sprite to a given location, using the current lighting data. If IgnoreSprite specifies a sprite, that sprite will be ignored."
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

Public Sub Attach(ByRef Sprite, Optional ByVal XOffset As Single = 0, Optional ByVal YOffset As Single = 0, Optional ByVal HOffset As Single = 0, Optional ByVal VOffset As Single = 0)
Attribute Attach.VB_Description = "Attaches the sprite to another sprite."
On Error Resume Next
    Set m_sprAttached = Sprite
    m_sngAttachX = XOffset
    m_sngAttachY = YOffset
    m_sngAttachH = HOffset
    m_sngAttachV = VOffset
End Sub

Public Property Let AttachX(ByVal vData As Single)
Attribute AttachX.VB_Description = "*"
    m_sngAttachX = vData
End Property

Public Property Get AttachX() As Single
    AttachX = m_sngAttachX
End Property

Public Property Let AttachY(ByVal vData As Single)
Attribute AttachY.VB_Description = "*"
    m_sngAttachY = vData
End Property

Public Property Get AttachY() As Single
    AttachY = m_sngAttachY
End Property

Public Property Let AttachH(ByVal vData As Single)
Attribute AttachH.VB_Description = "*"
    m_sngAttachH = vData
End Property

Public Property Get AttachH() As Single
    AttachH = m_sngAttachH
End Property

Public Property Let AttachV(ByVal vData As Single)
Attribute AttachV.VB_Description = "*"
    m_sngAttachV = vData
End Property

Public Property Get AttachV() As Single
    AttachV = m_sngAttachV
End Property

Public Property Get Blocked(ByVal X As Single, ByVal Y As Single) As Boolean
Attribute Blocked.VB_Description = "Checks to see if a given location would be blocked if this sprite were to be there."
On Error Resume Next
    Blocked = Map.Blocked(Layer, X - (m_spParam.Obstruction.Width / 2), Y - (m_spParam.Obstruction.Height / IIf(m_lngBlockingType >= 1, 2, 1)), m_spParam.Obstruction.Width, m_spParam.Obstruction.Height, Me)
End Property

Public Sub AttachGraphic(Graphic)
Attribute AttachGraphic.VB_Description = "Attaches a graphic to this sprite."
On Error Resume Next
Dim l_gphGraphic As Fury2Graphic
    Set l_gphGraphic = Graphic
    Set AttachedGraphic = l_gphGraphic
    RefreshAttachedGraphic
    Err.Clear
End Sub

Sub RefreshAttachedGraphic()
Attribute RefreshAttachedGraphic.VB_Description = "Refreshes the graphic attached to this sprite, if it has changed."
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
Attribute Refresh.VB_Description = "Refreshes the state of the sprite."
On Error Resume Next
    UpdateGraphic
End Sub

Public Property Get WalkState() As Long
Attribute WalkState.VB_Description = "The state used while the sprite is in motion."
    WalkState = m_lngWalkState
End Property

Public Property Let WalkState(ByVal NewState As Long)
On Error Resume Next
    If State = m_lngWalkState Then
        State = NewState
    End If
    m_lngWalkState = NewState
End Property

Public Property Get IdleState() As Long
Attribute IdleState.VB_Description = "The state used when the sprite is not in motion."
    IdleState = m_lngIdleState
End Property

Public Property Let IdleState(ByVal NewState As Long)
On Error Resume Next
    If State = m_lngIdleState Then
        State = NewState
    End If
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

Public Property Get RelativeRotation() As Boolean
Attribute RelativeRotation.VB_Description = "If DisplayRotation is set, causes the display rotation to be based on the angle of the currently autoselected pose to approximate intermediate poses."
    RelativeRotation = m_booRelativeRotation And m_booDisplayRotation
End Property

Public Property Let RelativeRotation(ByVal Value As Boolean)
    m_booRelativeRotation = Value
End Property

Public Property Get Destroyed() As Boolean
Attribute Destroyed.VB_Description = "*This value is true if the sprite has been destroyed by a Die command."
    Destroyed = m_booDestroyed
End Property

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Sprite: " & m_strName
End Function

Function SupportsAction(ByVal Name As String) As Boolean
Attribute SupportsAction.VB_Description = "Returns True if the specified action is supported by the sprite's script."
On Error Resume Next
    With mvarEngine.ScriptEngine.Language
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
        Case "die"
            SupportsAction = m_booScript_Die
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
Attribute Action.VB_Description = "Performs an action on the sprite's script."
On Error Resume Next
    Action = True
    mvarEngine.ContextLevelAdd "Sprite_" + ActionName
    If m_objScript Is Nothing Then Action = False
    If m_objScript Is Nothing Then Else CallByName m_objScript, ActionName, VbMethod, Activator
    If Err.Number <> 0 Then
        Err.Clear
        If m_objScript Is Nothing Then Else CallByName m_objScript, ActionName, VbMethod
        If Err.Number <> 0 Then Action = False
    End If
    mvarEngine.ContextLevelRemove
End Function

Function ActionAhead(Optional ByVal Distance As Single = -1, Optional ByVal ActionName As String = "Activate", Optional ByVal Class As Long = -1) As Boolean
Attribute ActionAhead.VB_Description = "Attempts to perform an action on a sprite ahead of the current sprite."
On Error Resume Next
Dim l_objTarget As Object
Dim l_sprTarget As Fury2Sprite, l_araTarget As Fury2Area
    If Distance = -1 Then
        Distance = mvarEngine.ActivationDistance
    End If
    Set l_objTarget = Me.ObjectCollisionCheck(Sin(m_spParam.Velocity.B * c_dblRadian) * Distance, -Cos(m_spParam.Velocity.B * c_dblRadian) * Distance, Class)
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
On Error Resume Next
    m_spParam.Params.Alpha = NewValue
End Property

Public Property Get Angle() As Single
Attribute Angle.VB_Description = "The direction the sprite is facing. (Also affects Pose unless PoseOverride is True)"
    Angle = m_spParam.Velocity.B
End Property

Public Property Let Angle(ByVal NewValue As Single)
On Error Resume Next
    If NewValue = m_spParam.Velocity.B Then Exit Property
    m_spParam.Velocity.B = NewValue
    If m_booDisplayRotation Then
        If m_booRelativeRotation Then
            Dim l_lngHeading As Long, l_sngHeading As Single
            If m_staState.DirectionPoses > 0 Then
                l_lngHeading = CLng((((Angle * CSng(m_staState.DirectionPoses)) / 360!) + 0!)) Mod m_staState.DirectionPoses
                l_sngHeading = l_lngHeading * (360 / m_staState.DirectionPoses)
                RotationAngle = Angle + RotationOffset - l_sngHeading
            Else
                RotationAngle = Angle + RotationOffset
            End If
        Else
            RotationAngle = Angle + RotationOffset
        End If
    End If
    ReselectPose
End Property

Public Property Let Animate(ByVal NewValue As Boolean)
Attribute Animate.VB_Description = "If True, the sprite will animate, both while moving and while stopped."
On Error Resume Next
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
Attribute PushPath.VB_Description = "Pushes the current path onto the path stack."
On Error Resume Next
    SavedPaths.Add m_pthPath
    Set m_pthPath = New Fury2Path
    m_pthPath.SetParent Me
End Sub

Public Sub PopPath()
Attribute PopPath.VB_Description = "Pops a path from the path stack."
On Error Resume Next
    Set m_pthPath = SavedPaths.Item(SavedPaths.Count)
    SavedPaths.Remove SavedPaths.Count
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Set SavedPaths = New Fury2Collection
    Set m_colPaths = New Fury2Collection
    Set m_posPoses = New Fury2Poses
    m_posPoses.SetParent Me
    Set m_staStates = New Fury2States
    Set m_pthPath = New Fury2Path
    m_spParam.Velocity.CFM = 1
    m_pthPath.SetParent Me
    m_colPaths.Add m_pthPath
    m_lngAnimationDirection = 1
    m_lngState = 1
    m_lngPose = 1
    m_lngFrame = 1
    m_sngVelocityDecay = -1
    m_sngVelocityAccel = -1
    PathIndex = 1
    Alpha = 1
    ScaleLevel = 1
    Visible = True
    LinkState = True
    LinkPose = True
    LinkFrame = True
    LinkProperties = True
    Name = "Sprite"
    SpriteType = F2ST_Entity
    VelocityMultiplier = -32767
    AnimationMultiplier = -32767
    m_spParam.Obj = ObjPtr(Me)
    m_spOldParam = m_spParam
'    SpriteCount = SpriteCount + 1
'    SpriteID = SpriteID + 1
'    SpriteID_ = SpriteID
'    Debug.Print "Fury2Sprite_Initialize(" & SpriteID_ & ")"
End Sub

Public Sub CopyTo(ByVal OtherSprite, Optional ByVal AsTemplate As Boolean = False, Optional ByVal Fast As Boolean = False)
Attribute CopyTo.VB_Description = "Copies the contents of this sprite to another sprite, optionally performing a template-style copy or a fast copy."
On Error Resume Next
Dim l_sprOther As Fury2Sprite
Dim l_spOld As SpriteParam
Dim l_pthPath As Fury2Path
    Set l_sprOther = OtherSprite
    If l_sprOther Is Nothing Then Exit Sub
    With l_sprOther
        If Fast Then
            Set .States = Me.States.Clone
            Set .Poses = Me.Poses.Clone
        Else
            Set .States = Me.States.Duplicate
            Set .Poses = Me.Poses.Duplicate
        End If

        l_spOld = .GetParam
        .SetParam m_spParam
        If AsTemplate Then
            If Trim(.Name) = "" Then
                .Name = m_strName
            End If
        Else
            .Name = m_strName
        End If
        If Not Fast Then
            .DepthmapOverride = Me.DepthmapOverride
            .PoseOverride = Me.PoseOverride
        End If

        .Alpha = Me.Alpha
        .Angle = Me.Angle
        .AnimateWhileMoving = Me.AnimateWhileMoving
        .AnimateWhileStopped = Me.AnimateWhileStopped
        .AnimationDelay = Me.AnimationDelay
        .AnimationDirection = Me.AnimationDirection
        .AnimationMultiplier = Me.AnimationMultiplier
        .Class = Me.Class
        .Effect = Me.Effect
        If Not Fast Then
            .FadeTarget = Me.FadeTarget
            .FadeVelocity = Me.FadeVelocity
        End If
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

        .LinkBearing = Me.LinkBearing
        .LinkFrame = Me.LinkFrame
        .LinkPose = Me.LinkPose
        .LinkProperties = Me.LinkProperties
        .LinkState = Me.LinkState

        .DisplayRotation = Me.DisplayRotation
        .RelativeRotation = Me.RelativeRotation
        .RotationAngle = Me.RotationAngle
        .RotationOffset = Me.RotationOffset
        .ScaleLevel = Me.ScaleLevel
        .Solid = Me.Solid
        If Not Fast Then
            .SpinTarget = Me.SpinTarget
            .SpinVelocity = Me.SpinVelocity
        End If
        .SpriteType = Me.SpriteType
        If Not Fast Then
            .Velocity = Me.Velocity
            .VelocityMultiplier = Me.VelocityMultiplier
        End If
        .Weight = Me.Weight
        .Pushable = Me.Pushable
        .CanPush = Me.CanPush
        If Not Fast Then
            .XVelocity = Me.XVelocity
            .YVelocity = Me.YVelocity
            .ZVelocity = Me.ZVelocity
            .VelocityDecay = m_sngVelocityDecay
            .VelocityAccel = m_sngVelocityAccel
        End If

        .Tag = Me.Tag

        .IdleState = Me.IdleState
        .WalkState = Me.WalkState

        .AIUpdateRate = Me.AIUpdateRate

        If Not Fast Then
            .Paths.Clear
            For Each l_pthPath In Paths
                .Paths.Add l_pthPath.Duplicate()
            Next l_pthPath
            Set .Path = .Paths(1)
        End If

        If .ScriptOverride And AsTemplate Then
        Else
            .ScriptSource = Me.ScriptSource
        End If

        If Not Fast Then
            If AsTemplate Then
            Else
                .Template = m_strTemplate
            End If
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
Attribute Die.VB_Description = "Destroys the current sprite, removing it from its map and unloading its associated objects."
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    m_booDestroyed = True
    mvarEngine.ContextLevelAdd Name + " : Die"
    If m_booScript_Die Then
        If (m_lngID_Die <> -1) Then
            mvarEngine.InvokeMember m_objScript, m_lngID_Die
        Else
            m_objScript.Die
        End If
    End If
    For Each l_lyrLayer In m_mapMap.Layers
        l_lyrLayer.Sprites.DelayRemove Me
    Next l_lyrLayer
    m_mapMap.Sprites.Remove Me
    Free
    mvarEngine.ContextLevelRemove
End Sub

Public Function Duplicate() As Fury2Sprite
Attribute Duplicate.VB_Description = "Creates a new deep copy of the sprite."
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Set l_sprSprite = New Fury2Sprite
    CopyTo l_sprSprite, False
    Set Duplicate = l_sprSprite
End Function

Public Function Clone() As Fury2Sprite
Attribute Clone.VB_Description = "Creates a clone of the sprite. A clone shares some objects with the object it was cloned from, and is faster to create than a duplicate."
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Set l_sprSprite = New Fury2Sprite
    CopyTo l_sprSprite, False, True
    Set Clone = l_sprSprite
End Function

Public Property Let Effect(ByVal NewValue As Fury2SpriteEffects)
Attribute Effect.VB_Description = "The rendering effect used to draw the sprite."
    m_spParam.Params.Effect = NewValue
End Property

Public Property Get Effect() As Fury2SpriteEffects
    Effect = m_spParam.Params.Effect
End Property

Public Property Let BlockingType(ByVal NewValue As Fury2SpriteBlockingTypes)
Attribute BlockingType.VB_Description = "Specifies the type of blocking this sprite has. If set to CenteredPolygon, this sprite uses polygonal collision."
    m_lngBlockingType = NewValue
    UpdateObstruction
End Property

Public Property Get BlockingType() As Fury2SpriteBlockingTypes
    BlockingType = m_lngBlockingType
End Property

Public Sub Face(ByRef Point)
Attribute Face.VB_Description = "Rotates the sprite to face a given point."
On Error Resume Next
    If VarType(Point) = vbObject Then
        Angle = mvarEngine.Math.AngleBetween(X, Y, Point.X, Point.Y)
    ElseIf (VarType(Point) And vbArray) = vbArray Then
        Angle = mvarEngine.Math.AngleBetween(X, Y, Point(0), Point(1))
    End If
End Sub

Public Sub Fade(Optional ByVal FromAlpha = -1, Optional ByVal ToAlpha = -1, Optional ByVal Speed = 1 / 60)
Attribute Fade.VB_Description = "Begins fading the sprite's alpha to a new value."
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
On Error Resume Next
    If NewValue = m_lngFrame Then Exit Property
    m_lngFrame = NewValue
    UpdateFrame
End Property

Public Property Get Frame() As Long
    Frame = m_lngFrame
End Property

Public Sub Free()
Attribute Free.VB_Description = "Frees the sprite and its associated objects."
On Error Resume Next
Dim l_posPose As Fury2Pose
Dim l_pthPath As Fury2Path
    Set m_staState = Nothing
    Set m_posPose = Nothing
    Set m_fraFrame = Nothing
    If Not (m_posPoses Is Nothing) Then
        If m_posPoses.Parent Is Me Then
            m_posPoses.Free
            m_posPoses.Clear
        End If
    End If
    If Not (m_pthPath Is Nothing) Then
        m_pthPath.Free
    End If
    For Each l_pthPath In Paths
        l_pthPath.Free
    Next l_pthPath
    Paths.Clear
    If Not (m_staStates Is Nothing) Then
        m_staStates.Clear
    End If
    Set SavedPaths = Nothing
    Set AttachedGraphic = Nothing
    Set m_posPoses = Nothing
    Set m_staStates = Nothing
    Set m_pthPath = Nothing
    FreeScript
    Set m_sprAttached = Nothing
    'WRAssign m_sprSprites, Nothing
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
    Set l_objScript = Script

    l_objScript.Init

    l_objScript.Initialize

    If m_booDestroyed Then Exit Sub
    For Each l_posPose In m_posPoses
        l_posPose.SetParent Me
    Next l_posPose
End Sub

Private Sub Class_Terminate()
On Error Resume Next
'    SpriteCount = SpriteCount - 1
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_booValue As Boolean, l_sngValue As Single, l_lngValue As Long, l_bytValue As Byte
Dim l_lngIndex As Long
Dim l_sngStateVelocity As Single, l_staState As Fury2State
Dim l_pthPath As Fury2Path
    With File
        m_booLoaded = False
        Paths.Clear
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
                .RawLoad VarPtr(m_spParam.Velocity.VM), 4
                .RawLoad VarPtr(m_spParam.Velocity.XF), 4
                .RawLoad VarPtr(m_spParam.Velocity.YF), 4
                .RawLoad VarPtr(m_spParam.Velocity.FW), 4
                .Load m_sngVelocityDecay
                If (m_sngVelocityDecay <= 0) Then
                    m_sngVelocityDecay = -1
                End If
                .Load m_sngVelocityAccel
                If (m_sngVelocityAccel <= 0) Then
                    m_sngVelocityAccel = -1
                End If
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
                l_booValue = True
                If Not .EOS Then .Load l_booValue
                LinkState = l_booValue
                l_booValue = True
                If Not .EOS Then .Load l_booValue
                LinkPose = l_booValue
                l_booValue = True
                If Not .EOS Then .Load l_booValue
                LinkFrame = l_booValue
                l_booValue = True
                If Not .EOS Then .Load l_booValue
                LinkProperties = l_booValue
                l_booValue = True
                If Not .EOS Then
                    .Load l_booValue
                    LinkBearing = l_booValue
                Else
                    LinkBearing = LinkProperties
                End If
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
                m_spParam.Stats.CanPush = CByte(Abs(l_booValue))
                .Load m_lngBlockingType
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
                .Load m_booDisplayRotation
                .Load l_lngValue
                m_spParam.Params.RenderTarget = CByte(l_lngValue)
                .Load m_sngRotationOffset
                .Load m_booRelativeRotation
            .ReadSegment_End
            .ReadSegment_Begin
                .Load m_strScript
                .Load m_booScriptOverride
                l_booValue = False
                .Load l_booValue
                SaveScriptData = l_booValue
                .ReadSegment_Begin
                If SaveScriptData Then
                    .Load Script
                End If
                Err.Clear
                .ReadSegment_End
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

            Set m_posPoses = New Fury2Poses
            m_posPoses.SetParent Me
            .Load m_posPoses

            ' path
            Set m_pthPath = New Fury2Path
            m_pthPath.SetParent Me
            .Load m_pthPath
            Paths.Add m_pthPath

            ' paths
            .ReadSegment_Begin
                l_lngValue = 0
                .Load l_lngValue
                If l_lngValue > 0 Then
                    For l_lngIndex = 1 To l_lngValue
                        Set l_pthPath = New Fury2Path
                        l_pthPath.SetParent Me
                        .Load l_pthPath
                        Paths.Add l_pthPath
                    Next l_lngIndex
                End If
            .ReadSegment_End

            .Load m_staStates
        .ReadSegment_End
        VelocityMultiplier = -32767
        AnimationMultiplier = -32767
        m_spOldParam = m_spParam
        LoadTemplate
        UpdateState True
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_pthPath As Fury2Path
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
                .RawSave VarPtr(m_spParam.Velocity.VM), 4
                .RawSave VarPtr(m_spParam.Velocity.XF), 4
                .RawSave VarPtr(m_spParam.Velocity.YF), 4
                .RawSave VarPtr(m_spParam.Velocity.FW), 4
                .Save m_sngVelocityDecay
                .Save m_sngVelocityAccel
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
                .Save LinkState
                .Save LinkPose
                .Save LinkFrame
                .Save LinkProperties
                .Save LinkBearing
            .WriteSegment_End
            .WriteSegment_Begin
                .Save CBool(m_spParam.Stats.Solid)
                .Save m_spParam.Stats.Weight
                .Save CBool(m_spParam.Stats.Pushable)
                .Skip 2
                .Skip 2
                .Save CBool(m_spParam.Stats.CanPush)
                .Save m_lngBlockingType
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
                .Save m_booDisplayRotation
                .Save CLng(m_spParam.Params.RenderTarget)
                .Save m_sngRotationOffset
                .Save m_booRelativeRotation
            .WriteSegment_End
            .WriteSegment_Begin
                .Save m_strScript
                .Save m_booScriptOverride
                .Save SaveScriptData
                .WriteSegment_Begin
                If SaveScriptData Then
                    .Save Script
                End If
                Err.Clear
                .WriteSegment_End
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

            ' paths
            .WriteSegment_Begin
                .Save Paths.Count - 1
                For Each l_pthPath In Paths
                    If l_pthPath Is Paths(1) Then
                    Else
                        .Save l_pthPath
                    End If
                Next l_pthPath
            .WriteSegment_End

            .Save m_staStates

        .WriteSegment_End
    End With
End Sub

Friend Sub SetLayer(ByVal NewLayer As Long)
    m_lngLayer = NewLayer
End Sub

Public Property Let Layer(ByVal NewValue As Long)
On Error Resume Next
    Parent.Layers(m_lngLayer).Sprites.MoveTo Index, NewValue
End Property

Public Property Get Layer() As Long
Attribute Layer.VB_Description = "*The numeric index of the layer containing the sprite (if any)."
    Layer = m_lngLayer
End Property

Public Sub Load()
Attribute Load.VB_Description = "Loads the sprite's configuration data."
On Error Resume Next
    If Engine.EditMode Then
    Else
        If Not m_booLoaded Then
            Script.Load
            m_booLoaded = True
        End If
    End If
    If m_booDestroyed Then Exit Sub
    m_posPoses.LoadGraphics
    UpdateState True

    InitAreas
'    UpdatePose True
'    UpdateFrame True
End Sub

Public Sub LoadTemplate()
Attribute LoadTemplate.VB_Description = "Loads the sprite's template and applies it."
On Error Resume Next
Dim l_sprTemplate As Fury2Sprite, l_strTemplateSprite As String, l_strTemplateIndex As String
    If Trim(m_strTemplate) = "" Then
    Else
        l_strTemplateSprite = left(m_strTemplate, InStrRev(m_strTemplate, ":") - 1)
        l_strTemplateIndex = Mid(m_strTemplate, InStrRev(m_strTemplate, ":") + 1)
        Set l_sprTemplate = mvarEngine.LoadSprite(l_strTemplateSprite, l_strTemplateIndex)
        If l_sprTemplate Is Nothing Then
        Else
            l_sprTemplate.CopyTo Me, True
            l_sprTemplate.Free
            Initialize
            Load
        End If
    End If
End Sub

Public Property Let Name(NewName As String)
Attribute Name.VB_Description = "The sprite's name."
On Error Resume Next
    m_strName = NewName
End Property

Public Property Get Name() As String
    Name = m_strName
End Property

Public Function ObjectCollisionCheck(Optional ByVal XOffset As Single = 0, Optional ByVal YOffset As Single = 0, Optional ByVal Class As Long = -1) As Object
Attribute ObjectCollisionCheck.VB_Description = "Performs a collision check and returns the object that was collided with."
On Error Resume Next
Dim l_rctThis As Fury2Rect, l_rctThat As Fury2Rect
Dim l_sprSprite As Fury2Sprite, l_araArea As Fury2Area
    Set l_rctThis = Me.Rectangle(False)
    l_rctThis.Translate CLng(XOffset), CLng(YOffset)

    For Each l_sprSprite In m_sprSprites
        If l_sprSprite Is Me Then
        Else
            If Class = -1 Or (l_sprSprite.Class = Class) Then
                Set l_rctThat = l_sprSprite.Rectangle(False)
                If l_rctThat.Intersect(l_rctThis) Then
                    Set ObjectCollisionCheck = l_sprSprite
                    Err.Clear
                    Exit Function
                End If
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

Friend Sub SetParent(Parent As Fury2Map)
    Set m_mapMap = Parent
End Sub

Public Property Get Parent() As Fury2Map
Attribute Parent.VB_Description = "*The sprite's parent (if any)."
    Set Parent = m_mapMap
End Property

Public Function PerformAction(Optional ByVal Activator = Nothing, Optional ByRef Action = "Activate") As Boolean
Attribute PerformAction.VB_Description = "Performs an action on the sprite's script. Returns True if successful."
On Error Resume Next
    mvarEngine.ContextLevelAdd Name + " : PerformAction(" + Action + ")"
    Err.Clear
    CallByName m_objScript, Action, VbMethod, Activator
    If Err.Number <> 0 Then
        Err.Clear
        CallByName m_objScript, Action, VbMethod
        If Err.Number <> 0 Then PerformAction = False
    End If
    Err.Clear
    mvarEngine.ContextLevelRemove
End Function

Public Property Get Pointer() As Long
Attribute Pointer.VB_Description = "*A numeric pointer to the sprite's internal representation."
On Error Resume Next
    Pointer = VarPtr(m_spParam)
End Property

Public Property Get Paths() As Fury2Collection
Attribute Paths.VB_Description = "The paths defined for this sprite."
On Error Resume Next
    Set Paths = m_colPaths
End Property

Public Property Get Path() As Fury2Path
Attribute Path.VB_Description = "The sprite's path."
    Set Path = m_pthPath
End Property

Public Property Set Path(ByVal NewPath As Fury2Path)
    Set m_pthPath = NewPath
End Property

Public Sub SwitchPath(ByVal ID As Variant)
Attribute SwitchPath.VB_Description = "Switches the current path to a new one."
On Error Resume Next
    PathIndex = CLng(ID)
    Set m_pthPath = Paths(ID)
End Sub

Public Property Let Pose(ByVal NewValue As Long)
Attribute Pose.VB_Description = "The current pose."
On Error Resume Next
    If NewValue = m_lngPose Then Exit Property
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

Public Property Set Poses(ByVal NewValue As Fury2Poses)
On Error Resume Next
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
Attribute PointInside.VB_Description = "Returns True if the specified point is inside the sprite. If Onscreen is true, the point is in display coordinates, otherwise it is in blocking coordinates."
On Error Resume Next
    If Onscreen Then
        If X < Floor(m_spParam.Position.X - (m_spParam.Graphic.Rectangle.Width / 2)) Then Exit Function
        If Y < Floor(m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height)) Then Exit Function
        If X >= Floor(m_spParam.Position.X + (m_spParam.Graphic.Rectangle.Width / 2)) Then Exit Function
        If Y >= Floor(m_spParam.Position.Y) Then Exit Function
    Else
        If m_lngBlockingType >= 1 Then
            If X < Floor(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2)) Then Exit Function
            If Y < Floor(m_spParam.Position.Y - (m_spParam.Obstruction.Height / 2)) Then Exit Function
            If X >= Floor(m_spParam.Position.X + (m_spParam.Obstruction.Width / 2)) Then Exit Function
            If Y >= Floor(m_spParam.Position.Y + (m_spParam.Obstruction.Height / 2)) Then Exit Function
        Else
            If X < Floor(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2)) Then Exit Function
            If Y < Floor(m_spParam.Position.Y - (m_spParam.Obstruction.Height)) Then Exit Function
            If X >= Floor(m_spParam.Position.X + (m_spParam.Obstruction.Width / 2)) Then Exit Function
            If Y >= Floor(m_spParam.Position.Y) Then Exit Function
        End If
    End If
    PointInside = True
End Function

Public Property Get Rectangle(Optional ByVal Onscreen As Boolean = True) As Fury2Rect
Attribute Rectangle.VB_Description = "*Returns a rectangle enclosing the sprite."
On Error Resume Next
    If Onscreen Then
        Set Rectangle = F2Rect(Floor(m_spParam.Position.X - m_spParam.Graphic.XCenter), Floor(m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height) + (m_spParam.Graphic.Rectangle.Height - m_spParam.Graphic.YCenter)), m_spParam.Graphic.Rectangle.Width, m_spParam.Graphic.Rectangle.Height, False)
    Else
        If (m_lngBlockingType = 1) Or (m_lngBlockingType = 2) Or (m_lngBlockingType = 4) Then
            Set Rectangle = F2Rect(Ceil(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2)), Ceil(m_spParam.Position.Y - (m_spParam.Obstruction.Height / 2)), m_spParam.Obstruction.Width, m_spParam.Obstruction.Height, False)
        Else
            Set Rectangle = F2Rect(Ceil(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2)), Ceil(m_spParam.Position.Y - (m_spParam.Obstruction.Height)), m_spParam.Obstruction.Width, m_spParam.Obstruction.Height, False)
        End If
    End If
End Property

Public Sub Render(ByVal Surface As Fury2Image, ByVal X As Long, ByVal Y As Long)
Attribute Render.VB_Description = "Renders the sprite to an image."
On Error Resume Next
Dim m_cpCamera As CameraParam
Dim m_spSprite As SpriteParam
Dim l_lngTargets(0 To 0) As Long
Dim l_optOptions As RenderSpritesParam
    m_spSprite = m_spParam
    m_cpCamera.Rectangle = Surface.Rectangle.GetRectangle
    l_lngTargets(0) = Surface.Handle
    m_cpCamera.pRenderTargets = VarPtr(l_lngTargets(0))
    m_cpCamera.RenderTargetCount = 1
    m_cpCamera.ViewportX = 0
    m_cpCamera.ViewportY = 0
    m_cpCamera.ParallaxX = 1
    m_cpCamera.ParallaxY = 1
    m_cpCamera.Alpha = 255
    m_spSprite.Position.X = X + Floor(m_spSprite.Graphic.Rectangle.Width / 2) + (Floor(m_spSprite.Graphic.Rectangle.Width / 2) - m_spSprite.Graphic.XCenter)
    m_spSprite.Position.Y = Y + m_spSprite.Graphic.Rectangle.Height - (m_spSprite.Graphic.Rectangle.Height - m_spSprite.Graphic.YCenter)
    m_spSprite.SortedNext = 0
    m_spSprite.Next = 0
    l_optOptions.DrawFrames = 1
    l_optOptions.DrawSecondaryImages = 1
    RenderSprites VarPtr(m_spSprite), m_cpCamera, l_optOptions
End Sub

Private Sub ReselectPose()
On Error Resume Next
Dim l_lngHeading As Long
Dim l_lngOldPose As Long
    If m_booPoseOverride Then Exit Sub
    If m_staState.DirectionPoses > 0 Then
        l_lngHeading = CLng((((Angle * CSng(m_staState.DirectionPoses)) / 360!) + 0!)) Mod m_staState.DirectionPoses
        l_lngOldPose = m_lngPose
        Pose = (l_lngHeading) + 1
        UpdatePose
    End If
End Sub

Friend Sub ResetPositionHistory()
On Error Resume Next
    m_spOldParam = m_spParam
End Sub

Public Property Get RotationAngle() As Single
Attribute RotationAngle.VB_Description = "The angle to rotate the sprite's onscreen display by."
    RotationAngle = m_spParam.Params.RotationAngle
End Property

Public Property Let RotationAngle(ByVal NewValue As Single)
    m_spParam.Params.RotationAngle = NewValue
End Property

Public Property Get RotationOffset() As Single
    RotationOffset = m_sngRotationOffset
End Property

Public Property Let RotationOffset(ByVal NewValue As Single)
    m_sngRotationOffset = NewValue
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
            If left(Trim(m_strScript), 2) = "<@" Then
                l_strScript = Trim(m_strScript)
                l_strName = Mid(l_strScript, 3, Len(l_strScript) - 3)
                Set m_objScript = mvarEngine.ScriptEngine.EvalObject(l_strName)
                m_booScript_Update = m_objScript.ImplementsMethod("Update")
                m_booScript_Hit = m_objScript.ImplementsMethod("Hit")
                m_booScript_HitMap = m_objScript.ImplementsMethod("HitMap")
                m_booScript_Decay = m_objScript.ImplementsMethod("Decay")
                m_booScript_AIUpdate = m_objScript.ImplementsMethod("AIUpdate")
                m_booScript_Activate = m_objScript.ImplementsMethod("Activate")
                m_booScript_Load = m_objScript.ImplementsMethod("Load")
                m_booScript_Die = m_objScript.ImplementsMethod("Die")
                m_booScript_Step = m_objScript.ImplementsMethod("Step")
                m_booScript_FadeOut = m_objScript.ImplementsMethod("FadeOut")
                m_booScript_WaypointReached = m_objScript.ImplementsMethod("WaypointReached")
            Else
                With mvarEngine.ScriptEngine.Language
                    If left(Trim(m_strScript), 2) = "<#" Then
                        l_strScript = Trim(m_strScript)
                        l_strScript = Mid(l_strScript, 3, Len(l_strScript) - 3)
                        l_strScript = mvarEngine.GetFile(mvarEngine.Filesystem.AutoSelectExtension(l_strScript, "f2script")) & .LineTerminator & _
                        .GenerateClassVariable("Object") & .LineTerminator & _
                        .GenerateClassVariable("Map") & .LineTerminator & _
                        .GenerateClassMethod("F2OInit", Array("newThis", "newMap"), _
                        .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "newThis", True) & .LineTerminator & _
                        .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "newMap", True) & .LineTerminator _
                        ) & .LineTerminator
                    Else
                        l_strScript = m_strScript & .LineTerminator & _
                        .GenerateClassVariable("Object") & .LineTerminator & _
                        .GenerateClassVariable("Map") & .LineTerminator & _
                        .GenerateClassMethod("F2OInit", Array("newThis", "newMap"), _
                        .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "newThis", True) & .LineTerminator & _
                        .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "newMap", True) & .LineTerminator _
                        ) & .LineTerminator
                    End If
                    m_booScript_Update = .ImplementsMethod(l_strScript, "Update")
                    m_booScript_Hit = .ImplementsMethod(l_strScript, "Hit")
                    m_booScript_HitMap = .ImplementsMethod(l_strScript, "HitMap")
                    m_booScript_Decay = .ImplementsMethod(l_strScript, "Decay")
                    m_booScript_AIUpdate = .ImplementsMethod(l_strScript, "AIUpdate")
                    m_booScript_Activate = .ImplementsMethod(l_strScript, "Activate")
                    m_booScript_Load = .ImplementsMethod(l_strScript, "Load")
                    m_booScript_Die = .ImplementsMethod(l_strScript, "Die")
                    m_booScript_Step = .ImplementsMethod(l_strScript, "Step")
                    m_booScript_FadeOut = .ImplementsMethod(l_strScript, "FadeOut")
                    m_booScript_WaypointReached = .ImplementsMethod(l_strScript, "WaypointReached")
                    Set m_objScript = mvarEngine.GenerateScriptObject(l_strScript)
                End With
            End If
            Set mvarEngine.IObjThis = Me

            m_objScript.F2OInit Me, m_mapMap

            If (m_objScript Is Nothing) Then
                Engine.DebugOut "Unable to load script for sprite """ & Name & """"
                m_booScriptLoadFailed = True
            End If
        End If
    End If
    BindScript
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
    If l_lngPose >= 1 Then
        If m_lngPose <> l_lngPose Then
            m_lngPose = l_lngPose
            UpdatePose True
        End If
    End If
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
Attribute ZHeight.VB_Description = "Sets the Z height value used in lighting calculations."
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

Public Property Set States(ByVal NewValue As Fury2States)
On Error Resume Next
    Set m_staStates = NewValue
    UpdateState True
End Property

Public Property Get States() As Fury2States
Attribute States.VB_Description = "~The sprite's defined states."
    Set States = m_staStates
End Property

Public Sub Teleport(ByVal X, ByVal Y, Optional ByVal Z = 0, Optional ByVal Layer = -1)
Attribute Teleport.VB_Description = "Teleports the sprite to a new location."
On Error Resume Next
    If Layer <> -1 Then
        If Layer <> m_lngLayer Then
            m_mapMap.Layers(m_lngLayer).Sprites.MoveTo m_spParam.Index, CLng(Layer)
        End If
    End If
    m_spParam.Position.X = CSng(X)
    m_spParam.Position.Y = CSng(Y)
    ResetPositionHistory
End Sub

Public Property Let Template(NewTemplate As String)
Attribute Template.VB_Description = "{Path}The template to sync the sprite with. Format: (filename):(spritename)"
On Error Resume Next
    m_strTemplate = NewTemplate
    LoadTemplate
End Property

Public Property Get Template() As String
    Template = m_strTemplate
End Property

Friend Sub WaypointReached(ByVal Waypoint As Fury2Waypoint)
On Error Resume Next
    If m_booScript_WaypointReached Then
        mvarEngine.ContextLevelAdd "Sprite_WaypointReached"
        If (m_lngID_WaypointReached <> -1) Then
            mvarEngine.InvokeMember m_objScript, m_lngID_WaypointReached, Array(Waypoint)
        Else
            m_objScript.WaypointReached Waypoint
        End If
        mvarEngine.ContextLevelRemove
    End If
End Sub

Public Sub Update()
Attribute Update.VB_Description = "Updates the sprite."
On Error Resume Next
Dim l_sprCollided As Fury2Sprite
    If Not Ignore Then
        m_lngAIDelay = m_lngAIDelay + 1
        If m_lngAIDelay > m_lngAIUpdateRate Then
            m_lngAIDelay = 0
            If m_booScript_AIUpdate Then
                mvarEngine.ContextLevelAdd "Sprite_AIUpdate"
                If (m_lngID_AIUpdate <> -1) Then
                    mvarEngine.InvokeMember m_objScript, m_lngID_AIUpdate
                Else
                    m_objScript.AIUpdate
                End If
                mvarEngine.ContextLevelRemove
            End If
        End If
    End If

    If AttachedGraphic Is Nothing Then
    Else
        AttachedGraphic.Update
    End If

    'm_spParam.Params.SpecialFX = fxSoftShadow
    If m_booDisplayRotation Then
        If m_booRelativeRotation Then
            Dim l_lngHeading As Long, l_sngHeading As Single
            If m_staState.DirectionPoses > 0 Then
                l_lngHeading = CLng((((Angle * CSng(m_staState.DirectionPoses)) / 360!) + 0!)) Mod m_staState.DirectionPoses
                l_sngHeading = l_lngHeading * (360 / m_staState.DirectionPoses)
                RotationAngle = Angle + RotationOffset - l_sngHeading
            Else
                RotationAngle = Angle + RotationOffset
            End If
        Else
            RotationAngle = Angle + RotationOffset
        End If
    End If

    If m_spParam.Events.Moved Then
        UpdateAreas
    End If

    If SpinVelocity <> 0 Then
        ReselectPose
    End If

    UpdateAnimation

    If Not Ignore Then
        UpdatePath

        If (CLng(m_spParam.Position.X) <> CLng(m_spOldParam.Position.X)) Or (CLng(m_spParam.Position.Y) <> CLng(m_spOldParam.Position.Y)) Then
            If m_booScript_Step Then
                mvarEngine.ContextLevelAdd "Sprite_Step"
                Err.Clear
                If (m_lngID_Step <> -1) Then
                    mvarEngine.InvokeMember m_objScript, m_lngID_Step
                Else
                    m_objScript.Step
                End If
                Err.Clear
                mvarEngine.ContextLevelRemove
            End If
        End If

        If m_spParam.Events.FadedOut Then
            mvarEngine.ContextLevelAdd "Sprite_FadeOut"
            Err.Clear
            If (m_lngID_FadeOut <> -1) Then
                mvarEngine.InvokeMember m_objScript, m_lngID_FadeOut
            Else
                m_objScript.FadeOut
            End If
            Err.Clear
            mvarEngine.ContextLevelRemove
        End If

        If m_spParam.Events.CollidedWith <> 0 Then
            mvarEngine.ContextLevelAdd "Sprite_Hit"
            Set l_sprCollided = m_mapMap.Layers(m_lngLayer).Sprites(m_spParam.Events.CollidedWith)

            Err.Clear
            If (m_lngID_Hit <> -1) Then
                mvarEngine.InvokeMember m_objScript, m_lngID_Hit, Array(l_sprCollided)
            Else
                m_objScript.Hit l_sprCollided
            End If
            If Err <> 0 Then
                Err.Clear
                If (m_lngID_Hit <> -1) Then
                    mvarEngine.InvokeMember m_objScript, m_lngID_Hit
                Else
                    m_objScript.Hit
                End If
            End If
            Err.Clear
            l_sprCollided.Script.Hit Me
            If Err <> 0 Then
                Err.Clear
                l_sprCollided.Script.Hit
            End If
            mvarEngine.ContextLevelRemove
        ElseIf m_spParam.Events.CollidedWithMap Then
            mvarEngine.ContextLevelAdd "Sprite_HitMap"
            Err.Clear
            If (m_lngID_HitMap <> -1) Then
                mvarEngine.InvokeMember m_objScript, m_lngID_HitMap
            Else
                m_objScript.HitMap
            End If
            Err.Clear
            mvarEngine.ContextLevelRemove
        End If
    End If

    If m_sprAttached Is Nothing Then
    Else
        With m_sprAttached
            Dim l_spAttached As SpriteParam
            Dim l_varVx As Variant, l_varH As Variant, l_varV As Variant
            Dim l_sngW As Single, l_sngH As Single
            Dim l_sngHp As Single, l_sngVp As Single, l_sngVxw As Single, l_sngHw As Single, l_sngVw As Single
            Dim l_sngAX As Single, l_sngAY As Single
            Dim l_sngSw As Single, l_sngSh As Single
            If (m_sngAttachH <> 0) Or (m_sngAttachV <> 0) Then
                l_spAttached = .GetParam
                l_sngSw = (l_spAttached.Graphic.Rectangle.Width)
                l_sngSh = (l_spAttached.Graphic.Rectangle.Height)
                l_sngW = (l_sngSw / 2) * Abs(l_spAttached.Params.ScaleLevel)
                l_sngH = (l_sngSh / 2) * l_spAttached.Params.ScaleLevel
                l_varVx = Array(0, 0) ' mvarEngine.RotatePoint(0, 0, l_spAttached.Velocity.B) ' -1, -1
                With mvarEngine.Math
                    l_varH = .RotatePoint(l_sngW, 0, l_spAttached.Velocity.B)  ' 1, -1
                    l_varV = .RotatePoint(0, l_sngH, l_spAttached.Velocity.B)  ' -1, 1
                End With
                l_sngHp = (m_sngAttachH) / (l_sngW)
                l_sngVp = (m_sngAttachV) / (l_sngH)
                l_sngVxw = (1 - PClip(Abs(l_sngHp))) * (1 - PClip(Abs(l_sngVp)))
                l_sngHw = (l_sngHp) * (1 - Abs(l_sngVp))
                l_sngVw = (1 - Abs(l_sngHp)) * (l_sngVp)
                l_sngAX = (l_varVx(0) * l_sngVxw) + (l_varH(0) * l_sngHp) + (l_varV(0) * l_sngVw)
                l_sngAY = (l_varVx(1) * l_sngVxw) + (l_varH(1) * l_sngHw) + (l_varV(1) * l_sngVp)
            End If
            m_spParam.Position.X = .X + m_sngAttachX + l_sngAX
            m_spParam.Position.Y = .Y + m_sngAttachY + l_sngAY
            If LinkBearing Then Me.Angle = .Angle
            If LinkBearing And Not DisplayRotation Then Me.RotationAngle = .RotationAngle
            If LinkBearing Then Me.SpinVelocity = .SpinVelocity
            If LinkBearing Then Me.SpinTarget = .SpinTarget
            If LinkProperties Then Me.ScaleLevel = .ScaleLevel
            If LinkProperties Then Me.Alpha = .Alpha
            If LinkProperties Then Me.Visible = .Visible
            If LinkState Then Me.State = .State
            Me.Velocity = .Velocity
            If LinkPose Then Me.Pose = .Pose
            If LinkFrame Then Me.Frame = .Frame
        End With
    End If

    m_spOldParam = m_spParam
    If Not Ignore Then
        If m_booScript_Update Then
            If (m_lngID_Update <> -1) Then
                mvarEngine.InvokeMember m_objScript, m_lngID_Update
            Else
                m_objScript.Update
            End If
        End If
    End If
End Sub

Private Sub UpdatePath()
On Error Resume Next
Dim l_sngPathDistance As Single
Dim l_lngRCX As Long, l_lngRCY As Long, l_lngRPX As Long, l_lngRPY As Long
    If m_pthPath Is Nothing Then Exit Sub
    If m_pthPath.Index <= m_pthPath.Count Then
        If m_pthPath.Current Is Nothing Then Exit Sub
        With m_pthPath.Current
            If .WaitRemainder > 0 Then
                .WaitRemainder = .WaitRemainder - 1
            Else
                If State = IdleState Then State = WalkState
                l_sngPathDistance = mvarEngine.DistanceBetween(m_spParam.Position.X, m_spParam.Position.Y, .X, .Y)
                l_lngRCX = CLng(m_spParam.Position.X)
                l_lngRCY = CLng(m_spParam.Position.Y)
                l_lngRPX = CLng(m_spOldParam.Position.X)
                l_lngRPY = CLng(m_spOldParam.Position.X)
                If (l_lngRPX = l_lngRCX) And (l_lngRPY = l_lngRCY) And (CLng(.X) = l_lngRCX) And (CLng(.Y) = l_lngRCY) Then
                    m_spParam.Position.X = .X
                    m_spParam.Position.Y = .Y
                    m_pthPath.Completed
                    If m_pthPath.Current Is Nothing Then
                        If State = WalkState Then State = IdleState
                        Velocity = 0
                    ElseIf .Wait > 0 Then
                        If State = WalkState Then State = IdleState
                        Velocity = 0
                    End If
                Else
                    If (Abs(l_sngPathDistance) >= (WalkSpeed)) Then
                        Angle = mvarEngine.Math.AngleBetween(m_spParam.Position.X, m_spParam.Position.Y, .X, .Y)
                        Velocity = Velocity + VelocityAccel
                        If Velocity < 0 Then Velocity = 0
                        If Velocity > WalkSpeed Then Velocity = WalkSpeed
                        If Velocity > l_sngPathDistance Then Velocity = l_sngPathDistance
                    Else
                        m_spParam.Position.X = .X
                        m_spParam.Position.Y = .Y
                        m_pthPath.Completed
                        If m_pthPath.Current Is Nothing Then
                            If State = WalkState Then State = IdleState
                            Velocity = 0
                        ElseIf .Wait > 0 Then
                            If State = WalkState Then State = IdleState
                            Velocity = 0
                        End If
                    End If
                End If
            End If
        End With
    Else
    End If
End Sub

Private Sub UpdateAnimation()
On Error Resume Next
Dim l_lngOldFrame As Long
Dim l_lngPrePose As Long, l_lngPreFrame As Long, l_lngPreState As Long
    l_lngOldFrame = m_lngFrame
    If m_posPose Is Nothing Then
    ElseIf m_posPose.Frames Is Nothing Then
    ElseIf m_fraFrame Is Nothing Then
    Else
        If (m_spParam.Events.Moved And m_booAnimateWhileMoving) Or ((Not m_spParam.Events.Moved) And m_booAnimateWhileStopped) Then
            m_sngAnimationDelay = m_sngAnimationDelay + (1 * IIf(AnimationMultiplier = -32767, Engine.AnimationMultiplier, AnimationMultiplier))
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
                    Exit Sub
                End If
                If m_posPose Is Nothing Then
                    Exit Sub
                ElseIf m_posPose.Frames Is Nothing Then
                    Exit Sub
                ElseIf m_fraFrame Is Nothing Then
                    Exit Sub
                End If
                If m_posPose.Frames.Count = 1 Then
                    m_lngFrame = Frame = 1
                Else
                    m_lngFrame = m_lngFrame + m_lngAnimationDirection
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
                            m_lngAnimationDirection = -m_lngAnimationDirection
                            m_lngFrame = m_posPose.Frames.Count - 1
                        ElseIf m_lngFrame < 2 Then
                            m_lngAnimationDirection = -m_lngAnimationDirection
                            m_lngFrame = 3
                        End If
                    Case Else
                    End Select
                End If
            End If
        Else
            m_sngAnimationDelay = 0
            If m_lngFrame <> 1 Then
                m_lngFrame = 1
            End If
        End If
    End If
    If m_lngFrame <> l_lngOldFrame Then
        UpdateFrame
    End If
End Sub

Private Sub UpdateAreas()
On Error Resume Next
Dim l_rctCurrent As Rectangle, l_rctOld As Rectangle
Dim l_araArea As Fury2Area, l_booWasIn As Boolean, l_booIsIn As Boolean
    If m_booAreasUpdated Then Exit Sub
    m_booAreasUpdated = True
    With l_rctCurrent
        .left = CLng(m_spParam.Position.X - (m_spParam.Obstruction.Width / 2))
        If (m_lngBlockingType >= 1) Then
            .top = CLng(m_spParam.Position.Y - (m_spParam.Obstruction.Height / 2))
        Else
            .top = CLng(m_spParam.Position.Y - (m_spParam.Obstruction.Height))
        End If
        .Width = m_spParam.Obstruction.Width
        .Height = m_spParam.Obstruction.Height
    End With
    With l_rctOld
        .left = CLng(m_spOldParam.Position.X - (m_spOldParam.Obstruction.Width / 2))
        If (m_lngBlockingType >= 1) Then
            .top = CLng(m_spOldParam.Position.Y - (m_spOldParam.Obstruction.Height / 2))
        Else
            .top = CLng(m_spOldParam.Position.Y - (m_spOldParam.Obstruction.Height))
        End If
        .Width = m_spOldParam.Obstruction.Width
        .Height = m_spOldParam.Obstruction.Height
    End With
    For Each l_araArea In m_mapMap.Areas
        l_booWasIn = False
        l_booIsIn = False
        l_araArea.DualIntersect l_rctOld, l_rctCurrent, l_booWasIn, l_booIsIn

        If (Not l_booWasIn) And (l_booIsIn) Then
            ' Enter
            If Me.Name Like l_araArea.NamePattern Then
                l_araArea.Action Me, "Enter"
            End If
        ElseIf (l_booWasIn) And (l_booIsIn) Then
            ' Move
            If Me.Name Like l_araArea.NamePattern Then
                l_araArea.Action Me, "Move"
            End If
        ElseIf (l_booWasIn) And (Not l_booIsIn) Then
            ' Leave
            If Me.Name Like l_araArea.NamePattern Then
                l_araArea.Action Me, "Leave"
            End If
        End If

        Err.Clear
    Next l_araArea
    m_booAreasUpdated = False
End Sub

Private Sub InitAreas()
On Error Resume Next
Dim l_rctMe As Fury2Rect
Dim l_araArea As Fury2Area
    If m_mapMap Is Nothing Then Exit Sub
    If m_mapMap.Areas.Count = 0 Then Exit Sub
    Set l_rctMe = Me.Rectangle(False)
    For Each l_araArea In m_mapMap.Areas
        If (l_araArea.Rectangle.Intersect(l_rctMe)) Then
            ' Enter
            If Me.Name Like l_araArea.NamePattern Then
                l_araArea.Action Me, "Enter"
            End If
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
        .SecondaryImageCount = m_fraFrame.SecondaryImages.Count
        If .SecondaryImageCount > 0 Then
            .pSecondaryImages = m_fraFrame.SecondaryImagePointer
        Else
            .pSecondaryImages = 0
        End If
    End With
End Sub

Private Sub UpdateObstruction()
On Error Resume Next
Dim l_rctBounds As FRect
    If m_posPose Is Nothing Then Exit Sub
    With m_spParam.Obstruction
        If ((m_lngBlockingType = 3) Or (m_lngBlockingType = 4)) And (Not (m_posPose.BlockingPolygon Is Nothing)) Then
            .pPolygon = m_posPose.BlockingPolygon.Handle
            SoftFX.GetPolygonBounds .pPolygon, l_rctBounds
            .Width = Abs(l_rctBounds.x2) + Abs(l_rctBounds.x1)
            .Height = Abs(l_rctBounds.y2) + Abs(l_rctBounds.y1)
        Else
            .Width = m_posPose.Blocking.Width
            .Height = m_posPose.Blocking.Height
        End If
        .Type = m_lngBlockingType
    End With
    With m_spParam
        .ZLeft = m_posPose.ZLeft
        .ZRight = m_posPose.ZRight
    End With
End Sub

Private Sub UpdatePose(Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim l_posPose As Fury2Pose
    If m_staState Is Nothing Then
        m_lngPose = ClipValue(m_lngPose, 1, m_posPoses.Count)
        Set l_posPose = m_posPoses(m_lngPose)
    Else
        m_lngPose = ClipValue(m_lngPose, 1, m_posPoses.Count - m_staState.PoseOffset)
        Set l_posPose = m_posPoses(m_staState.PoseOffset + m_lngPose)
    End If
    If (l_posPose Is m_posPose) And (Not Force) Then
    Else
        Set m_posPose = l_posPose
        UpdateObstruction
        UpdateFrame Force
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
On Error Resume Next
    m_spParam.Velocity.V = NewValue
End Property

Public Property Get WalkSpeed() As Single
Attribute WalkSpeed.VB_Description = "The maximum speed for the sprite's movement. (This is an alias for States(State).MaxVelocity.)"
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

Public Property Get ConstantForceMultiplier() As Single
Attribute ConstantForceMultiplier.VB_Description = "Determines the rate at which constant force decays."
    ConstantForceMultiplier = m_spParam.Velocity.CFM
End Property

Public Property Let ConstantForceMultiplier(ByVal NewValue As Single)
    m_spParam.Velocity.CFM = NewValue
End Property

Public Property Get X() As Single
    X = m_spParam.Position.X
End Property

Public Property Let X(ByVal NewValue As Single)
Attribute X.VB_Description = "The sprite's X position."
    m_spParam.Position.X = NewValue
End Property

Public Property Get xLeft() As Single
Attribute xLeft.VB_Description = "*"
    xLeft = m_spParam.Position.X - (m_spParam.Graphic.Rectangle.Width / 2)
End Property

Public Property Get xRight() As Single
Attribute xRight.VB_Description = "*"
    xRight = m_spParam.Position.X + (m_spParam.Graphic.Rectangle.Width / 2)
End Property

Public Property Get XCenter() As Single
Attribute XCenter.VB_Description = "*"
    XCenter = m_spParam.Position.X
End Property

Public Property Get XVelocity() As Single
Attribute XVelocity.VB_Description = "*"
    XVelocity = m_spParam.Velocity.X
End Property

Public Property Let XVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.X = NewValue
End Property

Public Property Get ConstantXForce() As Single
Attribute ConstantXForce.VB_Description = "A constant amount of force to apply to the sprite on the X axis."
    ConstantXForce = m_spParam.Velocity.CXF
End Property

Public Property Let ConstantXForce(ByVal NewValue As Single)
    m_spParam.Velocity.CXF = NewValue
End Property

Public Property Get XForce() As Single
Attribute XForce.VB_Description = "*"
    XForce = m_spParam.Velocity.XF + m_spParam.Velocity.CXF
End Property

Public Property Get Y() As Single
Attribute Y.VB_Description = "The sprite's Y position."
    Y = m_spParam.Position.Y
End Property

Public Property Let Y(ByVal NewValue As Single)
    m_spParam.Position.Y = NewValue
End Property

Public Property Get YCenter() As Single
Attribute YCenter.VB_Description = "*"
    YCenter = m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height \ 2)
End Property

Public Property Get yTop() As Single
Attribute yTop.VB_Description = "*"
    yTop = m_spParam.Position.Y - (m_spParam.Graphic.Rectangle.Height)
End Property

Public Property Get ConstantYForce() As Single
Attribute ConstantYForce.VB_Description = "A constant amount of force to apply to the sprite on the Y axis."
    ConstantYForce = m_spParam.Velocity.CYF
End Property

Public Property Let ConstantYForce(ByVal NewValue As Single)
    m_spParam.Velocity.CYF = NewValue
End Property

Public Property Get YForce() As Single
Attribute YForce.VB_Description = "*"
    YForce = m_spParam.Velocity.YF + m_spParam.Velocity.CYF
End Property

Public Property Get YVelocity() As Single
Attribute YVelocity.VB_Description = "*"
    YVelocity = m_spParam.Velocity.Y
End Property

Public Property Let YVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.Y = NewValue
End Property

Public Property Get Z() As Single
Attribute Z.VB_Description = "The sprite's Z position."
    Z = m_spParam.Position.Z
End Property

Public Property Let Z(ByVal NewValue As Single)
    m_spParam.Position.Z = NewValue
End Property

Public Property Get ZLeft() As Single
Attribute ZLeft.VB_Description = "The sorting value for the left side of the sprite."
    ZLeft = m_spParam.ZLeft
End Property

Public Property Get ZRight() As Single
Attribute ZRight.VB_Description = "The sorting value for the right side of the sprite."
    ZRight = m_spParam.ZRight
End Property

Public Property Get ZVelocity() As Single
Attribute ZVelocity.VB_Description = "*"
    ZVelocity = m_spParam.Velocity.Z
End Property

Public Property Let ZVelocity(ByVal NewValue As Single)
    m_spParam.Velocity.Z = NewValue
End Property

Public Property Get Pressure() As Single
Attribute Pressure.VB_Description = "*"
    Pressure = m_spParam.Velocity.FW
End Property

Public Sub ApplyForce(ByVal X As Single, ByVal Y As Single)
Attribute ApplyForce.VB_Description = "Applies a force to the sprite."
On Error Resume Next
    m_spParam.Velocity.CXF = m_spParam.Velocity.CXF + X
    m_spParam.Velocity.CYF = m_spParam.Velocity.CYF + Y
End Sub

Public Function CreatePath() As Fury2Path
Attribute CreatePath.VB_Description = "Creates a new path."
On Error Resume Next
    Set CreatePath = New Fury2Path
    CreatePath.SetParent Me
End Function

Private Sub m_colPaths_Add(ByVal Index As Long)
On Error Resume Next
    m_colPaths(Index).SetParent Me
End Sub

Public Property Get TileX() As Single
Attribute TileX.VB_Description = "*"
    TileX = (m_spParam.Position.X) / Map.Layers(Layer).Tileset.TileWidth
End Property

Public Property Let TileX(ByVal NewValue As Single)
    m_spParam.Position.X = (NewValue * Map.Layers(Layer).Tileset.TileWidth)
End Property

Public Property Get TileY() As Single
Attribute TileY.VB_Description = "*"
    TileY = (m_spParam.Position.Y) / Map.Layers(Layer).Tileset.TileHeight
End Property

Public Property Let TileY(ByVal NewValue As Single)
    m_spParam.Position.Y = (NewValue * Map.Layers(Layer).Tileset.TileHeight)
End Property

Public Property Get XDelta() As Single
    XDelta = m_spParam.Position.X - m_spOldParam.Position.X
End Property

Public Property Get YDelta() As Single
    YDelta = m_spParam.Position.Y - m_spOldParam.Position.Y
End Property

Public Property Get LastVelocity() As Single
    LastVelocity = m_spOldParam.Velocity.V
End Property



