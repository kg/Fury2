VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ParticleType"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_gphGraphic As ParticleGraphicParam
Private m_agpGraphic As Fury2Graphic
Private m_evtDie As Fury2Event
Private m_evtCollide As Fury2Event
Private m_typType As ParticleType
Private m_engEngine As Fury2Engine

Public Property Get Engine() As Fury2Engine
On Error Resume Next
    Set Engine = m_engEngine
End Property

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    If m_engEngine Is Engine Then Exit Sub
    Set m_engEngine = Engine
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    m_typType.UserData = ObjPtr(Me)
    m_typType.Color1 = F2White
    m_typType.Color2 = F2White
    m_typType.LBehavior = plbRemove
    m_typType.AMode = pamFade
    m_typType.RenderMode = RenderMode_SourceAlpha
    m_typType.RenderType = prtAntiAliasPixel
End Sub

Friend Sub AddTo(ByVal Engine As Long)
On Error Resume Next
    AddParticleType Engine, m_typType
End Sub

Friend Sub RemoveFrom(ByVal Engine As Long)
On Error Resume Next
    RemoveParticleType Engine, m_typType
End Sub

Public Property Set Graphic(ByVal NewValue As Fury2Graphic)
Attribute Graphic.VB_Description = "The image to draw if the particle is an image particle."
On Error Resume Next
Dim l_agpParam As AnimatedGraphicParam
    Set m_agpGraphic = NewValue
    l_agpParam = NewValue.GetParam()
    With m_gphGraphic
        .Alpha = l_agpParam.Alpha
        .FrameCount = l_agpParam.FrameCount
        .FrameIncrement = NewValue.AnimationDelay * (1 / m_engEngine.DesiredFramerate)
        .LoopMode = NewValue.LoopMode
        .MatteColor = l_agpParam.MatteColor
        .XCenter = l_agpParam.XCenter
        .YCenter = l_agpParam.YCenter
        .pFrames = l_agpParam.pFrames
    End With
    m_typType.pGraphic = VarPtr(m_gphGraphic)
End Property

Public Property Get Handle() As Long
On Error Resume Next
    Handle = VarPtr(m_typType)
End Property

Public Property Get LVDecay() As Single
Attribute LVDecay.VB_Description = "The decay rate for life velocity."
On Error Resume Next
    LVDecay = m_typType.LVDecay
End Property

Public Property Let LVDecay(ByVal NewValue As Single)
On Error Resume Next
    m_typType.LVDecay = NewValue
End Property

Public Property Get AVDecay() As Single
Attribute AVDecay.VB_Description = "The decay rate for alpha velocity."
On Error Resume Next
    AVDecay = m_typType.AVDecay
End Property

Public Property Let AVDecay(ByVal NewValue As Single)
On Error Resume Next
    m_typType.AVDecay = NewValue
End Property

Public Property Get XVDecay() As Single
Attribute XVDecay.VB_Description = "The decay rate for X velocity."
On Error Resume Next
    XVDecay = m_typType.XVDecay
End Property

Public Property Let XVDecay(ByVal NewValue As Single)
On Error Resume Next
    m_typType.XVDecay = NewValue
End Property

Public Property Get YVDecay() As Single
Attribute YVDecay.VB_Description = "The decay rate for Y velocity."
On Error Resume Next
    YVDecay = m_typType.YVDecay
End Property

Public Property Let YVDecay(ByVal NewValue As Single)
On Error Resume Next
    m_typType.YVDecay = NewValue
End Property

Public Property Get LVDecayMode() As ParticleDecayModes
Attribute LVDecayMode.VB_Description = "The type of decay to apply to life velocity."
On Error Resume Next
    LVDecayMode = m_typType.LVDecayMode
End Property

Public Property Let LVDecayMode(ByVal NewValue As ParticleDecayModes)
On Error Resume Next
    m_typType.LVDecayMode = NewValue
End Property

Public Property Get AVDecayMode() As ParticleDecayModes
Attribute AVDecayMode.VB_Description = "The type of decay to apply to alpha velocity."
On Error Resume Next
    AVDecayMode = m_typType.AVDecayMode
End Property

Public Property Let AVDecayMode(ByVal NewValue As ParticleDecayModes)
Attribute AVDecayMode.VB_Description = " "
On Error Resume Next
    m_typType.AVDecayMode = NewValue
End Property

Public Property Get XVDecayMode() As ParticleDecayModes
Attribute XVDecayMode.VB_Description = "The type of decay to apply to X velocity."
On Error Resume Next
    XVDecayMode = m_typType.XVDecayMode
End Property

Public Property Let XVDecayMode(ByVal NewValue As ParticleDecayModes)
On Error Resume Next
    m_typType.XVDecayMode = NewValue
End Property

Public Property Get YVDecayMode() As ParticleDecayModes
Attribute YVDecayMode.VB_Description = "The type of decay to apply to Y velocity."
On Error Resume Next
    YVDecayMode = m_typType.YVDecayMode
End Property

Public Property Let YVDecayMode(ByVal NewValue As ParticleDecayModes)
On Error Resume Next
    m_typType.YVDecayMode = NewValue
End Property

Public Property Get AMode() As ParticleAModes
Attribute AMode.VB_Description = "Determines the effect of a particle's alpha on its rendering."
On Error Resume Next
    AMode = m_typType.AMode
End Property

Public Property Let AMode(ByVal NewValue As ParticleAModes)
On Error Resume Next
    m_typType.AMode = NewValue
End Property

Public Property Get Color() As Long
Attribute Color.VB_Description = "Determines the particle's color."
On Error Resume Next
    Color = Colors(0)
End Property

Public Property Get Colors(ByVal Index As Long) As Long
Attribute Colors.VB_Description = "Determines the colors used for the particle."
On Error Resume Next
    Select Case Index
    Case 0
        Colors = m_typType.Color1
    Case 1
        Colors = m_typType.Color2
    Case Else
    End Select
End Property

Public Property Let Color(ByVal NewValue As Long)
On Error Resume Next
    Colors(0) = NewValue
End Property

Public Property Let Colors(ByVal Index As Long, ByVal NewValue As Long)
On Error Resume Next
    Select Case Index
    Case 0
        m_typType.Color1 = NewValue
    Case 1
        m_typType.Color2 = NewValue
    Case Else
    End Select
End Property

Public Property Get LBehavior() As ParticleLBehaviors
Attribute LBehavior.VB_Description = "Determines the effect of a particle's life."
On Error Resume Next
    LBehavior = m_typType.LBehavior
End Property

Public Property Let LBehavior(ByVal NewValue As ParticleLBehaviors)
On Error Resume Next
    m_typType.LBehavior = NewValue
End Property

Public Property Get LColorMode() As ParticleLColorModes
Attribute LColorMode.VB_Description = "Determines the effect of a particle's life on its displayed color."
On Error Resume Next
    LColorMode = m_typType.LColorMode
End Property

Public Property Let LColorMode(ByVal NewValue As ParticleLColorModes)
On Error Resume Next
    m_typType.LColorMode = NewValue
End Property

Public Property Get RenderTarget() As Long
Attribute RenderTarget.VB_Description = "Determines the render target that particles of this type should be drawn to."
On Error Resume Next
    RenderTarget = m_typType.RenderTarget
End Property

Public Property Let RenderTarget(ByVal NewValue As Long)
On Error Resume Next
    m_typType.RenderTarget = NewValue
End Property

Public Property Get RenderType() As ParticleRenderTypes
Attribute RenderType.VB_Description = "Determines the type of rendering to use for particles of this type."
On Error Resume Next
    RenderType = m_typType.RenderType
End Property

Public Property Let RenderType(ByVal NewValue As ParticleRenderTypes)
On Error Resume Next
    m_typType.RenderType = CByte(NewValue)
End Property

Public Property Get RenderMode() As SFXRenderModes
Attribute RenderMode.VB_Description = "Determines the blend mode to use when rendering particles of this type."
On Error Resume Next
    RenderMode = m_typType.RenderMode
End Property

Public Property Let RenderMode(ByVal NewValue As SFXRenderModes)
On Error Resume Next
    m_typType.RenderMode = CByte(NewValue)
End Property

Public Property Get Softness() As Single
Attribute Softness.VB_Description = "Determines the softness of strokes rendered by particles of this type."
On Error Resume Next
    Softness = m_typType.Softness
End Property

Public Property Let Softness(ByVal NewValue As Single)
On Error Resume Next
    m_typType.Softness = NewValue
End Property

Public Property Get Thickness() As Single
Attribute Thickness.VB_Description = "Determines the thickness of strokes rendered by particles of this type."
On Error Resume Next
    Thickness = m_typType.Thickness
End Property

Public Property Let Thickness(ByVal NewValue As Single)
On Error Resume Next
    m_typType.Thickness = NewValue
End Property

Public Property Get EnableCollision() As Boolean
Attribute EnableCollision.VB_Description = "If True, particles of this type will collide with objects and map collision."
On Error Resume Next
    EnableCollision = m_typType.EnableCollision <> 0
End Property

Public Property Let EnableCollision(ByVal NewValue As Boolean)
On Error Resume Next
    m_typType.EnableCollision = CByte(Abs(CLng(NewValue)))
End Property

Public Property Get CollisionResponse() As Single
Attribute CollisionResponse.VB_Description = "Determines the amount of response to apply to a particle when it collides with something. 1.0 will provide a full strength bounce, and 0.0 will cause the particle to stop in place."
On Error Resume Next
    CollisionResponse = m_typType.CollisionResponse
End Property

Public Property Let CollisionResponse(ByVal NewValue As Single)
On Error Resume Next
    m_typType.CollisionResponse = NewValue
End Property

Friend Sub SetDieCallback(ByVal Callback As Long)
On Error Resume Next
    m_typType.DieCallback = Callback
End Sub

Friend Sub SetCollideCallback(ByVal Callback As Long)
On Error Resume Next
    m_typType.CollideCallback = Callback
End Sub

Public Property Get DieEvent() As Fury2Event
Attribute DieEvent.VB_Description = "This event will be invoked when a particle dies."
On Error Resume Next
    Set DieEvent = m_evtDie
End Property

Public Property Set DieEvent(ByVal NewValue As Fury2Event)
On Error Resume Next
    Set m_evtDie = NewValue
    If m_evtDie Is Nothing Then
        SetDieCallback 0
    Else
        SetDieCallback AddressOf ParticleDieCallback
    End If
End Property

Public Property Get CollideEvent() As Fury2Event
Attribute CollideEvent.VB_Description = "This event will be invoked when a particle collides with something."
On Error Resume Next
    Set CollideEvent = m_evtCollide
End Property

Public Property Set CollideEvent(ByVal NewValue As Fury2Event)
On Error Resume Next
    Set m_evtCollide = NewValue
    If m_evtCollide Is Nothing Then
        SetCollideCallback 0
    Else
        SetCollideCallback AddressOf ParticleCollideCallback
    End If
End Property


