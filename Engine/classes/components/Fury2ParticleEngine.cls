VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ParticleEngine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements Fury2Object
Implements Fury2MapObject
Implements IInspectable
Private Const CollisionAccuracy As Long = 32
Private WithEvents m_colTypes As Fury2Collection
Attribute m_colTypes.VB_VarHelpID = -1
Private WithEvents m_colGenerators As Fury2Collection
Attribute m_colGenerators.VB_VarHelpID = -1
Private WithEvents m_colModifiers As Fury2Collection
Attribute m_colModifiers.VB_VarHelpID = -1
Public Framerate As Double
Attribute Framerate.VB_VarDescription = "The framerate at which particle updates should occur."
Public RenderLayer As Long
Attribute RenderLayer.VB_VarDescription = "The layer where rendering should occur."
Private m_mapMap As Fury2Map
Private m_lngSprites() As Long
Private m_lngHandle As Long
Private m_lngMatrix As Long
Private m_dblLastUpdate As Double
Private m_engEngine As Fury2Engine

Public Property Get Engine() As Fury2Engine
On Error Resume Next
    Set Engine = m_engEngine
End Property

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    If m_engEngine Is Engine Then Exit Sub
    Set m_engEngine = Engine
    Set m_colTypes = m_engEngine.F2Collection()
    Set m_colGenerators = m_engEngine.F2Collection()
    Set m_colModifiers = m_engEngine.F2Collection()
End Sub

Public Property Get Count(Optional ByVal ParticleType As Long = -1) As Long
Attribute Count.VB_Description = "The number of particles (optionally of a specific type) currently alive."
On Error Resume Next
    Count = GetParticleCount(m_lngHandle, ParticleType)
End Property

Public Function AddType() As Fury2ParticleType
Attribute AddType.VB_Description = "Defines a new type and returns it."
On Error Resume Next
Dim l_typType As Fury2ParticleType
    Set l_typType = New Fury2ParticleType
    m_colTypes.Add l_typType
    Set AddType = l_typType
End Function

Public Function AddGenerator() As Fury2ParticleGenerator
Attribute AddGenerator.VB_Description = "Defines a new generator and returns it."
On Error Resume Next
Dim l_genGenerator As Fury2ParticleGenerator
    Set l_genGenerator = New Fury2ParticleGenerator
    m_colGenerators.Add l_genGenerator
    Set AddGenerator = l_genGenerator
End Function

Public Function AddModifier() As Fury2ParticleModifier
Attribute AddModifier.VB_Description = "Defines a new modifier and returns it."
On Error Resume Next
Dim l_modModifier As Fury2ParticleModifier
    Set l_modModifier = New Fury2ParticleModifier
    m_colModifiers.Add l_modModifier
    Set AddModifier = l_modModifier
End Function

Public Property Get Types() As Fury2Collection
Attribute Types.VB_Description = "The types currently defined."
On Error Resume Next
    Set Types = m_colTypes
End Property

Public Property Get Generators() As Fury2Collection
Attribute Generators.VB_Description = "The generators currently defined."
On Error Resume Next
    Set Generators = m_colGenerators
End Property

Public Property Get Modifiers() As Fury2Collection
Attribute Modifiers.VB_Description = "The modifiers currently defined."
On Error Resume Next
    Set Modifiers = m_colModifiers
End Property

Public Sub Update()
Attribute Update.VB_Description = "Updates the particle engine. Note that this is done based on the time elapsed since the previous update."
On Error Resume Next
Dim l_genGenerator As Fury2ParticleGenerator
Dim l_dblNow As Double
Dim l_dblElapsed As Double
    l_dblNow = m_engEngine.HiTimer
    l_dblElapsed = l_dblNow - IIf(m_dblLastUpdate = -1, l_dblNow, m_dblLastUpdate)
    m_dblLastUpdate = l_dblNow
    For Each l_genGenerator In m_colGenerators
        l_genGenerator.Update
    Next l_genGenerator
    UpdateParticleEngine m_lngHandle, l_dblElapsed * Framerate
End Sub

Public Sub Render(ByVal Surface As Fury2Image, Optional ByVal XOffset As Single = 0, Optional ByVal YOffset As Single = 0)
Attribute Render.VB_Description = "Renders the particle engine to a surface."
On Error Resume Next
Dim l_camCamera As ParticleCamera
Dim l_lngSurface As Long
    l_camCamera.Alpha = 1#
    l_camCamera.Rectangle = Surface.Rectangle.GetRectangle()
    l_camCamera.ViewportX = XOffset
    l_camCamera.ViewportY = YOffset
    l_lngSurface = Surface.Handle
    l_camCamera.pRenderTargets = VarPtr(l_lngSurface)
    l_camCamera.RenderTargetCount = 1
    RenderParticleEngine m_lngHandle, l_camCamera
End Sub

Public Property Get Handle() As Long
Attribute Handle.VB_Description = "R"
On Error Resume Next
    Handle = m_lngHandle
End Property

Public Sub Allocate()
On Error Resume Next
    Deallocate
    m_lngHandle = SoftFX.CreateParticleEngine()
End Sub

Public Sub AddCollisionLine(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single)
Attribute AddCollisionLine.VB_Description = "Adds a collision line to the particle engine's collision matrix."
On Error Resume Next
End Sub

Public Sub AllocateMatrix(ByVal Width As Long, ByVal Height As Long)
On Error Resume Next
End Sub

Public Sub DeallocateMatrix()
On Error Resume Next
    If m_lngMatrix <> 0 Then
        SoftFX.DeleteCollisionMatrix m_lngMatrix
        m_lngMatrix = 0
    End If
End Sub

Public Sub Deallocate()
On Error Resume Next
    DeallocateMatrix
    If m_lngHandle <> 0 Then
        SoftFX.DestroyParticleEngine m_lngHandle
        m_lngHandle = 0
    End If
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    Framerate = 10
    m_dblLastUpdate = -1
    RenderLayer = -1
    Allocate
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Deallocate
End Sub

Private Sub Fury2MapObject_Load(ByVal Engine As Fury2Engine, ByVal Map As Fury2Map)
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
Dim l_lnLines() As FLine
Dim l_lngLayer As Long
    Set m_mapMap = Map
    m_lngMatrix = CreateCollisionMatrixEx(Map.MaxX, Map.MaxY, CollisionAccuracy, CollisionAccuracy)
    ReDim m_lngSprites(0 To Map.Layers.Count)
    l_lngLayer = 1
    For Each l_lyrLayer In Map.Layers
        If (l_lyrLayer.CollisionLineCount) > 0 Then
            l_lnLines = l_lyrLayer.CollisionLines
            AppendLinesToCollisionMatrix m_lngMatrix, l_lnLines(0), UBound(l_lnLines) + 1
        End If
        m_lngSprites(l_lngLayer) = l_lyrLayer.Sprites.Pointer
        l_lngLayer = l_lngLayer + 1
    Next l_lyrLayer
    m_lngSprites(UBound(m_lngSprites)) = -1
    SetParticleEngineSprites m_lngHandle, VarPtr(m_lngSprites(0))
    SetParticleEngineCollisionMatrix m_lngHandle, m_lngMatrix
End Sub

Private Sub Fury2MapObject_Render(ByVal Camera As Fury2Camera)
On Error Resume Next
Dim l_camCamera As ParticleCamera
Dim l_lngTargets() As Long
    l_camCamera.Alpha = 1
    l_camCamera.Rectangle.left = Camera.X
    l_camCamera.Rectangle.top = Camera.Y
    l_camCamera.Rectangle.Width = Camera.Width
    l_camCamera.Rectangle.Height = Camera.Height
    l_camCamera.ViewportX = Camera.ViewportX
    l_camCamera.ViewportY = Camera.ViewportY
    l_lngTargets = Camera.RenderTargetArray()
    l_lngTargets(0) = Camera.OutputSurface.Handle
    l_camCamera.pRenderTargets = VarPtr(l_lngTargets(0))
    l_camCamera.RenderTargetCount = Camera.RenderTargetCount
    RenderParticleEngine m_lngHandle, l_camCamera
End Sub

Private Property Get Fury2MapObject_RenderLayer() As Long
On Error Resume Next
    Fury2MapObject_RenderLayer = RenderLayer
End Property

Private Sub Fury2MapObject_Unload()
On Error Resume Next
    Set m_mapMap = Nothing
End Sub

Private Sub Fury2MapObject_Update()
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
Dim l_lngLayer As Long
    l_lngLayer = 1
    For Each l_lyrLayer In m_mapMap.Layers
        m_lngSprites(l_lngLayer) = l_lyrLayer.Sprites.Pointer
        l_lngLayer = l_lngLayer + 1
    Next l_lyrLayer
    m_lngSprites(UBound(m_lngSprites)) = -1
    Update
End Sub

Private Sub m_colTypes_Add(ByVal Index As Long)
On Error Resume Next
Dim m_typType As Fury2ParticleType
    Set m_typType = m_colTypes(Index)
    m_typType.AddTo m_lngHandle
End Sub

Private Sub m_colGenerators_Add(ByVal Index As Long)
On Error Resume Next
Dim m_genGenerator As Fury2ParticleGenerator
    Set m_genGenerator = m_colGenerators(Index)
    m_genGenerator.AddTo m_lngHandle
End Sub

Private Sub m_colModifiers_Add(ByVal Index As Long)
On Error Resume Next
Dim m_modModifier As Fury2ParticleModifier
    Set m_modModifier = m_colModifiers(Index)
    m_modModifier.AddTo m_lngHandle
End Sub

Private Sub m_colTypes_Remove(ByVal Index As Long)
On Error Resume Next
Dim m_typType As Fury2ParticleType
    Set m_typType = m_colTypes(Index)
    m_typType.RemoveFrom m_lngHandle
End Sub

Private Sub m_colGenerators_Remove(ByVal Index As Long)
On Error Resume Next
Dim m_genGenerator As Fury2ParticleGenerator
    Set m_genGenerator = m_colGenerators(Index)
    m_genGenerator.RemoveFrom m_lngHandle
End Sub

Private Sub m_colModifiers_Remove(ByVal Index As Long)
On Error Resume Next
Dim m_modModifier As Fury2ParticleModifier
    Set m_modModifier = m_colModifiers(Index)
    m_modModifier.RemoveFrom m_lngHandle
End Sub


