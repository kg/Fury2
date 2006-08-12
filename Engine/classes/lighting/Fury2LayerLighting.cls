VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2LayerLighting"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
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
Implements IInspectable
Implements IVirtualFileSaveable
Public ResetLight As Boolean
Attribute ResetLight.VB_VarDescription = "If True, when this layer is rendered it will erase the lightmap."
Public RenderLight As Boolean
Attribute RenderLight.VB_VarDescription = "If True, when this layer is rendered it will render the lightmap to the camera."
Private m_lngMatrix As Long
Private m_envEnvironment As LightingEnvironment
Private m_colLights As Fury2LightSources
Private m_litLights() As LightSource
Private m_obsObstructions() As LightingObstruction
Private m_plnPlanes() As LightingPlane2
Private m_lyrLayer As Fury2MapLayer

Public Function LightFromSprite(ByVal Sprite As Fury2Sprite) As Fury2LightSource
Attribute LightFromSprite.VB_Description = "Returns the light that is attached to a given sprite, if any."
On Error Resume Next
Dim l_litLight As Fury2LightSource
    For Each l_litLight In m_colLights
        If l_litLight.AttachedSprite Is Sprite Then
            Set LightFromSprite = l_litLight
            Exit For
        End If
    Next l_litLight
End Function

Friend Sub SetParent(ByVal Parent As Fury2MapLayer)
On Error Resume Next
Dim l_litLight As Fury2LightSource
    Set m_lyrLayer = Parent
    m_colLights.SetParent m_lyrLayer.Map
    For Each l_litLight In m_colLights
        l_litLight.SetEngine m_lyrLayer.Map.Engine
    Next l_litLight
End Sub

Public Property Get Layer() As Fury2MapLayer
Attribute Layer.VB_Description = "The map layer this lighting is attached to."
    Set Layer = m_lyrLayer
End Property

Public Sub Free()
On Error Resume Next
Dim l_litLight As Fury2LightSource
    DeallocateLightingMatrix
    If Not (m_colLights Is Nothing) Then
        For Each l_litLight In m_colLights
            l_litLight.Free
        Next l_litLight
        m_colLights.Free
    End If
    Set m_colLights = Nothing
    Erase m_litLights
    Erase m_obsObstructions
    Erase m_plnPlanes
End Sub

Public Sub Init(W As Long, H As Long)
On Error Resume Next
    CreateLightingMatrix W, H
End Sub

Public Sub Refresh()
On Error Resume Next
Dim l_lsLight As Fury2LightSource
    If m_lngMatrix <> 0 Then
        EraseLightingMatrix m_lngMatrix
        AppendObstructionsToLightingMatrix m_lngMatrix, m_obsObstructions(1), UBound(m_obsObstructions)
    End If
    For Each l_lsLight In m_colLights
        l_lsLight.CacheValid = False
    Next l_lsLight
End Sub

Public Sub CreateLightingMatrix(Width As Long, Height As Long)
On Error Resume Next
    DeallocateLightingMatrix
    m_lngMatrix = SoftFX.CreateLightingMatrix(Width, Height)
    Refresh
End Sub

Public Sub DeallocateLightingMatrix()
On Error Resume Next
    If m_lngMatrix <> 0 Then
        DeleteLightingMatrix m_lngMatrix
        m_lngMatrix = 0
    End If
End Sub

Public Function PlanePointer(Index As Long) As Long
On Error Resume Next
    PlanePointer = VarPtr(m_plnPlanes(Index))
End Function

Public Function SightCheck(ByVal FromX As Single, ByVal FromY As Single, ByVal ToX As Single, ByVal ToY As Single, Optional ByRef IgnoreSprite1 = Nothing, Optional ByRef IgnoreSprite2 = Nothing) As Boolean
Attribute SightCheck.VB_Description = "Performs a sight check from one location to another. IgnoreSprite1 and IgnoreSprite2 allow excluding one or two sprites from the calculation."
    SightCheck = SoftFX.SightCheck(m_envEnvironment, FromX, FromY, ToX, ToY, IIf(IgnoreSprite1 Is Nothing, 0, IgnoreSprite1.Pointer), IIf(IgnoreSprite2 Is Nothing, 0, IgnoreSprite2.Pointer))
End Function

Public Function Raycast(ByVal X As Single, ByVal Y As Single, Optional ByRef IgnoreSprite = Nothing) As Long
Attribute Raycast.VB_Description = "Calculates the illumination value of a given point. IgnoreSprite allows excluding a given sprite from the lighting calculation."
    Raycast = SoftFX.RaycastPoint(m_envEnvironment, X, Y, IIf(IgnoreSprite Is Nothing, 0, IgnoreSprite.Pointer))
End Function

Public Sub RemovePlane(Index As Long)
On Error Resume Next
Dim m_lngLines As Long
    If Index < 0 Then Exit Sub
    If Index > UBound(m_plnPlanes) Then Exit Sub
    m_plnPlanes(Index).Start.X = -1
    m_plnPlanes(Index).Start.Y = -1
    m_plnPlanes(Index).end.X = -1
    m_plnPlanes(Index).end.Y = -1
    m_plnPlanes(Index).Height = 0
    If Index = UBound(m_plnPlanes) Then
    Else
        For m_lngLines = Index + 1 To UBound(m_plnPlanes)
            m_plnPlanes(m_lngLines - 1) = m_plnPlanes(m_lngLines)
        Next m_lngLines
    End If
    ReDim Preserve m_plnPlanes(LBound(m_plnPlanes) To UBound(m_plnPlanes) - 1)
    m_envEnvironment.PlaneCount = UBound(m_plnPlanes)
End Sub

Public Sub AddPlane(x1, y1, x2, y2, H)
On Error Resume Next
    ReDim Preserve m_plnPlanes(LBound(m_plnPlanes) To UBound(m_plnPlanes) + 1)
    With m_plnPlanes(UBound(m_plnPlanes))
        .Start.X = CSng(x1)
        .Start.Y = CSng(y1)
        .end.X = CSng(x2)
        .end.Y = CSng(y2)
        .Height = CLng(H)
    End With
    m_envEnvironment.PlaneCount = UBound(m_plnPlanes)
End Sub

Sub ClearPlanes()
On Error Resume Next
    ReDim m_plnPlanes(0 To 0)
    m_envEnvironment.PlaneCount = 0
End Sub

Public Property Get Planes() As LightingPlane2()
On Error Resume Next
    Planes = m_plnPlanes
End Property

Public Property Let Planes(NewPlanes() As LightingPlane2)
On Error Resume Next
    m_plnPlanes() = NewPlanes()
    m_envEnvironment.PlaneCount = UBound(m_plnPlanes)
End Property

Public Function GetPlane(Index) As Variant
On Error Resume Next
    With m_plnPlanes(Index)
        GetPlane = Array(.Start.X, .Start.Y, .end.X, .end.Y, .Height)
    End With
End Function

Public Function SetPlane(Index, NewValue)
On Error Resume Next
    With m_plnPlanes(Index)
        .Start.X = CSng(NewValue(0))
        .Start.Y = CSng(NewValue(1))
        .end.X = CSng(NewValue(2))
        .end.Y = CSng(NewValue(3))
        .Height = CLng(NewValue(4))
    End With
End Function

Public Function ObstructionPointer(Index As Long) As Long
On Error Resume Next
    ObstructionPointer = VarPtr(m_obsObstructions(Index))
End Function

Public Sub RemoveObstruction(Index As Long)
On Error Resume Next
Dim m_lngLines As Long
    If Index < 0 Then Exit Sub
    If Index > UBound(m_obsObstructions) Then Exit Sub
    m_obsObstructions(Index).Line.Start.X = -1
    m_obsObstructions(Index).Line.Start.Y = -1
    m_obsObstructions(Index).Line.end.X = -1
    m_obsObstructions(Index).Line.end.Y = -1
    If Index = UBound(m_obsObstructions) Then
    Else
        For m_lngLines = Index + 1 To UBound(m_obsObstructions)
            m_obsObstructions(m_lngLines - 1) = m_obsObstructions(m_lngLines)
        Next m_lngLines
    End If
    ReDim Preserve m_obsObstructions(LBound(m_obsObstructions) To UBound(m_obsObstructions) - 1)
    m_envEnvironment.ObstructionCount = UBound(m_obsObstructions)
End Sub

Public Sub AddObstruction(x1, y1, x2, y2)
On Error Resume Next
    ReDim Preserve m_obsObstructions(LBound(m_obsObstructions) To UBound(m_obsObstructions) + 1)
    With m_obsObstructions(UBound(m_obsObstructions)).Line
        .Start.X = CSng(x1)
        .Start.Y = CSng(y1)
        .end.X = CSng(x2)
        .end.Y = CSng(y2)
    End With
    m_envEnvironment.ObstructionCount = UBound(m_obsObstructions)
End Sub

Sub ClearObstructions()
On Error Resume Next
    ReDim m_obsObstructions(0 To 0)
    m_envEnvironment.ObstructionCount = 0
End Sub

Public Property Get Obstructions() As LightingObstruction()
Attribute Obstructions.VB_Description = "*"
On Error Resume Next
    Obstructions = m_obsObstructions
End Property

Public Property Let Obstructions(NewObstructions() As LightingObstruction)
On Error Resume Next
    m_obsObstructions() = NewObstructions()
    m_envEnvironment.ObstructionCount = UBound(m_obsObstructions)
End Property

Public Function GetObstruction(Index) As Variant
On Error Resume Next
    With m_obsObstructions(Index)
        GetObstruction = Array(.Line.Start.X, .Line.Start.Y, .Line.end.X, .Line.end.Y)
    End With
End Function

Public Function SetObstruction(Index, NewValue)
On Error Resume Next
    With m_obsObstructions(Index).Line
        .Start.X = NewValue(0)
        .Start.Y = NewValue(1)
        .end.X = NewValue(2)
        .end.Y = NewValue(3)
    End With
End Function

Public Sub Changed()
On Error Resume Next
    RefreshLights
End Sub

Public Property Get Lights() As Fury2LightSources
On Error Resume Next
    Set Lights = m_colLights
End Property

Public Sub RefreshLights()
On Error Resume Next
    ReDim Preserve m_litLights(0 To m_colLights.Count - 1)
    m_envEnvironment.LightCount = m_colLights.Count
End Sub

Public Function Render(ByRef Camera As LightingCamera) As Long
On Error Resume Next
Dim l_litLight As Fury2LightSource
Dim l_lngIndex As Long
Dim l_booDestroyed As Boolean
Dim l_lngCount As Long
    l_lngIndex = 0
    If m_colLights.Count > 0 Then
        For Each l_litLight In m_colLights
            l_booDestroyed = False
            If l_litLight.AttachedSprite Is Nothing Then
            ElseIf l_litLight.AttachedSprite.Destroyed Then
                l_booDestroyed = True
            End If
            If Not l_booDestroyed Then
                m_litLights(l_lngIndex) = l_litLight.LightSource
                l_lngIndex = l_lngIndex + 1
            Else
                Lights.Remove Lights.Find(l_litLight)
            End If
        Next l_litLight
        m_envEnvironment.Lights = VarPtr(m_litLights(0))
    Else
        m_envEnvironment.Lights = 0
    End If
    m_envEnvironment.ObstructionCount = UBound(m_obsObstructions)
    m_envEnvironment.Obstructions = VarPtr(m_obsObstructions(1))
    m_envEnvironment.PlaneCount = UBound(m_plnPlanes)
    m_envEnvironment.Planes = VarPtr(m_plnPlanes(1))
    m_envEnvironment.Matrix = m_lngMatrix
    m_envEnvironment.CollisionMatrix = m_lyrLayer.CollisionMatrix
    l_lngCount = SoftFX.RenderLightingEnvironment(Camera, m_envEnvironment)
    l_lngIndex = 0
    For Each l_litLight In Lights
        With l_litLight
            .CacheValid = True
        End With
        l_lngIndex = l_lngIndex + 1
    Next l_litLight
    Render = l_lngCount
End Function

Public Property Get AmbientLight() As Long
Attribute AmbientLight.VB_Description = "{Color}The ambient color of the lighting. If ResetLight is true, the lightmap is cleared to this color."
    AmbientLight = m_envEnvironment.AmbientLight
End Property

Public Property Let AmbientLight(Color As Long)
    m_envEnvironment.AmbientLight = Color
End Property

Public Property Get ObstructionCount() As Long
    ObstructionCount = m_envEnvironment.ObstructionCount
End Property

Public Property Get PlaneCount() As Long
    PlaneCount = m_envEnvironment.PlaneCount
End Property

Friend Sub SetFirstSprite(Sprite As Long)
    m_envEnvironment.Sprites = Sprite
End Sub

Public Function AddLight(ByVal X As Single, ByVal Y As Single, ByVal Color As Long) As Fury2LightSource
On Error Resume Next
    Set AddLight = New Fury2LightSource
    With AddLight
        .X = X
        .Y = Y
        .Color = Color
        .Name = "Light " & (m_colLights.Count + 1)
    End With
    m_colLights.Add AddLight
End Function

Private Sub Class_Initialize()
On Error Resume Next
    ReDim m_litLights(0 To 0)
    ReDim m_obsObstructions(0 To 0)
    ReDim m_plnPlanes(0 To 0)
    Set m_colLights = New Fury2LightSources
    m_colLights.SetParent Me
    m_colLights.Hook Me
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngLight As Long
Dim l_lngCount As Long
Dim l_lngIndex As Long
Dim l_litLight As Fury2LightSource
Dim l_lsLight As LightSource
    With File
        .ReadSegment_Begin
            .Progress_Begin 0.3333
            .ReadSegment_Begin
                If Not .EOS Then
                    Set m_colLights = New Fury2LightSources
                    m_colLights.SetParent Me
                    .Load m_colLights
                    m_colLights.Hook Me
                    m_colLights.Refresh
                End If
            .ReadSegment_End
            .Progress_End
            .Progress_Begin 0.3333
            .ReadSegment_Begin
                If Not .EOS Then
                    l_lngCount = 0
                    .Load l_lngCount
                    If l_lngCount < 1 Then
                        ReDim m_obsObstructions(0 To 0)
                    Else
                        ReDim m_obsObstructions(0 To l_lngCount)
                        .RawLoad VarPtr(m_obsObstructions(1)), Len(m_obsObstructions(0)) * (l_lngCount)
                    End If
                End If
            .ReadSegment_End
            .Progress_End
            .ReadSegment_Begin
            .ReadSegment_End
            .Progress_Begin 0.3333
            .ReadSegment_Begin
                If Not .EOS Then
                    l_lngCount = 0
                    .Load l_lngCount
                    If l_lngCount < 1 Then
                        ReDim m_plnPlanes(0 To 0)
                    Else
                        ReDim m_plnPlanes(0 To l_lngCount)
                        .RawLoad VarPtr(m_plnPlanes(1)), Len(m_plnPlanes(1)) * (l_lngCount)
                    End If
                End If
            .ReadSegment_End
            .Progress_End
            .ReadSegment_Begin
                .Load l_lngLight
                AmbientLight = l_lngLight
                .Load ResetLight
                .Load RenderLight
            .ReadSegment_End
            RefreshLights
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .WriteSegment_Begin
                .Save m_colLights
            .WriteSegment_End
            .WriteSegment_Begin
                .Save CLng(UBound(m_obsObstructions))
                .RawSave VarPtr(m_obsObstructions(1)), Len(m_obsObstructions(1)) * (UBound(m_obsObstructions))
            .WriteSegment_End
            .WriteSegment_Begin
                .Save CLng(0)
            .WriteSegment_End
            .WriteSegment_Begin
                .Save CLng(UBound(m_plnPlanes))
                .RawSave VarPtr(m_plnPlanes(1)), Len(m_plnPlanes(1)) * (UBound(m_plnPlanes))
            .WriteSegment_End
            .WriteSegment_Begin
                .Save AmbientLight
                .Save ResetLight
                .Save RenderLight
            .WriteSegment_End
        .WriteSegment_End
    End With
End Sub

Public Sub Resync()
On Error Resume Next
    If Lights.Count < 1 Then Exit Sub
Dim l_litLight As Fury2LightSource
    For Each l_litLight In Lights
        With l_litLight
            If .AttachedSprite Is Nothing Then
            Else
                .Alpha = .AttachedSprite.Alpha
                .Angle = .AttachedSprite.Angle
                .X = .AttachedSprite.X + .AttachX
                .Y = .AttachedSprite.Y + .AttachY
            End If
        End With
    Next l_litLight
End Sub

Public Sub Update()
On Error Resume Next
    If Lights.Count < 1 Then Exit Sub
Dim l_litLight As Fury2LightSource
    For Each l_litLight In Lights
        With l_litLight
            If .SpinRate <> 0 Then .Angle = .Angle + .SpinRate
            If .AttachedSprite Is Nothing Then
            Else
                .Alpha = .AttachedSprite.Alpha
            End If
        End With
    Next l_litLight
End Sub

