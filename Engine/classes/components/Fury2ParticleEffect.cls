VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ParticleEffect"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Particle Effect Object"
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
Implements Fury2Object
Implements IVirtualFileSaveable
Public X As Single
Public Y As Single
Public Angle As Single
Public ScrollWithCamera As Boolean
Public Active As Boolean
Public AdditiveBlend As Boolean
Private m_lngParticleCount As Long
Private m_lngActive As Long
Private m_parParticles() As Fury2Particle
Private m_lngEffect As Fury2ParticleEffectTypes
Private m_lngWidth As Long, m_lngHeight As Long
Private m_sngWind As Single
Private m_lngDelay As Long
Public DynamicWind As Boolean

Public Property Get DrawBehind() As Boolean
    DrawBehind = False
End Property

Public Function BoundsInitialized() As Boolean
On Error Resume Next
'    BoundsInitialized = (m_lngWidth > 0) And (m_lngHeight > 0)
End Function

Private Sub InitParticle(Index As Long)
On Error Resume Next
'Dim m_lngBright As Long
'Dim m_sngAngle As Single
'    Select Case m_lngEffect
'    Case PE_Rain
'        With m_parParticles(Index)
'            .x = Rnd * CSng(m_lngWidth)
'            .y = -(Rnd * (CSng(m_lngHeight) * 0.3)) + (Rnd * (CSng(m_lngHeight) * 0.15))
'            m_lngBright = 160 + (Rnd * 48)
'            .Color = F2RGB(m_lngBright, m_lngBright, m_lngBright + (Rnd * 64) + 32, m_lngBright + (Rnd * 16))
'            .Life = Rnd * 100
'            .Reserved = 0
'            .XVelocity = (Rnd * m_sngWind)
'            .YVelocity = (Rnd * 0.65) + 1.6
'        End With
'    Case PE_Snow
'        With m_parParticles(Index)
'            .x = Rnd * CSng(m_lngWidth)
'            .y = -(Rnd * (CSng(m_lngHeight) * 0.3)) + (Rnd * (CSng(m_lngHeight) * 0.15))
'            m_lngBright = 220 + (Rnd * 32)
'            .Color = F2RGB(m_lngBright, m_lngBright, m_lngBright + (Rnd * 8) + 8, m_lngBright + (Rnd * 32))
'            .Life = Rnd * 200
'            .Reserved = 0
'            .XVelocity = (Rnd * m_sngWind)
'            .YVelocity = (Rnd * 0.7) + 0.55
'        End With
'    Case PE_Starfield
'        With m_parParticles(Index)
'            .x = (Rnd * m_lngWidth)
'            .y = (Rnd * m_lngHeight)
'            m_sngAngle = Angle * m_Engine.Radian
'            m_lngBright = 32 + (Rnd * 64)
'            .Color = F2RGB(255, 255, 255, m_lngBright + 128)
'            .Life = m_lngBright
'            .Reserved = 0
'            If DynamicWind Then
'                .XVelocity = ((Rnd * m_sngWind) + 0.5) * Cos(m_sngAngle)
'                .YVelocity = ((Rnd * m_sngWind) + 0.5) * Sin(m_sngAngle)
'            Else
'                .XVelocity = ((Rnd * 2.5) + 0.5) * Cos(m_sngAngle)
'                .YVelocity = ((Rnd * 2.5) + 0.5) * Sin(m_sngAngle)
'            End If
'        End With
'    Case Else
'    End Select
End Sub

Public Property Get Wind() As Single
On Error Resume Next
    Wind = m_sngWind
End Property

Public Property Let Wind(NewWind As Single)
On Error Resume Next
    m_sngWind = NewWind
End Property

Public Sub SetBounds(Width As Long, Height As Long)
On Error Resume Next
    m_lngWidth = Width
    m_lngHeight = Height
End Sub

Public Property Get Effect() As Fury2ParticleEffectTypes
On Error Resume Next
    Effect = CLng(m_lngEffect)
End Property

Public Property Let Effect(NewEffect As Fury2ParticleEffectTypes)
On Error Resume Next
    m_lngEffect = CLng(NewEffect)
End Property

Public Sub Render(Surf As Fury2Image, XOffset As Long, YOffset As Long)
On Error Resume Next
    If Active Then Else Exit Sub
    If Surf Is Nothing Then Exit Sub
'    Select Case m_lngEffect
'    Case PE_Rain
'        If Not BoundsInitialized Then Exit Sub
'        If AdditiveBlend Then
'            Surf.DrawParticlesAdditiveEx (-x) + XOffset, (-y) + YOffset, m_parParticles(), 0, m_lngActive
'        Else
'            Surf.DrawParticlesEx (-x) + XOffset, (-y) + YOffset, m_parParticles(), 0, m_lngActive
'        End If
'        If AdditiveBlend Then
'            Surf.DrawParticlesAdditiveEx (-x) + XOffset + m_sngWind, (-y) + YOffset + 1, m_parParticles(), 0, m_lngActive
'        Else
'            Surf.DrawParticlesEx (-x) + XOffset + m_sngWind, (-y) + YOffset + 1, m_parParticles(), 0, m_lngActive
'        End If
'    Case PE_Snow, PE_Starfield
'        If Not BoundsInitialized Then Exit Sub
'        If AdditiveBlend Then
'            Surf.DrawParticlesAdditiveEx (-x) + XOffset, (-y) + YOffset, m_parParticles(), 0, m_lngActive
'        Else
'            Surf.DrawParticlesEx (-x) + XOffset, (-y) + YOffset, m_parParticles(), 0, m_lngActive
'        End If
'    Case Else
'        If AdditiveBlend Then
'            Surf.DrawParticlesAdditiveEx (-x) + XOffset, (-y) + YOffset, m_parParticles(), 0, m_lngParticleCount
'        Else
'            Surf.DrawParticlesEx (-x) + XOffset, (-y) + YOffset, m_parParticles(), 0, m_lngParticleCount
'        End If
'    End Select
End Sub

Public Sub Update()
On Error Resume Next
'Dim m_imgUpdater As Fury2Image
'Dim m_lngParticles As Long
'    If Active Then Else Exit Sub
'    m_Backbuffer.UpdateParticles m_parParticles(), 1, m_lngActive
'    Set m_imgUpdater = Nothing
'    Select Case m_lngEffect
'    Case PE_Rain
'        If Not BoundsInitialized Then Exit Sub
'        If m_lngActive < m_lngParticleCount Then
'            m_lngDelay = m_lngDelay + 1
'            If m_lngDelay >= c_lngRainDelay Then
'                m_lngDelay = 0
'                m_lngActive = m_lngActive + 1
'                InitParticle m_lngActive - 1
'            End If
'        End If
'        For m_lngParticles = 0 To m_lngActive - 1
'            If GetAlpha(m_parParticles(m_lngParticles).Color) <= 0 Then
'                InitParticle m_lngParticles
'                If m_lngActive < m_lngParticleCount Then
'                    m_lngDelay = m_lngDelay + 1
'                    If m_lngDelay >= c_lngRainDelay Then
'                        m_lngDelay = 0
'                        m_lngActive = m_lngActive + 1
'                        InitParticle m_lngActive - 1
'                    End If
'                End If
'            End If
'        Next m_lngParticles
'        If DynamicWind Then
'            m_sngWind = (m_sngWind * 0.975) + (((Rnd * 4) - 2) * 0.025)
'        End If
'    Case PE_Snow
'        If Not BoundsInitialized Then Exit Sub
'        If m_lngActive < m_lngParticleCount Then
'            m_lngDelay = m_lngDelay + 1
'            If m_lngDelay >= c_lngRainDelay Then
'                m_lngDelay = 0
'                m_lngActive = m_lngActive + 1
'                InitParticle m_lngActive - 1
'            End If
'        End If
'        For m_lngParticles = 0 To m_lngActive - 1
'            If GetAlpha(m_parParticles(m_lngParticles).Color) <= 0 Then
'                InitParticle m_lngParticles
'                If m_lngActive < m_lngParticleCount Then
'                    m_lngDelay = m_lngDelay + 1
'                    If m_lngDelay >= c_lngRainDelay Then
'                        m_lngDelay = 0
'                        m_lngActive = m_lngActive + 1
'                        InitParticle m_lngActive - 1
'                    End If
'                End If
'            Else
'                With m_parParticles(m_lngParticles)
'                    .XVelocity = (.XVelocity * 0.8) + (((Rnd * 4) - 2) * 0.025) + (m_sngWind * 0.175)
'                End With
'            End If
'        Next m_lngParticles
'        If DynamicWind Then
'            m_sngWind = (m_sngWind * 0.975) + (((Rnd * 4) - 2) * 0.025)
'        End If
'    Case PE_Starfield
'        If m_lngActive < m_lngParticleCount Then
'            m_lngDelay = m_lngDelay + 1
'            If m_lngDelay >= c_lngRainDelay Then
'                m_lngDelay = 0
'                m_lngActive = m_lngActive + 1
'                InitParticle m_lngActive - 1
'            End If
'        End If
'        For m_lngParticles = 0 To m_lngActive - 1
'            If GetAlpha(m_parParticles(m_lngParticles).Color) <= 0 Then
'                InitParticle m_lngParticles
'                If m_lngActive < m_lngParticleCount Then
'                    m_lngDelay = m_lngDelay + 1
'                    If m_lngDelay >= c_lngRainDelay Then
'                        m_lngDelay = 0
'                        m_lngActive = m_lngActive + 1
'                        InitParticle m_lngActive - 1
'                    End If
'                End If
'            End If
'        Next m_lngParticles
'        If DynamicWind Then
'            m_sngWind = (m_sngWind * 0.975) + (((Rnd * 2.5)) * 0.025)
'        End If
'    Case Else
'    End Select
End Sub

Public Sub Init(Optional Effect = 0, Optional XOffset = 0, Optional YOffset = 0, Optional Count = -1)
On Error Resume Next
Dim Temp As Single
'    Active = True
'    x = XOffset
'    y = YOffset
'    m_lngWidth = 0
'    m_lngHeight = 0
'    m_lngEffect = CLng(Effect)
'    m_lngParticleCount = CLng(Count)
'    m_lngActive = 0
'    m_lngDelay = 0
'    Select Case m_lngEffect
'    Case PE_Rain
'        If m_lngParticleCount = -1 Then m_lngParticleCount = (150 * IIf(m_Engine.SpecialFX, 2, 0)) + 150
'    Case PE_Snow
'        If m_lngParticleCount = -1 Then m_lngParticleCount = (100 * IIf(m_Engine.SpecialFX, 2, 0)) + 100
'    Case PE_Starfield
'        If m_lngParticleCount = -1 Then m_lngParticleCount = (75 * IIf(m_Engine.SpecialFX, 2, 0)) + 50
'    Case Else
'    End Select
'    ReDim m_parParticles(0 To m_lngParticleCount - 1)
End Sub

Public Sub ResizeArray(NewCount)
On Error Resume Next
'    m_lngParticleCount = CLng(NewCount)
'    ReDim Preserve m_parParticles(0 To m_lngParticleCount - 1)
End Sub

Public Sub SetParticle(Index, X As Single, Y As Single, XV As Single, YV As Single, Color, Optional Life = 32766)
On Error Resume Next
'    With m_parParticles(CLng(Index))
'        .x = x
'        .y = y
'        .XVelocity = XV
'        .YVelocity = YV
'        .Color = CLng(Color)
'        .Life = CLng(Life)
'    End With
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

