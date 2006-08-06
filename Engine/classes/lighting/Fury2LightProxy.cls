VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2LightSource"
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
Private mvarAttachTo As String
Private mvarAngle As Single 'local copy
Private mvarAttachedSprite As Fury2Sprite 'local copy
Private mvarAttachX As Single, mvarAttachY As Single, mvarAttachH As Single, mvarAttachV As Single
Private mvarAlpha As Single
Private mvarColor As Long 'local copy
Private mvarFalloffDistance As Long 'local copy
Private mvarLightSize As Single
Private mvarFlickerLevel As Single 'local copy
Private mvarNoiseOpacity As Single 'local copy
Private mvarSpinRate As Single 'local copy
Private mvarSpread As Single 'local copy
Private mvarVisible As Boolean 'local copy
Private mvarX As Single 'local copy
Private mvarY As Single 'local copy
Private mvarName As String
Private mvarEnableCache As Boolean
Private mvarCacheValid As Boolean
Private mvarCache As Fury2Image
Private mvarImageFilename As String
Private mvarImage As Fury2Image
Private mvarImageAlignX As Long, mvarImageAlignY As Long
Private mvarFuzziness As Byte
Private m_llParent As Fury2LayerLighting
Private m_engEngine As Fury2Engine

Public Sub UpdateAttachment()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    If m_llParent Is Nothing Then Exit Sub
    Set l_sprSprite = m_llParent.Layer.Sprites(mvarAttachTo)
    If l_sprSprite Is Nothing Then
        Set l_sprSprite = m_llParent.Layer.Sprites(CLng(mvarAttachTo))
    End If
    Attach l_sprSprite
End Sub

Public Property Get Rectangle() As Fury2Rect
Attribute Rectangle.VB_Description = "The rectangle of this light source."
On Error Resume Next
Dim l_sngX1 As Single, l_sngY1 As Single, l_sngX2 As Single, l_sngY2 As Single
Dim l_sngW As Single, l_sngH As Single
    If mvarImage Is Nothing Then
        l_sngX1 = mvarX - mvarFalloffDistance
        l_sngY1 = mvarY - mvarFalloffDistance
        l_sngX2 = mvarX + mvarFalloffDistance
        l_sngY2 = mvarY + mvarFalloffDistance
    Else
        l_sngW = mvarImage.Width / 2
        l_sngH = mvarImage.Height / 2
        l_sngX1 = mvarX - l_sngW - mvarImageAlignX
        l_sngY1 = mvarY - l_sngH - mvarImageAlignY
        l_sngX2 = mvarX + l_sngW - mvarImageAlignX
        l_sngY2 = mvarY + l_sngH - mvarImageAlignY
    End If
    Set Rectangle = F2Rect(Floor(l_sngX1), Floor(l_sngY1), Ceil(l_sngX2 - l_sngX1), Ceil(l_sngY2 - l_sngY1), False)
End Property

Friend Sub SetParent(ByRef Parent As Fury2LayerLighting)
On Error Resume Next
    Set m_llParent = Parent
    If Not (m_llParent Is Nothing) Then
        Set m_engEngine = m_llParent.Layer.Parent.Engine
    End If
End Sub

Friend Sub SetEngine(ByRef Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
End Sub

Public Sub Free()
On Error Resume Next
    Set m_llParent = Nothing
    Set m_engEngine = Nothing
    Set mvarCache = Nothing
    Set mvarImage = Nothing
    Set mvarAttachedSprite = Nothing
End Sub

Public Property Get AttachTo() As String
Attribute AttachTo.VB_Description = "The name of the sprite to attach this light source to when the map loads."
On Error Resume Next
    AttachTo = mvarAttachTo
End Property

Public Property Let AttachTo(ByRef NewValue As String)
On Error Resume Next
    mvarAttachTo = NewValue
    UpdateAttachment
End Property

Public Property Get Alpha() As Single
Attribute Alpha.VB_Description = "The opacity of this light source."
On Error Resume Next
    Alpha = mvarAlpha
End Property

Public Property Let Alpha(ByVal NewAlpha As Single)
On Error Resume Next
    mvarAlpha = NewAlpha
    mvarCacheValid = False
End Property

Public Sub UpdateImage()
On Error Resume Next
    Set mvarImage = m_engEngine.LoadImage(mvarImageFilename)
End Sub

Public Sub UpdateCache()
On Error Resume Next
Dim l_lngCacheWidth As Long, l_lngCacheHeight As Long
    mvarCacheValid = False
    If (mvarEnableCache) And (mvarFalloffDistance > 0) Then
        l_lngCacheWidth = mvarFalloffDistance * 2 + 1
        l_lngCacheHeight = mvarFalloffDistance * 2 + 1
        If mvarCache Is Nothing Then
            Set mvarCache = F2Image(l_lngCacheWidth, l_lngCacheHeight)
        ElseIf (mvarCache.Width <> l_lngCacheWidth) Or (mvarCache.Height <> l_lngCacheHeight) Then
            mvarCache.Resize l_lngCacheWidth, l_lngCacheHeight, False
        End If
    Else
        Set mvarCache = Nothing
    End If
End Sub

Public Property Get EnableCache() As Boolean
Attribute EnableCache.VB_Description = "If True, this light source will be cached when possible. Be aware that caching uses additional memory."
    EnableCache = mvarEnableCache
End Property

Public Property Let EnableCache(NewValue As Boolean)
    mvarEnableCache = NewValue
    UpdateCache
End Property

Public Property Get CacheValid() As Boolean
    CacheValid = mvarCacheValid
End Property

Friend Property Let CacheValid(NewValue As Boolean)
    mvarCacheValid = NewValue
End Property

Public Property Set Image(ByVal NewValue As Fury2Image)
Attribute Image.VB_Description = "The image used by this light source. Allows specialized bitmap lighting."
    Set mvarImage = NewValue
End Property

Public Property Get Image() As Fury2Image
    Set Image = mvarImage
End Property

Public Property Get ImageFilename() As String
Attribute ImageFilename.VB_Description = "The filename of the image to use for this lightsource. Loaded at startup."
    ImageFilename = mvarImageFilename
End Property

Public Property Let ImageFilename(ByRef NewValue As String)
    mvarImageFilename = NewValue
    UpdateImage
    UpdateCache
End Property

Public Property Get Cache() As Fury2Image
    Set Cache = mvarCache
End Property

Public Property Get Name() As String
Attribute Name.VB_Description = "The name of this light source."
    Name = mvarName
End Property

Public Property Let Name(ByRef Value As String)
    mvarName = Value
End Property

Public Property Get Fuzziness() As Long
Attribute Fuzziness.VB_Description = "The fuzziness level of this light source if it is directional."
    Fuzziness = mvarFuzziness
End Property

Public Property Let Fuzziness(ByVal NewValue As Long)
    mvarFuzziness = ClipByte(NewValue)
End Property

Public Sub Attach(ByVal Sprite As Fury2Sprite)
Attribute Attach.VB_Description = "Attaches this light source to a sprite."
On Error Resume Next
    Set mvarAttachedSprite = Sprite
End Sub

Friend Property Get LightSource() As LightSource
Attribute LightSource.VB_Description = "*"
On Error Resume Next
    With LightSource
        .Angle = mvarAngle
        If mvarAttachedSprite Is Nothing Then
            .AttachedSprite = 0
        Else
            .AttachedSprite = mvarAttachedSprite.Pointer
        End If
        .Color = BlendColors(0, mvarColor, mvarAlpha * 255)
        .FalloffDistance = mvarFalloffDistance
        .LightSize = mvarLightSize
        .FlickerLevel = mvarFlickerLevel
        .NoiseOpacity = mvarNoiseOpacity
        .SpinRate = mvarSpinRate
        .Spread = mvarSpread
        .Visible = CByte(Abs(CLng(mvarVisible)))
        .Fuzziness = mvarFuzziness
        .X = mvarX
        .Y = mvarY
        .AttachX = mvarAttachX
        .AttachY = mvarAttachY
        .AttachH = mvarAttachH
        .AttachV = mvarAttachV
        If mvarEnableCache Then
            .Cache = mvarCache.Handle
        Else
            .Cache = 0
        End If
        .CacheValid = CByte(Abs(mvarCacheValid))
        .Image = 0
        If Not (mvarImage Is Nothing) Then .Image = mvarImage.Handle
        .ImageAlignX = mvarImageAlignX
        .ImageAlignY = mvarImageAlignY
    End With
End Property

Friend Property Let LightSource(NewValue As LightSource)
On Error Resume Next
    With NewValue
        mvarAngle = .Angle
        Set mvarAttachedSprite = Nothing
        mvarColor = .Color
        mvarFalloffDistance = .FalloffDistance
        mvarLightSize = .LightSize
        mvarFlickerLevel = .FlickerLevel
        mvarNoiseOpacity = .NoiseOpacity
        mvarSpinRate = .SpinRate
        mvarSpread = .Spread
        mvarVisible = CBool(.Visible)
        mvarX = .X
        mvarY = .Y
        mvarCacheValid = CBool(.CacheValid)
        mvarImageAlignX = .ImageAlignX
        mvarImageAlignY = .ImageAlignY
    End With
End Property

Public Property Let Y(ByVal vData As Single)
Attribute Y.VB_Description = "The Y coordinate of this light source."
    mvarY = vData
    UpdateCache
End Property

Public Property Get Y() As Single
Attribute Y.VB_Description = "T"
    Y = mvarY
End Property

Public Property Let AttachX(ByVal vData As Single)
Attribute AttachX.VB_Description = "The attachment X coordinate."
    mvarAttachX = vData
    UpdateCache
End Property

Public Property Get AttachX() As Single
    AttachX = mvarAttachX
End Property

Public Property Let AttachY(ByVal vData As Single)
Attribute AttachY.VB_Description = "The attachment Y coordinate."
    mvarAttachY = vData
    UpdateCache
End Property

Public Property Get AttachY() As Single
    AttachY = mvarAttachY
End Property

Public Property Let ImageAlignX(ByVal vData As Long)
Attribute ImageAlignX.VB_Description = "The X alignment of the image if this is a bitmap light."
    mvarImageAlignX = vData
    UpdateCache
End Property

Public Property Get ImageAlignX() As Long
    ImageAlignX = mvarImageAlignX
End Property

Public Property Let ImageAlignY(ByVal vData As Long)
Attribute ImageAlignY.VB_Description = "The Y alignment of the image if this is a bitmap light."
    mvarImageAlignY = vData
    UpdateCache
End Property

Public Property Get ImageAlignY() As Long
    ImageAlignY = mvarImageAlignY
End Property

Public Property Let AttachH(ByVal vData As Single)
    mvarAttachH = vData
    UpdateCache
End Property

Public Property Get AttachH() As Single
    AttachH = mvarAttachH
End Property

Public Property Let AttachV(ByVal vData As Single)
    mvarAttachV = vData
    UpdateCache
End Property

Public Property Get AttachV() As Single
    AttachV = mvarAttachV
End Property

Public Property Let X(ByVal vData As Single)
Attribute X.VB_Description = "The X coordinate of the light."
    mvarX = vData
    UpdateCache
End Property

Public Property Get X() As Single
    X = mvarX
End Property

Public Property Let Visible(ByVal vData As Boolean)
Attribute Visible.VB_Description = "Determines whether this light is visible."
    mvarVisible = vData
End Property

Public Property Get Visible() As Boolean
    Visible = mvarVisible
End Property

Public Property Let Spread(ByVal vData As Single)
Attribute Spread.VB_Description = "The spread angle of the light. If <= 90, this is a directional light, otherwise it is a round light."
    mvarSpread = vData
    UpdateCache
End Property

Public Property Get Spread() As Single
    Spread = mvarSpread
End Property

Public Property Let LightSize(ByVal vData As Single)
    mvarLightSize = vData
    UpdateCache
End Property

Public Property Get LightSize() As Single
    LightSize = mvarLightSize
End Property

Public Property Let SpinRate(ByVal vData As Single)
Attribute SpinRate.VB_Description = "The rate at which this light should rotate if directional."
    mvarSpinRate = vData
End Property

Public Property Get SpinRate() As Single
    SpinRate = mvarSpinRate
End Property

Public Property Let NoiseOpacity(ByVal vData As Single)
    mvarNoiseOpacity = vData
End Property

Public Property Get NoiseOpacity() As Single
    NoiseOpacity = mvarNoiseOpacity
End Property

Public Property Let FlickerLevel(ByVal vData As Single)
Attribute FlickerLevel.VB_Description = "The amount of flickering this light should exhibit."
    mvarFlickerLevel = vData
End Property

Public Property Get FlickerLevel() As Single
    FlickerLevel = mvarFlickerLevel
End Property

Public Property Let FalloffDistance(ByVal vData As Long)
Attribute FalloffDistance.VB_Description = "The radius of the light."
    mvarFalloffDistance = vData
    UpdateCache
End Property

Public Property Get FalloffDistance() As Long
    FalloffDistance = mvarFalloffDistance
End Property

Public Property Let Color(ByVal vData As Long)
Attribute Color.VB_Description = "{Color}"
    mvarColor = vData
    UpdateCache
End Property

Public Property Get Color() As Long
    Color = mvarColor
End Property

Public Property Get AttachedSprite() As Fury2Sprite
Attribute AttachedSprite.VB_Description = "The sprite this light is attached to."
    Set AttachedSprite = mvarAttachedSprite
End Property

Public Property Let Angle(ByVal vData As Single)
    mvarAngle = vData
    UpdateCache
End Property

Public Property Get Angle() As Single
Attribute Angle.VB_Description = "The angle this light is pointing towards."
    Angle = mvarAngle
End Property

Private Sub Class_Initialize()
On Error Resume Next
    mvarSpread = 360
    mvarColor = F2RGB(255, 255, 255, 255)
    mvarVisible = True
    mvarName = "Light"
    mvarAlpha = 1
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
            .Load mvarAngle
            .Load mvarColor
            .Load mvarFalloffDistance
            .Load mvarFlickerLevel
            .Load mvarNoiseOpacity
            .Load mvarSpinRate
            .Load mvarSpread
            .Load mvarVisible
            .Load mvarX
            .Load mvarY
            .Load mvarName
            .Load mvarEnableCache
            .Load mvarFuzziness
            If .EOS Then
                mvarAlpha = 1
            Else
                .Load mvarAlpha
            End If
            Set mvarCache = Nothing
            UpdateCache
            .Load mvarImageFilename
            .Load mvarImageAlignX
            .Load mvarImageAlignY
            UpdateImage
            .Load mvarAttachTo
            UpdateAttachment
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save mvarAngle
            .Save mvarColor
            .Save mvarFalloffDistance
            .Save mvarFlickerLevel
            .Save mvarNoiseOpacity
            .Save mvarSpinRate
            .Save mvarSpread
            .Save mvarVisible
            .Save mvarX
            .Save mvarY
            .Save mvarName
            .Save mvarEnableCache
            .Save mvarFuzziness
            .Save mvarAlpha
            .Save mvarImageFilename
            .Save mvarImageAlignX
            .Save mvarImageAlignY
            .Save mvarAttachTo
        .WriteSegment_End
    End With
End Sub

