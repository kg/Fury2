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
Private mvarAngle As Single 'local copy
Private mvarAttachedSprite As Fury2Sprite 'local copy
Private mvarColor As Long 'local copy
Private mvarFalloffDistance As Long 'local copy
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
Private mvarFuzziness As Byte

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

Public Property Get Cache() As Fury2Image
    Set Cache = mvarCache
End Property

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Let Name(ByRef Value As String)
    mvarName = Value
End Property

Public Property Get Fuzziness() As Long
    Fuzziness = mvarFuzziness
End Property

Public Property Let Fuzziness(ByVal NewValue As Long)
    mvarFuzziness = ClipByte(NewValue)
End Property

Public Sub Attach(Sprite)
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
        .Color = mvarColor
        .FalloffDistance = mvarFalloffDistance
        .FlickerLevel = mvarFlickerLevel
        .NoiseOpacity = mvarNoiseOpacity
        .SpinRate = mvarSpinRate
        .Spread = mvarSpread
        .Visible = CByte(Abs(CLng(mvarVisible)))
        .Fuzziness = mvarFuzziness
        .X = mvarX
        .Y = mvarY
        If mvarEnableCache Then
            .Cache = mvarCache.Handle
        Else
            .Cache = 0
        End If
        .CacheValid = CByte(Abs(mvarCacheValid))
    End With
End Property

Friend Property Let LightSource(NewValue As LightSource)
On Error Resume Next
    With NewValue
        mvarAngle = .Angle
        Set mvarAttachedSprite = Nothing
        mvarColor = .Color
        mvarFalloffDistance = .FalloffDistance
        mvarFlickerLevel = .FlickerLevel
        mvarNoiseOpacity = .NoiseOpacity
        mvarSpinRate = .SpinRate
        mvarSpread = .Spread
        mvarVisible = CBool(.Visible)
        mvarX = .X
        mvarY = .Y
        mvarCacheValid = CBool(.CacheValid)
    End With
End Property

Public Property Let Y(ByVal vData As Single)
    mvarY = vData
    UpdateCache
End Property

Public Property Get Y() As Single
    Y = mvarY
End Property

Public Property Let X(ByVal vData As Single)
    mvarX = vData
    UpdateCache
End Property

Public Property Get X() As Single
    X = mvarX
End Property

Public Property Let Visible(ByVal vData As Boolean)
    mvarVisible = vData
End Property

Public Property Get Visible() As Boolean
    Visible = mvarVisible
End Property

Public Property Let Spread(ByVal vData As Single)
    mvarSpread = vData
    UpdateCache
End Property

Public Property Get Spread() As Single
    Spread = mvarSpread
End Property

Public Property Let SpinRate(ByVal vData As Single)
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
    mvarFlickerLevel = vData
End Property

Public Property Get FlickerLevel() As Single
    FlickerLevel = mvarFlickerLevel
End Property

Public Property Let FalloffDistance(ByVal vData As Long)
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
Attribute AttachedSprite.VB_Description = "*"
    Set AttachedSprite = mvarAttachedSprite
End Property

Public Property Let Angle(ByVal vData As Single)
    mvarAngle = vData
    UpdateCache
End Property

Public Property Get Angle() As Single
    Angle = mvarAngle
End Property

Private Sub Class_Initialize()
    mvarSpread = 360
    mvarColor = F2RGB(255, 255, 255, 255)
    mvarVisible = True
    mvarName = "Light"
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
            Set mvarCache = Nothing
            UpdateCache
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
        .WriteSegment_End
    End With
End Sub
