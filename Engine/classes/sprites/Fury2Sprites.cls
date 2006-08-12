VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Sprites"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Sprite Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Fury2Sprite"
Attribute VB_Ext_KEY = "Member0" ,"Fury2Sprite"
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
Implements IVirtualFileCollection
Implements IVirtualFileSaveable
Implements IInspectableCollection

Public Loaded As Boolean
Private mvarDirty As Boolean
Private mCol As Collection
Private mColUpdate As Collection
Private mvarIndex As Long
Private mvarFirst As Long, mvarSortedFirst As Long
Private mvarFilename As String
Private mvarParent As Fury2Map
Private mvarEngine As Fury2Engine
Private mvarMatrix As Long
Private m_optOptions As RenderSpritesParam

Friend Sub SetMatrix(ByVal Matrix As Long)
On Error Resume Next
    mvarMatrix = Matrix
End Sub

Public Sub SetOptions(ByRef NewOptions As RenderSpritesParam)
On Error Resume Next
    m_optOptions = NewOptions
End Sub

Friend Function GetOptions() As RenderSpritesParam
On Error Resume Next
    GetOptions = m_optOptions
End Function

Private Property Get IInspectableCollection_ItemCount() As Long
On Error Resume Next
    IInspectableCollection_ItemCount = mCol.Count
End Property

Private Property Set IInspectableCollection_ItemValue(ByVal Index As Long, ByVal RHS As Variant)
On Error Resume Next
End Property

Private Property Let IInspectableCollection_ItemValue(ByVal Index As Long, ByVal RHS As Variant)
On Error Resume Next
End Property

Private Property Get IInspectableCollection_ItemValue(ByVal Index As Long) As Variant
On Error Resume Next
    Err.Clear
    Set IInspectableCollection_ItemValue = mCol(Index)
    If Err <> 0 Then
        IInspectableCollection_ItemValue = mCol(Index)
    End If
End Property

Public Function CreateSprite() As Fury2Sprite
On Error Resume Next
Dim l_sprNew As Fury2Sprite
    Set l_sprNew = New Fury2Sprite
    Set l_sprNew.Map = mvarParent
    Set l_sprNew.Engine = mvarEngine
    Set CreateSprite = l_sprNew
End Function

Friend Sub SetEngine(NewEngine As Fury2Engine)
    Set mvarEngine = NewEngine
End Sub

Friend Property Get Engine() As Fury2Engine
    Set Engine = mvarEngine
End Property

Public Property Get Pointer() As Long
On Error Resume Next
    Pointer = mvarFirst
End Property

Friend Sub SetLayer(Layer As Long)
On Error Resume Next
    mvarIndex = Layer
End Sub

Sub Free()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Set mvarParent = Nothing
    If mCol.Count = 0 Then Exit Sub
    For Each l_sprSprite In mCol
        l_sprSprite.Free
    Next l_sprSprite
    Do While mCol.Count > 0
        mCol.Remove 1
    Loop
    mvarDirty = True
End Sub

Friend Sub SetParent(NewParent As Fury2Map)
On Error Resume Next
    Set mvarParent = NewParent
End Sub

Public Property Get Parent() As Fury2Map
    Set Parent = mvarParent
End Property

Sub Clear(Optional ByVal Force As Boolean = False)
On Error Resume Next
Dim m_lngSprites As Long
    If (mvarParent Is Nothing) And (Force = False) Then
    Else
        Do While mCol.Count > 0
            Remove 1
        Loop
    End If
    Set mCol = New Collection
    mvarDirty = True
    mvarFilename = ""
End Sub

Public Sub RefreshList()
On Error Resume Next
Dim l_lngFillSprites As Long
Dim l_sprSprite As Fury2Sprite, l_sprNext As Fury2Sprite
    If mvarParent Is Nothing Then Exit Sub
    If mvarIndex < 1 Then Exit Sub
    If mCol.Count < 1 Then Exit Sub
    mvarDirty = False
    l_lngFillSprites = 1
    mvarFirst = mCol.Item(1).Pointer
    For Each l_sprSprite In mCol
        Set l_sprNext = Nothing
        Set l_sprNext = mCol.Item(l_lngFillSprites + 1)
        If l_sprSprite Is Nothing Then
        Else
            l_sprSprite.Index = l_lngFillSprites
            If l_sprNext Is Nothing Then
                l_sprSprite.SetListAttributes 0
            Else
                l_sprSprite.SetListAttributes l_sprNext.Pointer
            End If
        End If
        l_lngFillSprites = l_lngFillSprites + 1
    Next l_sprSprite
    mvarFirst = mCol.Item(1).Pointer
    FillSpriteMatrix mvarFirst, mvarMatrix
    Err.Clear
End Sub

Public Sub Cull(CameraParam As CameraParam)
On Error Resume Next
    If mvarDirty Then RefreshList
    Call SoftFX.CullSprites(mvarFirst, CameraParam)
End Sub

Public Sub Sort()
On Error Resume Next
    If (mvarDirty) Then RefreshList
    If (mvarFirst = 0) Then Exit Sub
    mvarSortedFirst = SoftFX.SortSprites(mvarFirst)
End Sub

Public Function SpriteFromPoint(ByVal X As Single, ByVal Y As Single, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Fury2Sprite
On Error Resume Next
Dim l_lngResult As Long, l_lngExclude As Long
Dim l_rctArea As FRect
    Set SpriteFromPoint = Nothing
    If mCol.Count < 1 Then Exit Function
    With l_rctArea
        .x1 = X
        .y1 = Y
        .x2 = X + 1
        .y2 = Y + 1
    End With
    l_lngExclude = Exclude.Pointer
    l_lngResult = SoftFX.FindSprite(mCol.Item(1).Pointer, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
    If l_lngResult > 0 Then Set SpriteFromPoint = mCol.Item(l_lngResult)
End Function

Public Function SpriteFromScreenPoint(ByVal X As Single, ByVal Y As Single, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Fury2Sprite
On Error Resume Next
Dim l_lngResult As Long, l_lngExclude As Long
Dim l_rctArea As FRect
    Set SpriteFromScreenPoint = Nothing
    If mCol.Count < 1 Then Exit Function
    With l_rctArea
        .x1 = X
        .y1 = Y
        .x2 = X + 1
        .y2 = Y + 1
    End With
    l_lngExclude = Exclude.Pointer
    l_lngResult = SoftFX.FindSpriteOnscreen(mCol.Item(1).Pointer, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
    If l_lngResult > 0 Then Set SpriteFromScreenPoint = mCol.Item(l_lngResult)
End Function

Public Function SpriteFromArea(ByVal Area As Fury2Rect, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Fury2Sprite
On Error Resume Next
Dim l_lngResult As Long, l_lngExclude As Long
Dim l_rctArea As FRect
    l_rctArea.x1 = Area.left
    l_rctArea.y1 = Area.top
    l_rctArea.x2 = Area.right
    l_rctArea.y2 = Area.bottom
    Set SpriteFromArea = Nothing
    If mCol.Count < 1 Then Exit Function
    l_lngExclude = Exclude.Pointer
    l_lngResult = SoftFX.FindSprite(mCol.Item(1).Pointer, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
    If l_lngResult > 0 Then Set SpriteFromArea = mCol.Item(l_lngResult)
End Function

Public Function SpriteFromScreenArea(ByVal Area As Fury2Rect, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Fury2Sprite
On Error Resume Next
Dim l_lngResult As Long, l_lngExclude As Long
Dim l_rctArea As FRect
    l_rctArea.x1 = Area.left
    l_rctArea.y1 = Area.top
    l_rctArea.x2 = Area.right
    l_rctArea.y2 = Area.bottom
    Set SpriteFromScreenArea = Nothing
    If mCol.Count < 1 Then Exit Function
    l_lngExclude = Exclude.Pointer
    l_lngResult = SoftFX.FindSpriteOnscreen(mCol.Item(1).Pointer, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
    If l_lngResult > 0 Then Set SpriteFromScreenArea = mCol.Item(l_lngResult)
End Function

Public Function SpritesFromDistance(ByVal Sprite As Fury2Sprite, ByVal MaxDistance As Single, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_lngResult As Long, l_lngStart As Long
Dim l_lngCount As Long, l_lngSize As Long
Dim l_sprSprite As Fury2Sprite, l_sprCheck As Fury2Sprite
Dim l_sngDistance As Single
    ReDim l_varResult(0 To 7)
    l_lngSize = 8
    For Each l_sprSprite In mCol
        If l_sprSprite Is Exclude Then
        Else
            If ((l_sprSprite.Class = Class) Or (Class = -1)) And (l_sprSprite.Class <> ExcludedClass) Then
                If (Not MustBeSolid) Or (l_sprSprite.Solid) Then
                    l_sngDistance = mvarEngine.DistanceBetween(l_sprSprite.X, l_sprSprite.Y, Sprite.X, Sprite.Y)
                    If l_sngDistance <= MaxDistance Then
                        If l_lngCount > (UBound(l_varResult)) Then
                            l_lngSize = l_lngSize + 8
                            ReDim Preserve l_varResult(0 To l_lngCount - 1)
                        End If
                        Set l_varResult(l_lngCount) = l_sprSprite
                        l_lngCount = l_lngCount + 1
                    End If
                End If
            End If
        End If
    Next l_sprSprite
    If l_lngCount < 1 Then
        SpritesFromDistance = Array()
        Exit Function
    End If
    ReDim Preserve l_varResult(0 To l_lngCount - 1)
    SpritesFromDistance = l_varResult
End Function

Public Function SpritesFromArea(Area, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_lngResult As Long, l_lngStart As Long
Dim l_lngCount As Long, l_lngSize As Long
Dim l_rctArea As FRect
Dim l_lngExclude As Long
    l_lngExclude = Exclude.Pointer
    l_rctArea.x1 = Area.left
    l_rctArea.y1 = Area.top
    l_rctArea.x2 = Area.right
    l_rctArea.y2 = Area.bottom
    ReDim l_varResult(0 To 7)
    l_lngSize = 8
    l_lngStart = Item(1).Pointer
    Do
        l_lngResult = SoftFX.FindSprite(l_lngStart, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
        If l_lngResult = 0 Then Exit Do
        If l_lngCount > (UBound(l_varResult)) Then
            l_lngSize = l_lngSize + 8
            ReDim Preserve l_varResult(0 To l_lngSize - 1)
        End If
        Set l_varResult(l_lngCount) = Item(l_lngResult)
        If (l_lngResult = mCol.Count) Then
            l_lngStart = 0
        Else
            l_lngStart = Item(l_lngResult + 1).Pointer
        End If
        l_lngCount = l_lngCount + 1
    Loop
    If l_lngCount < 1 Then
        SpritesFromArea = Array()
        Exit Function
    End If
    ReDim Preserve l_varResult(0 To l_lngCount - 1)
    SpritesFromArea = l_varResult
End Function

Public Function SpritesFromScreenArea(Area, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_lngResult As Long, l_lngStart As Long
Dim l_lngCount As Long, l_lngSize As Long
Dim l_rctArea As FRect
Dim l_lngExclude As Long
    l_lngExclude = Exclude.Pointer
    l_rctArea.x1 = Area.left
    l_rctArea.y1 = Area.top
    l_rctArea.x2 = Area.right
    l_rctArea.y2 = Area.bottom
    ReDim l_varResult(0 To 7)
    l_lngSize = 8
    l_lngStart = Item(1).Pointer
    Do
        l_lngResult = SoftFX.FindSpriteOnscreen(l_lngStart, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
        If l_lngResult = 0 Then Exit Do
        If l_lngCount > (UBound(l_varResult)) Then
            l_lngSize = l_lngSize + 8
            ReDim Preserve l_varResult(0 To l_lngSize - 1)
        End If
        Set l_varResult(l_lngCount) = Item(l_lngResult)
        If (l_lngResult = mCol.Count) Then
            l_lngStart = 0
        Else
            l_lngStart = Item(l_lngResult + 1).Pointer
        End If
        l_lngCount = l_lngCount + 1
    Loop
    If l_lngCount < 1 Then
        SpritesFromScreenArea = Array()
        Exit Function
    End If
    ReDim Preserve l_varResult(0 To l_lngCount - 1)
    SpritesFromScreenArea = l_varResult
End Function

' todo
Public Function SpritesFromPolygon(Poly, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_sprSprite As Fury2Sprite
Dim l_lngCount As Long, l_lngSize As Long
    ReDim l_varResult(0 To 7)
    l_lngSize = 8
    For Each l_sprSprite In mCol
        If PointInPolygon(Poly, Array(l_sprSprite.X, l_sprSprite.Y)) Then
            If l_lngCount > (UBound(l_varResult)) Then
                l_lngSize = l_lngSize + 8
                ReDim Preserve l_varResult(0 To l_lngSize - 1)
            End If
            Set l_varResult(l_lngCount) = l_sprSprite
            l_lngCount = l_lngCount + 1
        End If
    Next l_sprSprite
    If l_lngCount < 1 Then
        SpritesFromPolygon = Array()
        Exit Function
    End If
    ReDim Preserve l_varResult(0 To l_lngCount - 1)
    SpritesFromPolygon = l_varResult
End Function

' todo
Public Function SpritesFromOnscreenPolygon(Poly, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_lngResult As Long, l_lngStart As Long
Dim l_lngCount As Long, l_lngSize As Long
    ReDim l_varResult(0 To 7)
    l_lngSize = 8
    l_lngStart = Item(1).Pointer
    Do
        l_lngResult = 0
        If l_lngResult = 0 Then Exit Do
        If l_lngCount > (UBound(l_varResult)) Then
            l_lngSize = l_lngSize + 8
            ReDim Preserve l_varResult(0 To l_lngSize - 1)
        End If
        Set l_varResult(l_lngCount) = Item(l_lngResult)
        If (l_lngResult = mCol.Count) Then
            l_lngStart = 0
        Else
            l_lngStart = Item(l_lngResult + 1).Pointer
        End If
        l_lngCount = l_lngCount + 1
    Loop
    If l_lngCount < 1 Then
        SpritesFromOnscreenPolygon = Array()
        Exit Function
    End If
    ReDim Preserve l_varResult(0 To l_lngCount - 1)
    SpritesFromOnscreenPolygon = l_varResult
End Function

Public Function GetClosest(ByVal Check As Fury2Sprite, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any, Optional ByRef OutputDistance) As Fury2Sprite
On Error Resume Next
Dim m_sngDistance As Single
Dim l_lngResult As Long
    l_lngResult = SoftFX.GetClosestSprite(mCol.Item(1).Pointer, Check.Pointer, CLng(MustBeSolid), Class, ExcludedClass, m_sngDistance)
    If l_lngResult > 0 Then Set GetClosest = mCol.Item(l_lngResult)
    If Not IsMissing(OutputDistance) Then
        OutputDistance = m_sngDistance
    End If
    Err.Clear
End Function

Public Function GetFarthest(ByVal Check As Fury2Sprite, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Fury2SpriteClasses = F2SC_Any, Optional ByVal ExcludedClass As Fury2SpriteClasses = F2SC_Any, Optional ByRef OutputDistance) As Fury2Sprite
On Error Resume Next
Dim m_sngDistance As Single
Dim l_lngResult As Long
    l_lngResult = SoftFX.GetFarthestSprite(mCol.Item(1).Pointer, Check.Pointer, CLng(MustBeSolid), Class, ExcludedClass, m_sngDistance)
    If l_lngResult > 0 Then Set GetFarthest = mCol.Item(l_lngResult)
    If Not IsMissing(OutputDistance) Then
        OutputDistance = m_sngDistance
    End If
    Err.Clear
End Function

Friend Sub CullDead()
On Error Resume Next
Dim l_lngSprites As Long
    If mCol.Count = 0 Then Exit Sub
    l_lngSprites = 1
    Do While l_lngSprites <= mCol.Count
        If mCol(l_lngSprites) Is Nothing Then
            mCol.Remove l_lngSprites
        Else
            l_lngSprites = l_lngSprites + 1
        End If
    Loop
End Sub

Public Sub Update(Map As Fury2Map, Optional CLayer As Long = 1, Optional SetIndex As Boolean = False)
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_seoOptions As SpriteEngineOptions
Dim l_lngSprites As Long
    If mCol.Count = 0 Then Exit Sub
    l_lngSprites = 1
    If mvarDirty Then RefreshList
    With Map.Layers(CLayer)
        l_seoOptions.CollisionMatrix = .CollisionMatrix
        l_seoOptions.VelocityMultiplier = Engine.VelocityMultiplier
        SoftFX.UpdateSprites mvarFirst, l_seoOptions
    End With
    For Each l_sprSprite In mCol
        With l_sprSprite
            .SetAttributes l_lngSprites, CLayer, Me, Map
            .Update
            l_lngSprites = l_lngSprites + 1
        End With
    Next l_sprSprite
    CullDead
End Sub

Public Sub Render(Surface As Fury2Image, Optional ByVal XOffset As Long = 0, Optional ByVal YOffset As Long = 0)
On Error Resume Next
Dim l_camCamera As CameraParam
Dim l_optOptions As RenderSpritesParam
Dim l_lngRenderTargets() As Long
    If mvarDirty Then RefreshList
    Sort
    Err.Clear
    If mCol.Count = 0 Then
    Else
        Err.Clear
        With l_camCamera
            ReDim l_lngRenderTargets(0 To 0)
            l_lngRenderTargets(0) = Surface.Handle
            .pRenderTargets = VarPtr(l_lngRenderTargets(0))
            .RenderTargetCount = 1
            .ViewportX = XOffset
            .ViewportY = YOffset
            .ParallaxX = 1
            .ParallaxY = 1
            .Alpha = 255
            .Rectangle.left = 0
            .Rectangle.top = 0
            .Rectangle.Width = Surface.Width
            .Rectangle.Height = Surface.Height
        End With
        l_optOptions = GetOptions
        RenderSprites mvarSortedFirst, l_camCamera, l_optOptions
    End If
End Sub

Public Function Redraw(ByRef Param As CameraParam, Optional OutputBuffer As Fury2Image = Nothing) As Long
On Error Resume Next
Dim l_camCamera As CameraParam
Dim l_optOptions As RenderSpritesParam
Dim l_lngCount As Long
    If mvarDirty Then RefreshList
    Err.Clear
    If mCol.Count = 0 Then
    Else
        Err.Clear
        l_camCamera = Param
        l_optOptions = GetOptions
        l_lngCount = RenderSprites(mvarSortedFirst, l_camCamera, l_optOptions)
    End If
    Redraw = l_lngCount
End Function

Friend Sub RemoveQuick(Obj)
On Error Resume Next
Dim m_lngSprites As Long
    If TypeOf Obj Is Fury2Sprite Then Else Exit Sub
    m_lngSprites = Find(Obj)
    If m_lngSprites Then
        Remove m_lngSprites
    End If
    mvarDirty = True
    Err.Clear
End Sub

Friend Sub AcceptNew(ByVal Obj As Fury2Sprite)
On Error Resume Next
    If Find(Obj) Then Exit Sub
    mCol.Add Obj
    mvarDirty = True
End Sub

Public Sub MoveTo(ByRef Index, ByVal NewLayer As Long)
On Error Resume Next
Dim l_sprItem As Fury2Sprite
    Err.Clear
    Set l_sprItem = Item(Index)
    Remove Index
    With mvarParent.Layers(NewLayer).Sprites
        .Add l_sprItem
    End With
    mvarDirty = True
End Sub

Public Sub Replace(Index, Obj, Optional LoadScript As Boolean = True)
On Error Resume Next
    If TypeOf Obj Is Fury2Sprite Then Else Exit Sub
    If Find(Obj) Then Exit Sub
    Add Obj, LoadScript, CLng(Index)
    Remove CLng(Index) + 1
    mvarDirty = True
End Sub

Public Sub Add(ByVal Obj As Fury2Sprite, Optional LoadScript As Boolean = True, Optional Position As Long = -1)
On Error Resume Next
Dim m_lngSprites As Long
Dim m_objScript As Object
    If Position = -1 Then Position = mCol.Count + 1
    If mCol.Count >= c_lngMaxSprites Then Exit Sub
    If TypeOf Obj Is Fury2Sprite Then Else Exit Sub
    If Find(Obj) Then Exit Sub
    Obj.SetParent Parent
    If Engine Is Nothing Then Stop
    Obj.SetEngine Engine
    If mvarParent Is Nothing Then
    Else
        mvarParent.Sprites.Add Obj, False
    End If
    If LoadScript Then
        Set m_objScript = Obj.Script
        m_objScript.Load
        Set m_objScript = Nothing
    End If
    If Loaded Then
        Obj.Initialize
        Obj.Load
    End If
    Obj.Index = mCol.Count + 1
    Obj.SetLayer mvarIndex
    If (Position > mCol.Count) Then
        mCol.Add Obj
    ElseIf (Position <= 1) Then
        mCol.Add Obj, , 1
    Else
        mCol.Add Obj, , Position
    End If
    mvarDirty = True
    Err.Clear
End Sub

Public Sub Move(ByVal FromIndex As Long, ByVal ToIndex As Long)
On Error Resume Next
Dim l_objObject As Object
    If FromIndex < 1 Or FromIndex > mCol.Count Then Exit Sub
    If ToIndex < 1 Or ToIndex > (mCol.Count + 1) Then Exit Sub
    If FromIndex = ToIndex Then Exit Sub
    Set l_objObject = mCol.Item(FromIndex)
    Remove FromIndex
    If (FromIndex < ToIndex) Then
'        Trust me, this works - even though it doesn't make sense unless you think about it
'        ToIndex = ToIndex - 1
    End If
    Add l_objObject, , ToIndex
End Sub

Public Function FindByNames(ByVal Match As String) As Fury2Collection
On Error Resume Next
Dim l_colItems As Fury2Collection
Dim l_sprSprite As Fury2Sprite
    Set l_colItems = Engine.F2Collection()
    For Each l_sprSprite In mCol
        If l_sprSprite.Name Like Match Then
            l_colItems.Add l_sprSprite
        End If
    Next l_sprSprite
    Set FindByNames = l_colItems
End Function

Public Function Find(ByVal vntIndexKey As Variant) As Long
On Error Resume Next
Dim l_lngSprites As Long
Dim l_objObject As Object, l_strKey As String
Dim l_sprSprite As Fury2Sprite
    If mCol.Count < 1 Then Exit Function
    If VarType(vntIndexKey) = vbObject Then
        Set l_objObject = vntIndexKey
        l_lngSprites = 1
        For Each l_sprSprite In mCol
            If l_sprSprite Is l_objObject Then
                Find = l_lngSprites
                Exit For
            End If
            l_lngSprites = l_lngSprites + 1
        Next l_sprSprite
    ElseIf VarType(vntIndexKey) = vbString Then
        l_strKey = CStr(vntIndexKey)
        l_lngSprites = 1
        For Each l_sprSprite In mCol
            If l_sprSprite.Name Like l_strKey Then
                Find = l_lngSprites
                Exit For
            End If
            l_lngSprites = l_lngSprites + 1
        Next l_sprSprite
    Else
        Find = CLng(vntIndexKey)
    End If
End Function

Public Property Get Sprites(vntIndexKey As Variant) As Fury2Sprite
On Error Resume Next
    Set Sprites = Item(vntIndexKey)
End Property

Public Property Get Sprite(vntIndexKey As Variant) As Fury2Sprite
On Error Resume Next
    Set Sprite = Item(vntIndexKey)
End Property

Public Property Get Item(ByVal vntIndexKey As Variant) As Fury2Sprite
Attribute Item.VB_UserMemId = 0
On Error Resume Next
Dim m_lngSprites As Long
Dim m_strName As String
    If mCol.Count < 1 Then Exit Property
    If VarType(vntIndexKey) = vbObject Then
        Set Item = vntIndexKey
    ElseIf VarType(vntIndexKey) = vbString Then
        Set Item = mCol(Find(vntIndexKey))
    Else
        Set Item = mCol(CLng(vntIndexKey))
    End If
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = mCol.Count
End Property

Public Sub Remove(vntIndexKey As Variant)
On Error Resume Next
Dim m_lngSprites As Long, m_lngIndex As Long
Dim l_sprSprite As Fury2Sprite
Dim l_sprFind As Fury2Sprite
    If mCol.Count < 1 Then Exit Sub
    m_lngSprites = Find(vntIndexKey)
    Set l_sprFind = mCol(m_lngSprites)
    If mvarParent Is Nothing Then
    Else
        mvarParent.Sprites.Remove l_sprFind
    End If
    mCol.Remove m_lngSprites
    mvarDirty = True
    If mCol.Count < 1 Then Exit Sub
    Err.Clear
End Sub

Public Sub DelayRemove(vntIndexKey As Variant)
On Error Resume Next
Dim m_lngSprites As Long, m_lngIndex As Long
Dim l_sprSprite As Fury2Sprite
Dim l_sprFind As Fury2Sprite
    If mCol.Count < 1 Then Exit Sub
    m_lngSprites = Find(vntIndexKey)
    Set l_sprFind = mCol(m_lngSprites)
    If mvarParent Is Nothing Then
    Else
        mvarParent.Sprites.Remove l_sprFind
    End If
    mCol.Add Nothing, , , m_lngSprites
    mCol.Remove m_lngSprites
    mvarDirty = True
    If mCol.Count < 1 Then Exit Sub
    Err.Clear
End Sub


Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    'this property allows you to enumerate
    'this collection with the For...Each syntax
    Set NewEnum = mCol.[_NewEnum]
End Property

Private Sub Class_Initialize()
    'creates the collection when this class is created
    SetEngine DefaultEngine
    Set mCol = New Collection
    Set mColUpdate = New Collection
    mvarDirty = True
    m_optOptions.DrawFrames = True
    m_optOptions.DrawSecondaryImages = True
    m_optOptions.DrawAttachedGraphics = True
End Sub

Private Sub Class_Terminate()
    'destroys collection when this class is terminated
    Set mvarParent = Nothing
    Set mCol = Nothing
    Set mColUpdate = Nothing
End Sub

Private Sub IVirtualFileCollection_AddItem(Item As Object)
On Error Resume Next
'    mvarEngine.ContextLevelAdd "Add Sprite(""" + CStr(Item.Name) + """)"
    Add Item, False
'    mvarEngine.ContextLevelRemove
    Err.Clear
End Sub

Private Property Get IVirtualFileCollection_Count() As Long
On Error Resume Next
    IVirtualFileCollection_Count = mCol.Count
End Property

Private Function IVirtualFileCollection_CreateNew() As Object
On Error Resume Next
Dim l_sprNew As Fury2Sprite
    Set l_sprNew = New Fury2Sprite
    l_sprNew.SetParent Parent
    l_sprNew.SetEngine Engine
    Set IVirtualFileCollection_CreateNew = l_sprNew
End Function

Private Function IVirtualFileCollection_GetItem(ByVal Index As Long) As Object
On Error Resume Next
    Set IVirtualFileCollection_GetItem = mCol.Item(Index)
End Function

Private Sub IVirtualFileCollection_Reset()
On Error Resume Next
    Do While mCol.Count > 0
        Remove 1
    Loop
    Set mCol = New Collection
    mvarDirty = True
End Sub

Private Sub IVirtualFileCollection_SaveHeader(File As libGraphics.VirtualFile)
On Error Resume Next
End Sub

Private Sub IVirtualFileCollection_LoadHeader(File As libGraphics.VirtualFile)
On Error Resume Next
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
End Sub

