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
Implements Fury2TrackedObject
Implements IVirtualFileCollection

Public Loaded As Boolean
Private mvarDirty As Boolean
Private mvarRevision As Fury2RevisionData
Private mCol As Collection
Private mColUpdate As Collection
Private mvarIndex As Long
Private mvarFirst As Long, mvarSortedFirst As Long
Private mvarFilename As String
Private mvarParent As Fury2Map
Private mvarEngine As Fury2Engine

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
    mvarDirty = True
End Sub

Private Property Get Fury2TrackedObject_Revision() As Fury2RevisionData
    Set Fury2TrackedObject_Revision = mvarRevision
End Property

Public Property Get Revision() As Fury2RevisionData
    Set Revision = mvarRevision
End Property

Friend Sub SetParent(NewParent As Fury2Map)
On Error Resume Next
    Set mvarParent = NewParent
End Sub

Public Property Get Parent() As Fury2Map
    Set Parent = mvarParent
End Property

Sub Clear()
On Error Resume Next
Dim m_lngSprites As Long
    If mvarParent Is Nothing Then
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
    ProfileStart "RefreshList"
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
    ProfileStop "RefreshList"
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

Public Function SpriteFromPoint(ByVal X As Single, ByVal Y As Single, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Fury2Sprite
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

Public Function SpriteFromScreenPoint(ByVal X As Single, ByVal Y As Single, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Fury2Sprite
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

Public Function SpriteFromArea(ByVal Area As Fury2Rect, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Fury2Sprite
On Error Resume Next
Dim l_lngResult As Long, l_lngExclude As Long
Dim l_rctArea As FRect
    l_rctArea.x1 = Area.Left
    l_rctArea.y1 = Area.Top
    l_rctArea.x2 = Area.Right
    l_rctArea.y2 = Area.Bottom
    Set SpriteFromArea = Nothing
    If mCol.Count < 1 Then Exit Function
    l_lngExclude = Exclude.Pointer
    l_lngResult = SoftFX.FindSprite(mCol.Item(1).Pointer, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
    If l_lngResult > 0 Then Set SpriteFromArea = mCol.Item(l_lngResult)
End Function

Public Function SpriteFromScreenArea(ByVal Area As Fury2Rect, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Fury2Sprite
On Error Resume Next
Dim l_lngResult As Long, l_lngExclude As Long
Dim l_rctArea As FRect
    l_rctArea.x1 = Area.Left
    l_rctArea.y1 = Area.Top
    l_rctArea.x2 = Area.Right
    l_rctArea.y2 = Area.Bottom
    Set SpriteFromScreenArea = Nothing
    If mCol.Count < 1 Then Exit Function
    l_lngExclude = Exclude.Pointer
    l_lngResult = SoftFX.FindSpriteOnscreen(mCol.Item(1).Pointer, l_rctArea, l_lngExclude, CLng(MustBeSolid), Class, ExcludedClass)
    If l_lngResult > 0 Then Set SpriteFromScreenArea = mCol.Item(l_lngResult)
End Function

Public Function SpritesFromDistance(ByVal Sprite As Long, ByVal MaxDistance As Single, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_lngResult As Long, l_lngStart As Long
Dim l_lngCount As Long, l_lngSize As Long
Dim l_sprSprite As Fury2Sprite, l_sprCheck As Fury2Sprite
Dim l_sngDistance As Single
    ReDim l_varResult(0 To 7)
    l_lngSize = 8
    Set l_sprCheck = mCol(Sprite)
    For Each l_sprSprite In mCol
        If l_sprSprite Is Exclude Then
        Else
            l_sngDistance = mvarParent.Engine.DistanceBetween(l_sprSprite.X, l_sprSprite.Y, l_sprCheck.X, l_sprCheck.Y)
            If l_sngDistance <= MaxDistance Then
                If l_lngCount > (UBound(l_varResult)) Then
                    l_lngSize = l_lngSize + 8
                    ReDim Preserve l_varResult(0 To l_lngCount - 1)
                End If
                Set l_varResult(l_lngCount) = l_sprSprite
                l_lngCount = l_lngCount + 1
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

Public Function SpritesFromArea(Area, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_lngResult As Long, l_lngStart As Long
Dim l_lngCount As Long, l_lngSize As Long
Dim l_rctArea As FRect
Dim l_lngExclude As Long
    l_lngExclude = Exclude.Pointer
    l_rctArea.x1 = Area.Left
    l_rctArea.y1 = Area.Top
    l_rctArea.x2 = Area.Right
    l_rctArea.y2 = Area.Bottom
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

Public Function SpritesFromScreenArea(Area, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Variant
On Error Resume Next
Dim l_varResult As Variant
Dim l_lngResult As Long, l_lngStart As Long
Dim l_lngCount As Long, l_lngSize As Long
Dim l_rctArea As FRect
Dim l_lngExclude As Long
    l_lngExclude = Exclude.Pointer
    l_rctArea.x1 = Area.Left
    l_rctArea.y1 = Area.Top
    l_rctArea.x2 = Area.Right
    l_rctArea.y2 = Area.Bottom
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
Public Function SpritesFromPolygon(Poly, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Variant
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
Public Function SpritesFromOnscreenPolygon(Poly, Optional ByVal Exclude As Fury2Sprite = Nothing, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1) As Variant
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

Public Function GetClosest(ByVal Check As Fury2Sprite, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1, Optional ByRef OutputDistance) As Fury2Sprite
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

Public Function GetFarthest(ByVal Check As Fury2Sprite, Optional ByVal MustBeSolid As Boolean = False, Optional ByVal Class As Long = -1, Optional ByVal ExcludedClass As Long = -1, Optional ByRef OutputDistance) As Fury2Sprite
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

Public Sub Update(Map As Fury2Map, Optional CLayer As Long = 1, Optional SetIndex As Boolean = False)
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_lngSprites As Long
    If mCol.Count = 0 Then Exit Sub
    l_lngSprites = 1
    If mvarDirty Then RefreshList
    With Map.Layers(CLayer)
        SoftFX.UpdateSprites mvarFirst, .CollisionMatrix
    End With
    ProfileStart "Sprite.Update"
    For Each l_sprSprite In mCol
        With l_sprSprite
            .SetAttributes l_lngSprites, CLayer, Me, Map
            .Update
            l_lngSprites = l_lngSprites + 1
        End With
    Next l_sprSprite
    ProfileStop "Sprite.Update"
End Sub

Public Sub Redraw(Camera As Fury2Camera, Optional OutputBuffer As Fury2Image = Nothing)
On Error Resume Next
Dim m_camCamera As CameraParam
    If mvarDirty Then RefreshList
    Err.Clear
    If mCol.Count = 0 Then
    Else
        Err.Clear
        With m_camCamera
            If OutputBuffer Is Nothing Then
                If (mvarParent.Engine.Cameras.Count = 1) And (mvarParent.Engine.DisableCameraBuffers) Then
                    .pImage = mvarParent.Engine.Backbuffer.Handle
                Else
                    .pImage = Camera.Buffer.Handle
                End If
            Else
                .pImage = OutputBuffer.Handle
            End If
            .ViewportX = Camera.ViewportX
            .ViewportY = Camera.ViewportY
            .Alpha = 255
            .Rectangle.Left = 0
            .Rectangle.Top = 0
            .Rectangle.Width = Camera.Width
            .Rectangle.Height = Camera.Height
        End With
        RenderSprites mvarSortedFirst, m_camCamera
        If Err.Number <> 0 Then Engine.CriticalError "Fury2Sprites.Redraw", "Call RedrawSprites2 mvarFirst, m_camCamera": Exit Sub
    End If
End Sub

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

Friend Sub AcceptNew(Obj)
On Error Resume Next
    If TypeOf Obj Is Fury2Sprite Then Else Exit Sub
    If Find(Obj) Then Exit Sub
    mCol.Add Obj
    mvarDirty = True
End Sub

Public Sub MoveTo(ByRef Index, ByVal NewLayer As Long)
On Error Resume Next
    Err.Clear
    With mvarParent.Layers(NewLayer).Sprites
        .AcceptNew Item(Index)
        Item(Index).SetAttributes .Count, NewLayer, mvarParent.Layers(NewLayer).Sprites, mvarParent
    End With
    Remove Index
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
    Set l_colItems = New Fury2Collection
    For Each l_sprSprite In mCol
        If l_sprSprite.Name Like Match Then
            l_colItems.Add l_sprSprite
        End If
    Next l_sprSprite
    Set FindByNames = l_colItems
End Function

Public Function Find(vntIndexKey As Variant) As Long
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

Public Property Get Item(vntIndexKey As Variant) As Fury2Sprite
Attribute Item.VB_UserMemId = 0
On Error Resume Next
Dim m_lngSprites As Long
Dim m_strName As String
    If mCol.Count < 1 Then Exit Property
    Set Item = mCol(Find(vntIndexKey))
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
    Set mvarRevision = New Fury2RevisionData
    mvarDirty = True
End Sub

Private Sub Class_Terminate()
    'destroys collection when this class is terminated
    Set mvarParent = Nothing
    Set mvarRevision = Nothing
    Set mCol = Nothing
    Set mColUpdate = Nothing
End Sub

Private Sub IVirtualFileCollection_AddItem(Item As Object)
On Error Resume Next
'    mvarParent.Engine.ContextLevelAdd "Add Sprite(""" + CStr(Item.Name) + """)"
    Add Item, False
'    mvarParent.Engine.ContextLevelRemove
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
    DoEvents
End Sub

