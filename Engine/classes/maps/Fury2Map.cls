VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Map"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Map Object"
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
Implements Fury2Object
Implements Fury2TrackedObject
Implements IVirtualFileSaveable

Public EditMode As Boolean
Attribute EditMode.VB_VarDescription = "Indicates whether or not the map is in editing mode. In editing mode, certain automatic behaviors are disabled (for example, AI)"
Private m_mobObjects As Fury2MapObjects
Private m_strSettings() As String
Private m_revRevision As Fury2RevisionData
Private m_lyrLayers As Fury2MapLayers
Private m_strFilename As String
Private m_strName As String
Private m_sprSprites As Fury2Sprites
Private m_lngCollisionType As Long
Private m_booScriptFailed As Boolean
Private m_strScriptSource As String
Private m_objScript As Object
Private m_araAreas As Fury2Areas
Private m_tmrTimerEvents As Fury2TimerEvents
Private m_lngWidth As Long, m_lngHeight As Long
Public Paused As Boolean
Attribute Paused.VB_VarDescription = "*This flag pauses the updating of a map."
Public Started As Boolean
Attribute Started.VB_VarMemberFlags = "40"
Attribute Started.VB_VarDescription = "*"

Private m_booScript_Start As Boolean
Private m_booScript_Load As Boolean
Private m_booScript_Unload As Boolean
Private m_booScript_Update As Boolean
Private m_booScript_Redraw As Boolean

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
    Set Rectangle = F2Rect(0, 0, MaxX, MaxY)
End Property

Friend Property Get lWidth() As Long
    lWidth = m_lngWidth
End Property

Friend Property Get lHeight() As Long
    lHeight = m_lngHeight
End Property

Public Sub Load()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_lyrLayer As Fury2MapLayer
Dim l_araArea As Fury2Area
Dim l_mobObject As Fury2MapObject
    m_Engine.ContextLevelAdd "Map.Load"
    Set m_sprSprites = New Fury2Sprites
    For Each l_lyrLayer In m_lyrLayers
        For Each l_sprSprite In l_lyrLayer.Sprites
            If Not l_sprSprite.Destroyed Then
                m_sprSprites.AcceptNew l_sprSprite
            End If
        Next l_sprSprite
    Next l_lyrLayer
    Select Case LCase(Trim(GetSetting("bgm")))
    Case "[silence]"
        m_Engine.StopBGM
    Case "[no change]", ""
    Case Else
        m_Engine.SwitchBGM m_Engine.ParseString(GetSetting("bgm"))
    End Select
    If m_booEditor Then
    Else
        Err.Clear
        m_Engine.ContextLevelAdd "Script"
        m_objScript.Load
        m_Engine.ContextLevelRemove
    End If
    m_Engine.ContextLevelAdd "Sprites"
    For Each l_lyrLayer In m_lyrLayers
        l_lyrLayer.Sprites.Loaded = True
        For Each l_sprSprite In l_lyrLayer.Sprites
            l_sprSprite.Load
        Next l_sprSprite
    Next l_lyrLayer
    m_Engine.ContextLevelRemove
    If m_booEditor Then
    Else
        m_Engine.ContextLevelAdd "Areas"
        For Each l_araArea In m_araAreas
            l_araArea.Script.Load
        Next l_araArea
        Err.Clear
        m_Engine.ContextLevelRemove
    End If
    Set m_sprSprites = New Fury2Sprites
    For Each l_lyrLayer In m_lyrLayers
        For Each l_sprSprite In l_lyrLayer.Sprites
            If Not l_sprSprite.Destroyed Then
                m_sprSprites.AcceptNew l_sprSprite
            End If
        Next l_sprSprite
        l_lyrLayer.Sprites.Sort
    Next l_lyrLayer
    m_Engine.ContextLevelAdd "Objects"
    For Each l_mobObject In m_mobObjects
        l_mobObject.Load m_Engine, Me
    Next l_mobObject
    m_Engine.ContextLevelRemove
    m_Engine.ContextLevelRemove
End Sub

Friend Sub Unload()
On Error Resume Next
    Select Case LCase(Trim(GetSetting("bgm")))
    Case "[silence]"
    Case "[no change]", ""
    Case Else
    End Select
    If m_booScript_Unload Then
        Err.Clear
        m_objScript.Unload
    End If
    Err.Clear
End Sub

Function GetSetting(Setting) As String
Attribute GetSetting.VB_Description = "Retrieves a setting."
On Error Resume Next
Dim m_lngLBound As Long, m_lngUBound As Long, m_lngItems As Long
Dim m_strFind As String, m_strValue As String, m_strKey As String
    GetSetting = vbNullString
    m_lngLBound = LBound(m_strSettings)
    m_lngUBound = UBound(m_strSettings)
    m_strFind = Trim(LCase(CStr(Setting)))
    For m_lngItems = m_lngLBound To m_lngUBound
        m_strValue = m_strSettings(m_lngItems)
        m_strKey = ""
        m_strKey = Left(m_strValue, InStr(m_strValue, "=") - 1)
        If Trim(LCase(m_strKey)) = m_strFind Then
            GetSetting = Mid(m_strValue, InStr(m_strValue, "=") + 1)
            Exit Function
        End If
    Next m_lngItems
End Function

Public Property Let Settings(NewSettings As String)
Attribute Settings.VB_Description = "The map's settings as a string."
On Error Resume Next
Dim m_lngItems As Long
Dim m_varNewSettings As Variant
    m_varNewSettings = Split(NewSettings, vbCrLf)
    ReDim m_strSettings(LBound(m_varNewSettings) To UBound(m_varNewSettings))
    For m_lngItems = LBound(m_varNewSettings) To UBound(m_varNewSettings)
        m_strSettings(m_lngItems) = CStr(m_varNewSettings(m_lngItems))
    Next m_lngItems
End Property

Public Property Get Settings() As String
On Error Resume Next
    Settings = Join(m_strSettings, vbCrLf)
End Property

Sub SaveSetting(Setting, Value)
Attribute SaveSetting.VB_Description = "Saves a setting."
On Error Resume Next
Dim m_lngLBound As Long, m_lngUBound As Long, m_lngItems As Long
Dim m_strFind As String, m_strValue As String, m_strKey As String
    m_lngLBound = LBound(m_strSettings)
    m_lngUBound = UBound(m_strSettings)
    m_strFind = Trim(LCase(CStr(Setting)))
    For m_lngItems = m_lngLBound To m_lngUBound
        m_strValue = m_strSettings(m_lngItems)
        m_strKey = ""
        m_strKey = Left(m_strValue, InStr(m_strValue, "=") - 1)
        If Trim(LCase(m_strKey)) = m_strFind Then
            m_strSettings(m_lngItems) = CStr(Setting) + "=" + CStr(Value)
            Exit Sub
        End If
    Next m_lngItems
    If (Len(Trim(CStr(m_strSettings(m_lngLBound)))) <= 0) And (m_lngLBound = m_lngUBound) Then
        ReDim m_strSettings(0 To 0)
        m_lngLBound = 0
        m_lngUBound = 0
    Else
        m_lngUBound = m_lngUBound + 1
        ReDim Preserve m_strSettings(m_lngLBound To m_lngUBound)
    End If
    m_strSettings(m_lngUBound) = CStr(Setting) + "=" + CStr(Value)
End Sub

Sub Free()
Attribute Free.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lyrLayer As Fury2MapLayer
    For Each m_lyrLayer In m_lyrLayers
        m_lyrLayer.Free
    Next m_lyrLayer
    Set m_objScript = Nothing
    Class_Initialize
End Sub

Public Function MaxX() As Single
Attribute MaxX.VB_Description = "The maximum X coordinate for any objects on the map."
    MaxX = (Width * m_lyrLayers(1).Tileset.TileWidth) - 1
End Function

Public Function MaxY() As Single
Attribute MaxY.VB_Description = "The maximum Y coordinate for any objects on the map."
    MaxY = (Height * m_lyrLayers(1).Tileset.TileHeight) - 1
End Function

Private Property Get Fury2TrackedObject_Revision() As Fury2RevisionData
    Set Fury2TrackedObject_Revision = m_revRevision
End Property

Public Property Get Revision() As Fury2RevisionData
Attribute Revision.VB_MemberFlags = "40"
    Set Revision = m_revRevision
End Property

Public Property Get Areas() As Fury2Areas
Attribute Areas.VB_Description = "All the areas defined for the map."
    Set Areas = m_araAreas
End Property

Function Blocked(ByVal Layer As Long, ByVal X As Long, ByVal Y As Long, Optional ByVal W As Long = -32767, Optional ByVal H As Long = -32767, Optional ByRef ExcludeSprite) As Boolean
On Error Resume Next
Dim l_lngResult As Long
Dim l_rctArea As Fury2Rect
Dim l_frArea As FRect
Dim l_booColliderBlocked As Boolean
Dim l_lngLayer As Long
    l_lngLayer = CLng(Layer)
    If l_lngLayer < 1 Or l_lngLayer > m_lyrLayers.Count Then
        Exit Function
    End If
    If W = -32767 Then W = 1
    If H = -32767 Then H = 1
    Set l_rctArea = F2Rect(X, Y, W, H, False)
    With l_frArea
        .x1 = X
        .y1 = Y
        .x2 = X + W
        .y2 = Y + H
    End With
    Select Case CollisionType
    Case 0
    Case 1
    Case 2
        l_lngResult = SoftFX.CheckLineCollide(l_frArea, ByVal m_lyrLayers(Layer).CollisionLinePointer(0), m_lyrLayers(Layer).CollisionLineCount)
        If l_lngResult > 0 Then
            Blocked = True
            Exit Function
        End If
    Case Else
    End Select
    If TypeOf ExcludeSprite Is Fury2Sprite Then
        ExcludeSprite = m_lyrLayers(Layer).Sprites.Find(ExcludeSprite)
    End If
    If m_lyrLayers(Layer).Sprites.SpriteFromArea(l_rctArea, CLng(ExcludeSprite), True) Is Nothing Then
    Else
        Blocked = True
    End If
    Err.Clear
End Function

Friend Property Get Script_Update() As Boolean
On Error Resume Next
    Script_Update = m_booScript_Update
End Property

Friend Property Get Script_Redraw() As Boolean
On Error Resume Next
    Script_Redraw = m_booScript_Redraw
End Property

Public Property Get Script() As Object
Attribute Script.VB_Description = "*The map's script."
On Error Resume Next
Dim MapName As String
Dim l_strScript As String
    Err.Clear
    If Trim(m_strScriptSource) = "" Then Exit Property
    If m_objScript Is Nothing Then
        If (m_booScriptFailed = False) Then
            Err.Clear
            m_Engine.MapObjectCount = m_Engine.MapObjectCount + 1
            MapName = m_Engine.VariableName(Filename + "_M")
            With m_Engine.ScriptEngine.Language
                l_strScript = .GenerateClassVariable("Object") & .LineTerminator & _
                .GenerateClassVariable("Map") & .LineTerminator & _
                .GenerateClassMethod("F2OInit", Array("T", "M"), _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Object", "T", True) & .LineTerminator & _
                .GenerateAssignment(.ThisKeyword & .MethodOperator & "Map", "M", True) _
                ) & .LineTerminator & m_strScriptSource
                m_booScript_Update = .ImplementsMethod(l_strScript, "Update")
                m_booScript_Redraw = .ImplementsMethod(l_strScript, "Redraw")
                m_booScript_Load = .ImplementsMethod(l_strScript, "Load")
                m_booScript_Unload = .ImplementsMethod(l_strScript, "Unload")
                m_booScript_Start = .ImplementsMethod(l_strScript, "Start")
                m_Engine.CacheScript Filename + "_M", .GenerateClass(MapName, l_strScript)
                Err.Clear
'                m_Engine.ScriptEngine.Error.Clear
                m_Engine.ScriptEngine.AddCode .GenerateAssignment("New" & MapName, .GenerateInstantiation(MapName), True)
                Set m_objScript = m_Engine.ScriptEngine.Eval("New" & MapName)
            End With
            Set m_Engine.IObjThis = Me
            Err.Clear
            m_objScript.F2OInit Me, Me
            m_objScript.Initialize
            If (m_objScript Is Nothing) Then
                m_Engine.TextOut "Error in Map script"
                m_booScriptFailed = True
            End If
        End If
    End If
    Set Script = m_objScript
    Err.Clear
End Property

Public Property Let ScriptSource(vData As String)
Attribute ScriptSource.VB_Description = "*The source code to the map's script."
    m_strScriptSource = vData
End Property

Public Property Get ScriptSource() As String
    ScriptSource = m_strScriptSource
End Property

Public Sub Init(Optional ShowStatus As Boolean = True)
Attribute Init.VB_MemberFlags = "40"
On Error Resume Next
Dim l_lngLayers As Long
Dim l_lngSprites As Long
Dim l_lngAreas As Long
Dim l_objScript As Object
Dim l_sprSprite As Fury2Sprite
Dim l_sngProgMul As Single
    m_Engine.ContextLevelAdd "Map.Init"
    m_Engine.LoadProgress "Map", 0
    m_sprSprites.Clear
    Set l_objScript = Script
    l_objScript.Initialize
    l_objScript.Init
    Set l_objScript = Nothing
    l_sngProgMul = 60 / m_lyrLayers.Count
    m_Engine.LoadProgress "Map", 10
    If m_lyrLayers.Count > 0 Then
        For l_lngLayers = 1 To m_lyrLayers.Count
            With m_lyrLayers(l_lngLayers)
                If Me.CollisionType = 2 Then
                    ' Create vector collision matrix
                    .CreateCollisionMatrix Me.MaxX, Me.MaxY
                End If
                .UpdateDepthmap
                Set .Sprites.Parent = Me
                .Sprites.SetLayer l_lngLayers
                If .Sprites.Count > 0 Then
                    l_lngSprites = 1
                    For Each l_sprSprite In .Sprites
                        Set l_sprSprite = .Sprites(l_lngSprites)
                        l_sprSprite.SetAttributes l_lngSprites, l_lngLayers, .Sprites, Me
                        l_sprSprite.Initialize
                        If l_sprSprite.Destroyed Then
                            ' Sprite removed itself from sprite list (Sprite.Die)
                            l_lngSprites = l_lngSprites - 1
                            If l_lngSprites < 1 Or l_lngSprites > .Sprites.Count Then Exit For
                        Else
                            m_sprSprites.Add l_sprSprite, False
                        End If
                    Next l_sprSprite
                End If
                .TileAnimations.RedimMapTable .Tileset.TileCount
                .Lighting.Init Me.MaxX, Me.MaxY
            End With
            m_Engine.LoadProgress "Map", 10 + ((80 / m_lyrLayers.Count) * l_lngLayers)
        Next l_lngLayers
    End If
    If m_araAreas.Count > 0 Then
        For l_lngAreas = 1 To m_araAreas.Count
            With m_araAreas(l_lngAreas)
                .SetParent Me
                .Index = l_lngAreas
                Set l_objScript = .Script
                    l_objScript.Init
                    l_objScript.Initialize
                Set l_objScript = Nothing
            End With
            m_Engine.LoadProgress "Map", 90 + ((10 / m_araAreas.Count) * l_lngAreas)
        Next l_lngAreas
    End If
    m_Engine.LoadProgress "Map", 100
    m_Engine.ContextLevelRemove
End Sub

Public Property Get CollisionType() As Long
Attribute CollisionType.VB_Description = "The map's collision type."
Attribute CollisionType.VB_MemberFlags = "40"
    CollisionType = m_lngCollisionType
End Property

Public Property Let CollisionType(NewType As Long)
On Error Resume Next
    m_lngCollisionType = NewType
End Property

Public Property Get Sprites() As Fury2Sprites
Attribute Sprites.VB_Description = "All the sprites from the map's layers."
    Set Sprites = m_sprSprites
End Property

Public Function SpriteFromPoint(X, Y, Optional SolidOnly As Boolean = True, Optional Feet As Boolean = True) As Fury2Sprite
Attribute SpriteFromPoint.VB_Description = "Returns a sprite from the map's sprite array, based on an X/Y coordinate."
'On Error Resume Next
'Dim m_rctRect As Fury2Rect
'Dim m_rctSource As Win32.RECT
'Dim m_rctDest As Win32.RECT
'Dim m_rctIntersect As Win32.RECT
'Dim m_sprSprite As Fury2Sprite
'    With m_rctSource
'        .Left = X
'        .Right = X + 1
'        .Top = Y
'        .Bottom = Y + 1
'    End With
'    For Each m_sprSprite In m_sprSprites
'        If ((m_sprSprite.Solid = True) And (SolidOnly = True)) Or (SolidOnly = False) Then
'            With m_sprSprite
'                If (Feet) Then
'                    Set m_rctRect = .Rectangle(False)
'                Else
'                    Set m_rctRect = .Rectangle(True)
'                End If
'                m_rctDest = m_rctRect.GetRect
'                With m_rctDest
'                    .Left = (.Left + m_sprSprite.X)
'                    .Right = (.Right + m_sprSprite.X)
'                    .Top = (.Top + (m_sprSprite.Y - m_sprSprite.Z))
'                    .Bottom = (.Bottom + (m_sprSprite.Y - m_sprSprite.Z))
'                End With
'                If IntersectRect(m_rctIntersect, m_rctSource, m_rctDest) <> 0 Then
'                    Set SpriteFromPoint = m_sprSprite
'                    Set m_sprSprite = Nothing
'                    Exit Function
'                End If
'            End With
'        End If
'    Next m_sprSprite
'    Set m_sprSprite = Nothing
End Function

Public Property Get Filename() As String
Attribute Filename.VB_Description = "The map's filename (if any)."
    Filename = m_strFilename
End Property

Public Property Let Filename(NewFilename As String)
    m_strFilename = NewFilename
End Property

Public Sub Clear()
Attribute Clear.VB_Description = "Erases all the map's layers."
On Error Resume Next
    Set m_lyrLayers = New Fury2MapLayers
    m_strFilename = ""
    m_strName = ""
    m_lngWidth = 0
    m_lngHeight = 0
End Sub

Public Sub ResizePreserve(Width As Long, Height As Long)
Attribute ResizePreserve.VB_Description = "Resizes the map's layers, preserving any existing data if possible."
Dim m_lyrLayer As Fury2MapLayer
    If m_lyrLayers.Count > 0 Then
        For Each m_lyrLayer In m_lyrLayers
            m_lyrLayer.ResizePreserve Width, Height
        Next m_lyrLayer
    End If
End Sub

Public Sub Render(Surface, Optional ByVal SourceX As Long = 0, Optional ByVal SourceY As Long = 0, Optional ByVal ShowSprites As Boolean = True)
On Error Resume Next
Dim l_imgSurface As Fury2Image
Dim l_camCamera As Fury2Camera
    Set l_imgSurface = Surface
    If l_imgSurface Is Nothing Then Exit Sub
    Set l_camCamera = New Fury2Camera
    Set l_camCamera.ViewingMap = Me
    l_camCamera.EnableLighting = False
    l_camCamera.EnableParallax = False
    l_camCamera.ShowSprites = ShowSprites
    l_camCamera.ViewportX = SourceX
    l_camCamera.ViewportY = SourceY
    l_camCamera.Width = l_imgSurface.Width
    l_camCamera.Height = l_imgSurface.Height
    l_camCamera.Render , l_imgSurface
End Sub

Public Sub Resize(Width As Long, Height As Long)
Attribute Resize.VB_Description = "Resizes the map's layers, throwing away any existing data."
Dim m_lyrLayer As Fury2MapLayer
    If m_lyrLayers.Count > 0 Then
        For Each m_lyrLayer In m_lyrLayers
            m_lyrLayer.Resize Width, Height
        Next m_lyrLayer
    End If
End Sub

Sub AddSprite(ByVal Sprite, ByVal Layer, Optional ByVal X = -32767, Optional ByVal Y = -32767)
On Error Resume Next
Dim m_lngLayer As Long
Dim m_sngX As Single, m_sngY As Single
    Err.Clear
    m_lngLayer = CLng(Layer)
    m_sngX = CSng(X)
    m_sngY = CSng(Y)
    If Sprite Is Nothing Then Exit Sub
    If m_lngLayer < 1 Then m_lngLayer = 1
    If m_lngLayer > m_lyrLayers.Count Then m_lngLayer = m_lyrLayers.Count
    m_lyrLayers(m_lngLayer).Sprites.Add Sprite, True
    If m_sngX <> -32767 And m_sngY <> -32767 Then
        Sprite.Initialize
        Sprite.Load
        Sprite.Teleport m_sngX, m_sngY
    End If
End Sub

Sub CreateNew(Name As String, ByVal Width As Long, ByVal Height As Long, ByVal LayerCount As Long, Tileset As Fury2Tileset)
Attribute CreateNew.VB_Description = "Creates a new map."
On Error Resume Next
Dim m_CreateLayers As Long, m_NewLayer As Fury2MapLayer, cX As Long, cY As Long
    m_strFilename = ""
    m_strName = Name
    Width = ClipValue(Width, c_lngMinimumMapWidth, c_lngMaximumMapWidth)
    Height = ClipValue(Height, c_lngMinimumMapHeight, c_lngMaximumMapHeight)
    Set m_lyrLayers = New Fury2MapLayers
    If Tileset Is Nothing Then
    ElseIf Tileset.TileCount = 0 Then
        Tileset.Load
    End If
    If LayerCount > 0 Then
        For m_CreateLayers = 1 To ClipValue(LayerCount, c_lngMinimumMapLayers, c_lngMaximumMapLayers)
            Err.Clear
            Set m_NewLayer = New Fury2MapLayer
            If Tileset Is Nothing Then
                Set m_NewLayer.Tileset = New Fury2Tileset
            Else
                Set m_NewLayer.Tileset = Tileset.Duplicate
            End If
            m_NewLayer.Resize Width, Height
            m_NewLayer.Alpha = 1
            m_NewLayer.Visible = True
            If Tileset Is Nothing Then
            Else
                m_NewLayer.Clear Tileset.TransparentTile
            End If
            m_lyrLayers.Add m_NewLayer
        Next m_CreateLayers
    End If
    CollisionType = m_lngCollisionType
    Err.Clear
    Init
End Sub

Public Property Let Height(NewHeight As Long)
    If NewHeight <> Height Then
        ResizePreserve Width, NewHeight
    End If
End Property

Public Property Get Height() As Long
Attribute Height.VB_Description = "The map's height."
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    For Each l_lyrLayer In m_lyrLayers
        With l_lyrLayer
            If .Height > Height Then Height = .Height
        End With
    Next l_lyrLayer
End Property

Public Property Let Width(NewWidth As Long)
    If NewWidth <> Width Then
        ResizePreserve NewWidth, Height
    End If
End Property

Public Property Get Width() As Long
Attribute Width.VB_Description = "The map's width."
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    For Each l_lyrLayer In m_lyrLayers
        With l_lyrLayer
            If .Width > Width Then Width = .Width
        End With
    Next l_lyrLayer
End Property

Public Property Let Name(vData As String)
Attribute Name.VB_Description = "The map's name."
Attribute Name.VB_MemberFlags = "200"
    m_strName = vData
End Property

Public Property Get Name() As String
    Name = m_strName
End Property

Public Property Get Objects() As Fury2MapObjects
    Set Objects = m_mobObjects
End Property

Public Property Set Layers(vData As Fury2MapLayers)
Attribute Layers.VB_Description = "The map's layers."
    Set m_lyrLayers = vData
End Property

Public Property Get Layers() As Fury2MapLayers
    Set Layers = m_lyrLayers
End Property

Public Property Get TimerEvents() As Fury2TimerEvents
    Set TimerEvents = m_tmrTimerEvents
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_sprSprites = New Fury2Sprites
    Set m_araAreas = New Fury2Areas
    m_araAreas.SetParent Me
    Set m_mobObjects = New Fury2MapObjects
    m_mobObjects.SetParent Me
    Set m_lyrLayers = New Fury2MapLayers
    Set m_lyrLayers.Parent = Me
    Set m_revRevision = New Fury2RevisionData
    Set m_tmrTimerEvents = New Fury2TimerEvents
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    If Script Is Nothing Then
    Else
        If m_booScript_Unload Then Script.Unload
    End If
End Sub

Sub Update()
Attribute Update.VB_MemberFlags = "40"
On Error Resume Next
Dim l_lngLayers As Long
Dim l_lyrLayer As Fury2MapLayer
Dim l_lngObject As Long
Dim l_mobObject As Fury2MapObject
    If Width < 0 Or Height < 0 Then
        CriticalError "Fury2Map.Update", "Sanity Check", "Map has no width and/or height. Possibly corrupted. If you want an empty map, use the UnloadAllMaps command.": Exit Sub
    End If
    If Paused Then Exit Sub
    If Started = False Then
        Started = True
        m_objScript.Start
    End If
    Err.Clear
    If m_booScript_Update = True Then
        m_Engine.ContextLevelAdd "Map_Update"
        ProfileStart "Map_Update"
        If m_objScript Is Nothing Then Else m_objScript.Update
        ProfileStop "Map_Update"
        m_Engine.ContextLevelRemove
    End If
    If m_lyrLayers.Count > 0 Then
        l_lngLayers = 1
        For Each l_lyrLayer In m_lyrLayers
            m_Engine.ContextLevelAdd "Layers(" + CStr(l_lngLayers) + ")"
            ProfileStart "Map.Sprites.Update"
            l_lyrLayer.Sprites.Update Me, CLng(l_lngLayers), True
            ProfileStop "Map.Sprites.Update"
            ProfileStart "Map.Lights.Update"
            l_lyrLayer.Lighting.Update
            ProfileStop "Map.Lights.Update"
            m_Engine.ContextLevelRemove
            If l_lyrLayer.TileAnimations Is Nothing Then
            Else
                ProfileStart "Map.TileAnimations.Update"
                l_lyrLayer.TileAnimations.Update
                ProfileStop "Map.TileAnimations.Update"
            End If
            l_lngLayers = l_lngLayers + 1
        Next l_lyrLayer
    End If
    For Each l_mobObject In m_mobObjects
            l_mobObject.Update
    Next l_mobObject
    Err.Clear
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    m_booScriptFailed = False
    Set m_objScript = Nothing
    
    With File
        .ReadSegment_Begin
            .ReadSegment_Begin
                .Load m_strFilename
                .Load m_strName
            .ReadSegment_End
            .ReadSegment_Begin
                .Load m_lngWidth
                .Load m_lngHeight
            .ReadSegment_End
            .ReadSegment_Begin
                .Load m_lngCollisionType
            .ReadSegment_End
            .ReadSegment_Begin
                .Load m_strScriptSource
            .ReadSegment_End
            .Load m_araAreas
            m_araAreas.SetParent Me
            .Load m_lyrLayers
            .ReadSegment_Begin
                .Load m_strSettings()
            .ReadSegment_End
            .Load m_tmrTimerEvents
            Set m_mobObjects = New Fury2MapObjects
            m_mobObjects.SetParent Me
            .Load m_mobObjects
        .ReadSegment_End
    End With
    
    ' Initialize
    CollisionType = m_lngCollisionType
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .WriteSegment_Begin
                .Save m_strFilename
                .Save m_strName
            .WriteSegment_End
            .WriteSegment_Begin
                .Save Width
                .Save Height
            .WriteSegment_End
            .WriteSegment_Begin
                .Save m_lngCollisionType
            .WriteSegment_End
            .WriteSegment_Begin
                .Save m_strScriptSource
            .WriteSegment_End
            .Save m_araAreas
            .Save m_lyrLayers
            .WriteSegment_Begin
                .Save m_strSettings()
            .WriteSegment_End
            .Save m_tmrTimerEvents
            .Save m_mobObjects
        .WriteSegment_End
    End With
End Sub

