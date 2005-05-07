VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#8.12#0"; "ngUI.ocx"
Begin VB.Form frmMap 
   BorderStyle     =   0  'None
   ClientHeight    =   5580
   ClientLeft      =   0
   ClientTop       =   15
   ClientWidth     =   9510
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmMap.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   372
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   634
   ShowInTaskbar   =   0   'False
   Begin VB.Timer tmrResize 
      Interval        =   1
      Left            =   4155
      Top             =   2550
   End
   Begin VB.PictureBox picDisplayBuffer 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   885
      Left            =   8295
      ScaleHeight     =   59
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   80
      TabIndex        =   0
      Top             =   15
      Visible         =   0   'False
      Width           =   1200
      Begin VB.PictureBox picHarmless 
         BorderStyle     =   0  'None
         Height          =   15
         Left            =   0
         ScaleHeight     =   1
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   1
         TabIndex        =   21
         Top             =   0
         Width           =   15
      End
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   -15
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin VB.PictureBox picContainer 
      BorderStyle     =   0  'None
      Height          =   5145
      Left            =   240
      ScaleHeight     =   343
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   602
      TabIndex        =   1
      Top             =   165
      Width           =   9030
      Begin ngPlugins.Script scMap 
         Height          =   210
         Left            =   345
         TabIndex        =   17
         Top             =   4950
         Visible         =   0   'False
         Width           =   795
         _ExtentX        =   1402
         _ExtentY        =   370
      End
      Begin VB.PictureBox picMapView 
         BorderStyle     =   0  'None
         Height          =   4515
         Left            =   -30
         ScaleHeight     =   301
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   438
         TabIndex        =   3
         Top             =   300
         Visible         =   0   'False
         Width           =   6570
         Begin cFScroll.FlatScrollBar vsMap 
            Height          =   6315
            Left            =   0
            TabIndex        =   6
            Top             =   0
            Width           =   270
            _ExtentX        =   476
            _ExtentY        =   11139
            Orientation     =   1
            Max             =   100
            Style           =   -1
         End
         Begin VB.PictureBox picMapViewport 
            BackColor       =   &H00000000&
            BorderStyle     =   0  'None
            Height          =   4275
            Left            =   180
            ScaleHeight     =   285
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   422
            TabIndex        =   4
            Top             =   -15
            Width           =   6330
            Begin VB.PictureBox picOverlay 
               AutoRedraw      =   -1  'True
               BackColor       =   &H00000000&
               BorderStyle     =   0  'None
               Height          =   435
               Left            =   1110
               ScaleHeight     =   29
               ScaleMode       =   3  'Pixel
               ScaleWidth      =   36
               TabIndex        =   5
               Top             =   825
               Visible         =   0   'False
               Width           =   540
            End
         End
         Begin cFScroll.FlatScrollBar hsMap 
            Height          =   270
            Left            =   0
            TabIndex        =   7
            Top             =   0
            Width           =   6315
            _ExtentX        =   11139
            _ExtentY        =   476
            Max             =   100
            Style           =   -1
         End
      End
      Begin VB.PictureBox picSidebar 
         BorderStyle     =   0  'None
         Height          =   4920
         Left            =   6735
         ScaleHeight     =   328
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   180
         TabIndex        =   8
         Top             =   15
         Width           =   2700
         Begin ngUI.ngTabStrip tsLists 
            Height          =   570
            Left            =   180
            TabIndex        =   23
            Top             =   465
            Width           =   795
            _ExtentX        =   1402
            _ExtentY        =   1005
         End
         Begin VB.PictureBox picInspectors 
            BorderStyle     =   0  'None
            Height          =   3030
            Left            =   165
            ScaleHeight     =   202
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   139
            TabIndex        =   13
            Top             =   1860
            Width           =   2085
            Begin VB.PictureBox picBrush 
               BackColor       =   &H80000014&
               BorderStyle     =   0  'None
               Height          =   615
               Left            =   900
               ScaleHeight     =   41
               ScaleMode       =   3  'Pixel
               ScaleWidth      =   42
               TabIndex        =   20
               Top             =   1830
               Visible         =   0   'False
               Width           =   630
            End
            Begin ngPlugins.ObjectInspector insTool 
               Height          =   660
               Left            =   90
               TabIndex        =   16
               Top             =   1890
               Visible         =   0   'False
               Width           =   660
               _ExtentX        =   1164
               _ExtentY        =   1164
            End
            Begin ngPlugins.TilePicker tpkTiles 
               Height          =   675
               Left            =   1335
               TabIndex        =   14
               Top             =   600
               Width           =   420
               _ExtentX        =   741
               _ExtentY        =   1191
            End
            Begin ngPlugins.ObjectInspector insInspect 
               Height          =   660
               Left            =   75
               TabIndex        =   15
               Top             =   75
               Visible         =   0   'False
               Width           =   660
               _ExtentX        =   1164
               _ExtentY        =   1164
            End
            Begin ngPlugins.Script scObject 
               Height          =   975
               Left            =   0
               TabIndex        =   19
               Top             =   0
               Visible         =   0   'False
               Width           =   810
               _ExtentX        =   1429
               _ExtentY        =   1720
            End
            Begin ngUI.ngTabStrip tsInspector 
               Height          =   1110
               Left            =   345
               TabIndex        =   24
               Top             =   180
               Width           =   1380
               _ExtentX        =   2434
               _ExtentY        =   1958
            End
            Begin ngUI.ngTabStrip tsTool 
               Height          =   1425
               Left            =   285
               TabIndex        =   25
               Top             =   1590
               Width           =   1425
               _ExtentX        =   2514
               _ExtentY        =   2514
            End
         End
         Begin ngPlugins.EntityList elLayers 
            Height          =   1680
            Left            =   -540
            TabIndex        =   9
            Top             =   15
            Visible         =   0   'False
            Width           =   1020
            _ExtentX        =   1799
            _ExtentY        =   2963
            EnableDragging  =   -1  'True
         End
         Begin ngPlugins.EntityList elAreas 
            Height          =   1770
            Left            =   -195
            TabIndex        =   10
            Top             =   15
            Visible         =   0   'False
            Width           =   1020
            _ExtentX        =   1799
            _ExtentY        =   3122
            EnableDragging  =   -1  'True
         End
         Begin ngPlugins.EntityList elSprites 
            Height          =   1770
            Left            =   150
            TabIndex        =   12
            Top             =   15
            Visible         =   0   'False
            Width           =   1020
            _ExtentX        =   1799
            _ExtentY        =   3122
            EnableDragging  =   -1  'True
         End
         Begin ngPlugins.EntityList elLights 
            Height          =   1770
            Left            =   465
            TabIndex        =   11
            Top             =   15
            Visible         =   0   'False
            Width           =   1020
            _ExtentX        =   1799
            _ExtentY        =   3122
            EnableDragging  =   -1  'True
         End
         Begin ngPlugins.EntityList elObjects 
            Height          =   1770
            Left            =   765
            TabIndex        =   18
            Top             =   45
            Visible         =   0   'False
            Width           =   1020
            _ExtentX        =   1799
            _ExtentY        =   3122
            EnableDragging  =   -1  'True
         End
      End
      Begin ngPlugins.ObjectInspector insMap 
         Height          =   660
         Left            =   0
         TabIndex        =   2
         Top             =   0
         Visible         =   0   'False
         Width           =   660
         _ExtentX        =   1164
         _ExtentY        =   1164
      End
      Begin ngUI.ngTabStrip tsViews 
         Height          =   1890
         Left            =   3390
         TabIndex        =   22
         Top             =   105
         Width           =   3075
         _ExtentX        =   5424
         _ExtentY        =   3334
      End
   End
End
Attribute VB_Name = "frmMap"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
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
Private Declare Function ScreenToClient Lib "user32" (ByVal hwnd As Long, lpPoint As PointAPI) As Long
Implements iExtendedForm
Implements iEditingCommands
Implements iCustomMenus
Implements iDocument
Implements iToolbar

Private Const c_lngAutoscrollOffset As Long = 8

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private Enum TileTools
    TileTool_Pen
    TileTool_Line
    TileTool_Rectangle
    TileTool_FilledRectangle
    TileTool_Circle
    TileTool_FilledCircle
    TileTool_FloodFill
    TileTool_Replace
    TileTool_Selection
End Enum

Private Enum BlockingTools
    BlockingTool_Line
    BlockingTool_PolyLine
    BlockingTool_Rectangle
    BlockingTool_Select
    BlockingTool_Move
End Enum

Private Enum LightingTools
    LightingTool_Cursor
    LightingTool_Light
    LightingTool_Obstruction
    LightingTool_ObstructionRectangle
    LightingTool_SelectObstructions
    LightingTool_MoveObstructions
    LightingTool_Plane
    LightingTool_SelectPlanes
    LightingTool_MovePlanes
End Enum

Private Enum AreaTools
    AreaTool_Cursor
    AreaTool_Draw
End Enum

Private Enum SpriteTools
    SpriteTool_Cursor
    SpriteTool_Insert
    SpriteTool_Add_Path
    SpriteTool_Select_Path
End Enum

Private Enum ObjectTools
    ObjectTool_Cursor
    ObjectTool_Create_Sound
End Enum

Private Enum MapViews
    View_Tiles
    View_Blocking
    View_Lighting
    View_Areas
    View_Sprites
    View_Objects
    View_Script
    View_Properties
End Enum

Private m_voViewOptions As New MapEditorViewOptions

Private m_rctActionArea As Fury2Rect

Private m_colUndo As New Engine.Fury2Collection
Private m_colRedo As New Engine.Fury2Collection

Private m_splSidebar As New cSplitter, m_splSidebarPanels As New cSplitter, m_splSidebarInspectors As New cSplitter
Private m_mapMap As Fury2Map
Private m_camCamera As Fury2Camera
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin
Private m_imgBackbuffer As Fury2Image
Private m_imgOverlay As Fury2Image, m_lngOverlayDC As Long
Private m_imgBrush As Fury2Image
Private m_lngBackbufferOldHandle As Long

Private m_ctlCurrentList As Control
Private m_ctlCurrentInspector As Control
Private m_ctlCurrentTool As Control

Private m_lngCurrentView As MapViews
Private m_lngCurrentTool(View_Tiles To View_Properties) As Long
Private m_lngViewWidth As Long, m_lngViewHeight As Long
Private m_lngDisplayWidth As Long, m_lngDisplayHeight As Long
Private m_booVisible As Boolean

Private m_booDraggingArea As Boolean
Private m_booDraggingSprite As Boolean
Private m_booDraggingLight As Boolean, m_booResizingLight As Boolean, m_booRotatingLight As Boolean
Private m_booDraggingObject As Boolean
Private m_booDraggingCollisionLines As Boolean
Private m_booDraggingLightingObstructions As Boolean
Private m_booDraggingLightingPlanes As Boolean
Private m_booDraggingPathNodes As Boolean
Private m_booSelectingBlocking As Boolean, m_booSelectingLightingObstructions As Boolean, m_booSelectingLightingPlanes As Boolean, m_booSelectingPathNodes As Boolean
Private m_rctDragStart As Fury2Rect, m_sngDragStartX As Single, m_sngDragStartY As Single

Private m_lngSelectedLayer As Long
Private m_lngSelectedArea As Long
Private m_lngSelectedSprite As Long
Private m_lngSelectedLight As Long
Private m_lngSelectedObject As Long
Private m_bytSelectedCollisionLines() As Byte
Private m_bytSelectedLightingObstructions() As Byte
Private m_bytSelectedLightingPlanes() As Byte
Private m_bytSelectedPathNodes() As Byte

Private m_plnLightingPlaneCache() As LightingPlane2
Private m_lobLightingObstructionCache() As LightingObstruction
Private m_lnCollisionLineCache() As FLine

Private m_booMouseMoved As Boolean, m_booMouseMovedTile As Boolean

Private m_fptPoints(0 To 7) As FPoint
Private m_lngPoint As Long

Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngMouseTileX As Long, m_lngMouseTileY As Long
Private m_lngRealMouseX As Long, m_lngRealMouseY As Long

Private m_lngLastMouseX As Long, m_lngLastMouseY As Long
Private m_lngLastMouseTileX As Long, m_lngLastMouseTileY As Long

Private m_lngStartMouseX As Long, m_lngStartMouseY As Long
Private m_lngStartMouseTileX As Long, m_lngStartMouseTileY As Long

Private m_lngOverlayWidth As Long, m_lngOverlayHeight As Long, m_lngOverlayX As Long, m_lngOverlayY As Long
Private m_booOverlayLocked As Boolean

Private m_lngTileWidth As Long, m_lngTileHeight As Long

Private m_lngMouseButtons As Long

Private m_brsBrush As Fury2Brush

Private m_intLayerCache() As Integer

Private WithEvents m_tbrToolbar As ngToolbar
Attribute m_tbrToolbar.VB_VarHelpID = -1

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iDocument_DocumentIcon = Editor.LoadResources("ng").ItemData("icons\map.png")
End Property

Private Function Engine() As Fury2Engine
On Error Resume Next
    Set Engine = Editor.Engine
End Function

Public Property Get Map() As Fury2Map
On Error Resume Next
    Set Map = m_mapMap
End Property

Public Sub RunMacro()
On Error Resume Next
Dim l_strFilename As String
    l_strFilename = Editor.SelectFile("Macros|*.f2macro;*.f2script", "Select Macro")
    If Len(Trim(l_strFilename)) > 0 Then
        Engine.ScriptEngine.AddObject "Document", Me, True, True, True
        Editor.LogOutput "Running macro """ & l_strFilename & """"
        Engine.ScriptEngine.AddCode ReadTextFile(l_strFilename)
    End If
End Sub

Public Property Get ActiveType() As String
On Error Resume Next
    Select Case LCase(Trim(Me.ActiveControl.Name))
    Case "scmap"
        ActiveType = "Script"
    Case "scobject"
        ActiveType = "Object Script"
    Case "elareas"
        ActiveType = "Areas"
    Case "elsprites"
        ActiveType = "Sprites"
    Case "ellights"
        ActiveType = "Lights"
    Case "ellayers"
        ActiveType = "Layers"
    Case "picbrush", "tpktiles"
        ActiveType = "Brush"
    Case "picoverlay", "picmapview", "picmapviewport", "hsmap", "vsmap", "tsViews"
        Select Case m_lngCurrentView
        Case View_Tiles
            ActiveType = "Tiles"
        Case View_Blocking
            ActiveType = "Blocking"
        Case View_Lighting
            Select Case Tool_Lighting
            Case LightingTool_Cursor, LightingTool_Light
                ActiveType = "Lights"
            Case LightingTool_Obstruction, LightingTool_ObstructionRectangle, LightingTool_SelectObstructions, LightingTool_MoveObstructions
                ActiveType = "Lighting Obstructions"
            Case LightingTool_Plane, LightingTool_SelectPlanes, LightingTool_MovePlanes
                ActiveType = "Lighting Planes"
            End Select
        Case View_Areas
            ActiveType = "Areas"
        Case View_Sprites
            Select Case Tool_Sprites
            Case SpriteTool_Cursor, SpriteTool_Insert
                ActiveType = "Sprites"
            Case SpriteTool_Add_Path, SpriteTool_Select_Path
                ActiveType = "Path Nodes"
            End Select
        Case View_Objects
            ActiveType = "Objects"
        Case Else
        End Select
    Case Else
    End Select
End Property

Public Sub AddEdgeBlocking()
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
Dim l_lngW As Long, l_lngH As Long
    For Each l_lyrLayer In m_mapMap.Layers
        With l_lyrLayer
            l_lngW = (.Width * .Tileset.TileWidth) - 1
            l_lngH = (.Height * .Tileset.TileHeight) - 1
            .AddCollisionLine 0, 0, l_lngW, 0
            .AddCollisionLine 0, 0, 0, l_lngH
            .AddCollisionLine l_lngW, 0, l_lngW, l_lngH
            .AddCollisionLine 0, l_lngH, l_lngW, l_lngH
        End With
    Next l_lyrLayer
    ReDim Preserve m_bytSelectedCollisionLines(0 To m_mapMap.Layers(m_lngSelectedLayer).CollisionLineCount)
    SelectCollisionLines m_mapMap.Layers(m_lngSelectedLayer).CollisionLineCount - 3, m_mapMap.Layers(m_lngSelectedLayer).CollisionLineCount
    Redraw
End Sub

Public Sub AllocateBackbuffer()
On Error Resume Next
    picDisplayBuffer.AutoRedraw = True
    Set m_imgBackbuffer = F2DIBSection(picDisplayBuffer.ScaleWidth, picDisplayBuffer.ScaleHeight, picMapViewport.hdc)
    m_lngBackbufferOldHandle = SelectObject(picDisplayBuffer.hdc, m_imgBackbuffer.DIBHandle)
End Sub

Public Function AreaFromPoint(ByVal X As Long, ByVal Y As Long) As Fury2Area
On Error Resume Next
Dim l_araArea As Fury2Area
    For Each l_araArea In m_mapMap.Areas
        If l_araArea.PointInside(X, Y) Then
            Set AreaFromPoint = l_araArea
            Exit For
        End If
    Next l_araArea
End Function

Public Sub AutoScroll(Optional ByVal X As Long = -32767, Optional ByVal Y As Long = -32767, Optional ByVal DoRedraw As Boolean = True)
On Error Resume Next
Dim l_lngX1 As Long, l_lngY1 As Long, l_lngX2 As Long, l_lngY2 As Long
Dim l_lngScrollX As Long, l_lngScrollY As Long
Dim l_ptCursor As PointAPI
Dim l_lngCapture As Long
    If Not m_voViewOptions.AutoScroll Then Exit Sub
    hsMap.Tag = "lock"
    vsMap.Tag = "lock"
    If X = -32767 Then
        X = m_lngMouseX
    End If
    If Y = -32767 Then
        Y = m_lngMouseY
    End If
    l_lngX1 = hsMap.Value + c_lngAutoscrollOffset
    l_lngY1 = vsMap.Value + c_lngAutoscrollOffset
    l_lngX2 = hsMap.Value + m_lngViewWidth - c_lngAutoscrollOffset
    l_lngY2 = vsMap.Value + m_lngViewHeight - c_lngAutoscrollOffset
    If l_lngX2 < l_lngX1 Then l_lngX2 = l_lngX1
    If l_lngY2 < l_lngY1 Then l_lngY1 = l_lngY1
    If (X < l_lngX1) Then
        l_lngScrollX = -(l_lngX1 - X)
    ElseIf (X > l_lngX2) Then
        l_lngScrollX = X - l_lngX2
    End If
    If (Y < l_lngY1) Then
        l_lngScrollY = -(l_lngY1 - Y)
    ElseIf (Y > l_lngY2) Then
        l_lngScrollY = Y - l_lngY2
    End If
    If (hsMap.Value <= hsMap.Min) And (l_lngScrollX < 0) Then l_lngScrollX = 0
    If (hsMap.Value >= hsMap.Max) And (l_lngScrollX > 0) Then l_lngScrollX = 0
    If (vsMap.Value <= vsMap.Min) And (l_lngScrollY < 0) Then l_lngScrollY = 0
    If (vsMap.Value >= vsMap.Max) And (l_lngScrollY > 0) Then l_lngScrollY = 0
    If (l_lngScrollX) Or (l_lngScrollY) Then
        l_lngCapture = GetCapture()
        If hsMap.Enabled Then
            hsMap.Value = ClipValue(hsMap.Value + l_lngScrollX, hsMap.Min, hsMap.Max)
        Else
            l_lngScrollX = 0
        End If
        If vsMap.Enabled Then
            vsMap.Value = ClipValue(vsMap.Value + l_lngScrollY, vsMap.Min, vsMap.Max)
        Else
            l_lngScrollY = 0
        End If
        GetCursorPos l_ptCursor
        SetCursorPos l_ptCursor.X - l_lngScrollX, l_ptCursor.Y - l_lngScrollY
        If DoRedraw Then Redraw
        SetCapture l_lngCapture
    End If
    hsMap.Tag = ""
    vsMap.Tag = ""
End Sub

Public Sub Blocking_ToolChanged()
On Error Resume Next
    If Tool_Blocking = BlockingTool_PolyLine Then
        SelectCollisionLines -1, -1
    End If
    m_lngPoint = 0
    Redraw
End Sub

Public Sub BlockingUndoPush(Optional ByVal Layer As Long = -1)
On Error Resume Next
Dim l_undUndo As cBlockingUndoEntry
    If Layer = -1 Then Layer = m_lngSelectedLayer
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cBlockingUndoEntry
    With l_undUndo
        Set .Map = m_mapMap
        .Layer = Layer
        .Lines = m_mapMap.Layers(Layer).CollisionLines
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Property Get BrushXCenter() As Long
On Error Resume Next
    BrushXCenter = Floor(m_brsBrush.Width / 2)
End Property

Public Property Get BrushYCenter() As Long
On Error Resume Next
    BrushYCenter = Floor(m_brsBrush.Height / 2)
End Property

Public Sub CacheLayer()
On Error Resume Next
    m_intLayerCache = m_mapMap.Layers(m_lngSelectedLayer).Tiles
End Sub

Public Sub Cleanup()
On Error Resume Next
    Set elAreas.BoundObject = Nothing
    Set elLayers.BoundObject = Nothing
    Set elLights.BoundObject = Nothing
    Set elObjects.BoundObject = Nothing
    Set elSprites.BoundObject = Nothing
    DeallocateBackbuffer
    Set m_imgOverlay = Nothing
    DeleteDC m_lngOverlayDC
    Set m_fpgPlugin = Nothing
    Set m_camCamera = Nothing
    Set m_splSidebar = Nothing
    Set m_splSidebarPanels = Nothing
    Set m_splSidebarInspectors = Nothing
    Set m_ctlCurrentList = Nothing
    Set m_ctlCurrentInspector = Nothing
    m_mapMap.Free
    Set m_mapMap = Nothing
End Sub

Public Sub ClearOverlay()
On Error Resume Next
Dim l_lngXO As Long, l_lngYO As Long
    m_imgOverlay.Clear
    If (m_lngOverlayX - hsMap.Value) < 0 Then
        l_lngXO = -(m_lngOverlayX - hsMap.Value)
    End If
    If (m_lngOverlayY - vsMap.Value) < 0 Then
        l_lngYO = -(m_lngOverlayY - vsMap.Value)
    End If
    m_imgOverlay.Blit m_imgOverlay.Rectangle.Translate(l_lngXO, l_lngYO), F2Rect(ClipValue(m_lngOverlayX - hsMap.Value, 0, m_imgBackbuffer.Width), ClipValue(m_lngOverlayY - vsMap.Value, 0, m_imgBackbuffer.Width), m_lngOverlayWidth, m_lngOverlayHeight, False), m_imgBackbuffer
End Sub

Private Function ClipboardContainsFormat(Format As MapEditorClipboardFormats) As Boolean
On Error Resume Next
Dim l_objPlugin As MapEditor
    Set l_objPlugin = m_fpgPlugin
    With l_objPlugin.CustomClipboard
        .GetCurrentFormats Me.hwnd
        ClipboardContainsFormat = .HasCurrentFormat(l_objPlugin.ClipboardFormat(Format))
    End With
End Function

Private Function ClipboardFormat(Format As MapEditorClipboardFormats) As Long
On Error Resume Next
Dim l_objPlugin As MapEditor
    Set l_objPlugin = m_fpgPlugin
    ClipboardFormat = l_objPlugin.ClipboardFormat(Format)
End Function

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Public Sub CopyArea()
On Error Resume Next
Dim l_araArea As Fury2Area
    BeginProcess "Performing Copy..."
    Set l_araArea = m_mapMap.Areas(m_lngSelectedArea)
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_Area), l_araArea
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopyBrush()
On Error Resume Next
    BeginProcess "Performing Copy..."
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_Brush), m_brsBrush
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopyCollisionLines()
On Error Resume Next
Dim l_lngLine As Long
Dim l_lngCount As Long
Dim l_lnLines() As FLine
Dim l_lnLayer() As FLine
Dim l_vfLines As VirtualFile
    BeginProcess "Performing Copy..."
    ReDim l_lnLines(0 To 0)
    With m_mapMap.Layers(m_lngSelectedLayer)
        l_lnLayer = .CollisionLines
        For l_lngLine = UBound(m_bytSelectedCollisionLines) To 1 Step -1
            If m_bytSelectedCollisionLines(l_lngLine) Then
                ReDim Preserve l_lnLines(0 To l_lngCount)
                l_lnLines(l_lngCount) = l_lnLayer(l_lngLine - 1)
                l_lngCount = l_lngCount + 1
            End If
        Next l_lngLine
    End With
    Set l_vfLines = New VirtualFile
    l_vfLines.Save l_lngCount
    l_vfLines.RawSave ByVal VarPtr(l_lnLines(0)), l_lngCount * Len(l_lnLines(0))
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_MapObstructions), l_vfLines
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopyLayer()
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    BeginProcess "Performing Copy..."
    Set l_lyrLayer = m_mapMap.Layers(m_lngSelectedLayer)
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_MapLayer), l_lyrLayer
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopyLight()
On Error Resume Next
Dim l_litLight As Fury2LightSource
    BeginProcess "Performing Copy..."
    Set l_litLight = m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights(m_lngSelectedLight)
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_LightSource), l_litLight
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopyLightingObstructions()
On Error Resume Next
Dim l_lngLine As Long
Dim l_lngCount As Long
Dim l_obsLines() As LightingObstruction
Dim l_obsLayer() As LightingObstruction
Dim l_vfLines As VirtualFile
    BeginProcess "Performing Copy..."
    ReDim l_obsLines(0 To 0)
    With m_mapMap.Layers(m_lngSelectedLayer).Lighting
        l_obsLayer = .Obstructions
        For l_lngLine = UBound(m_bytSelectedLightingObstructions) To 1 Step -1
            If m_bytSelectedLightingObstructions(l_lngLine) Then
                ReDim Preserve l_obsLines(0 To l_lngCount)
                l_obsLines(l_lngCount) = l_obsLayer(l_lngLine - 1)
                l_lngCount = l_lngCount + 1
            End If
        Next l_lngLine
    End With
    Set l_vfLines = New VirtualFile
    l_vfLines.Save l_lngCount
    l_vfLines.RawSave ByVal VarPtr(l_obsLines(0)), l_lngCount * Len(l_obsLines(0))
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_LightObstructions), l_vfLines
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopyLightingPlanes()
On Error Resume Next
Dim l_lngPlane As Long
Dim l_lngCount As Long
Dim l_plnPlanes() As LightingPlane2
Dim l_plnLayer() As LightingPlane2
Dim l_vfPlanes As VirtualFile
    BeginProcess "Performing Copy..."
    ReDim l_plnPlanes(0 To 0)
    With m_mapMap.Layers(m_lngSelectedLayer).Lighting
        l_plnLayer = .Planes
        For l_lngPlane = UBound(m_bytSelectedLightingPlanes) To 1 Step -1
            If m_bytSelectedLightingPlanes(l_lngPlane) Then
                ReDim Preserve l_plnPlanes(0 To l_lngCount)
                l_plnPlanes(l_lngCount) = l_plnLayer(l_lngPlane - 1)
                l_lngCount = l_lngCount + 1
            End If
        Next l_lngPlane
    End With
    Set l_vfPlanes = New VirtualFile
    l_vfPlanes.Save l_lngCount
    l_vfPlanes.RawSave ByVal VarPtr(l_plnPlanes(0)), l_lngCount * Len(l_plnPlanes(0))
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_LightPlanes), l_vfPlanes
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopyPathNodes()
On Error Resume Next
Dim l_lngNode As Long
Dim l_wpNode As Fury2Waypoint
Dim l_vfNodes As VirtualFile
Dim l_colNodes As New Fury2Collection
    BeginProcess "Performing Copy..."
    Set l_vfNodes = New VirtualFile
    With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
        l_lngNode = 1
        For Each l_wpNode In .Path
            If m_bytSelectedPathNodes(l_lngNode) Then
                l_colNodes.Add l_wpNode
            End If
            l_lngNode = l_lngNode + 1
        Next l_wpNode
    End With
    l_vfNodes.Save l_colNodes.Count
    For Each l_wpNode In l_colNodes
        l_vfNodes.Save l_wpNode
    Next l_wpNode
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_PathNodes), l_vfNodes
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Public Sub CopySprite()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    BeginProcess "Performing Copy..."
    Set l_sprSprite = m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_Sprite), l_sprSprite
    CustomClipboard.ClipboardClose
    EndProcess
End Sub

Private Function CustomClipboard() As cCustomClipboard
On Error Resume Next
Dim l_objPlugin As MapEditor
    Set l_objPlugin = m_fpgPlugin
    Set CustomClipboard = l_objPlugin.CustomClipboard
End Function

Public Sub CutArea()
On Error Resume Next
    CopyArea
    DeleteArea
End Sub

Public Sub CutBrush()
On Error Resume Next
    CopyBrush
    DeleteBrush
End Sub

Public Sub CutCollisionLines()
On Error Resume Next
    CopyCollisionLines
    DeleteCollisionLines
End Sub

Public Sub CutLayer()
On Error Resume Next
    CopyLayer
    DeleteLayer
End Sub

Public Sub CutLight()
On Error Resume Next
    CopyLight
    DeleteLight
End Sub

Public Sub CutLightingObstructions()
On Error Resume Next
    CopyLightingObstructions
    DeleteLightingObstructions
End Sub

Public Sub CutLightingPlanes()
On Error Resume Next
    CopyLightingPlanes
    DeleteLightingPlanes
End Sub

Public Sub CutPathNodes()
On Error Resume Next
    CopyPathNodes
    DeletePathNodes
End Sub

Public Sub CutSprite()
On Error Resume Next
    CopySprite
    DeleteSprite
End Sub

Public Sub DeallocateBackbuffer()
On Error Resume Next
    If m_imgBackbuffer Is Nothing Then
    Else
        SelectObject picDisplayBuffer.hdc, m_lngBackbufferOldHandle
        m_lngBackbufferOldHandle = 0
        Set m_imgBackbuffer = Nothing
    End If
    picDisplayBuffer.AutoRedraw = False
End Sub

Public Sub DeleteArea()
On Error Resume Next
    BeginProcess "Performing Delete..."
    ObjectUndoPush m_mapMap.Areas, m_mapMap.Areas(m_lngSelectedArea), m_lngSelectedArea, OUO_Add
    m_mapMap.Areas.Remove m_lngSelectedArea
    RefreshAreas
    Redraw
    EndProcess
End Sub

Public Sub DeleteBrush()
On Error Resume Next
    BeginProcess "Performing Delete..."
    m_brsBrush.Resize 1, 1
    m_brsBrush.Tile(0, 0) = m_mapMap.Layers(m_lngSelectedLayer).Tileset.TransparentTile
    RefreshBrush
    Redraw
    EndProcess
End Sub

Public Sub DeleteCollisionLines()
On Error Resume Next
Dim l_lngLine As Long
    BeginProcess "Performing Delete..."
    BlockingUndoPush
    With m_mapMap.Layers(m_lngSelectedLayer)
        For l_lngLine = UBound(m_bytSelectedCollisionLines) To 1 Step -1
            If m_bytSelectedCollisionLines(l_lngLine) Then
                .RemoveCollisionLine l_lngLine - 1
            End If
        Next l_lngLine
        ReDim m_bytSelectedCollisionLines(0 To .CollisionLineCount)
    End With
    Redraw
    EndProcess
End Sub

Public Sub DeleteLayer()
On Error Resume Next
    BeginProcess "Performing Delete..."
    ObjectUndoPush m_mapMap.Layers, m_mapMap.Layers(m_lngSelectedLayer), m_lngSelectedLayer, OUO_Add
    m_mapMap.Layers.Remove m_lngSelectedLayer
    RefreshAll
    EndProcess
End Sub

Public Sub DeleteLight()
On Error Resume Next
    BeginProcess "Performing Delete..."
    ObjectUndoPush m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights, m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights(m_lngSelectedLight), m_lngSelectedLight, OUO_Add
    m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights.Remove m_lngSelectedLight
    RefreshLights
    Redraw
    EndProcess
End Sub

Public Sub DeleteLightingObstructions()
On Error Resume Next
Dim l_lngLine As Long
    BeginProcess "Performing Delete..."
    With m_mapMap.Layers(m_lngSelectedLayer).Lighting
        For l_lngLine = UBound(m_bytSelectedLightingObstructions) To 1 Step -1
            If m_bytSelectedLightingObstructions(l_lngLine) Then
                .RemoveObstruction l_lngLine
            End If
        Next l_lngLine
        ReDim m_bytSelectedLightingObstructions(0 To .ObstructionCount)
        .Refresh
    End With
    Redraw
    EndProcess
End Sub

Public Sub DeleteLightingPlanes()
On Error Resume Next
Dim l_lngPlane As Long
    BeginProcess "Performing Delete..."
    With m_mapMap.Layers(m_lngSelectedLayer).Lighting
        For l_lngPlane = UBound(m_bytSelectedLightingPlanes) To 1 Step -1
            If m_bytSelectedLightingPlanes(l_lngPlane) Then
                .RemovePlane l_lngPlane
            End If
        Next l_lngPlane
        ReDim m_bytSelectedLightingPlanes(0 To .PlaneCount)
        .Refresh
    End With
    Redraw
    EndProcess
End Sub

Public Sub DeletePathNodes()
On Error Resume Next
Dim l_lngNode As Long
    BeginProcess "Performing Delete..."
    With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite).Path
        For l_lngNode = UBound(m_bytSelectedPathNodes) To 1 Step -1
            If m_bytSelectedPathNodes(l_lngNode) Then
                .Remove l_lngNode
            End If
        Next l_lngNode
        ReDim m_bytSelectedPathNodes(0 To .Count)
    End With
    Redraw
    EndProcess
End Sub

Public Sub DeleteSprite()
On Error Resume Next
    BeginProcess "Performing Delete..."
    ObjectUndoPush m_mapMap.Layers(m_lngSelectedLayer).Sprites, m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite), m_lngSelectedSprite, OUO_Add
    m_mapMap.Layers(m_lngSelectedLayer).Sprites.Remove m_lngSelectedSprite
    RefreshSprites
    Redraw
    EndProcess
End Sub

Public Sub Draw_Line(ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngXMin As Long, l_lngYMin As Long
Dim l_lngXMax As Long, l_lngYMax As Long
Dim l_lngDelta(0 To 1) As Long, l_lngD As Long
Dim l_lngDInc(0 To 1) As Long
Dim l_lngXInc(0 To 1) As Long
Dim l_lngYInc(0 To 1) As Long
Dim l_booAlternate As Boolean, l_lngTemp As Long
Dim l_lngPixels As Long
    BeginProcess "Drawing Line..."
    l_lngXMin = IIf(X1 < X2, X1, X2): l_lngYMin = IIf(Y1 < Y2, Y1, Y2)
    l_lngXMax = IIf(X1 > X2, X1, X2): l_lngYMax = IIf(Y1 > Y2, Y1, Y2)
    m_rctActionArea.Accomodate F2Rect(l_lngXMin - m_brsBrush.XCenter, l_lngYMin - m_brsBrush.YCenter, (l_lngXMax - l_lngXMin) + 1 + (m_brsBrush.Width + m_brsBrush.XCenter), (l_lngYMax - l_lngYMin) + 1 + (m_brsBrush.Height + m_brsBrush.YCenter), False)
    l_lngDelta(0) = Abs(X2 - X1): l_lngDelta(1) = Abs(Y2 - Y1)
    l_booAlternate = (l_lngDelta(1) > l_lngDelta(0))
    If l_booAlternate Then
        l_lngTemp = l_lngDelta(0)
        l_lngDelta(0) = l_lngDelta(1)
        l_lngDelta(1) = l_lngTemp
    End If
    l_lngXInc(0) = 1: l_lngXInc(1) = 1
    l_lngYInc(0) = 1: l_lngYInc(1) = 1
    If l_booAlternate Then
        l_lngXInc(0) = 0
    Else
        l_lngYInc(0) = 0
    End If
    If X1 > X2 Then
        l_lngXInc(0) = -l_lngXInc(0)
        l_lngXInc(1) = -l_lngXInc(1)
    End If
    If Y1 > Y2 Then
        l_lngYInc(0) = -l_lngYInc(0)
        l_lngYInc(1) = -l_lngYInc(1)
    End If
    l_lngDInc(0) = l_lngDelta(1) * 2
    l_lngDInc(1) = (l_lngDelta(1) - l_lngDelta(0)) * 2
    l_lngX = X1
    l_lngY = Y1
    l_lngD = (2 * l_lngDelta(1)) - l_lngDelta(0)
    For l_lngPixels = 0 To l_lngDelta(0)
        m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, l_lngX - m_brsBrush.XCenter, l_lngY - m_brsBrush.YCenter, , , , , l_lngX - X1, l_lngY - Y1
        
        l_lngTemp = -CLng(l_lngD >= 0)
        l_lngD = l_lngD + (l_lngDInc(l_lngTemp))
        l_lngX = l_lngX + (l_lngXInc(l_lngTemp))
        l_lngY = l_lngY + (l_lngYInc(l_lngTemp))
        UpdateProcess l_lngPixels / l_lngDelta(0)
    Next l_lngPixels
    EndProcess
End Sub

Public Sub Draw_Rectangle(ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngTemp As Long
    If X1 > X2 Then
        l_lngTemp = X2
        X2 = X1
        X1 = l_lngTemp
    End If
    If Y1 > Y2 Then
        l_lngTemp = Y2
        Y2 = Y1
        Y1 = l_lngTemp
    End If
    m_rctActionArea.Accomodate F2Rect(X1 - m_brsBrush.XCenter, Y1 - m_brsBrush.YCenter, (X2 - X1) + 1 + (m_brsBrush.Width - m_brsBrush.XCenter), (Y2 - Y1) + 1 + (m_brsBrush.Height - m_brsBrush.YCenter), False)
    For l_lngX = X1 To X2 Step m_brsBrush.Width
        m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, l_lngX - m_brsBrush.XCenter, Y1 - m_brsBrush.YCenter
        m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, l_lngX - m_brsBrush.XCenter, Y2 - m_brsBrush.YCenter
    Next l_lngX
    For l_lngY = Y1 To Y2 Step m_brsBrush.Height
        m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, X1 - m_brsBrush.XCenter, l_lngY - m_brsBrush.YCenter
        m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, X2 - m_brsBrush.XCenter, l_lngY - m_brsBrush.YCenter
    Next l_lngY
End Sub

Public Sub Draw_Rectangle_Filled(ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngTemp As Long
    If X1 > X2 Then
        l_lngTemp = X2
        X2 = X1
        X1 = l_lngTemp
    End If
    If Y1 > Y2 Then
        l_lngTemp = Y2
        Y2 = Y1
        Y1 = l_lngTemp
    End If
    m_rctActionArea.Accomodate F2Rect(X1, Y1, (X2 - X1) + 1, (Y2 - Y1) + 1, False)
    For l_lngY = Y1 To Y2 Step m_brsBrush.Height
        For l_lngX = X1 To X2 Step m_brsBrush.Width
            m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, l_lngX, l_lngY, X2 - l_lngX + 1, Y2 - l_lngY + 1
        Next l_lngX
    Next l_lngY
End Sub

Private Sub tmrRefreshTileset_Timer()

End Sub

Private Sub tpkTiles_TilesetModified()
On Error Resume Next
    RefreshBrush
    Redraw
End Sub

Private Sub tsInspector_Resize()
On Error Resume Next
    If m_ctlCurrentInspector Is Nothing Then
    Else
        m_ctlCurrentInspector.Move 2, tsInspector.Top + tsInspector.IdealHeight + 1, tsInspector.Width - 4, tsInspector.Height - tsInspector.IdealHeight - 3
        m_ctlCurrentInspector.ZOrder
    End If
End Sub

Private Sub tsInspector_TabSelected(TheTab As ngTab)
On Error Resume Next
    InspectorChanged
End Sub

Private Sub tsLists_Resize()
On Error Resume Next
    If m_ctlCurrentList Is Nothing Then
    Else
        m_ctlCurrentList.Move 2, tsLists.Top + tsLists.IdealHeight + 1, tsLists.Width - 4, tsLists.Height - tsLists.IdealHeight - 3
        m_ctlCurrentList.ZOrder
    End If
End Sub

Private Sub tsLists_TabSelected(TheTab As ngTab)
On Error Resume Next
    ListChanged
End Sub

Private Sub tsTool_Resize()
On Error Resume Next
    If m_ctlCurrentTool Is Nothing Then
    Else
        m_ctlCurrentTool.Move 2, tsTool.Top + tsTool.IdealHeight + 1, tsTool.Width - 4, tsTool.Height - tsLists.IdealHeight - 3
        m_ctlCurrentTool.ZOrder
    End If
End Sub

Private Sub tsTool_TabSelected(TheTab As ngTab)
On Error Resume Next
    ToolInspectorChanged
End Sub

Private Sub tsViews_Resize()
On Error Resume Next
    picMapView.Move (2) + tsViews.Left, tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - (tsViews.IdealHeight + 3)
    If picMapView.Visible Then picMapView.ZOrder
    insMap.Move picMapView.Left, picMapView.Top, picMapView.Width, picMapView.Height
    If insMap.Visible Then insMap.ZOrder
    scMap.Move picMapView.Left, picMapView.Top, picMapView.Width, picMapView.Height
    If scMap.Visible Then scMap.ZOrder
End Sub

Private Sub tsViews_TabSelected(TheTab As ngTab)
On Error Resume Next
    m_lngCurrentView = CLng(Mid(TheTab.key, 2))
    ViewChanged
End Sub

Public Sub DuplicateLayer(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    BeginProcess "Duplicating Layer..."
    If Index = -1 Then
        Index = m_lngSelectedLayer
    End If
    Set l_lyrLayer = m_mapMap.Layers(Index).Duplicate
    l_lyrLayer.Name = "Copy Of " & l_lyrLayer.Name
    l_lyrLayer.Tileset.Reload
    m_mapMap.Layers.Add l_lyrLayer, Index + 1
    ObjectUndoPush m_mapMap.Layers, l_lyrLayer, Index + 1, OUO_Remove
    m_lngSelectedLayer = Index + 1
    RefreshLayers
    Redraw
    Editor.ToolbarUpdate
    EndProcess
End Sub

Private Function Editor() As Object
On Error Resume Next
Dim l_objPlugin As MapEditor
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Function

Private Sub elAreas_ContextMenu(ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elAreas.SetFocus
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
        frmIcons.ilContextMenus)
    Case 1
        PasteArea
    Case Else
    End Select
End Sub

Private Sub elAreas_ItemContextMenu(ByVal Item As Long, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elAreas.SetFocus
    elAreas.SelectedItem = Item
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
        frmIcons.ilContextMenus)
    Case 1
        CutArea
    Case 2
        CopyArea
    Case 3
        PasteArea Item
    Case 4
        DeleteArea
    Case Else
    End Select
End Sub

Private Sub elAreas_ItemDragComplete(ByVal Item As Long)
On Error Resume Next
    InspectorChanged
    Redraw
End Sub

Private Sub elAreas_ItemSelected(ByVal Item As Long)
On Error Resume Next
    m_lngSelectedArea = ClipValue(Item, 0, m_mapMap.Areas.Count)
    Redraw
    InspectorChanged
End Sub

Private Sub elLayers_ContextMenu(ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elLayers.SetFocus
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
        frmIcons.ilContextMenus)
    Case 1
        PasteLayer
    Case Else
    End Select
End Sub

Private Sub elLayers_ItemContextMenu(ByVal Item As Long, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elLayers.SetFocus
    elLayers.SelectedItem = Item
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("Visible", , , , , m_mapMap.Layers(Item).Visible), _
        MenuString("-"), _
        MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Duplicate", , , "DUPLICATE", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
        frmIcons.ilContextMenus)
    Case 1
        ToggleLayerVisibility Item
    Case 3
        CutLayer
    Case 4
        CopyLayer
    Case 5
        DuplicateLayer Item
    Case 6
        PasteLayer Item
    Case 7
        DeleteLayer
    Case Else
    End Select
End Sub

Private Sub elLayers_ItemDragComplete(ByVal Item As Long)
On Error Resume Next
    InspectorChanged
    Redraw
End Sub

Private Sub elLayers_ItemSelected(ByVal Item As Long)
On Error Resume Next
    m_lngSelectedLayer = ClipValue(Item, 1, m_mapMap.Layers.Count)
    Redraw
    RefreshBrush
    RefreshTiles
    InspectorChanged
End Sub

Private Sub elLayers_ItemVisibilityChanged(ByVal Item As Long)
On Error Resume Next
    PropertyUndoPush elLayers.BoundObject.Item(Item), "Visible", Not elLayers.BoundObject(Item).Visible
    Redraw
    RefreshInspector
End Sub

Private Sub elLayers_ToolbarClick(ByVal Button As ngUI.ngToolButton)
On Error Resume Next
    Select Case LCase(Trim(Button.key))
    Case "ghostlayers", "tintlayers"
        Redraw
    Case "removelayer"
        DeleteLayer
    Case "duplicatelayer"
        DuplicateLayer
    Case "newlayer"
        NewLayer
    Case Else
    End Select
End Sub

Private Sub elLights_ContextMenu(ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elLights.SetFocus
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
        frmIcons.ilContextMenus)
    Case 1
        PasteLight
    Case Else
    End Select
End Sub

Private Sub elLights_ItemContextMenu(ByVal Item As Long, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elLights.SetFocus
    elLights.SelectedItem = Item
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
        frmIcons.ilContextMenus)
    Case 1
        CutLight
    Case 2
        CopyLight
    Case 3
        PasteLight Item
    Case 4
        DeleteLight
    Case Else
    End Select
End Sub

Private Sub elLights_ItemDragComplete(ByVal Item As Long)
On Error Resume Next
    InspectorChanged
    Redraw
End Sub

Private Sub elLights_ItemSelected(ByVal Item As Long)
On Error Resume Next
    m_lngSelectedLight = ClipValue(Item, 0, m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights.Count)
    RefreshLights
    InspectorChanged
    Redraw
End Sub

Private Sub elLights_ItemVisibilityChanged(ByVal Item As Long)
On Error Resume Next
    PropertyUndoPush elLights.BoundObject.Item(Item), "Visible", Not elLights.BoundObject(Item).Visible
    InspectorChanged
    Redraw
End Sub

Private Sub elObjects_ItemDragComplete(ByVal Item As Long)
On Error Resume Next
    InspectorChanged
    Redraw
End Sub

Private Sub elObjects_ItemSelected(ByVal Item As Long)
On Error Resume Next
    m_lngSelectedObject = ClipValue(Item, 0, m_mapMap.Objects.Count)
    InspectorChanged
    Redraw
End Sub

Private Sub elObjects_ItemVisibilityChanged(ByVal Item As Long)
On Error Resume Next
'    PropertyUndoPush elObjects.BoundObject.Item(Item), "Visible", Not elLights.BoundObject(Item).Visible
    InspectorChanged
    Redraw
End Sub

Private Sub elSprites_ContextMenu(ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elSprites.SetFocus
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
        frmIcons.ilContextMenus)
    Case 1
        PasteSprite
    Case Else
    End Select
End Sub

Private Sub elSprites_ItemContextMenu(ByVal Item As Long, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    elSprites.SetFocus
    elSprites.SelectedItem = Item
    Editor.ActionUpdate
    Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("Visible", , , , , m_mapMap.Layers(m_lngSelectedLayer).Sprites(Item).Visible), _
        MenuString("-"), _
        MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
        frmIcons.ilContextMenus)
    Case 1
        With m_mapMap.Layers(m_lngSelectedLayer).Sprites(Item)
            .Visible = Not .Visible
            RefreshSprites
            Redraw
        End With
    Case 3
        CutSprite
    Case 4
        CopySprite
    Case 5
        PasteSprite Item
    Case 6
        DeleteSprite
    Case Else
    End Select
End Sub

Private Sub elSprites_ItemDragComplete(ByVal Item As Long)
On Error Resume Next
    InspectorChanged
    Redraw
End Sub

Private Sub elSprites_ItemSelected(ByVal Item As Long)
On Error Resume Next
    m_lngSelectedSprite = ClipValue(Item, 0, m_mapMap.Layers(m_lngSelectedLayer).Sprites.Count)
    InspectorChanged
    Redraw
End Sub

Private Sub elSprites_ItemVisibilityChanged(ByVal Item As Long)
On Error Resume Next
    PropertyUndoPush elSprites.BoundObject.Item(Item), "Visible", Not elSprites.BoundObject(Item).Visible
    Redraw
End Sub

Private Sub FixRectCoords(ByRef X1 As Long, ByRef Y1 As Long, ByRef X2 As Long, ByRef Y2 As Long)
On Error Resume Next
Dim Temp As Long
    If X1 > X2 Then
        Temp = X1
        X1 = X2
        X2 = Temp
    End If
    If Y1 > Y2 Then
        Temp = Y1
        Y1 = Y2
        Y2 = Temp
    End If
End Sub

Public Sub Form_Activate()
On Error Resume Next
    Set insInspect.Editor = Editor
    Set insMap.Editor = Editor
    Set insTool.Editor = Editor
    picMapViewport.AutoRedraw = True
    picOverlay.AutoRedraw = True
    Form_Resize
    Redraw
    RefreshOverlay
    Editor.SetLocation ""
End Sub

Private Sub Form_Deactivate()
On Error Resume Next
    picHarmless.SetFocus
    picMapViewport.AutoRedraw = False
    picOverlay.AutoRedraw = False
    Editor.SetLocation
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
End Sub

Private Sub Form_Load()
On Error Resume Next
    elLayers.ShowVisibilityToggles = True
    elSprites.ShowVisibilityToggles = True
    elLights.ShowVisibilityToggles = True
    elObjects.ShowVisibilityToggles = True
    insTool.ShowInfobox = False
    insTool.ShowHierarchy = False
    InitViews
    InitSplitters
    hsMap.Height = GetScrollbarSize(hsMap) + 1
    vsMap.Width = GetScrollbarSize(vsMap) + 1
    m_lngSelectedLayer = 1
    Set m_brsBrush = New Fury2Brush
    m_brsBrush.Resize 1, 1
    m_brsBrush.Fill -1
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    picHarmless.SetFocus
    Editor.SaveSettings "MapEditor\View", m_voViewOptions
    Cleanup
End Sub

Private Sub Form_Resize()
On Error Resume Next
    If WindowState = 2 And m_booVisible = False Then
        m_booVisible = True
        RefreshAll
    End If
    picContainer.Move 2, 2, Me.ScaleWidth - 4, Me.ScaleHeight - 4
    ResizeAll
End Sub

Public Sub HideOverlay()
On Error Resume Next
    picOverlay.Visible = False
End Sub

Private Sub hsMap_Change()
On Error Resume Next
    If hsMap.Tag = "" Then Redraw
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DestroyMenu "ResizeMap"
        .DestroyMenu "Zoom"
        .DestroyMenu "SetZoom(25)"
        .DestroyMenu "SetZoom(50)"
        .DestroyMenu "SetZoom(100)"
        .DestroyMenu "SetZoom(200)"
        .DestroyMenu "SetZoom(400)"
        .DestroyMenu "SetZoom(800)"
        .DestroyMenu "SetZoom(1600)"
        .DestroyMenu "ZoomEndSeparator"
        .DestroyMenu "Tools"
        .DestroyMenu "BlockingTools"
        .DestroyMenu "ToolsEndSeparator"
        .DestroyMenu "AddEdgeBlocking"
        .DestroyMenu "BlockingToolsEndSeparator"
        .DestroyMenu "RunMacro"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DefineMenu "&Resize...", "ResizeMap", , , ContextMenuIcon("RESIZE MAP")
        .DefineMenu "&Zoom", "Zoom", , , ContextMenuIcon("ZOOM")
        .DefineMenu "25%", "SetZoom(25)", "Zoom", , , , Zoom <= 0.25
        .DefineMenu "50%", "SetZoom(50)", "Zoom", , , , Zoom = 0.5
        .DefineMenu "100%", "SetZoom(100)", "Zoom", , , , Zoom = 1
        .DefineMenu "200%", "SetZoom(200)", "Zoom", , , , Zoom = 2
        .DefineMenu "400%", "SetZoom(400)", "Zoom", , , , Zoom = 4
        .DefineMenu "800%", "SetZoom(800)", "Zoom", , , , Zoom = 8
        .DefineMenu "1600%", "SetZoom(1600)", "Zoom", , , , Zoom >= 16
        .DefineMenu "-", "ZoomEndSeparator", "Zoom"
        .DefineMenu "&Tools", "Tools"
        .DefineMenu "&Tiles", "TileTools", "Tools"
        .DefineMenu "&Blocking", "BlockingTools", "Tools"
        .DefineMenu "-", "ToolsEndSeparator", "Tools"
        .DefineMenu "Reload all tilesets", "ReloadTilesets", "TileTools"
        .DefineMenu "-", "TileToolsEndSeparator", "TileTools"
        .DefineMenu "Place blocking around edges", "AddEdgeBlocking", "BlockingTools"
        .DefineMenu "-", "BlockingToolsEndSeparator", "BlockingTools"
        .DefineMenu "Run &Macro...", "RunMacro"
    End With
End Sub

Private Sub iCustomMenus_MenuClick(key As String)
On Error Resume Next
Dim l_strParameter As String, l_lngParameter As Long
Dim CommandName As String
    CommandName = key
    If InStr(CommandName, "(") Then
        l_strParameter = Trim(Mid(CommandName, InStr(CommandName, "(") + 1))
        If Right(l_strParameter, 1) = ")" Then l_strParameter = Trim(Left(l_strParameter, Len(l_strParameter) - 1))
        CommandName = Left(CommandName, InStr(CommandName, "(") - 1)
        CommandName = Replace(CommandName, ":", "_")
        If Left(l_strParameter, 1) = """" Then
            ' String
            l_strParameter = CStr(Mid(l_strParameter, 2, Len(l_strParameter) - 2))
            CallByName Me, CommandName, VbMethod, l_strParameter
        Else
            ' Integer
            l_lngParameter = CLng(l_strParameter)
            CallByName Me, CommandName, VbMethod, l_lngParameter
        End If
    Else
        CommandName = Replace(CommandName, ":", "_")
        CallByName Me, CommandName, VbMethod
    End If
    Err.Clear
End Sub

Private Property Get iDocument_CanSave() As Boolean
On Error Resume Next
    iDocument_CanSave = True
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = m_strFilename
End Property

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = m_fpgPlugin
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
    Set m_fpgPlugin = RHS
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
Dim l_vfFile As VirtualFile
    Err.Clear
    Set l_vfFile = F2File()
    SaveToFile m_mapMap, l_vfFile
    l_vfFile.SaveFile Filename
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
    Redraw
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Map"
End Property

Private Sub iEditingCommands_CanCopy(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        NewValue = scMap.Control.CanCopy
    Case "Object Script"
        NewValue = scObject.Control.CanCopy
    Case "Layers"
        NewValue = (elLayers.SelectedItem > 0)
    Case "Areas"
        NewValue = (elAreas.SelectedItem > 0)
    Case "Sprites"
        NewValue = (elSprites.SelectedItem > 0)
    Case "Lights"
        NewValue = (elLights.SelectedItem > 0)
    Case "Brush"
        NewValue = True
    Case "Path Nodes"
        NewValue = True
    Case "Blocking"
        NewValue = True
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanCut(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        NewValue = scMap.Control.CanCut
    Case "Object Script"
        NewValue = scObject.Control.CanCut
    Case "Layers"
        NewValue = (elLayers.SelectedItem > 0) And (m_mapMap.Layers.Count > 1)
    Case "Areas"
        NewValue = (elAreas.SelectedItem > 0)
    Case "Sprites"
        NewValue = (elSprites.SelectedItem > 0)
    Case "Lights"
        NewValue = (elLights.SelectedItem > 0)
    Case "Brush"
        NewValue = True
    Case "Path Nodes"
        NewValue = True
    Case "Blocking"
        NewValue = True
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanDelete(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        NewValue = scMap.Control.CanCut
    Case "Object Script"
        NewValue = scObject.Control.CanCut
    Case "Layers"
        NewValue = (elLayers.SelectedItem > 0) And (m_mapMap.Layers.Count > 1)
    Case "Areas"
        NewValue = (elAreas.SelectedItem > 0)
    Case "Sprites"
        NewValue = (elSprites.SelectedItem > 0)
    Case "Brush"
        NewValue = True
    Case "Path Nodes"
        NewValue = True
    Case "Blocking"
        NewValue = True
    Case "Lighting Obstructions"
        NewValue = True
    Case "Lighting Planes"
        NewValue = True
    Case "Lights"
        NewValue = (elLights.SelectedItem > 0)
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanPaste(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        NewValue = scMap.Control.CanPaste
    Case "Object Script"
        NewValue = scObject.Control.CanPaste
    Case "Layers"
        NewValue = ClipboardContainsFormat(CF_MapLayer)
    Case "Areas"
        NewValue = ClipboardContainsFormat(CF_Area)
    Case "Sprites"
        NewValue = ClipboardContainsFormat(CF_Sprite)
    Case "Path Nodes"
        NewValue = ClipboardContainsFormat(CF_PathNodes)
    Case "Blocking"
        NewValue = ClipboardContainsFormat(CF_MapObstructions)
    Case "Lights"
        NewValue = ClipboardContainsFormat(CF_LightSource)
    Case "Brush"
        NewValue = ClipboardContainsFormat(CF_Brush)
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanRedo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        NewValue = scMap.Control.CanRedo
    Case "Object Script"
        NewValue = scObject.Control.CanRedo
    Case Else
        NewValue = (m_colRedo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_CanSelectAll(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Blocking"
        NewValue = True
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanSelectNone(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Blocking"
        NewValue = True
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanUndo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        NewValue = scMap.Control.CanUndo
    Case "Object Script"
        NewValue = scObject.Control.CanUndo
    Case Else
        NewValue = (m_colUndo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_Copy()
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        scMap.Control.Copy
    Case "Object Script"
        scObject.Control.Copy
    Case "Layers"
        CopyLayer
    Case "Areas"
        CopyArea
    Case "Sprites"
        CopySprite
    Case "Lights"
        CopyLight
    Case "Blocking"
        CopyCollisionLines
    Case "Path Nodes"
        CopyPathNodes
    Case "Brush"
        CopyBrush
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Cut()
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        scMap.Control.Cut
    Case "Object Script"
        scObject.Control.Cut
    Case "Layers"
        CutLayer
    Case "Areas"
        CutArea
    Case "Sprites"
        CutSprite
    Case "Sprites"
        CutLight
    Case "Blocking"
        CutCollisionLines
    Case "Path Nodes"
        CutPathNodes
    Case "Brush"
        CutBrush
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Delete()
On Error Resume Next
    Select Case ActiveType
    Case "Layers"
        DeleteLayer
    Case "Areas"
        DeleteArea
    Case "Sprites"
        DeleteSprite
    Case "Blocking"
        DeleteCollisionLines
    Case "Path Nodes"
        DeletePathNodes
    Case "Lighting Obstructions"
        DeleteLightingObstructions
    Case "Lighting Planes"
        DeleteLightingPlanes
    Case "Lights"
        DeleteLight
    Case "Brush"
        DeleteBrush
    Case "Script"
        scMap.Control.ExecuteCmd cmCmdDelete
    Case "Object Script"
        scObject.Control.ExecuteCmd cmCmdDelete
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Paste()
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        scMap.Control.Paste
    Case "Object Script"
        scObject.Control.Paste
    Case "Layers"
        PasteLayer
    Case "Areas"
        PasteArea
    Case "Sprites"
        PasteSprite
    Case "Lights"
        PasteLight
    Case "Blocking"
        PasteCollisionLines
    Case "Path Nodes"
        PastePathNodes
    Case "Brush"
        PasteBrush
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Redo()
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        scMap.Control.Redo
    Case "Object Script"
        scObject.Control.Redo
    Case Else
        Me.Redo
    End Select
    Redraw
End Sub

Private Sub iEditingCommands_SelectAll()
On Error Resume Next
    Select Case ActiveType
    Case "Blocking"
        SelectCollisionLines 1, m_mapMap.Layers(m_lngSelectedLayer).CollisionLineCount
        Redraw
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_SelectNone()
On Error Resume Next
    Select Case ActiveType
    Case "Blocking"
        SelectCollisionLines
        Redraw
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Undo()
On Error Resume Next
    Select Case ActiveType
    Case "Script"
        scMap.Control.Undo
    Case "Object Script"
        scObject.Control.Undo
    Case Else
        Me.Undo
    End Select
    Redraw
End Sub

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Public Sub InitSplitters()
On Error Resume Next
    With m_splSidebar
        .Orientation = cSPLTOrientationVertical
        .Bind tsViews, picSidebar
        .Orientation = cSPLTOrientationVertical
        .MinimumSize(cSPLTRightOrBottomPanel) = 125
        .MaximumSize(cSPLTRightOrBottomPanel) = 450
        .KeepProportion = True
        .Position = picContainer.ScaleWidth - 150
    End With
    With m_splSidebarPanels
        .Orientation = cSPLTOrientationHorizontal
        .Bind tsLists, picInspectors
        .Orientation = cSPLTOrientationHorizontal
        .MinimumSize(cSPLTLeftOrTopPanel) = 75
        .MaximumSize(cSPLTLeftOrTopPanel) = 400
        .KeepProportion = True
        .Position = 75
    End With
    With m_splSidebarInspectors
        .Orientation = cSPLTOrientationHorizontal
        .Bind tsInspector, tsTool
        .Orientation = cSPLTOrientationHorizontal
        .MinimumSize(cSPLTRightOrBottomPanel) = 75
        .MaximumSize(cSPLTRightOrBottomPanel) = 500
        .KeepProportion = True
        .Position = 100000
    End With
End Sub

Public Sub InitViews()
On Error Resume Next
    tsViews.Tabs.AddNew "Tiles", "t" & CStr(View_Tiles)
    tsViews.Tabs.AddNew "Blocking", "t" & CStr(View_Blocking)
    tsViews.Tabs.AddNew "Lighting", "t" & CStr(View_Lighting)
    tsViews.Tabs.AddNew "Areas", "t" & CStr(View_Areas)
    tsViews.Tabs.AddNew "Sprites", "t" & CStr(View_Sprites)
    tsViews.Tabs.AddNew "Objects", "t" & CStr(View_Objects)
    tsViews.Tabs.AddNew "Script", "t" & CStr(View_Script)
    tsViews.Tabs.AddNew "Properties", "t" & CStr(View_Properties)
End Sub

Private Sub insInspect_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    PropertyUndoPush insInspect.CurrentObject, insInspect.ItemName(insInspect.SelectedItem), OldValue
    RefreshList
    Redraw
End Sub

Private Sub insInspect_EllipsisPressed(ByVal Index As Long)
On Error Resume Next
Dim l_objObject As Object
Dim l_strItemName As String
    Set l_objObject = insInspect.CurrentObject
    l_strItemName = LCase(Trim(insInspect.ItemName(insInspect.SelectedItem)))
    If TypeOf l_objObject Is Fury2Tileset Then
    ElseIf TypeOf l_objObject Is Fury2MapLayer Then
        Select Case l_strItemName
        Case "sprites"
            tsViews.SelectTab tsViews.Tabs(View_Sprites + 1)
            tsLists.SelectTab tsLists.Tabs(2)
        Case Else
        End Select
    End If
End Sub

Public Sub InspectorChanged()
On Error Resume Next
Dim l_objInspect As Object
Dim l_sndObject As Fury2SoundObject
    m_ctlCurrentInspector.Visible = False
    If (tsInspector.Tabs.Count = 0) Then
        Set m_ctlCurrentInspector = Nothing
    Else
        Select Case LCase(Trim(tsInspector.SelectedTab.key))
        Case "tileset"
            Set m_ctlCurrentInspector = tpkTiles
        Case "layer"
            Set m_ctlCurrentInspector = insInspect
            With insInspect
                .ClearStack
                If m_lngSelectedLayer Then
                    .AddToStack m_mapMap, "Map"
                    .Inspect m_mapMap.Layers(m_lngSelectedLayer), "Layer " & m_lngSelectedLayer, False
                Else
                    .Inspect Nothing
                End If
            End With
        Case "area"
            Set m_ctlCurrentInspector = insInspect
            With insInspect
                .ClearStack
                If m_lngSelectedArea Then
                    .AddToStack m_mapMap, "Map"
                    .Inspect m_mapMap.Areas(m_lngSelectedArea), "Area " & m_lngSelectedArea, False
                Else
                    .Inspect Nothing
                End If
            End With
        Case "sprite"
            Set m_ctlCurrentInspector = insInspect
            With insInspect
                .ClearStack
                If (m_lngSelectedLayer > 0) And (m_lngSelectedSprite > 0) Then
                    .AddToStack m_mapMap, "Map"
                    .AddToStack m_mapMap.Layers(m_lngSelectedLayer), "Layer " & m_lngSelectedLayer
                    .Inspect m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite), "Sprite " & m_lngSelectedSprite, False
                Else
                    .Inspect Nothing
                End If
            End With
        Case "path"
            Set m_ctlCurrentInspector = insInspect
            With insInspect
                .ClearStack
                If (m_lngSelectedLayer > 0) And (m_lngSelectedSprite > 0) Then
                    .AddToStack m_mapMap, "Map"
                    .AddToStack m_mapMap.Layers(m_lngSelectedLayer), "Layer " & m_lngSelectedLayer
                    .AddToStack m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite), "Sprite " & m_lngSelectedSprite
                    .Inspect m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite).Path, "Path", False
                Else
                    .Inspect Nothing
                End If
            End With
        Case "script"
            Set m_ctlCurrentInspector = scObject
            If m_lngCurrentView = View_Sprites Then
                scObject.Text = m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite).ScriptSource
            ElseIf m_lngCurrentView = View_Areas Then
                scObject.Text = m_mapMap.Areas(m_lngSelectedArea).ScriptSource
            End If
        Case "light"
            Set m_ctlCurrentInspector = insInspect
            With insInspect
                .ClearStack
                If (m_lngSelectedLayer > 0) And (m_lngSelectedLight > 0) Then
                    .AddToStack m_mapMap, "Map"
                    .AddToStack m_mapMap.Layers(m_lngSelectedLayer), "Layer " & m_lngSelectedLayer
                    .AddToStack m_mapMap.Layers(m_lngSelectedLayer).Lighting, "Lighting"
                    .Inspect m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights(m_lngSelectedLight), "Light " & m_lngSelectedLight, False
                ElseIf (m_lngSelectedLayer > 0) Then
                    .AddToStack m_mapMap, "Map"
                    .AddToStack m_mapMap.Layers(m_lngSelectedLayer), "Layer " & m_lngSelectedLayer
                    .Inspect m_mapMap.Layers(m_lngSelectedLayer).Lighting, "Lighting", False
                Else
                    .Inspect Nothing
                End If
            End With
        Case "sound"
            Set m_ctlCurrentInspector = insInspect
            With insInspect
                .ClearStack
                If (m_lngSelectedObject > 0) Then
                    .AddToStack m_mapMap, "Map"
                    Set l_sndObject = m_mapMap.Objects(m_lngSelectedObject)
                    .Inspect l_sndObject, "Object " & m_lngSelectedObject, False
                Else
                    .Inspect Nothing
                End If
            End With
        Case Else
            Set m_ctlCurrentInspector = Nothing
        End Select
    End If
    tsInspector_Resize
    m_ctlCurrentInspector.Visible = True
End Sub

Private Sub insTool_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    RefreshTool
End Sub

Public Sub Lighting_ToolChanged()
On Error Resume Next
    m_booDraggingLight = False
End Sub

Public Function LightSourceFromPoint(ByVal X As Long, ByVal Y As Long, Optional ByVal Layer As Long = -1) As Fury2LightSource
On Error Resume Next
Dim l_lsLight As Fury2LightSource
Dim l_colList As New Collection
Dim l_sngXDistance As Single, l_sngYDistance As Single, l_sngDistance As Single
Dim l_sngSmallestDistance As Single, l_lsSmallest As Fury2LightSource
    For Each l_lsLight In m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights
        l_sngDistance = Sqr((Abs(l_lsLight.X - X) ^ 2) + (Abs(l_lsLight.Y - Y) ^ 2))
        If (l_sngDistance) <= ClipValue(l_lsLight.FalloffDistance, 10, 10000) Then
            l_colList.Add l_lsLight
        End If
    Next l_lsLight
    If l_colList.Count = 1 Then
        Set LightSourceFromPoint = l_colList(1)
    ElseIf l_colList.Count > 1 Then
        l_sngSmallestDistance = 99999999
        For Each l_lsLight In l_colList
            l_sngDistance = Sqr((Abs(l_lsLight.X - X) ^ 2) + (Abs(l_lsLight.Y - Y) ^ 2))
            If l_sngDistance <= l_sngSmallestDistance Then
                l_sngSmallestDistance = l_sngDistance
                Set l_lsSmallest = l_lsLight
            End If
        Next l_lsLight
        Set LightSourceFromPoint = l_lsSmallest
    End If
End Function

Public Sub ListChanged()
On Error Resume Next
Dim l_strSelectedTab As String
    m_ctlCurrentList.Visible = False
    tsInspector.DisableUpdates = True
    tsInspector.Tabs.Clear
    l_strSelectedTab = tsLists.SelectedTab.key
    Select Case LCase(Trim(tsLists.SelectedTab.key))
    Case "layers"
        Set m_ctlCurrentList = elLayers
        RefreshLayers
        With tsInspector.Tabs
            If (m_lngCurrentView = View_Tiles) Then
                .AddNew "Tileset", "Tileset"
            End If
            .AddNew "Layer", "Layer"
        End With
    Case "sprites"
        Set m_ctlCurrentList = elSprites
        RefreshSprites
        With tsInspector.Tabs
            .AddNew "Sprite", "Sprite"
            .AddNew "Script", "Script"
            .AddNew "Path", "Path"
        End With
    Case "lights"
        Set m_ctlCurrentList = elLights
        RefreshLights
        With tsInspector.Tabs
            .AddNew "Light", "Light"
        End With
    Case "areas"
        Set m_ctlCurrentList = elAreas
        RefreshAreas
        With tsInspector.Tabs
            .AddNew "Area", "Area"
            .AddNew "Script", "Script"
        End With
    Case "objects"
        Set m_ctlCurrentList = elObjects
        With tsInspector.Tabs
        End With
    Case Else
        Set m_ctlCurrentList = Nothing
    End Select
    tsInspector.DisableUpdates = False
    tsInspector.Reflow
    InspectorChanged
    m_ctlCurrentList.Visible = True
    tsLists_Resize
End Sub

Public Function MapObjectFromPoint(ByVal X As Long, ByVal Y As Long) As Fury2MapObject
On Error Resume Next
Dim l_mobObject As Fury2MapObject
Dim l_sndObject As Fury2SoundObject
Dim l_colList As New Collection
Dim l_sngXDistance As Single, l_sngYDistance As Single, l_sngDistance As Single
Dim l_sngSize As Single
Dim l_sngX As Single, l_sngY As Single
Dim l_sngSmallestDistance As Single, l_mobSmallest As Fury2MapObject
    For Each l_mobObject In m_mapMap.Objects
        If TypeOf l_mobObject Is Fury2SoundObject Then
            Set l_sndObject = l_mobObject
            l_sngSize = l_sndObject.FalloffDistance + l_sndObject.FalloffOffset
            l_sngX = l_sndObject.X
            l_sngY = l_sndObject.Y
        Else
            l_sngSize = 0
            l_sngX = 0
            l_sngY = 0
        End If
        l_sngDistance = Sqr((Abs(l_sngX - X) ^ 2) + (Abs(l_sngY - Y) ^ 2))
        If (l_sngDistance) <= l_sngSize Then
            l_colList.Add l_mobObject
        End If
    Next l_mobObject
    If l_colList.Count = 1 Then
        Set MapObjectFromPoint = l_colList(1)
    ElseIf l_colList.Count > 1 Then
        l_sngSmallestDistance = 99999999
        For Each l_mobObject In l_colList
            If TypeOf l_mobObject Is Fury2SoundObject Then
                Set l_sndObject = l_mobObject
                l_sngX = l_sndObject.X
                l_sngY = l_sndObject.Y
            Else
                l_sngX = 0
                l_sngY = 0
            End If
            l_sngDistance = Sqr((Abs(l_sngX - X) ^ 2) + (Abs(l_sngY - Y) ^ 2))
            If l_sngDistance <= l_sngSmallestDistance Then
                l_sngSmallestDistance = l_sngDistance
                Set l_mobSmallest = l_mobObject
            End If
        Next l_mobObject
        Set MapObjectFromPoint = l_mobSmallest
    End If
End Function

Friend Sub SetVisibilityIcons(ByRef List As EntityList, ByRef Name As String)
On Error Resume Next
    With Editor.LoadResources("ng")
        Set List.VisibleIcon = .ItemData("map editor\icons\" & Name & "\visible.png")
        Set List.InvisibleIcon = .ItemData("map editor\icons\" & Name & "\invisible.png")
    End With
End Sub

Public Sub MoveOverlay(ByVal X As Long, ByVal Y As Long, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1)
On Error Resume Next
'Static m_lngOldBitmap As Long
    m_booOverlayLocked = True
    m_lngOverlayX = X
    m_lngOverlayY = Y
    If Width = -1 Then Else m_lngOverlayWidth = Width
    If Height = -1 Then Else m_lngOverlayHeight = Height
    If m_imgOverlay Is Nothing Then
        Set m_imgOverlay = F2DIBSection(m_lngOverlayWidth, m_lngOverlayHeight, Me.hdc)
        m_lngOverlayDC = CreateCompatibleDC(Me.hdc)
        SelectObject m_lngOverlayDC, m_imgOverlay.DIBHandle
    ElseIf (m_imgOverlay.Width <> m_lngOverlayWidth) Or (m_imgOverlay.Height <> m_lngOverlayHeight) Then
        m_imgOverlay.Deallocate
        m_imgOverlay.AllocateDIBSection m_lngOverlayWidth, m_lngOverlayHeight, Me.hdc
        Call SelectObject(m_lngOverlayDC, m_imgOverlay.DIBHandle)
    End If
    RefreshOverlay
    If picOverlay.Visible Then picMapViewport_Paint
    m_booOverlayLocked = False
End Sub

Public Sub MultiPropertyUndoPush(ByRef Obj As Object, ByRef Methods As Variant, ByRef Values As Variant)
On Error Resume Next
Dim l_undUndo As cMultiUndoEntry
Dim l_undProp As cPropertyUndoEntry
Dim l_lngIndex As Long
    If Obj Is Nothing Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cMultiUndoEntry
    With l_undUndo.Entries
        For l_lngIndex = LBound(Methods) To UBound(Methods)
            Set l_undProp = New cPropertyUndoEntry
            With l_undProp
                Set .Object = Obj
                .MethodName = CStr(Methods(l_lngIndex))
                If (VarType(Values(l_lngIndex)) And vbObject) = vbObject Then
                    Set .Value = Values(l_lngIndex)
                Else
                    .Value = Values(l_lngIndex)
                End If
            End With
            .Add l_undProp
        Next l_lngIndex
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub NewLayer()
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    BeginProcess "Creating Layer..."
    Set l_lyrLayer = m_mapMap.Layers(1).Duplicate
    l_lyrLayer.Name = "Layer " & m_mapMap.Layers.Count + 1
    l_lyrLayer.Clear l_lyrLayer.Tileset.TransparentTile
    l_lyrLayer.Tileset.Reload
    m_mapMap.Layers.Add l_lyrLayer
    ObjectUndoPush m_mapMap.Layers, l_lyrLayer, m_mapMap.Layers.Count, OUO_Remove
    m_lngSelectedLayer = m_mapMap.Layers.Count
    RefreshLayers
    Redraw
    Editor.ToolbarUpdate
    EndProcess
End Sub

Public Sub ObjectUndoPush(ByRef Container As Object, ByRef Value As Object, ByVal Index As Long, ByVal Operation As ObjectUndoOperations)
On Error Resume Next
Dim l_undUndo As cObjectUndoEntry
    If Container Is Nothing Then Exit Sub
    If Value Is Nothing Then Exit Sub
    If Index < 1 Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cObjectUndoEntry
    With l_undUndo
        Set .Container = Container
        Set .Value = Value
        .Index = Index
        .Operation = Operation
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Function PasteArea(Optional ByVal AtIndex As Long = -1, Optional ByVal DoRedraw As Boolean = True) As Fury2Area
On Error Resume Next
Dim l_araArea As Fury2Area
    BeginProcess "Performing Paste..."
    If AtIndex < 1 Then
        AtIndex = m_mapMap.Areas.Count + 1
    End If
    Set l_araArea = New Fury2Area
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_Area), l_araArea) Then
        CustomClipboard.ClipboardClose
        ObjectUndoPush m_mapMap.Areas, l_araArea, AtIndex, OUO_Remove
        m_mapMap.Areas.Add l_araArea, , AtIndex
        m_lngSelectedArea = AtIndex
        If DoRedraw Then
            RefreshAreas
            Redraw
        End If
        Editor.ToolbarUpdate
        Set PasteArea = l_araArea
    Else
        CustomClipboard.ClipboardClose
    End If
    EndProcess
End Function

Public Sub PasteBrush()
On Error Resume Next
    BeginProcess "Performing Paste..."
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_Brush), m_brsBrush) Then
        CustomClipboard.ClipboardClose
        RefreshBrush
    Else
        CustomClipboard.ClipboardClose
    End If
    EndProcess
End Sub

Public Sub PasteCollisionLines()
On Error Resume Next
Dim l_lngLine As Long
Dim l_lngCount As Long
Dim l_lnLines() As FLine
Dim l_vfLines As VirtualFile
    BeginProcess "Performing Paste..."
    ReDim l_lnLines(0 To 0)
    Set l_vfLines = New VirtualFile
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_MapObstructions), l_vfLines) Then
        CustomClipboard.ClipboardClose
        l_vfLines.Load l_lngCount
        ReDim l_lnLines(0 To l_lngCount - 1)
        l_vfLines.RawLoad ByVal VarPtr(l_lnLines(0)), l_lngCount * Len(l_lnLines(0))
        BlockingUndoPush
        With m_mapMap.Layers(m_lngSelectedLayer)
            For l_lngLine = LBound(l_lnLines) To UBound(l_lnLines)
                .AddCollisionLine l_lnLines(l_lngLine).Start.X, l_lnLines(l_lngLine).Start.Y, l_lnLines(l_lngLine).end.X, l_lnLines(l_lngLine).end.Y
            Next l_lngLine
            ReDim Preserve m_bytSelectedCollisionLines(0 To .CollisionLineCount)
            SelectCollisionLines .CollisionLineCount - l_lngCount + 1, .CollisionLineCount
        End With
        Redraw
        Editor.ToolbarUpdate
    Else
        CustomClipboard.ClipboardClose
    End If
    EndProcess
End Sub

Public Function PasteLayer(Optional ByVal AtIndex As Long = -1, Optional ByVal DoRedraw As Boolean = True) As Fury2MapLayer
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    BeginProcess "Performing Paste..."
    If AtIndex < 1 Then
        AtIndex = m_mapMap.Layers.Count + 1
    End If
    Set l_lyrLayer = m_mapMap.CreateLayer()
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_MapLayer), l_lyrLayer) Then
        CustomClipboard.ClipboardClose
        ObjectUndoPush m_mapMap.Layers, l_lyrLayer, AtIndex, OUO_Remove
        m_mapMap.Layers.Add l_lyrLayer, AtIndex
        l_lyrLayer.Tileset.Reload
        m_lngSelectedLayer = AtIndex
        If DoRedraw Then
            RefreshLayers
            Redraw
        End If
        Editor.ToolbarUpdate
        Set PasteLayer = l_lyrLayer
    Else
        CustomClipboard.ClipboardClose
    End If
    EndProcess
End Function

Public Function PasteLight(Optional ByVal AtIndex As Long = -1, Optional ByVal DoRedraw As Boolean = True) As Fury2LightSource
On Error Resume Next
Dim l_litLight As Fury2LightSource
    With m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights
        BeginProcess "Performing Paste..."
        If AtIndex < 1 Then
            AtIndex = .Count + 1
        End If
        Set l_litLight = New Fury2LightSource
        CustomClipboard.ClipboardOpen Me.hwnd
        If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_LightSource), l_litLight) Then
            CustomClipboard.ClipboardClose
            ObjectUndoPush m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights, l_litLight, AtIndex, OUO_Remove
            .Add l_litLight, , AtIndex
            m_lngSelectedLight = AtIndex
            If DoRedraw Then
                RefreshLights
                Redraw
            End If
            Editor.ToolbarUpdate
            Set PasteLight = l_litLight
        Else
            CustomClipboard.ClipboardClose
        End If
        EndProcess
    End With
End Function

Public Sub PastePathNodes()
On Error Resume Next
Dim l_lngNode As Long
Dim l_lngCount As Long
Dim l_vfNodes As VirtualFile
Dim l_wpNode As Fury2Waypoint
    BeginProcess "Performing Paste..."
    Set l_vfNodes = New VirtualFile
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_PathNodes), l_vfNodes) Then
        CustomClipboard.ClipboardClose
        l_vfNodes.Load l_lngCount
        With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
            For l_lngNode = 1 To l_lngCount
                Set l_wpNode = New Fury2Waypoint
                l_vfNodes.Load l_wpNode
                .Path.AddObject l_wpNode
            Next l_lngNode
            ReDim Preserve m_bytSelectedPathNodes(0 To .Path.Count)
            SelectPathNodes .Path.Count - l_lngCount, .Path.Count
        End With
        Redraw
        Editor.ToolbarUpdate
    Else
        CustomClipboard.ClipboardClose
    End If
    EndProcess
End Sub

Public Function PasteSprite(Optional ByVal AtIndex As Long = -1, Optional ByVal DoRedraw As Boolean = True) As Fury2Sprite
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_ptMouse As PointAPI
    With m_mapMap.Layers(m_lngSelectedLayer).Sprites
        BeginProcess "Performing Paste..."
        If AtIndex < 1 Then
            AtIndex = .Count + 1
        End If
        Set l_sprSprite = m_mapMap.Layers(m_lngSelectedLayer).Sprites.CreateSprite()
        CustomClipboard.ClipboardOpen Me.hwnd
        If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_Sprite), l_sprSprite) Then
            CustomClipboard.ClipboardClose
            l_sprSprite.Initialize
            l_sprSprite.Load
            ObjectUndoPush m_mapMap.Layers(m_lngSelectedLayer).Sprites, l_sprSprite, AtIndex, OUO_Remove
            GetCursorPos l_ptMouse
            ScreenToClient picMapViewport.hwnd, l_ptMouse
            If (l_ptMouse.X >= 0) And (l_ptMouse.Y >= 0) And (l_ptMouse.X < picMapViewport.ScaleWidth) And (l_ptMouse.Y < picMapViewport.ScaleHeight) Then
                With l_sprSprite
                    .X = m_lngMouseX
                    .Y = m_lngMouseY
                End With
            End If
            .Add l_sprSprite, , AtIndex
            m_lngSelectedSprite = AtIndex
            If DoRedraw Then
                RefreshSprites
                Redraw
            End If
            Editor.ToolbarUpdate
            Set PasteSprite = l_sprSprite
        Else
            CustomClipboard.ClipboardClose
        End If
        EndProcess
    End With
End Function

Public Function PathNodeFromPoint(ByVal X As Long, ByVal Y As Long) As Fury2Waypoint
On Error Resume Next
Dim l_wpWaypoint As Fury2Waypoint
Dim l_colList As New Collection
Dim l_sngXDistance As Single, l_sngYDistance As Single, l_sngDistance As Single
Dim l_sngSmallestDistance As Single, l_wpSmallest As Fury2Waypoint
    For Each l_wpWaypoint In m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite).Path
        l_sngDistance = Sqr((Abs(l_wpWaypoint.X - X) ^ 2) + (Abs(l_wpWaypoint.Y - Y) ^ 2))
        If (l_sngDistance) <= 3 Then
            l_colList.Add l_wpWaypoint
        End If
    Next l_wpWaypoint
    If l_colList.Count = 1 Then
        Set PathNodeFromPoint = l_colList(1)
    ElseIf l_colList.Count > 1 Then
        l_sngSmallestDistance = 99999999
        For Each l_wpWaypoint In l_colList
            l_sngDistance = Sqr((Abs(l_wpWaypoint.X - X) ^ 2) + (Abs(l_wpWaypoint.Y - Y) ^ 2))
            If l_sngDistance <= l_sngSmallestDistance Then
                l_sngSmallestDistance = l_sngDistance
                Set l_wpSmallest = l_wpWaypoint
            End If
        Next l_wpWaypoint
        Set PathNodeFromPoint = l_wpSmallest
    End If
End Function

Private Sub iToolbar_HideToolbar(Toolbar As Object)
On Error Resume Next
    Set m_tbrToolbar = Toolbar
    m_tbrToolbar.Visible = False
    Set m_tbrToolbar = Nothing
End Sub

Private Sub iToolbar_ShowToolbar(Toolbar As Object)
On Error Resume Next
    Set m_tbrToolbar = Toolbar
    m_tbrToolbar.Visible = True
    RefreshMapTools
End Sub

Private Sub picBrush_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    picBrush.SetFocus
    Editor.ActionUpdate
    Select Case QuickShowMenu(picBrush, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        Menus(MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
        frmIcons.ilContextMenus)
    Case 1
        CutBrush
    Case 2
        CopyBrush
    Case 3
        PasteBrush
    Case 4
        DeleteBrush
    Case Else
    End Select
End Sub

Private Sub picBrush_Paint()
On Error Resume Next
Dim l_sngXScale As Single, l_sngYScale As Single
Dim l_lngW As Long, l_lngH As Long
    If picBrush.Visible = False Then Exit Sub
    If (picBrush.ScaleWidth < m_imgBrush.Width) Then
        l_sngXScale = picBrush.ScaleWidth / m_imgBrush.Width
    Else
        l_sngXScale = 1
    End If
    If (picBrush.ScaleHeight < m_imgBrush.Height) Then
        l_sngYScale = picBrush.ScaleHeight / m_imgBrush.Height
    Else
        l_sngYScale = 1
    End If
    If (l_sngYScale < l_sngXScale) Then l_sngXScale = l_sngYScale
    l_lngW = m_imgBrush.Width * l_sngXScale
    l_lngH = m_imgBrush.Height * l_sngYScale
    SetStretchBltMode picBrush.hdc, StretchBlt_ColorOnColor
    DrawImageToDC picBrush.hdc, F2Rect(0, 0, l_lngW, l_lngH, False), m_imgBrush.Rectangle, m_imgBrush
    picBrush.Line (l_lngW, 0)-(picBrush.ScaleWidth, picBrush.ScaleHeight), picBrush.BackColor, BF
    picBrush.Line (0, l_lngH)-(l_lngW, picBrush.ScaleHeight), picBrush.BackColor, BF
End Sub

Private Sub picBrush_Resize()
On Error Resume Next
    picBrush_Paint
End Sub

Private Sub picContainer_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebar.MouseDown Button, Shift, X, Y
End Sub

Private Sub picContainer_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebar.MouseMove Button, Shift, X, Y
End Sub

Private Sub picContainer_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebar.MouseUp Button, Shift, X, Y
End Sub

Private Sub picContainer_Resize()
On Error Resume Next
    m_splSidebar.Resize
End Sub

Private Sub picDisplayBuffer_Resize()
On Error Resume Next
    DeallocateBackbuffer
    AllocateBackbuffer
    Redraw
End Sub

Private Sub picInspectors_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebarInspectors.MouseDown Button, Shift, X, Y
End Sub

Private Sub picInspectors_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebarInspectors.MouseMove Button, Shift, X, Y
End Sub

Private Sub picInspectors_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebarInspectors.MouseUp Button, Shift, X, Y
End Sub

Private Sub picInspectors_Resize()
On Error Resume Next
    m_splSidebarInspectors.Resize
End Sub

Private Sub picMapView_Resize()
On Error Resume Next
    hsMap.Move 0, picMapView.ScaleHeight - hsMap.Height, picMapView.ScaleWidth - (vsMap.Width - 1), hsMap.Height
    vsMap.Move picMapView.ScaleWidth - vsMap.Width, 0, vsMap.Width, picMapView.ScaleHeight - (hsMap.Height - 1)
    picMapViewport.Move 0, 0, picMapView.ScaleWidth - vsMap.Width, picMapView.ScaleHeight - hsMap.Height
End Sub

Private Sub picMapViewport_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_lngMouseX = (X / Zoom) + hsMap.Value
    m_lngMouseY = (Y / Zoom) + vsMap.Value
    RefreshMouse
    m_lngMouseButtons = Button
    m_lngStartMouseX = m_lngMouseX
    m_lngStartMouseY = m_lngMouseY
    m_lngStartMouseTileX = m_lngMouseTileX
    m_lngStartMouseTileY = m_lngMouseTileY
    Tool_Down Button, Shift, X, Y
    
    m_lngLastMouseX = m_lngMouseX
    m_lngLastMouseY = m_lngMouseY
    m_lngLastMouseTileX = m_lngMouseTileX
    m_lngLastMouseTileY = m_lngMouseTileY
End Sub

Private Sub picMapViewport_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_lngMouseX = (X / Zoom) + hsMap.Value
    m_lngMouseY = (Y / Zoom) + vsMap.Value
    RefreshMouse
    m_lngMouseButtons = Button
    If Button <> 0 Then
        If m_booMouseMoved Then
            AutoScroll , , True
            m_lngMouseX = (X / Zoom) + hsMap.Value
            m_lngMouseY = (Y / Zoom) + vsMap.Value
            RefreshMouse
        End If
    End If
    Tool_Move Button, Shift, X, Y

    m_lngLastMouseX = m_lngMouseX
    m_lngLastMouseY = m_lngMouseY
    m_lngLastMouseTileX = m_lngMouseTileX
    m_lngLastMouseTileY = m_lngMouseTileY
End Sub

Private Sub picMapViewport_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_lngMouseX = (X / Zoom) + hsMap.Value
    m_lngMouseY = (Y / Zoom) + vsMap.Value
    RefreshMouse
    Tool_Up Button, Shift, X, Y
    
    m_lngMouseButtons = 0
End Sub

Private Sub picMapViewport_Paint()
On Error Resume Next
    RefreshViewport
End Sub

Private Sub picMapViewport_Resize()
On Error Resume Next
    tmrResize.Enabled = False
    tmrResize.Enabled = True
End Sub

Private Sub picOverlay_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If m_booOverlayLocked Then Exit Sub
    picMapViewport_MouseDown Button, Shift, X + picOverlay.Left, Y + picOverlay.Top
End Sub

Private Sub picOverlay_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If m_booOverlayLocked Then Exit Sub
    picMapViewport_MouseMove Button, Shift, X + picOverlay.Left, Y + picOverlay.Top
End Sub

Private Sub picOverlay_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If m_booOverlayLocked Then Exit Sub
    picMapViewport_MouseUp Button, Shift, X + picOverlay.Left, Y + picOverlay.Top
End Sub

Private Sub picOverlay_Paint()
On Error Resume Next
    BitBlt picOverlay.hdc, 0, 0, picOverlay.ScaleWidth, picOverlay.ScaleHeight, m_lngOverlayDC, 0, 0, vbSrcCopy
End Sub

Private Sub picOverlay_Resize()
On Error Resume Next
End Sub

Private Sub picSidebar_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebarPanels.MouseDown Button, Shift, X, Y
End Sub

Private Sub picSidebar_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebarPanels.MouseMove Button, Shift, X, Y
End Sub

Private Sub picSidebar_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebarPanels.MouseUp Button, Shift, X, Y
End Sub

Private Sub picSidebar_Resize()
On Error Resume Next
    m_splSidebarPanels.Resize
End Sub

Public Sub PropertyUndoPush(ByRef Obj As Object, ByRef Method As String, ByRef Value As Variant)
On Error Resume Next
Dim l_undUndo As cPropertyUndoEntry
    If Obj Is Nothing Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cPropertyUndoEntry
    With l_undUndo
        Set .Object = Obj
        .MethodName = Method
        If (VarType(Value) And vbObject) = vbObject Then
            Set .Value = Value
        Else
            .Value = Value
        End If
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Function Redo() As Boolean
On Error Resume Next
Dim l_undRedo As iUndoEntry
Dim l_undUndo As iUndoEntry
    If m_colRedo.Count > 0 Then
        BeginProcess "Preparing to Redo..."
        Set l_undRedo = m_colRedo.Item(1)
        Set l_undUndo = l_undRedo.CreateReverse()
        m_colRedo.Remove 1
        m_colUndo.Add l_undUndo
        UpdateProcess 0, "Performing Redo..."
        Redo = SmartApply(l_undRedo)
        EndProcess
        Editor.ActionUpdate
    End If
End Function

Public Sub Redraw()
On Error Resume Next
    ' Very bad for very arcane reasons
    'BeginProcess "Redrawing..."
    If m_imgBackbuffer Is Nothing Then AllocateBackbuffer
    m_imgBackbuffer.Clear m_voViewOptions.BackgroundColor
    If m_mapMap Is Nothing Then
    Else
        UpdateCamera
        m_camCamera.Render , m_imgBackbuffer
    End If
    If m_voViewOptions.ShowGrid Then
'        Filter_Grid_SourceAlpha m_imgBackbuffer.Handle, m_imgBackbuffer.Rectangle.GetRectangle, F2RGB(0, 0, 0, 96), m_lngTileWidth, m_lngTileHeight, -hsMap.Value + 1, -vsMap.Value + 1
'        Filter_Grid_SourceAlpha m_imgBackbuffer.Handle, m_imgBackbuffer.Rectangle.GetRectangle, F2RGB(255, 255, 255, 127), m_lngTileWidth, m_lngTileHeight, -hsMap.Value, -vsMap.Value
        Filter_Grid_SourceAlpha m_imgBackbuffer.Handle, m_imgBackbuffer.Rectangle.GetRectangle, m_voViewOptions.GridColor, m_lngTileWidth, m_lngTileHeight, -hsMap.Value, -vsMap.Value
    End If
    Select Case m_lngCurrentView
    Case View_Blocking
        Redraw_Blocking
    Case View_Lighting
        Redraw_Lighting
    Case View_Areas
        Redraw_Areas
    Case View_Sprites
        Redraw_Sprites
    Case View_Objects
        Redraw_Objects
    Case Else
    End Select
    If m_mapMap Is Nothing Then
    Else
        m_lngTileWidth = m_mapMap.Layers(m_lngSelectedLayer).Tileset.TileWidth
        m_lngTileHeight = m_mapMap.Layers(m_lngSelectedLayer).Tileset.TileHeight
    End If
    RefreshViewport
    RefreshOverlay
    ' Very bad for very arcane reasons
    'EndProcess
End Sub

Public Sub Redraw_Areas()
On Error Resume Next
Dim l_araArea As Fury2Area
Dim l_rctArea As Fury2Rect
Dim l_sngAlpha As Single
Dim l_lngIndex As Long
    With m_imgBackbuffer
        For Each l_araArea In m_mapMap.Areas
            l_lngIndex = l_lngIndex + 1
            l_sngAlpha = IIf(l_lngIndex = m_lngSelectedArea, 1, 0.66)
            Set l_rctArea = l_araArea.Rectangle.Copy.Translate(-hsMap.Value, -vsMap.Value)
            If l_lngIndex = m_lngSelectedArea Then
                .Fill l_rctArea, SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
            End If
            .Box l_rctArea, F2RGB(255, 255, 255, 220 * l_sngAlpha), RenderMode_SourceAlpha
            .Box l_rctArea.Adjust(-1, -1), F2RGB(0, 0, 0, 220 * l_sngAlpha), RenderMode_SourceAlpha
        Next l_araArea
    End With
End Sub

Public Sub Redraw_Blocking()
On Error Resume Next
Dim l_rctSelection As Fury2Rect
    With m_mapMap.Layers(m_lngSelectedLayer)
        ReDim Preserve m_bytSelectedCollisionLines(0 To .CollisionLineCount)
        SoftFX.MultiPrimitive_Line_AA_Ptr m_imgBackbuffer.Handle, .CollisionLinePointer(0), m_voViewOptions.BlockingColor, .CollisionLineCount, hsMap.Value, vsMap.Value
        SoftFX.RenderLines_Masked m_imgBackbuffer.Handle, .CollisionLinePointer(0), VarPtr(m_bytSelectedCollisionLines(1)), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), 0, 2), 255), .CollisionLineCount, hsMap.Value, vsMap.Value
    End With
    Select Case Tool_Blocking
    Case BlockingTool_Line, BlockingTool_PolyLine
        If m_lngPoint > 0 Then
            m_imgBackbuffer.FilledEllipse Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 255), 1, 1
            m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
        End If
        m_imgBackbuffer.FilledEllipse Array(m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 220, 220, 127), 1, 1, RenderMode_SourceAlpha
    Case BlockingTool_Rectangle
        If m_lngPoint > 0 Then
            m_imgBackbuffer.FilledEllipse Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 255), 1, 1
            m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_fptPoints(0).X - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
            m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 127)
            m_imgBackbuffer.AntiAliasLine Array(m_lngMouseX - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
            m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_lngMouseY - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
        End If
        m_imgBackbuffer.FilledEllipse Array(m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 220, 220, 127), 1, 1, RenderMode_SourceAlpha
    Case BlockingTool_Select
        If m_booSelectingBlocking Then
            Set l_rctSelection = F2Rect(m_lngStartMouseX - hsMap.Value, m_lngStartMouseY - vsMap.Value, m_lngMouseX - hsMap.Value + Sgn(m_lngMouseX - m_lngStartMouseX), m_lngMouseY - vsMap.Value + Sgn(m_lngMouseY - m_lngStartMouseY)).Rationalize
            If l_rctSelection.Width >= 4 And l_rctSelection.Height >= 4 Then
                m_imgBackbuffer.Box l_rctSelection, F2Black
                m_imgBackbuffer.Box l_rctSelection.Adjust(-1, -1), F2White
                m_imgBackbuffer.Fill l_rctSelection.Adjust(-1, -1), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
            End If
        End If
    Case Else
    End Select
End Sub

Public Sub Redraw_Lighting()
On Error Resume Next
Dim l_litLight As Fury2LightSource
Dim l_sngX As Single, l_sngY As Single
Dim l_sngAlpha As Single
Dim l_varPlane As Variant, l_varLine As Variant
Dim l_lngIndex As Long, l_lngItems As Long
Dim l_lngPlaneColor As Long
Dim l_rctSelection As Fury2Rect
    With m_imgBackbuffer
        For Each l_litLight In m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights
            l_lngIndex = l_lngIndex + 1
            l_sngAlpha = IIf(l_lngIndex = m_lngSelectedLight, 1, 0.66)
            l_sngX = l_litLight.X - hsMap.Value
            l_sngY = l_litLight.Y - vsMap.Value
            If Tool_Lighting = LightingTool_Cursor Then
                If (l_lngIndex = m_lngSelectedLight) Then
                    .FilledEllipse Array(l_sngX, l_sngY), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), ClipValue(l_litLight.FalloffDistance, 2, 9999) + 1, ClipValue(l_litLight.FalloffDistance, 2, 9999) + 1, RenderMode_SourceAlpha
                End If
                .Ellipse Array(l_sngX, l_sngY), SetAlpha(F2Black, l_sngAlpha * 255), ClipValue(l_litLight.FalloffDistance, 2, 9999) + 1, ClipValue(l_litLight.FalloffDistance, 2, 9999) + 1, RenderMode_SourceAlpha
                .Ellipse Array(l_sngX, l_sngY), SetAlpha(F2Black, l_sngAlpha * 255), ClipValue(l_litLight.FalloffDistance, 2, 9999) - 1, ClipValue(l_litLight.FalloffDistance, 2, 9999) - 1, RenderMode_SourceAlpha
                .Ellipse Array(l_sngX, l_sngY), SetAlpha(F2White, l_sngAlpha * 255), ClipValue(l_litLight.FalloffDistance, 2, 9999), ClipValue(l_litLight.FalloffDistance, 2, 9999), RenderMode_SourceAlpha
                If l_litLight.Spread > 90 Then
                Else
                    .AntiAliasLine Array(l_sngX, l_sngY, l_sngX + (Sin((l_litLight.Angle - (l_litLight.Spread / 2)) * c_dblRadian) * l_litLight.FalloffDistance), l_sngY + (-Cos((l_litLight.Angle - (l_litLight.Spread / 2)) * c_dblRadian) * l_litLight.FalloffDistance)), SetAlpha(F2White, l_sngAlpha * 255)
                    .AntiAliasLine Array(l_sngX, l_sngY, l_sngX + (Sin((l_litLight.Angle + (l_litLight.Spread / 2)) * c_dblRadian) * l_litLight.FalloffDistance), l_sngY + (-Cos((l_litLight.Angle + (l_litLight.Spread / 2)) * c_dblRadian) * l_litLight.FalloffDistance)), SetAlpha(F2White, l_sngAlpha * 255)
                End If
            End If
            .FilledEllipse Array(l_sngX, l_sngY), F2RGB(0, 0, 0, 255 * l_sngAlpha), 3, 3, RenderMode_SourceAlpha
            .FilledEllipse Array(l_sngX, l_sngY), F2RGB(255, 255, 255, 255 * l_sngAlpha), 2, 2, RenderMode_SourceAlpha
        Next l_litLight
    End With
    With m_mapMap.Layers(m_lngSelectedLayer).Lighting
        l_lngPlaneColor = m_voViewOptions.LightingPlaneColor
        If .ObstructionCount Then
            ReDim Preserve m_bytSelectedLightingObstructions(0 To .ObstructionCount)
            SoftFX.MultiPrimitive_Line_AA_Ptr m_imgBackbuffer.Handle, .ObstructionPointer(1), m_voViewOptions.LightingObstructionColor, .ObstructionCount, hsMap.Value, vsMap.Value
            SoftFX.RenderLines_Masked m_imgBackbuffer.Handle, .ObstructionPointer(1), VarPtr(m_bytSelectedLightingObstructions(1)), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), 0, 2), 255), .ObstructionCount, hsMap.Value, vsMap.Value
        End If
        If .PlaneCount Then
            ReDim Preserve m_bytSelectedLightingPlanes(0 To .PlaneCount)
            SoftFX.MultiPrimitive_Plane_AA_Ptr m_imgBackbuffer.Handle, .PlanePointer(1), m_voViewOptions.LightingPlaneColor, .PlaneCount, hsMap.Value, vsMap.Value
            SoftFX.MultiPrimitive_Plane_AA_Masked_Ptr m_imgBackbuffer.Handle, .PlanePointer(1), VarPtr(m_bytSelectedLightingPlanes(1)), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), 0, 2), 255), .PlaneCount, hsMap.Value, vsMap.Value
        End If
        Select Case Tool_Lighting
        Case LightingTool_Obstruction
            If m_lngPoint > 0 Then
                m_imgBackbuffer.FilledEllipse Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 255), 1, 1
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
            End If
            m_imgBackbuffer.FilledEllipse Array(m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 220, 220, 127), 1, 1, RenderMode_SourceAlpha
        Case LightingTool_ObstructionRectangle
            If m_lngPoint > 0 Then
                m_imgBackbuffer.FilledEllipse Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 255), 1, 1
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_fptPoints(0).X - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_lngMouseX - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_lngMouseY - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
            End If
            m_imgBackbuffer.FilledEllipse Array(m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 220, 220, 127), 1, 1, RenderMode_SourceAlpha
        Case LightingTool_Plane
            If m_lngPoint > 1 Then
                m_imgBackbuffer.FilledEllipse Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 255), 1, 1
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_fptPoints(0).X - hsMap.Value, m_fptPoints(1).Y - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_fptPoints(1).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(1).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_fptPoints(1).X - hsMap.Value, m_fptPoints(1).Y - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(1).Y - vsMap.Value, m_fptPoints(1).X - hsMap.Value, m_fptPoints(1).Y - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.FilledEllipse Array(m_fptPoints(1).X - hsMap.Value, m_fptPoints(1).Y - vsMap.Value), F2RGB(220, 0, 220, 255), 1, 1
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(1).X - hsMap.Value, m_fptPoints(1).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
            ElseIf m_lngPoint > 0 Then
                m_imgBackbuffer.FilledEllipse Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 255), 1, 1
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_fptPoints(0).X - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_fptPoints(0).Y - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_lngMouseX - hsMap.Value, m_fptPoints(0).Y - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
                m_imgBackbuffer.AntiAliasLine Array(m_fptPoints(0).X - hsMap.Value, m_lngMouseY - vsMap.Value, m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 0, 220, 127)
            End If
            m_imgBackbuffer.FilledEllipse Array(m_lngMouseX - hsMap.Value, m_lngMouseY - vsMap.Value), F2RGB(220, 220, 220, 127), 1, 1, RenderMode_SourceAlpha
        Case LightingTool_SelectObstructions, LightingTool_SelectPlanes
            If (m_booSelectingLightingObstructions) Or (m_booSelectingLightingPlanes) Then
                Set l_rctSelection = F2Rect(m_lngStartMouseX - hsMap.Value, m_lngStartMouseY - vsMap.Value, m_lngMouseX - hsMap.Value + Sgn(m_lngMouseX - m_lngStartMouseX), m_lngMouseY - vsMap.Value + Sgn(m_lngMouseY - m_lngStartMouseY)).Rationalize
                If l_rctSelection.Width >= 4 And l_rctSelection.Height >= 4 Then
                    m_imgBackbuffer.Box l_rctSelection, F2Black
                    m_imgBackbuffer.Box l_rctSelection.Adjust(-1, -1), F2White
                    m_imgBackbuffer.Fill l_rctSelection.Adjust(-1, -1), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
                End If
            End If
        Case Else
        End Select
    End With
End Sub

Public Sub Redraw_Objects()
On Error Resume Next
Dim l_mobObject As Fury2MapObject
Dim l_sndObject As Fury2SoundObject
Dim l_lngIndex As Long
Dim l_lngRadius As Long
Dim l_sngAlpha As Single
Dim l_lngX As Long, l_lngY As Long
    With m_imgBackbuffer
        For Each l_mobObject In m_mapMap.Objects
            l_lngIndex = l_lngIndex + 1
            l_sngAlpha = IIf(l_lngIndex = m_lngSelectedObject, 1, 0.66)
            l_mobObject.Render m_imgBackbuffer, hsMap.Value, vsMap.Value
            If TypeOf l_mobObject Is Fury2SoundObject Then
                Set l_sndObject = l_mobObject
                l_lngX = l_sndObject.X - hsMap.Value
                l_lngY = l_sndObject.Y - vsMap.Value
                If (l_lngIndex = m_lngSelectedObject) Then
                    .FilledEllipse Array(l_lngX, l_lngY), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), l_sndObject.FalloffDistance + l_sndObject.FalloffOffset, l_sndObject.FalloffDistance + l_sndObject.FalloffOffset, RenderMode_SourceAlpha
                End If
                l_lngRadius = l_sndObject.FalloffOffset
                .Ellipse Array(l_lngX, l_lngY), SetAlpha(F2White, l_sngAlpha * 255), l_lngRadius, l_lngRadius, RenderMode_SourceAlpha
                l_lngRadius = l_sndObject.FalloffDistance + l_sndObject.FalloffOffset
                .Ellipse Array(l_lngX, l_lngY), SetAlpha(F2Black, l_sngAlpha * 255), l_lngRadius - 1, l_lngRadius - 1, RenderMode_SourceAlpha
                .Ellipse Array(l_lngX, l_lngY), SetAlpha(F2White, l_sngAlpha * 255), l_lngRadius, l_lngRadius, RenderMode_SourceAlpha
                .Ellipse Array(l_lngX, l_lngY), SetAlpha(F2Black, l_sngAlpha * 255), l_lngRadius + 1, l_lngRadius + 1, RenderMode_SourceAlpha
            End If
        Next l_mobObject
    End With
End Sub

Public Sub Redraw_Sprites()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_rctSprite As Fury2Rect
Dim l_sngAlpha As Single
Dim l_lngIndex As Long, l_lngNode As Long
Dim l_wpNode As Fury2Waypoint
Dim l_sngLastX As Single, l_sngLastY As Single
Dim l_rctSelection As Fury2Rect
    With m_imgBackbuffer
        For Each l_sprSprite In m_mapMap.Layers(m_lngSelectedLayer).Sprites
            l_lngIndex = l_lngIndex + 1
            l_sngAlpha = IIf(l_lngIndex = m_lngSelectedSprite, 1, 0.66)
            Set l_rctSprite = l_sprSprite.Rectangle(True).Copy.Translate(-hsMap.Value, -vsMap.Value)
            If l_lngIndex = m_lngSelectedSprite Then
                .Fill l_rctSprite, SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
            End If
            .Box l_rctSprite, F2RGB(255, 255, 255, 220 * l_sngAlpha), RenderMode_SourceAlpha
            .Box l_rctSprite.Adjust(-1, -1), F2RGB(0, 0, 0, 220 * l_sngAlpha), RenderMode_SourceAlpha
            Set l_rctSprite = l_sprSprite.Rectangle(False).Copy.Translate(-hsMap.Value, -vsMap.Value)
            .Box l_rctSprite, F2RGB(220, 0, 0, 220 * l_sngAlpha), RenderMode_SourceAlpha
        Next l_sprSprite
        With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
            If .Path.Count > 0 Then
                ReDim Preserve m_bytSelectedPathNodes(0 To .Path.Count)
                If .Path.Looping Then
                    l_sngLastX = .Path(.Path.Count).X
                    l_sngLastY = .Path(.Path.Count).Y
                Else
                    l_sngLastX = .X
                    l_sngLastY = .Y
                End If
                l_lngNode = 1
                For Each l_wpNode In .Path
                    With l_wpNode
                        m_imgBackbuffer.AntiAliasLine Array(l_sngLastX - hsMap.Value, l_sngLastY - vsMap.Value, .X - hsMap.Value, .Y - vsMap.Value), m_voViewOptions.PathColor
                        m_imgBackbuffer.FilledEllipse Array(.X - hsMap.Value, .Y - vsMap.Value), m_voViewOptions.PathNodeColor, 3, 3, RenderMode_SourceAlpha
                        If m_bytSelectedPathNodes(l_lngNode) Then
                            m_imgBackbuffer.FilledEllipse Array(.X - hsMap.Value, .Y - vsMap.Value), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), 3, 3, RenderMode_SourceAlpha
                        End If
                        l_sngLastX = .X
                        l_sngLastY = .Y
                    End With
                    l_lngNode = l_lngNode + 1
                Next l_wpNode
            End If
        End With
        Select Case Tool_Sprites
        Case SpriteTool_Select_Path
            If m_booSelectingPathNodes Then
                Set l_rctSelection = F2Rect(m_lngStartMouseX - hsMap.Value, m_lngStartMouseY - vsMap.Value, m_lngMouseX - hsMap.Value + Sgn(m_lngMouseX - m_lngStartMouseX), m_lngMouseY - vsMap.Value + Sgn(m_lngMouseY - m_lngStartMouseY)).Rationalize
                If l_rctSelection.Width >= 4 And l_rctSelection.Height >= 4 Then
                    m_imgBackbuffer.Box l_rctSelection, F2Black
                    m_imgBackbuffer.Box l_rctSelection.Adjust(-1, -1), F2White
                    m_imgBackbuffer.Fill l_rctSelection.Adjust(-1, -1), SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
                End If
            End If
        Case Else
        End Select
    End With
End Sub

Public Sub RefreshAll()
On Error Resume Next
    Set tpkTiles.ResourceFile = Editor.LoadResources("ng")
    tpkTiles.InitToolbar
    ViewChanged
End Sub

Public Sub RefreshAreas()
On Error Resume Next
    Set elAreas.BoundObject = m_mapMap.Areas
    If elAreas.SelectedItem <> m_lngSelectedArea Then
        elAreas.SelectedItem = m_lngSelectedArea
    Else
        elAreas.Refresh
    End If
End Sub

Public Sub RefreshBrush()
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long
    tpkTiles.SetSelectedTiles m_brsBrush.Tiles
    l_lngWidth = m_brsBrush.Width * m_lngTileWidth
    l_lngHeight = m_brsBrush.Height * m_lngTileHeight
    If m_imgBrush Is Nothing Then
        Set m_imgBrush = F2Image(l_lngWidth, l_lngHeight)
    ElseIf (m_imgBrush.Width <> l_lngWidth) Or (m_imgBrush.Height <> l_lngHeight) Then
        m_imgBrush.Resize l_lngWidth, l_lngHeight
    End If
    m_imgBrush.Clear SwapChannels(GetSystemColor(SystemColor_Button_Highlight), Red, Blue)
    m_brsBrush.Render m_imgBrush, m_mapMap.Layers(m_lngSelectedLayer), 1
    picBrush_Paint
End Sub

Public Sub RefreshBrushOverlay()
On Error Resume Next
Static s_lngEntryCount As Long
Dim l_lngEntryIndex As Long
    s_lngEntryCount = s_lngEntryCount + 1
    l_lngEntryIndex = s_lngEntryCount
    HideOverlay
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RBO_End
    MoveOverlay (m_lngMouseTileX - BrushXCenter) * m_lngTileWidth, (m_lngMouseTileY - BrushYCenter) * m_lngTileHeight, m_brsBrush.Width * m_lngTileWidth, m_brsBrush.Height * m_lngTileHeight
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RBO_End
    ClearOverlay
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RBO_End
    m_brsBrush.Render m_imgOverlay, m_mapMap.Layers(m_lngSelectedLayer), 0.5
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RBO_End
    m_imgOverlay.Box m_imgOverlay.Rectangle.Adjust(-1, -1), F2White, RenderMode_SourceAlpha
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RBO_End
    m_imgOverlay.Box m_imgOverlay.Rectangle, F2Black, RenderMode_SourceAlpha
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RBO_End
    ShowOverlay
RBO_End:
    s_lngEntryCount = s_lngEntryCount - 1
End Sub

Public Sub RefreshDragOverlay()
On Error Resume Next
Static s_lngEntryCount As Long
Dim l_lngEntryIndex As Long
    s_lngEntryCount = s_lngEntryCount + 1
    l_lngEntryIndex = s_lngEntryCount
    HideOverlay
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RDO_End
Dim l_lngX1 As Long, l_lngY1 As Long, l_lngX2 As Long, l_lngY2 As Long
    l_lngX1 = m_lngStartMouseTileX
    l_lngY1 = m_lngStartMouseTileY
    l_lngX2 = m_lngMouseTileX
    l_lngY2 = m_lngMouseTileY
    FixRectCoords l_lngX1, l_lngY1, l_lngX2, l_lngY2
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RDO_End
    MoveOverlay l_lngX1 * m_lngTileWidth, l_lngY1 * m_lngTileHeight, (l_lngX2 - l_lngX1 + 1) * m_lngTileWidth, (l_lngY2 - l_lngY1 + 1) * m_lngTileHeight
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RDO_End
    ClearOverlay
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RDO_End
    m_imgOverlay.Fill m_imgOverlay.Rectangle, SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RDO_End
    m_imgOverlay.Box m_imgOverlay.Rectangle.Adjust(-1, -1), F2White, RenderMode_SourceAlpha
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RDO_End
    m_imgOverlay.Box m_imgOverlay.Rectangle, F2Black, RenderMode_SourceAlpha
    If (s_lngEntryCount > l_lngEntryIndex) Then GoTo RDO_End
    ShowOverlay
RDO_End:
    s_lngEntryCount = s_lngEntryCount - 1
End Sub

Public Sub RefreshInspector()
On Error Resume Next
    m_ctlCurrentInspector.RefreshValues
    m_ctlCurrentInspector.Redraw
End Sub

Public Sub RefreshLayers()
On Error Resume Next
    With elLayers.Toolbar
        If (.Buttons.Count = 0) And (Not (Editor Is Nothing)) Then
            Set .ResourceFile = Editor.LoadResources("ng")
            .ResourcePattern = "map editor\layers\*.png"
            With .Buttons
                .AddNew , "NewLayer", "add", "Add New Layer"
                .AddNew , "DuplicateLayer", "duplicate", "Duplicate Selected Layer"
                .AddNew , "RemoveLayer", "remove", "Remove Selected Layer"
                .AddNew "-"
                .AddNew , "GhostLayers", "ghost", "Ghost Non-Selected Layers", bsyCheck
                .AddNew , "TintLayers", "tint", "Auto-Tint Layers", bsyCheck
            End With
        End If
    End With
    elLayers.ShowToolbar
    SetVisibilityIcons elLayers, "layers"
    If m_mapMap Is Nothing Then
    Else
        Set elLayers.BoundObject = m_mapMap.Layers
        With elLayers.Toolbar
            .Buttons("DeleteLayer").Enabled = (m_mapMap.Layers.Count > 1)
        End With
        If elLayers.SelectedItem <> m_lngSelectedLayer Then
            elLayers.SelectedItem = m_lngSelectedLayer
        Else
            elLayers.Refresh
        End If
        m_lngTileWidth = m_mapMap.Layers(m_lngSelectedLayer).Tileset.TileWidth
        m_lngTileHeight = m_mapMap.Layers(m_lngSelectedLayer).Tileset.TileHeight
    End If
End Sub

Public Sub RefreshLights()
On Error Resume Next
    SetVisibilityIcons elLights, "lights"
    Set elLights.BoundObject = m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights
    If elLights.SelectedItem <> m_lngSelectedLight Then
        elLights.SelectedItem = m_lngSelectedLight
    Else
        elLights.Refresh
    End If
End Sub

Public Sub RefreshList()
On Error Resume Next
Dim l_objObject As Object
    Set l_objObject = insInspect.CurrentObject
    If TypeOf l_objObject Is Fury2Sprite Then
        RefreshSprites
    ElseIf TypeOf l_objObject Is Fury2Area Then
        RefreshAreas
    ElseIf TypeOf l_objObject Is Fury2MapLayer Then
        RefreshLayers
    ElseIf TypeOf l_objObject Is Fury2LayerLighting Then
        RefreshLights
    ElseIf TypeOf l_objObject Is Fury2LightSource Then
        RefreshLights
    ElseIf TypeOf l_objObject Is Fury2MapObject Then
        RefreshObjects
    End If
End Sub

Public Sub RefreshMapTools()
On Error Resume Next
Dim l_lngButtons As Long
    If m_tbrToolbar Is Nothing Then Exit Sub
    If m_lngCurrentView < View_Script Then
        Set m_tbrToolbar.ResourceFile = Editor.LoadResources("ng")
        m_tbrToolbar.DisableUpdates = True
        m_tbrToolbar.Buttons.Clear
        With m_tbrToolbar.Buttons
            Select Case m_lngCurrentView
            Case View_Tiles
                m_tbrToolbar.ResourcePattern = "map editor\tools\tiles\*.png"
                .AddNew , "Tool(0)", "pen", "Pen", bsyGroup
                .AddNew , "Tool(1)", "line", "Line", bsyGroup
                .AddNew "-"
                .AddNew , "Tool(2)", "box", "Box", bsyGroup
                .AddNew , "Tool(3)", "rectangle", "Rectangle", bsyGroup
                .AddNew "-"
                .AddNew , "Tool(4)", "ellipse", "Ellipse", bsyGroup, , False
                .AddNew , "Tool(5)", "filled ellipse", "Filled Ellipse", bsyGroup, , False
                .AddNew "-"
                .AddNew , "Tool(6)", "flood fill", "Flood Fill", bsyGroup
                .AddNew , "Tool(7)", "replace", "Replace", bsyGroup
                .AddNew "-"
                .AddNew , "Tool(8)", "selection", "Selection", bsyGroup, , False
            Case View_Blocking
                m_tbrToolbar.ResourcePattern = "map editor\tools\blocking\*.png"
                .AddNew , "Tool(0)", "line", "Line", bsyGroup
                .AddNew , "Tool(1)", "polygon", "Polygon", bsyGroup
                .AddNew , "Tool(2)", "box", "Box", bsyGroup
                .AddNew "-"
                .AddNew , "Tool(3)", "selection", "Selection", bsyGroup
                .AddNew , "Tool(4)", "mover", "Mover", bsyGroup
            Case View_Lighting
                m_tbrToolbar.ResourcePattern = "map editor\tools\lighting\*.png"
                .AddNew , "Tool(0)", "cursor", "Cursor", bsyGroup
                .AddNew , "Tool(1)", "create light", "Create Light", bsyGroup
                .AddNew "-"
                .AddNew , "Tool(2)", "obstruction", "Place Obstruction", bsyGroup
                .AddNew , "Tool(3)", "obstruction box", "Place Obstruction Box", bsyGroup
                .AddNew , "Tool(4)", "obstruction selection", "Select Obstructions", bsyGroup
                .AddNew , "Tool(5)", "obstruction mover", "Move Obstructions", bsyGroup
                .AddNew "-"
                .AddNew , "Tool(6)", "plane", "Place Plane", bsyGroup
                .AddNew , "Tool(7)", "plane selection", "Select Planes", bsyGroup
                .AddNew , "Tool(8)", "plane mover", "Move Planes", bsyGroup
            Case View_Areas
                m_tbrToolbar.ResourcePattern = "map editor\tools\areas\*.png"
                .AddNew , "Tool(0)", "cursor", "Cursor", bsyGroup
                .AddNew , "Tool(1)", "create", "Create Area", bsyGroup
            Case View_Sprites
                m_tbrToolbar.ResourcePattern = "map editor\tools\sprites\*.png"
                .AddNew , "Tool(0)", "cursor", "Cursor", bsyGroup
                .AddNew , "Tool(1)", "insert", "Insert Sprite", bsyGroup, , False
                .AddNew "-"
                .AddNew , "Tool(2)", "add path node", "Add Path Node", bsyGroup
                .AddNew , "Tool(3)", "select path nodes", "Select Path Nodes", bsyGroup
            Case View_Objects
                m_tbrToolbar.ResourcePattern = "map editor\tools\objects\*.png"
                .AddNew , "Tool(0)", "cursor", "Cursor", bsyGroup
'                .AddNew , "Tool(1)", "create sound", "Create Sound", bsyGroup
            Case Else
            End Select
            m_tbrToolbar.ResourcePattern = "map editor\tools\*.png"
            .AddNew "-"
            .AddNew , "ZoomIn", "zoom in", "Zoom In"
            .AddNew , "ZoomOut", "zoom out", "Zoom Out"
            .AddNew "-"
            .AddNew , "ShowGrid", "show grid", "Show Grid", bsyCheck
            .AddNew , "SnapToGrid", "snap to grid", "Snap To Grid", bsyCheck
        End With
        m_tbrToolbar.Buttons("Tool(" & m_lngCurrentTool(m_lngCurrentView) & ")").Checked = True
        m_tbrToolbar.Buttons("ShowGrid").Checked = m_voViewOptions.ShowGrid
        m_tbrToolbar.Buttons("SnapToGrid").Checked = m_voViewOptions.SnapToGrid
        m_tbrToolbar.Buttons("ZoomIn").Enabled = (Zoom < 16)
        m_tbrToolbar.Buttons("ZoomOut").Enabled = (Zoom > 0.25)
        m_tbrToolbar.Reflow
        m_tbrToolbar.DisableUpdates = False
        m_tbrToolbar.Width = m_tbrToolbar.IdealVerticalWidth
        m_tbrToolbar.Visible = True
        m_tbrToolbar.Reflow
    Else
        m_tbrToolbar.Visible = False
    End If
End Sub

Public Sub RefreshMouse()
On Error Resume Next
    m_lngRealMouseX = m_lngMouseX
    m_lngRealMouseY = m_lngMouseY
    m_lngMouseTileX = (m_lngMouseX) \ m_lngTileWidth
    m_lngMouseTileY = (m_lngMouseY) \ m_lngTileHeight
    If m_voViewOptions.SnapToGrid Then
        m_lngMouseX = Round(m_lngMouseX / m_lngTileWidth) * m_lngTileWidth
        m_lngMouseY = Round(m_lngMouseY / m_lngTileHeight) * m_lngTileHeight
    End If
    m_booMouseMoved = (m_lngMouseX <> m_lngLastMouseX) Or (m_lngMouseY <> m_lngLastMouseY)
    m_booMouseMovedTile = (m_lngMouseTileX <> m_lngLastMouseTileX) Or (m_lngMouseTileY <> m_lngLastMouseTileY)
    Editor.SetLocation CStr(m_lngMouseX) & ", " & CStr(m_lngMouseY) & " (Tile " & CStr(m_lngMouseTileX) & ", " & CStr(m_lngMouseTileY) & ")"
End Sub

Public Sub RefreshObjects()
On Error Resume Next
Dim l_mobObject As Fury2MapObject
    SetVisibilityIcons elObjects, "objects"
    Set elObjects.BoundObject = m_mapMap.Objects
    If elObjects.SelectedItem <> m_lngSelectedObject Then
        elObjects.SelectedItem = m_lngSelectedObject
    Else
        elObjects.Refresh
    End If
    Set l_mobObject = m_mapMap.Objects(m_lngSelectedObject)
    tsInspector.DisableUpdates = True
    With tsInspector.Tabs
        If TypeOf l_mobObject Is Fury2SoundObject Then
            If .Item(1).key <> "Sound" Then
                .Clear
                .AddNew "Sound", "Sound"
            End If
        Else
            If .Item(1).key <> "Object" Then
                .Clear
                .AddNew "Object", "Object"
            End If
        End If
    End With
    tsInspector.DisableUpdates = False
    tsInspector.Reflow
    InspectorChanged
End Sub

Public Sub RefreshOverlay()
On Error Resume Next
    If m_imgOverlay Is Nothing Then Exit Sub
    picOverlay.Move (m_lngOverlayX - hsMap.Value) * Zoom, (m_lngOverlayY - vsMap.Value) * Zoom, m_lngOverlayWidth * Zoom, m_lngOverlayHeight * Zoom
    StretchBlt picOverlay.hdc, 0, 0, picOverlay.ScaleWidth, picOverlay.ScaleHeight, m_lngOverlayDC, 0, 0, m_imgOverlay.Width, m_imgOverlay.Height, vbSrcCopy
    If picOverlay.Visible Then
        picOverlay.Refresh
    End If
End Sub

Public Sub RefreshSprites()
On Error Resume Next
    SetVisibilityIcons elSprites, "sprites"
    Set elSprites.BoundObject = m_mapMap.Layers(m_lngSelectedLayer).Sprites
    If elSprites.SelectedItem <> m_lngSelectedSprite Then
        elSprites.SelectedItem = m_lngSelectedSprite
    Else
        elSprites.Refresh
    End If
End Sub

Public Sub RefreshTiles()
On Error Resume Next
    If m_mapMap Is Nothing Then Exit Sub
    Set tpkTiles.Editor = Editor
    Set tpkTiles.Clipboard = CustomClipboard
    Set tpkTiles.Tileset = m_mapMap.Layers(m_lngSelectedLayer).Tileset
    m_lngTileWidth = m_mapMap.Layers(m_lngSelectedLayer).Tileset.TileWidth
    m_lngTileHeight = m_mapMap.Layers(m_lngSelectedLayer).Tileset.TileHeight
End Sub

Public Sub RefreshTool()
On Error Resume Next
Dim l_objObject As Object
    Set l_objObject = insTool.CurrentObject
    If TypeOf l_objObject Is MapEditorViewOptions Then
        ResizeViewport
        ZoomChanged
        Redraw
    End If
End Sub

Public Sub RefreshToolInspector()
On Error Resume Next
    insTool.RefreshValues
    insTool.Redraw
End Sub

Public Sub RefreshViewport(Optional Flip As Boolean = True)
On Error Resume Next
Dim l_lngZoom As Long
Dim l_lngWidth As Long, l_lngHeight As Long
    SetStretchBltMode picMapViewport.hdc, 3
    l_lngZoom = ClipValue(Ceil(Zoom * 100), 25, 1600)
    l_lngWidth = Ceil(m_lngViewWidth * l_lngZoom / 100)
    l_lngHeight = Ceil(m_lngViewHeight * l_lngZoom / 100)
    StretchBlt picMapViewport.hdc, 0, 0, l_lngWidth, l_lngHeight, picDisplayBuffer.hdc, 0, 0, m_lngViewWidth, m_lngViewHeight, vbSrcCopy
    picMapViewport.Line (l_lngWidth, 0)-(picMapViewport.ScaleWidth, picMapViewport.ScaleHeight), GetSystemColor(SystemColor_Button_Highlight), BF
    picMapViewport.Line (0, l_lngHeight)-(picMapViewport.ScaleWidth, picMapViewport.ScaleHeight), GetSystemColor(SystemColor_Button_Highlight), BF
    If (picMapViewport.AutoRedraw) And (Flip) Then picMapViewport.Refresh
End Sub

Public Sub ReloadTilesets()
On Error Resume Next
Dim l_lyrLayer As Fury2MapLayer
    For Each l_lyrLayer In m_mapMap.Layers
        l_lyrLayer.Tileset.Reload
    Next l_lyrLayer
    RefreshAll
    Redraw
End Sub

Public Sub ResetOverlay()
On Error Resume Next
    HideOverlay
    picOverlay.Move 0, 0, 1, 1
End Sub

Public Sub ResizeAll()
On Error Resume Next
    picMapView_Resize
    tsLists_Resize
    tsInspector_Resize
    tsTool_Resize
End Sub

Public Sub ResizeMapTools()
On Error Resume Next
    m_tbrToolbar.Reflow
End Sub

Public Sub ResizeViewport()
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long
    hsMap.Tag = "lock"
    vsMap.Tag = "lock"
    l_lngWidth = ClipValue(picMapViewport.ScaleWidth, 1, m_mapMap.MaxX + 1)
    l_lngHeight = ClipValue(picMapViewport.ScaleHeight, 1, m_mapMap.MaxY + 1)
    m_lngViewWidth = ClipValue(Ceil(picMapViewport.ScaleWidth / Zoom), 1, m_mapMap.MaxX + 1)
    m_lngViewHeight = ClipValue(Ceil(picMapViewport.ScaleHeight / Zoom), 1, m_mapMap.MaxY + 1)
    m_lngDisplayWidth = l_lngWidth
    m_lngDisplayHeight = l_lngHeight
    picDisplayBuffer.Move 0, 0, m_lngViewWidth, m_lngViewHeight
    If m_mapMap.MaxX > m_lngViewWidth Then
        hsMap.Enabled = True
        hsMap.LargeChange = m_lngViewWidth
        hsMap.SmallChange = m_mapMap.Layers(1).Tileset.TileWidth
        hsMap.Max = m_mapMap.MaxX - m_lngViewWidth
        hsMap.Value = ClipValue(hsMap.Value, hsMap.Min, hsMap.Max)
    Else
        hsMap.Enabled = False
        hsMap.Value = 0
    End If
    If m_mapMap.MaxY > m_lngViewHeight Then
        vsMap.Enabled = True
        vsMap.LargeChange = m_lngViewHeight
        vsMap.SmallChange = m_mapMap.Layers(1).Tileset.TileHeight
        vsMap.Max = m_mapMap.MaxY - m_lngViewHeight
        vsMap.Value = ClipValue(vsMap.Value, vsMap.Min, vsMap.Max)
    Else
        vsMap.Enabled = False
        vsMap.Value = 0
    End If
    hsMap.Tag = ""
    vsMap.Tag = ""
End Sub

Public Sub RestoreLayer()
On Error Resume Next
    m_mapMap.Layers(m_lngSelectedLayer).Tiles = m_intLayerCache
End Sub

Private Sub scMap_Change()
On Error Resume Next
    m_mapMap.ScriptSource = scMap.Text
End Sub

Private Sub scObject_Change()
On Error Resume Next
    If m_lngCurrentView = View_Sprites Then
        m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite).ScriptSource = scObject.Text
    ElseIf m_lngCurrentView = View_Areas Then
        m_mapMap.Areas(m_lngSelectedArea).ScriptSource = scObject.Text
    End If
End Sub

Public Sub SelectCollisionLines(Optional ByVal First As Long = 0, Optional ByVal Last As Long = -32767, Optional ByVal Toggle As Boolean = False, Optional ByVal Combine As Boolean = False)
On Error Resume Next
Dim l_lngLines As Long
    If Last = -32767 Then Last = First
    If Combine Then
        For l_lngLines = LBound(m_bytSelectedCollisionLines) To UBound(m_bytSelectedCollisionLines)
            If (l_lngLines >= First) And (l_lngLines <= Last) Then
                m_bytSelectedCollisionLines(l_lngLines) = 255
            End If
        Next l_lngLines
    ElseIf Toggle Then
        For l_lngLines = LBound(m_bytSelectedCollisionLines) To UBound(m_bytSelectedCollisionLines)
            m_bytSelectedCollisionLines(l_lngLines) = IIf((l_lngLines >= First) And (l_lngLines <= Last), IIf(m_bytSelectedCollisionLines(l_lngLines) = 255, 0, 255), 0)
        Next l_lngLines
    Else
        For l_lngLines = LBound(m_bytSelectedCollisionLines) To UBound(m_bytSelectedCollisionLines)
            m_bytSelectedCollisionLines(l_lngLines) = IIf((l_lngLines >= First) And (l_lngLines <= Last), 255, 0)
        Next l_lngLines
    End If
End Sub

Public Sub SelectLayer(ByVal Layer As Long)
On Error Resume Next
    If Layer < 1 Then Exit Sub
    If Layer > m_mapMap.Layers.Count Then Exit Sub
    m_lngSelectedLayer = Layer
    RefreshLayers
    Redraw
End Sub

Public Sub SelectLightingObstructions(Optional ByVal First As Long = 0, Optional ByVal Last As Long = -32767)
On Error Resume Next
Dim l_lngLines As Long
    If Last = -32767 Then Last = First
    For l_lngLines = LBound(m_bytSelectedLightingObstructions) To UBound(m_bytSelectedLightingObstructions)
        m_bytSelectedLightingObstructions(l_lngLines) = IIf((l_lngLines >= First) And (l_lngLines <= Last), 255, 0)
    Next l_lngLines
End Sub

Public Sub SelectLightingPlanes(Optional ByVal First As Long = 0, Optional ByVal Last As Long = -32767)
On Error Resume Next
Dim l_lngPlanes As Long
    If Last = -32767 Then Last = First
    For l_lngPlanes = LBound(m_bytSelectedLightingPlanes) To UBound(m_bytSelectedLightingPlanes)
        m_bytSelectedLightingPlanes(l_lngPlanes) = IIf((l_lngPlanes >= First) And (l_lngPlanes <= Last), 255, 0)
    Next l_lngPlanes
End Sub

Public Sub SelectPathNodes(Optional ByVal First As Long = 0, Optional ByVal Last As Long = -32767)
On Error Resume Next
Dim l_lngNodes As Long
    If Last = -32767 Then Last = First
    For l_lngNodes = LBound(m_bytSelectedPathNodes) To UBound(m_bytSelectedPathNodes)
        m_bytSelectedPathNodes(l_lngNodes) = IIf((l_lngNodes >= First) And (l_lngNodes <= Last), 255, 0)
    Next l_lngNodes
End Sub

Public Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2map", GetTitle(Name))
End Sub

Public Sub SetMap(Map As Fury2Map)
On Error Resume Next
    Set m_mapMap = Map
    m_mapMap.EditMode = True
    m_mapMap.Init
    m_mapMap.Load
End Sub

Public Sub SetZoom(ByVal Percentage As Long)
On Error Resume Next
    Zoom = ClipValue(Percentage, 10, 1600) / 100#
    ZoomChanged
End Sub

Public Sub ShowOverlay()
On Error Resume Next
    picOverlay.Visible = True
    SetStretchBltMode picOverlay.hdc, StretchBlt_ColorOnColor
    StretchBlt picOverlay.hdc, 0, 0, picOverlay.ScaleWidth, picOverlay.ScaleHeight, m_lngOverlayDC, 0, 0, m_imgOverlay.Width, m_imgOverlay.Height, vbSrcCopy
    picOverlay.Refresh
End Sub

Private Function SmartApply(Obj As iUndoEntry) As Boolean
On Error Resume Next
Dim l_ouUndo As cObjectUndoEntry
Dim l_prUndo As cPropertyUndoEntry
    SmartApply = Obj.Apply()
    If TypeOf Obj Is cTileUndoEntry Then
        Redraw
    ElseIf TypeOf Obj Is cObjectUndoEntry Then
        Redraw
        Set l_ouUndo = Obj
        If l_ouUndo.Container Is m_mapMap.Layers Then
            RefreshLayers
        ElseIf l_ouUndo.Container Is m_mapMap.Areas Then
            RefreshAreas
        Else
        End If
    ElseIf TypeOf Obj Is cPropertyUndoEntry Then
        Redraw
        insInspect.RefreshValues
        Set l_prUndo = Obj
    ElseIf TypeOf Obj Is cMultiUndoEntry Then
        Redraw
        insInspect.RefreshValues
    End If
End Function

Public Function SpriteFromPoint(ByVal X As Long, ByVal Y As Long, Optional ByVal Layer As Long = -1) As Fury2Sprite
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    If Layer = -1 Then Layer = m_lngSelectedLayer
    For Each l_sprSprite In m_mapMap.Layers(m_lngSelectedLayer).Sprites
        If l_sprSprite.PointInside(X, Y) Then
            Set SpriteFromPoint = l_sprSprite
            Exit For
        End If
    Next l_sprSprite
End Function

Private Sub m_tbrToolbar_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
Dim l_strKey As String
    l_strKey = LCase(Trim(Button.key))
    If Left(l_strKey, 4) = "tool" Then
        m_lngCurrentTool(m_lngCurrentView) = CLng(Mid(l_strKey, 6, Len(l_strKey) - 6))
        ToolChanged
    Else
        Select Case l_strKey
        Case "showgrid"
            m_voViewOptions.ShowGrid = Button.Checked
            RefreshToolInspector
            Redraw
        Case "snaptogrid"
            m_voViewOptions.SnapToGrid = Button.Checked
            RefreshToolInspector
        Case "zoomin"
            Zoom = Zoom * 2
            If Zoom > 16 Then Zoom = 16
            ResizeViewport
            ZoomChanged
        Case "zoomout"
            Zoom = Zoom / 2
            If Zoom < 0.25 Then Zoom = 0.25
            ResizeViewport
            ZoomChanged
        Case Else
        End Select
    End If
End Sub

Public Sub Tiles_ToolChanged()
On Error Resume Next
    With tsTool.Tabs
        Select Case m_lngCurrentTool(View_Tiles)
        Case TileTool_Pen, TileTool_Line, TileTool_Rectangle, TileTool_FilledRectangle, _
            TileTool_Circle, TileTool_FilledCircle, TileTool_FloodFill, TileTool_Replace
            .AddNew "Brush", "Brush"
            .AddNew "Options", "Options"
        Case TileTool_Selection
            .AddNew "Options", "Options"
        Case Else
        End Select
    End With
End Sub

Private Sub tmrResize_Timer()
On Error Resume Next
    tmrResize.Enabled = False
    DoEvents
    ResizeViewport
    Redraw
End Sub

Public Sub ToggleLayerVisibility(ByVal Layer As Long)
On Error Resume Next
    If Layer < 1 Then Exit Sub
    If Layer > m_mapMap.Layers.Count Then Exit Sub
    PropertyUndoPush m_mapMap.Layers(Layer), "Visible", m_mapMap.Layers(Layer).Visible
    m_mapMap.Layers(Layer).Visible = Not m_mapMap.Layers(Layer).Visible
    RefreshLayers
    Redraw
End Sub

Private Property Get Tool_Areas() As AreaTools
On Error Resume Next
    Tool_Areas = m_lngCurrentTool(View_Areas)
End Property

Public Sub Tool_Areas_Down(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_araOld As Fury2Area
Dim l_araNew As Fury2Area
    Set l_araOld = m_mapMap.Areas(m_lngSelectedArea)
    Select Case Tool_Areas
    Case AreaTool_Cursor
        Set l_araNew = AreaFromPoint(m_lngRealMouseX, m_lngRealMouseY)
        If Button = 1 Then
            If l_araOld Is l_araNew Then
                m_booDraggingArea = True
                Set m_rctDragStart = l_araNew.Rectangle
                MultiPropertyUndoPush l_araNew, Array("X", "Y"), Array(m_rctDragStart.Left, m_rctDragStart.Top)
            Else
                m_lngSelectedArea = m_mapMap.Areas.Find(l_araNew)
            End If
            RefreshAreas
            Redraw
        ElseIf Button = 2 Then
            m_lngSelectedArea = m_mapMap.Areas.Find(l_araNew)
            RefreshAreas
            Redraw
            picMapViewport.SetFocus
            Editor.ActionUpdate
            If m_lngSelectedArea > 0 Then
                Select Case QuickShowMenu(picMapViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
                    Menus(MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
                    frmIcons.ilContextMenus)
                Case 1
                    CutArea
                Case 2
                    CopyArea
                Case 3
                    With PasteArea(m_lngSelectedArea, False)
                        .X = m_lngMouseX - (.Width \ 2)
                        .Y = m_lngMouseY - (.Height \ 2)
                    End With
                Case 4
                    DeleteArea
                Case Else
                End Select
            Else
                Select Case QuickShowMenu(picMapViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
                    Menus(MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
                    frmIcons.ilContextMenus)
                Case 1
                    With PasteArea(m_lngSelectedArea, False)
                        .X = m_lngMouseX - (.Width \ 2)
                        .Y = m_lngMouseY - (.Height \ 2)
                    End With
                Case Else
                End Select
            End If
            RefreshAreas
            Redraw
        End If
    Case AreaTool_Draw
    Case Else
    End Select
End Sub

Public Sub Tool_Areas_Move(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Select Case Tool_Areas
    Case AreaTool_Cursor
        If (m_booDraggingArea) Then
            If m_booMouseMoved Then
                With m_mapMap.Areas(m_lngSelectedArea)
                    .X = m_rctDragStart.Left + (m_lngMouseX - m_lngStartMouseX)
                    .Y = m_rctDragStart.Top + (m_lngMouseY - m_lngStartMouseY)
                End With
                Redraw
            End If
        End If
    Case AreaTool_Draw
    Case Else
    End Select
End Sub

Public Sub Tool_Areas_Up(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_araNew As Fury2Area
    Select Case Tool_Areas
    Case AreaTool_Cursor
        If (m_booDraggingArea) Then
            With m_mapMap.Areas(m_lngSelectedArea)
                .X = m_rctDragStart.Left + (m_lngMouseX - m_lngStartMouseX)
                .Y = m_rctDragStart.Top + (m_lngMouseY - m_lngStartMouseY)
            End With
            insInspect.RefreshValues
            insInspect.Redraw
            Redraw
        End If
    Case AreaTool_Draw
        If Button = 1 Then
            Set l_araNew = New Fury2Area
            With l_araNew
                Set .Rectangle = F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY)
                .Name = "New Area"
            End With
            m_mapMap.Areas.Add l_araNew
            m_lngSelectedArea = m_mapMap.Areas.Count
            RefreshAreas
            insInspect.RefreshValues
            insInspect.Redraw
            Redraw
        End If
    Case Else
    End Select
    m_booDraggingArea = False
End Sub

Private Property Get Tool_Blocking() As BlockingTools
On Error Resume Next
    Tool_Blocking = m_lngCurrentTool(View_Blocking)
End Property

Public Sub Tool_Blocking_Down(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button = 2 Then
        m_lngPoint = 0
        Redraw
        Select Case QuickShowMenu(picMapViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
            Menus(MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
            frmIcons.ilContextMenus)
        Case 1
            CutCollisionLines
        Case 2
            CopyCollisionLines
        Case 3
            PasteCollisionLines
        Case 4
            DeleteCollisionLines
        Case Else
        End Select
    Else
        Select Case Tool_Blocking
        Case BlockingTool_Select
            If Button = 1 Then
                m_booSelectingBlocking = True
                Redraw
            End If
        Case BlockingTool_Line, BlockingTool_PolyLine, BlockingTool_Rectangle
            If Button = 1 Then
                m_fptPoints(m_lngPoint).X = m_lngMouseX
                m_fptPoints(m_lngPoint).Y = m_lngMouseY
                m_lngPoint = m_lngPoint + 1
                Redraw
            End If
        Case BlockingTool_Move
            If Button = 1 Then
                BlockingUndoPush
                m_lnCollisionLineCache = m_mapMap.Layers(m_lngSelectedLayer).CollisionLines
                m_booDraggingCollisionLines = True
            End If
        Case Else
        End Select
    End If
End Sub

Public Sub Tool_Blocking_Move(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lnLayer() As FLine
Dim l_lngLine As Long
    Select Case Tool_Blocking
    Case BlockingTool_Select
        If (Button = 1) And m_booSelectingBlocking Then
            With m_mapMap.Layers(m_lngSelectedLayer)
                SoftFX.SelectLines F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize().GetRectangle(), .CollisionLinePointer(0), VarPtr(m_bytSelectedCollisionLines(1)), .CollisionLineCount, 0, 0
            End With
            Redraw
        End If
    Case BlockingTool_Line, BlockingTool_PolyLine, BlockingTool_Rectangle
        If m_booMouseMoved Then
            Redraw
        End If
    Case BlockingTool_Move
        If m_booMouseMoved And m_booDraggingCollisionLines Then
            With m_mapMap.Layers(m_lngSelectedLayer)
                l_lnLayer = m_lnCollisionLineCache
                For l_lngLine = UBound(m_bytSelectedCollisionLines) To 1 Step -1
                    If m_bytSelectedCollisionLines(l_lngLine) Then
                        With l_lnLayer(l_lngLine - 1)
                            With .Start
                                .X = .X + (m_lngMouseX - m_lngStartMouseX)
                                .Y = .Y + (m_lngMouseY - m_lngStartMouseY)
                            End With
                            With .end
                                .X = .X + (m_lngMouseX - m_lngStartMouseX)
                                .Y = .Y + (m_lngMouseY - m_lngStartMouseY)
                            End With
                        End With
                    End If
                Next l_lngLine
                .CollisionLines = l_lnLayer
            End With
            Redraw
        End If
    Case Else
    End Select
End Sub

Public Sub Tool_Blocking_Up(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_rctSelection As Fury2Rect
    m_booDraggingCollisionLines = False
    Select Case Tool_Blocking
    Case BlockingTool_Select
        If Button = 1 Then
            With m_mapMap.Layers(m_lngSelectedLayer)
                Set l_rctSelection = F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize()
                If l_rctSelection.Width < 3 And l_rctSelection.Height < 3 Then
                    Set l_rctSelection = F2Rect(m_lngMouseX - 1, m_lngMouseY - 1, 3, 3, False)
                End If
                SoftFX.SelectLines l_rctSelection.GetRectangle(), .CollisionLinePointer(0), VarPtr(m_bytSelectedCollisionLines(1)), .CollisionLineCount, 0, 0
            End With
            m_booSelectingBlocking = False
            Redraw
        End If
    Case BlockingTool_Line
        If Button = 1 Then
            If m_lngPoint > 1 Then
                BlockingUndoPush
                With m_mapMap.Layers(m_lngSelectedLayer)
                    m_lngPoint = 0
                    .AddCollisionLine m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(1).Y
                    ReDim Preserve m_bytSelectedCollisionLines(0 To .CollisionLineCount)
                    SelectCollisionLines .CollisionLineCount
                End With
                Redraw
            End If
        End If
    Case BlockingTool_PolyLine
        If Button = 1 Then
            If m_lngPoint > 1 Then
                BlockingUndoPush
                With m_mapMap.Layers(m_lngSelectedLayer)
                    m_lngPoint = 1
                    .AddCollisionLine m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(1).Y
                    m_fptPoints(0) = m_fptPoints(1)
                    ReDim Preserve m_bytSelectedCollisionLines(0 To .CollisionLineCount)
                    SelectCollisionLines .CollisionLineCount, , , True
                End With
                Redraw
            End If
        End If
    Case BlockingTool_Rectangle
        If Button = 1 Then
            If m_lngPoint > 1 Then
                BlockingUndoPush
                With m_mapMap.Layers(m_lngSelectedLayer)
                    m_lngPoint = 0
                    .AddCollisionLine m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(0).Y
                    .AddCollisionLine m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(0).X, m_fptPoints(1).Y
                    .AddCollisionLine m_fptPoints(1).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(1).Y
                    .AddCollisionLine m_fptPoints(0).X, m_fptPoints(1).Y, m_fptPoints(1).X, m_fptPoints(1).Y
                    ReDim Preserve m_bytSelectedCollisionLines(0 To .CollisionLineCount)
                    SelectCollisionLines .CollisionLineCount - 3, .CollisionLineCount
                End With
                Redraw
            End If
        End If
    Case Else
    End Select
End Sub

Public Sub Tool_Down(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    HideOverlay
    Set m_rctActionArea = New Fury2Rect
    Select Case m_lngCurrentView
    Case View_Tiles
        Tool_Tiles_Down Button, Shift, X, Y
    Case View_Blocking
        Tool_Blocking_Down Button, Shift, X, Y
    Case View_Areas
        Tool_Areas_Down Button, Shift, X, Y
    Case View_Sprites
        Tool_Sprites_Down Button, Shift, X, Y
    Case View_Lighting
        Tool_Lighting_Down Button, Shift, X, Y
    Case View_Objects
        Tool_Objects_Down Button, Shift, X, Y
    Case Else
    End Select
End Sub

Private Property Get Tool_Lighting() As LightingTools
On Error Resume Next
    Tool_Lighting = m_lngCurrentTool(View_Lighting)
End Property

Public Sub Tool_Lighting_Down(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_litOld As Fury2LightSource
Dim l_litNew As Fury2LightSource
    If Button = 2 Then
        Select Case QuickShowMenu(picMapViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
            Menus(MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
            frmIcons.ilContextMenus)
        Case 1
            Select Case Tool_Lighting
            Case LightingTool_Cursor, LightingTool_Light
                CutLight
            Case LightingTool_Obstruction, LightingTool_ObstructionRectangle, LightingTool_SelectObstructions, LightingTool_MoveObstructions
                CutLightingObstructions
            Case LightingTool_Plane, LightingTool_SelectPlanes, LightingTool_MovePlanes
                CutLightingPlanes
            End Select
        Case 2
            Select Case Tool_Lighting
            Case LightingTool_Cursor, LightingTool_Light
                CopyLight
            Case LightingTool_Obstruction, LightingTool_ObstructionRectangle, LightingTool_SelectObstructions, LightingTool_MoveObstructions
                CopyLightingObstructions
            Case LightingTool_Plane, LightingTool_SelectPlanes, LightingTool_MovePlanes
                CopyLightingPlanes
            End Select
        Case 3
            Select Case Tool_Lighting
            Case LightingTool_Cursor, LightingTool_Light
                With PasteLight(, False)
                    .X = m_lngMouseX
                    .Y = m_lngMouseY
                End With
                RefreshLights
                Redraw
            Case LightingTool_Obstruction, LightingTool_ObstructionRectangle, LightingTool_SelectObstructions, LightingTool_MoveObstructions
            Case LightingTool_Plane, LightingTool_SelectPlanes, LightingTool_MovePlanes
            End Select
        Case 4
            Select Case Tool_Lighting
            Case LightingTool_Cursor, LightingTool_Light
                DeleteLight
            Case LightingTool_Obstruction, LightingTool_ObstructionRectangle, LightingTool_SelectObstructions, LightingTool_MoveObstructions
                DeleteLightingObstructions
            Case LightingTool_Plane, LightingTool_SelectPlanes, LightingTool_MovePlanes
                DeleteLightingPlanes
            End Select
        Case Else
        End Select
    Else
        Set l_litOld = m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights(m_lngSelectedLight)
        Select Case Tool_Lighting
        Case LightingTool_Cursor
            Set l_litNew = LightSourceFromPoint(m_lngRealMouseX, m_lngRealMouseY)
            If l_litOld Is l_litNew Then
                m_booDraggingLight = True
                m_sngDragStartX = l_litOld.X
                m_sngDragStartY = l_litOld.Y
                MultiPropertyUndoPush l_litNew, Array("X", "Y"), Array(m_sngDragStartX, m_sngDragStartY)
            Else
                m_lngSelectedLight = m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights.Find(l_litNew)
            End If
            RefreshLights
            Redraw
        Case LightingTool_Light
            Set l_litNew = New Fury2LightSource
            With l_litNew
                .X = m_lngMouseX
                .Y = m_lngMouseY
                .FalloffDistance = 50
                .Color = F2White
                .Visible = True
                .Name = "New Light"
                .Spread = 180
            End With
            m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights.Add l_litNew
            m_lngSelectedLight = m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights.Find(l_litNew)
            RefreshLights
            Redraw
        Case LightingTool_SelectObstructions
            If Button = 1 Then
                m_booSelectingLightingObstructions = True
                Redraw
            End If
        Case LightingTool_SelectPlanes
            If Button = 1 Then
                m_booSelectingLightingPlanes = True
                Redraw
            End If
        Case LightingTool_MoveObstructions
            If Button = 1 Then
                m_lobLightingObstructionCache = m_mapMap.Layers(m_lngSelectedLayer).Lighting.Obstructions
                m_booDraggingLightingObstructions = True
            End If
        Case LightingTool_MovePlanes
            If Button = 1 Then
                m_plnLightingPlaneCache = m_mapMap.Layers(m_lngSelectedLayer).Lighting.Planes
                m_booDraggingLightingPlanes = True
            End If
        Case LightingTool_Obstruction
            If Button = 1 Then
                m_fptPoints(m_lngPoint).X = m_lngMouseX
                m_fptPoints(m_lngPoint).Y = m_lngMouseY
                m_lngPoint = m_lngPoint + 1
                Redraw
            End If
        Case LightingTool_ObstructionRectangle
            If Button = 1 Then
                m_fptPoints(m_lngPoint).X = m_lngMouseX
                m_fptPoints(m_lngPoint).Y = m_lngMouseY
                m_lngPoint = m_lngPoint + 1
                Redraw
            End If
        Case LightingTool_Plane
            If Button = 1 Then
                m_fptPoints(m_lngPoint).X = m_lngMouseX
                m_fptPoints(m_lngPoint).Y = m_lngMouseY
                m_lngPoint = m_lngPoint + 1
                Redraw
            End If
        Case Else
        End Select
    End If
End Sub

Public Sub Tool_Lighting_Move(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lobObstructions() As LightingObstruction
Dim l_plnPlanes() As LightingPlane2
Dim l_lngLine As Long, l_lngPlane As Long
    Select Case Tool_Lighting
    Case LightingTool_Cursor
        If (Button = 1) And (m_booDraggingLight) Then
            If m_booMouseMoved Then
                With m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights(m_lngSelectedLight)
                    .X = m_sngDragStartX + (m_lngMouseX - m_lngStartMouseX)
                    .Y = m_sngDragStartY + (m_lngMouseY - m_lngStartMouseY)
                End With
                Redraw
            End If
        End If
    Case LightingTool_SelectObstructions
        If (Button = 1) And m_booSelectingLightingObstructions Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                SoftFX.SelectLines F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize().GetRectangle(), .ObstructionPointer(1), VarPtr(m_bytSelectedLightingObstructions(1)), .ObstructionCount, 0, 0
            End With
            Redraw
        End If
    Case LightingTool_SelectPlanes
        If (Button = 1) And m_booSelectingLightingPlanes Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                SoftFX.SelectPlanes F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize().GetRectangle(), .PlanePointer(1), VarPtr(m_bytSelectedLightingPlanes(1)), .PlaneCount, 0, 0
            End With
            Redraw
        End If
    Case LightingTool_Plane, LightingTool_Obstruction, LightingTool_ObstructionRectangle
        If m_booMouseMoved Then
            Redraw
        End If
    Case LightingTool_MoveObstructions
        If m_booMouseMoved And m_booDraggingLightingObstructions Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                l_lobObstructions = m_lobLightingObstructionCache
                For l_lngLine = UBound(m_bytSelectedLightingObstructions) To 1 Step -1
                    If m_bytSelectedLightingObstructions(l_lngLine) Then
                        With l_lobObstructions(l_lngLine).Line
                            With .Start
                                .X = .X + (m_lngMouseX - m_lngStartMouseX)
                                .Y = .Y + (m_lngMouseY - m_lngStartMouseY)
                            End With
                            With .end
                                .X = .X + (m_lngMouseX - m_lngStartMouseX)
                                .Y = .Y + (m_lngMouseY - m_lngStartMouseY)
                            End With
                        End With
                    End If
                Next l_lngLine
                .Obstructions = l_lobObstructions
                .Refresh
            End With
            Redraw
        End If
    Case LightingTool_MovePlanes
        If m_booMouseMoved And m_booDraggingLightingPlanes Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                l_plnPlanes = m_plnLightingPlaneCache
                For l_lngPlane = UBound(m_bytSelectedLightingPlanes) To 1 Step -1
                    If m_bytSelectedLightingPlanes(l_lngPlane) Then
                        With l_plnPlanes(l_lngPlane)
                            With .Start
                                .X = .X + (m_lngMouseX - m_lngStartMouseX)
                                .Y = .Y + (m_lngMouseY - m_lngStartMouseY)
                            End With
                            With .end
                                .X = .X + (m_lngMouseX - m_lngStartMouseX)
                                .Y = .Y + (m_lngMouseY - m_lngStartMouseY)
                            End With
                        End With
                    End If
                Next l_lngPlane
                .Planes = l_plnPlanes
                .Refresh
            End With
            Redraw
        End If
    Case Else
    End Select
End Sub

Public Sub Tool_Lighting_Up(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_rctSelection As Fury2Rect
    m_booDraggingLightingObstructions = False
    m_booDraggingLightingPlanes = False
    Select Case Tool_Lighting
    Case LightingTool_Cursor
        If (m_booDraggingLight) Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting.Lights(m_lngSelectedLight)
                .X = m_sngDragStartX + (m_lngMouseX - m_lngStartMouseX)
                .Y = m_sngDragStartY + (m_lngMouseY - m_lngStartMouseY)
            End With
            insInspect.RefreshValues
            insInspect.Redraw
            Redraw
        End If
        m_booDraggingLight = False
    Case LightingTool_SelectObstructions
        If Button = 1 Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                Set l_rctSelection = F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize()
                If l_rctSelection.Width < 3 And l_rctSelection.Height < 3 Then
                    Set l_rctSelection = F2Rect(m_lngMouseX - 1, m_lngMouseY - 1, 3, 3, False)
                End If
                SoftFX.SelectLines l_rctSelection.GetRectangle(), .ObstructionPointer(1), VarPtr(m_bytSelectedLightingObstructions(1)), .ObstructionCount, 0, 0
            End With
            m_booSelectingLightingObstructions = False
            Redraw
        End If
    Case LightingTool_SelectPlanes
        If Button = 1 Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                Set l_rctSelection = F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize()
                If l_rctSelection.Width < 3 And l_rctSelection.Height < 3 Then
                    Set l_rctSelection = F2Rect(m_lngMouseX - 1, m_lngMouseY - 1, 3, 3, False)
                End If
                SoftFX.SelectPlanes l_rctSelection.GetRectangle(), .PlanePointer(1), VarPtr(m_bytSelectedLightingPlanes(1)), .PlaneCount, 0, 0
            End With
            m_booSelectingLightingPlanes = False
            Redraw
        End If
    Case LightingTool_Obstruction
        If m_lngPoint > 1 Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                m_lngPoint = 0
                .AddObstruction m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(1).Y
                ReDim Preserve m_bytSelectedLightingObstructions(0 To .ObstructionCount)
                SelectLightingObstructions .ObstructionCount
                .Refresh
            End With
            Redraw
        End If
    Case LightingTool_ObstructionRectangle
        If m_lngPoint > 1 Then
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                m_lngPoint = 0
                .AddObstruction m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(0).X, m_fptPoints(1).Y
                .AddObstruction m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(0).Y
                .AddObstruction m_fptPoints(0).X, m_fptPoints(1).Y, m_fptPoints(1).X, m_fptPoints(1).Y
                .AddObstruction m_fptPoints(1).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(1).Y
                ReDim Preserve m_bytSelectedLightingObstructions(0 To .ObstructionCount)
                SelectLightingObstructions .ObstructionCount - 3, .ObstructionCount
                .Refresh
            End With
            Redraw
        End If
    Case LightingTool_Plane
        If m_lngPoint > 2 Then
            m_lngPoint = 0
            With m_mapMap.Layers(m_lngSelectedLayer).Lighting
                .AddPlane m_fptPoints(0).X, m_fptPoints(0).Y, m_fptPoints(1).X, m_fptPoints(1).Y, IIf(m_fptPoints(1).Y > m_fptPoints(0).Y, m_fptPoints(0).Y, m_fptPoints(1).Y) - m_fptPoints(2).Y
                ReDim Preserve m_bytSelectedLightingPlanes(0 To .PlaneCount)
                SelectLightingPlanes .PlaneCount
                .Refresh
            End With
            Redraw
        End If
    Case Else
    End Select
End Sub

Public Sub Tool_Move(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Tiles
        Tool_Tiles_Move Button, Shift, X, Y
    Case View_Blocking
        Tool_Blocking_Move Button, Shift, X, Y
    Case View_Areas
        Tool_Areas_Move Button, Shift, X, Y
    Case View_Sprites
        Tool_Sprites_Move Button, Shift, X, Y
    Case View_Lighting
        Tool_Lighting_Move Button, Shift, X, Y
    Case View_Objects
        Tool_Objects_Move Button, Shift, X, Y
    Case Else
    End Select
End Sub

Private Property Get Tool_Objects() As ObjectTools
On Error Resume Next
    Tool_Objects = m_lngCurrentTool(View_Objects)
End Property

Public Sub Tool_Objects_Down(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sndSound As Fury2SoundObject
Dim l_mobOld As Fury2MapObject
Dim l_mobNew As Fury2MapObject
    If Button = 2 Then
        Select Case QuickShowMenu(picMapViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
            Menus(MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
            frmIcons.ilContextMenus)
        Case 1
        Case 2
        Case 3
        Case 4
        Case Else
        End Select
    Else
        Select Case Tool_Objects
        Case ObjectTool_Cursor
            Set l_mobNew = MapObjectFromPoint(m_lngRealMouseX, m_lngRealMouseY)
            If l_mobOld Is l_mobNew Then
                'm_booDraggingObject = True
'                m_sngDragStartX = l_litOld.X
'                m_sngDragStartY = l_litOld.Y
'                MultiPropertyUndoPush l_litNew, Array("X", "Y"), Array(m_sngDragStartX, m_sngDragStartY)
            Else
                m_lngSelectedObject = m_mapMap.Objects.Find(l_mobNew)
            End If
            RefreshObjects
            Redraw
        Case ObjectTool_Create_Sound
            Set l_sndSound = New Fury2SoundObject
            l_sndSound.X = m_lngMouseX
            l_sndSound.Y = m_lngMouseY
            l_sndSound.FalloffOffset = 10
            l_sndSound.FalloffDistance = 50
            m_mapMap.Objects.Add l_sndSound
            m_lngSelectedObject = m_mapMap.Objects.Count
            RefreshObjects
            Redraw
        Case Else
        End Select
    End If
End Sub

Public Sub Tool_Objects_Move(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Select Case Tool_Objects
    Case Else
    End Select
End Sub

Public Sub Tool_Objects_Up(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Select Case Tool_Objects
    Case Else
    End Select
End Sub

Private Property Get Tool_Sprites() As SpriteTools
On Error Resume Next
    Tool_Sprites = m_lngCurrentTool(View_Sprites)
End Property

Public Sub Tool_Sprites_Down(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sprOld As Fury2Sprite
Dim l_sprNew As Fury2Sprite
    Set l_sprOld = m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
    Select Case Tool_Sprites
    Case SpriteTool_Cursor
        Set l_sprNew = SpriteFromPoint(m_lngRealMouseX, m_lngRealMouseY)
        If Button = 1 Then
            If l_sprOld Is l_sprNew Then
                m_booDraggingSprite = True
                m_sngDragStartX = l_sprOld.X
                m_sngDragStartY = l_sprOld.Y
                MultiPropertyUndoPush l_sprNew, Array("X", "Y"), Array(m_sngDragStartX, m_sngDragStartY)
            Else
                m_lngSelectedSprite = m_mapMap.Layers(m_lngSelectedLayer).Sprites.Find(l_sprNew)
            End If
            RefreshSprites
            Redraw
        ElseIf Button = 2 Then
            m_lngSelectedSprite = m_mapMap.Layers(m_lngSelectedLayer).Sprites.Find(l_sprNew)
            RefreshSprites
            Redraw
            picMapViewport.SetFocus
            Editor.ActionUpdate
            If m_lngSelectedSprite > 0 Then
                Select Case QuickShowMenu(picMapViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
                    Menus(MenuString("Visible", , , , , l_sprNew.Visible), _
                    MenuString("-"), _
                    MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
                    frmIcons.ilContextMenus)
                Case 1
                    With l_sprNew
                        PropertyUndoPush l_sprNew, "Visible", l_sprNew.Visible
                        .Visible = Not .Visible
                        RefreshSprites
                        Redraw
                    End With
                Case 3
                    CutSprite
                Case 4
                    CopySprite
                Case 5
                    PasteSprite m_lngSelectedSprite, False
                Case 6
                    DeleteSprite
                Case Else
                End Select
            Else
                Select Case QuickShowMenu(picMapViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
                    Menus(MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
                    frmIcons.ilContextMenus)
                Case 1
                    With PasteSprite(m_lngSelectedSprite, False)
                        .X = m_lngMouseX
                        .Y = m_lngMouseY
                    End With
                Case Else
                End Select
            End If
            RefreshSprites
            Redraw
        End If
    Case SpriteTool_Select_Path
        m_booSelectingPathNodes = True
    Case SpriteTool_Add_Path
    Case Else
    End Select
End Sub

Public Sub Tool_Sprites_Move(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_wpNode As Fury2Waypoint, l_lngNode As Long
Dim l_rctArea As Fury2Rect
    Select Case Tool_Sprites
    Case SpriteTool_Cursor
        If (m_booDraggingSprite) Then
            If m_booMouseMoved Then
                With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
                    .X = m_sngDragStartX + (m_lngMouseX - m_lngStartMouseX)
                    .Y = m_sngDragStartY + (m_lngMouseY - m_lngStartMouseY)
                End With
                Redraw
            End If
        End If
    Case SpriteTool_Select_Path
        If (Button = 1) And m_booSelectingPathNodes Then
            With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
                If .Path.Count > 0 Then
                    Set l_rctArea = F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize()
                    ReDim Preserve m_bytSelectedPathNodes(0 To .Path.Count)
                    l_lngNode = 1
                    For Each l_wpNode In .Path
                        With l_wpNode
                            If l_rctArea.PointInside(CLng(.X), CLng(.Y)) Then
                                m_bytSelectedPathNodes(l_lngNode) = 255
                            Else
                                m_bytSelectedPathNodes(l_lngNode) = 0
                            End If
                        End With
                        l_lngNode = l_lngNode + 1
                    Next l_wpNode
                End If
            End With
            Redraw
        End If
    Case SpriteTool_Add_Path
    Case Else
    End Select
End Sub

Public Sub Tool_Sprites_Up(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_wpNode As Fury2Waypoint, l_lngNode As Long
Dim l_rctArea As Fury2Rect
    Select Case Tool_Sprites
    Case SpriteTool_Cursor
        If (m_booDraggingSprite) Then
            With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
                .X = m_sngDragStartX + (m_lngMouseX - m_lngStartMouseX)
                .Y = m_sngDragStartY + (m_lngMouseY - m_lngStartMouseY)
            End With
            insInspect.RefreshValues
            insInspect.Redraw
            Redraw
        End If
    Case SpriteTool_Select_Path
        If (Button = 1) And m_booSelectingPathNodes Then
            With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
                If .Path.Count > 0 Then
                    Set l_rctArea = F2Rect(m_lngStartMouseX, m_lngStartMouseY, m_lngMouseX, m_lngMouseY).Rationalize()
                    If l_rctArea.Width < 3 And l_rctArea.Height < 3 Then
                        Set l_rctArea = F2Rect(m_lngMouseX - 2, m_lngMouseY - 2, 5, 5, False)
                    End If
                    ReDim Preserve m_bytSelectedPathNodes(0 To .Path.Count)
                    l_lngNode = 1
                    For Each l_wpNode In .Path
                        With l_wpNode
                            If l_rctArea.PointInside(CLng(.X), CLng(.Y)) Then
                                m_bytSelectedPathNodes(l_lngNode) = 255
                            Else
                                m_bytSelectedPathNodes(l_lngNode) = 0
                            End If
                        End With
                        l_lngNode = l_lngNode + 1
                    Next l_wpNode
                End If
            End With
        End If
        m_booSelectingPathNodes = False
        Redraw
    Case SpriteTool_Add_Path
        With m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
            .Path.Add CSng(m_lngMouseX), CSng(m_lngMouseY)
            ReDim Preserve m_bytSelectedPathNodes(0 To .Path.Count)
            SelectPathNodes .Path.Count
            insInspect.RefreshValues
            insInspect.Redraw
            Redraw
        End With
        Redraw
    Case Else
    End Select
    m_booDraggingSprite = False
End Sub

Private Property Get Tool_Tiles() As TileTools
On Error Resume Next
    Tool_Tiles = m_lngCurrentTool(View_Tiles)
End Property

Public Sub Tool_Tiles_Down(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngBrushXCenter As Long, l_lngBrushYCenter As Long
    l_lngBrushXCenter = -m_brsBrush.XCenter
    l_lngBrushYCenter = -m_brsBrush.YCenter
    If Button = 2 Then
        RefreshDragOverlay
    End If
    Select Case Tool_Tiles
    Case TileTool_Pen
        Select Case Button
        Case 1
            CacheLayer
            BeginProcess "Performing Draw..."
            m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, m_lngMouseTileX + l_lngBrushXCenter, m_lngMouseTileY + l_lngBrushYCenter
            m_rctActionArea.Accomodate m_brsBrush.GetRectangle.Translate(m_lngMouseTileX + l_lngBrushXCenter, m_lngMouseTileY + l_lngBrushYCenter)
            EndProcess
            Redraw
        Case Else
        End Select
    Case TileTool_Line, TileTool_Rectangle, TileTool_FilledRectangle
        Select Case Button
        Case 1
            CacheLayer
        Case Else
        End Select
    Case TileTool_FloodFill
        Select Case Button
        Case 1
            BeginProcess "Preparing to Flood Fill..."
            CacheLayer
            UpdateProcess , "Performing Flood Fill..."
            m_mapMap.Layers(m_lngSelectedLayer).PatternFloodFill m_lngMouseTileX, m_lngMouseTileY, m_brsBrush.Tiles
            UndoPush
            Redraw
            EndProcess
        Case Else
        End Select
    Case TileTool_Replace
        Select Case Button
        Case 1
            BeginProcess "Preparing to Replace..."
            CacheLayer
            UpdateProcess , "Performing Replace..."
            m_mapMap.Layers(m_lngSelectedLayer).PatternReplace m_mapMap.Layers(m_lngSelectedLayer).Tile(m_lngMouseTileX, m_lngMouseTileY), m_brsBrush.Tiles
            UndoPush
            Redraw
            Screen.MousePointer = 0
        Case Else
        End Select
    Case Else
    End Select
End Sub

Public Sub Tool_Tiles_Move(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Static l_lngCount As Long
Dim l_lngBrushXCenter As Long, l_lngBrushYCenter As Long
    l_lngCount = l_lngCount + 1
    If l_lngCount > 2 Then Stop
    l_lngBrushXCenter = -m_brsBrush.XCenter
    l_lngBrushYCenter = -m_brsBrush.YCenter
    If Button = 2 Then
        ' Global grabber override
        If m_booMouseMovedTile Then RefreshDragOverlay
    Else
        Select Case Tool_Tiles
        Case TileTool_Pen
            Select Case Button
            Case 0
                If m_booMouseMovedTile Then
                    RefreshBrushOverlay
                End If
            Case 1
                If m_booMouseMovedTile Then
                    BeginProcess "Performing Draw..."
                    m_brsBrush.Draw m_mapMap, m_lngSelectedLayer, m_lngMouseTileX - BrushXCenter, m_lngMouseTileY - BrushYCenter, , , , , m_lngMouseTileX - m_lngStartMouseTileX, m_lngMouseTileY - m_lngStartMouseTileY
                    m_rctActionArea.Accomodate m_brsBrush.GetRectangle.Translate(m_lngMouseTileX + l_lngBrushXCenter, m_lngMouseTileY + l_lngBrushYCenter)
                    EndProcess
                    Redraw
                End If
            Case Else
            End Select
        Case TileTool_Line
            Select Case Button
            Case 0
                If m_booMouseMovedTile Then
                    RefreshBrushOverlay
                End If
            Case 1
                If m_booMouseMovedTile Then
                    RestoreLayer
                    Draw_Line m_lngStartMouseTileX, m_lngStartMouseTileY, m_lngMouseTileX, m_lngMouseTileY
                    Redraw
                End If
            Case Else
            End Select
        Case TileTool_Rectangle
            Select Case Button
            Case 0
                If m_booMouseMovedTile Then
                    RefreshBrushOverlay
                End If
            Case 1
                If m_booMouseMovedTile Then
                    RestoreLayer
                    Draw_Rectangle m_lngStartMouseTileX, m_lngStartMouseTileY, m_lngMouseTileX, m_lngMouseTileY
                    Redraw
                End If
            Case Else
            End Select
        Case TileTool_FilledRectangle
            Select Case Button
            Case 0
                If m_booMouseMovedTile Then
                    RefreshBrushOverlay
                End If
            Case 1
                If m_booMouseMovedTile Then
                    RestoreLayer
                    Draw_Rectangle_Filled m_lngStartMouseTileX, m_lngStartMouseTileY, m_lngMouseTileX, m_lngMouseTileY
                    Redraw
                End If
            Case Else
            End Select
        Case TileTool_FloodFill, TileTool_Replace
            Select Case Button
            Case 0
                If m_booMouseMovedTile Then
                    RefreshBrushOverlay
                End If
            Case Else
            End Select
        Case Else
        End Select
    End If
    l_lngCount = l_lngCount - 1
End Sub

Public Sub Tool_Tiles_Up(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    HideOverlay
    If Button = 2 Then
    Dim l_lngX1 As Long, l_lngY1 As Long, l_lngX2 As Long, l_lngY2 As Long
        BeginProcess "Preparing to Grab..."
        l_lngX1 = m_lngStartMouseTileX
        l_lngY1 = m_lngStartMouseTileY
        l_lngX2 = m_lngMouseTileX
        l_lngY2 = m_lngMouseTileY
        FixRectCoords l_lngX1, l_lngY1, l_lngX2, l_lngY2
        UpdateProcess , "Performing Grab..."
        m_brsBrush.Grab m_mapMap, m_lngSelectedLayer, l_lngX1, l_lngY1, (l_lngX2 - l_lngX1) + 1, (l_lngY2 - l_lngY1) + 1, , , True
        RefreshBrush
        Redraw
        RefreshBrushOverlay
        EndProcess
    Else
        Select Case Tool_Tiles
        Case TileTool_Pen
            UndoPush m_rctActionArea
            Redraw
            RefreshBrushOverlay
        Case TileTool_Line
            RestoreLayer
            Draw_Line m_lngStartMouseTileX, m_lngStartMouseTileY, m_lngMouseTileX, m_lngMouseTileY
            UndoPush m_rctActionArea
            Redraw
            RefreshBrushOverlay
        Case TileTool_Rectangle
            RestoreLayer
            Draw_Rectangle m_lngStartMouseTileX, m_lngStartMouseTileY, m_lngMouseTileX, m_lngMouseTileY
            UndoPush m_rctActionArea
            Redraw
            RefreshBrushOverlay
        Case TileTool_FilledRectangle
            RestoreLayer
            Draw_Rectangle_Filled m_lngStartMouseTileX, m_lngStartMouseTileY, m_lngMouseTileX, m_lngMouseTileY
            UndoPush m_rctActionArea
            Redraw
            RefreshBrushOverlay
        Case TileTool_FloodFill, TileTool_Replace
            Redraw
            RefreshBrushOverlay
        Case Else
        End Select
    End If
End Sub

Public Sub Tool_Up(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Tiles
        Tool_Tiles_Up Button, Shift, X, Y
    Case View_Blocking
        Tool_Blocking_Up Button, Shift, X, Y
    Case View_Areas
        Tool_Areas_Up Button, Shift, X, Y
    Case View_Sprites
        Tool_Sprites_Up Button, Shift, X, Y
    Case View_Lighting
        Tool_Lighting_Up Button, Shift, X, Y
    Case View_Objects
        Tool_Objects_Up Button, Shift, X, Y
    Case Else
    End Select
End Sub

Public Sub ToolChanged()
On Error Resume Next
Dim l_lngButtons As Long
Dim l_strTool As String
    If m_tbrToolbar Is Nothing Then
    Else
        For l_lngButtons = 1 To m_tbrToolbar.Buttons.Count
            l_strTool = LCase(Trim(m_tbrToolbar.Buttons(l_lngButtons).key))
            If Left(l_strTool, 4) = "tool" Then
                m_tbrToolbar.Buttons(l_lngButtons).Checked = (CLng(Mid(l_strTool, 6, Len(l_strTool) - 6)) = m_lngCurrentTool(m_lngCurrentView))
            End If
        Next l_lngButtons
    End If
    Screen.MousePointer = 11
    m_lngPoint = 0
    m_booDraggingSprite = False
    m_booDraggingLight = False
    m_booDraggingCollisionLines = False
    m_booDraggingLightingObstructions = False
    m_booDraggingLightingPlanes = False
    ResetOverlay
    tsTool.DisableUpdates = True
    tsTool.Tabs.Clear
    Select Case m_lngCurrentView
    Case View_Tiles
        Tiles_ToolChanged
    Case View_Blocking
        Blocking_ToolChanged
    Case View_Lighting
        Lighting_ToolChanged
    Case Else
    End Select
    tsTool.Tabs.AddNew "View", "View"
    tsTool.DisableUpdates = False
    tsTool.Reflow
    ToolInspectorChanged
    Redraw
    Screen.MousePointer = 0
End Sub

Public Sub ToolInspectorChanged()
On Error Resume Next
    m_ctlCurrentTool.Visible = False
    Select Case LCase(Trim(tsTool.SelectedTab.key))
    Case "view"
        Set m_ctlCurrentTool = insTool
        With insTool
            .Inspect m_voViewOptions, "View"
        End With
    Case "brush"
        Set m_ctlCurrentTool = picBrush
    Case Else
        Set m_ctlCurrentTool = Nothing
    End Select
    m_ctlCurrentTool.Visible = True
    tsTool_Resize
End Sub

Private Sub tpkTiles_SelectionChanged(ByRef Tiles() As Integer)
On Error Resume Next
    Set m_brsBrush = New Fury2Brush
    m_brsBrush.Resize UBound(Tiles, 1) + 1, UBound(Tiles, 2) + 1
    m_brsBrush.GrabFromArray Tiles
    RefreshBrush
End Sub

Private Sub tpkTiles_TileHover(Tile As Integer)
On Error Resume Next
    Editor.SetLocation "Tile " & Tile
End Sub

Public Function Undo() As Boolean
On Error Resume Next
Dim l_undRedo As iUndoEntry
Dim l_undUndo As iUndoEntry
    If m_colUndo.Count > 0 Then
        BeginProcess "Preparing to Undo..."
        Set l_undUndo = m_colUndo.Item(m_colUndo.Count)
        Set l_undRedo = l_undUndo.CreateReverse()
        m_colUndo.Remove m_colUndo.Count
        m_colRedo.Add l_undRedo, , 1
        UpdateProcess 0, "Performing Undo..."
        Undo = SmartApply(l_undUndo)
        EndProcess
        Editor.ActionUpdate
    End If
End Function

Public Sub UndoPush(Optional ByRef Area As Fury2Rect = Nothing, Optional ByVal Layer As Long = -1)
On Error Resume Next
Dim l_undUndo As cTileUndoEntry, l_brsBrush As Fury2Brush
    If Layer = -1 Then Layer = m_lngSelectedLayer
    If Area Is Nothing Then Set Area = F2Rect(0, 0, m_mapMap.Layers(Layer).Width, m_mapMap.Layers(Layer).Height, False)
    BeginProcess "Storing Undo Data..."
    Area.Rationalize
    If Area.Left < 0 Then
        Area.RelLeft = 0
    End If
    If Area.Top < 0 Then
        Area.RelTop = 0
    End If
    If Area.Right > m_mapMap.Layers(Layer).Width Then
        Area.Width = m_mapMap.Layers(Layer).Width - Area.Left
    End If
    If Area.Bottom > m_mapMap.Layers(Layer).Height Then
        Area.Height = m_mapMap.Layers(Layer).Height - Area.Top
    End If
    Set l_undUndo = New cTileUndoEntry
    With l_undUndo
        .X = Area.Left
        .Y = Area.Top
        .Layer = Layer
        Set l_brsBrush = New Fury2Brush
        With l_brsBrush
            .GrabFromArray m_intLayerCache, Area.Left, Area.Top, Area.Width, Area.Height, , , True
        End With
        Set .Brush = l_brsBrush
        Set .Map = m_mapMap
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub UpdateCamera()
On Error Resume Next
    If m_camCamera Is Nothing Then Set m_camCamera = DefaultEngine.F2Camera()
    With m_camCamera
        .DisableBuffer = True
        .ShowSprites = (m_lngCurrentView = View_Sprites) Or (m_voViewOptions.AlwaysShowSprites)
        .EnableLighting = (m_lngCurrentView = View_Lighting) Or (m_voViewOptions.AlwaysShowLighting)
        .EnableParallax = m_voViewOptions.EnableParallax
        .EnableWrapping = m_voViewOptions.EnableWrapping
        .ViewportX = hsMap.Value
        .ViewportY = vsMap.Value
        .Width = m_imgBackbuffer.Width
        .Height = m_imgBackbuffer.Height
        .SetMap m_mapMap
        .UnGhostLayer = IIf(elLayers.Toolbar.Buttons("GhostLayers").Checked, m_lngSelectedLayer, 0)
        .AutoTintLayers = elLayers.Toolbar.Buttons("TintLayers").Checked
        .Alpha = 1
    End With
End Sub

Public Sub ViewChanged()
On Error Resume Next
Dim l_objObject As Object
    Screen.MousePointer = 11
    ResetOverlay
    picSidebar.Visible = Not ((m_lngCurrentView = View_Script) Or (m_lngCurrentView = View_Properties))
    picMapView.Visible = picSidebar.Visible
    insMap.Visible = (m_lngCurrentView = View_Properties)
    scMap.Visible = (m_lngCurrentView = View_Script)
    tsLists.DisableUpdates = True
    With tsLists.Tabs
        .Clear
        Select Case m_lngCurrentView
        Case View_Tiles
            .AddNew "Layers", "Layers"
        Case View_Blocking
            .AddNew "Layers", "Layers"
        Case View_Lighting
            .AddNew "Layers", "Layers"
            .AddNew "Lights", "Lights"
        Case View_Areas
            .AddNew "Areas", "Areas"
        Case View_Sprites
            .AddNew "Layers", "Layers"
            .AddNew "Sprites", "Sprites"
        Case View_Objects
            .AddNew "Objects", "Objects"
        Case Else
        End Select
    End With
    tsLists.DisableUpdates = False
    tsLists.Reflow
    Select Case m_lngCurrentView
    Case View_Properties
        insMap.Inspect m_mapMap, "Map"
    Case View_Script
        scMap.Text = m_mapMap.ScriptSource
    Case Else
    End Select
    ListChanged
    m_splSidebar.Resize
    Redraw
    RefreshMapTools
    ResizeMapTools
    RefreshBrush
    RefreshTiles
    tsViews_Resize
    ToolChanged
    Screen.MousePointer = 0
End Sub

Public Property Get ViewOptions() As MapEditorViewOptions
On Error Resume Next
    Set ViewOptions = m_voViewOptions
End Property

Private Sub vsMap_Change()
On Error Resume Next
    If vsMap.Tag = "" Then Redraw
End Sub

Public Property Get Zoom() As Single
On Error Resume Next
    Zoom = m_voViewOptions.Zoom / 100#
End Property

Public Property Let Zoom(NewZoom As Single)
On Error Resume Next
    If (CLng(NewZoom * 100) <> m_voViewOptions.Zoom) Then
        m_voViewOptions.Zoom = CLng(NewZoom * 100)
        ZoomChanged
    End If
End Property

Public Sub ZoomChanged()
On Error Resume Next
    m_tbrToolbar.Buttons("ZoomIn").Enabled = (Zoom < 16)
    m_tbrToolbar.Buttons("ZoomOut").Enabled = (Zoom > 0.25)
    RefreshToolInspector
    ResizeViewport
    Redraw
End Sub

