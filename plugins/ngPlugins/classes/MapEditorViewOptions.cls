VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "MapEditorViewOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements IInspectable
Implements IVirtualFileSaveable
Private m_lngBackgroundColor As Long
Private m_lngLightingObstructionColor As Long
Private m_lngLightingPlaneColor As Long
Private m_lngBlockingColor As Long
Private m_lngPathColor As Long
Private m_lngPathNodeColor As Long
Private m_lngZoom As Long
Private m_booAutoScroll As Boolean
Private m_booAlwaysShowLighting As Boolean
Private m_booShowGrid As Boolean
Private m_booSnapToGrid As Boolean
Private m_booEnableWrapping As Boolean
Private m_booEnableParallax As Boolean
Private m_booPreserveTilesetRows As Boolean

Public Property Get AlwaysShowLighting() As Boolean
    AlwaysShowLighting = m_booAlwaysShowLighting
End Property

Public Property Let AlwaysShowLighting(ByVal NewValue As Boolean)
    m_booAlwaysShowLighting = NewValue
End Property

Public Property Get PreserveTilesetRows() As Boolean
    PreserveTilesetRows = m_booPreserveTilesetRows
End Property

Public Property Let PreserveTilesetRows(NewValue As Boolean)
    m_booPreserveTilesetRows = NewValue
End Property

Public Property Get EnableWrapping() As Boolean
    EnableWrapping = m_booEnableWrapping
End Property

Public Property Let EnableWrapping(NewValue As Boolean)
    m_booEnableWrapping = NewValue
End Property

Public Property Get EnableParallax() As Boolean
    EnableParallax = m_booEnableParallax
End Property

Public Property Let EnableParallax(NewValue As Boolean)
    m_booEnableParallax = NewValue
End Property

Public Property Get AutoScroll() As Boolean
    AutoScroll = m_booAutoScroll
End Property

Public Property Let AutoScroll(NewValue As Boolean)
    m_booAutoScroll = NewValue
End Property

Public Property Get ShowGrid() As Boolean
    ShowGrid = m_booShowGrid
End Property

Public Property Let ShowGrid(NewValue As Boolean)
    m_booShowGrid = NewValue
End Property

Public Property Get SnapToGrid() As Boolean
    SnapToGrid = m_booSnapToGrid
End Property

Public Property Let SnapToGrid(NewValue As Boolean)
    m_booSnapToGrid = NewValue
End Property

Public Property Get Zoom() As Long
Attribute Zoom.VB_Description = "Percentage to zoom the map editor view by."
    Zoom = m_lngZoom
End Property

Public Property Let Zoom(NewZoom As Long)
    m_lngZoom = NewZoom
End Property

Public Property Get LightingObstructionColor() As Long
Attribute LightingObstructionColor.VB_Description = "The color to use when drawing lighting obstructions.{Color}"
    LightingObstructionColor = m_lngLightingObstructionColor
End Property

Public Property Let LightingObstructionColor(NewColor As Long)
    m_lngLightingObstructionColor = NewColor
End Property

Public Property Get LightingPlaneColor() As Long
Attribute LightingPlaneColor.VB_Description = "The color to use when drawing lighting planes.{Color}"
    LightingPlaneColor = m_lngLightingPlaneColor
End Property

Public Property Let LightingPlaneColor(NewColor As Long)
    m_lngLightingPlaneColor = NewColor
End Property

Public Property Get BlockingColor() As Long
Attribute BlockingColor.VB_Description = "The color to use when drawing blocking obstructions.{Color}"
    BlockingColor = m_lngBlockingColor
End Property

Public Property Let BlockingColor(NewColor As Long)
    m_lngBlockingColor = NewColor
End Property

Public Property Get PathNodeColor() As Long
Attribute PathNodeColor.VB_Description = "{Color}"
    PathNodeColor = m_lngPathNodeColor
End Property

Public Property Let PathNodeColor(NewColor As Long)
    m_lngPathNodeColor = NewColor
End Property

Public Property Get PathColor() As Long
Attribute PathColor.VB_Description = "{Color}"
    PathColor = m_lngPathColor
End Property

Public Property Let PathColor(NewColor As Long)
    m_lngPathColor = NewColor
End Property

Public Property Get BackgroundColor() As Long
Attribute BackgroundColor.VB_Description = "The color to draw behind the map view.{Color}"
    BackgroundColor = m_lngBackgroundColor
End Property

Public Property Let BackgroundColor(NewColor As Long)
    m_lngBackgroundColor = NewColor
End Property

Private Sub Class_Initialize()
On Error Resume Next
    m_booAutoScroll = True
    m_lngZoom = 100
    m_lngBlockingColor = F2RGB(255, 0, 127, 220)
    m_lngLightingObstructionColor = F2RGB(63, 220, 63, 220)
    m_lngLightingPlaneColor = F2RGB(63, 63, 220, 220)
    m_lngPathColor = F2RGB(192, 64, 0, 160)
    m_lngPathNodeColor = F2RGB(240, 96, 16, 220)
    m_lngBackgroundColor = F2Black
    m_booEnableWrapping = False
    m_booEnableParallax = False
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load m_lngBackgroundColor
        .Load m_lngLightingObstructionColor
        .Load m_lngLightingPlaneColor
        .Load m_lngBlockingColor
        .Load m_lngZoom
        .Load m_booAutoScroll
        .Load m_booShowGrid
        .Load m_booSnapToGrid
        .Load m_booEnableWrapping
        .Load m_booEnableParallax
        .Load m_booPreserveTilesetRows
        .Load m_lngPathColor
        .Load m_lngPathNodeColor
        .Load m_booAlwaysShowLighting
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save m_lngBackgroundColor
        .Save m_lngLightingObstructionColor
        .Save m_lngLightingPlaneColor
        .Save m_lngBlockingColor
        .Save m_lngZoom
        .Save m_booAutoScroll
        .Save m_booShowGrid
        .Save m_booSnapToGrid
        .Save m_booEnableWrapping
        .Save m_booEnableParallax
        .Save m_booPreserveTilesetRows
        .Save m_lngPathColor
        .Save m_lngPathNodeColor
        .Save m_booAlwaysShowLighting
    End With
End Sub
