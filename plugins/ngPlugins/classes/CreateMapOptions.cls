VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CreateMapOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Implements IVirtualFileSaveable
Implements IInspectable

Private mvarLayers As Long
Private mvarWidth As Long
Private mvarHeight As Long
Private mvarTileset As String
Private mvarTileWidth As Long
Private mvarTileHeight As Long
Private mvarLayerEffect As SFXBlitModes
Private mvarCollisionType As Fury2CollisionTypes
Private mvarName As String

Public Property Let Name(ByVal vData As String)
Attribute Name.VB_Description = "The name of the new map."
    mvarName = vData
End Property

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Let CollisionType(ByVal vData As Fury2CollisionTypes)
Attribute CollisionType.VB_Description = "The map's collision type."
    mvarCollisionType = vData
End Property

Public Property Get CollisionType() As Fury2CollisionTypes
    CollisionType = mvarCollisionType
End Property

Public Property Let LayerEffect(ByVal vData As SFXBlitModes)
Attribute LayerEffect.VB_Description = "The rendering effect to use on the map's layers."
    mvarLayerEffect = vData
End Property

Public Property Get LayerEffect() As SFXBlitModes
    LayerEffect = mvarLayerEffect
End Property

Public Property Let TileHeight(ByVal vData As Long)
Attribute TileHeight.VB_Description = "The height of the tiles in the tileset."
    mvarTileHeight = vData
End Property

Public Property Get TileHeight() As Long
    TileHeight = mvarTileHeight
End Property

Public Property Let TileWidth(ByVal vData As Long)
Attribute TileWidth.VB_Description = "The width of the tiles in the tileset."
    mvarTileWidth = vData
End Property

Public Property Get TileWidth() As Long
    TileWidth = mvarTileWidth
End Property

Public Property Let Tileset(ByVal vData As String)
Attribute Tileset.VB_Description = "The filename of the tileset to use.{ImageFilename}"
    mvarTileset = vData
End Property

Public Property Get Tileset() As String
    Tileset = mvarTileset
End Property

Public Property Let Height(ByVal vData As Long)
Attribute Height.VB_Description = "The height of the map's layers."
    mvarHeight = vData
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Public Property Let Width(ByVal vData As Long)
Attribute Width.VB_Description = "The width of the map's layers."
    mvarWidth = vData
End Property

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Let Layers(ByVal vData As Long)
Attribute Layers.VB_Description = "The number of layers to create."
    mvarLayers = vData
End Property

Public Property Get Layers() As Long
    Layers = mvarLayers
End Property

Private Sub Class_Initialize()
    mvarLayers = 2
    mvarWidth = 20
    mvarHeight = 20
    mvarTileset = ""
    mvarTileWidth = 16
    mvarTileHeight = 16
    mvarLayerEffect = BlitMode_Normal
    mvarCollisionType = CT_Line
    mvarName = "Untitled Map"
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
    With File
        .Load mvarLayers
        .Load mvarWidth
        .Load mvarHeight
        .Load mvarTileset
        .Load mvarTileWidth
        .Load mvarTileHeight
        .Load mvarLayerEffect
        .Load mvarCollisionType
        .Load mvarName
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
    With File
        .Save mvarLayers
        .Save mvarWidth
        .Save mvarHeight
        .Save mvarTileset
        .Save mvarTileWidth
        .Save mvarTileHeight
        .Save mvarLayerEffect
        .Save mvarCollisionType
        .Save mvarName
    End With
End Sub
