VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ImageGridRemoverOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements IVirtualFileSaveable
Implements IInspectable
Private mvarGridWidth As Long
Private mvarGridHeight As Long
Private mvarCellWidth As Long
Private mvarCellHeight As Long
Private mvarMarginLeft As Long
Private mvarMarginTop As Long
Private mvarMarginRight As Long
Private mvarMarginBottom As Long
Private mvarImage As String
Private mvarSliceImages As Boolean

Public Property Get SliceImages() As Boolean
    SliceImages = mvarSliceImages
End Property

Public Property Let SliceImages(ByVal vData As Boolean)
    mvarSliceImages = vData
End Property

Public Property Let Image(ByVal vData As String)
Attribute Image.VB_Description = "The filename of the image.{ImagePath}"
    mvarImage = vData
End Property

Public Property Get Image() As String
    Image = mvarImage
End Property

Public Property Let MarginBottom(ByVal vData As Long)
Attribute MarginBottom.VB_Description = "The number of additional pixels on the bottom edge of the image."
    mvarMarginBottom = vData
End Property

Public Property Get MarginBottom() As Long
    MarginBottom = mvarMarginBottom
End Property

Public Property Let MarginRight(ByVal vData As Long)
Attribute MarginRight.VB_Description = "The number of additional pixels on the right edge of the image."
    mvarMarginRight = vData
End Property

Public Property Get MarginRight() As Long
    MarginRight = mvarMarginRight
End Property

Public Property Let MarginTop(ByVal vData As Long)
Attribute MarginTop.VB_Description = "The number of additional pixels on the top edge of the image."
    mvarMarginTop = vData
End Property

Public Property Get MarginTop() As Long
    MarginTop = mvarMarginTop
End Property

Public Property Let MarginLeft(ByVal vData As Long)
Attribute MarginLeft.VB_Description = "The number of additional pixels on the left edge of the image."
    mvarMarginLeft = vData
End Property

Public Property Get MarginLeft() As Long
    MarginLeft = mvarMarginLeft
End Property

Public Property Let CellHeight(ByVal vData As Long)
Attribute CellHeight.VB_Description = "The height of each cell in pixels, not including the grid."
    mvarCellHeight = vData
End Property

Public Property Get CellHeight() As Long
    CellHeight = mvarCellHeight
End Property

Public Property Let CellWidth(ByVal vData As Long)
Attribute CellWidth.VB_Description = "The width of each cell in pixels, not including the grid."
    mvarCellWidth = vData
End Property

Public Property Get CellWidth() As Long
    CellWidth = mvarCellWidth
End Property

Public Property Let GridHeight(ByVal vData As Long)
Attribute GridHeight.VB_Description = "The height of each grid line in pixels."
    mvarGridHeight = vData
End Property

Public Property Get GridHeight() As Long
    GridHeight = mvarGridHeight
End Property

Public Property Let GridWidth(ByVal vData As Long)
Attribute GridWidth.VB_Description = "The width of each grid line in pixels."
    mvarGridWidth = vData
End Property

Public Property Get GridWidth() As Long
    GridWidth = mvarGridWidth
End Property

Private Sub Class_Initialize()
    GridWidth = 1
    GridHeight = 1
    CellWidth = 16
    CellHeight = 16
    MarginLeft = 1
    MarginTop = 1
    MarginRight = 1
    MarginBottom = 1
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load mvarGridWidth
        .Load mvarGridHeight
        .Load mvarCellWidth
        .Load mvarCellHeight
        .Load mvarMarginLeft
        .Load mvarMarginTop
        .Load mvarMarginRight
        .Load mvarMarginBottom
        .Load mvarSliceImages
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save mvarGridWidth
        .Save mvarGridHeight
        .Save mvarCellWidth
        .Save mvarCellHeight
        .Save mvarMarginLeft
        .Save mvarMarginTop
        .Save mvarMarginRight
        .Save mvarMarginBottom
        .Save mvarSliceImages
    End With
End Sub

