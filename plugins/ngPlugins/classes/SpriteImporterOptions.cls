VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SpriteImporterOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements IVirtualFileSaveable
Implements IInspectable
Private mvarCellWidth As Long
Private mvarCellHeight As Long
Private mvarAlignmentX As Single
Private mvarAlignmentY As Single
Private mvarFrameDelay As Long
Private mvarImage As String

Public Property Let Image(ByVal vData As String)
Attribute Image.VB_Description = "The filename of the image.{ImagePath}"
    mvarImage = vData
End Property

Public Property Get Image() As String
    Image = mvarImage
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

Public Property Let AlignmentX(ByVal vData As Single)
    mvarAlignmentX = vData
End Property

Public Property Get AlignmentX() As Single
    AlignmentX = mvarAlignmentX
End Property

Public Property Let AlignmentY(ByVal vData As Single)
    mvarAlignmentY = vData
End Property

Public Property Get AlignmentY() As Single
    AlignmentY = mvarAlignmentY
End Property

Public Property Let FrameDelay(ByVal vData As Long)
    mvarFrameDelay = vData
End Property

Public Property Get FrameDelay() As Long
    FrameDelay = mvarFrameDelay
End Property

Private Sub Class_Initialize()
    CellWidth = 16
    CellHeight = 32
    FrameDelay = 5
    AlignmentX = 7.5
    AlignmentY = 31
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load mvarCellWidth
        .Load mvarCellHeight
        .Load mvarFrameDelay
        .Load mvarAlignmentX
        .Load mvarAlignmentY
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save mvarCellWidth
        .Save mvarCellHeight
        .Save mvarFrameDelay
        .Save mvarAlignmentX
        .Save mvarAlignmentY
    End With
End Sub

