VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2MapLayer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Map Layer Object"
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
Implements IVirtualFileSaveable

Private mvarWidth As Long
Private mvarHeight As Long
Private mvarTintColor As Long
Private mvarMatrix As Long
Private mvarDepthmap As String
Private mvarDepthmapImage As Fury2Image
Private mvarDepthmapXRatio As Single, mvarDepthmapYRatio As Single
Private mvarName As String
Private mvarDisableBlocking As Boolean
Private mvarBlockGridWidth As Long
Private mvarBlockGridHeight As Long
Private mvarPreRendered As Boolean
Private mvarParallaxX As Single
Private mvarParallaxY As Single
Private mvarVisible As Boolean
Private mvarAlpha As Single
Private mvarEffect As SFXBlitModes
Private mvarLit As Boolean
Private mvarTiles() As Integer
Private mvarBlocking() As Byte
Private mvarBlockingLines() As FLine
Private mvarSprites As Fury2Sprites
Private mvarTileset As Fury2Tileset
Private mvarAnimations As Fury2TileAnimations
Private mvarLighting As Fury2LayerLighting
Private mvarWrapX As Boolean
Private mvarWrapY As Boolean
Private mvarParent As Fury2Map

Public Property Set Parent(NewParent As Fury2Map)
On Error Resume Next
    Set mvarParent = NewParent
End Property

Public Property Get Parent() As Fury2Map
    Set Parent = mvarParent
End Property

Public Property Let Height(NewHeight As Long)
    If NewHeight <> mvarHeight Then
        ResizePreserve mvarWidth, NewHeight
    End If
End Property

Public Property Get Height() As Long
    Height = mvarHeight
End Property

Public Property Let Width(NewWidth As Long)
    If NewWidth <> mvarWidth Then
        ResizePreserve NewWidth, mvarHeight
    End If
End Property

Public Property Get Width() As Long
    Width = mvarWidth
End Property

Public Property Get Lighting() As Fury2LayerLighting
Attribute Lighting.VB_Description = "~The layer's lighting data."
    Set Lighting = mvarLighting
End Property

Public Sub CreateCollisionMatrix(Width As Long, Height As Long)
On Error Resume Next
    DeallocateCollisionMatrix
    mvarMatrix = SoftFX.CreateCollisionMatrix(Width, Height)
    If mvarMatrix <> 0 Then
        AppendLinesToCollisionMatrix mvarMatrix, mvarBlockingLines(0), (UBound(mvarBlockingLines) - LBound(mvarBlockingLines)) + 1
    End If
End Sub

Public Sub DeallocateCollisionMatrix()
On Error Resume Next
    If mvarMatrix <> 0 Then
        DeleteCollisionMatrix mvarMatrix
        mvarMatrix = 0
    End If
End Sub

Public Sub RenderBlocking(Surface, Optional ByVal XOffset As Single = 0, Optional ByVal YOffset As Single = 0)
On Error Resume Next
    SoftFX.RenderCollisionLines Surface.Handle, VarPtr(mvarBlockingLines(LBound(mvarBlockingLines))), UBound(mvarBlockingLines) - LBound(mvarBlockingLines) + 1, XOffset, YOffset
End Sub

Function DepthmapRead(X As Single, Y As Single) As Single
On Error Resume Next
Dim m_sngX As Single, m_sngY As Single
Dim m_lngColor As Long
    If mvarDepthmapImage Is Nothing Then Exit Function
    If Trim(mvarDepthmap) = "" Then Exit Function
    m_sngX = X * mvarDepthmapXRatio
    m_sngY = Y * mvarDepthmapYRatio
    m_lngColor = GetRed(mvarDepthmapImage.GetPixelAA(m_sngX, m_sngY))
    If m_lngColor = 127 Then
        DepthmapRead = 1
    ElseIf m_lngColor >= 255 Then
        DepthmapRead = 2
    ElseIf m_lngColor <= 0 Then
        DepthmapRead = 0
    Else
        DepthmapRead = CSng(m_lngColor) / 127.5
    End If
End Function

Sub UpdateDepthmap()
On Error Resume Next
    If Not m_booEditor Then
        If Trim(mvarDepthmap) = "" Then
            Set mvarDepthmapImage = Nothing
        Else
            Set mvarDepthmapImage = m_Engine.LoadImage(mvarDepthmap)
        End If
    End If
End Sub

Public Property Get HasDepthmap() As Boolean
    HasDepthmap = Trim(mvarDepthmap) <> ""
End Property

Public Property Get Depthmap() As String
    Depthmap = mvarDepthmap
End Property

Public Property Let Depthmap(NewFilename As String)
    mvarDepthmap = NewFilename
    UpdateDepthmap
End Property

Public Function CollisionLinePointer(Index As Long) As Long
Attribute CollisionLinePointer.VB_Description = "*"
On Error Resume Next
    CollisionLinePointer = VarPtr(mvarBlockingLines(Index))
End Function

Public Function CollisionLineCount() As Long
Attribute CollisionLineCount.VB_Description = "*"
On Error Resume Next
    CollisionLineCount = (UBound(mvarBlockingLines) - LBound(mvarBlockingLines)) + 1
End Function

Public Sub RemoveCollisionLine(Index As Long)
On Error Resume Next
Dim m_lngLines As Long
    If Index < 0 Then Exit Sub
    If Index > UBound(mvarBlockingLines) Then Exit Sub
    mvarBlockingLines(Index).Start.X = -1
    mvarBlockingLines(Index).Start.Y = -1
    mvarBlockingLines(Index).end.X = -1
    mvarBlockingLines(Index).end.Y = -1
    If Index = UBound(mvarBlockingLines) Then
    Else
        For m_lngLines = Index + 1 To UBound(mvarBlockingLines)
            mvarBlockingLines(m_lngLines - 1) = mvarBlockingLines(m_lngLines)
        Next m_lngLines
    End If
    ReDim Preserve mvarBlockingLines(LBound(mvarBlockingLines) To UBound(mvarBlockingLines) - 1)
End Sub

Public Sub AddCollisionLine(x1, y1, x2, y2)
On Error Resume Next
    ReDim Preserve mvarBlockingLines(LBound(mvarBlockingLines) To UBound(mvarBlockingLines) + 1)
    With mvarBlockingLines(UBound(mvarBlockingLines))
        .Start.X = CSng(x1)
        .Start.Y = CSng(y1)
        .end.X = CSng(x2)
        .end.Y = CSng(y2)
    End With
End Sub

Sub ClearCollisionLines()
On Error Resume Next
    ReDim mvarBlockingLines(0 To 0)
End Sub

Public Property Get CollisionLines() As FLine()
Attribute CollisionLines.VB_Description = "*"
On Error Resume Next
    CollisionLines = mvarBlockingLines
End Property

Public Property Let CollisionLines(NewLines() As FLine)
On Error Resume Next
    mvarBlockingLines() = NewLines()
End Property

Public Function GetCollisionLine(Index) As Fury2Rect
On Error Resume Next
    With mvarBlockingLines(Index)
        Set GetCollisionLine = F2Rect(.Start.X, .Start.Y, .end.X, .end.Y)
    End With
End Function

Public Function SetCollisionLine(Index, NewLine)
On Error Resume Next
    With mvarBlockingLines(Index)
        .Start.X = NewLine.Left
        .Start.Y = NewLine.Top
        .end.X = NewLine.Right
        .end.Y = NewLine.Bottom
    End With
End Function

Sub Free()
Attribute Free.VB_MemberFlags = "40"
On Error Resume Next
    Erase mvarBlocking
    Erase mvarTiles
    Set mvarAnimations = New Fury2TileAnimations
    Set mvarDepthmapImage = Nothing
    mvarSprites.Free
    Set mvarSprites = New Fury2Sprites
    mvarTileset.Free
    Set mvarTileset = New Fury2Tileset
End Sub

Public Property Get DisableBlocking() As Boolean
Attribute DisableBlocking.VB_Description = "This flag disables loading, saving, and manipulation of this layer's blocking data."
    DisableBlocking = mvarDisableBlocking
End Property

Public Property Let DisableBlocking(NewDisable As Boolean)
    mvarDisableBlocking = NewDisable
    If NewDisable = True Then
        ReDim mvarBlocking(0 To 0, 0 To 0)
    Else
        ReDim mvarBlocking(0 To (((UBound(mvarTiles, 1) + 1) * mvarTileset.TileWidth) \ mvarBlockGridWidth) - 1, 0 To (((UBound(mvarTiles, 2) + 1) * mvarTileset.TileHeight) \ mvarBlockGridHeight) - 1)
    End If
End Property

Public Property Get Name() As String
Attribute Name.VB_Description = "The layer's name."
    Name = mvarName
End Property

Public Property Let Name(NewName As String)
    mvarName = NewName
End Property

Sub ResizeBlocking()
Attribute ResizeBlocking.VB_MemberFlags = "40"
On Error Resume Next
Dim mvarCopyRows As Long, m_lngRowCount As Long, mvarOldBlocking() As Byte
Dim m_lngWidth As Long, m_lngHeight As Long
    m_lngWidth = UBound(mvarTiles, 1) + 1
    m_lngHeight = UBound(mvarTiles, 2) + 1
    Err.Clear
    mvarOldBlocking() = mvarBlocking()
    ReDim mvarBlocking(0 To ((m_lngWidth * mvarTileset.TileWidth) \ mvarBlockGridWidth) - 1, 0 To ((m_lngHeight * mvarTileset.TileHeight) \ mvarBlockGridHeight) - 1)
    m_lngRowCount = UBound(mvarOldBlocking, 2)
    If m_lngRowCount > UBound(mvarBlocking, 2) Then m_lngRowCount = UBound(mvarBlocking, 2)
    For mvarCopyRows = 0 To m_lngRowCount
        CopyMemory mvarBlocking(0, mvarCopyRows), mvarOldBlocking(0, mvarCopyRows), ClipValue((UBound(mvarOldBlocking, 1) + 1), 0, UBound(mvarBlocking, 1))
    Next mvarCopyRows
    Err.Clear
End Sub

Public Property Get BlockingGridWidth() As Long
Attribute BlockingGridWidth.VB_Description = "The width (in pixels) of each cell in the layer's blocking grid."
    BlockingGridWidth = mvarBlockGridWidth
End Property

Public Property Let BlockingGridWidth(NewWidth As Long)
    If NewWidth < 1 Then
        mvarBlockGridWidth = mvarTileset.TileWidth
    Else
        mvarBlockGridWidth = NewWidth
    End If
End Property

Public Property Get BlockingGridHeight() As Long
Attribute BlockingGridHeight.VB_Description = "The height (in pixels) of each cell in the layer's blocking grid."
    BlockingGridHeight = mvarBlockGridHeight
End Property

Public Property Let BlockingGridHeight(NewHeight As Long)
    If NewHeight < 1 Then
        mvarBlockGridHeight = mvarTileset.TileHeight
    Else
        mvarBlockGridHeight = NewHeight
    End If
End Property

Public Function TileGravity(Optional Column = -1, Optional EmptyTile = -1) As Boolean
Attribute TileGravity.VB_MemberFlags = "40"
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
Dim m_lngEmpty As Long, m_lngLoops As Long
Dim m_booFell As Boolean
    Err.Clear
    m_lngX = CLng(Column)
    If m_lngX < 0 Then Exit Function
    If m_lngX > UBound(mvarTiles, 1) Then Exit Function
    m_lngEmpty = CLng(EmptyTile)
    If m_lngX = -1 Then
        For m_lngX = 0 To UBound(mvarTiles, 1)
            For m_lngY = 0 To UBound(mvarTiles, 2) - 1
                If (mvarTiles(m_lngX, m_lngY + 1) = EmptyTile) And (mvarTiles(m_lngX, m_lngY) <> EmptyTile) Then
                    mvarTiles(m_lngX, m_lngY + 1) = mvarTiles(m_lngX, m_lngY)
                    mvarTiles(m_lngX, m_lngY) = EmptyTile
                    m_booFell = m_booFell Or True
                    m_lngY = 0
                End If
            Next m_lngY
        Next m_lngX
    Else
        For m_lngY = 0 To UBound(mvarTiles, 2) - 1
            If (mvarTiles(m_lngX, m_lngY + 1) = EmptyTile) And (mvarTiles(m_lngX, m_lngY) <> EmptyTile) Then
                mvarTiles(m_lngX, m_lngY + 1) = mvarTiles(m_lngX, m_lngY)
                mvarTiles(m_lngX, m_lngY) = EmptyTile
                m_booFell = m_booFell Or True
                m_lngY = 0
            End If
        Next m_lngY
    End If
    TileGravity = m_booFell
End Function

Public Sub FloodFill(X, Y, Tile)
Attribute FloodFill.VB_Description = "Flood-fills the layer's tile data."
On Error Resume Next
Dim isSeen() As Byte, isDrawn() As Byte
Dim xPosition As New Collection, yPosition As New Collection
Dim sTile As Integer, nTile As Integer
Dim NextX As Long, NextY As Long, deltaX As Long, deltaY As Long, CurrentX As Long, CurrentY As Long
    If (X < 0) Or (Y < 0) Or (X > UBound(mvarTiles, 1)) Or (Y > UBound(mvarTiles, 2)) Then Err.Raise 10241, "FloodFill", "Out of bounds": Exit Sub
    ReDim isSeen(0 To UBound(mvarTiles, 1), 0 To UBound(mvarTiles, 2))
    isDrawn = isSeen
    sTile = mvarTiles(X, Y)
    nTile = Int(Tile)
    xPosition.Add X
    yPosition.Add Y
    isSeen(X, Y) = 1
    Do Until xPosition.Count <= 0
        CurrentX = CLng(xPosition.Item(xPosition.Count))
        CurrentY = CLng(yPosition.Item(yPosition.Count))
        ProgressUpdate (Height / CurrentY) * 100
        xPosition.Remove xPosition.Count
        yPosition.Remove yPosition.Count
        If isDrawn(CurrentX, CurrentY) = 1 Then Else mvarTiles(CurrentX, CurrentY) = nTile
        isDrawn(CurrentX, CurrentY) = 1
        For deltaX = -1 To 1
            NextX = CurrentX + deltaX
            If (NextX >= 0) And (NextX <= UBound(mvarTiles, 1)) Then
                For deltaY = -1 To 1
                    NextY = CurrentY + deltaY
                    If (NextY >= 0) And (NextY <= UBound(mvarTiles, 2)) Then
                        If mvarTiles(NextX, NextY) <> sTile Then
                        Else
                            If isSeen(NextX, NextY) = 0 Then
                                xPosition.Add NextX
                                yPosition.Add NextY
                                isSeen(NextX, NextY) = 1
                            End If
                        End If
                    End If
                Next deltaY
            End If
        Next deltaX
    Loop
    ProgressHide
End Sub

Public Sub PatternFloodFill(X, Y, Pattern() As Integer)
On Error Resume Next
Dim isSeen() As Byte, isDrawn() As Byte
Dim xPosition As New Collection, yPosition As New Collection
Dim sTile As Integer, nTile As Integer
Dim NextX As Long, NextY As Long, deltaX As Long, deltaY As Long, CurrentX As Long, CurrentY As Long
Dim m_lngXMod As Long, m_lngYMod As Long
    If (UBound(Pattern, 1) = LBound(Pattern, 1)) And (UBound(Pattern, 2) = LBound(Pattern, 2)) Then FloodFill X, Y, Pattern(0, 0): Exit Sub
    m_lngXMod = UBound(Pattern, 1) + 1
    m_lngYMod = UBound(Pattern, 2) + 1
    If (X < 0) Or (Y < 0) Or (X > UBound(mvarTiles, 1)) Or (Y > UBound(mvarTiles, 2)) Then Err.Raise 10241, "FloodFill", "Out of bounds": Exit Sub
    ReDim isSeen(0 To UBound(mvarTiles, 1), 0 To UBound(mvarTiles, 2))
    isDrawn = isSeen
    sTile = mvarTiles(X, Y)
    xPosition.Add X
    yPosition.Add Y
    isSeen(X, Y) = 1
    Do Until xPosition.Count <= 0
        CurrentX = CLng(xPosition.Item(xPosition.Count))
        CurrentY = CLng(yPosition.Item(yPosition.Count))
        ProgressUpdate (Height / CurrentY) * 100
        xPosition.Remove xPosition.Count
        yPosition.Remove yPosition.Count
        If isDrawn(CurrentX, CurrentY) = 1 Then Else mvarTiles(CurrentX, CurrentY) = Pattern(Abs(CurrentX - X) Mod m_lngXMod, Abs(CurrentY - Y) Mod m_lngYMod)
        isDrawn(CurrentX, CurrentY) = 1
        For deltaX = -1 To 1
            NextX = CurrentX + deltaX
            If (NextX >= 0) And (NextX <= UBound(mvarTiles, 1)) Then
                For deltaY = -1 To 1
                    NextY = CurrentY + deltaY
                    If (NextY >= 0) And (NextY <= UBound(mvarTiles, 2)) Then
                        If mvarTiles(NextX, NextY) <> sTile Then
                        Else
                            If isSeen(NextX, NextY) = 0 Then
                                xPosition.Add NextX
                                yPosition.Add NextY
                                isSeen(NextX, NextY) = 1
                            End If
                        End If
                    End If
                Next deltaY
            End If
        Next deltaX
    Loop
    ProgressHide
End Sub

Public Sub PatternReplace(Tile, Pattern() As Integer)
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
Dim m_lngXMod As Long, m_lngYMod As Long
Dim m_intTile As Integer
    m_intTile = CInt(Tile)
    m_lngXMod = UBound(Pattern, 1) + 1
    m_lngYMod = UBound(Pattern, 2) + 1
    If (UBound(Pattern, 1) = LBound(Pattern, 1)) And (UBound(Pattern, 2) = LBound(Pattern, 2)) Then
        For m_lngY = 0 To UBound(mvarTiles, 2)
            ProgressUpdate (Height / m_lngY) * 100
            For m_lngX = 0 To UBound(mvarTiles, 1)
                If mvarTiles(m_lngX, m_lngY) = m_intTile Then
                    mvarTiles(m_lngX, m_lngY) = Pattern(0, 0)
                End If
            Next m_lngX
        Next m_lngY
    ElseIf (UBound(Pattern, 1) = LBound(Pattern, 1)) Then
        For m_lngY = 0 To UBound(mvarTiles, 2)
            ProgressUpdate (Height / m_lngY) * 100
            For m_lngX = 0 To UBound(mvarTiles, 1)
                If mvarTiles(m_lngX, m_lngY) = m_intTile Then
                    mvarTiles(m_lngX, m_lngY) = Pattern(0, m_lngY Mod m_lngYMod)
                End If
            Next m_lngX
        Next m_lngY
    ElseIf (UBound(Pattern, 2) = LBound(Pattern, 2)) Then
        For m_lngY = 0 To UBound(mvarTiles, 2)
            ProgressUpdate (Height / m_lngY) * 100
            For m_lngX = 0 To UBound(mvarTiles, 1)
                If mvarTiles(m_lngX, m_lngY) = m_intTile Then
                    mvarTiles(m_lngX, m_lngY) = Pattern(m_lngX Mod m_lngXMod, 0)
                End If
            Next m_lngX
        Next m_lngY
    Else
        For m_lngY = 0 To UBound(mvarTiles, 2)
            ProgressUpdate (Height / m_lngY) * 100
            For m_lngX = 0 To UBound(mvarTiles, 1)
                If mvarTiles(m_lngX, m_lngY) = m_intTile Then
                    mvarTiles(m_lngX, m_lngY) = Pattern(m_lngX Mod m_lngXMod, m_lngY Mod m_lngYMod)
                End If
            Next m_lngX
        Next m_lngY
    End If
    ProgressHide
End Sub

Public Sub Replace(Tile, NewTile)
Attribute Replace.VB_Description = "Replaces one tile value with another."
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
Dim m_intTile As Integer, m_intNewTile As Integer
    m_intTile = CInt(Tile)
    m_intNewTile = CInt(NewTile)
    For m_lngY = 0 To UBound(mvarTiles, 2)
        For m_lngX = 0 To UBound(mvarTiles, 1)
            ProgressUpdate (Height / m_lngY) * 100
            If mvarTiles(m_lngX, m_lngY) = m_intTile Then mvarTiles(m_lngX, m_lngY) = m_intNewTile
        Next m_lngX
    Next m_lngY
    ProgressHide
    Err.Clear
End Sub

Public Sub RandomReplace(Tile, ParamArray Replacements())
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
Dim m_lngCount As Long
Dim m_intTile As Integer
    m_intTile = CInt(Tile)
    m_lngCount = (UBound(Replacements) - LBound(Replacements)) + 1
    For m_lngY = 0 To UBound(mvarTiles, 2)
        For m_lngX = 0 To UBound(mvarTiles, 1)
            If mvarTiles(m_lngX, m_lngY) = m_intTile Then
                mvarTiles(m_lngX, m_lngY) = Replacements(Int(Rnd * m_lngCount))
            End If
        Next m_lngX
    Next m_lngY
    Err.Clear
End Sub

Public Function TileCount(Tile)
Attribute TileCount.VB_Description = "*Counts the number of tiles in the layer."
On Error Resume Next
Dim m_intTile As Integer
Dim m_lngX As Long, m_lngY As Long
Dim m_lngCount As Long
    m_intTile = CInt(Tile)
    For m_lngY = 0 To UBound(mvarTiles, 2)
        For m_lngX = 0 To UBound(mvarTiles, 1)
            If mvarTiles(m_lngX, m_lngY) = m_intTile Then
                m_lngCount = m_lngCount + 1
            End If
        Next m_lngX
    Next m_lngY
    TileCount = m_lngCount
End Function

Public Function TileArray(StartX, StartY, EndX, EndY) As Integer()
Attribute TileArray.VB_Description = "*Returns the layer's tile values as an array."
Dim m_lngStartX As Long, m_lngStartY As Long
Dim m_lngEndX As Long, m_lngEndY As Long
Dim m_intTiles() As Integer
Dim m_lngRows As Long
    m_lngStartX = ClipValue(CLng(StartX), 0, UBound(mvarTiles, 1) - 1)
    m_lngStartY = ClipValue(CLng(StartY), 0, UBound(mvarTiles, 2) - 1)
    m_lngEndX = ClipValue(CLng(EndX), 0, UBound(mvarTiles, 1) - 1)
    m_lngEndY = ClipValue(CLng(EndY), 0, UBound(mvarTiles, 2) - 1)
    ReDim m_intTiles(0 To m_lngEndX - m_lngStartX, 0 To m_lngEndY - m_lngStartY)
    For m_lngRows = m_lngStartY To m_lngEndY
        CopyMemory m_intTiles(0, m_lngRows - m_lngStartY), mvarTiles(m_lngStartX, m_lngRows), (m_lngEndX - m_lngStartX) * 2
    Next m_lngRows
    TileArray = m_intTiles
End Function

Public Property Get WrapX() As Boolean
Attribute WrapX.VB_MemberFlags = "40"
    WrapX = mvarWrapX
End Property

Public Property Let WrapX(NewWrapX As Boolean)
    mvarWrapX = NewWrapX
End Property

Public Property Get WrapY() As Boolean
Attribute WrapY.VB_MemberFlags = "40"
    WrapY = mvarWrapY
End Property

Public Property Let WrapY(NewWrapY As Boolean)
    mvarWrapY = NewWrapY
End Property

Public Sub GeneratePrerenderedTiles()
Attribute GeneratePrerenderedTiles.VB_Description = "Fills the layer with sequential tile indexes, used to display pre-rendered image backgrounds."
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
Dim m_lngTile As Long
    For m_lngY = 0 To UBound(mvarTiles, 2)
        ProgressUpdate (Height / m_lngY) * 100
        m_lngTile = m_lngY * (UBound(mvarTiles, 1) + 1)
        For m_lngX = 0 To UBound(mvarTiles, 1)
            mvarTiles(m_lngX, m_lngY) = m_lngTile
            m_lngTile = m_lngTile + 1
        Next m_lngX
    Next m_lngY
    ProgressHide
End Sub

Public Property Get Prerendered() As Boolean
Attribute Prerendered.VB_Description = "This flag controls whether the layer is drawn as a single bitmap or as tiles."
On Error Resume Next
    Prerendered = mvarPreRendered
End Property

Public Property Let Prerendered(NewPrerendered As Boolean)
On Error Resume Next
    mvarPreRendered = NewPrerendered
End Property

Public Property Get TileAnimations() As Fury2TileAnimations
Attribute TileAnimations.VB_Description = "~The layer's tile animations."
    Set TileAnimations = mvarAnimations
End Property

Public Property Get ParallaxX() As Single
Attribute ParallaxX.VB_Description = "The X multiplier for the layer's coordinates."
    ParallaxX = mvarParallaxX
End Property

Public Property Let ParallaxX(NewParallaxX As Single)
    mvarParallaxX = NewParallaxX
    m_Engine.Cameras.Dirty
End Property

Public Property Get ParallaxY() As Single
Attribute ParallaxY.VB_Description = "The Y multiplier for the layer's coordinates."
    ParallaxY = mvarParallaxY
End Property

Public Property Let ParallaxY(NewParallaxY As Single)
    mvarParallaxY = NewParallaxY
    m_Engine.Cameras.Dirty
End Property

Public Property Get DepthmapXRatio() As Single
    DepthmapXRatio = mvarDepthmapXRatio
End Property

Public Property Let DepthmapXRatio(NewDepthmapXRatio As Single)
    mvarDepthmapXRatio = NewDepthmapXRatio
    m_Engine.Cameras.Dirty
End Property

Public Property Get DepthmapYRatio() As Single
    DepthmapYRatio = mvarDepthmapYRatio
End Property

Public Property Let DepthmapYRatio(NewDepthmapYRatio As Single)
    mvarDepthmapYRatio = NewDepthmapYRatio
    m_Engine.Cameras.Dirty
End Property

Public Function Duplicate() As Fury2MapLayer
Attribute Duplicate.VB_Description = "Creates a copy of the layer."
Dim m_lyrNew As Fury2MapLayer
    Set m_lyrNew = New Fury2MapLayer
    With m_lyrNew
        .WrapX = mvarWrapX
        .WrapY = mvarWrapY
        .Visible = mvarVisible
        .Alpha = mvarAlpha
        .Effect = mvarEffect - 1
        .Name = mvarName
        .Lit = mvarLit
        .Tiles = mvarTiles
        .BlockingArray = mvarBlocking
        .BlockingGridWidth = mvarBlockGridWidth
        .BlockingGridHeight = mvarBlockGridHeight
        Set .Tileset = mvarTileset.Duplicate
    End With
    Set Duplicate = m_lyrNew
    Set m_lyrNew = Nothing
End Function

Public Property Get Tiles() As Integer()
Attribute Tiles.VB_Description = "*"
Attribute Tiles.VB_MemberFlags = "40"
On Error Resume Next
    Tiles = mvarTiles()
End Property

Public Property Let Tiles(NewTiles() As Integer)
On Error Resume Next
    mvarTiles() = NewTiles()
End Property

Public Property Get BlockingArray() As Byte()
Attribute BlockingArray.VB_Description = "*Returns all the layer's blocking data as an array."
On Error Resume Next
    BlockingArray = mvarBlocking()
End Property

Public Property Let BlockingArray(NewBlocking() As Byte)
On Error Resume Next
    mvarBlocking() = NewBlocking()
End Property

Public Sub Clear(Tile As Integer)
Attribute Clear.VB_Description = "Erases a layer."
On Error Resume Next
Dim m_lngX As Long, m_lngY As Long
    SoftFX.FillMemoryShort VarPtr(mvarTiles(0, 0)), Tile, ((UBound(mvarTiles, 2) + 1) * (UBound(mvarTiles, 1) + 1))
'    For m_lngY = 0 To UBound(mvarTiles, 2)
'        For m_lngX = 0 To UBound(mvarTiles, 1)
'            mvarTiles(m_lngX, m_lngY) = Tile
'        Next m_lngX
'    Next m_lngY
End Sub

Public Sub ResizePreserve(Width As Long, Height As Long)
Attribute ResizePreserve.VB_MemberFlags = "40"
On Error Resume Next
Dim mvarOldTiles() As Integer, mvarCopyRows As Long, m_lngRowCount As Long, mvarOldBlocking() As Byte
    Err.Clear
    mvarOldTiles() = mvarTiles()
    mvarOldBlocking() = mvarBlocking()
    ReDim mvarTiles(0 To Width - 1, 0 To Height - 1)
    ReDim mvarBlocking(0 To ((Width * mvarTileset.TileWidth) \ mvarBlockGridWidth) - 1, 0 To ((Height * mvarTileset.TileHeight) \ mvarBlockGridHeight) - 1)
    Clear mvarTileset.TransparentTile
    m_lngRowCount = UBound(mvarOldTiles, 2)
    If m_lngRowCount > UBound(mvarTiles, 2) Then m_lngRowCount = UBound(mvarTiles, 2)
    For mvarCopyRows = 0 To m_lngRowCount
        ProgressUpdate (m_lngRowCount / mvarCopyRows) * 100
        CopyMemory mvarTiles(0, mvarCopyRows), mvarOldTiles(0, mvarCopyRows), ClipValue((UBound(mvarOldTiles, 1) + 1), 0, UBound(mvarTiles, 1) + 1) * 2
    Next mvarCopyRows
    m_lngRowCount = UBound(mvarOldBlocking, 2)
    If m_lngRowCount > UBound(mvarBlocking, 2) Then m_lngRowCount = UBound(mvarBlocking, 2)
    For mvarCopyRows = 0 To m_lngRowCount
        ProgressUpdate (m_lngRowCount / mvarCopyRows) * 100
        CopyMemory mvarBlocking(0, mvarCopyRows), mvarOldBlocking(0, mvarCopyRows), ClipValue((UBound(mvarOldBlocking, 1) + 1), 0, UBound(mvarBlocking, 1) + 1)
    Next mvarCopyRows
    mvarWidth = Width
    mvarHeight = Height
    ProgressHide
End Sub

Public Sub Resize(Width As Long, Height As Long)
Attribute Resize.VB_MemberFlags = "40"
On Error Resume Next
    If (Width <= 0) Or (Height <= 0) Then Exit Sub
    ReDim mvarTiles(0 To Width - 1, 0 To Height - 1)
    ReDim mvarBlocking(0 To ((Width * mvarTileset.TileWidth) \ mvarBlockGridWidth) - 1, 0 To ((Height * mvarTileset.TileHeight) \ mvarBlockGridHeight) - 1)
    mvarWidth = Width
    mvarHeight = Height
    Clear mvarTileset.TransparentTile
End Sub

Public Property Get BlockingPointer() As Long
Attribute BlockingPointer.VB_Description = "*"
Attribute BlockingPointer.VB_MemberFlags = "40"
On Error Resume Next
    BlockingPointer = VarPtr(mvarBlocking(0, 0))
End Property

Public Property Get CollisionMatrix() As Long
Attribute CollisionMatrix.VB_Description = "*"
On Error Resume Next
    CollisionMatrix = mvarMatrix
End Property

Public Property Get TilePointer(X As Long, Y As Long) As Long
Attribute TilePointer.VB_Description = "*"
Attribute TilePointer.VB_MemberFlags = "40"
On Error Resume Next
    If (X < 0) Or (Y < 0) Or (X > UBound(mvarTiles, 1)) Or (Y > UBound(mvarTiles, 2)) Then Exit Property
    TilePointer = VarPtr(mvarTiles(X, Y))
End Property

Public Property Get Tile(X, Y) As Integer
Attribute Tile.VB_Description = "*Gets or sets a tile value from the layer."
On Error Resume Next
    If (X < 0) Or (Y < 0) Or (X > UBound(mvarTiles, 1)) Or (Y > UBound(mvarTiles, 2)) Then Tile = -1: Exit Property
    Tile = mvarTiles(X, Y)
End Property

Public Property Let Tile(X, Y, Value As Integer)
On Error Resume Next
    If (X < 0) Or (Y < 0) Or (X > UBound(mvarTiles, 1)) Or (Y > UBound(mvarTiles, 2)) Then Exit Property
    mvarTiles(X, Y) = ClipValue(Value, -1, mvarTileset.TileCount - 1)
    m_Engine.Cameras.Dirty
End Property

Public Property Get Blocking(X, Y)
Attribute Blocking.VB_Description = "Gets or sets a blocking value from the layer."
On Error Resume Next
    If (X < 0) Or (Y < 0) Or (X > UBound(mvarBlocking, 1)) Or (Y > UBound(mvarBlocking, 2)) Then Exit Property
    Blocking = mvarBlocking(X, Y)
End Property

Public Property Let Blocking(X, Y, Value)
On Error Resume Next
    If (X < 0) Or (Y < 0) Or (X > UBound(mvarBlocking, 1)) Or (Y > UBound(mvarBlocking, 2)) Then Exit Property
    mvarBlocking(X, Y) = CByte(Abs(Value))
End Property

Public Property Set Tileset(vData As Fury2Tileset)
Attribute Tileset.VB_Description = "~The layer's tileset."
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set X.Tileset = Form1
    Set mvarTileset = vData
End Property

Public Property Get Tileset() As Fury2Tileset
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Tileset
    Set Tileset = mvarTileset
End Property

Public Property Set Sprites(vData As Fury2Sprites)
Attribute Sprites.VB_Description = "~The layer's sprites."
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set X.Sprites = Form1
    Set mvarSprites = vData
End Property

Public Property Get Sprites() As Fury2Sprites
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Sprites
    Set Sprites = mvarSprites
End Property

Public Property Let Lit(ByVal vData As Boolean)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.Lit = 5
    mvarLit = vData
    m_Engine.Cameras.Dirty
End Property

Public Property Get Lit() As Boolean
Attribute Lit.VB_MemberFlags = "40"
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Lit
    Lit = mvarLit
End Property

Public Property Let Effect(ByVal vData As SFXBlitModes)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set X.Effect = Form1
    mvarEffect = vData + 1
    m_Engine.Cameras.Dirty
End Property

Public Property Get Effect() As SFXBlitModes
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Effect
    Effect = mvarEffect - 1 ' Hack to make a value of 0 (the default) actually be -1
End Property

Public Property Let Alpha(ByVal vData As Single)
Attribute Alpha.VB_Description = "The opacity of the layer."
    mvarAlpha = vData
    If mvarAlpha < -1 Then mvarAlpha = -1
    If mvarAlpha > 1 Then mvarAlpha = 1
    m_Engine.Cameras.Dirty
End Property

Public Property Get Alpha() As Single
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Alpha
    Alpha = mvarAlpha
End Property

Public Property Let Visible(ByVal vData As Boolean)
Attribute Visible.VB_Description = "This flag controls whether the layer is shown."
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.Visible = 5
    mvarVisible = vData
    m_Engine.Cameras.Dirty
End Property

Public Property Get Visible() As Boolean
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Visible
    Visible = mvarVisible
End Property

Private Sub Class_Initialize()
On Error Resume Next
    mvarVisible = True
    ParallaxX = 1
    ParallaxY = 1
    mvarBlockGridWidth = 16
    mvarBlockGridHeight = 16
    mvarName = "Untitled"
    ReDim mvarBlockingLines(0)
    Set mvarLighting = New Fury2LayerLighting
    Set mvarSprites = New Fury2Sprites
    Set mvarTileset = New Fury2Tileset
    Set mvarAnimations = New Fury2TileAnimations
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    DeallocateCollisionMatrix
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim m_lngValue As Long
Dim m_lngLineCount As Long
    m_Engine.ContextLevelAdd "Load Layer"
    With File
        .ReadSegment_Begin
            .ReadSegment_Begin
                .Skip 4
                .Load mvarLit
                .Load mvarName
                .Load mvarWidth
                .Load mvarHeight
                If mvarWidth <= 0 Then mvarWidth = Parent.lWidth
                If mvarHeight <= 0 Then mvarHeight = Parent.lHeight
            .ReadSegment_End
            .ReadSegment_Begin
                .Load mvarAlpha
                .Load mvarEffect
                .Load mvarVisible
                .ReadSegment_Begin
                    .Load mvarParallaxX
                    .Load mvarParallaxY
                    .Load mvarWrapX
                    .Load mvarWrapY
                .ReadSegment_End
                .Load mvarPreRendered
                .ReadSegment_Begin
'                    .Load mvarPreRenderedAnimDelay
'                    .Load mvarPreRenderedAnimFrame
                .ReadSegment_End
                If .SegmentLeft < 12 Then
                    mvarBlockGridWidth = -1
                    mvarBlockGridHeight = -1
                Else
                    .ReadSegment_Begin
                        .Load mvarBlockGridWidth
                        .Load mvarBlockGridHeight
                    .ReadSegment_End
                End If
                If .SegmentLeft < 16 Then
                Else
                    .Load mvarDepthmap
                    .ReadSegment_Begin
                        .Load mvarDepthmapXRatio
                        .Load mvarDepthmapYRatio
                    .ReadSegment_End
                End If
            .ReadSegment_End
            .Load mvarTileset
            If mvarBlockGridWidth <= 0 Then
                mvarBlockGridWidth = mvarTileset.TileWidth
            End If
            If mvarBlockGridHeight <= 0 Then
                mvarBlockGridHeight = mvarTileset.TileHeight
            End If
            .Load m_lngValue
            .Skip m_lngValue
            mdlFileUtils.Load2DIntArray mvarTiles, File
            If ((UBound(mvarTiles, 1) <> (mvarWidth - 1)) Or (UBound(mvarTiles, 2) <> (mvarHeight - 1))) Then
                ResizePreserve mvarWidth, mvarHeight
            End If
            mdlFileUtils.Load2DByteArray mvarBlocking, File
            .Load mvarAnimations
            .Load mvarSprites
            .Load mvarDisableBlocking
            .ReadSegment_Begin
                .Load m_lngLineCount
                If m_lngLineCount > 0 Then
                    ReDim mvarBlockingLines(0 To m_lngLineCount - 1)
                    .RawLoad VarPtr(mvarBlockingLines(0)), m_lngLineCount * 16
                Else
                    ReDim mvarBlockingLines(0)
                End If
            .ReadSegment_End
            .Load mvarLighting
        .ReadSegment_End
    End With
    If mvarDisableBlocking Then ReDim mvarBlocking(0 To 0, 0 To 0)
    m_Engine.ContextLevelRemove
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .WriteSegment_Begin
                .Skip 4
                .Save mvarLit
                .Save mvarName
                .Save mvarWidth
                .Save mvarHeight
            .WriteSegment_End
            .WriteSegment_Begin
                .Save mvarAlpha
                .Save mvarEffect
                .Save mvarVisible
                .WriteSegment_Begin
                    .Save mvarParallaxX
                    .Save mvarParallaxY
                    .Save mvarWrapX
                    .Save mvarWrapY
                .WriteSegment_End
                .Save mvarPreRendered
                .WriteSegment_Begin
'                    .Save mvarPreRenderedAnimDelay
'                    .Save mvarPreRenderedAnimFrame
                .WriteSegment_End
                .WriteSegment_Begin
                    .Save mvarBlockGridWidth
                    .Save mvarBlockGridHeight
                .WriteSegment_End
                .Save mvarDepthmap
                .WriteSegment_Begin
                    .Save mvarDepthmapXRatio
                    .Save mvarDepthmapYRatio
                .WriteSegment_End
            .WriteSegment_End
            .Save mvarTileset
            .Skip 4
            mdlFileUtils.Save2DIntArray mvarTiles, File
            mdlFileUtils.Save2DByteArray mvarBlocking, File
            .Save mvarAnimations
            .Save mvarSprites
            .Save mvarDisableBlocking
            .WriteSegment_Begin
                .Save CLng(CollisionLineCount)
                .RawSave VarPtr(mvarBlockingLines(0)), CollisionLineCount * 16
            .WriteSegment_End
            .Save mvarLighting
        .WriteSegment_End
    End With
End Sub


