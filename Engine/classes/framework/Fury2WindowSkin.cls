VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2WindowSkin"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
Implements Fury2Object
Implements IVirtualFileSaveable

Private m_wspParam As WindowSkinParam
Private m_imgImages(0 To 8) As Fury2Image
Private m_lngImages(0 To 8) As Long
Private m_lngWidths(0 To 8) As Long
Private m_lngHeights(0 To 8) As Long
Private m_lngEdgeOverlap As Long
Private m_booEdgeOverlap As Boolean
Private m_strTitle As String
Private m_strAuthor As String
Private m_strFilename As String
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Window Skin: " & m_strTitle
End Function

Public Property Let Tint(ByVal Color As Long)
On Error Resume Next
Dim l_lngSection As Long
    For l_lngSection = LBound(m_wspParam.TintColors) To UBound(m_wspParam.TintColors)
        m_wspParam.TintColors(l_lngSection) = Color
    Next l_lngSection
End Property

Public Property Get SectionTint(Section) As Long
    SectionTint = m_wspParam.TintColors(CLng(Section))
End Property

Public Property Let SectionTint(Section, Color As Long)
    m_wspParam.TintColors(CLng(Section)) = Color
End Property

Public Property Get BlitMode() As SFXBlitModes
    BlitMode = m_wspParam.RenderMode
End Property

Public Property Let BlitMode(ByVal Value As SFXBlitModes)
    m_wspParam.RenderMode = Value
End Property

Public Property Get EdgeOverlap() As Boolean
    EdgeOverlap = m_booEdgeOverlap
End Property

Public Property Let EdgeOverlap(ByVal Value As Boolean)
    m_booEdgeOverlap = Value
End Property

Public Property Get EdgeOffset(Optional ByVal Edge As Long = 0) As Long
On Error Resume Next
    If Edge = 0 Then
        EdgeOffset = (m_wspParam.EdgeOffsets(0) + m_wspParam.EdgeOffsets(1) + m_wspParam.EdgeOffsets(2) + m_wspParam.EdgeOffsets(3)) \ 4
    Else
        EdgeOffset = m_wspParam.EdgeOffsets(Edge - 1)
    End If
End Property

Public Property Let EdgeOffset(Optional ByVal Edge As Long = 0, ByVal Value As Long)
On Error Resume Next
    If Edge = 0 Then
        m_wspParam.EdgeOffsets(0) = Value
        m_wspParam.EdgeOffsets(1) = Value
        m_wspParam.EdgeOffsets(2) = Value
        m_wspParam.EdgeOffsets(3) = Value
    Else
        m_wspParam.EdgeOffsets(Edge - 1) = Value
    End If
End Property

Public Property Get BackgroundColor(ByVal Corner As Long) As Long
On Error Resume Next
    BackgroundColor = m_wspParam.CornerColors(Corner)
End Property

Public Property Let BackgroundColor(ByVal Corner As Long, ByVal Value As Long)
On Error Resume Next
    m_wspParam.CornerColors(Corner) = Value
End Property

Public Property Get Overlap() As Long
    Overlap = m_lngEdgeOverlap
End Property

Public Property Let Overlap(ByVal NewValue As Long)
    m_lngEdgeOverlap = NewValue
End Property

Public Property Let BackgroundMode(ByVal NewValue As Fury2WindowSkinBackgroundModes)
    m_wspParam.BackgroundMode = NewValue
End Property

Public Property Get BackgroundMode() As Fury2WindowSkinBackgroundModes
    BackgroundMode = m_wspParam.BackgroundMode
End Property

Public Property Get Title() As String
Attribute Title.VB_MemberFlags = "200"
    Title = m_strTitle
End Property

Public Property Let Title(NewTitle As String)
    m_strTitle = NewTitle
End Property

Public Property Get Author() As String
    Author = m_strAuthor
End Property

Public Property Let Author(NewAuthor As String)
    m_strAuthor = NewAuthor
End Property

Public Property Get Filename() As String
    Filename = m_strFilename
End Property

Public Property Get MaskColor() As Long
    MaskColor = m_wspParam.MaskColor
End Property

Public Property Let MaskColor(NewColor As Long)
    m_wspParam.MaskColor = NewColor
End Property

Public Sub AutoScaleSizes()
On Error Resume Next
'    Set m_imgImages(WS_Top) = m_imgImages(WS_Top).TileUp(c_lngWindowSkinSegmentSize, 1)
'    Set m_imgImages(WS_Bottom) = m_imgImages(WS_Bottom).TileUp(c_lngWindowSkinSegmentSize, 1)
'    Set m_imgImages(WS_Left) = m_imgImages(WS_Left).TileUp(1, c_lngWindowSkinSegmentSize)
'    Set m_imgImages(WS_Right) = m_imgImages(WS_Right).TileUp(1, c_lngWindowSkinSegmentSize)
'    Set m_imgImages(WS_Middle) = m_imgImages(WS_Middle).TileUp(c_lngWindowSkinSegmentSize, c_lngWindowSkinSegmentSize)
    Refresh
End Sub

Public Property Get AdjustmentWidth() As Long
On Error Resume Next
    AdjustmentWidth = (vbMax(m_lngWidths(WS_TopLeft), m_lngWidths(WS_Left), m_lngWidths(WS_BottomLeft)) - m_lngEdgeOverlap) + (vbMax(m_lngWidths(WS_TopRight), m_lngWidths(WS_Right), m_lngWidths(WS_BottomRight)) + m_lngEdgeOverlap)
End Property

Public Property Get AdjustmentHeight() As Long
On Error Resume Next
    AdjustmentHeight = (vbMax(m_lngHeights(WS_TopLeft), m_lngHeights(WS_Top), m_lngHeights(WS_TopRight)) - m_lngEdgeOverlap) + (vbMax(m_lngHeights(WS_BottomLeft), m_lngHeights(WS_Bottom), m_lngHeights(WS_BottomRight)) + m_lngEdgeOverlap)
End Property

Public Function AdjustRectangle(Rectangle) As Fury2Rect
On Error Resume Next
Dim l_rctRectangle As Fury2Rect
Dim l_lngMax As Long
    Set l_rctRectangle = Rectangle
    If Err <> 0 Then Exit Function
    With l_rctRectangle
        .left = .left + vbMax(m_lngWidths(WS_TopLeft), m_lngWidths(WS_Left), m_lngWidths(WS_BottomLeft)) - m_lngEdgeOverlap
        .right = .right - vbMax(m_lngWidths(WS_TopRight), m_lngWidths(WS_Right), m_lngWidths(WS_BottomRight)) + m_lngEdgeOverlap
        .top = .top + vbMax(m_lngHeights(WS_TopLeft), m_lngHeights(WS_Top), m_lngHeights(WS_TopRight)) - m_lngEdgeOverlap
        .bottom = .bottom - vbMax(m_lngHeights(WS_BottomLeft), m_lngHeights(WS_Bottom), m_lngHeights(WS_BottomRight)) + m_lngEdgeOverlap
    End With
    Err.Clear
    Set AdjustRectangle = l_rctRectangle
End Function

Public Sub Draw(Surface, Rectangle, Optional ByVal Alpha As Single = 1, Optional ByVal SectionFlags As Long = 0)
On Error Resume Next
Dim l_imgSurface As Fury2Image
Dim l_rctRectangle As Fury2Rect
Dim l_rctArea As Rectangle
    Set l_imgSurface = Surface
    If Err <> 0 Then Exit Sub
    Set l_rctRectangle = Rectangle.Copy
    If Err <> 0 Then Exit Sub
    l_rctArea = l_rctRectangle.GetRectangle
    With l_rctArea
        .left = .left + m_lngEdgeOverlap
        .top = .top + m_lngEdgeOverlap
        .Width = .Width - (m_lngEdgeOverlap * 2)
        .Height = .Height - (m_lngEdgeOverlap * 2)
    End With
    m_wspParam.Alpha = ClipValue(Alpha * 255, 0, 255)
    SoftFX.RenderWindow l_imgSurface.Handle, l_rctArea, m_wspParam, CLng(SectionFlags)
    l_imgSurface.Dirty = True
End Sub

Sub Create(ByVal Width As Long, ByVal Height As Long, ByVal MiddleWidth As Long, ByVal MiddleHeight As Long)
On Error Resume Next
    Set m_imgImages(WS_TopLeft) = F2Image(Width, Height)
    Set m_imgImages(WS_TopRight) = F2Image(Width, Height)
    Set m_imgImages(WS_BottomLeft) = F2Image(Width, Height)
    Set m_imgImages(WS_BottomRight) = F2Image(Width, Height)
    Set m_imgImages(WS_Top) = F2Image(MiddleWidth, Height)
    Set m_imgImages(WS_Bottom) = F2Image(MiddleWidth, Height)
    Set m_imgImages(WS_Left) = F2Image(Width, MiddleHeight)
    Set m_imgImages(WS_Right) = F2Image(Width, MiddleHeight)
    Set m_imgImages(WS_Middle) = F2Image(MiddleWidth, MiddleHeight)
    With m_wspParam
        .BackgroundMode = 0
        .EdgeMode = 0
        .MaskColor = 0
    End With
    Refresh
End Sub

Function ImportImage(ByVal Filename, Optional ByVal CornerWidth As Long = 8, Optional ByVal CornerHeight As Long = 8, Optional ByVal MiddleWidth As Long = 48, Optional ByVal MiddleHeight As Long = 48, Optional ByVal MaskColor As Long = 0) As Boolean
On Error Resume Next
Dim l_imgFile As Fury2Image
    Set l_imgFile = Engine.LoadImage(Filename, True)
    If l_imgFile Is Nothing Then ImportImage = False: Exit Function
    If (CornerWidth < 1) Or (CornerHeight < 1) Or (MiddleWidth < 1) Or (MiddleHeight < 1) Then ImportImage = False: Exit Function
    Set m_imgImages(WS_TopLeft) = l_imgFile.Subimage(F2Rect(0, 0, CornerWidth, CornerHeight, False))
    Set m_imgImages(WS_Top) = l_imgFile.Subimage(F2Rect(CornerWidth, 0, MiddleWidth, CornerHeight, False))
    Set m_imgImages(WS_TopRight) = l_imgFile.Subimage(F2Rect((CornerWidth + MiddleWidth), 0, CornerWidth, CornerHeight, False))
    Set m_imgImages(WS_Left) = l_imgFile.Subimage(F2Rect(0, CornerHeight, CornerWidth, MiddleHeight, False))
    Set m_imgImages(WS_Right) = l_imgFile.Subimage(F2Rect(CornerWidth + MiddleWidth, CornerHeight, CornerWidth, MiddleHeight, False))
    Set m_imgImages(WS_BottomLeft) = l_imgFile.Subimage(F2Rect(0, CornerHeight + MiddleHeight, CornerWidth, CornerHeight, False))
    Set m_imgImages(WS_Bottom) = l_imgFile.Subimage(F2Rect(CornerWidth, CornerHeight + MiddleHeight, MiddleWidth, CornerHeight, False))
    Set m_imgImages(WS_BottomRight) = l_imgFile.Subimage(F2Rect((CornerWidth + MiddleWidth), CornerHeight + MiddleHeight, CornerWidth, CornerHeight, False))
    Set m_imgImages(WS_Middle) = l_imgFile.Subimage(F2Rect(CornerWidth, CornerHeight, MiddleWidth, MiddleHeight, False))
    With m_wspParam
        .BackgroundMode = 0
        .EdgeMode = 0
        .MaskColor = MaskColor
        If l_imgFile.AlphaChannel Then
            .RenderMode = BlitMode_SourceAlpha
        Else
            .RenderMode = BlitMode_Matte
        End If
    End With
    m_strTitle = Filename
    Set l_imgFile = Nothing
    AutoScaleSizes
    ImportImage = True
End Function

Sub Refresh()
On Error Resume Next
Dim l_lngBitmaps As Long
    For l_lngBitmaps = 0 To 8
        m_lngImages(l_lngBitmaps) = m_imgImages(l_lngBitmaps).Handle
        m_lngWidths(l_lngBitmaps) = m_imgImages(l_lngBitmaps).Width
        m_lngHeights(l_lngBitmaps) = m_imgImages(l_lngBitmaps).Height
        m_imgImages(l_lngBitmaps).Optimize
    Next l_lngBitmaps
    With m_wspParam
        .Alpha = 255
        .pImages = VarPtr(m_lngImages(0))
    End With
End Sub

Function ImportRWS(ByVal Filename) As Boolean
On Error Resume Next
Dim l_vfSkin As VirtualFile
Dim l_lngLoadBitmaps As Long
Dim l_intWidth As Integer, l_intHeight As Integer
Dim l_rwsHeader As RWS_Header
    If Not Engine.Filesystem.FileExists(Filename) Then Exit Function
    Set l_vfSkin = Engine.Filesystem.File(Filename).Load
    l_vfSkin.Skip 4
    l_vfSkin.Load l_rwsHeader.Version
    l_vfSkin.Load l_rwsHeader.EdgeWidth
    l_vfSkin.Load l_rwsHeader.BackgroundMode
    l_vfSkin.Load l_rwsHeader.CornerColor1
    l_vfSkin.Load l_rwsHeader.CornerColor2
    l_vfSkin.Load l_rwsHeader.CornerColor3
    l_vfSkin.Load l_rwsHeader.CornerColor4
    l_vfSkin.Load l_rwsHeader.BorderOffsetLeft
    l_vfSkin.Load l_rwsHeader.BorderOffsetTop
    l_vfSkin.Load l_rwsHeader.BorderOffsetRight
    l_vfSkin.Load l_rwsHeader.BorderOffsetBottom
    l_vfSkin.Skip 36
    If l_rwsHeader.Version = 1 Then
        For l_lngLoadBitmaps = 0 To 8
            Set m_imgImages(l_lngLoadBitmaps) = F2Image(l_rwsHeader.EdgeWidth, l_rwsHeader.EdgeWidth)
            l_vfSkin.RawLoad m_imgImages(l_lngLoadBitmaps).Pointer(0, 0), l_rwsHeader.EdgeWidth * l_rwsHeader.EdgeWidth * 4&
            m_imgImages(l_lngLoadBitmaps).SwapChannels Red, Blue
        Next l_lngLoadBitmaps
    ElseIf (l_rwsHeader.Version = 2) Or (l_rwsHeader.Version = 3) Then
        For l_lngLoadBitmaps = 0 To 8
            l_vfSkin.Load l_intWidth
            l_vfSkin.Load l_intHeight
            Set m_imgImages(l_lngLoadBitmaps) = F2Image(l_intWidth, l_intHeight)
            l_vfSkin.RawLoad m_imgImages(l_lngLoadBitmaps).Pointer(0, 0), l_intWidth * l_intHeight * 4&
            m_imgImages(l_lngLoadBitmaps).SwapChannels Red, Blue
        Next l_lngLoadBitmaps
    Else
        ImportRWS = False
        Exit Function
    End If
    With m_wspParam
        .RenderMode = BlitMode_SourceAlpha
        .BackgroundMode = l_rwsHeader.BackgroundMode
        .EdgeMode = 0
        .MaskColor = 0
        .CornerColors(0) = l_rwsHeader.CornerColor1
        .CornerColors(1) = l_rwsHeader.CornerColor2
        .CornerColors(2) = l_rwsHeader.CornerColor3
        .CornerColors(3) = l_rwsHeader.CornerColor4
        .EdgeOffsets(0) = l_rwsHeader.BorderOffsetLeft
        .EdgeOffsets(1) = l_rwsHeader.BorderOffsetTop
        .EdgeOffsets(2) = l_rwsHeader.BorderOffsetRight
        .EdgeOffsets(3) = l_rwsHeader.BorderOffsetBottom
    End With
    AutoScaleSizes
    Set l_vfSkin = Nothing
    m_strTitle = Filename
    ImportRWS = True
End Function

Function Load(ByRef Filename As String) As Boolean
On Error Resume Next
Dim l_strExtension As String
    If InStr(Filename, ".") Then
        l_strExtension = LCase(Trim(Mid(Filename, InStrRev(Filename, ".") + 1)))
    End If
    m_strFilename = Filename
    Select Case l_strExtension
    Case "rws"
        Load = ImportRWS(Filename)
    Case "f2ws"
        Load = Engine.LoadFromFile(Me, Engine.Filesystem.File(Filename))
        m_strTitle = Filename
    Case Else
        Load = ImportImage(Filename)
    End Select
End Function

Function Save(ByVal Filename) As Boolean
On Error Resume Next
    m_strFilename = Filename
    Save = Engine.SaveToFile(Me, Engine.Filesystem.CreateFile(Filename))
End Function

Function Duplicate() As Fury2WindowSkin
End Function

Private Sub Class_Initialize()
On Error Resume Next
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_intValue As Integer
Dim l_lngValue As Long
Dim l_lngImages As Long
    With File
        .ReadSegment_Begin
            .Load m_strTitle
            .Load m_strAuthor
        .ReadSegment_End
        .ReadSegment_Begin
            If Len(m_wspParam) > .SegmentLength Then
                .RawLoad VarPtr(m_wspParam), .SegmentLength
            Else
                .RawLoad VarPtr(m_wspParam), Len(m_wspParam)
            End If
        .ReadSegment_End
        Erase m_imgImages
        .ReadSegment_Begin
            For l_lngImages = 0 To 8
                Set m_imgImages(l_lngImages) = New Fury2Image
                .Load m_imgImages(l_lngImages)
            Next l_lngImages
        .ReadSegment_End
        .Load m_lngEdgeOverlap
        .Load m_booEdgeOverlap
        AutoScaleSizes
        Err.Clear
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
Dim l_lngImages As Long
    With File
        .WriteSegment_Begin
            .Save m_strTitle
            .Save m_strAuthor
        .WriteSegment_End
        .WriteSegment_Begin
            .RawSave VarPtr(m_wspParam), Len(m_wspParam)
        .WriteSegment_End
        .WriteSegment_Begin
            For l_lngImages = 0 To 8
                .Save m_imgImages(l_lngImages)
            Next l_lngImages
        .WriteSegment_End
        .Save m_lngEdgeOverlap
        .Save m_booEdgeOverlap
        Err.Clear
    End With
End Sub

