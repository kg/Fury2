VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TSTFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

'
'   RPG Toolkit TST File Class
'   Copyright 2002, Kevin Gadd
'

Private Type RGBTriplet
    Red As Byte
    Green As Byte
    Blue As Byte
End Type

Public Filename As String
Private m_lngTileCount As Long
Private m_lngTileWidth As Long
Private m_lngTileHeight As Long
Private m_colTiles As Collection
Private m_booInitialized As Boolean

Sub Load(Filename As String)
On Error Resume Next
Dim m_vfFile As VirtualFile
Dim m_intVersion As Integer, m_intDetail As Integer, m_intCount As Integer
Dim m_lngX As Long, m_lngY As Long, m_lngTiles As Long
Dim m_tleTile As TSTTile
    Set m_vfFile = New VirtualFile
    m_vfFile.LoadFile Filename
    m_vfFile.Load m_intVersion
    m_vfFile.Load m_intCount
    m_vfFile.Load m_intDetail
    Select Case m_intDetail
    Case 1
        m_lngTileWidth = 32
        m_lngTileHeight = 32
    Case 2
        m_lngTileWidth = 16
        m_lngTileHeight = 16
    Case Else
        ' Unsupported format
        Exit Sub
    End Select
    For m_lngTiles = 1 To m_intCount
        Set m_tleTile = New TSTTile
        m_tleTile.Load m_vfFile, m_lngTileWidth, m_lngTileHeight
        m_colTiles.Add m_tleTile
    Next m_lngTiles
    m_booInitialized = True
End Sub
'
'Sub Save(Filename As String)
'Dim m_vfFile As VirtualFile
'Dim m_intVersion As Integer, m_intDetail As Integer, m_intCount As Integer
'Dim m_lngX As Long, m_lngY As Long, m_lngTiles As Long
'Dim m_tleTile As TSTTile
'Dim m_lngPixels() As Long
'    If m_booInitialized = False Then Exit Sub
'    Set m_vfFile = New VirtualFile
'    m_vfFile.Save CInt(20)
'    m_vfFile.Save CInt(m_colTiles.Count)
'    If (m_lngTileWidth = 32) And (m_lngTileHeight = 32) Then
'        m_vfFile.Save CInt(1)
'    ElseIf (m_lngTileWidth = 16) And (m_lngTileHeight = 16) Then
'        m_vfFile.Save CInt(2)
'    Else
'        Exit Sub
'    End If
'    For m_lngTiles = 1 To m_colTiles.Count
'        m_colTiles(m_lngTiles).Save m_vfFile
'    Next m_lngTiles
'    m_vfFile.SaveFile Filename
'End Sub
'
'Sub CreateNew()
'    Set m_colTiles = New Collection
'    m_lngTileWidth = 32
'    m_lngTileHeight = 32
'    m_booInitialized = True
'End Sub
'
'Sub RemoveTile(Index As Long)
'On Error Resume Next
'    If m_booInitialized = False Then Exit Sub
'    m_colTiles.Remove Index
'End Sub
'
'Sub AddTile(Tile As TSTTile)
'On Error Resume Next
'    If m_booInitialized = False Then Exit Sub
'    m_colTiles.Add Tile
'End Sub

Public Property Get Tiles(Index As Long) As TSTTile
On Error Resume Next
    If m_booInitialized = False Then Exit Property
    Set Tiles = m_colTiles(Index)
End Property

Public Property Get TileCount() As Long
    TileCount = CLng(m_colTiles.count)
End Property

Private Sub Class_Initialize()
    Set m_colTiles = New Collection
    m_lngTileWidth = 0
    m_lngTileHeight = 0
    m_booInitialized = False
End Sub

Public Property Get Image() As Fury2Image
Dim m_lngTiles As Long
Dim m_tleTile As TSTTile
Dim l_lngX As Long, l_lngY As Long
    Set Image = F2Image(8 * m_lngTileWidth, m_lngTileHeight * Ceil(m_colTiles.count / 8))
    l_lngX = 0
    l_lngY = 0
    For m_lngTiles = 1 To m_colTiles.count
        Set m_tleTile = m_colTiles(m_lngTiles)
        Image.Blit F2Rect(l_lngX, l_lngY, m_lngTileWidth, m_lngTileHeight, False), , m_tleTile.Image()
        l_lngX = l_lngX + m_lngTileWidth
        If l_lngX >= Image.Width Then
            l_lngX = 0
            l_lngY = l_lngY + m_lngTileHeight
        End If
    Next m_lngTiles
    Image.FillChannel Image.Rectangle, 3, 255
    Image.ReplaceColor F2RGB(0, 1, 2), 0
    Image.AlphaChannel = True
End Property
