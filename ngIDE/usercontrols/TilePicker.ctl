VERSION 5.00
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Begin VB.UserControl TilePicker 
   ClientHeight    =   4125
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   1785
   ScaleHeight     =   275
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   119
   Begin VB.PictureBox picTilesetCache 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   285
      ScaleHeight     =   16
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   17
      TabIndex        =   2
      Top             =   1815
      Visible         =   0   'False
      Width           =   255
   End
   Begin cFScroll.FlatScrollBar hsScrollbar 
      Height          =   255
      Left            =   0
      TabIndex        =   1
      Top             =   3870
      Width           =   1545
      _ExtentX        =   2725
      _ExtentY        =   450
      Max             =   100
      Style           =   -1
   End
   Begin cFScroll.FlatScrollBar vsScrollbar 
      Height          =   3885
      Left            =   1545
      TabIndex        =   0
      Top             =   0
      Width           =   240
      _ExtentX        =   423
      _ExtentY        =   6853
      Orientation     =   1
      Max             =   100
      Style           =   -1
   End
End
Attribute VB_Name = "TilePicker"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Public Event SelectionChanged(Tile As Integer)

Private m_lngBuffer As Long
Private m_lngWidth As Long, m_lngHeight As Long
Private m_tstTileset As Fury2Tileset
Private m_booPreserveRows As Boolean
Private m_intSelectedTiles() As Integer

Public Sub RebuildCache()
On Error Resume Next
Dim l_imgTilesetCache As Fury2Image
    m_lngBuffer = m_tstTileset.Buffer.Handle
    Set l_imgTilesetCache = m_tstTileset.Buffer.Duplicate
    With l_imgTilesetCache
        If .AlphaChannel Then
            .Composite SwapChannels(GetSystemColor(SystemColor_Button_Face), Blue, Red)
        Else
            .ReplaceColor .Rectangle, m_tstTileset.MaskColor, SwapChannels(GetSystemColor(SystemColor_Button_Face), Blue, Red)
        End If
    End With
    Set picTilesetCache.Picture = l_imgTilesetCache.Picture
End Sub

Public Function HitTest(ByVal X As Long, ByVal Y As Long) As Integer
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngSX As Long, l_lngSY As Long
Dim l_lngHeight As Long, l_lngMax As Long
Dim l_lngWidth As Long
Dim l_lngTile As Long
Dim l_lngMaxX As Long, l_lngMaxY As Long
Dim l_lngSelectedTiles As Long
Dim l_booSelected As Boolean
    l_lngY = -vsScrollbar.Value
    l_lngX = -hsScrollbar.Value
    HitTest = -1
    
    If Not (m_tstTileset Is Nothing) Then
        Do Until ((l_lngSY + m_tstTileset.TileHeight) > picTilesetCache.Height)
            If (Y >= l_lngY) And (Y < (l_lngY + m_tstTileset.TileHeight)) Then
                If (X >= l_lngX) And (X < (l_lngX + m_tstTileset.TileWidth)) Then
                    HitTest = l_lngTile
                    Exit Do
                End If
            End If
        
            l_lngSX = l_lngSX + m_tstTileset.TileWidth
            If (l_lngSX + m_tstTileset.TileWidth) > picTilesetCache.ScaleWidth Then
                l_lngSX = 0
                l_lngSY = l_lngSY + m_tstTileset.TileHeight
                If m_booPreserveRows Then
                    l_lngMaxX = l_lngX + m_tstTileset.TileWidth
                    l_lngX = -hsScrollbar.Value - m_tstTileset.TileWidth
                    l_lngY = l_lngY + m_tstTileset.TileHeight
                    l_lngHeight = l_lngHeight + m_tstTileset.TileHeight
                End If
            End If
            
            l_lngX = l_lngX + m_tstTileset.TileWidth
            If m_booPreserveRows Then
                If l_lngX > l_lngWidth Then
                    l_lngWidth = l_lngX
                End If
            Else
                If (l_lngX + m_tstTileset.TileWidth + (hsScrollbar.Value)) > (m_lngWidth) Then
                    l_lngMaxX = l_lngX
                    l_lngX = -hsScrollbar.Value
                    l_lngY = l_lngY + m_tstTileset.TileHeight
                    l_lngHeight = l_lngHeight + m_tstTileset.TileHeight
                End If
            End If
            l_lngTile = l_lngTile + 1
        Loop
    End If
End Function

Public Property Get hWnd() As Long
    hWnd = UserControl.hWnd
End Property

Public Property Get PreserveRows() As Boolean
    PreserveRows = m_booPreserveRows
End Property

Public Property Let PreserveRows(NewValue As Boolean)
On Error Resume Next
    m_booPreserveRows = NewValue
    Redraw
End Property

Public Sub SetSelectedTiles(ByRef Tiles() As Integer)
On Error Resume Next
Dim l_lngUBX As Long, l_lngUBY As Long
Dim l_lngY As Long
    l_lngUBX = -1
    l_lngUBY = -1
    l_lngUBX = UBound(Tiles, 1)
    l_lngUBY = UBound(Tiles, 2)
    Err.Clear
    If (l_lngUBX = 0 And l_lngUBY = 0) Then
        ReDim m_intSelectedTiles(0 To 0)
        m_intSelectedTiles(0) = Tiles(0, 0)
    ElseIf l_lngUBY > -1 Then
        ReDim m_intSelectedTiles(0 To ((l_lngUBX + 1) * (l_lngUBY + 1)) - 1)
        For l_lngY = LBound(Tiles, 2) To UBound(Tiles, 2)
            CopyMemory m_intSelectedTiles((l_lngY * (l_lngUBX + 1)) - 1), Tiles(0, l_lngY), (l_lngUBX + 1) * 2
        Next l_lngY
    Else
        m_intSelectedTiles = Tiles
    End If
    Redraw
End Sub

Private Sub AllocateBackbuffer()
On Error Resume Next
'    Set m_imgBuffer = F2Image(UserControl.ScaleWidth, UserControl.ScaleHeight)
End Sub

Private Sub DeallocateBackbuffer()
On Error Resume Next
'    Set m_imgBuffer = Nothing
End Sub

Public Sub Redraw()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngSX As Long, l_lngSY As Long
Dim l_lngHeight As Long, l_lngMax As Long
Dim l_lngWidth As Long
Dim l_lngTile As Long
Dim l_lngMaxX As Long, l_lngMaxY As Long
Dim l_lngSelectedTiles As Long
Dim l_booSelected As Boolean
Dim l_imgTile As Fury2Image, l_lngBackgroundColor As Long
    l_lngY = -vsScrollbar.Value
    l_lngX = -hsScrollbar.Value
    
    If Not (m_tstTileset Is Nothing) Then
        If m_lngBuffer <> m_tstTileset.Buffer.Handle Then RebuildCache
        If (m_tstTileset.TileWidth > 0) And (m_tstTileset.TileHeight > 0) Then
            Set l_imgTile = F2Image(m_tstTileset.TileWidth, m_tstTileset.TileHeight)
            l_lngBackgroundColor = SwapChannels(GetSystemColor(SystemColor_Button_Face), Blue, Red)
            Do Until ((l_lngSY + m_tstTileset.TileHeight) > picTilesetCache.Height)
                If (l_lngSY + m_tstTileset.TileHeight) > 0 Then
                    l_booSelected = False
                    For l_lngSelectedTiles = 0 To UBound(m_intSelectedTiles)
                        If m_intSelectedTiles(l_lngSelectedTiles) = l_lngTile Then
                            l_booSelected = True
                            Exit For
                        End If
                    Next l_lngSelectedTiles
                    If l_booSelected Then
                        l_imgTile.Copy m_tstTileset.Tile(l_lngTile)
                        l_imgTile.Composite l_lngBackgroundColor
                        l_imgTile.Box l_imgTile.Rectangle.Adjust(-1, -1), F2RGB(0, 0, 0, 160), RenderMode_SourceAlpha
                        l_imgTile.Box l_imgTile.Rectangle, F2RGB(255, 255, 255, 160), RenderMode_SourceAlpha
                        CopyImageToDC UserControl.hdc, F2Rect(l_lngX, l_lngY, m_tstTileset.TileWidth, m_tstTileset.TileHeight, False), l_imgTile
                    Else
                        BitBlt UserControl.hdc, l_lngX, l_lngY, m_tstTileset.TileWidth, m_tstTileset.TileHeight, picTilesetCache.hdc, l_lngSX, l_lngSY, vbSrcCopy
                    End If
                    'm_imgBuffer.Blit F2Rect(l_lngX, l_lngY, m_tstTileset.TileWidth, m_tstTileset.TileHeight, False), F2Rect(l_lngSX, l_lngSY, m_tstTileset.TileWidth, m_tstTileset.TileHeight, False), m_imgTilesetCache
                End If
                      
                l_lngSX = l_lngSX + m_tstTileset.TileWidth
                If (l_lngSX + m_tstTileset.TileWidth) > picTilesetCache.ScaleWidth Then
                    l_lngSX = 0
                    l_lngSY = l_lngSY + m_tstTileset.TileHeight
                    If m_booPreserveRows Then
                        l_lngMaxX = l_lngX + m_tstTileset.TileWidth
                        l_lngX = -hsScrollbar.Value - m_tstTileset.TileWidth
                        l_lngY = l_lngY + m_tstTileset.TileHeight
                        l_lngHeight = l_lngHeight + m_tstTileset.TileHeight
                    End If
                End If
                
                l_lngX = l_lngX + m_tstTileset.TileWidth
                If m_booPreserveRows Then
                    If (l_lngX + m_tstTileset.TileWidth) > l_lngWidth Then
                        l_lngWidth = (l_lngX + m_tstTileset.TileWidth)
                    End If
                Else
                    If (l_lngX + m_tstTileset.TileWidth + (hsScrollbar.Value)) > (m_lngWidth) Then
                        l_lngMaxX = l_lngX
                        l_lngX = -hsScrollbar.Value
                        l_lngY = l_lngY + m_tstTileset.TileHeight
                        l_lngHeight = l_lngHeight + m_tstTileset.TileHeight
                    End If
                End If
                l_lngTile = l_lngTile + 1
            Loop
            Set l_imgTile = Nothing
        End If
    End If
    
    l_lngMaxY = l_lngY
    If l_lngHeight > m_lngHeight Then
        l_lngMax = (l_lngHeight - m_lngHeight)
        If l_lngMax > 0 Then
            If (vsScrollbar.Enabled) Then Else vsScrollbar.Enabled = True
            If vsScrollbar.LargeChange <> m_tstTileset.TileHeight * 4 Then vsScrollbar.LargeChange = m_tstTileset.TileHeight * 4
            If vsScrollbar.SmallChange <> m_tstTileset.TileHeight Then vsScrollbar.SmallChange = m_tstTileset.TileHeight
            If vsScrollbar.Max <> l_lngMax Then vsScrollbar.Max = l_lngMax
        End If
    Else
        vsScrollbar.Value = 0
        vsScrollbar.Enabled = False
    End If
    If l_lngWidth > m_lngWidth Then
        l_lngMax = (l_lngWidth - m_lngWidth)
        If l_lngMax > 0 Then
            If (hsScrollbar.Enabled) Then Else hsScrollbar.Enabled = True
            If hsScrollbar.LargeChange <> m_tstTileset.TileWidth * 4 Then hsScrollbar.LargeChange = m_tstTileset.TileWidth * 4
            If hsScrollbar.SmallChange <> m_tstTileset.TileWidth Then hsScrollbar.SmallChange = m_tstTileset.TileWidth
            If hsScrollbar.Max <> l_lngMax Then hsScrollbar.Max = l_lngMax
        End If
    Else
        hsScrollbar.Value = 0
        hsScrollbar.Enabled = False
    End If
    UserControl.Line (l_lngMaxX, 0)-(m_lngWidth, m_lngHeight), UserControl.BackColor, BF
    UserControl.Line (0, l_lngMaxY)-(l_lngMaxX, m_lngHeight), UserControl.BackColor, BF
    If UserControl.AutoRedraw Then UserControl.Refresh
    vsScrollbar.Visible = vsScrollbar.Enabled
    hsScrollbar.Visible = hsScrollbar.Enabled
    l_lngWidth = UserControl.ScaleWidth - IIf(vsScrollbar.Visible, vsScrollbar.Width, 0)
    l_lngHeight = UserControl.ScaleHeight - IIf(hsScrollbar.Visible, hsScrollbar.Height, 0)
    If (l_lngWidth <> m_lngWidth) Or (l_lngHeight <> m_lngHeight) Then
        m_lngWidth = l_lngWidth
        m_lngHeight = l_lngHeight
        UserControl_Resize
    End If
End Sub

Public Property Get Tileset() As Fury2Tileset
    Set Tileset = m_tstTileset
End Property

Public Property Set Tileset(NewTileset As Fury2Tileset)
    Set m_tstTileset = NewTileset
    RebuildCache
    Redraw
End Property

Private Sub hsScrollbar_Change()
    Redraw
End Sub

Private Sub UserControl_Hide()
    DeallocateBackbuffer
End Sub

Private Sub UserControl_Initialize()
    ReDim m_intSelectedTiles(0 To 0)
    m_intSelectedTiles(0) = -1
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button = 1 Then
        ReDim m_intSelectedTiles(0 To 0)
        m_intSelectedTiles(0) = HitTest(X, Y)
        RaiseEvent SelectionChanged(m_intSelectedTiles(0))
    ElseIf Button = 2 Then
        Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, Menus(MenuString("Preserve Rows", , , , , m_booPreserveRows)))
        Case 1
            m_booPreserveRows = Not m_booPreserveRows
            Redraw
        Case Else
        End Select
    End If
End Sub

Private Sub UserControl_Paint()
    If Not UserControl.AutoRedraw Then Redraw
End Sub

Private Sub UserControl_Resize()
'    If m_imgBuffer Is Nothing Then
'    Else
'        m_imgBuffer.Resize UserControl.ScaleWidth - vsScrollbar.Width, UserControl.ScaleHeight - hsScrollbar.Width
'        Redraw
'    End If
    vsScrollbar.Move UserControl.ScaleWidth - (GetScrollbarSize(vsScrollbar) + 1), 0, GetScrollbarSize(vsScrollbar) + 1, UserControl.ScaleHeight - IIf(hsScrollbar.Visible, hsScrollbar.Height, 0)
    hsScrollbar.Move 0, UserControl.ScaleHeight - (GetScrollbarSize(hsScrollbar) + 1), UserControl.ScaleWidth - IIf(vsScrollbar.Visible, vsScrollbar.Width, 0), GetScrollbarSize(hsScrollbar) + 1
    vsScrollbar.Visible = vsScrollbar.Enabled
    hsScrollbar.Visible = hsScrollbar.Enabled
    m_lngWidth = UserControl.ScaleWidth - IIf(vsScrollbar.Visible, vsScrollbar.Width, 0)
    m_lngHeight = UserControl.ScaleHeight - IIf(hsScrollbar.Visible, hsScrollbar.Height, 0)
    Redraw
End Sub

Private Sub UserControl_Show()
    AllocateBackbuffer
    Redraw
End Sub

Private Sub UserControl_Terminate()
    DeallocateBackbuffer
End Sub

Private Sub vsScrollbar_Change()
    Redraw
End Sub

