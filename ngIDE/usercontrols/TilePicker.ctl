VERSION 5.00
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#9.0#0"; "ngUI.ocx"
Begin VB.UserControl TilePicker 
   AutoRedraw      =   -1  'True
   BackColor       =   &H80000014&
   ClientHeight    =   4125
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   1785
   KeyPreview      =   -1  'True
   ScaleHeight     =   275
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   119
   Begin VB.Timer tmrRefreshToolbar 
      Interval        =   1000
      Left            =   285
      Top             =   1815
   End
   Begin ngUI.ngToolbar tbrTileset 
      Height          =   360
      Left            =   0
      TabIndex        =   2
      Top             =   0
      Width           =   765
      _ExtentX        =   1349
      _ExtentY        =   635
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
      Height          =   3855
      Left            =   1545
      TabIndex        =   0
      Top             =   60
      Width           =   240
      _ExtentX        =   423
      _ExtentY        =   6800
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
Public Event SelectionChanged(Tiles() As Integer)
Public Event TileHover(Tile As Integer)
Public Event TilesetModified()

Public Editor As Object
Public ResourceFile As ngResourceFile
Public Clipboard As cCustomClipboard
Public BlitMode As SFXBlitModes
Private m_lngClipboardFormat As Long
Private m_imgBuffer As Fury2Image
Private m_lngBuffer As Long
Private m_lngWidth As Long, m_lngHeight As Long
Private m_lngStartX As Long, m_lngStartY As Long
Private m_tstTileset As Fury2Tileset
Private m_booPreserveRows As Boolean
Private m_booTileIsSelected() As Boolean
Private m_intSelectedTiles() As Integer

Friend Sub CutTile(ByVal Index As Long)
On Error Resume Next
    CopyTile Index
    DeleteTile Index
End Sub

Friend Sub CopyTile(ByVal Index As Long)
On Error Resume Next
    If m_lngClipboardFormat = 0 Then
        m_lngClipboardFormat = Clipboard.AddFormat("Fury2Image")
    End If
    Clipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize Clipboard, m_lngClipboardFormat, m_tstTileset.Tile(Index + 1)
    Clipboard.ClipboardClose
End Sub

Friend Sub PasteTile(ByVal AtIndex As Long)
On Error Resume Next
Dim l_imgTile As Fury2Image
    If m_lngClipboardFormat = 0 Then
        m_lngClipboardFormat = Clipboard.AddFormat("Fury2Image")
    End If
    Clipboard.ClipboardOpen Me.hwnd
    Set l_imgTile = New Fury2Image
    If ClipboardDeserialize(Clipboard, m_lngClipboardFormat, l_imgTile) Then
        SoftFX.InsertTile m_tstTileset.Handle, l_imgTile.Handle, AtIndex
        TilesetModified
    End If
    Clipboard.ClipboardClose
End Sub

Friend Sub DeleteTile(ByVal Index As Long)
On Error Resume Next
    m_tstTileset.Remove Index + 1
    TilesetModified
End Sub

Private Property Get CanPaste() As Boolean
On Error Resume Next
    If m_lngClipboardFormat = 0 Then
        m_lngClipboardFormat = Clipboard.AddFormat("Fury2Image")
    End If
    Clipboard.GetCurrentFormats UserControl.hwnd
    CanPaste = Clipboard.HasCurrentFormat(m_lngClipboardFormat)
End Property

Friend Sub TilesetModified()
On Error Resume Next
    m_lngBuffer = 0
    RebuildCache
    UserControl_Resize
    Redraw
    RaiseEvent TilesetModified
End Sub

Public Sub RebuildCache()
On Error Resume Next
'Dim l_imgTilesetCache As Fury2Image
'    m_lngBuffer = m_tstTileset.Handle
'    Set l_imgTilesetCache = m_tstTileset.GenerateBuffer
'    With l_imgTilesetCache
'        If .AlphaChannel Then
'            .Composite SwapChannels(GetSystemColor(SystemColor_Button_Highlight), Blue, Red)
'        Else
'            .ReplaceColor .Rectangle, m_tstTileset.MaskColor, SwapChannels(GetSystemColor(SystemColor_Button_Highlight), Blue, Red)
'        End If
'    End With
'    Set picTilesetCache.Picture = l_imgTilesetCache.Picture
End Sub

Public Function HitTest(ByVal X As Long, ByVal Y As Long) As Integer
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngSX As Long, l_lngSY As Long
Dim l_lngHeight As Long, l_lngMax As Long
Dim l_lngWidth As Long
Dim l_lngTile As Long
Dim l_lngRowWidth As Long
    HitTest = -1
    If Not (m_tstTileset Is Nothing) Then
        If (m_tstTileset.TileWidth > 0) And (m_tstTileset.TileHeight > 0) Then
            l_lngY = -vsScrollbar.Value + tbrTileset.Height
            l_lngX = -hsScrollbar.Value
            If PreserveRows Then
                l_lngRowWidth = m_tstTileset.RowWidth
            Else
                l_lngRowWidth = m_lngWidth \ m_tstTileset.TileWidth
            End If
            If (X > (l_lngRowWidth * m_tstTileset.TileWidth)) Then Exit Function
            l_lngWidth = l_lngRowWidth * m_tstTileset.TileWidth
            l_lngHeight = m_tstTileset.TileCount / (l_lngWidth \ m_tstTileset.TileWidth) * m_tstTileset.TileHeight
            l_lngTile = ((X - l_lngX) \ m_tstTileset.TileWidth) + (((Y - l_lngY) \ m_tstTileset.TileHeight) * (l_lngRowWidth))
            If (l_lngTile >= 0) And (l_lngTile < m_tstTileset.TileCount) Then HitTest = l_lngTile
        End If
    End If
'    #If 0 Then
'    Y = Y - IIf(tbrTileset.Visible, tbrTileset.Height, 0)
'    l_lngY = -vsScrollbar.Value
'    l_lngX = -hsScrollbar.Value
'    HitTest = -1
'
'    If Not (m_tstTileset Is Nothing) Then
'        Do Until ((l_lngSY + m_tstTileset.TileHeight) > picTilesetCache.Height)
'            If (Y >= l_lngY) And (Y < (l_lngY + m_tstTileset.TileHeight)) Then
'                If (X >= l_lngX) And (X < (l_lngX + m_tstTileset.TileWidth)) Then
'                    HitTest = l_lngTile
'                    Exit Do
'                End If
'            End If
'
'            l_lngSX = l_lngSX + m_tstTileset.TileWidth
'            If (l_lngSX + m_tstTileset.TileWidth) > picTilesetCache.ScaleWidth Then
'                l_lngSX = 0
'                l_lngSY = l_lngSY + m_tstTileset.TileHeight
'                If m_booPreserveRows Then
'                    l_lngX = -hsScrollbar.Value - m_tstTileset.TileWidth
'                    l_lngY = l_lngY + m_tstTileset.TileHeight
'                End If
'            End If
'
'            l_lngX = l_lngX + m_tstTileset.TileWidth
'            If m_booPreserveRows Then
'            Else
'                If (l_lngX + m_tstTileset.TileWidth) > (m_lngWidth) Then
'                    l_lngX = -hsScrollbar.Value
'                    l_lngY = l_lngY + m_tstTileset.TileHeight
'                End If
'            End If
'            l_lngTile = l_lngTile + 1
'        Loop
'    End If
'    #End If
End Function

Public Property Get hwnd() As Long
    hwnd = UserControl.hwnd
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
Dim l_lngY As Long, l_lngX As Long, l_lngI As Long
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
            For l_lngX = LBound(Tiles, 1) To UBound(Tiles, 1)
                m_intSelectedTiles(l_lngI) = Tiles(l_lngX, l_lngY)
                l_lngI = l_lngI + 1
            Next l_lngX
        Next l_lngY
    Else
        m_intSelectedTiles = Tiles
    End If
    ReDim m_booTileIsSelected(0 To m_tstTileset.TileCount - 1)
    For l_lngI = LBound(m_intSelectedTiles) To UBound(m_intSelectedTiles)
        m_booTileIsSelected(m_intSelectedTiles(l_lngI)) = True
    Next l_lngI
    Redraw
End Sub

Private Sub AllocateBackbuffer()
On Error Resume Next
    DeallocateBackbuffer
    Set m_imgBuffer = F2DIBSection(UserControl.ScaleWidth, UserControl.ScaleHeight, UserControl.hdc)
    m_lngBuffer = SelectObject(UserControl.hdc, m_imgBuffer.DIBHandle)
End Sub

Private Sub DeallocateBackbuffer()
On Error Resume Next
    SelectObject UserControl.hdc, m_lngBuffer
    m_lngBuffer = 0
    Set m_imgBuffer = Nothing
End Sub

Public Sub Redraw()
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_lngSX As Long, l_lngSY As Long
Dim l_lngHeight As Long, l_lngMax As Long
Dim l_lngWidth As Long
Dim l_lngTile As Long, l_lngOffset As Long
Dim l_lngMaxX As Long, l_lngMaxY As Long
Dim l_lngSelectedTiles As Long
Dim l_lngRowWidth As Long
Dim l_booSelected As Boolean
Dim l_imgTile As Fury2Image
Dim l_rctTile As Fury2Rect
Dim l_lngBackgroundColor As Long, l_lngHighlightColor As Long
    
    l_lngBackgroundColor = SwapChannels(GetSystemColor(SystemColor_Button_Face), Blue, Red)
    l_lngHighlightColor = SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Blue, Red), 127)
    m_imgBuffer.Clear l_lngBackgroundColor
    If Not (m_tstTileset Is Nothing) Then
        l_lngMaxX = m_tstTileset.TileWidth
        l_lngMaxY = m_tstTileset.TileHeight
        If UBound(m_booTileIsSelected) <> (m_tstTileset.TileCount - 1) Then
            ReDim Preserve m_booTileIsSelected(0 To m_tstTileset.TileCount - 1)
        End If
        If (m_tstTileset.TileWidth > 0) And (m_tstTileset.TileHeight > 0) Then
            Set l_imgTile = New Fury2Image
            Set l_rctTile = F2Rect(0, 0, m_tstTileset.TileWidth, m_tstTileset.TileHeight, False)
            l_imgTile.Deallocate
            l_lngOffset = (vsScrollbar.Value \ m_tstTileset.TileHeight)
            l_lngY = -vsScrollbar.Value + (l_lngOffset * m_tstTileset.TileHeight) + tbrTileset.Height
            l_lngX = -hsScrollbar.Value
            If PreserveRows Then
                l_lngRowWidth = m_tstTileset.RowWidth
            Else
                l_lngRowWidth = m_lngWidth \ m_tstTileset.TileWidth
            End If
            l_lngOffset = l_lngOffset * l_lngRowWidth
            l_lngWidth = l_lngRowWidth * m_tstTileset.TileWidth
            l_lngHeight = m_tstTileset.TileCount / (l_lngWidth \ m_tstTileset.TileWidth) * m_tstTileset.TileHeight
            For l_lngTile = l_lngOffset To m_tstTileset.TileCount - 1
                l_imgTile.SetHandle GetTile(m_tstTileset.Handle, l_lngTile)
                l_rctTile.RelLeft = l_lngX
                l_rctTile.RelTop = l_lngY
                If m_booTileIsSelected(l_lngTile) Then
                    m_imgBuffer.Blit l_rctTile, Nothing, l_imgTile, , BlitMode
                    m_imgBuffer.Fill l_rctTile, l_lngHighlightColor, RenderMode_SourceAlpha
                    m_imgBuffer.Box l_rctTile.Adjust(-1, -1), F2White
                    m_imgBuffer.Box l_rctTile.Adjust(1, 1), F2Black
                Else
                    m_imgBuffer.Blit l_rctTile, Nothing, l_imgTile, , BlitMode
                End If
                l_lngX = l_lngX + m_tstTileset.TileWidth
                If (l_lngX + m_tstTileset.TileWidth) > (l_lngRowWidth * m_tstTileset.TileWidth) Then
                    l_lngX = -hsScrollbar.Value
                    l_lngY = l_lngY + m_tstTileset.TileHeight
                End If
                If (l_lngY) > (m_lngHeight + m_tstTileset.TileHeight + tbrTileset.Height) Then
                    Exit For
                End If
            Next l_lngTile
'            Do Until ((l_lngSY + m_tstTileset.TileHeight) > picTilesetCache.Height)
'                If l_lngMaxX < (l_lngX + m_tstTileset.TileWidth) Then l_lngMaxX = (l_lngX + m_tstTileset.TileWidth)
'                If l_lngMaxY < (l_lngY + m_tstTileset.TileHeight) Then l_lngMaxY = (l_lngY + m_tstTileset.TileHeight)
'                If (l_lngSY + m_tstTileset.TileHeight) > 0 Then
'                    l_booSelected = False
'                    For l_lngSelectedTiles = 0 To UBound(m_intSelectedTiles)
'                        If m_intSelectedTiles(l_lngSelectedTiles) = l_lngTile Then
'                            l_booSelected = True
'                            Exit For
'                        End If
'                    Next l_lngSelectedTiles
'                    If l_booSelected Then
'                        l_imgTile.Copy m_tstTileset.Tile(l_lngTile + 1)
'                        l_imgTile.Composite l_lngBackgroundColor
'                        l_imgTile.Box l_imgTile.Rectangle.Adjust(-1, -1), F2RGB(0, 0, 0, 160), RenderMode_SourceAlpha
'                        l_imgTile.Box l_imgTile.Rectangle, F2RGB(255, 255, 255, 160), RenderMode_SourceAlpha
'                        CopyImageToDC UserControl.hdc, F2Rect(l_lngX, l_lngY + IIf(tbrTileset.Visible, tbrTileset.Height, 0), m_tstTileset.TileWidth, m_tstTileset.TileHeight, False), l_imgTile
'                    Else
'                        BitBlt UserControl.hdc, l_lngX, l_lngY + IIf(tbrTileset.Visible, tbrTileset.Height, 0), m_tstTileset.TileWidth, m_tstTileset.TileHeight, picTilesetCache.hdc, l_lngSX, l_lngSY, vbSrcCopy
'                    End If
'                    'm_imgBuffer.Blit F2Rect(l_lngX, l_lngY, m_tstTileset.TileWidth, m_tstTileset.TileHeight, False), F2Rect(l_lngSX, l_lngSY, m_tstTileset.TileWidth, m_tstTileset.TileHeight, False), m_imgTilesetCache
'                End If
'
'                l_lngSX = l_lngSX + m_tstTileset.TileWidth
'                If (l_lngSX + m_tstTileset.TileWidth) > (picTilesetCache.ScaleWidth) Then
'                    l_lngSX = 0
'                    l_lngSY = l_lngSY + m_tstTileset.TileHeight
'                    If m_booPreserveRows Then
'                        l_lngX = -hsScrollbar.Value - m_tstTileset.TileWidth
'                        l_lngY = l_lngY + m_tstTileset.TileHeight
'                        l_lngHeight = l_lngHeight + m_tstTileset.TileHeight
'                    End If
'                End If
'
'                l_lngX = l_lngX + m_tstTileset.TileWidth
'                If m_booPreserveRows Then
'                    If (l_lngX + hsScrollbar.Value + m_tstTileset.TileWidth) > l_lngWidth Then
'                        l_lngWidth = (l_lngX + hsScrollbar.Value + m_tstTileset.TileWidth)
'                    End If
'                Else
'                    If (l_lngX + m_tstTileset.TileWidth) > (m_lngWidth) Then
'                        l_lngX = -hsScrollbar.Value
'                        l_lngY = l_lngY + m_tstTileset.TileHeight
'                        l_lngHeight = l_lngHeight + m_tstTileset.TileHeight
'                    End If
'                    If (l_lngX + hsScrollbar.Value + m_tstTileset.TileWidth) > l_lngWidth Then
'                        l_lngWidth = (l_lngX + hsScrollbar.Value + m_tstTileset.TileWidth)
'                    End If
'                End If
'                l_lngTile = l_lngTile + 1
'            Loop
            Set l_imgTile = Nothing
        End If
    End If
    
    If l_lngHeight > m_lngHeight Then
        l_lngMax = l_lngHeight - m_lngHeight - 1
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
        l_lngMax = l_lngWidth - m_lngWidth - 1
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
    'UserControl.Line (l_lngMaxX, IIf(tbrTileset.Visible, tbrTileset.Height, 0))-(m_lngWidth, m_lngHeight + IIf(tbrTileset.Visible, tbrTileset.Height, 0)), UserControl.BackColor, BF
    'UserControl.Line (0, l_lngMaxY + IIf(tbrTileset.Visible, tbrTileset.Height, 0))-(l_lngMaxX, m_lngHeight + IIf(tbrTileset.Visible, tbrTileset.Height, 0)), UserControl.BackColor, BF
    If UserControl.AutoRedraw Then UserControl.Refresh
    vsScrollbar.Visible = vsScrollbar.Enabled
    hsScrollbar.Visible = hsScrollbar.Enabled
    l_lngWidth = UserControl.ScaleWidth - IIf(vsScrollbar.Visible, vsScrollbar.Width, 0)
    l_lngHeight = UserControl.ScaleHeight - IIf(hsScrollbar.Visible, hsScrollbar.Height, 0) - IIf(tbrTileset.Visible, tbrTileset.Height, 0)
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
    ReDim m_booTileIsSelected(0 To m_tstTileset.TileCount - 1)
    RebuildCache
    Redraw
End Property

Private Sub hsScrollbar_Change()
    Redraw
End Sub

Private Sub tbrTileset_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
Dim l_strFilename As String
Dim l_imgTileset As Fury2Image
Dim l_plgPlugin As iFileTypePlugin
Dim l_docDocument As iDocument, l_frmDocument As frmTileset
    Select Case LCase(Trim(Button.key))
    Case "importtileset"
        l_strFilename = Editor.SelectFile("Tilesets|*.f2tileset;*.rts;*.vsp;*.png;*.gif;*.bmp;*.jpg;*.tga", "Import Tileset...")
        If Len(Trim(l_strFilename)) > 0 Then
            If InStr(l_strFilename, m_tstTileset.Engine.FileSystem.Root) Then
                m_tstTileset.Embed = False
                m_tstTileset.Filename = Replace(Replace(l_strFilename, m_tstTileset.Engine.FileSystem.Root, "/"), "\", "/")
                m_tstTileset.Reload
                TilesetModified
                RefreshToolbar
            Else
                Editor.ShowNotice "Warning", "Selected tileset not in game folder. Tiles embedded."
                Set l_imgTileset = F2LoadImage(l_strFilename)
                m_tstTileset.Embed = True
                m_tstTileset.RemoveAll
                If l_imgTileset Is Nothing Then
                    m_tstTileset.LoadTileset F2File(l_strFilename)
                Else
                    m_tstTileset.AddTiles l_imgTileset
                End If
                TilesetModified
                RefreshToolbar
            End If
        End If
    Case "exporttileset"
        Set l_plgPlugin = Editor.FindFileTypePlugin("Tileset")
        Set l_docDocument = l_plgPlugin.CreateNew(False)
        Set l_frmDocument = l_docDocument
        l_frmDocument.SetTileset m_tstTileset.Duplicate
        Editor.NewDocument l_docDocument
    Case "cut"
        CutTile m_intSelectedTiles(0)
    Case "copy"
        CopyTile m_intSelectedTiles(0)
    Case "paste"
        PasteTile m_intSelectedTiles(0)
    Case "delete"
        DeleteTile m_intSelectedTiles(0)
    Case Else
    End Select
End Sub

Private Sub tbrTileset_ButtonPress(Button As ngUI.ngToolButton, Cancel As Boolean)
On Error Resume Next
    Select Case LCase(Trim(Button.key))
    Case "embed"
        m_tstTileset.Embed = Not Button.Checked
        m_tstTileset.Reload
        TilesetModified
    Case Else
    End Select
End Sub

Private Sub tmrRefreshToolbar_Timer()
On Error Resume Next
    RefreshToolbar
End Sub

Private Sub UserControl_Hide()
    DeallocateBackbuffer
End Sub

Private Sub UserControl_Initialize()
On Error Resume Next
    tbrTileset.Visible = False
    ReDim m_intSelectedTiles(0 To 0)
    m_intSelectedTiles(0) = -1
    BlitMode = BlitMode_Normal
End Sub

Private Sub UserControl_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    Select Case KeyCode
    Case vbKeyLeft
        hsScrollbar.Value = hsScrollbar.Value - m_tstTileset.TileWidth
    Case vbKeyUp
        vsScrollbar.Value = vsScrollbar.Value - m_tstTileset.TileHeight
    Case vbKeyDown
        vsScrollbar.Value = vsScrollbar.Value + m_tstTileset.TileHeight
    Case vbKeyRight
        hsScrollbar.Value = hsScrollbar.Value + m_tstTileset.TileWidth
    Case Else
    End Select
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_booCanPaste As Boolean
Dim l_booEmbed As Boolean
Dim l_lngFormat As Long
Dim l_lngTile As Long
Dim l_imgTile As Fury2Image
Dim l_intSelectedTiles() As Integer
    ReDim l_intSelectedTiles(0 To 0, 0 To 0)
    l_intSelectedTiles(0, 0) = HitTest(X, Y)
    SetSelectedTiles l_intSelectedTiles
    If Button = 1 Then
        m_lngStartX = X
        m_lngStartY = Y
    ElseIf Button = 2 Then
        l_lngTile = HitTest(X, Y)
        l_booEmbed = m_tstTileset.Embed
        Select Case QuickShowMenu(Me, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
            Menus(MenuString("Preserve Rows", , , , , m_booPreserveRows), "-", _
            MenuString("Cu&t", , , "CUT", , , l_booEmbed), MenuString("&Copy", , , "COPY"), MenuString("&Paste", , , "PASTE", , , CanPaste And l_booEmbed), MenuString("&Delete", , , "DELETE", , , l_booEmbed)) _
            , frmIcons.ilContextMenus)
        Case 1
            m_booPreserveRows = Not m_booPreserveRows
            Cls
            Redraw
        Case 3
            CutTile l_lngTile
        Case 4
            CopyTile l_lngTile
        Case 5
            PasteTile l_lngTile
        Case 6
            DeleteTile l_lngTile
        Case Else
        End Select
    End If
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_intSelectedTiles() As Integer
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngX As Long, l_lngY As Long
    If Button = 1 Then
        l_lngWidth = Ceil((X - m_lngStartX) / m_tstTileset.TileWidth)
        l_lngHeight = Ceil((Y - m_lngStartY) / m_tstTileset.TileHeight)
        If l_lngWidth < 1 Then l_lngWidth = 1
        If l_lngHeight < 1 Then l_lngHeight = 1
        ReDim l_intSelectedTiles(0 To l_lngWidth - 1, 0 To l_lngHeight - 1)
        For l_lngY = 0 To l_lngHeight - 1
            For l_lngX = 0 To l_lngWidth - 1
                l_intSelectedTiles(l_lngX, l_lngY) = HitTest(m_lngStartX + (l_lngX * m_tstTileset.TileWidth), m_lngStartY + (l_lngY * m_tstTileset.TileHeight))
            Next l_lngX
        Next l_lngY
        SetSelectedTiles l_intSelectedTiles
    Else
        RaiseEvent TileHover(HitTest(X, Y))
    End If
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_intSelectedTiles() As Integer
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngX As Long, l_lngY As Long
    If Button = 1 Then
        l_lngWidth = Ceil((X - m_lngStartX) / m_tstTileset.TileWidth)
        l_lngHeight = Ceil((Y - m_lngStartY) / m_tstTileset.TileHeight)
        If l_lngWidth < 1 Then l_lngWidth = 1
        If l_lngHeight < 1 Then l_lngHeight = 1
        ReDim l_intSelectedTiles(0 To l_lngWidth - 1, 0 To l_lngHeight - 1)
        For l_lngY = 0 To l_lngHeight - 1
            For l_lngX = 0 To l_lngWidth - 1
                l_intSelectedTiles(l_lngX, l_lngY) = HitTest(m_lngStartX + (l_lngX * m_tstTileset.TileWidth), m_lngStartY + (l_lngY * m_tstTileset.TileHeight))
            Next l_lngX
        Next l_lngY
        SetSelectedTiles l_intSelectedTiles
        RaiseEvent SelectionChanged(l_intSelectedTiles)
    End If
End Sub

Private Sub UserControl_Paint()
On Error Resume Next
    If Not UserControl.AutoRedraw Then Redraw
End Sub

Private Sub UserControl_Resize()
On Error Resume Next
    DeallocateBackbuffer
    AllocateBackbuffer
    tbrTileset.Move 0, 0, UserControl.ScaleWidth, tbrTileset.IdealHeight
    vsScrollbar.Move UserControl.ScaleWidth - (GetScrollbarSize(vsScrollbar) + 1), tbrTileset.Height, GetScrollbarSize(vsScrollbar) + 1, UserControl.ScaleHeight - IIf(hsScrollbar.Visible, hsScrollbar.Height, 0) - IIf(tbrTileset.Visible, tbrTileset.Height, 0)
    hsScrollbar.Move 0, UserControl.ScaleHeight - (GetScrollbarSize(hsScrollbar) + 1), UserControl.ScaleWidth - IIf(vsScrollbar.Visible, vsScrollbar.Width, 0), GetScrollbarSize(hsScrollbar) + 1
    vsScrollbar.Visible = vsScrollbar.Enabled
    hsScrollbar.Visible = hsScrollbar.Enabled
    m_lngWidth = UserControl.ScaleWidth - IIf(vsScrollbar.Visible, vsScrollbar.Width, 0)
    m_lngHeight = UserControl.ScaleHeight - IIf(hsScrollbar.Visible, hsScrollbar.Height, 0) - IIf(tbrTileset.Visible, tbrTileset.Height, 0)
    Redraw
End Sub

Public Sub InitToolbar()
On Error Resume Next
    Set tbrTileset.ResourceFile = ResourceFile
    tbrTileset.ResourcePattern = "map editor\tile picker\*.png"
    With tbrTileset.Buttons
        .Clear
        .AddNew , "ImportTileset", "import", "Import Tileset"
        .AddNew , "ExportTileset", "export", "Export Tileset"
        .AddNew "-"
        .AddNew , "Cut", "cut", "Cut Tile"
        .AddNew , "Copy", "copy", "Copy Tile"
        .AddNew , "Paste", "paste", "Paste Tile"
        .AddNew , "Delete", "delete", "Delete Tile"
        .AddNew "-"
        .AddNew , "Embed", "embed", "Embed Tileset", bsyCheck
    End With
    RefreshToolbar
    tbrTileset.Visible = True
    UserControl_Resize
End Sub

Public Sub RefreshToolbar()
On Error Resume Next
Dim l_booEmbed As Boolean
    l_booEmbed = m_tstTileset.Embed
    With tbrTileset.Buttons
        .Item("Cut").Enabled = l_booEmbed
        .Item("Paste").Enabled = l_booEmbed And CanPaste
        .Item("Delete").Enabled = l_booEmbed
        .Item("Embed").Checked = l_booEmbed
    End With
End Sub

Private Sub UserControl_Show()
On Error Resume Next
    AllocateBackbuffer
    Redraw
End Sub

Private Sub UserControl_Terminate()
    DeallocateBackbuffer
End Sub

Private Sub vsScrollbar_Change()
    Redraw
End Sub

