VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TKBoardImporter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements iPlugin
Private m_objEditor As Object

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Public Function ImportBoard(ByRef Options As TKBoardImporterOptions) As Fury2Map
On Error Resume Next
Dim l_brdBoard As TKBoard
Dim l_mapNew As Fury2Map
Dim l_tstTileset As Fury2Tileset
Dim l_lngLayers As Long, l_lngX As Long, l_lngY As Long, l_lngIndex As Long
Dim l_imgTile As Fury2Image
Dim l_tsTileset As TSTFile
Dim l_tleTile As TSTTile
Dim l_strTile As String, l_strIndex As String
    Commonboard.openBoard Options.Filename, l_brdBoard
    Set l_tstTileset = DefaultEngine.F2Tileset(, 32, 32)
    With l_tstTileset
        .TileWidth = 32
        .TileHeight = 32
        .HasAlpha = True
        .TransparentTile = 0
        .Embed = True
    End With
    Set l_mapNew = DefaultEngine.F2Map()
    l_mapNew.EditMode = True
    Set l_tsTileset = New TSTFile
    With l_brdBoard
        l_tstTileset.Add F2Image(32, 32)
        For l_lngIndex = LBound(.tileIndex) To UBound(.tileIndex)
            l_strTile = Trim(.tileIndex(l_lngIndex))
            If Len(l_strTile) > 0 Then
                If InStr(l_strTile, ".tst") Then
                    l_strIndex = Mid(l_strTile, InStrRev(l_strTile, ".tst") + 4)
                    l_strTile = Left(l_strTile, InStrRev(l_strTile, ".tst") + 3)
                    If (l_tsTileset.Filename <> l_strTile) Then
                        l_tsTileset.Filename = l_strTile
                        l_tsTileset.Load GetPath(Options.Filename) & "\..\tiles\" & l_strTile
                    End If
                    Set l_tleTile = l_tsTileset.Tiles(CLng(l_strIndex))
                    Set l_imgTile = l_tleTile.Image
                    l_imgTile.FillChannel l_imgTile.Rectangle, 3, 255
                    l_imgTile.ReplaceColor F2RGB(0, 1, 2), 0
                    l_tstTileset.Add l_imgTile
                End If
            End If
        Next l_lngIndex
        l_mapNew.CreateNew .boardTitle(0), .bSizeX, .bSizeY, .bSizeL, l_tstTileset
        For l_lngLayers = 1 To .bSizeL
            l_mapNew.Layers(l_lngLayers).Effect = F2LE_Alpha
            For l_lngY = 1 To .bSizeY
                For l_lngX = 1 To .bSizeX
                    l_mapNew.Layers(l_lngLayers).Tile(l_lngX - 1, l_lngY - 1) = .board(l_lngX, l_lngY, l_lngLayers)
                Next l_lngX
            Next l_lngY
        Next l_lngLayers
    End With
    Set ImportBoard = l_mapNew
End Function

Private Sub iPlugin_Activate()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
Dim l_docDocument As iDocument
    Load frmImportTKBoard
    m_objEditor.LoadSettings "TKBoardImporter\Settings", frmImportTKBoard.Options
    frmImportTKBoard.RefreshSettings
    frmImportTKBoard.Show vbModal
    If Not (frmImportTKBoard.Options Is Nothing) Then
        m_objEditor.SaveSettings "TKBoardImporter\Settings", frmImportTKBoard.Options
        Set l_plgPlugin = m_objEditor.FindFileTypePlugin("Map")
        Set l_docDocument = l_plgPlugin.CreateNew(False)
        With l_docDocument.Object
            .SetFilename ""
            .RefreshAll
            .SetMap ImportBoard(frmImportTKBoard.Options)
            m_objEditor.LoadSettings "MapEditor\View", .ViewOptions
            .ViewChanged
        End With
        m_objEditor.NewDocument l_docDocument
    End If
    Unload frmImportTKBoard
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As Fury2Image
On Error Resume Next
    Set iPlugin_Icon = LoadResPicture("TILESET", vbResIcon)
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Imports an RPG Toolkit Board"
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "RPG Toolkit Board Importer"
End Property

Private Property Get iPlugin_PluginPath() As String
On Error Resume Next
    iPlugin_PluginPath = App.Path & IIf(Right(App.Path, 1) = "\", "tk.dll", "\tk.dll")
End Property

Private Property Get iPlugin_ShowInPluginMenu() As Boolean
    iPlugin_ShowInPluginMenu = True
End Property

Private Sub iPlugin_Shutdown()
End Sub

Private Property Get iPlugin_ToolbarIcon() As libGraphics.Fury2Image
On Error Resume Next
End Property

