VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SpriteImporter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements iPlugin
Private m_objEditor As Object

Public Sub ImportSprite(ByRef Document As iDocument, ByRef Options As SpriteImporterOptions)
On Error Resume Next
Dim l_imgImage As Fury2Image
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngCellsX As Long, l_lngCellsY As Long
Dim l_posPose As Fury2Pose, l_fraFrame As Fury2PoseFrame
Dim l_lngX As Long, l_lngY As Long
Dim l_lngInsertionPoint As Long
Dim Doc As frmSprites
    Set Doc = Document
    With Options
'        Set l_imgImage = F2LoadImage(Options.Image)
        Set l_imgImage = DefaultEngine.LoadImage(Options.Image, True)
        If l_imgImage Is Nothing Then
            MsgBox "Unable to load image.", vbExclamation, "Error"
            Exit Sub
        End If
        l_lngWidth = l_imgImage.Width
        l_lngHeight = l_imgImage.Height
        l_lngCellsX = l_lngWidth \ (.CellWidth)
        l_lngCellsY = l_lngHeight \ (.CellHeight)
        l_lngInsertionPoint = Doc.SelectedSprite.Poses.Find(Doc.SelectedPose)
        For l_lngY = 0 To l_lngCellsY - 1
            Set l_posPose = New Fury2Pose
            For l_lngX = 0 To l_lngCellsX - 1
                Set l_fraFrame = New Fury2PoseFrame
                Set l_fraFrame.Rectangle = F2Rect(l_lngX * .CellWidth, l_lngY * .CellHeight, .CellWidth, .CellHeight, False)
                l_fraFrame.XCenter = .AlignmentX
                l_fraFrame.YCenter = .AlignmentY
                l_fraFrame.Delay = .FrameDelay
                l_fraFrame.Filename = Options.Image
                l_fraFrame.MatteColor = .MatteColor
                l_posPose.Frames.Add l_fraFrame
                If (l_lngX = 0) And (.DuplicateFirstFrame) Then
                    l_posPose.Frames.Add l_fraFrame.Duplicate
                End If
            Next l_lngX
            l_posPose.Name = "Imported Pose #" & l_lngY
            Doc.SelectedSprite.Poses.Add l_posPose, , l_lngInsertionPoint + l_lngY + 1
        Next l_lngY
        Doc.SelectedSprite.Initialize
        Doc.SelectedSprite.Load
        Doc.ReloadGraphics
        Doc.PosesViewChanged
        Doc.FramesViewChanged
        Doc.Redraw
    End With
End Sub

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Sub iPlugin_Activate()
On Error Resume Next
Dim l_plgPlugin As iFileTypePlugin
Dim l_docDocument As iDocument
Dim l_docSprites As frmSprites
    Set l_docDocument = m_objEditor.ActiveDocument
    Set l_docSprites = l_docDocument
    If l_docSprites Is Nothing Then
        MsgBox "You must have a sprite collection open", vbExclamation, "Error"
        Exit Sub
    End If
    Load frmSpriteImporter
    m_objEditor.LoadSettings "SpriteImporter\Settings", frmSpriteImporter.Options
    frmSpriteImporter.RefreshSettings
    frmSpriteImporter.Show vbModal
    If Not (frmSpriteImporter.Options Is Nothing) Then
        m_objEditor.SaveSettings "SpriteImporter\Settings", frmSpriteImporter.Options
        If l_docSprites.SelectedSprite Is Nothing Then
            MsgBox "You must have a sprite selected", vbExclamation, "Error"
            Exit Sub
        End If
        ImportSprite l_docSprites, frmSpriteImporter.Options
    End If
    Unload frmSpriteImporter
    Err.Clear
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As stdole.Picture
    Set iPlugin_Icon = LoadResPicture("SPRITES", vbResIcon)
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Imports one or more sequences of sprite frames from an image"
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "Sprite Importer"
End Property

Private Property Get iPlugin_PluginPath() As String
On Error Resume Next
    iPlugin_PluginPath = App.Path & IIf(Right(App.Path, 1) = "\", "ng.dll", "\ng.dll")
End Property

Private Property Get iPlugin_ShowInPluginMenu() As Boolean
    iPlugin_ShowInPluginMenu = True
End Property

Private Sub iPlugin_Shutdown()
End Sub


