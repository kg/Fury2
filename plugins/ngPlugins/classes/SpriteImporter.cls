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
Implements iPlugin
Private m_objEditor As Object

Public Sub ImportSprite(ByRef Options As SpriteImporterOptions)
On Error Resume Next
Dim l_imgImage As Fury2Image
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngCellsX As Long, l_lngCellsY As Long
Dim l_posPose As Fury2Pose, l_fraFrame As Fury2PoseFrame
Dim l_lngX As Long, l_lngY As Long
Dim l_lngInsertionPoint As Long
Dim l_plgPlugin As iFileTypePlugin
Dim l_docSprites As frmSprites
Dim l_sprSprite As Fury2Sprite
Dim l_booNewPose As Boolean
Dim l_lngPose As Long
    Set l_docSprites = m_objEditor.ActiveDocument
    If l_docSprites Is Nothing Then
        Set l_plgPlugin = m_objEditor.FindFileTypePlugin("Sprite Collection")
        Set l_docSprites = l_plgPlugin.CreateNew
        m_objEditor.NewDocument l_docSprites
    End If
    Set l_sprSprite = l_docSprites.SelectedSprite
    If l_sprSprite Is Nothing Then
        Set l_sprSprite = l_docSprites.InsertSprite
    End If
    With Options
'        Set l_imgImage = F2LoadImage(Options.Image)
        Set l_imgImage = DefaultEngine.LoadImage(Options.Image, True)
        If l_imgImage Is Nothing Then
            m_objEditor.ShowNotice "Error", "Unable to load image.", m_objEditor.NoticeIcon("error")
            Exit Sub
        End If
        l_lngWidth = l_imgImage.Width - .MarginLeft - .MarginRight
        l_lngHeight = l_imgImage.Height - .MarginTop - .MarginBottom
        l_lngCellsX = ClipValue(Ceil(l_lngWidth / (.CellWidth + .GridWidth)), 0, .MaxFrames)
        l_lngCellsY = ClipValue(Ceil(l_lngHeight / (.CellHeight + .GridHeight)), 0, IIf(.MultiRowPoses, 999, .MaxPoses))
        l_lngInsertionPoint = l_docSprites.SelectedSprite.Poses.Find(l_docSprites.SelectedPose)
        l_booNewPose = True
        For l_lngY = 0 To l_lngCellsY - 1
            If (.MultiRowPoses) Then
            Else
                l_booNewPose = True
            End If
            For l_lngX = 0 To l_lngCellsX - 1
                If l_booNewPose Then
                    l_booNewPose = False
                    l_lngPose = l_lngPose + 1
                    Set l_posPose = New Fury2Pose
                    l_posPose.Name = "Imported Pose #" & l_lngPose
                    l_docSprites.SelectedSprite.Poses.Add l_posPose, , l_lngInsertionPoint + l_lngPose
                End If
                Set l_fraFrame = New Fury2PoseFrame
                Set l_fraFrame.Rectangle = F2Rect(l_lngX * (.CellWidth + .GridWidth) + .MarginLeft, l_lngY * (.CellHeight + .GridHeight) + .MarginTop, .CellWidth, .CellHeight, False)
                l_fraFrame.XCenter = .AlignmentX
                l_fraFrame.YCenter = .AlignmentY
                l_fraFrame.Delay = .FrameDelay
                l_fraFrame.Filename = Options.Image
                l_fraFrame.MatteColor = .MatteColor
                l_posPose.Frames.Add l_fraFrame
                If (l_posPose.Frames.Count = 1) And (.DuplicateFirstFrame) Then
                    l_posPose.Frames.Add l_fraFrame.Duplicate
                End If
                If l_posPose.Frames.Count >= .MaxFrames Then
                    If .MultiRowPoses Then
                        l_booNewPose = True
                    End If
                End If
            Next l_lngX
        Next l_lngY
        l_docSprites.SelectedSprite.Initialize
        l_docSprites.SelectedSprite.Load
        l_docSprites.ReloadGraphics
        l_docSprites.PosesViewChanged
        l_docSprites.FramesViewChanged
        l_docSprites.Redraw
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
    Load frmSpriteImporter
    m_objEditor.LoadSettings "SpriteImporter\Settings", frmSpriteImporter.Options
    frmSpriteImporter.RefreshSettings
    frmSpriteImporter.Show vbModal
    If Not (frmSpriteImporter.Options Is Nothing) Then
        m_objEditor.SaveSettings "SpriteImporter\Settings", frmSpriteImporter.Options
        ImportSprite frmSpriteImporter.Options
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

Private Property Get iPlugin_ToolbarIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iPlugin_ToolbarIcon = m_objEditor.LoadResources("ng").ItemData("icons\sprite importer.png")
End Property

