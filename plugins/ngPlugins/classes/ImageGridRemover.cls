VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ImageGridRemover"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements iPlugin
Private m_objEditor As Object

Public Function RemoveGrid(ByRef Options As ImageGridRemoverOptions) As Fury2Image
On Error Resume Next
Dim l_imgImage As Fury2Image
Dim l_imgNew As Fury2Image
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_lngCellsX As Long, l_lngCellsY As Long
Dim l_lngX As Long, l_lngY As Long
Dim l_rctSource As Fury2Rect, l_rctDest As Fury2Rect
    With Options
'        Set l_imgImage = F2LoadImage(Options.Image)
        Set l_imgImage = Engine.Engine.LoadImage(Options.Image, True)
        If l_imgImage Is Nothing Then
            MsgBox "Unable to load image.", vbExclamation, "Error"
            Exit Function
        End If
        l_lngWidth = l_imgImage.Width
        l_lngHeight = l_imgImage.Height
        l_lngWidth = l_lngWidth - .MarginLeft - .MarginRight
        l_lngHeight = l_lngHeight - .MarginTop - .MarginBottom
        l_lngCellsX = l_lngWidth \ (.CellWidth + .GridWidth)
        l_lngCellsY = l_lngHeight \ (.CellHeight + .GridHeight)
        l_lngWidth = l_lngWidth - (l_lngCellsX * .GridWidth)
        l_lngHeight = l_lngHeight - (l_lngCellsY * .GridHeight)
        Set l_imgNew = F2Image(l_lngWidth, l_lngHeight)
        l_imgNew.AlphaChannel = l_imgImage.AlphaChannel
        For l_lngY = 0 To l_lngCellsY
            For l_lngX = 0 To l_lngCellsX
                Set l_rctDest = F2Rect(l_lngX * .CellWidth, l_lngY * .CellHeight, .CellWidth, .CellHeight, False)
                Set l_rctSource = F2Rect((l_lngX * (.CellWidth + .GridWidth)) + .MarginLeft, (l_lngY * (.CellHeight + .GridHeight)) + .MarginTop, .CellWidth, .CellHeight, False)
                l_imgNew.Blit l_rctDest, l_rctSource, l_imgImage, , BlitMode_Normal
            Next l_lngX
        Next l_lngY
        Set RemoveGrid = l_imgNew
    End With
End Function

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Sub iPlugin_Activate()
On Error Resume Next
Dim l_imgImage As Fury2Image
Dim l_plgPlugin As iFileTypePlugin
Dim l_docImage As frmImage
    Load frmGridRemover
    m_objEditor.LoadSettings "ImageGridRemover\Settings", frmGridRemover.Options
    frmGridRemover.RefreshSettings
    frmGridRemover.Show vbModal
    If Not (frmGridRemover.Options Is Nothing) Then
        m_objEditor.SaveSettings "ImageGridRemover\Settings", frmGridRemover.Options
        Set l_imgImage = RemoveGrid(frmGridRemover.Options)
        If l_imgImage Is Nothing Then
        Else
            Set l_plgPlugin = m_objEditor.FindFileTypePlugin("Image")
            Set l_docImage = l_plgPlugin.CreateNew
            l_docImage.SetImage l_imgImage
            m_objEditor.NewDocument l_docImage
        End If
    End If
    Unload frmGridRemover
    Err.Clear
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As stdole.Picture
    Set iPlugin_Icon = LoadResPicture("GRIDREMOVER", vbResIcon)
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Strips a grid from a tileset or spriteset."
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "Image Grid Remover"
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

