VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TilesetAssembler"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements iPlugin
Private m_objEditor As Object

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Sub iPlugin_Activate()
On Error Resume Next
Dim l_frmAssembler As New frmTilesetAssembler
    Load l_frmAssembler
    l_frmAssembler.Show vbModal
    Unload l_frmAssembler
    Err.Clear
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As stdole.Picture
    Load frmTilesetAssembler
    Set iPlugin_Icon = frmTilesetAssembler.Icon
    Unload frmTilesetAssembler
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Generates a tileset from individual tile images."
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "Tileset Assembler"
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

