VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iEditorEvents"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Enum Editor_DockPoints
    Dock_Top = 0
    Dock_Right = 1
    Dock_Bottom = 2
    Dock_Left = 3
End Enum

Public Sub BeforeLoadDocument(ByRef Filename As String, ByRef Cancel As Boolean)
End Sub
Public Sub LoadedDocument(ByRef Document As iDocument)
End Sub
Public Sub BeforeCreateDocument(ByRef DocumentType As String, ByRef Cancel As Boolean)
End Sub
Public Sub CreatedDocument(ByRef Document As iDocument)
End Sub
Public Sub BeforeSaveDocument(ByRef Document As iDocument, ByRef NewFilename As String, ByRef Cancel As Boolean)
End Sub
Public Sub SavedDocument(ByRef Document As iDocument)
End Sub

Public Sub PluginLoaded(ByRef Plugin As iPlugin)
End Sub
Public Sub BeforeActivatePlugin(ByRef Plugin As iPlugin, ByRef Cancel As Boolean)
End Sub
Public Sub PluginActivated(ByRef Plugin As iPlugin)
End Sub
Public Sub PluginUnloaded(ByRef Plugin As iPlugin)
End Sub

Public Sub ToolbarHidden(ByRef Toolbar As String)
End Sub
Public Sub ToolbarShown(ByRef Toolbar As String)
End Sub
Public Sub ToolbarDocked(ByRef Toolbar As String, ByRef DockPoint As Editor_DockPoints)
End Sub
Public Sub ToolbarUndocked(ByRef Toolbar As String, ByRef DockPoint As Editor_DockPoints)
End Sub

Public Sub StatusChange(ByRef Status As String)
End Sub
