VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "RM2kXSpriteImporter"
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

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Sub iPlugin_Activate()
On Error Resume Next
Dim l_strFilename As String
Dim l_plgPlugin As iFileTypePlugin
Dim l_docSprite As frmSprites
Dim l_sprSprite As Fury2Sprite
    l_strFilename = SelectFiles("Images|" + libGraphics.SupportedGraphicsFormats, "Select Character Set", False)
    If Trim(l_strFilename) = "" Then Exit Sub
    l_strFilename = Replace(l_strFilename, DefaultEngine.FileSystem.Root, "/")
    l_strFilename = Replace(l_strFilename, "\", "/")
    Set l_plgPlugin = m_objEditor.FindFileTypePlugin("Sprite Collection")
    Set l_docSprite = l_plgPlugin.CreateNew
    Set l_sprSprite = DefaultEngine.RM2kSprite(l_strFilename, CLng(InputBox("Character number:", "Select Character", 0)))
    l_sprSprite.Initialize
    l_sprSprite.Load
    l_docSprite.Sprites.Add l_sprSprite
    l_docSprite.ReloadGraphics
    m_objEditor.NewDocument l_docSprite
    Err.Clear
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As Fury2Image
On Error Resume Next
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Creates sprites from RPG Maker 200x character sets."
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "RPG Maker 200x Sprite Importer"
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
    Set iPlugin_ToolbarIcon = m_objEditor.LoadResources("ng").ItemData("icons\rm2kx sprite importer.png")
End Property

