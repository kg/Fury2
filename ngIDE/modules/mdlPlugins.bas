Attribute VB_Name = "mdlPlugins"
'
'    ngIDE (Fury² Game Creation System Next-Generation Editor)
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
Global g_colPlugins As Engine.Fury2Collection
Global g_colFileTypePlugins As Engine.Fury2Collection

Public Function IsPluginInstalled(ByRef ClassName As String) As Boolean
On Error Resume Next
On Error Resume Next
Dim l_lngCount As Long, l_lngPlugins As Long
Dim l_strName As String, l_lngEnabled As Long, l_strFilename As String
    l_lngCount = ReadRegSetting("Plugins\Count", 0)
    If l_lngCount > 0 Then
        For l_lngPlugins = 1 To l_lngCount
            l_strName = ReadRegSetting("Plugins\" & l_lngPlugins & "::Name")
            Err.Clear
            If Trim(LCase(l_strName)) = Trim(LCase(ClassName)) Then
                IsPluginInstalled = True
                Exit Function
            End If
        Next l_lngPlugins
    End If
End Function

Public Sub InstallPlugin(ByRef ClassName As String, ByRef Filename As String)
On Error Resume Next
Dim l_lngCount As Long
    If IsPluginInstalled(ClassName) Then Exit Sub
    l_lngCount = ReadRegSetting("Plugins\Count", 0)
    l_lngCount = l_lngCount + 1
    WriteRegSetting "Plugins\Count", l_lngCount
    WriteRegSetting "Plugins\" & l_lngCount & "::Name", ClassName
    WriteRegSetting "Plugins\" & l_lngCount & "::Filename", Filename
End Sub

Public Function XCreateObject(ByRef Name As String, Optional ByRef Filename As String = "") As Object
On Error Resume Next
Dim l_strLibrary As String, l_strClass As String
    If InStr(Name, ".") Then
        l_strLibrary = Left(Name, InStr(Name, ".") - 1)
        l_strClass = Mid(Name, InStr(Name, ".") + 1)
    Else
        l_strLibrary = "internal"
        l_strClass = Name
    End If
    If LCase(Trim(l_strLibrary)) = "internal" Then
        Select Case LCase(Trim(l_strClass))
        Case "cunknownfiletype"
            Set XCreateObject = New cUnknownFileType
        Case Else
        End Select
    Else
        Err.Clear
        Set XCreateObject = CreateObject(Name)
        If Err <> 0 Or XCreateObject Is Nothing Then
            If FileExists(Filename) Then
                RegisterServer Filename, False
                RegisterServer Filename, True
            End If
            Err.Clear
            Set XCreateObject = CreateObject(Name)
        End If
    End If
End Function

Public Sub InitPlugins()
On Error Resume Next
    SetStatus "Initializing Plugin System"
    Set g_colPlugins = New Engine.Fury2Collection
    Set g_colFileTypePlugins = New Engine.Fury2Collection
    SetStatus
End Sub

Public Sub ShutdownPlugins()
On Error Resume Next
Dim l_plgPlugin As iPlugin
    SetStatus "Unloading Plugins"
    For Each l_plgPlugin In g_colPlugins
        l_plgPlugin.Shutdown
        Set l_plgPlugin.Editor = Nothing
    Next l_plgPlugin
    Set g_colPlugins = Nothing
    Set g_colFileTypePlugins = Nothing
    SetStatus
End Sub

Public Function LoadPlugin(Plugin As Object) As Boolean
On Error Resume Next
Dim l_plgPlugin As iPlugin, l_fpgPlugin As iFileTypePlugin
    If Plugin Is Nothing Then Exit Function
    If TypeOf Plugin Is iPlugin Then
        ' Valid plugin
        Set l_plgPlugin = Plugin
        Set l_plgPlugin.Editor = g_edEditor
        l_plgPlugin.Initialize
        If TypeOf Plugin Is iFileTypePlugin Then
            ' File Type Plugin
            g_colPlugins.Add Plugin
            ' Add file plugins in reverse for proper override order
            g_colFileTypePlugins.Add Plugin, , 1
        Else
            ' Normal Plugin
            g_colPlugins.Add Plugin
        End If
    Else
    End If
End Function

Public Sub LoadPlugins()
On Error Resume Next
Dim l_lngCount As Long, l_lngPlugins As Long
Dim l_strName As String, l_lngEnabled As Long, l_strFilename As String
Dim l_plgPlugin As iPlugin
    SetStatus "Loading Plugins"
    frmMain.SetProgress 0
    l_lngCount = ReadRegSetting("Plugins\Count", 0)
    If l_lngCount = 0 Then
        SetStatus "Installing Standard Plugins"
        InstallPlugin "internal.cUnknownFileType", App.Path
        InstallPlugin "ngPlugins.RM2kXSpriteImporter", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.TextFile", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.HTMLFile", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.ImageFile", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.TilesetAssembler", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.ImageGridRemover", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.SpriteImporter", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.AudioFile", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.MapEditor", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.ScriptFile", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.SpriteEditor", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.FontEditor", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.CommandBrowser", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.ShowPictureDesigner", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "ngPlugins.UserDataEditor", GetPath(App.Path) & "\ng.dll"
        InstallPlugin "tk.TKTilesetImporter", GetPath(App.Path) & "\tk.dll"
'        InstallPlugin "tk.TKBoardImporter", GetPath(App.Path) & "\tk.dll"
        l_lngCount = ReadRegSetting("Plugins\Count", 0)
    End If
    If l_lngCount > 0 Then
        For l_lngPlugins = 1 To l_lngCount
            l_strName = ReadRegSetting("Plugins\" & l_lngPlugins & "::Name")
            l_strFilename = ""
            l_strFilename = ReadRegSetting("Plugins\" & l_lngPlugins & "::Filename")
            Err.Clear
            Set l_plgPlugin = Nothing
            Set l_plgPlugin = XCreateObject(l_strName, l_strFilename)
            If l_plgPlugin Is Nothing Then
            Else
                LoadPlugin l_plgPlugin
            End If
            frmMain.SetProgress ((l_lngPlugins - 1) / (l_lngCount - 1))
        Next l_lngPlugins
    End If
    frmMain.RefreshPluginToolbar
    frmMain.SetProgress
    SetStatus
End Sub

