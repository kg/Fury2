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

Public Function UninstallPlugin(ByRef ClassName As String) As Long
On Error Resume Next
Dim l_lngCount As Long, l_lngPlugins As Long
Dim l_strName As String, l_lngEnabled As Long, l_strFilename As String
Dim l_lngRemoved As Long
    l_lngCount = ReadRegSetting("Plugins\Count", 0)
    If l_lngCount > 0 Then
        For l_lngPlugins = 1 To l_lngCount
            l_strName = ReadRegSetting("Plugins\" & l_lngPlugins & "::Name")
            Err.Clear
            If Trim(LCase(l_strName)) Like Trim(LCase(ClassName)) Then
                Call WriteRegSetting("Plugins\" & l_lngPlugins & "::Name", "")
                Call WriteRegSetting("Plugins\" & l_lngPlugins & "::Filename", "")
                l_lngRemoved = l_lngRemoved + 1
            End If
        Next l_lngPlugins
    End If
    UninstallPlugin = l_lngRemoved
End Function

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

Public Sub InstallPluginSet(ByVal Filename As String)
On Error Resume Next
Dim l_strText As String
Dim l_strLines() As String
Dim l_strName As String
Dim l_strLibrary As String
Dim l_strPath As String
Dim l_strDLL As String
Dim l_lngPlugins As Long
Dim l_strLine As String
Dim l_strParts() As String
    l_strText = ReadTextFile(Filename)
    l_strPath = GetPath(Filename)
    l_strLines = Split(l_strText, vbCrLf)
    l_strName = l_strLines(0)
    l_strLibrary = l_strLines(1)
    l_strDLL = l_strLines(2)
    For l_lngPlugins = 3 To UBound(l_strLines)
        l_strLine = l_strLines(l_lngPlugins)
        l_strParts = Split(l_strLine, ":")
        InstallPlugin l_strLibrary & "." & l_strParts(1), l_strPath & "\" & l_strDLL
    Next l_lngPlugins
End Sub

Public Sub InstallPlugin(ByRef ClassName As String, ByRef Filename As String)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngResult As Long
Dim l_lngIndex As Long
Dim l_lngPlugins As Long
Dim l_strName As String
    If IsPluginInstalled(ClassName) Then Exit Sub
    l_lngCount = ReadRegSetting("Plugins\Count", 0)
    l_lngIndex = -1
    If l_lngCount > 0 Then
        For l_lngPlugins = 1 To l_lngCount
            l_strName = ReadRegSetting("Plugins\" & l_lngPlugins & "::Name")
            If Trim(LCase(l_strName)) = "" Then
                l_lngIndex = l_lngPlugins
                Exit For
            End If
        Next l_lngPlugins
    End If
    If (l_lngIndex = -1) Then
        l_lngCount = l_lngCount + 1
        l_lngIndex = l_lngCount
        WriteRegSetting "Plugins\Count", l_lngCount
    End If
    WriteRegSetting "Plugins\" & l_lngIndex & "::Name", ClassName
    If Not InIDE Then
        l_lngResult = Compromise.Register(Filename)
        If (l_lngResult = 1) Then
            WriteRegSetting "Plugins\" & l_lngIndex & "::Filename", Filename
        Else
            l_lngResult = Compromise.Register(App.Path & "\" & Filename)
            If (l_lngResult = 1) Then
                WriteRegSetting "Plugins\" & l_lngIndex & "::Filename", App.Path & "\" & Filename
            Else
                l_lngResult = Compromise.Register(App.Path & "\plugin\" & Filename)
                If (l_lngResult = 1) Then
                    WriteRegSetting "Plugins\" & l_lngIndex & "::Filename", App.Path & "\plugin\" & Filename
                Else
                    WriteRegSetting "Plugins\" & l_lngIndex & "::Filename", ""
                End If
            End If
        End If
    End If
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
        If (Not InIDE) And (Compromise.IsSupported() = 1) Then
            Compromise.Register Filename
        End If
        Set XCreateObject = CreateObject(Name)
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
        InstallPlugin "internal.cUnknownFileType", ""
        InstallPlugin "ngPlugins.RM2kXSpriteImporter", "\ng.dll"
        InstallPlugin "ngPlugins.TextFile", "\ng.dll"
        InstallPlugin "ngPlugins.ImageFile", "\ng.dll"
        InstallPlugin "ngPlugins.ImageGridRemover", "\ng.dll"
        InstallPlugin "ngPlugins.SpriteImporter", "\ng.dll"
        InstallPlugin "ngPlugins.ImageMapImporter", "\ng.dll"
        InstallPlugin "ngPlugins.AudioFile", "\ng.dll"
        InstallPlugin "ngPlugins.MapEditor", "\ng.dll"
        InstallPlugin "ngPlugins.ScriptFile", "\ng.dll"
        InstallPlugin "ngPlugins.SpriteEditor", "\ng.dll"
        InstallPlugin "ngPlugins.FontEditor", "\ng.dll"
        InstallPlugin "ngPlugins.TilesetEditor", "\ng.dll"
        InstallPlugin "ngPlugins.CommandBrowser", "\ng.dll"
        InstallPlugin "ngPlugins.UserDataEditor", "\ng.dll"
        InstallPlugin "ngPlugins.GamePacker", "\ng.dll"
        InstallPlugin "tk.TKTilesetImporter", "\tk.dll"
        InstallPlugin "tk.TKBoardImporter", "\tk.dll"
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

