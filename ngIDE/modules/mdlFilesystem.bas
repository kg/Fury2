Attribute VB_Name = "mdlFilesystem"
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
Private Const c_lngFilesystemUpdateDelay As Long = 10

Global g_fsFilesystem As Fury2Filesystem

Private Declare Function GetShortPathName Lib "kernel32" Alias _
    "GetShortPathNameA" (ByVal lpszLongPath As String, _
    ByVal lpszShortPath As String, ByVal cchBuffer As Long) As Long

Function GetShortFileName(ByVal LongFileName As String) As String
On Error Resume Next
    Dim buffer As String, length As Long
    buffer = Space(256)
    length = GetShortPathName(LongFileName, buffer, Len(buffer))
    GetShortFileName = Left(buffer, length)
End Function

Public Sub EnumFilesystem(Output As vbalTreeView, FS As Fury2Filesystem, Optional Path As String = "/", Optional Filter As String = "*.*", Optional Recursive As Boolean = True, Optional HardDisk As Boolean = False)
On Error Resume Next
Dim l_flsFiles As Fury2Files, l_varNames As Variant, l_fdsFolders As Fury2Folders
Dim l_lngFile As Long, l_strIconKey As String, l_lngFolder As Long
Dim l_silList As cVBALSysImageList, l_nodParent As cTreeViewNode, l_nodItem As cTreeViewNode
Dim l_lngTotalCount As Long, l_lngCurrentCount As Long, l_lngUpdateDelay As Long
Dim l_strSelected As String
    If FS Is Nothing Then
        Output.Nodes.Clear
    End If
    SetStatus "Scanning Filesystem"
    frmMain.SetProgress 0
    SetBusyState True
    With Output
        .Visible = False
        If .SelectedItem Is Nothing Then
        Else
            l_strSelected = .SelectedItem.key
        End If
        .Nodes.Clear
        Set l_silList = New cVBALSysImageList
        l_silList.IconSizeX = 16
        l_silList.IconSizeY = 16
        l_silList.Create
        .ImageList = l_silList.hIml
        Set l_fdsFolders = FS.EnumFolders(IIf(Right(Path, 1) = "/", Path, Path + "/") + "*", Recursive)
        l_fdsFolders.Sort True
        Set l_flsFiles = FS.EnumFiles(Path, Filter, Recursive)
        l_lngTotalCount = l_fdsFolders.Count + l_flsFiles.Count
        l_lngCurrentCount = 0
        If l_fdsFolders.Count > 0 Then
            l_varNames = l_fdsFolders.Names
            For l_lngFolder = LBound(l_varNames) To UBound(l_varNames)
                If l_fdsFolders Is Nothing Then Exit For
                Set l_nodParent = Nothing
                If InStr(l_varNames(l_lngFolder), "/") <> InStrRev(l_varNames(l_lngFolder), "/") Then
                    Set l_nodParent = .Nodes(FS.GetPath(l_varNames(l_lngFolder)))
                    Err.Clear
                End If
                If l_nodParent Is Nothing Then
                    Set l_nodItem = .Nodes.Add(, , l_varNames(l_lngFolder) + "/", FS.GetTitle(l_varNames(l_lngFolder)), l_silList.FolderIndex(FS.TranslateFilename(l_varNames(l_lngFolder)) + "\"))
                Else
                    Set l_nodItem = .Nodes.Add(l_nodParent, etvwChild, l_varNames(l_lngFolder) + "/", FS.GetTitle(l_varNames(l_lngFolder)), l_silList.FolderIndex(FS.TranslateFilename(l_varNames(l_lngFolder)) + "\"))
                End If
                If Recursive Then
                Else
                    l_nodItem.Children.Add , , l_nodItem.key & "children", "[loading]"
                End If
                l_lngUpdateDelay = (l_lngUpdateDelay + 1) Mod c_lngFilesystemUpdateDelay
                If l_lngUpdateDelay = 0 Then
                    frmMain.SetProgress l_lngFolder / l_lngTotalCount
                End If
            Next l_lngFolder
        End If
        l_lngCurrentCount = l_fdsFolders.Count
        If l_flsFiles.Count > 0 Then
            l_varNames = l_flsFiles.Names
            For l_lngFile = LBound(l_varNames) To UBound(l_varNames)
                If l_flsFiles Is Nothing Then Exit For
                Set l_nodParent = Nothing
                Set l_nodParent = .Nodes(FS.GetPath(l_varNames(l_lngFile)))
                Err.Clear
                If l_nodParent Is Nothing Then
                    .Nodes.Add , , l_varNames(l_lngFile), FS.GetTitle(l_varNames(l_lngFile)), l_silList.ItemIndex(FS.TranslateFilename(l_varNames(l_lngFile)))
                Else
                    .Nodes.Add l_nodParent, etvwChild, l_varNames(l_lngFile), FS.GetTitle(l_varNames(l_lngFile)), l_silList.ItemIndex(FS.TranslateFilename(l_varNames(l_lngFile)))
                End If
                l_lngUpdateDelay = (l_lngUpdateDelay + 1) Mod c_lngFilesystemUpdateDelay
                If l_lngUpdateDelay = 0 Then
                    frmMain.SetProgress (l_lngFile + l_lngCurrentCount) / l_lngTotalCount
                End If
            Next l_lngFile
        End If
        If HardDisk Then
            .Nodes.Add , , "..", "[Up One Folder]", l_silList.FolderIndex(GetPath(FS.Root))
        End If
        Set l_silList = Nothing
        If l_strSelected <> "" Then
            Set .SelectedItem = .Nodes(l_strSelected)
        End If
        .Visible = True
    End With
    SetBusyState False
    frmMain.SetProgress
    SetStatus
End Sub

Public Sub InitFilesystem(Root As String)
On Error Resume Next
    SetStatus "Initializing Filesystem"
    Set g_fsFilesystem = New Fury2Filesystem
    g_fsFilesystem.Root = Root
    SetStatus
End Sub

Public Sub ShutdownFilesystem()
On Error Resume Next
    Set g_fsFilesystem = Nothing
    g_edEditor.Engine.Filesystem.Cleanup
End Sub

Public Function SelectLocalFile(Optional ByRef Filter As String = "", Optional ByRef Title As String = "Open...") As String
On Error Resume Next
Dim l_varFiles As Variant
    l_varFiles = SelectLocalFiles(Filter, Title, False)
    If UBound(l_varFiles) >= 0 Then
        SelectLocalFile = CStr(l_varFiles(0))
    End If
End Function

Public Function SelectNewFilename(ByRef Document As iDocument, Optional ByRef Title As String = "Save As...") As String
On Error Resume Next
Dim l_fpgPlugin As iFileTypePlugin
Dim l_dlgDialog As New GCommonDialog
Dim l_strFilename As String
Dim l_booOldState As Boolean
    Set l_fpgPlugin = Document.Plugin
    l_strFilename = Document.Filename
    If Trim(l_strFilename) = "" Then
        l_strFilename = "Untitled"
        l_fpgPlugin.FixUpSaveFilename l_strFilename
    End If
    l_booOldState = g_edEditor.AcceleratorManager.Enabled
    g_edEditor.AcceleratorManager.Enabled = False
    If l_dlgDialog.VBGetSaveFileName(l_strFilename, , True, l_fpgPlugin.FilterString & "|All Files|*.*", , , Title) Then
        If FileExists(l_strFilename) Then
        Else
            l_fpgPlugin.FixUpSaveFilename l_strFilename
        End If
        SelectNewFilename = l_strFilename
    End If
    g_edEditor.AcceleratorManager.Enabled = l_booOldState
End Function

Public Function SelectLocalFiles(Optional ByRef Filter As String = "", Optional ByRef Title As String = "Open...", Optional ByVal MultiSelect As Boolean = True) As Variant
On Error Resume Next
Dim l_strFilename As String
Dim l_dlgDialog As New GCommonDialog
Dim l_strFilter As String, l_strPluginFilter As String
Dim l_plgPlugin As iFileTypePlugin
Dim l_varFilters As Variant, l_lngFilters As Long
Dim l_booOldState As Boolean
    l_strFilter = Filter
    If l_strFilter = "" Then
        For Each l_plgPlugin In g_colFileTypePlugins
            If ReadRegSetting("Plugins\Show In Open Dialog\" & TypeName(l_plgPlugin), 1) Then
                l_strPluginFilter = l_plgPlugin.FilterString
                If Len(l_strPluginFilter) > 0 Then
                    l_strFilter = l_strFilter & l_strPluginFilter & "|"
                End If
            End If
        Next l_plgPlugin
        If Trim(l_strFilter) = "" Then
            l_strFilter = "All Files|*.*"
        Else
            l_strFilter = StripEndCharacters(l_strFilter, 1)
            l_varFilters = Split(l_strFilter, "|")
            l_strPluginFilter = ""
            If UBound(l_varFilters) = 1 Then
                l_strPluginFilter = l_varFilters(l_lngFilters) & ";"
            Else
                For l_lngFilters = LBound(l_varFilters) + 1 To UBound(l_varFilters) Step 2
                    If Len(l_varFilters(l_lngFilters)) > 0 Then
                        l_strPluginFilter = l_strPluginFilter & l_varFilters(l_lngFilters) & ";"
                    End If
                Next l_lngFilters
            End If
            l_strPluginFilter = Replace(l_strPluginFilter, "*.*;", "")
            l_strPluginFilter = StripEndCharacters(l_strPluginFilter, 1)
            l_strFilter = "All Supported Formats|" & l_strPluginFilter & "|" & l_strFilter
        End If
    End If
    l_booOldState = g_edEditor.AcceleratorManager.Enabled
    g_edEditor.AcceleratorManager.Enabled = False
    l_dlgDialog.VBGetOpenFileName l_strFilename, , True, MultiSelect, False, True, l_strFilter, , , Title
    g_edEditor.AcceleratorManager.Enabled = l_booOldState
    If InStr(l_strFilename, Chr(0)) Then
        l_strFilename = Left(l_strFilename, InStr(l_strFilename, Chr(0) & Chr(0)) - 1)
        SelectLocalFiles = Split(l_strFilename, Chr(0))
    Else
        SelectLocalFiles = Array(l_strFilename)
    End If
End Function
