VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "GamePacker"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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

Private Function Engine() As Fury2Engine
On Error Resume Next
    Set Engine = m_objEditor.Engine
End Function

Public Sub PackGame(ByRef Options As GamePackerOptions)
On Error Resume Next
Dim l_manManifest As PackManifest
Dim l_resResources As ResourceSet
Dim l_strTemplate As String
Dim l_strOutput As String
Dim l_strGameName As String
Dim l_strFile As String
Dim l_strIcon As String
Dim l_strPatterns() As String, l_lngPatterns As Long
Dim l_strLibPath As String
Dim l_bytVersion() As Byte
Dim l_filFile As VirtualFile
Dim l_verVersion As cVersionInfo
Dim l_filFiles As Fury2Files
#If ResPack = 1 Then
    With Options
        Err.Clear
        m_objEditor.SetBusyState True
        m_objEditor.SetStatus "Preparing to pack..."
        Kill Engine.FileSystem.Root & .OutputFilename
        l_strOutput = m_objEditor.Folder & "pack.exe"
        l_strTemplate = .Stub
        l_strIcon = .Icon
        If Not FileExists(l_strIcon) Then
            If FileExists(Engine.FileSystem.Root & l_strIcon) Then
                l_strIcon = Engine.FileSystem.Root & l_strIcon
            Else
                l_strIcon = ""
            End If
        End If
        If Not FileExists(l_strTemplate) Then
            l_strTemplate = m_objEditor.EnginePath
        End If
        l_strGameName = .GameName
        If Len(Trim(l_strGameName)) = 0 Then
            l_strGameName = m_objEditor.GameName
        End If
        l_strLibPath = .LibraryFolder
        If InStr(l_strLibPath, ":") Then
        Else
            l_strLibPath = m_objEditor.Folder & l_strLibPath
        End If
        l_strPatterns = Split(.InputFiles, ";")
        m_objEditor.SetStatus "Reading template..."
        m_objEditor.SetProgress 0.05
        l_resResources = ReadFileResources(l_strTemplate)
        m_objEditor.SetProgress 0.15
        m_objEditor.SetStatus "Beginning pack..."
        l_manManifest = BeginPackResources(l_strOutput, l_strTemplate)
        m_objEditor.SetProgress 0.175
        m_objEditor.SetStatus "Packing resources..."
        WriteResourceEntries l_manManifest.Handle, l_resResources, RT_GROUP_ICON
        WriteResourceEntries l_manManifest.Handle, l_resResources, RT_ICON
        Set l_verVersion = New cVersionInfo
        l_bytVersion = ReadResourceEntryByType(l_resResources, RT_VERSION)
        Set l_filFile = New VirtualFile
        l_filFile.Data = l_bytVersion
        l_filFile.Load l_verVersion
        If Len(.Company) > 0 Then l_verVersion.Values("CompanyName") = .Company
        If Len(.ProductVersion) > 0 Then l_verVersion.Values("ProductVersion") = .ProductVersion
        If Len(.ProductName) > 0 Then
            l_verVersion.Values("ProductName") = .ProductName
        Else
            l_verVersion.Values("ProductName") = l_strGameName
        End If
        l_verVersion.Values("OriginalFilename") = .OutputFilename
        l_verVersion.Values("FileDescription") = l_strGameName
        l_verVersion.Values("LegalCopyright") = .Copyright
        l_verVersion.Values("Engine Version") = Engine.Version
        l_verVersion.Values("Editor Version") = m_objEditor.Version
        l_verVersion.Values("Packer Version") = App.Major & "." & App.Minor & "." & App.Revision
        l_verVersion.Values("Pack Date") = CStr(Now)
        Set l_filFile = New VirtualFile
        l_filFile.Save l_verVersion
        l_bytVersion = l_filFile.Data
        WriteResourceBytesEx l_manManifest.Handle, RT_VERSION, 1, l_bytVersion, 1033
        m_objEditor.SetStatus "Packing engine files..."
        m_objEditor.SetProgress 0.25
        ManifestWrite l_manManifest, "GameName", l_strGameName
        PackResourceFile l_manManifest, l_strLibPath & "compromise.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "engine.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "graphics.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "video.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "sound2.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "packages3.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "filesystem.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "vbscript.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "script2.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "scriptengine.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "vbscript.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "corona.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "softfx.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "glfx.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "fmodex.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "Video_GDI.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "Video_DirectDraw.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "Video_OpenGL.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "http.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "vbenet.dll", "$sys$\"
        PackResourceFile l_manManifest, l_strLibPath & "uikit.dll", "$sys$\"
        PackResourceFiles l_manManifest, m_objEditor.Folder & "\shaders\*.*", "$sys$\shaders\", False, False
        m_objEditor.SetStatus "Packing game files..."
        m_objEditor.SetProgress 0.5
        For l_lngPatterns = LBound(l_strPatterns) To UBound(l_strPatterns)
            PackResourceFiles l_manManifest, Engine.FileSystem.Root & "*.*", "$game$\", .CompressData, True
            m_objEditor.SetProgress 0.5 + (l_lngPatterns / UBound(l_strPatterns)) * 0.4
        Next l_lngPatterns
        If FileExists(l_strIcon) Then
'            MsgBox "Warning: Icon replacement isn't working yet. Sorry!", vbInformation, "Error"
'            ' 5 = RT_ICON 14 = RT_ICON_GROUP
'            RemoveResourceEx l_manManifest.Handle, 5, 1
'            WriteResourceFileEx l_manManifest.Handle, 5, "ICON", l_strIcon
        End If
        m_objEditor.SetStatus "Saving..."
        FinishPackResources l_manManifest
        Err.Clear
        m_objEditor.SetProgress 0.9
        Kill Engine.FileSystem.Root & .OutputFilename
        Name l_strOutput As Engine.FileSystem.Root & .OutputFilename
        m_objEditor.RefreshFileSidebar
        m_objEditor.SetProgress 0
        m_objEditor.SetStatus l_manManifest.FilesPacked & " file(s) packed into " & l_manManifest.FileCount & " resource blocks."
        m_objEditor.SetBusyState False
    End With
#End If
End Sub

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
End Property

Private Sub iPlugin_Activate()
On Error Resume Next
#If ResPack = 1 Then
Dim l_plgPlugin As iFileTypePlugin
    Load frmGamePacker
    m_objEditor.LoadSettings "GamePacker\Settings", frmGamePacker.Options
    frmGamePacker.Options.SetEngine m_objEditor.Engine
    frmGamePacker.RefreshSettings
    frmGamePacker.Show vbModal
    If Not (frmGamePacker.Options Is Nothing) Then
        m_objEditor.SaveSettings "GamePacker\Settings", frmGamePacker.Options
        PackGame frmGamePacker.Options
    End If
    Unload frmGamePacker
    Err.Clear
#End If
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As Fury2Image
On Error Resume Next
    Set iPlugin_Icon = m_objEditor.LoadResources("ng").ItemData("icons\game packer.png")
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Packs game files into a self-extracting game executable"
End Property

Private Property Get iPlugin_PluginName() As String
    iPlugin_PluginName = "Game Packer"
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
    Set iPlugin_ToolbarIcon = m_objEditor.LoadResources("ng").ItemData("icons\game packer.png")
End Property


