Attribute VB_Name = "mdlPlugins"
Option Explicit
Global g_colPlugins As Engine.Fury2Collection
Global g_colFileTypePlugins As Engine.Fury2Collection

Public Function XCreateObject(ByRef Name As String, Optional ByRef Filename As String = "") As Object
On Error Resume Next
Dim l_strLibrary As String, l_strClass As String
    If InStr(Name, ".") Then
        l_strLibrary = left(Name, InStr(Name, ".") - 1)
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
    l_lngCount = ReadRegSetting("Plugins\Count")
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

