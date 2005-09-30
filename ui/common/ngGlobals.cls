VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngGlobals"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Function ConvertSystemColor(ByVal Index As SystemColors) As Long
On Error Resume Next
    ConvertSystemColor = SetAlpha(SwapChannels(GetSystemColor(Index), Red, Blue), 255)
End Function

Public Function DisplayMenuStats() As String
    DisplayMenuStats = "Menus: " & g_lngMenus & vbCrLf & _
            "Menu Hosts: " & g_lngMenuHosts & vbCrLf & _
            "Menu Items: " & g_lngMenuItems & vbCrLf & _
            "Font Hosts: " & g_lngFontForms & vbCrLf & _
            "Forms: " & Forms.Count
End Function

Public Sub CloseAllMenus()
On Error Resume Next
Dim l_mnuMenu As ngMenu
    For Each l_mnuMenu In g_colOpenMenus
        l_mnuMenu.DoClose
    Next l_mnuMenu
    Set g_colOpenMenus = New Collection
End Sub

Public Property Get MenuIsOpen() As Boolean
    MenuIsOpen = g_colOpenMenus.Count > 0
End Property

Public Function CreateMenu() As ngMenu
On Error Resume Next
    Set CreateMenu = New ngMenu
    Set CreateMenu.ResourceFile = g_rfMenuThemeFile
    CreateMenu.ResourcePattern = g_strMenuThemePattern
End Function

Public Sub SetMenuTheme(ByRef ResourceFile As ngResourceFile, Optional Pattern As String = "*.png")
On Error Resume Next
    Set g_rfMenuThemeFile = ResourceFile
    g_strMenuThemePattern = Pattern
End Sub

Public Sub SetTheme(ByRef ResourceFile As ngResourceFile, Optional Pattern As String = "*.png")
On Error Resume Next
    Set g_rfThemeFile = ResourceFile
    g_strThemePattern = Pattern
End Sub

Public Sub SetTabTheme(Theme As String)
On Error Resume Next
    g_strTabTheme = Theme
End Sub

Public Sub SetToolbarTheme(Theme As String)
On Error Resume Next
    g_strToolbarTheme = Theme
End Sub

Public Property Get ThemesEnabled() As Boolean
On Error Resume Next
    ThemesEnabled = g_booEnableThemes
End Property

Public Property Let ThemesEnabled(ByVal NewValue As Boolean)
On Error Resume Next
    g_booEnableThemes = NewValue
End Property

Public Function BindEvent(ByRef Obj As Object, ByRef Method As String, Optional ByRef Parameters = Nothing) As ngEvent
On Error Resume Next
Dim l_evtEvent As ngEvent
    Set l_evtEvent = New ngEvent
    l_evtEvent.Bind Obj, Method, Parameters
    Set BindEvent = l_evtEvent
End Function

Public Function LoadResourceFile(ByRef Filename As String) As ngResourceFile
On Error Resume Next
Dim l_szaArchive As SAWZipNG.Archive
Dim l_rfFile As ngResourceFile
    Set l_rfFile = g_colResourceCache(Filename)
    If l_rfFile Is Nothing Then
        Set l_szaArchive = New SAWZipNG.Archive
        Err.Clear
        Call l_szaArchive.Open(Filename, OM_READONLY)
        If Err <> 0 Then
            Err.Clear
            Exit Function
        End If
        If l_szaArchive.Closed Then
            Exit Function
        End If
        Set l_rfFile = New ngResourceFile
        Set l_rfFile.Archive = l_szaArchive
        l_rfFile.Filename = Filename
        l_rfFile.Refresh
        g_colResourceCache.Add l_rfFile
    End If
    Set LoadResourceFile = l_rfFile
End Function

Private Sub Class_Initialize()
On Error Resume Next
    F2Init
    g_booEnableThemes = True
    If g_colResourceCache Is Nothing Then
        Set g_colResourceCache = New Collection
    End If
    Set g_fntMarlett = New StdFont
    g_fntMarlett.Name = "Marlett"
    g_fntMarlett.Size = 9
End Sub

Private Sub Class_Terminate()
On Error Resume Next
Dim l_rfFile As ngResourceFile
    For Each l_rfFile In g_colResourceCache
        l_rfFile.Free
    Next l_rfFile
    Set g_colResourceCache = Nothing
End Sub
