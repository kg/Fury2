Attribute VB_Name = "mdlMenus"
Option Explicit
Private l_colMenus As New Collection

Public Function QuickShowMenu(ByRef Ctl As Object, ByVal X As Long, ByVal Y As Long, ByRef Menus As Variant, Optional Icons As vbalImageList = Nothing, Optional ByVal Index = 0, Optional ByVal LeftButtonOnly As Boolean = False)
On Error Resume Next
Dim l_lngIndex As Long, l_mnuNew As cPopupMenu
Dim l_mgrNew As cMenuManager
    Set l_mgrNew = New cMenuManager
    Set l_mnuNew = New cPopupMenu
    With l_mgrNew
        Set .Menu = l_mnuNew
        Set .ImageList = Icons
    End With
    With l_mnuNew
        .ImageList = Icons.hIml
        .hWndOwner = Ctl.hwnd
        .HeaderStyle = ecnmHeaderSeparator
        .OfficeXpStyle = True
    End With
    ParseMenu l_mgrNew, Menus
    l_mnuNew.AddItem "-"
    Set l_mgrNew = Nothing
    If VarType(Index) = vbString Then
        l_lngIndex = l_mnuNew.IndexForKey(Index)
    Else
        l_lngIndex = CLng(Index)
    End If
    QuickShowMenu = l_mnuNew.ShowPopupMenuAtIndex(X, Y, lIndex:=l_lngIndex, bLeftButtonOnly:=LeftButtonOnly)
    Set l_mnuNew = Nothing
End Function

Public Function ShowMenu(ByRef Form As Form, ByVal X As Long, ByVal Y As Long, ByVal Menu As String, Optional ByVal Index = -1, Optional ByVal Absolute As Boolean = False) As Long
On Error Resume Next
Dim l_lngIndex As Long, l_mnuMenu As cPopupMenu
    Set l_mnuMenu = GetMenu(Menu)
    If VarType(Index) = vbString Then
        l_lngIndex = l_mnuMenu.IndexForKey(Index)
    Else
        l_lngIndex = CLng(Index)
    End If
    l_lngIndex = l_lngIndex + 1
    l_mnuMenu.hWndOwner = Form.hwnd
    If Absolute Then
        ShowMenu = l_mnuMenu.ShowPopupAbsolute(X, Y, l_lngIndex)
    Else
        ShowMenu = l_mnuMenu.ShowPopupMenuAtIndex(X, Y, lIndex:=l_lngIndex, bLeftButtonOnly:=True)
    End If
    l_mnuMenu.hWndOwner = frmIcons.hwnd
End Function

Public Sub CleanupMenus()
On Error Resume Next
    Set l_colMenus = Nothing
End Sub

Public Sub DefineMenus()
On Error Resume Next
    DefineMenu "Main Menu", _
        Menus(MenuString("&File", , "FileMenu"), Menus(MenuString("&New", "", "NewMenu", "NEW"), _
        Menus(MenuString("Game", , "Game:New", "NEW GAME"), "-", MenuString("-", , "NewEndSeparator")), _
        , MenuString("&Open...", "Ctrl+O", "File:Open", "OPEN"), MenuString("Open &Recent", , "RecentFiles"), Menus(MenuString("-", , "RecentFilesEndSeparator")), MenuString("&Save", "Ctrl+S", "File:Save", "SAVE", , , False), MenuString("Save As...", "Ctrl+Shift+S", "File:SaveAs", "SAVE AS", , , False), "-", _
        MenuString("E&xit", "Alt+F4", "Editor:Exit", "EXIT"), "-"), _
        MenuString("&Edit", , "EditMenu"), Menus(MenuString("&Undo", "Ctrl+Z", "Action:Undo", "UNDO", , , False), MenuString("&Redo", "Shift+Ctrl+Z", "Action:Redo", "REDO", , , False), "-", _
        MenuString("Cu&t", "Ctrl+X", "Action:Cut", "CUT", , , False), MenuString("&Copy", "Ctrl+C", "Action:Copy", "COPY", , , False), MenuString("&Paste", "Ctrl+V", "Action:Paste", "PASTE"), _
        MenuString("&Delete", "Del", "Action:Delete", "DELETE", , , False), "-", MenuString("Select &All", "Ctrl+A", "Action:SelectAll", "SELECT ALL", , , False), MenuString("Select &None", "Ctrl+D", "Action:SelectNone", "SELECT NONE", , , False), "-"), _
        MenuString("&View", , "ViewMenu"), Menus("-Sidebars", MenuString("&File Sidebar", "F8", "Show:FileSidebar", , , False, True), "-Toolbars", MenuString("&Main", "", "Show:MainToolbar", , , True, True), MenuString("&Game", "", "Show:GameToolbar", , , True, True), MenuString("&Plugins", "", "Show:PluginToolbar", , , True, True), "-"), _
        MenuString("&Document", , "DocumentMenu"), Menus(MenuString("-", , "DocumentEndSeparator"), MenuString("&Close", "Ctrl+F4", "Action:CloseWindow", "CLOSE WINDOW"), MenuString("&Next", "Ctrl+F6", "Action:NextWindow", "NEXT WINDOW"), "-"), _
        MenuString("&Game", , "GameMenu"), Menus(MenuString("&Open...", , "Game:Open", "OPEN GAME"), MenuString("Open &Recent", , "RecentGames"), Menus(MenuString("-", , "RecentGamesEndSeparator")), "-", MenuString("&Play", "F9", "Game:Play", "PLAY"), "-"), _
        MenuString("&Tools", , "ToolMenu"), Menus(, MenuString("-", , "PluginsEndSeparator"), MenuString("Manage Plugins...", , "Plugins:Manage", "PLUGIN"), MenuString("Options...", , "Show:Options", "PROPERTIES", , , False), MenuString("-", , "PluginsEndSeparator2")), _
        MenuString("&Window", , "WindowMenu"), Menus(MenuString("-", , "WindowsEndSeparator"), MenuString("Close All", , "Action:CloseAllWindows", "CLOSE ALL WINDOWS", , , False), MenuString("-", , "WindowsEndSeparator2")), _
        MenuString("&Help", , "HelpMenu"), Menus(MenuString("Online &Documentation", , "Help:OnlineDocs", "HELP"), MenuString("Online &Tutorials", , "Help:OnlineTutorials", "HELP"), "-", MenuString("&About...", , "Help:About", "HELP"), "-")), _
        frmIcons.ilIcons
End Sub

Public Sub SetMenuHandler(Name As String, Handler As Object)
On Error Resume Next
    Set l_colMenus(LCase(Trim(Name))).EventHandler = Handler
End Sub

Public Function GetMenu(Name As String) As cPopupMenu
On Error Resume Next
    Set GetMenu = l_colMenus(LCase(Trim(Name))).Menu
End Function

Public Function GetMenuManager(Name As String) As cMenuManager
On Error Resume Next
    Set GetMenuManager = l_colMenus(LCase(Trim(Name)))
End Function

Public Sub SetMenuOwner(Name As String, hwnd As Long)
On Error Resume Next
    l_colMenus(LCase(Trim(Name))).Menu.hWndOwner = hwnd
End Sub

Public Function DefineMenu(Name As String, Items As Variant, Optional Icons As vbalImageList = Nothing) As cPopupMenu
On Error Resume Next
Dim l_mnuNew As cPopupMenu
Dim l_mgrNew As cMenuManager
    Set l_mnuNew = New cPopupMenu
    Set l_mgrNew = New cMenuManager
    Set l_mgrNew.Menu = l_mnuNew
    Set l_mgrNew.ImageList = Icons
    l_mnuNew.ImageList = Icons.hIml
    l_colMenus.Add l_mgrNew, LCase(Trim(Name))
    Set DefineMenu = l_mnuNew
    With l_mnuNew
        .hWndOwner = frmIcons.hwnd
        .HeaderStyle = ecnmHeaderSeparator
        .OfficeXpStyle = True
    End With
    ParseMenu l_mgrNew, Items
End Function

Public Function Menus(ParamArray Values() As Variant) As Variant
On Error Resume Next
Dim l_lngItems As Long
Dim l_varValue As Variant
    ReDim l_varValue(LBound(Values) To UBound(Values))
    For l_lngItems = LBound(Values) To UBound(Values)
        l_varValue(l_lngItems) = Values(l_lngItems)
    Next l_lngItems
    Menus = l_varValue
End Function

Public Function MenusFromStringArray(ByRef Values() As String) As Variant
On Error Resume Next
Dim l_lngItems As Long
Dim l_varValue As Variant
    ReDim l_varValue(LBound(Values) To UBound(Values))
    For l_lngItems = LBound(Values) To UBound(Values)
        l_varValue(l_lngItems) = Values(l_lngItems)
    Next l_lngItems
    MenusFromStringArray = l_varValue
End Function

Public Function MenuString(ByRef Name As String, Optional ByRef Accelerator As String = "", Optional ByRef key As String = "", Optional ByVal Icon = "", Optional ByRef HelpText As String = "", Optional ByVal Checked As Boolean = False, Optional ByVal Enabled As Boolean = True, Optional ByVal ItemData As Long = -1) As String
On Error Resume Next
    MenuString = Name & IIf(Accelerator <> "", Chr(9) & Accelerator, "") & "·" & HelpText & "·" & ItemData & "·" & CStr(Icon) & "·" & Checked & "·" & Enabled & "·" & key
End Function

Public Function ParseMenu(Menu As cMenuManager, Items As Variant, Optional ByVal Parent As Long = -1, Optional ByRef InsertionPoint As Long = -1)
On Error Resume Next
Dim l_lngItems As Long, l_varItem As Variant, l_varParameters As Variant
    With Menu.Menu
        For l_lngItems = LBound(Items) To UBound(Items)
            l_varItem = Items(l_lngItems)
            Select Case VarType(l_varItem)
            Case vbString
                If InStr(l_varItem, "·") Then
                    l_varParameters = Split(l_varItem, "·")
                    If VarType(l_varParameters(3)) = vbString Then
                        Err.Clear
                        l_varParameters(3) = CLng(Menu.ImageList.ItemIndex(CStr(l_varParameters(3)))) - 1
                        If Err <> 0 Then
                            l_varParameters(3) = -1
                        End If
                    End If
                    If InsertionPoint = -1 Then
                        If Parent = -1 Then
                            .AddItem l_varParameters(0), l_varParameters(1), l_varParameters(2), , l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6)
                        Else
                            .AddItem l_varParameters(0), l_varParameters(1), l_varParameters(2), Parent, l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6)
                        End If
                    Else
                        .InsertItem l_varParameters(0), InsertionPoint, l_varParameters(1), l_varParameters(2), l_varParameters(3), l_varParameters(4), l_varParameters(5), l_varParameters(6)
                    End If
                Else
                    If InsertionPoint = -1 Then
                        If Parent = -1 Then
                            .AddItem l_varItem
                        Else
                            .AddItem l_varItem, , , Parent
                        End If
                    Else
                        .InsertItem l_varItem, InsertionPoint
                    End If
                End If
            Case vbVariant Or vbArray
                ParseMenu Menu, l_varItem, .Count
            Case Else
            End Select
        Next l_lngItems
    End With
End Function
