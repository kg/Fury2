Attribute VB_Name = "mdlMenus"
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

'Public Function QuickShowMenu(ByRef Ctl As Object, ByVal X As Long, ByVal Y As Long, ByRef Menus As Variant, Optional Icons As vbalImageList = Nothing, Optional ByVal Index = 0, Optional ByVal LeftButtonOnly As Boolean = False)
'On Error Resume Next
'Dim l_lngIndex As Long, l_mnuNew As cPopupMenu
'Dim l_mgrNew As cMenuManager
'    Set l_mgrNew = New cMenuManager
'    Set l_mnuNew = New cPopupMenu
'    With l_mgrNew
'        Set .Menu = l_mnuNew
'        Set .ImageList = Icons
'    End With
'    With l_mnuNew
'        .ImageList = Icons.hIml
'        .hWndOwner = Ctl.hwnd
'        .HeaderStyle = ecnmHeaderSeparator
'        .OfficeXpStyle = True
'    End With
'    ParseMenu l_mgrNew, Menus
'    l_mnuNew.AddItem "-"
'    Set l_mgrNew = Nothing
'    If VarType(Index) = vbString Then
'        l_lngIndex = l_mnuNew.IndexForKey(Index)
'    Else
'        l_lngIndex = CLng(Index)
'    End If
'    QuickShowMenu = l_mnuNew.ShowPopupMenuAtIndex(X, Y, lIndex:=l_lngIndex, bLeftButtonOnly:=LeftButtonOnly)
'    Set l_mnuNew = Nothing
'End Function
'
'Public Function ShowMenu(ByRef Form As Form, ByVal X As Long, ByVal Y As Long, ByVal Menu As String, Optional ByVal Index = -1, Optional ByVal Absolute As Boolean = False) As Long
'On Error Resume Next
'Dim l_lngIndex As Long, l_mnuMenu As cPopupMenu
'    Set l_mnuMenu = GetMenu(Menu)
'    If VarType(Index) = vbString Then
'        l_lngIndex = l_mnuMenu.IndexForKey(Index)
'    Else
'        l_lngIndex = CLng(Index)
'    End If
'    l_lngIndex = l_lngIndex + 1
'    l_mnuMenu.hWndOwner = Form.hwnd
'    If Absolute Then
'        ShowMenu = l_mnuMenu.ShowPopupAbsolute(X, Y, l_lngIndex)
'    Else
'        ShowMenu = l_mnuMenu.ShowPopupMenuAtIndex(X, Y, lIndex:=l_lngIndex, bLeftButtonOnly:=True)
'    End If
'    l_mnuMenu.hWndOwner = frmIcons.hwnd
'End Function

Public Function QuickShowMenu2(ByRef Ctl As Object, ByVal X As Long, ByVal Y As Long, ByRef Menus As Variant, Optional Icons As vbalImageList = Nothing, Optional ByVal Index = 0, Optional ByVal LeftButtonOnly As Boolean = False)
On Error Resume Next
Dim l_lngIndex As Long, l_mnuNew As ngMenu
    Set l_mnuNew = CreateMenu()
    ParseMenu2 l_mnuNew, Menus
    If VarType(Index) = vbString Then
        l_lngIndex = l_mnuNew.Items.FindKey(Index)
    Else
        l_lngIndex = CLng(Index)
    End If
    If l_lngIndex > 0 Then
        Set l_mnuNew = l_mnuNew.Items(l_lngIndex).ChildMenu
    End If
    QuickShowMenu2 = 0
    QuickShowMenu2 = l_mnuNew.Show(X, Y, Ctl.hwnd, True, False).Index
    Set l_mnuNew = Nothing
End Function

Public Function ShowMenu2(ByRef Form As Form, ByVal X As Long, ByVal Y As Long, ByVal Menu As String, Optional ByVal Index = -1, Optional ByVal Absolute As Boolean = False) As Long
On Error Resume Next
Dim l_lngIndex As Long, l_mnuMenu As ngMenu
    Set l_mnuMenu = GetMenu2(Menu)
    If VarType(Index) = vbString Then
        l_lngIndex = l_mnuMenu.Items.FindKey(Index)
    Else
        l_lngIndex = CLng(Index)
    End If
    If l_lngIndex > 0 Then
        Set l_mnuMenu = l_mnuMenu.Items(l_lngIndex).ChildMenu
    End If
    ShowMenu2 = 0
    ShowMenu2 = l_mnuMenu.Show(X, Y, Form.hwnd, True, Absolute).Index
End Function

Public Sub CleanupMenus()
On Error Resume Next
'    Set l_colMenus = Nothing
'    Set l_colMenus2 = Nothing
End Sub

Public Sub DefineMenus()
On Error Resume Next
    DefineMenu2 "Main Menu", _
        Menus(MenuString("&File", , "FileMenu"), Menus(MenuString("&New", "", "NewMenu", "NEW"), _
                Menus(MenuString("Game", , "Game:New", "NEW GAME"), "-"), _
            , MenuString("&Open...", "Ctrl+O", "File:Open", "OPEN"), MenuString("Open &Recent", , "RecentFiles"), Array(), MenuString("&Save", "Ctrl+S", "File:Save", "SAVE", , , False), MenuString("Save As...", "", "File:SaveAs", "SAVE AS", , , False), MenuString("Save All", "Ctrl+Shift+S", "File:SaveAll", , , , False), "-", _
            MenuString("E&xit", "Alt+F4", "Editor:Exit", "EXIT")), _
        MenuString("&Edit", , "EditMenu"), Menus(MenuString("&Undo", "Ctrl+Z", "Action:Undo", "UNDO", , , False), MenuString("&Redo", "Shift+Ctrl+Z", "Action:Redo", "REDO", , , False), "-", _
            MenuString("Cu&t", "Ctrl+X", "Action:Cut", "CUT", , , False), MenuString("&Copy", "Ctrl+C", "Action:Copy", "COPY", , , False), MenuString("&Paste", "Ctrl+V", "Action:Paste", "PASTE"), _
            MenuString("&Delete", "Del", "Action:Delete", "DELETE", , , False), "-", MenuString("Select &All", "Ctrl+A", "Action:SelectAll", "SELECT ALL", , , False), MenuString("Select &None", "Ctrl+D", "Action:SelectNone", "SELECT NONE", , , False)), _
        MenuString("&View", , "ViewMenu"), Menus("-", MenuString("&Filesystem", "F8", "Show:FileSidebar", , , False, True), MenuString("&Log", , "Show:Log", , , False, True), "-", MenuString("&Main", "", "Show:MainToolbar", , , True, True), MenuString("&Game", "", "Show:GameToolbar", , , True, True), MenuString("&Plugins", "", "Show:PluginToolbar", , , True, True)), _
        MenuString("&Document", , "DocumentMenu"), Menus(MenuString("-", , "DocumentEndSeparator"), MenuString("&Close", "Ctrl+F4", "Action:CloseWindow", "CLOSE WINDOW"), MenuString("&Previous", "Shift+Ctrl+F6", "Action:PreviousWindow", "PREVIOUS WINDOW"), MenuString("&Next", "Ctrl+F6", "Action:NextWindow", "NEXT WINDOW")), _
        MenuString("&Game", , "GameMenu"), Menus(MenuString("&Open...", , "Game:Open", "OPEN GAME"), MenuString("Open &Recent", , "RecentGames"), Array(), "-", MenuString("&Play", "F9", "Game:Play", "PLAY")), _
        MenuString("&Tools", , "ToolMenu"), Menus(, MenuString("-", , "PluginsEndSeparator"), MenuString("Manage Plugins...", , "Plugins:Manage", "PLUGIN"), MenuString("Options...", , "Show:Options", "PROPERTIES")), _
        MenuString("&Macros", , "MacroMenu"), Menus(MenuString("Run Macro...", , "Macro:Run"), MenuString("Enter Macro...", , "Macro:RunCustom")), _
        MenuString("&Window", , "WindowMenu"), Menus(MenuString("-", , "WindowsEndSeparator"), MenuString("Close All", , "Action:CloseAllWindows", "CLOSE ALL WINDOWS", , , False)), _
        MenuString("&Help", , "HelpMenu"), Menus(MenuString("Online &Documentation", , "Help:OnlineDocs", "HELP"), MenuString("Online &Tutorials", , "Help:OnlineTutorials", "HELP"), "-", MenuString("&About...", , "Help:About", "HELP"), MenuString("View ChangeLog", , "Help:ChangeLog", "HELP")))
End Sub

Public Sub SetMenuHandler(Name As String, Handler As Object)
On Error Resume Next
'    Set l_colMenus(LCase(Trim(Name))).EventHandler = Handler
End Sub

Public Sub SetMenuHandler2(Name As String, Handler As Object)
On Error Resume Next
Dim l_mnuMenu As ngMenu
Dim l_miItem As ngMenuItem
'    Set l_mnuMenu = l_colMenus2(LCase(Trim(Name)))
    With l_mnuMenu
        Set .SelectEvent = BindEvent(Handler, "Menu_Click")
        Set .ShowEvent = BindEvent(Handler, "Menu_Initialize", Array(l_mnuMenu))
        Set .HideEvent = BindEvent(Handler, "Menu_UnInitialize")
        SetChildHandlers l_mnuMenu, Handler
    End With
End Sub

Private Sub SetChildHandlers(ByVal Menu As ngMenu, Handler As Object)
On Error Resume Next
Dim l_mnuMenu As ngMenu
Dim l_miItem As ngMenuItem
    For Each l_miItem In Menu.Items
        If l_miItem.ChildMenu Is Nothing Then
        Else
            With l_miItem.ChildMenu
                Set .SelectEvent = BindEvent(Handler, "Menu_Click")
                Set .ShowEvent = BindEvent(Handler, "Menu_Initialize", Array(l_miItem.ChildMenu))
                Set .HideEvent = BindEvent(Handler, "Menu_UnInitialize")
            End With
            SetChildHandlers l_miItem.ChildMenu, Handler
        End If
    Next l_miItem
End Sub

Public Function GetMenu(Name As String) As cPopupMenu
On Error Resume Next
'    Set GetMenu = l_colMenus(LCase(Trim(Name))).Menu
End Function

Public Function GetMenuManager(Name As String) As cMenuManager
On Error Resume Next
'    Set GetMenuManager = l_colMenus(LCase(Trim(Name)))
End Function

Public Function GetMenu2(Name As String) As ngMenu
On Error Resume Next
'    Set GetMenu2 = l_colMenus2(LCase(Trim(Name)))
End Function

Public Sub SetMenuOwner(Name As String, hwnd As Long)
On Error Resume Next
'    l_colMenus(LCase(Trim(Name))).Menu.hWndOwner = hwnd
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
'    l_colMenus.Add l_mgrNew, LCase(Trim(Name))
    Set DefineMenu = l_mnuNew
    With l_mnuNew
        .hWndOwner = frmIcons.hwnd
        .HeaderStyle = ecnmHeaderSeparator
        .OfficeXpStyle = True
    End With
    ParseMenu l_mgrNew, Items
End Function

Public Function DefineMenu2(Name As String, Items As Variant) As ngMenu
On Error Resume Next
Dim l_mnuNew As ngMenu
    Set l_mnuNew = CreateMenu()
'    l_colMenus2.Add l_mnuNew, LCase(Trim(Name))
    Set DefineMenu2 = l_mnuNew
    ParseMenu2 l_mnuNew, Items
End Function

Public Function Menus(ParamArray Values() As Variant) As Variant
On Error Resume Next
Dim l_lngItems As Long
Dim l_varValue As Variant
    Err.Clear
    If UBound(Values) < LBound(Values) Then Exit Function
    If Err <> 0 Then Exit Function
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
    Err.Clear
    If UBound(Values) < LBound(Values) Then Exit Function
    If Err <> 0 Then Exit Function
    ReDim l_varValue(LBound(Values) To UBound(Values))
    For l_lngItems = LBound(Values) To UBound(Values)
        l_varValue(l_lngItems) = Values(l_lngItems)
    Next l_lngItems
    MenusFromStringArray = l_varValue
End Function

Public Function MenuString(ByRef Name As String, Optional ByRef Accelerator As String = "", Optional ByRef key As String = "", Optional ByVal Icon = "", Optional ByRef HelpText As String = "", Optional ByVal Checked As Boolean = False, Optional ByVal Enabled As Boolean = True, Optional ByVal ItemData As Long = -1) As String
On Error Resume Next
    MenuString = Name & "·" & Accelerator & "·" & ItemData & "·" & CStr(Icon) & "·" & Checked & "·" & Enabled & "·" & key
End Function

Public Function ParseMenu(Menu As cMenuManager, Items As Variant, Optional ByVal Parent As Long = -1, Optional ByRef InsertionPoint As Long = -1)
On Error Resume Next
Dim l_lngItems As Long, l_varItem As Variant, l_varParameters As Variant
    Err.Clear
    If UBound(Items) < LBound(Items) Then Exit Function
    If Err <> 0 Then Exit Function
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

Public Function ParseMenu2(Menu As ngMenu, Items As Variant, Optional ByRef InsertionPoint As Long = -1)
On Error Resume Next
Dim l_lngItems As Long, l_varItem As Variant, l_varParameters As Variant
Dim l_imgIcon As Fury2Image
    Err.Clear
    If UBound(Items) < LBound(Items) Then Exit Function
    If Err <> 0 Then Exit Function
    With Menu.Items
        For l_lngItems = LBound(Items) To UBound(Items)
            l_varItem = Items(l_lngItems)
            Select Case VarType(l_varItem)
            Case vbString
                If InStr(l_varItem, "·") Then
                    l_varParameters = Split(l_varItem, "·")
                    .AddNew CStr(l_varParameters(0)), CStr(l_varParameters(1)), CStr(l_varParameters(6)), l_varParameters(3), , , l_varParameters(5), l_varParameters(4), , , InsertionPoint
                Else
                    .AddNew CStr(l_varItem), , , , , , , , , , InsertionPoint
                End If
            Case vbVariant Or vbArray
                Set .Item(.Count).ChildMenu = CreateMenu()
                .Item(.Count).ChildMenu.Tag = .Item(.Count).key
                ParseMenu2 .Item(.Count).ChildMenu, l_varItem
            Case Else
            End Select
        Next l_lngItems
    End With
End Function

