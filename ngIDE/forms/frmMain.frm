VERSION 5.00
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Object = "{CA5A8E1E-C861-4345-8FF8-EF0A27CD4236}#1.0#0"; "vbalTreeView6.ocx"
Object = "{E142732F-A852-11D4-B06C-00500427A693}#2.0#0"; "vbalTbar6.ocx"
Object = "{76A5D4ED-0D69-44AD-835D-B1429EF8E25C}#1.1#0"; "vbalDkTb6.ocx"
Object = "{4F11FEBA-BBC2-4FB6-A3D3-AA5B5BA087F4}#1.0#0"; "vbalSbar6.ocx"
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Begin VB.MDIForm frmMain 
   AutoShowChildren=   0   'False
   BackColor       =   &H8000000C&
   Caption         =   "Editor²"
   ClientHeight    =   6600
   ClientLeft      =   60
   ClientTop       =   360
   ClientWidth     =   8325
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "MDIForm1"
   StartUpPosition =   3  'Windows Default
   Begin sMDIinActiveX.MDIActiveX maxContainer 
      Left            =   2970
      Top             =   405
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin VB.PictureBox picHiddenControls 
      Align           =   1  'Align Top
      BackColor       =   &H00808000&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3735
      Left            =   0
      ScaleHeight     =   249
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   555
      TabIndex        =   0
      Top             =   30
      Visible         =   0   'False
      Width           =   8325
      Begin VB.Timer tmrFocusTracker 
         Enabled         =   0   'False
         Interval        =   250
         Left            =   2025
         Top             =   885
      End
      Begin VB.Timer tmrRefreshFileSidebar 
         Enabled         =   0   'False
         Interval        =   1
         Left            =   2490
         Top             =   420
      End
      Begin VB.Timer tmrClock 
         Enabled         =   0   'False
         Interval        =   1000
         Left            =   2025
         Top             =   420
      End
      Begin VB.PictureBox picFileSidebar 
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   6.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000013&
         Height          =   3270
         Left            =   30
         ScaleHeight     =   218
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   130
         TabIndex        =   1
         Tag             =   "Hidden"
         Top             =   420
         Width           =   1950
         Begin vbalTreeViewLib6.vbalTreeView tvFileTree 
            Height          =   2655
            Left            =   15
            TabIndex        =   2
            Top             =   255
            Width           =   1920
            _ExtentX        =   3387
            _ExtentY        =   4683
            BorderStyle     =   0
            NoCustomDraw    =   0   'False
            HotTracking     =   0   'False
            Indentation     =   16
            LineColor       =   -2147483632
            LineStyle       =   0
            ScaleMode       =   3
            OLEDropMode     =   1
            DragAutoExpand  =   -1
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin vbalDTab6.vbalDTabControl tsFileTabs 
            Height          =   360
            Left            =   0
            TabIndex        =   3
            Top             =   2910
            Width           =   1950
            _ExtentX        =   3440
            _ExtentY        =   635
            AllowScroll     =   0   'False
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            BeginProperty SelectedFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ShowCloseButton =   0   'False
         End
      End
      Begin vbalTBar6.cToolbar tbrMenus 
         Height          =   360
         Left            =   30
         Top             =   15
         Width           =   1995
         _ExtentX        =   3519
         _ExtentY        =   635
         DrawStyle       =   2
      End
      Begin vbalTBar6.cToolbar tbrMain 
         Height          =   360
         Left            =   2055
         Top             =   15
         Width           =   1875
         _ExtentX        =   3307
         _ExtentY        =   635
         DrawStyle       =   2
      End
      Begin vbalTBar6.cToolbar tbrPlugins 
         Height          =   360
         Left            =   3945
         Top             =   15
         Width           =   2040
         _ExtentX        =   3598
         _ExtentY        =   635
         DrawStyle       =   2
      End
      Begin vbalTBar6.cToolbar tbrGame 
         Height          =   360
         Left            =   6015
         Top             =   15
         Width           =   1935
         _ExtentX        =   3413
         _ExtentY        =   635
         DrawStyle       =   2
      End
   End
   Begin vbalDkTb6.vbalDockContainer dockTop 
      Align           =   1  'Align Top
      Height          =   30
      Left            =   0
      TabIndex        =   4
      Top             =   0
      Width           =   8325
      _ExtentX        =   14684
      _ExtentY        =   53
      NonDockingArea  =   -1  'True
      NonDockingAreaSize=   1
   End
   Begin vbalDkTb6.vbalDockContainer dockLeft 
      Align           =   3  'Align Left
      Height          =   2505
      Left            =   0
      TabIndex        =   5
      Top             =   3765
      Width           =   30
      _ExtentX        =   53
      _ExtentY        =   4419
      NonDockingArea  =   -1  'True
      NonDockingAreaSize=   1
   End
   Begin vbalDkTb6.vbalDockContainer dockRight 
      Align           =   4  'Align Right
      Height          =   2505
      Left            =   8295
      TabIndex        =   6
      Top             =   3765
      Width           =   30
      _ExtentX        =   53
      _ExtentY        =   4419
      NonDockingArea  =   -1  'True
      NonDockingAreaSize=   1
   End
   Begin vbalDkTb6.vbalDockContainer dockBottom 
      Align           =   2  'Align Bottom
      Height          =   330
      Left            =   0
      TabIndex        =   7
      Top             =   6270
      Width           =   8325
      _ExtentX        =   14684
      _ExtentY        =   582
      NonDockingArea  =   -1  'True
      NonDockingAreaSize=   22
      Begin vbalSbar6.vbalStatusBar sbStatus 
         Height          =   300
         Left            =   30
         TabIndex        =   8
         Top             =   30
         Width           =   8265
         _ExtentX        =   14579
         _ExtentY        =   529
         SizeGrip        =   0   'False
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         BackColor       =   -2147483633
         SimpleStyle     =   0
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements iCustomMenuHandler
Implements iToolbarHandler
Private Const WM_MDIGETACTIVE = &H229

Private Declare Function ClientToScreen Lib "user32" (ByVal hwnd As Long, lpPoint As POINTAPI) As Long

Dim WithEvents m_mdiTabs As cMDITabs
Attribute m_mdiTabs.VB_VarHelpID = -1
Private m_aclMenus As cAcceleratorManager
Private m_booNonClientFocus As Boolean
Private m_colChildWindows As Engine.Fury2Collection
Private m_sngProgress As Single
Private m_cmnLastDocument As iCustomMenus

Public Sub RefreshGameState()
On Error Resume Next
    tbrMain.ButtonEnabled("File:Open") = GameIsLoaded
End Sub

Private Function GetToolbarX(Toolbar As cToolbar, Optional Docked As Boolean = True)
On Error Resume Next
Dim l_ptWindow As POINTAPI, l_ptToolbar As POINTAPI
    ClientToScreen Me.hwnd, l_ptWindow
    ClientToScreen Toolbar.hwnd, l_ptToolbar
    GetToolbarX = (l_ptToolbar.X - (IIf(Docked, l_ptWindow.X, 0)) - 10) * Screen.TwipsPerPixelX
End Function

Private Function GetToolbarY(Toolbar As cToolbar, Optional Docked As Boolean = True)
On Error Resume Next
Dim l_ptWindow As POINTAPI, l_ptToolbar As POINTAPI
    ClientToScreen Me.hwnd, l_ptWindow
    ClientToScreen Toolbar.hwnd, l_ptToolbar
    GetToolbarY = (l_ptToolbar.Y - (IIf(Docked, l_ptWindow.Y, 0)) - 23) * Screen.TwipsPerPixelY
End Function

Public Sub DocumentClosed(Document As cChildManager)
On Error Resume Next
Dim l_mgrForm As cChildManager
Dim l_booHidden As Boolean
    m_colChildWindows.Remove m_colChildWindows.Find(Document)
    m_colChildWindows(m_colChildWindows.Count).Activate
    HideInactiveWindows
End Sub

Public Sub HideInactiveWindows()
On Error Resume Next
Dim l_lnghWnd As Long
Dim l_mgrForm As cChildManager
    l_lnghWnd = maxContainer.ActiveWindow
    For Each l_mgrForm In m_colChildWindows
        If (l_mgrForm.Form.hwnd = l_lnghWnd) Then
            l_mgrForm.Visible = True
        ElseIf (l_mgrForm.Form.Extender.hwnd = l_lnghWnd) Then
            l_mgrForm.Visible = True
        Else
            l_mgrForm.Visible = False
        End If
    Next l_mgrForm
End Sub

Public Property Get NonClientActive() As Boolean
On Error Resume Next
    NonClientActive = m_booNonClientFocus
End Property

Public Property Get ActiveChild() As cChildManager
On Error Resume Next
Dim l_lnghWnd As Long
Dim l_mgrForm As cChildManager
    l_lnghWnd = maxContainer.ActiveWindow
    For Each l_mgrForm In m_colChildWindows
        If (l_mgrForm.Form.hwnd = l_lnghWnd) Then
            Set ActiveChild = l_mgrForm
            Exit For
        ElseIf (l_mgrForm.Form.Extender.hwnd = l_lnghWnd) Then
            Set ActiveChild = l_mgrForm
            Exit For
        End If
    Next l_mgrForm
End Property

Public Property Get Documents() As Engine.Fury2Collection
On Error Resume Next
    Set Documents = m_colChildWindows
End Property

Public Sub ActivateChild(Index As Long)
On Error Resume Next
Dim l_mgrForm As cChildManager
    Set l_mgrForm = m_colChildWindows(Index)
    l_mgrForm.Activate
End Sub

Public Sub CloseAllChildren()
On Error Resume Next
Dim l_lngForm As Long, l_mgrChild As cChildManager
Dim l_sngScale As Single
    l_sngScale = 1 / m_colChildWindows.Count
    SetProgress 0
    SetStatus "Closing Windows"
    SetBusyState True
    l_lngForm = 0
    For Each l_mgrChild In m_colChildWindows
        If l_mgrChild Is Nothing Then
        Else
            l_mgrChild.Hide
        End If
        l_lngForm = l_lngForm + 1
        SetProgress 1 - (l_lngForm * l_sngScale)
        DoEvents
    Next l_mgrChild
    SetProgress 100
    RefreshWindows
    SetStatus
    SetProgress
    SetBusyState False
    Err.Clear
End Sub

Public Sub RefreshFileSidebar()
On Error Resume Next
    If picFileSidebar.Tag <> "" Then Exit Sub
    tmrRefreshFileSidebar.Enabled = False
    tmrRefreshFileSidebar.Interval = 0
    tmrRefreshFileSidebar.Interval = 1
    tmrRefreshFileSidebar.Enabled = True
End Sub

Public Sub SetStatus(Optional ByRef Status As String = "Ready")
On Error Resume Next
    sbStatus.PanelText("Status") = Status
    sbStatus.RedrawPanel "Status"
    'DoEvents
End Sub

Public Sub SetProgress(Optional ByVal Progress As Single = 0)
On Error Resume Next
    m_sngProgress = Progress
    sbStatus.RedrawPanel "Progress"
    'DoEvents
End Sub

Public Sub ShowChild(ByRef Child As Object)
On Error Resume Next
Dim l_mgrManager As cChildManager
    If TypeOf Child Is cNullDocument Then Exit Sub
    Load Child
    Set l_mgrManager = New cChildManager
    l_mgrManager.Attach Child
    m_colChildWindows.Add l_mgrManager
    Child.WindowState = 2
    Child.Extender.WindowState = 2
    Child.Show
    m_mdiTabs.ForceRefresh
    g_edEditor.Event_DocumentActivate l_mgrManager
    Err.Clear
End Sub

Friend Sub DereferenceChildManager(ByRef Manager As cChildManager)
On Error Resume Next
    m_colChildWindows.Remove m_colChildWindows.Find(Manager)
End Sub

Public Sub MenuActivate(ByRef Accelerator As cAccelerator)
On Error Resume Next
Dim l_lngButtons As Long, l_strCaption As String, l_strAccel As String
    For l_lngButtons = 0 To tbrMenus.ButtonCount - 1
        l_strCaption = tbrMenus.ButtonCaption(l_lngButtons)
        tbrMenus.ButtonStyle(l_lngButtons) = CTBDropDownArrow Or CTBAutoSize
        If InStr(l_strCaption, "&") Then
            l_strAccel = UCase(Mid(l_strCaption, InStr(l_strCaption, "&") + 1, 1))
            If l_strAccel = Chr(Accelerator.KeyCode) Then
                tbrMenus.TriggerButtonClick l_lngButtons
                Exit For
            End If
        End If
    Next l_lngButtons
End Sub

Public Sub RefreshMenus()
On Error Resume Next
Dim l_lngButtons As Long
Dim l_strAccel As String, l_strCaption As String
    If m_aclMenus Is Nothing Then
    Else
        m_aclMenus.Detach
        Set m_aclMenus = Nothing
    End If
    tbrMenus.DestroyToolBar
    tbrMenus.CreateFromMenu GetMenu("Main Menu")
    tbrMenus.Wrappable = True
    tbrMenus.ListStyle = True
    With FindDocked("Menu Bar")
        .BandSizeChange "Menu Bar", tbrMenus.ToolbarWidth, tbrMenus.ToolbarHeight, 0, 0
    End With
    Set m_aclMenus = New cAcceleratorManager
    m_aclMenus.Attach Me.hwnd
    For l_lngButtons = 0 To tbrMenus.ButtonCount - 1
        l_strCaption = tbrMenus.ButtonCaption(l_lngButtons)
        tbrMenus.ButtonStyle(l_lngButtons) = CTBDropDownArrow Or CTBAutoSize
        If InStr(l_strCaption, "&") Then
            l_strAccel = Mid(l_strCaption, InStr(l_strCaption, "&") + 1, 1)
            m_aclMenus.AddAccelerator Asc(UCase(l_strAccel)), Me, "MenuActivate", , , True
        End If
    Next l_lngButtons
End Sub

Public Sub RefreshActiveDocument()
On Error Resume Next
Dim l_mgrDocument As cChildManager
    Set l_mgrDocument = ActiveChild
    If l_mgrDocument Is Nothing Then
        tbrMain.ButtonEnabled("File:Save") = False
        With GetMenu("Main Menu")
            .Enabled(.IndexForKey("File:Save")) = False
            .Enabled(.IndexForKey("File:SaveAs")) = False
        End With
    Else
        tbrMain.ButtonEnabled("File:Save") = l_mgrDocument.Document.CanSave
        With GetMenu("Main Menu")
            .Enabled(.IndexForKey("File:Save")) = l_mgrDocument.Document.CanSave
            .Enabled(.IndexForKey("File:SaveAs")) = l_mgrDocument.Document.CanSave
        End With
    End If
End Sub

Public Sub RefreshPluginToolbar()
On Error Resume Next
Dim l_plgPlugin As iPlugin
Dim l_icnIcon As IPictureDisp, l_icnPlugin As IPictureDisp
Dim l_lngPluginIndex As Long
    If tbrPlugins.ButtonCount < 2 Then
        tbrPlugins.Wrappable = True
        tbrPlugins.CreateToolbar 16, False, False, True, 16
        tbrPlugins.Wrappable = True
        frmIcons.ilPluginIcons.Clear
        Set l_icnPlugin = frmIcons.ilIcons.ItemPicture(frmIcons.ilIcons.ItemIndex("PLUGIN"))
        frmIcons.ilPluginIcons.AddFromHandle l_icnPlugin.Handle, Image_Icon, "PLUGIN"
        Set l_icnPlugin = Nothing
        DefineToolbar tbrPlugins, frmIcons.ilPluginIcons, _
        Buttons(ButtonString("Manage Plugins", , "Plugins:Manage", "PLUGIN"), "-")
    End If
    Do While tbrPlugins.ButtonCount > 2
        tbrPlugins.RemoveButton tbrPlugins.ButtonCount - 1
    Loop
    l_lngPluginIndex = 1
    If g_colPlugins.Count > 0 Then
        For Each l_plgPlugin In g_colPlugins
            If l_plgPlugin Is Nothing Then
            Else
                With l_plgPlugin
                    If .ShowInPluginMenu Then
                        If CLng(ReadRegSetting("Plugins\Show In Toolbar\" & TypeName(l_plgPlugin), 1)) Then
                            Set l_icnIcon = Nothing
                            Set l_icnIcon = .Icon
                            If l_icnIcon Is Nothing Then
                                tbrPlugins.AddButton .PluginName, frmIcons.ilPluginIcons.ItemIndex("PLUGIN") - 1, , , , , "Plugins:Activate(" & l_lngPluginIndex & ")"
                            Else
                                frmIcons.ilPluginIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, "ICON_" & l_icnIcon.Handle
                                tbrPlugins.AddButton .PluginName, frmIcons.ilPluginIcons.ItemIndex("ICON_" & l_icnIcon.Handle) - 1, , , , , "Plugins:Activate(" & l_lngPluginIndex & ")"
                            End If
                        End If
                    End If
                End With
            End If
            l_lngPluginIndex = l_lngPluginIndex + 1
        Next l_plgPlugin
    End If
    With FindDocked("Plugin Toolbar")
        .BandSizeChange "Plugin Toolbar", tbrPlugins.ToolbarWidth, tbrPlugins.ToolbarHeight, vbal_getVerticalHeight(tbrPlugins), vbal_getVerticalWidth(tbrPlugins)
    End With
End Sub

Public Sub RefreshWindows()
On Error Resume Next
    m_mdiTabs.ForceRefresh
End Sub

Private Sub DockShowToolbar(Container As vbalDockContainer, Toolbar As cToolbar, Title As String, Optional XDockable As Boolean = True, Optional YDockable As Boolean = True, Optional CanUndock As Boolean = True, Optional CanClose As Boolean = True, Optional FullRow As Boolean = False, Optional Row As Long = -1)
On Error Resume Next
Dim l_lngRow As Long
    With Container
        Toolbar.Tag = ""
        If .RowCount = 0 Then
            l_lngRow = -1
        Else
            l_lngRow = Row
        End If
        Do While l_lngRow <= .RowCount
            Err.Clear
            .Add Title, Toolbar.ToolbarWidth, Toolbar.ToolbarHeight, vbal_getVerticalHeight(Toolbar), vbal_getVerticalWidth(Toolbar), Title, l_lngRow, , , FullRow, XDockable, YDockable, CanClose, True, CanUndock
            If Err <> 0 Then
                If l_lngRow = -1 Then Exit Do
                l_lngRow = l_lngRow + 1
                If l_lngRow > .RowCount Then
                    l_lngRow = -1
                End If
            Else
                Exit Do
            End If
        Loop
        .Capture Title, Toolbar.hwnd
    End With
End Sub

Public Sub ShowDockObject(key As String)
On Error Resume Next
    If FindDocked(key) Is Nothing Then
        Select Case LCase(Trim(key))
        Case "file sidebar"
            picFileSidebar.Tag = ""
            dockRight.Add "File Sidebar", 150, 350, Screen.Height, 150, "Filesystem", , , , True, False, True, True, False, True
            dockRight.Capture "File Sidebar", picFileSidebar.hwnd
            RefreshFileSidebar
        Case "main toolbar"
            DockShowToolbar dockTop, tbrMain, "Main Toolbar", True, True, True, True, False, 1
        Case "game toolbar"
            DockShowToolbar dockTop, tbrGame, "Game Toolbar", True, True, True, True, False, 1
        Case "plugin toolbar"
            DockShowToolbar dockTop, tbrPlugins, "Plugin Toolbar", True, True, True, True, False, 1
        Case "menu bar"
            DockShowToolbar dockTop, tbrMenus, "Menu Bar", True, False, False, False, True, 0
        Case Else
        End Select
    Else
    End If
    StatusBarReposition
    ResizeSidebars
    m_mdiTabs.ForceRefresh
End Sub

Public Sub HideDockObject(key As String)
On Error Resume Next
    Select Case LCase(Trim(key))
    Case "file sidebar"
        picFileSidebar.Tag = "Hidden"
    Case "main toolbar"
        tbrMain.Tag = "Hidden"
    Case "game toolbar"
        tbrGame.Tag = "Hidden"
    Case "plugin toolbar"
        tbrPlugins.Tag = "Hidden"
    Case "menu bar"
    Case Else
    End Select
    With dockTop
        .RemoveUndocked key
        .Remove key
    End With
    With dockRight
        .RemoveUndocked key
        .Remove key
    End With
    With dockLeft
        .RemoveUndocked key
        .Remove key
    End With
    With dockBottom
        .RemoveUndocked key
        .Remove key
    End With
    StatusBarReposition
    ResizeSidebars
End Sub

Public Function FindDocked(key As String) As vbalDockContainer
On Error Resume Next
    If dockTop.IsDocked(key) Then
        Set FindDocked = dockTop
    ElseIf dockBottom.IsDocked(key) Then
        Set FindDocked = dockBottom
    ElseIf dockLeft.IsDocked(key) Then
        Set FindDocked = dockLeft
    ElseIf dockRight.IsDocked(key) Then
        Set FindDocked = dockRight
    End If
End Function

Public Function FindOwned(key As String) As vbalDockContainer
On Error Resume Next
    If dockTop.OwnsKey(key) Then
        Set FindOwned = dockTop
    ElseIf dockBottom.OwnsKey(key) Then
        Set FindOwned = dockBottom
    ElseIf dockLeft.OwnsKey(key) Then
        Set FindOwned = dockLeft
    ElseIf dockRight.OwnsKey(key) Then
        Set FindOwned = dockRight
    End If
End Function

Public Sub ResizeSidebars()
On Error Resume Next
    dockLeft.AutoSize
    dockRight.AutoSize
    picFileSidebar_Resize
End Sub

Private Sub StatusBarReposition()
On Error Resume Next
Dim l_lngLeft As Long, l_lngTop As Long, l_lngWidth As Long, l_lngHeight As Long
    If dockBottom.NonDockingAreaSize <> (sbStatus.Height / Screen.TwipsPerPixelY) Then
        dockBottom.NonDockingAreaSize = (sbStatus.Height / Screen.TwipsPerPixelY)
    End If
    sbStatus.Move 0, dockBottom.Height - sbStatus.Height, dockBottom.Width, sbStatus.Height
    sbStatus.GetPanelRect "Progress", l_lngLeft, l_lngTop, l_lngWidth, l_lngHeight
End Sub

Private Sub docks_GotFocusHandler()
On Error Resume Next
    Exit Sub
    m_booNonClientFocus = True
    g_edEditor.ActionUpdate
End Sub

Private Sub docks_LostFocusHandler()
On Error Resume Next
    Exit Sub
    m_booNonClientFocus = False
    g_edEditor.ActionUpdate
End Sub

Private Sub docks_ChevronHandler(ByVal key As String, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    Select Case LCase(Trim(key))
    Case "menu bar"
        tbrMenus.ChevronPress X, Y
    Case "main toolbar"
        tbrMain.ChevronPress X, Y
    Case "file sidebar"
    Case Else
    End Select
End Sub

Private Sub docks_CloseHandler(sKey As String, bCancel As Boolean)
On Error Resume Next
    Select Case LCase(Trim(sKey))
    Case "menu bar"
        bCancel = True
    Case "main toolbar"
        tbrMain.Tag = "Hidden"
    Case "file sidebar"
        picFileSidebar.Tag = "Hidden"
    Case Else
    End Select
    If Not bCancel Then HideDockObject sKey
End Sub

Private Sub dockBottom_BarClose(ByVal sKey As String, bCancel As Boolean)
On Error Resume Next
    docks_CloseHandler sKey, bCancel
End Sub

Private Sub dockBottom_ChevronPress(ByVal key As String, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    docks_ChevronHandler key, X, Y
End Sub

Private Sub dockBottom_Docked(ByVal key As String)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockBottom_GotFocus()
On Error Resume Next
    docks_GotFocusHandler
End Sub

Private Sub dockBottom_LostFocus()
On Error Resume Next
    docks_LostFocusHandler
End Sub

Private Sub dockBottom_Undocked(ByVal key As String)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockLeft_BarClose(ByVal sKey As String, bCancel As Boolean)
On Error Resume Next
    docks_CloseHandler sKey, bCancel
End Sub

Private Sub dockLeft_ChevronPress(ByVal key As String, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    docks_ChevronHandler key, X, Y
End Sub

Private Sub dockLeft_Docked(ByVal key As String)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockLeft_GotFocus()
On Error Resume Next
    docks_GotFocusHandler
End Sub

Private Sub dockLeft_LostFocus()
On Error Resume Next
    docks_LostFocusHandler
End Sub

Private Sub dockLeft_Undocked(ByVal key As String)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockRight_BarClose(ByVal sKey As String, bCancel As Boolean)
On Error Resume Next
    docks_CloseHandler sKey, bCancel
End Sub

Private Sub dockRight_ChevronPress(ByVal key As String, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    docks_ChevronHandler key, X, Y
End Sub

Private Sub dockRight_Docked(ByVal key As String)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockRight_GotFocus()
On Error Resume Next
    docks_GotFocusHandler
End Sub

Private Sub dockRight_LostFocus()
On Error Resume Next
    docks_LostFocusHandler
End Sub

Private Sub dockRight_Validate(Cancel As Boolean)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockTop_BarClose(ByVal sKey As String, bCancel As Boolean)
On Error Resume Next
    docks_CloseHandler sKey, bCancel
End Sub

Private Sub dockTop_ChevronPress(ByVal key As String, ByVal X As Long, ByVal Y As Long)
On Error Resume Next
    docks_ChevronHandler key, X, Y
End Sub

Public Sub InitToolbars()
On Error Resume Next
    RefreshMenus
    tbrMain.CreateToolbar 16, False, False, True, 16
    tbrMain.Wrappable = True
    DefineToolbar tbrMain, frmIcons.ilIcons, _
    Buttons(ButtonString("New", , "File:New", "NEW", True, CTBDropDownArrow), ButtonString("Open", , "File:Open", "OPEN", False, CTBDropDown), ButtonString("Save", , "File:Save", "SAVE", False), "-", _
    ButtonString("Undo", , "Action:Undo", "UNDO"), ButtonString("Redo", , "Action:Redo", "REDO"), "-", _
    ButtonString("Cut", , "Action:Cut", "CUT"), ButtonString("Copy", , "Action:Copy", "COPY"), ButtonString("Paste", , "Action:Paste", "PASTE"), ButtonString("Delete", , "Action:Delete", "DELETE"), "-", _
    ButtonString("Select All", , "Action:SelectAll", "SELECT ALL"), ButtonString("Select None", , "Action:SelectNone", "SELECT NONE"))
    tbrGame.CreateToolbar 16, False, False, True, 16
    tbrGame.Wrappable = True
    DefineToolbar tbrGame, frmIcons.ilIcons, _
    Buttons(ButtonString("Open Game", , "Game:Open", "OPEN GAME", , CTBDropDown), "-", _
    ButtonString("Play Game", , "Game:Play", "PLAY"))
    RefreshPluginToolbar
End Sub

Public Sub InitStatus()
On Error Resume Next
    sbStatus.AddPanel estbrStandard Or estbrNoBorders, "Ready", , , 4, True, False, , "Status"
    sbStatus.AddPanel estbrOwnerDraw, , "Progress", , 115, False, False, , "Progress"
    sbStatus.AddPanel estbrStandard Or estbrNoBorders, CStr(Now), , , 4, False, True, , "Time"
End Sub

Public Sub InitSidebars()
On Error Resume Next
    tsFileTabs.ImageList = frmIcons.ilIcons.hIml
    tsFileTabs.Tabs.Add "Game", , "Game", frmIcons.ilIcons.ItemIndex("GAME") - 1
    tsFileTabs.Tabs.Add "PC", , "My PC", frmIcons.ilIcons.ItemIndex("PC") - 1
End Sub

Public Sub InitDocking()
On Error Resume Next
    If InIDE Then
        dockBottom.AllowUndock = False
        dockTop.AllowUndock = False
        dockLeft.AllowUndock = False
        dockRight.AllowUndock = False
    End If
    ShowDockObject "Menu Bar"
    ShowDockObject "Main Toolbar"
    ShowDockObject "Game Toolbar"
    ShowDockObject "Plugin Toolbar"
End Sub

Public Sub InitMenus()
On Error Resume Next
Dim l_mnuMenu As cPopupMenu
    Set l_mnuMenu = GetMenu("Main Menu")
    With l_mnuMenu
    End With
End Sub

Public Sub InitMDITabs()
On Error Resume Next
    Set m_mdiTabs = New cMDITabs
    m_mdiTabs.Attach Me.hwnd
End Sub

Private Sub dockTop_Docked(ByVal key As String)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockTop_GotFocus()
On Error Resume Next
    docks_GotFocusHandler
End Sub

Private Sub dockTop_LostFocus()
On Error Resume Next
    docks_LostFocusHandler
End Sub

Private Sub dockTop_Undocked(ByVal key As String)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub dockTop_Validate(Cancel As Boolean)
On Error Resume Next
    StatusBarReposition
    ResizeSidebars
End Sub

Private Sub iCustomMenuHandler_DefineMenu(Caption As String, key As String, Optional ParentKey As String, Optional AcceleratorString As String = "", Optional Icon As stdole.Picture = Nothing, Optional HelpText As String = "", Optional ByVal Checked As Boolean = False, Optional ByVal Enabled As Boolean = True)
On Error Resume Next
    With GetMenu("Main Menu")
        If Icon Is Nothing Then
            If IsMissing(ParentKey) Or Trim(ParentKey) = "" Then
                .InsertItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), "DocumentEndSeparator", HelpText, , , Checked, Enabled, "CustomMenu(""" & key & """)"
            Else
                .AddItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), HelpText, , .IndexForKey("CustomMenu(""" & ParentKey & """)"), , Checked, Enabled, "CustomMenu(""" & key & """)"
            End If
        Else
            frmIcons.ilIcons.AddFromHandle Icon.Handle, Image_Icon, "Child_" & key
            If IsMissing(ParentKey) Or Trim(ParentKey) = "" Then
                .InsertItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), "DocumentEndSeparator", HelpText, , frmIcons.ilIcons.ImageCount - 1, Checked, Enabled, "CustomMenu(""" & key & """)"
            Else
                .AddItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), HelpText, , .IndexForKey("CustomMenu(""" & ParentKey & """)"), frmIcons.ilIcons.ImageCount - 1, Checked, Enabled, "CustomMenu(""" & key & """)"
            End If
        End If
    End With
    Err.Clear
End Sub

Private Sub iCustomMenuHandler_DestroyMenu(key As String)
On Error Resume Next
    frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex("Child_" & key)
    With GetMenu("Main Menu")
        .RemoveItem "CustomMenu(""" & key & """)"
    End With
    Err.Clear
End Sub

Private Sub iToolbarHandler_AddToolbar(ByVal Name As String, ByVal Window As Long, Options As ngInterfaces.ToolbarOptions)
On Error Resume Next
Dim l_lngRow As Long
Dim l_dckContainer As vbalDockContainer
    Select Case Options.DefaultPosition
    Case TBP_Top
        Set l_dckContainer = dockTop
    Case TBP_Right
        Set l_dckContainer = dockRight
    Case TBP_Bottom
        Set l_dckContainer = dockBottom
    Case TBP_Left
        Set l_dckContainer = dockLeft
    End Select
    With l_dckContainer
        If .RowCount = 0 Then
            l_lngRow = -1
        Else
            l_lngRow = 0
        End If
        Do While l_lngRow <= .RowCount
            Err.Clear
            With Options
                l_dckContainer.Add Name, .XWidth, .XHeight, .YHeight, .YWidth, .Title, l_lngRow, , , .FullRow, .XDockable, .YDockable, .Closable, .ShowChevron, .Undockable
            End With
            If Err <> 0 Then
                If l_lngRow = -1 Then Exit Do
                l_lngRow = l_lngRow + 1
                If l_lngRow > .RowCount Then
                    l_lngRow = -1
                End If
            Else
                Exit Do
            End If
        Loop
        .Capture Name, Window
    End With
    StatusBarReposition
    ResizeSidebars
    m_mdiTabs.ForceRefresh
End Sub

Private Sub iToolbarHandler_RemoveToolbar(ByVal Name As String)
On Error Resume Next
    With FindOwned(Name)
        .Remove Name
    End With
    StatusBarReposition
    ResizeSidebars
    m_mdiTabs.ForceRefresh
End Sub

Private Sub iToolbarHandler_ResizeToolbar(ByVal Name As String, ByVal Window As Long, Options As ngInterfaces.ToolbarOptions)
On Error Resume Next
Dim l_lngRow As Long
Dim l_dckContainer As vbalDockContainer
    Set l_dckContainer = FindDocked(Name)
    With Options
        l_dckContainer.BandSizeChange Name, .XWidth, .XHeight, .YHeight, .YWidth
        If Window <> 0 Then l_dckContainer.Capture Name, Window
    End With
    StatusBarReposition
    ResizeSidebars
    m_mdiTabs.ForceRefresh
End Sub

Private Sub m_mdiTabs_BeforeWindowSwitch(ByVal hwnd As Long, Cancel As Boolean)
On Error Resume Next
Dim l_booFound As Boolean
Dim l_mgrForm As cChildManager
    If Me.Enabled = False Then
        Cancel = True
    Else
        For Each l_mgrForm In m_colChildWindows
            Err.Clear
            If (l_mgrForm.Form.hwnd = hwnd) Then
                l_booFound = True
                Exit For
            ElseIf l_mgrForm.Extender Is Nothing Then
            Else
                If l_mgrForm.Extender.hwnd = hwnd Then
                    l_booFound = True
                    Exit For
                End If
            End If
        Next l_mgrForm
        If l_booFound Then
        Else
            Cancel = True
        End If
    End If
End Sub

Private Sub m_mdiTabs_CloseWindow(ByVal hwnd As Long)
On Error Resume Next
    If Not Me.Enabled Then Exit Sub
    SetBusyState True
    g_edEditor.Action_CloseWindow
    SetBusyState False
End Sub

Private Sub m_mdiTabs_TabClick(ByVal iButton As MouseButtonConstants, ByVal hwnd As Long, ByVal screenX As Long, ByVal screenY As Long)
On Error Resume Next
Dim l_mgrForm As cChildManager
    If Not Me.Enabled Then Exit Sub
    For Each l_mgrForm In m_colChildWindows
        Err.Clear
        If (l_mgrForm.Form.hwnd = hwnd) Then
            l_mgrForm.Activate
            Exit For
        ElseIf l_mgrForm.Extender Is Nothing Then
        Else
            If l_mgrForm.Extender.hwnd = hwnd Then
                l_mgrForm.Activate
                Exit For
            End If
        End If
    Next l_mgrForm
End Sub

Private Sub m_mdiTabs_WindowChanged(ByVal hwnd As Long)
On Error Resume Next
Dim l_mgrChild As cChildManager
    Set l_mgrChild = Me.ActiveChild
    g_edEditor.Event_DocumentActivate l_mgrChild
End Sub

Private Sub MDIForm_Activate()
On Error Resume Next
    tmrClock.Enabled = True
    tmrFocusTracker.Enabled = True
End Sub

Private Sub MDIForm_Deactivate()
On Error Resume Next
End Sub

Private Sub MDIForm_Load()
On Error Resume Next
    g_booMainWindowLoaded = True
    Set m_colChildWindows = New Engine.Fury2Collection
    InitMDITabs
    InitSidebars
    InitToolbars
    InitStatus
    InitDocking
    InitMenus
    StatusBarReposition
End Sub

Private Sub MDIForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
Dim l_lngForms As Long
    tvFileTree.ImageList = 0
    Select Case UnloadMode
    Case 0
        Cancel = True
        ExitProgram
    Case Else
        For l_lngForms = Forms.Count To 0 Step -1
            If Forms(l_lngForms).MDIChild Then
                Forms(l_lngForms).Hide
                Unload Forms(l_lngForms)
            End If
        Next l_lngForms
    End Select
End Sub

Private Sub MDIForm_Resize()
On Error Resume Next
End Sub

Private Sub MDIForm_Unload(Cancel As Integer)
    g_booMainWindowLoaded = False
End Sub

Private Sub picFileSidebar_Paint()
On Error Resume Next
    picFileSidebar.Cls
    picFileSidebar.Line (0, 0)-(picFileSidebar.ScaleWidth - 1, picFileSidebar.ScaleHeight - 1), SystemColorConstants.vb3DShadow, B
    picFileSidebar.Line (2, 2)-(picFileSidebar.ScaleWidth - 3, 14), SystemColorConstants.vbInactiveTitleBar, BF
    picFileSidebar.CurrentX = 3
    picFileSidebar.CurrentY = 2
    picFileSidebar.Print "Filesystem"
End Sub

Private Sub picFileSidebar_Resize()
On Error Resume Next
    tsFileTabs.Move 0, picFileSidebar.ScaleHeight - tsFileTabs.Height, picFileSidebar.ScaleWidth, tsFileTabs.Height
    tvFileTree.Move 2, 16, picFileSidebar.ScaleWidth - 4, picFileSidebar.ScaleHeight - (tsFileTabs.Height + 16)
End Sub

Private Sub picHiddenControls_Resize()
On Error Resume Next
    StatusBarReposition
End Sub

Private Sub sbStatus_DrawItem(ByVal lHDC As Long, ByVal iPanel As Long, ByVal lLeftPixels As Long, ByVal lTopPixels As Long, ByVal lRightPixels As Long, ByVal lBottomPixels As Long)
On Error Resume Next
Dim l_rctProgress As RECT
Dim l_lngBrush As Long
    If LCase(sbStatus.PanelKey(iPanel)) = "progress" Then
        With l_rctProgress
            .left = lLeftPixels
            .tOp = lTopPixels
            .Right = lRightPixels
            .Bottom = lBottomPixels
        End With
        l_lngBrush = gdi32.CreateSolidBrush(GetSystemColor(SystemColor_Button_Face))
        user32.FillRect lHDC, l_rctProgress, l_lngBrush
        DeleteObject l_lngBrush
        With l_rctProgress
            .Right = ClipValue(lLeftPixels + ((lRightPixels - lLeftPixels) * m_sngProgress), lLeftPixels, lRightPixels)
        End With
        l_lngBrush = gdi32.CreateSolidBrush(GetSystemColor(SystemColor_Highlight))
        user32.FillRect lHDC, l_rctProgress, l_lngBrush
        DeleteObject l_lngBrush
    End If
End Sub

Private Sub sbStatus_GotFocus()
On Error Resume Next
    m_booNonClientFocus = True
    g_edEditor.ActionUpdate
End Sub

Private Sub sbStatus_LostFocus()
On Error Resume Next
    m_booNonClientFocus = False
    g_edEditor.ActionUpdate
End Sub

Private Sub tbrGame_ButtonClick(ByVal lButton As Long)
On Error Resume Next
    ReleaseCapture
    Err.Clear
    If DoCommand(tbrGame.ButtonKey(lButton)) Then
    Else
        Debug.Print "Button: " & tbrGame.ButtonKey(lButton) & " has no command handler."
    End If
End Sub

Private Sub tbrGame_DropDownPress(ByVal lButton As Long)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_ptPosition As POINTAPI
    tbrGame.GetDropDownPosition lButton, l_lngX, l_lngY
    l_lngX = l_lngX + GetToolbarX(tbrGame)
    l_lngY = l_lngY + GetToolbarY(tbrGame)
    ReleaseCapture
    SetForegroundWindow Me.hwnd
    Select Case LCase(Trim(tbrGame.ButtonKey(lButton)))
    Case "game:open"
        ShowMenu Me, l_lngX, l_lngY + (tbrGame.ButtonHeight(lButton) * Screen.TwipsPerPixelY), "Main Menu", "RecentGames"
    Case Else
    End Select
End Sub

Private Sub tbrMain_ButtonClick(ByVal lButton As Long)
On Error Resume Next
    ReleaseCapture
    Err.Clear
    If DoCommand(tbrMain.ButtonKey(lButton)) Then
    Else
        Debug.Print "Button: " & tbrMain.ButtonKey(lButton) & " has no command handler."
    End If
End Sub

Private Sub tbrMain_CustomiseBegin()
On Error Resume Next
End Sub

Private Sub tbrMain_DropDownPress(ByVal lButton As Long)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    tbrMain.GetDropDownPosition lButton, l_lngX, l_lngY
    l_lngX = l_lngX + GetToolbarX(tbrMain)
    l_lngY = l_lngY + GetToolbarY(tbrMain)
    ReleaseCapture
    SetForegroundWindow Me.hwnd
    Select Case LCase(Trim(tbrMain.ButtonKey(lButton)))
    Case "file:new"
        ShowMenu Me, l_lngX, l_lngY + (tbrMain.ButtonHeight(lButton) * Screen.TwipsPerPixelY), "Main Menu", "NewMenu"
    Case "file:open"
        ShowMenu Me, l_lngX, l_lngY + (tbrMain.ButtonHeight(lButton) * Screen.TwipsPerPixelY), "Main Menu", "RecentFiles"
    Case Else
    End Select
End Sub

Private Sub tbrPlugins_ButtonClick(ByVal lButton As Long)
On Error Resume Next
    ReleaseCapture
    Err.Clear
    If DoCommand(tbrPlugins.ButtonKey(lButton)) Then
    Else
        Debug.Print "Button: " & tbrPlugins.ButtonKey(lButton) & " has no command handler."
    End If
End Sub

Private Sub tmrClock_Timer()
On Error Resume Next
    sbStatus.PanelText("Time") = CStr(Now)
    RefreshWindows
    RefreshActiveDocument
End Sub

Public Sub Menu_Click(Menu As cPopupMenu, Index As Long)
On Error Resume Next
    Err.Clear
    If DoCommand(Menu.ItemKey(Index)) Then
    Else
        Debug.Print "Menu: " & Menu.ItemKey(Index) & " has no command handler."
    End If
End Sub

Public Sub Menu_Initialize(Menu As cPopupMenu, Index As Long)
On Error Resume Next
Dim l_lngIndex As Long
Dim l_frmForm As Form, l_lngForm As Long, l_lngWindowsAdded As Long, l_strName As String
Dim l_fpgPlugin As iFileTypePlugin, l_lngPluginIndex As Long, l_strKey As String
Dim l_plgPlugin As iPlugin, l_icnIcon As IPictureDisp, l_mgrForm As cChildManager
Dim l_cmnDocument As iCustomMenus, l_lngFileCount As Long, l_lngFiles As Long, l_strFilename As String
    Select Case LCase(Trim(Replace(Menu.Caption(Index), "&", "")))
    Case "new"
        l_lngIndex = Index
        Do While l_lngIndex <= Menu.Count
            If InStr(Menu.ItemKey(l_lngIndex), "File:New(") Then
                If Menu.ItemIcon(l_lngIndex) > 0 Then
                    frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex(Menu.ItemKey(l_lngIndex))
                End If
                Menu.RemoveItem l_lngIndex
            Else
                l_lngIndex = l_lngIndex + 1
            End If
        Loop
        l_lngPluginIndex = 1
        For Each l_fpgPlugin In g_colFileTypePlugins
            Set l_plgPlugin = l_fpgPlugin
            If l_fpgPlugin.ShowInNewMenu Then
                If ReadRegSetting("Plugins\Show In New Menu\" & TypeName(l_fpgPlugin), 1) Then
                    Set l_icnIcon = l_plgPlugin.Icon
                    l_strName = "File:New(" & l_lngPluginIndex & ")"
                    If l_icnIcon Is Nothing Then
                        Menu.InsertItem l_fpgPlugin.FileTypeName, "NewEndSeparator", , , , , GameIsLoaded, l_strName
                    Else
                        frmIcons.ilIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, l_strName
                        Menu.InsertItem l_fpgPlugin.FileTypeName, "NewEndSeparator", , , frmIcons.ilIcons.ItemIndex(l_strName) - 1, , GameIsLoaded, l_strName
                    End If
                End If
            End If
            l_lngPluginIndex = l_lngPluginIndex + 1
        Next l_fpgPlugin
    Case "open recent"
        Select Case LCase(Trim(Menu.ItemKey(Index)))
        Case "recentfiles"
            l_lngIndex = Index
            Do While l_lngIndex <= Menu.Count
                If InStr(Menu.ItemKey(l_lngIndex), "File:Open(") Then
                    frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex(Menu.ItemKey(l_lngIndex))
                    Menu.RemoveItem l_lngIndex
                Else
                    l_lngIndex = l_lngIndex + 1
                End If
            Loop
            l_lngFileCount = ReadRegSetting("Recent\Files\Count", 0)
            If l_lngFileCount Then
                For l_lngFiles = 1 To l_lngFileCount
                    l_strFilename = ReadRegSetting("Recent\Files\File " & l_lngFiles, "")
                    If Trim(l_strFilename) <> "" Then
                        Set l_icnIcon = Nothing
                        Set l_plgPlugin = Nothing
                        Set l_plgPlugin = g_edEditor.FindCapablePlugin(l_strFilename)
                        Set l_icnIcon = l_plgPlugin.Icon
                        l_strName = "File:Open(""" & l_strFilename & """)"
                        If l_icnIcon Is Nothing Then
                            Menu.InsertItem GetTitle(l_strFilename), "RecentFilesEndSeparator", , , , , GameIsLoaded, l_strName
                        Else
                            frmIcons.ilIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, l_strName
                            Menu.InsertItem GetTitle(l_strFilename), "RecentFilesEndSeparator", , , frmIcons.ilIcons.ItemIndex(l_strName) - 1, , GameIsLoaded, l_strName
                        End If
                    End If
                Next l_lngFiles
            End If
        Case "recentgames"
            l_lngIndex = Index
            Do While l_lngIndex <= Menu.Count
                If InStr(Menu.ItemKey(l_lngIndex), "Game:Open(") Then
                    Menu.RemoveItem l_lngIndex
                Else
                    l_lngIndex = l_lngIndex + 1
                End If
            Loop
            l_lngFileCount = ReadRegSetting("Recent\Games\Count", 0)
            If l_lngFileCount Then
                For l_lngFiles = 1 To l_lngFileCount
                    l_strFilename = ReadRegSetting("Recent\Games\Game " & l_lngFiles, "")
                    If Trim(l_strFilename) <> "" Then
                        Menu.InsertItem l_strFilename, "RecentGamesEndSeparator", , , , , , "Game:Open(""" & l_strFilename & """)"
                    End If
                Next l_lngFiles
            End If
        Case Else
        End Select
    Case "file"
        g_edEditor.ActionUpdate
        With Menu
            .Enabled(.IndexForKey("File:Open")) = GameIsLoaded
        End With
    Case "edit"
        g_edEditor.ActionUpdate
        With Menu
            .Enabled(.IndexForKey("Action:Cut")) = g_edEditor.CanCut
            .Enabled(.IndexForKey("Action:Copy")) = g_edEditor.CanCopy
            .Enabled(.IndexForKey("Action:Paste")) = g_edEditor.CanPaste
            .Enabled(.IndexForKey("Action:Delete")) = g_edEditor.CanDelete
            .Enabled(.IndexForKey("Action:Undo")) = g_edEditor.CanUndo
            .Enabled(.IndexForKey("Action:Redo")) = g_edEditor.CanRedo
            .Enabled(.IndexForKey("Action:SelectAll")) = g_edEditor.CanSelectAll
        End With
    Case "document"
        If m_cmnLastDocument Is Nothing Then
        Else
            m_cmnLastDocument.DestroyMenus Me
        End If
        Set m_cmnLastDocument = Nothing
        With Menu
            .Enabled(.IndexForKey("Action:CloseWindow")) = Not (Me.ActiveChild Is Nothing)
        End With
        Set l_cmnDocument = Me.ActiveChild.Form
        If l_cmnDocument Is Nothing Then
        Else
            l_cmnDocument.InitializeMenus Me
        End If
        Set m_cmnLastDocument = l_cmnDocument
    Case "tools"
        l_lngIndex = Index
        Do While l_lngIndex <= Menu.Count
            If InStr(Menu.ItemKey(l_lngIndex), "Plugins:Activate(") Then
                frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex(Menu.ItemKey(l_lngIndex))
                Menu.RemoveItem l_lngIndex
            Else
                l_lngIndex = l_lngIndex + 1
            End If
        Loop
        l_lngPluginIndex = 1
        For Each l_plgPlugin In g_colPlugins
            If l_plgPlugin.ShowInPluginMenu Then
                If CLng(ReadRegSetting("Plugins\Show In Menu\" & TypeName(l_plgPlugin), 1)) Then
                    l_strName = "Plugins:Activate(" & l_lngPluginIndex & ")"
                    Set l_icnIcon = l_plgPlugin.Icon
                    If l_icnIcon Is Nothing Then
                        Menu.InsertItem l_plgPlugin.PluginName, "PluginsEndSeparator", , , , , , l_strName
                    Else
                        frmIcons.ilIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, l_strName
                        Menu.InsertItem l_plgPlugin.PluginName, "PluginsEndSeparator", , , frmIcons.ilIcons.ItemIndex(l_strName) - 1, , , l_strName
                    End If
                End If
            End If
            l_lngPluginIndex = l_lngPluginIndex + 1
        Next l_plgPlugin
    Case "view"
        Menu.Checked(Menu.IndexForKey("Show:FileSidebar")) = picFileSidebar.Tag = ""
        Menu.Checked(Menu.IndexForKey("Show:MainToolbar")) = tbrMain.Tag = ""
        Menu.Checked(Menu.IndexForKey("Show:GameToolbar")) = tbrGame.Tag = ""
        Menu.Checked(Menu.IndexForKey("Show:PluginToolbar")) = tbrPlugins.Tag = ""
    Case "window"
        l_lngIndex = Index
        Do While l_lngIndex <= Menu.Count
            l_strKey = Menu.ItemKey(l_lngIndex)
            If InStr(l_strKey, "Action:ActivateWindow(") Then
                frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex("ICON_" & Menu.ItemData(l_lngIndex))
                Menu.RemoveItem l_lngIndex
            Else
                l_lngIndex = l_lngIndex + 1
            End If
        Loop
        Menu.RemoveItem "Show:WindowList"
        l_lngForm = 1
        For Each l_mgrForm In m_colChildWindows
            Set l_frmForm = l_mgrForm.Form
            l_strName = "Action:ActivateWindow(" & l_lngForm & ")"
            If l_frmForm.Icon Is Nothing Then
            Else
                frmIcons.ilIcons.AddFromHandle l_frmForm.Icon.Handle, Image_Icon, "ICON_" & l_frmForm.Icon.Handle
                Menu.InsertItem "&" & (l_lngWindowsAdded) & ": " & EscapeAmpersands(l_frmForm.Caption), "WindowsEndSeparator", , l_frmForm.Icon.Handle, frmIcons.ilIcons.ImageCount - 1, , , l_strName
            End If
            l_lngWindowsAdded = l_lngWindowsAdded + 1
            l_lngForm = l_lngForm + 1
            If l_lngWindowsAdded = 10 Then
                Menu.InsertItem "&More Windows...", "WindowsEndSeparator", , , frmIcons.ilIcons.ItemIndex("WINDOW LIST") - 1, , , "Show:WindowList"
                Exit For
            End If
        Next l_mgrForm
        If l_lngWindowsAdded = 0 Then
            Menu.InsertItem "No Windows Open", "WindowsEndSeparator", , , , , False, "Action:ActivateWindow(-1)"
        End If
        Menu.Enabled(Menu.IndexForKey("Action:CloseAllWindows")) = l_lngWindowsAdded > 0
    Case Else
    End Select
End Sub

Public Sub Menu_UnInitialize(Menu As cPopupMenu, Index As Long)
On Error Resume Next
Dim l_lngIndex As Long
Dim l_frmForm As Form, l_lngForm As Long, l_lngWindowsAdded As Long, l_strName As String
Dim l_fpgPlugin As iFileTypePlugin, l_lngPluginIndex As Long, l_strKey As String
Dim l_plgPlugin As iPlugin, l_icnIcon As IPictureDisp, l_mgrForm As cChildManager
Dim l_cmnDocument As iCustomMenus
    Select Case LCase(Trim(Replace(Menu.Caption(Index), "&", "")))
    Case "new"
    Case "open recent"
    Case "edit"
    Case "document"
    Case "tools"
    Case "view"
    Case "window"
    Case Else
    End Select
End Sub

Private Sub tmrFocusTracker_Timer()
On Error Resume Next
Static s_lngLastDocument As Long
Static s_lngLastControl As Long
Dim l_lngDocument As Long, l_lngControl As Long
    If Me.ActiveChild Is Nothing Then
        l_lngDocument = 0
        l_lngControl = 0
        g_edEditor.ActionUpdate
    Else
        With Me.ActiveChild
            l_lngDocument = ObjPtr(.Form)
            If .Form Is Nothing Then
            Else
                l_lngControl = ObjPtr(.Form.ActiveControl)
            End If
            If (l_lngDocument <> s_lngLastDocument) Or (l_lngControl <> s_lngLastControl) Then
                g_edEditor.Event_FocusChanged
            Else
                g_edEditor.ActionUpdate
            End If
            s_lngLastDocument = l_lngDocument
            s_lngLastControl = l_lngControl
        End With
    End If
End Sub

Private Sub tmrRefreshFileSidebar_Timer()
On Error Resume Next
    tmrRefreshFileSidebar.Enabled = False
    If tsFileTabs.SelectedTab.key = "Game" Then
        Err.Clear
        If Engine.Engine Is Nothing Then
            tvFileTree.Nodes.Clear
            tvFileTree.Enabled = False
        ElseIf Engine.Engine.Filesystem Is Nothing Then
            tvFileTree.Nodes.Clear
            tvFileTree.Enabled = False
        Else
            EnumFilesystem tvFileTree, Engine.Engine.Filesystem
            tvFileTree.Enabled = True
        End If
    Else
        EnumFilesystem tvFileTree, g_fsFilesystem, , , , True
        tvFileTree.Enabled = True
    End If
End Sub

Private Sub tsFileTabs_TabClick(theTab As vbalDTab6.cTab, ByVal iButton As MouseButtonConstants, ByVal Shift As ShiftConstants, ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    RefreshFileSidebar
End Sub

Private Sub tvFileTree_DblClick()
On Error Resume Next
Dim l_strFilename As String
Dim l_filFile As Fury2File
    If Right(Trim(tvFileTree.SelectedItem.key), 1) = "/" Then Exit Sub
    If tsFileTabs.SelectedTab.key = "Game" Then
        Set l_filFile = Engine.Engine.Filesystem.File(tvFileTree.SelectedItem.key)
        l_strFilename = l_filFile.GetRealFilename
        g_edEditor.File_Open l_strFilename
    Else
        If tvFileTree.SelectedItem.key = ".." Then
            g_fsFilesystem.Root = GetPath(StripEndCharacters(g_fsFilesystem.Root, 1))
            RefreshFileSidebar
        Else
            l_strFilename = g_fsFilesystem.TranslateFilename(tvFileTree.SelectedItem.key)
            g_edEditor.File_Open l_strFilename
        End If
    End If
End Sub

Private Sub tvFileTree_DragDropRequest(Data As DataObject, nodeOver As vbalTreeViewLib6.cTreeViewNode, ByVal bAbove As Boolean, ByVal hitTest As Long)
On Error Resume Next
Dim l_lngFiles As Long
    If Data.Files.Count > 0 Then
        For l_lngFiles = 1 To Data.Files.Count
            If Right(Trim(nodeOver.key), 1) = "/" Then
                FileCopy Data.Files(l_lngFiles), g_edEditor.GamePath & "\" & Replace(nodeOver.key, "/", "\") & GetTitle(Data.Files(l_lngFiles))
            Else
                FileCopy Data.Files(l_lngFiles), g_edEditor.GamePath & "\" & GetTitle(Data.Files(l_lngFiles))
            End If
        Next l_lngFiles
        RefreshFileSidebar
    End If
End Sub

Private Sub tvFileTree_OLEDragOver(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single, State As Integer)
On Error Resume Next
    If Data.Files.Count > 0 Then
        Effect = vbDropEffectCopy
    End If
End Sub
