VERSION 5.00
Object = "{CA5A8E1E-C861-4345-8FF8-EF0A27CD4236}#2.0#0"; "vbalTreeView6.ocx"
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{EF59A10B-9BC4-11D3-8E24-44910FC10000}#11.0#0"; "vbalEdit.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#13.2#0"; "ngUI.ocx"
Begin VB.MDIForm frmMain 
   AutoShowChildren=   0   'False
   BackColor       =   &H8000000C&
   Caption         =   "Editor²"
   ClientHeight    =   6600
   ClientLeft      =   165
   ClientTop       =   495
   ClientWidth     =   8325
   Icon            =   "frmMain.frx":0000
   LinkMode        =   1  'Source
   LinkTopic       =   "Main"
   OLEDropMode     =   1  'Manual
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox picStatus 
      Align           =   2  'Align Bottom
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   330
      Left            =   0
      ScaleHeight     =   22
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   555
      TabIndex        =   7
      TabStop         =   0   'False
      Top             =   6270
      Width           =   8325
   End
   Begin VB.PictureBox picLog 
      Align           =   2  'Align Bottom
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
      Height          =   1500
      Left            =   0
      ScaleHeight     =   100
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   555
      TabIndex        =   2
      TabStop         =   0   'False
      Top             =   4770
      Visible         =   0   'False
      Width           =   8325
      Begin vbalEdit.vbalRichEdit reLog 
         Height          =   945
         Left            =   0
         TabIndex        =   3
         TabStop         =   0   'False
         Top             =   255
         Width           =   8325
         _ExtentX        =   14684
         _ExtentY        =   1667
         Version         =   1
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Courier New"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         BackColor       =   -2147483633
         ForeColor       =   -2147483630
         ViewMode        =   1
         Border          =   0   'False
         TextLimit       =   16777216
         AutoURLDetect   =   0   'False
         ScrollBars      =   3
      End
   End
   Begin ngUI.ngToolbar tbrLeft 
      Align           =   3  'Align Left
      Height          =   2610
      Left            =   0
      Top             =   2160
      Width           =   720
      _ExtentX        =   1270
      _ExtentY        =   4604
   End
   Begin VB.PictureBox picFileSidebar 
      Align           =   4  'Align Right
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
      Height          =   2610
      Left            =   6330
      ScaleHeight     =   174
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   133
      TabIndex        =   6
      TabStop         =   0   'False
      Top             =   2160
      Visible         =   0   'False
      Width           =   2000
      Begin vbalTreeViewLib6.vbalTreeView tvFileTree 
         Height          =   2655
         Left            =   15
         TabIndex        =   4
         TabStop         =   0   'False
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
         LabelEdit       =   -1  'True
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
   End
   Begin VB.PictureBox picToolbarsTop 
      Align           =   1  'Align Top
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
      Height          =   915
      Left            =   0
      ScaleHeight     =   61
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   555
      TabIndex        =   1
      TabStop         =   0   'False
      Top             =   1245
      Width           =   8325
      Begin ngUI.ngToolbar tbrMain 
         Height          =   360
         Left            =   1890
         Top             =   0
         Width           =   1860
         _ExtentX        =   3281
         _ExtentY        =   635
      End
      Begin ngUI.ngToolbar tbrGame 
         Height          =   360
         Left            =   5670
         Top             =   0
         Width           =   1860
         _ExtentX        =   3281
         _ExtentY        =   635
      End
      Begin ngUI.ngToolbar tbrPlugins 
         Height          =   360
         Left            =   3780
         Top             =   0
         Width           =   1860
         _ExtentX        =   3281
         _ExtentY        =   635
      End
      Begin ngUI.ngToolbar tbrMenus 
         Height          =   360
         Left            =   0
         Top             =   0
         Width           =   1860
         _ExtentX        =   3281
         _ExtentY        =   635
      End
      Begin ngUI.ngTabStrip tsDocuments 
         Height          =   525
         Left            =   0
         Top             =   570
         Width           =   8325
         _ExtentX        =   14684
         _ExtentY        =   926
      End
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
      Height          =   1245
      Left            =   0
      ScaleHeight     =   83
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   555
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   0
      Visible         =   0   'False
      Width           =   8325
      Begin VB.Timer tmrFixFocus 
         Enabled         =   0   'False
         Interval        =   1
         Left            =   465
         Top             =   930
      End
      Begin VB.Timer tmrOpenMenu 
         Enabled         =   0   'False
         Interval        =   1
         Left            =   0
         Top             =   930
      End
      Begin VB.Timer tmrTabs 
         Interval        =   5000
         Left            =   930
         Top             =   465
      End
      Begin VB.Timer tmrNotice 
         Enabled         =   0   'False
         Interval        =   100
         Left            =   930
         Top             =   0
      End
      Begin VB.PictureBox picNotice 
         AutoRedraw      =   -1  'True
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1155
         Left            =   1485
         ScaleHeight     =   77
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   380
         TabIndex        =   5
         TabStop         =   0   'False
         Top             =   30
         Width           =   5700
         Begin ngUI.ngToolbar tbrNotice 
            Height          =   240
            Left            =   0
            Top             =   915
            Width           =   5700
            _ExtentX        =   10054
            _ExtentY        =   423
         End
      End
      Begin VB.Timer tmrClock 
         Enabled         =   0   'False
         Interval        =   1000
         Left            =   0
         Top             =   0
      End
      Begin VB.Timer tmrRefreshFileSidebar 
         Enabled         =   0   'False
         Interval        =   1
         Left            =   465
         Top             =   0
      End
      Begin VB.Timer tmrFocusTracker 
         Enabled         =   0   'False
         Interval        =   333
         Left            =   0
         Top             =   465
      End
      Begin VB.Timer tmrPlayGame 
         Enabled         =   0   'False
         Interval        =   1
         Left            =   465
         Top             =   465
      End
   End
   Begin sMDIinActiveX.MDIActiveX maxContainer 
      Left            =   2970
      Top             =   405
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
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
Implements iCustomMenuHandler

Private Const WM_MDIGETACTIVE = &H229
Private Declare Function W32SetFocus Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As Win32.RECT) As Long
Private Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As Win32.RECT) As Long

Private m_lngFocus As Long
Private m_strStatusText As String
Private m_strProgressText As String
Private m_sngProgress As Single
Private m_lngMenusVisible As Long
Private m_btnActiveMenu As ngToolButton
Private m_colNoticeQueue As Engine.Fury2Collection
Private m_notNotice As cNotice
Private m_imgNotice As Fury2Image
Private m_booNoticeFocused As Boolean
Private m_aclMenus As cAcceleratorManager
Private m_colMenus As Engine.Fury2Collection
Private WithEvents m_mnuFile As ngMenu
Attribute m_mnuFile.VB_VarHelpID = -1
Private WithEvents m_mnuEdit As ngMenu
Attribute m_mnuEdit.VB_VarHelpID = -1
Private WithEvents m_mnuView As ngMenu
Attribute m_mnuView.VB_VarHelpID = -1
Private WithEvents m_mnuDocument As ngMenu
Attribute m_mnuDocument.VB_VarHelpID = -1
Private WithEvents m_mnuGame As ngMenu
Attribute m_mnuGame.VB_VarHelpID = -1
Private WithEvents m_mnuTools As ngMenu
Attribute m_mnuTools.VB_VarHelpID = -1
Private WithEvents m_mnuMacros As ngMenu
Attribute m_mnuMacros.VB_VarHelpID = -1
Private WithEvents m_mnuWindow As ngMenu
Attribute m_mnuWindow.VB_VarHelpID = -1
Private WithEvents m_mnuHelp As ngMenu
Attribute m_mnuHelp.VB_VarHelpID = -1
Private WithEvents m_mnuRecentFiles As ngMenu
Attribute m_mnuRecentFiles.VB_VarHelpID = -1
Private WithEvents m_mnuRecentGames As ngMenu
Attribute m_mnuRecentGames.VB_VarHelpID = -1
Private WithEvents m_mnuNew As ngMenu
Attribute m_mnuNew.VB_VarHelpID = -1
Private WithEvents m_mnuToolbars As ngMenu
Attribute m_mnuToolbars.VB_VarHelpID = -1
Private WithEvents m_mnuPanels As ngMenu
Attribute m_mnuPanels.VB_VarHelpID = -1
Private m_booNonClientFocus As Boolean
Private m_colChildWindows As Engine.Fury2Collection
Private m_cmnLastDocument As iCustomMenus
Private m_btnMenuToOpen As ngToolButton

Sub FixFocus(ByVal hwnd As Long)
On Error Resume Next
    m_lngFocus = hwnd
    tmrFixFocus.Enabled = True
End Sub

Public Sub InitTabs()
On Error Resume Next
    Set tsDocuments.ResourceFile = g_edEditor.Resources
    tsDocuments.LoadTheme "theme\tabstrip\"
End Sub

Public Sub RefreshDocumentTabs()
On Error Resume Next
Dim l_docDocument As cChildManager
    tsDocuments.DisableUpdates = True
    tsDocuments.ShowCloseButtons = True
    tsDocuments.Tabs.Clear
    For Each l_docDocument In m_colChildWindows
        With tsDocuments.Tabs.AddNew(l_docDocument.Form.Caption)
            If l_docDocument Is Me.ActiveChild Then
                tsDocuments.SelectTab tsDocuments.Tabs(.Index)
            End If
            Set .Image = l_docDocument.Document.DocumentIcon
            Set .Tag = l_docDocument
        End With
    Next l_docDocument
    tsDocuments.DisableUpdates = False
    tsDocuments.Reflow
    If tsDocuments.IdealHeight <> tsDocuments.Height Then
        RefreshToolbars
    End If
End Sub

Public Sub LogAppend(ByRef Text As String, Optional ByVal Color As Long = vbButtonText)
On Error Resume Next
Dim l_lngOldLength As Long, l_lngLength As Long
    reLog.Redraw = False
    l_lngLength = Len(reLog.Contents(SF_TEXT))
    reLog.SetSelection l_lngOldLength - 1, l_lngOldLength - 1
    reLog.SetFont reLog.Font, Color, ercTextNormal, False, ercSetFormatSelection
    reLog.InsertContents SF_TEXT, Text & vbCrLf
    l_lngLength = Len(reLog.Contents(SF_TEXT))
    reLog.Redraw = True
    reLog.SetSelection l_lngLength, l_lngLength
End Sub

Public Sub RefreshGameState()
On Error Resume Next
'    tbrMain.Buttons("File:Open").Enabled = GameIsLoaded
'    tbrMain.Buttons("File:OpenMenu").Enabled = GameIsLoaded
    tbrGame.Buttons("Game:Run").Enabled = GameIsLoaded
    tbrGame.Buttons("Game:Debug").Enabled = GameIsLoaded
    tbrGame.Buttons("Game:Debug").Checked = GameIsRunning
    tbrGame.Buttons("Game:Pause").Enabled = GameIsRunning And GameIsLoaded
    tbrGame.Buttons("Game:Pause").Checked = GameIsPaused
End Sub

Private Function GetToolbarX(Toolbar As Object, Optional Docked As Boolean = True)
On Error Resume Next
Dim l_ptWindow As POINTAPI, l_ptToolbar As POINTAPI
    ClientToScreen Me.hwnd, l_ptWindow
    ClientToScreen Toolbar.hwnd, l_ptToolbar
    GetToolbarX = (l_ptToolbar.X - (IIf(Docked, l_ptWindow.X, 0)))
End Function

Private Function GetToolbarY(Toolbar As Object, Optional Docked As Boolean = True)
On Error Resume Next
Dim l_ptWindow As POINTAPI, l_ptToolbar As POINTAPI
    ClientToScreen Me.hwnd, l_ptWindow
    ClientToScreen Toolbar.hwnd, l_ptToolbar
    GetToolbarY = (l_ptToolbar.Y - (IIf(Docked, l_ptWindow.Y, 0)))
End Function

Public Sub DocumentClosed(Document As cChildManager)
On Error Resume Next
Dim l_mgrForm As cChildManager
Dim l_booHidden As Boolean
Dim l_frmForm As Form
    m_colChildWindows.Remove m_colChildWindows.Find(Document)
    Set l_frmForm = Document.Form
    Document.Detach
    Unload l_frmForm
    m_colChildWindows(m_colChildWindows.Count).Activate
    HideInactiveWindows
    RefreshDocumentTabs
End Sub

Public Sub HideInactiveWindows()
On Error Resume Next
Dim l_lnghWnd As Long
Dim l_mgrForm As cChildManager
    l_lnghWnd = maxContainer.ActiveWindow
    For Each l_mgrForm In m_colChildWindows
        If (l_mgrForm.Form.hwnd = l_lnghWnd) Then
            l_mgrForm.Visible = True
        ElseIf (l_mgrForm.Form.extender.hwnd = l_lnghWnd) Then
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
        ElseIf (l_mgrForm.Form.extender.hwnd = l_lnghWnd) Then
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

Public Function CloseAllChildren(Optional ByVal Prompt As Boolean = True) As Boolean
On Error Resume Next
Dim l_lngForm As Long, l_mgrChild As cChildManager
Dim l_sngScale As Single
Dim l_docDocument As cChildManager
Dim l_lngCount As Long
    For Each l_docDocument In frmMain.Documents
        If l_docDocument.Document.CanSave Then l_lngCount = l_lngCount + 1
    Next l_docDocument
    If Prompt Then
        If l_lngCount > 0 Then
            Load frmSaveOpenDocuments
            frmSaveOpenDocuments.Cancelled = False
            frmSaveOpenDocuments.Show vbModal, frmMain
            If frmSaveOpenDocuments.Cancelled Then
                Unload frmSaveOpenDocuments
                Exit Function
            Else
                Unload frmSaveOpenDocuments
            End If
        End If
    End If
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
        SetProgress (l_lngForm * l_sngScale)
        DoEvents
    Next l_mgrChild
    SetProgress 100
    RefreshWindows
    SetStatus
    SetProgress
    SetBusyState False
    Err.Clear
    CloseAllChildren = True
End Function

Public Sub RefreshFileSidebar()
On Error Resume Next
    If picFileSidebar.Visible = False Then Exit Sub
    tvFileTree.DragStyle = etvwDropHighlight
    tmrRefreshFileSidebar.Enabled = False
    tmrRefreshFileSidebar.Interval = 0
    tmrRefreshFileSidebar.Interval = 1
    tmrRefreshFileSidebar.Enabled = True
End Sub

Public Sub SetStatus(Optional ByRef Status As String = "Ready")
On Error Resume Next
    m_strStatusText = Status
    picStatus_Paint
    picStatus.Refresh
End Sub

Public Sub SetLocation(Optional ByRef Value As String = "")
On Error Resume Next
    m_strProgressText = Value
    picStatus_Paint
    picStatus.Refresh
End Sub

Public Sub SetProgress(Optional ByVal Progress As Single = 0)
On Error Resume Next
    m_sngProgress = Progress
    picStatus_Paint
    picStatus.Refresh
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
    Child.extender.WindowState = 2
    Child.Show
    g_edEditor.Event_DocumentActivate l_mgrManager
    Err.Clear
    RefreshDocumentTabs
End Sub

Friend Sub DereferenceChildManager(ByRef Manager As cChildManager)
On Error Resume Next
    m_colChildWindows.Remove m_colChildWindows.Find(Manager)
End Sub

Public Sub MenuActivate(ByRef Accelerator As cAccelerator)
On Error Resume Next
Dim l_lngButtons As Long, l_strCaption As String, l_strAccel As String
    For l_lngButtons = 1 To tbrMenus.Buttons.Count
        l_strCaption = tbrMenus.Buttons(l_lngButtons).Text
        If InStr(l_strCaption, "&") Then
            l_strAccel = UCase(Mid(l_strCaption, InStr(l_strCaption, "&") + 1, 1))
            If l_strAccel = Chr(Accelerator.KeyCode) Then
                tbrMenus_ButtonClick tbrMenus.Buttons(l_lngButtons)
                Exit For
            End If
        End If
    Next l_lngButtons
End Sub

Public Sub RefreshMenus()
On Error Resume Next
Dim l_lngButtons As Long
Dim l_strAccel As String, l_strCaption As String
Dim l_btnButton As ngToolButton
    If m_aclMenus Is Nothing Then
    Else
        m_aclMenus.Detach
        Set m_aclMenus = Nothing
    End If
    tbrMenus.DisableUpdates = True
    With tbrMenus.Buttons
        .Clear
        Set m_mnuFile.Tag = .AddNew(" &File ", "File")
        Set m_mnuEdit.Tag = .AddNew(" &Edit ", "Edit")
        Set m_mnuView.Tag = .AddNew(" &View ", "View")
        Set m_mnuDocument.Tag = .AddNew(" &Document ", "Document")
        Set m_mnuGame.Tag = .AddNew(" &Game ", "Game")
        Set m_mnuTools.Tag = .AddNew(" &Tools ", "Tools")
        Set m_mnuMacros.Tag = .AddNew(" &Macros ", "Macros")
        Set m_mnuWindow.Tag = .AddNew(" &Window ", "Window")
        Set m_mnuHelp.Tag = .AddNew(" &Help ", "Help")
    End With
    tbrMenus.DisableUpdates = False
    tbrMenus.Reflow
    Set m_aclMenus = New cAcceleratorManager
    m_aclMenus.Attach Me.hwnd
    For l_lngButtons = 1 To tbrMenus.Buttons.Count
        l_strCaption = tbrMenus.Buttons(l_lngButtons).Text
        If InStr(l_strCaption, "&") Then
            l_strAccel = Mid(l_strCaption, InStr(l_strCaption, "&") + 1, 1)
            m_aclMenus.AddAccelerator Asc(UCase(l_strAccel)), BindEvent(Me, "MenuActivate"), , , True
        End If
    Next l_lngButtons
End Sub

Public Sub RefreshActiveDocument()
On Error Resume Next
Dim l_mgrDocument As cChildManager
    Set l_mgrDocument = ActiveChild
    If l_mgrDocument Is Nothing Then
        tbrMain.Buttons("File:Save").Enabled = False
        g_aclSave.Enabled = False
        With m_mnuFile
            .Items("File:Save").Enabled = False
            .Items("File:SaveAs").Enabled = False
        End With
    Else
        tbrMain.Buttons("File:Save").Enabled = l_mgrDocument.Document.CanSave
        g_aclSave.Enabled = l_mgrDocument.Document.CanSave
        With m_mnuFile
            .Items("File:Save").Enabled = l_mgrDocument.Document.CanSave
            .Items("File:SaveAs").Enabled = l_mgrDocument.Document.CanSave
        End With
    End If
End Sub

Public Sub RefreshPluginToolbar()
On Error Resume Next
Dim l_plgPlugin As iPlugin
Dim l_imgIcon As Fury2Image
Dim l_imgDefault As Fury2Image
Dim l_lngPluginIndex As Long
    Set l_imgDefault = F2ImageFromPicture(frmIcons.ilIcons.ItemPicture(frmIcons.ilIcons.ItemIndex("PLUGIN")))
    tbrPlugins.DisableUpdates = True
    tbrPlugins.Buttons.Clear
    tbrPlugins.Buttons.AddNew , "Plugins:Manage", l_imgDefault, "Manage Plugins"
    tbrPlugins.Buttons.AddNew , , , , bsySeparator
    l_lngPluginIndex = 1
    If g_colPlugins.Count > 0 Then
        For Each l_plgPlugin In g_colPlugins
            If l_plgPlugin Is Nothing Then
            Else
                With l_plgPlugin
                    If .ShowInPluginMenu Then
                        If CLng(ReadRegSetting("Plugins\Show In Toolbar\" & TypeName(l_plgPlugin), 1)) Then
                            Set l_imgIcon = Nothing
                            Set l_imgIcon = .ToolbarIcon
                            If l_imgIcon Is Nothing Then
                                Set l_imgIcon = Nothing
                                Set l_imgIcon = .Icon
                                If l_imgIcon Is Nothing Then
                                    Set l_imgIcon = l_imgDefault
                                End If
                            End If
                            tbrPlugins.Buttons.AddNew , "Plugins:Activate(" & l_lngPluginIndex & ")", l_imgIcon, .PluginName, , , GameIsLoaded
                        End If
                    End If
                End With
            End If
            l_lngPluginIndex = l_lngPluginIndex + 1
        Next l_plgPlugin
    End If
    tbrPlugins.DisableUpdates = False
    tbrPlugins.Reflow
    RefreshToolbars
End Sub

Public Sub RefreshWindows()
On Error Resume Next
End Sub

Public Sub ResizeSidebars()
On Error Resume Next
    picFileSidebar_Resize
End Sub

Public Sub InitToolbars()
On Error Resume Next
Dim l_fntMarlett As StdFont
    Set tbrMenus.ResourceFile = g_edEditor.Resources
    tbrMenus.ResourcePattern = "toolbar\*.png"
    Set tbrMain.ResourceFile = g_edEditor.Resources
    tbrMain.ResourcePattern = "toolbar\*.png"
    Set tbrGame.ResourceFile = g_edEditor.Resources
    tbrGame.ResourcePattern = "toolbar\*.png"
    Set tbrPlugins.ResourceFile = g_edEditor.Resources
    tbrPlugins.ResourcePattern = "toolbar\*.png"
    Set tbrLeft.ResourceFile = g_edEditor.Resources
    tbrLeft.ResourcePattern = "toolbar\*.png"
    tbrLeft.Orientation = tboVertical
    Set l_fntMarlett = New StdFont
    l_fntMarlett.Name = "Marlett"
    l_fntMarlett.Size = 8
    With tbrMain.Buttons
        Set .AddNew("7", "File:New", "new", "New").Font = l_fntMarlett
        .AddNew , "File:Open", "open", "Open"
        Set .AddNew("6", "File:OpenMenu", , "Open Recent").Font = l_fntMarlett
        .AddNew , "File:Save", "save", "Save"
        .AddNew , "File:SaveAll", "save all", "Save All"
        .AddNew "-"
        .AddNew , "Action:Undo", "undo", "Undo"
        .AddNew , "Action:Redo", "redo", "Redo"
        .AddNew "-"
        .AddNew , "Action:Cut", "cut", "Cut"
        .AddNew , "Action:Copy", "copy", "Copy"
        .AddNew , "Action:Paste", "paste", "Paste"
        .AddNew , "Action:Delete", "delete", "Delete"
        .AddNew "-"
        .AddNew , "Action:SelectAll", "select all", "Select All"
        .AddNew , "Action:SelectNone", "select none", "Select None"
    End With
    With tbrGame.Buttons
        .AddNew , "Game:Open", "open game", "Open Game"
        Set .AddNew("6", "Game:OpenMenu", , "Open Recent Game").Font = l_fntMarlett
        .AddNew , "Game:Reload", "reload game", "Reload Game"
        .AddNew "-"
        .AddNew , "Game:Run", "run", "Run Game"
        .AddNew , "Game:Debug", "debug", "Debug Game"
        .AddNew , "Game:Pause", "pause", "Pause Game"
    End With
    RefreshPluginToolbar
    RefreshGameState
End Sub

Public Sub InitStatus()
On Error Resume Next
End Sub

Public Sub InitSidebars()
On Error Resume Next
    picFileSidebar.Visible = ReadRegSetting("Sidebars\Filesystem", False)
    RefreshFileSidebar
    picLog.Visible = ReadRegSetting("Sidebars\Log", False)
End Sub

Public Sub SaveSettings()
On Error Resume Next
    WriteRegSetting "Sidebars\Filesystem", picFileSidebar.Visible
    WriteRegSetting "Sidebars\Log", picLog.Visible
End Sub

Public Sub InitMenus()
On Error Resume Next
Dim l_mnuMenu As ngMenu
    Set m_colMenus = New Engine.Fury2Collection
    Set m_mnuFile = CreateMenu()
    Set m_mnuEdit = CreateMenu()
    Set m_mnuView = CreateMenu()
    Set m_mnuDocument = CreateMenu()
    Set m_mnuGame = CreateMenu()
    Set m_mnuTools = CreateMenu()
    Set m_mnuMacros = CreateMenu()
    Set m_mnuWindow = CreateMenu()
    Set m_mnuHelp = CreateMenu()
    Set m_mnuNew = CreateMenu()
    Set m_mnuRecentFiles = CreateMenu()
    Set m_mnuRecentGames = CreateMenu()
    Set m_mnuPanels = CreateMenu()
    Set m_mnuToolbars = CreateMenu()
    m_colMenus.Add m_mnuFile, "File"
    m_colMenus.Add m_mnuEdit, "Edit"
    m_colMenus.Add m_mnuView, "View"
    m_colMenus.Add m_mnuDocument, "Document"
    m_colMenus.Add m_mnuGame, "Game"
    m_colMenus.Add m_mnuTools, "Tools"
    m_colMenus.Add m_mnuMacros, "Macros"
    m_colMenus.Add m_mnuWindow, "Window"
    m_colMenus.Add m_mnuHelp, "Help"
    m_colMenus.Add m_mnuNew, "New"
    m_colMenus.Add m_mnuRecentFiles, "RecentFiles"
    m_colMenus.Add m_mnuRecentGames, "RecentGames"
    m_colMenus.Add m_mnuPanels, "Panels"
    m_colMenus.Add m_mnuToolbars, "Toolbars"
    
    With m_mnuFile.Items
        Set .AddNew("&New", , , "new").ChildMenu = m_mnuNew
        With m_mnuNew.Items
            .AddNew "&Game", , "Game:New", "new game"
            .AddNew "-"
        End With
        .AddNew "&Open...", "Ctrl+O", "File:Open", "open"
        Set .AddNew("Open &Recent").ChildMenu = m_mnuRecentFiles
        .AddNew "&Save", "Ctrl+S", "File:Save", "save"
        .AddNew "Save As...", "", "File:SaveAs", "save as"
        .AddNew "Save All", "Ctrl+Shift+S", "File:SaveAll", "save all"
        .AddNew "-"
        .AddNew "E&xit", "Alt+F4", "Editor:Exit", "exit"
    End With
    With m_mnuEdit.Items
        .AddNew "&Undo", "Ctrl+Z", "Action:Undo", "undo"
        .AddNew "&Redo", "Shift+Ctrl+Z", "Action:Redo", "redo"
        .AddNew "-"
        .AddNew "Cu&t", "Ctrl+X", "Action:Cut", "cut"
        .AddNew "&Copy", "Ctrl+C", "Action:Copy", "copy"
        .AddNew "&Paste", "Ctrl+V", "Action:Paste", "paste"
        .AddNew "&Delete", "Del", "Action:Delete", "delete"
        .AddNew "-"
        .AddNew "Select &All", "Ctrl+A", "Action:SelectAll", "select all"
        .AddNew "Select &None", "Ctrl+D", "Action:SelectNone", "select none"
    End With
    With m_mnuView.Items
        Set .AddNew("&Panels").ChildMenu = m_mnuPanels
        Set .AddNew("&Toolbars").ChildMenu = m_mnuToolbars
        With m_mnuPanels.Items
            .AddNew "&Filesystem", "F8", "Show:FileSidebar"
            .AddNew "&Log", , "Show:Log"
        End With
        With m_mnuToolbars.Items
            .AddNew "&Main", , "Show:MainToolbar"
            .AddNew "&Game", , "Show:GameToolbar"
            .AddNew "&Plugins", , "Show:PluginToolbar"
        End With
        .AddNew "-"
        .AddNew "&Refresh", "F5", "View:Refresh"
    End With
    With m_mnuDocument.Items
        .AddNew "-", , "Separator"
        .AddNew "&Close", "Ctrl+F4", "Action:CloseWindow"
        .AddNew "&Previous", "Shift+Ctrl+F6", "Action:PreviousWindow"
        .AddNew "&Next", "Ctrl+F6", "Action:NextWindow"
    End With
    With m_mnuGame.Items
        .AddNew "&Open...", , "Game:Open", "open game"
        Set .AddNew("Open &Recent").ChildMenu = m_mnuRecentGames
        .AddNew "Reload", , "Game:Reload", "reload game"
        .AddNew "-"
        .AddNew "&Run Game", "F9", "Game:Play", "run"
        .AddNew "&Debug Game", , "Game:Debug", "debug"
        .AddNew "&Pause Game", , "Game:Pause", "pause"
    End With
    With m_mnuTools.Items
        .AddNew "-", , "Separator"
        .AddNew "Manage Plugins...", , "Plugins:Manage", "plugin"
        .AddNew "&Options", , "Show:Options", "properties"
    End With
    With m_mnuMacros.Items
        .AddNew "Run Macro...", , "Macro:Run"
        .AddNew "Enter Macro...", , "Macro:RunCustom"
    End With
    With m_mnuWindow.Items
        .AddNew "-", , "Separator"
        .AddNew "Close All", , "Action:CloseAllWindows", "close all windows"
    End With
    With m_mnuHelp.Items
        .AddNew "Online &Documentation", , "Help:OnlineDocs", "help"
        .AddNew "Online &Tutorials", , "Help:OnlineTutorials", "help"
        .AddNew "-"
        .AddNew "View &ChangeLog", , "Help:ChangeLog"
        .AddNew "&About", , "Help:About"
    End With
    
    For Each l_mnuMenu In m_colMenus
        With l_mnuMenu
            Set .SelectEvent = BindEvent(Me, "Menu_Click")
            Set .ShowEvent = BindEvent(Me, "Menu_Show", Array(l_mnuMenu))
            Set .HideEvent = BindEvent(Me, "Menu_Hide", Array(l_mnuMenu))
        End With
    Next l_mnuMenu
    RefreshMenus
End Sub

'Private Sub iCustomMenuHandler_DefineMenu(Caption As String, key As String, Optional ParentKey As String, Optional AcceleratorString As String = "", Optional Icon As stdole.Picture = Nothing, Optional HelpText As String = "", Optional ByVal Checked As Boolean = False, Optional ByVal Enabled As Boolean = True)
'On Error Resume Next
'    With GetMenu("Main Menu")
'        If Icon Is Nothing Then
'            If IsMissing(ParentKey) Or Trim(ParentKey) = "" Then
'                .InsertItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), "DocumentEndSeparator", HelpText, , , Checked, Enabled, "CustomMenu(""" & key & """)"
'            Else
'                .AddItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), HelpText, , .IndexForKey("CustomMenu(""" & ParentKey & """)"), , Checked, Enabled, "CustomMenu(""" & key & """)"
'            End If
'        Else
'            frmIcons.ilIcons.AddFromHandle Icon.Handle, Image_Icon, "Child_" & key
'            If IsMissing(ParentKey) Or Trim(ParentKey) = "" Then
'                .InsertItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), "DocumentEndSeparator", HelpText, , frmIcons.ilIcons.ImageCount - 1, Checked, Enabled, "CustomMenu(""" & key & """)"
'            Else
'                .AddItem Caption & IIf(AcceleratorString <> "", vbTab & AcceleratorString, ""), HelpText, , .IndexForKey("CustomMenu(""" & ParentKey & """)"), frmIcons.ilIcons.ImageCount - 1, Checked, Enabled, "CustomMenu(""" & key & """)"
'            End If
'        End If
'    End With
'    Err.Clear
'End Sub

'Private Sub iCustomMenuHandler_DestroyMenu(key As String)
'On Error Resume Next
'    frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex("Child_" & key)
'    With GetMenu("Main Menu")
'        .RemoveItem "CustomMenu(""" & key & """)"
'    End With
'    Err.Clear
'End Sub

Private Function iCustomMenuHandler_GetMenu() As ngUI.ngMenu
On Error Resume Next
    Set iCustomMenuHandler_GetMenu = m_mnuDocument
End Function

Private Sub MDIForm_Activate()
On Error Resume Next
    g_edEditor.AcceleratorManager.Enabled = True
    tmrClock.Enabled = True
    tmrFocusTracker.Enabled = True
    RefreshMenus
    RefreshToolbars
End Sub

Private Sub MDIForm_Deactivate()
On Error Resume Next
    g_edEditor.AcceleratorManager.Enabled = False
End Sub

Private Sub MDIForm_LinkExecute(CmdStr As String, Cancel As Integer)
On Error Resume Next
Dim l_varFiles As Variant
    l_varFiles = ParseFileList(CmdStr)
    g_edEditor.OpenFiles l_varFiles
    Cancel = 0
End Sub

Private Sub MDIForm_LinkOpen(Cancel As Integer)
On Error Resume Next
End Sub

Private Sub MDIForm_Load()
On Error Resume Next
    Set m_colNoticeQueue = New Fury2Collection
    Set m_imgNotice = F2Image(1, 1)
    g_booMainWindowLoaded = True
    Set m_colChildWindows = New Engine.Fury2Collection
    InitThemes
    InitSidebars
    InitToolbars
    InitTabs
    InitStatus
    InitMenus
    LoadFormPosition Me
    SetAppIcon Me
End Sub

Public Sub InitThemes()
On Error Resume Next
    SetTheme g_edEditor.Resources, "*.png"
    SetToolbarTheme "theme\toolbar\"
    SetTabTheme "theme\tabstrip\"
End Sub

Private Sub MDIForm_OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_varFiles As Variant, l_lngFiles As Long
    If Data.GetFormat(vbCFFiles) Then
        If Data.Files.Count > 0 Then
            Effect = vbDropEffectCopy
            ReDim l_varFiles(0 To Data.Files.Count - 1)
            For l_lngFiles = 1 To Data.Files.Count
                l_varFiles(l_lngFiles - 1) = Data.Files(l_lngFiles)
            Next l_lngFiles
            g_edEditor.OpenFiles l_varFiles
        End If
    End If
End Sub

Private Sub MDIForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
Dim l_lngForms As Long
    tvFileTree.ImageList = 0
    Select Case UnloadMode
    Case 0
        Cancel = True
        If GameIsRunning Then
            g_edEditor.Game_Debug
        Else
            ExitProgram
        End If
    Case Else
        SaveFormPosition Me
        For l_lngForms = Forms.Count To 0 Step -1
            If Forms(l_lngForms).MDIChild Then
                Forms(l_lngForms).Hide
                Unload Forms(l_lngForms)
            End If
        Next l_lngForms
    End Select
End Sub

Public Sub RefreshToolbars()
On Error Resume Next
Dim l_lngWidth As Long, l_lngHeight As Long, l_lngTotalHeight As Long
    tbrMenus.DrawBorder = False
    tbrMain.DrawBorder = True
    tbrGame.DrawBorder = True
    tbrPlugins.DrawBorder = True
    tbrMenus.Move 0, 0, picToolbarsTop.ScaleWidth, tbrMenus.IdealHeight
    l_lngWidth = IIf(tbrMain.Visible, tbrMain.IdealWidth, 0)
    l_lngHeight = IIf(tbrMain.Visible, tbrMain.Height, 0)
    If l_lngHeight > l_lngTotalHeight Then l_lngTotalHeight = l_lngHeight
    tbrMain.Move 0, tbrMenus.Height, l_lngWidth, tbrGame.IdealHeight
    l_lngWidth = IIf(tbrGame.Visible, tbrGame.IdealWidth, 0)
    l_lngHeight = IIf(tbrGame.Visible, tbrGame.Height, 0)
    If l_lngHeight > l_lngTotalHeight Then l_lngTotalHeight = l_lngHeight
    tbrGame.Move tbrMain.Left + IIf(tbrMain.Visible, tbrMain.Width, 0), tbrMain.Top, l_lngWidth, tbrGame.IdealHeight
    l_lngWidth = IIf(tbrPlugins.Visible, tbrPlugins.IdealWidth, 0)
    l_lngHeight = IIf(tbrPlugins.Visible, tbrPlugins.Height, 0)
    If l_lngHeight > l_lngTotalHeight Then l_lngTotalHeight = l_lngHeight
    tbrPlugins.Move tbrGame.Left + IIf(tbrGame.Visible, tbrGame.Width, 0), tbrMain.Top, l_lngWidth, tbrPlugins.IdealHeight
    tsDocuments.Move 0, IIf(tbrMain.Visible, tbrMain.Top + tbrMain.Height + 1, tbrMenus.Top + tbrMenus.Height + 1), picToolbarsTop.ScaleWidth, tsDocuments.IdealHeight
    picToolbarsTop.Height = (tbrMenus.Height + l_lngTotalHeight + tsDocuments.IdealHeight + 1) * Screen.TwipsPerPixelY
End Sub

Private Sub MDIForm_Resize()
On Error Resume Next
    If picNotice.Visible Then
        RefreshNotice
    End If
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
    tvFileTree.Move 2, 16, picFileSidebar.ScaleWidth - 4, picFileSidebar.ScaleHeight - 18
End Sub

Private Sub picHiddenControls_KeyDown(KeyCode As Integer, Shift As Integer)
'    MsgBox "pichiddencontrols what"
End Sub

Private Sub picLog_Paint()
On Error Resume Next
    picLog.Cls
    picLog.Line (0, 0)-(picLog.ScaleWidth - 1, picLog.ScaleHeight - 1), SystemColorConstants.vb3DShadow, B
    picLog.Line (2, 2)-(picLog.ScaleWidth - 3, 14), SystemColorConstants.vbInactiveTitleBar, BF
    picLog.CurrentX = 3
    picLog.CurrentY = 2
    picLog.Print "Log"
End Sub

Private Sub picLog_Resize()
On Error Resume Next
    reLog.Move 2, 16, picLog.ScaleWidth - 4, picLog.ScaleHeight - 16
End Sub

Private Sub picNotice_Click()
On Error Resume Next
    tmrNotice.Enabled = False
    m_notNotice.CloseTime = -1
    m_booNoticeFocused = True
    RefreshNotice False
End Sub

Private Sub picNotice_GotFocus()
On Error Resume Next
    m_booNoticeFocused = True
    RefreshNotice False
End Sub

Private Sub picNotice_LostFocus()
On Error Resume Next
    m_booNoticeFocused = False
    RefreshNotice False
End Sub

Private Sub picNotice_Resize()
On Error Resume Next
End Sub

Private Sub picStatus_Paint()
On Error Resume Next
Dim l_rctStatus As RECT
    picStatus.Line (0, 0)-(picStatus.ScaleWidth - 1, picStatus.ScaleHeight - 1), picStatus.BackColor, BF
    SetBackgroundMode picStatus.hdc, BackgroundMode_Opaque
    SetBackgroundColor picStatus.hdc, GetSystemColor(SystemColor_Button_Face)
    SetTextColor picStatus.hdc, GetSystemColor(SystemColor_Button_Text)
    l_rctStatus.Left = 1
    l_rctStatus.Top = 1
    l_rctStatus.Right = picStatus.ScaleWidth - 202
    l_rctStatus.Bottom = picStatus.ScaleHeight - 2
    DrawText picStatus.hdc, m_strStatusText, Len(m_strStatusText), l_rctStatus, DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_NoPrefix Or DrawText_Wrap_None
    l_rctStatus.Left = l_rctStatus.Right + 1
    l_rctStatus.Right = picStatus.ScaleWidth - 2
    If m_sngProgress < 0.01 Then
        DrawText picStatus.hdc, m_strProgressText, Len(m_strProgressText), l_rctStatus, DrawText_Align_Left Or DrawText_Align_Center_Vertical Or DrawText_NoPrefix Or DrawText_Wrap_None
    Else
        picStatus.Line (l_rctStatus.Left, l_rctStatus.Top)-(l_rctStatus.Left + (200 * m_sngProgress), l_rctStatus.Bottom), GetSystemColor(SystemColor_Highlight), BF
    End If
End Sub

Private Sub picToolbarsTop_Resize()
On Error Resume Next
    tbrMenus.Width = picToolbarsTop.ScaleWidth
    tsDocuments.Width = picToolbarsTop.ScaleWidth
End Sub

Private Sub tbrGame_ButtonClick(Button As ngToolButton)
On Error Resume Next
    ReleaseCapture
    Err.Clear
    If DoCommand(Button.key) Then
    Else
        Debug.Print "Button: " & Button.key & " has no command handler."
    End If
End Sub

Private Sub tbrGame_ButtonPress(Button As ngUI.ngToolButton, Cancel As Boolean)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    l_lngX = GetToolbarX(tbrGame) + ((Button.Left))
    l_lngY = GetToolbarY(tbrGame) + ((Button.Top + Button.Height))
    If Button.key = "Game:OpenMenu" Then
        Cancel = True
        ReleaseCapture
        m_mnuRecentGames.Show l_lngX, l_lngY, Me.hwnd, , False
    End If
End Sub

Private Sub tbrLeft_Reflow()
On Error Resume Next
    If tbrLeft.Width <> tbrLeft.IdealVerticalWidth Then
        tbrLeft.Width = tbrLeft.IdealVerticalWidth
    End If
End Sub

Private Sub tbrMain_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
    ReleaseCapture
    Err.Clear
    If DoCommand(Button.key) Then
    Else
        Debug.Print "Button: " & Button.key & " has no command handler."
    End If
End Sub

Private Sub tbrMain_ButtonPress(Button As ngUI.ngToolButton, Cancel As Boolean)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
    l_lngX = GetToolbarX(tbrMain) + ((Button.Left))
    l_lngY = GetToolbarY(tbrMain) + ((Button.Top + Button.Height))
    If Button.key = "File:New" Then
        Cancel = True
        ReleaseCapture
        m_mnuNew.Show l_lngX, l_lngY, Me.hwnd, , False
    ElseIf Button.key = "File:OpenMenu" Then
        Cancel = True
        ReleaseCapture
        m_mnuRecentFiles.Show l_lngX, l_lngY, Me.hwnd, , False
    End If
End Sub

Private Sub tbrMenus_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long
Dim l_mnuMenu As ngMenu
    l_lngX = (Button.Left)
    l_lngY = (Button.Top + Button.Height)
'    Cancel = True
    ReleaseCapture
    Set l_mnuMenu = m_colMenus(Button.key)
    l_mnuMenu.Show l_lngX, l_lngY, tbrMenus.hwnd, True, False
End Sub

Private Sub tbrMenus_ButtonHover(Button As ngUI.ngToolButton)
On Error Resume Next
Dim l_btnButton As ngToolButton
'    Set l_btnButton = Button
'    If MenuIsOpen Then
'        If l_btnButton Is Nothing Then Exit Sub
'        If l_btnButton Is m_btnActiveMenu Then
'        Else
'            CloseAllMenus
'            Set m_btnMenuToOpen = l_btnButton
'            tmrOpenMenu.Enabled = True
'        End If
'    End If
'    Debug.Print "tbrMenus_ButtonHover"
End Sub

Private Sub tbrMenus_MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
'    Debug.Print "tbrMenus_MouseMove"
End Sub

Private Sub tbrNotice_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
    DismissNotice
End Sub

Private Sub tbrNotice_GotFocus()
On Error Resume Next
    m_booNoticeFocused = True
    RefreshNotice False
End Sub

Private Sub tbrNotice_LostFocus()
On Error Resume Next
    m_booNoticeFocused = False
    RefreshNotice False
End Sub

Private Sub tbrPlugins_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
    Err.Clear
    If DoCommand(Button.key) Then
    Else
        Debug.Print "Button: " & Button.key & " has no command handler."
    End If
End Sub

Private Sub tmrClock_Timer()
On Error Resume Next
    RefreshWindows
    RefreshActiveDocument
End Sub
'
'Public Sub Menu_Click(Menu As cPopupMenu, Index As Long)
'On Error Resume Next
'    Err.Clear
'    If DoCommand(Menu.ItemKey(Index)) Then
'    Else
'        Debug.Print "Menu: " & Menu.ItemKey(Index) & " has no command handler."
'    End If
'End Sub
'
'Public Sub Menu_Initialize(Menu As cPopupMenu, Index As Long)
'On Error Resume Next
'Dim l_lngIndex As Long
'Dim l_frmForm As Form, l_lngForm As Long, l_lngWindowsAdded As Long, l_strName As String
'Dim l_fpgPlugin As iFileTypePlugin, l_lngPluginIndex As Long, l_strKey As String
'Dim l_plgPlugin As iPlugin, l_icnIcon As IPictureDisp, l_mgrForm As cChildManager
'Dim l_cmnDocument As iCustomMenus, l_lngFileCount As Long, l_lngFiles As Long, l_strFilename As String
'    Select Case LCase(Trim(Replace(Menu.Caption(Index), "&", "")))
'    Case "new"
'        l_lngIndex = Index
'        Do While l_lngIndex <= Menu.Count
'            If InStr(Menu.ItemKey(l_lngIndex), "File:New(") Then
'                If Menu.ItemIcon(l_lngIndex) > 0 Then
'                    frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex(Menu.ItemKey(l_lngIndex))
'                End If
'                Menu.RemoveItem l_lngIndex
'            Else
'                l_lngIndex = l_lngIndex + 1
'            End If
'        Loop
'        l_lngPluginIndex = 1
'        For Each l_fpgPlugin In g_colFileTypePlugins
'            Set l_plgPlugin = l_fpgPlugin
'            If l_fpgPlugin.ShowInNewMenu Then
'                If ReadRegSetting("Plugins\Show In New Menu\" & TypeName(l_fpgPlugin), 1) Then
'                    Set l_icnIcon = l_plgPlugin.Icon
'                    l_strName = "File:New(" & l_lngPluginIndex & ")"
'                    If l_icnIcon Is Nothing Then
'                        Menu.InsertItem l_fpgPlugin.FileTypeName, "NewEndSeparator", , , , , GameIsLoaded, l_strName
'                    Else
'                        frmIcons.ilIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, l_strName
'                        Menu.InsertItem l_fpgPlugin.FileTypeName, "NewEndSeparator", , , frmIcons.ilIcons.ItemIndex(l_strName) - 1, , GameIsLoaded, l_strName
'                    End If
'                End If
'            End If
'            l_lngPluginIndex = l_lngPluginIndex + 1
'        Next l_fpgPlugin
'    Case "open recent"
'        Select Case LCase(Trim(Menu.ItemKey(Index)))
'        Case "recentfiles"
'            l_lngIndex = Index
'            Do While l_lngIndex <= Menu.Count
'                If InStr(Menu.ItemKey(l_lngIndex), "File:Open(") Then
'                    frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex(Menu.ItemKey(l_lngIndex))
'                    Menu.RemoveItem l_lngIndex
'                Else
'                    l_lngIndex = l_lngIndex + 1
'                End If
'            Loop
'            l_lngFileCount = ReadRegSetting("Recent\Files\Count", 0)
'            If l_lngFileCount Then
'                For l_lngFiles = 1 To l_lngFileCount
'                    l_strFilename = ReadRegSetting("Recent\Files\File " & l_lngFiles, "")
'                    If Trim(l_strFilename) <> "" Then
'                        Set l_icnIcon = Nothing
'                        Set l_plgPlugin = Nothing
'                        Set l_plgPlugin = g_edEditor.FindCapablePlugin(l_strFilename)
'                        Set l_icnIcon = l_plgPlugin.Icon
'                        l_strName = "File:Open(""" & l_strFilename & """)"
'                        If l_icnIcon Is Nothing Then
'                            Menu.InsertItem GetTitle(l_strFilename), "RecentFilesEndSeparator", , , , , True, l_strName
''                            Menu.InsertItem GetTitle(l_strFilename), "RecentFilesEndSeparator", , , , , GameIsLoaded, l_strName
'                        Else
'                            frmIcons.ilIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, l_strName
'                            Menu.InsertItem GetTitle(l_strFilename), "RecentFilesEndSeparator", , , frmIcons.ilIcons.ItemIndex(l_strName) - 1, , True, l_strName
''                            Menu.InsertItem GetTitle(l_strFilename), "RecentFilesEndSeparator", , , frmIcons.ilIcons.ItemIndex(l_strName) - 1, , GameIsLoaded, l_strName
'                        End If
'                    End If
'                Next l_lngFiles
'            End If
'        Case "recentgames"
'            l_lngIndex = Index
'            Do While l_lngIndex <= Menu.Count
'                If InStr(Menu.ItemKey(l_lngIndex), "Game:Open(") Then
'                    Menu.RemoveItem l_lngIndex
'                Else
'                    l_lngIndex = l_lngIndex + 1
'                End If
'            Loop
'            l_lngFileCount = ReadRegSetting("Recent\Games\Count", 0)
'            If l_lngFileCount Then
'                For l_lngFiles = 1 To l_lngFileCount
'                    l_strFilename = ReadRegSetting("Recent\Games\Game " & l_lngFiles, "")
'                    If Trim(l_strFilename) <> "" Then
'                        Menu.InsertItem l_strFilename, "RecentGamesEndSeparator", , , , , , "Game:Open(""" & l_strFilename & """)"
'                    End If
'                Next l_lngFiles
'            End If
'        Case Else
'        End Select
'    Case "file"
'        g_edEditor.ActionUpdate
'        With Menu
'            .Enabled(.IndexForKey("File:Open")) = True
''            .Enabled(.IndexForKey("File:Open")) = GameIsLoaded
'        End With
'    Case "edit"
'        g_edEditor.ActionUpdate
'        With Menu
'            .Enabled(.IndexForKey("Action:Cut")) = g_edEditor.CanCut
'            .Enabled(.IndexForKey("Action:Copy")) = g_edEditor.CanCopy
'            .Enabled(.IndexForKey("Action:Paste")) = g_edEditor.CanPaste
'            .Enabled(.IndexForKey("Action:Delete")) = g_edEditor.CanDelete
'            .Enabled(.IndexForKey("Action:Undo")) = g_edEditor.CanUndo
'            .Enabled(.IndexForKey("Action:Redo")) = g_edEditor.CanRedo
'            .Enabled(.IndexForKey("Action:SelectAll")) = g_edEditor.CanSelectAll
'        End With
'    Case "document"
'        If m_cmnLastDocument Is Nothing Then
'        Else
'            m_cmnLastDocument.DestroyMenus Me
'        End If
'        Set m_cmnLastDocument = Nothing
'        With Menu
'            .Enabled(.IndexForKey("Action:CloseWindow")) = Not (Me.ActiveChild Is Nothing)
'            .Enabled(.IndexForKey("Action:PreviousWindow")) = (Me.Documents.Count > 1)
'            .Enabled(.IndexForKey("Action:NextWindow")) = (Me.Documents.Count > 1)
'        End With
'        Set l_cmnDocument = Me.ActiveChild.Form
'        If l_cmnDocument Is Nothing Then
'        Else
'            l_cmnDocument.InitializeMenus Me
'        End If
'        Set m_cmnLastDocument = l_cmnDocument
'    Case "tools"
'        l_lngIndex = Index
'        Do While l_lngIndex <= Menu.Count
'            If InStr(Menu.ItemKey(l_lngIndex), "Plugins:Activate(") Then
'                frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex(Menu.ItemKey(l_lngIndex))
'                Menu.RemoveItem l_lngIndex
'            Else
'                l_lngIndex = l_lngIndex + 1
'            End If
'        Loop
'        l_lngPluginIndex = 1
'        For Each l_plgPlugin In g_colPlugins
'            If l_plgPlugin.ShowInPluginMenu Then
'                If CLng(ReadRegSetting("Plugins\Show In Menu\" & TypeName(l_plgPlugin), 1)) Then
'                    l_strName = "Plugins:Activate(" & l_lngPluginIndex & ")"
'                    Set l_icnIcon = l_plgPlugin.Icon
'                    If l_icnIcon Is Nothing Then
'                        Menu.InsertItem l_plgPlugin.PluginName, "PluginsEndSeparator", , , , , , l_strName
'                    Else
'                        frmIcons.ilIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, l_strName
'                        Menu.InsertItem l_plgPlugin.PluginName, "PluginsEndSeparator", , , frmIcons.ilIcons.ItemIndex(l_strName) - 1, , , l_strName
'                    End If
'                End If
'            End If
'            l_lngPluginIndex = l_lngPluginIndex + 1
'        Next l_plgPlugin
'    Case "view"
'        Menu.Checked(Menu.IndexForKey("Show:FileSidebar")) = picFileSidebar.Visible
'        Menu.Checked(Menu.IndexForKey("Show:Log")) = picLog.Visible
'        Menu.Checked(Menu.IndexForKey("Show:MainToolbar")) = tbrMain.Visible
'        Menu.Checked(Menu.IndexForKey("Show:GameToolbar")) = tbrGame.Visible
'        Menu.Checked(Menu.IndexForKey("Show:PluginToolbar")) = tbrPlugins.Visible
'    Case "window"
'        l_lngIndex = Index
'        Do While l_lngIndex <= Menu.Count
'            l_strKey = Menu.ItemKey(l_lngIndex)
'            If InStr(l_strKey, "Action:ActivateWindow(") Then
'                frmIcons.ilIcons.RemoveImage frmIcons.ilIcons.ItemIndex("ICON_" & Menu.ItemData(l_lngIndex))
'                Menu.RemoveItem l_lngIndex
'            Else
'                l_lngIndex = l_lngIndex + 1
'            End If
'        Loop
'        Menu.RemoveItem "Show:WindowList"
'        l_lngForm = 1
'        For Each l_mgrForm In m_colChildWindows
'            Set l_frmForm = l_mgrForm.Form
'            l_strName = "Action:ActivateWindow(" & l_lngForm & ")"
'            If l_frmForm.Icon Is Nothing Then
'            Else
'                frmIcons.ilIcons.AddFromHandle l_frmForm.Icon.Handle, Image_Icon, "ICON_" & l_frmForm.Icon.Handle
'                Menu.InsertItem "&" & (l_lngWindowsAdded) & ": " & EscapeAmpersands(l_frmForm.Caption), "WindowsEndSeparator", , l_frmForm.Icon.Handle, frmIcons.ilIcons.ImageCount - 1, , , l_strName
'            End If
'            l_lngWindowsAdded = l_lngWindowsAdded + 1
'            l_lngForm = l_lngForm + 1
'            If l_lngWindowsAdded = 10 Then
'                Menu.InsertItem "&More Windows...", "WindowsEndSeparator", , , frmIcons.ilIcons.ItemIndex("WINDOW LIST") - 1, , , "Show:WindowList"
'                Exit For
'            End If
'        Next l_mgrForm
'        If l_lngWindowsAdded = 0 Then
'            Menu.InsertItem "No Windows Open", "WindowsEndSeparator", , , , , False, "Action:ActivateWindow(-1)"
'        End If
'        Menu.Enabled(Menu.IndexForKey("Action:CloseAllWindows")) = l_lngWindowsAdded > 0
'    Case "help"
'        With Menu
'            .Enabled(.IndexForKey("Help:About")) = GameIsLoaded
'        End With
'    Case Else
'    End Select
'End Sub
'
'Public Sub Menu_UnInitialize(Menu As cPopupMenu, Index As Long)
'On Error Resume Next
'Dim l_lngIndex As Long
'Dim l_frmForm As Form, l_lngForm As Long, l_lngWindowsAdded As Long, l_strName As String
'Dim l_fpgPlugin As iFileTypePlugin, l_lngPluginIndex As Long, l_strKey As String
'Dim l_plgPlugin As iPlugin, l_icnIcon As IPictureDisp, l_mgrForm As cChildManager
'Dim l_cmnDocument As iCustomMenus
'    Select Case LCase(Trim(Replace(Menu.Caption(Index), "&", "")))
'    Case "new"
'    Case "open recent"
'    Case "edit"
'    Case "document"
'    Case "tools"
'    Case "view"
'    Case "window"
'    Case Else
'    End Select
'End Sub

Public Sub Menu_Click(Item)
On Error Resume Next
    Err.Clear
    If DoCommand(Item.key) Then
    Else
        Debug.Print "Menu: " & Item.key & " has no command handler."
    End If
End Sub

Public Sub Menu_Hide(Menu)
On Error Resume Next
Dim l_objObject As Object
Dim l_btnButton As ngToolButton
    Err.Clear
    tbrMenus.DisableUpdates = True
    For Each l_btnButton In tbrMenus.Buttons
        l_btnButton.Checked = False
        l_btnButton.Hovering = False
        l_btnButton.Pressed = False
    Next l_btnButton
    tbrMenus.DisableUpdates = False
    tbrMenus.Reflow
    Set l_objObject = Menu.Tag
    If l_objObject Is Nothing Then
    Else
        Set m_btnActiveMenu = Nothing
    End If
End Sub

Public Sub Menu_Switch(Button)
On Error Resume Next

End Sub

Public Sub Menu_Show(Menu)
On Error Resume Next
Dim l_lngIndex As Long
Dim l_frmForm As Form, l_lngForm As Long, l_lngWindowsAdded As Long, l_strName As String
Dim l_fpgPlugin As iFileTypePlugin, l_lngPluginIndex As Long, l_strKey As String
Dim l_plgPlugin As iPlugin, l_mgrForm As cChildManager, l_imgIcon As Fury2Image
Dim l_cmnDocument As iCustomMenus, l_lngFileCount As Long, l_lngFiles As Long, l_strFilename As String, l_strTitle As String
Dim l_mnuMenu As ngMenu
Dim l_objObject As Object
    g_edEditor.ActionUpdate
    Err.Clear
    Set l_objObject = Menu.Tag
    If l_objObject Is Nothing Then
    Else
        Set m_btnActiveMenu = l_objObject
    End If
    Menu.Tag.Checked = True
    Set l_mnuMenu = Menu
    With l_mnuMenu
        If l_mnuMenu Is m_mnuNew Then
            .Items.RemoveByKeys "File:New(*"
            l_lngPluginIndex = 1
            For Each l_fpgPlugin In g_colFileTypePlugins
                Set l_plgPlugin = l_fpgPlugin
                If l_fpgPlugin.ShowInNewMenu Then
                    If ReadRegSetting("Plugins\Show In New Menu\" & TypeName(l_fpgPlugin), 1) Then
                        Set l_imgIcon = Nothing
                        Set l_imgIcon = l_plgPlugin.Icon
                        l_strName = "File:New(" & l_lngPluginIndex & ")"
                        .Items.AddNew l_fpgPlugin.FileTypeName, , l_strName, l_imgIcon, , , GameIsLoaded
                    End If
                End If
                l_lngPluginIndex = l_lngPluginIndex + 1
            Next l_fpgPlugin
        ElseIf l_mnuMenu Is m_mnuRecentFiles Then
            .Items.RemoveByKeys "File:Open(*"
            l_lngFileCount = ReadRegSetting("Recent\Files\Count", 0)
            If l_lngFileCount Then
                For l_lngFiles = 1 To l_lngFileCount
                    l_strFilename = ReadRegSetting("Recent\Files\File " & l_lngFiles, "")
                    If Trim(l_strFilename) <> "" Then
                        Set l_imgIcon = Nothing
                        Set l_plgPlugin = Nothing
                        Set l_plgPlugin = g_edEditor.FindCapablePlugin(l_strFilename)
                        Set l_imgIcon = l_plgPlugin.Icon
                        l_strName = "File:Open(""" & l_strFilename & """)"
                        .Items.AddNew GetTitle(l_strFilename), , l_strName, l_imgIcon, , , GameIsLoaded
                    End If
                Next l_lngFiles
            End If
        ElseIf l_mnuMenu Is m_mnuRecentGames Then
            .Items.RemoveByKeys "Game:Open(*"
            l_lngFileCount = ReadRegSetting("Recent\Games\Count", 0)
            If l_lngFileCount Then
                For l_lngFiles = 1 To l_lngFileCount
                    l_strFilename = ReadRegSetting("Recent\Games\Game " & l_lngFiles, "")
                    If Trim(l_strFilename) <> "" Then
                        l_strTitle = l_strFilename
                        If Len(l_strFilename) > 32 Then
                            l_strTitle = "..." & Right(l_strFilename, 32)
                        End If
                        .Items.AddNew l_strTitle, , "Game:Open(""" & l_strFilename & """)"
                    End If
                Next l_lngFiles
            End If
        ElseIf l_mnuMenu Is m_mnuFile Then
        ElseIf l_mnuMenu Is m_mnuEdit Then
            .Items("Action:Cut").Enabled = g_edEditor.CanCut
            .Items("Action:Copy").Enabled = g_edEditor.CanCopy
            .Items("Action:Paste").Enabled = g_edEditor.CanPaste
            .Items("Action:Delete").Enabled = g_edEditor.CanDelete
            .Items("Action:Undo").Enabled = g_edEditor.CanUndo
            .Items("Action:Redo").Enabled = g_edEditor.CanRedo
            .Items("Action:SelectAll").Enabled = g_edEditor.CanSelectAll
            .Items("Action:SelectNone").Enabled = g_edEditor.CanSelectNone
        ElseIf l_mnuMenu Is m_mnuView Then
        ElseIf l_mnuMenu Is m_mnuPanels Then
            .Items("Show:FileSidebar").Checked = picFileSidebar.Visible
            .Items("Show:Log").Checked = picLog.Visible
        ElseIf l_mnuMenu Is m_mnuToolbars Then
            .Items("Show:MainToolbar").Checked = tbrMain.Visible
            .Items("Show:GameToolbar").Checked = tbrGame.Visible
            .Items("Show:PluginToolbar").Checked = tbrPlugins.Visible
        ElseIf l_mnuMenu Is m_mnuDocument Then
            If m_cmnLastDocument Is Nothing Then
            Else
                m_cmnLastDocument.DestroyMenus Me
            End If
            Set m_cmnLastDocument = Nothing
            
            .Items("Action:CloseWindow").Enabled = Not (Me.ActiveChild Is Nothing)
            .Items("Action:PreviousWindow").Enabled = (Me.Documents.Count > 1)
            .Items("Action:NextWindow").Enabled = (Me.Documents.Count > 1)
            
            Set l_cmnDocument = Me.ActiveChild.Form
            If l_cmnDocument Is Nothing Then
            Else
                l_cmnDocument.InitializeMenus Me
            End If
            Set m_cmnLastDocument = l_cmnDocument
        ElseIf l_mnuMenu Is m_mnuGame Then
            .Items("Game:Reload").Enabled = GameIsLoaded
            .Items("Game:Play").Enabled = GameIsLoaded
            .Items("Game:Debug").Enabled = GameIsLoaded
            .Items("Game:Debug").Checked = GameIsRunning
            .Items("Game:Pause").Enabled = GameIsRunning And GameIsLoaded
            .Items("Game:Pause").Checked = GameIsPaused
        ElseIf l_mnuMenu Is m_mnuTools Then
            .Items.RemoveByKeys ("Plugins:Activate(*")
            l_lngPluginIndex = 1
            For Each l_plgPlugin In g_colPlugins
                If l_plgPlugin.ShowInPluginMenu Then
                    If CLng(ReadRegSetting("Plugins\Show In Menu\" & TypeName(l_plgPlugin), 1)) Then
                        l_strName = "Plugins:Activate(" & l_lngPluginIndex & ")"
                        Set l_imgIcon = Nothing
                        Set l_imgIcon = l_plgPlugin.ToolbarIcon
                        If l_imgIcon Is Nothing Then Set l_imgIcon = l_plgPlugin.Icon
                        .Items.AddNew l_plgPlugin.PluginName, , l_strName, l_imgIcon, , , , , , , .Items("Separator").Index
                    End If
                End If
                l_lngPluginIndex = l_lngPluginIndex + 1
            Next l_plgPlugin
        ElseIf l_mnuMenu Is m_mnuMacros Then
        ElseIf l_mnuMenu Is m_mnuWindow Then
            .Items.RemoveByKeys "Action:ActivateWindow(*"
            .Items.Remove "Show:WindowList"
            l_lngForm = 1
            For Each l_mgrForm In m_colChildWindows
                Set l_frmForm = l_mgrForm.Form
                l_strName = "Action:ActivateWindow(" & l_lngForm & ")"
                .Items.AddNew "&" & l_lngForm & " " & l_frmForm.Caption, , l_strName, l_mgrForm.Document.DocumentIcon, , , , , , , .Items("Separator").Index
                l_lngWindowsAdded = l_lngWindowsAdded + 1
                l_lngForm = l_lngForm + 1
                If l_lngWindowsAdded = 10 Then
                    .Items.AddNew "&More Windows...", , "Show:WindowList", "window list", , , , , , , .Items("Separator").Index
                    Exit For
                End If
            Next l_mgrForm
            If l_lngWindowsAdded = 0 Then
                .Items.AddNew "No Windows Open", , "Action:ActivateWindow(-1)", , , , False, , , , 1
            End If
            .Items("Action:CloseAllWindows").Enabled = l_lngWindowsAdded > 0
        ElseIf l_mnuMenu Is m_mnuHelp Then
'            .Items("Help:About").Enabled = GameIsLoaded
        End If
    End With
'    Case "view"
'        Menu.Checked(Menu.IndexForKey("Show:FileSidebar")) = picFileSidebar.Visible
'        Menu.Checked(Menu.IndexForKey("Show:Log")) = picLog.Visible
'        Menu.Checked(Menu.IndexForKey("Show:MainToolbar")) = tbrMain.Visible
'        Menu.Checked(Menu.IndexForKey("Show:GameToolbar")) = tbrGame.Visible
'        Menu.Checked(Menu.IndexForKey("Show:PluginToolbar")) = tbrPlugins.Visible
'    Case "help"
'        With Menu
'            .Enabled(.IndexForKey("Help:About")) = GameIsLoaded
'        End With
'    Case Else
'    End Select
End Sub

Private Sub tmrFixFocus_Timer()
On Error Resume Next
    tmrFixFocus.Enabled = False
    W32SetFocus m_lngFocus
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
            ElseIf .Form.Visible = False Then
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

Private Sub tmrNotice_Timer()
On Error Resume Next
    RefreshNotice False
    If HiResTimer > m_notNotice.CloseTime Then
        DismissNotice
    End If
End Sub

Private Sub tmrOpenMenu_Timer()
On Error Resume Next
Dim l_btnButton As ngToolButton
    If MenuIsOpen Then Exit Sub
    tmrOpenMenu.Enabled = False
    Set l_btnButton = m_btnMenuToOpen
    Set m_btnMenuToOpen = Nothing
    tbrMenus_ButtonClick l_btnButton
End Sub

Private Sub tmrPlayGame_Timer()
On Error Resume Next
    tmrPlayGame.Enabled = False
    PlayGame
End Sub

Private Sub tmrRefreshFileSidebar_Timer()
On Error Resume Next
    tmrRefreshFileSidebar.Enabled = False
    If g_engEngine Is Nothing Then
        tvFileTree.Nodes.Clear
        tvFileTree.Enabled = False
    ElseIf g_engEngine.Filesystem Is Nothing Then
        tvFileTree.Nodes.Clear
        tvFileTree.Enabled = False
    Else
        EnumFilesystem tvFileTree, g_engEngine.Filesystem
        tvFileTree.Enabled = True
    End If
End Sub

Private Sub tmrTabs_Timer()
On Error Resume Next
'    RefreshDocumentTabs
End Sub

Private Sub tsDocuments_TabClose(theTab As ngUI.ngTab)
On Error Resume Next
Dim l_mgrChild As cChildManager
    Set l_mgrChild = theTab.Tag
    l_mgrChild.Hide
End Sub

Private Sub tsDocuments_TabSelected(theTab As ngUI.ngTab)
On Error Resume Next
Dim l_mgrChild As cChildManager
    Set l_mgrChild = theTab.Tag
    If l_mgrChild Is ActiveChild Then
    Else
        l_mgrChild.Activate
    End If
End Sub

Private Sub tvFileTree_AfterLabelEdit(node As vbalTreeViewLib6.cTreeViewNode, NewString As String, Cancel As Boolean)
On Error Resume Next
Dim l_strPath As String, l_strNewPath As String
Dim l_fsFilesystem As Fury2Filesystem
    Set l_fsFilesystem = g_engEngine.Filesystem
    If Right(node.key, 1) = "/" Then
        ' Folder
        l_strPath = Replace(l_fsFilesystem.Root & Replace(node.key, "/", "\"), "\\", "\")
        l_strNewPath = Left(l_strPath, InStrRev(l_strPath, node.Text) - 1) & NewString
        Name l_strPath As l_strNewPath
    Else
        ' File
        l_strPath = Replace(l_fsFilesystem.Root & Replace(node.key, "/", "\"), "\\", "\")
        l_strNewPath = Left(l_strPath, InStrRev(l_strPath, node.Text) - 1) & NewString
        Name l_strPath As l_strNewPath
    End If
End Sub

Private Sub tvFileTree_DblClick()
On Error Resume Next
Dim l_strFilename As String
Dim l_filFile As Fury2File
    If Right(Trim(tvFileTree.SelectedItem.key), 1) = "/" Then Exit Sub
    Set l_filFile = g_engEngine.Filesystem.File(tvFileTree.SelectedItem.key)
    l_strFilename = l_filFile.GetRealFilename
    g_edEditor.File_Open l_strFilename
End Sub

Private Sub tvFileTree_DragDropRequest(Data As DataObject, nodeOver As vbalTreeViewLib6.cTreeViewNode, ByVal bAbove As Boolean, ByVal hitTest As Long)
On Error Resume Next
Dim l_lngFiles As Long
    If Data.Files.Count > 0 Then
        For l_lngFiles = 1 To Data.Files.Count
            If Right(Trim(nodeOver.key), 1) = "/" Then
                DoCopy Data.Files(l_lngFiles), g_edEditor.GamePath & "\" & Replace(nodeOver.key, "/", "\") & GetTitle(Data.Files(l_lngFiles))
            Else
                DoCopy Data.Files(l_lngFiles), g_edEditor.GamePath & "\" & GetTitle(Data.Files(l_lngFiles))
            End If
        Next l_lngFiles
        RefreshFileSidebar
    End If
End Sub

Private Sub tvFileTree_Expand(node As vbalTreeViewLib6.cTreeViewNode)
On Error Resume Next
End Sub

Private Sub tvFileTree_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyF5 Then
        RefreshFileSidebar
    End If
End Sub

Private Sub tvFileTree_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_nodNode As cTreeViewNode
Dim l_strFolderName As String
Dim l_fsFilesystem As Fury2Filesystem
    Set l_fsFilesystem = g_engEngine.Filesystem
    If Button = 2 Then
        Set l_nodNode = tvFileTree.hitTest(X, Y)
        If l_nodNode Is Nothing Then
            ' Blank
            Select Case QuickShowMenu2(tvFileTree, X, Y, _
                Menus("Explore", "-", "New Folder"))
            Case 1
                Shell "explorer """ & Replace(l_fsFilesystem.Root, "/", "\") & """", vbNormalFocus
            Case 3
                l_strFolderName = InputBox("Folder Name", "New Folder", "New Folder")
                MkDir l_fsFilesystem.Root & l_strFolderName
                RefreshFileSidebar
            Case Else
            End Select
        Else
            If Right(l_nodNode.key, 1) = "/" Then
                ' Folder
                Select Case QuickShowMenu2(tvFileTree, X, Y, _
                    Menus("Expand", "Explore", "-", "Rename", "Delete", "-", "New Folder"))
                Case 1
                    l_nodNode.Expanded = True
                Case 2
                    Shell "explorer """ & Replace(l_fsFilesystem.Root & Replace(l_nodNode.key, "/", "\"), "\\", "\") & """", vbNormalFocus
                Case 4
                    DoRename l_fsFilesystem.Root & Replace(l_nodNode.key, "/", "\")
                    RefreshFileSidebar
                Case 5
                    DoDelete l_fsFilesystem.Root & Replace(l_nodNode.key, "/", "\")
                    RefreshFileSidebar
                Case 7
                    l_strFolderName = InputBox("Folder Name", "New Folder", "New Folder")
                    MkDir l_fsFilesystem.Root & Replace(l_nodNode.key, "/", "\") & l_strFolderName
                    RefreshFileSidebar
                Case Else
                End Select
            Else
                ' File
                Select Case QuickShowMenu2(tvFileTree, X, Y, _
                    Menus("Open", "-", "Rename", "Delete"))
                Case 1
                    g_edEditor.File_Open l_fsFilesystem.File(l_nodNode.key).GetRealFilename
                Case 3
                    DoRename l_fsFilesystem.File(l_nodNode.key).GetRealFilename
                    RefreshFileSidebar
                Case 4
                    DoDelete l_fsFilesystem.File(l_nodNode.key).GetRealFilename
                    RefreshFileSidebar
                Case Else
                End Select
            End If
        End If
    End If
End Sub

Private Sub tvFileTree_OLEDragOver(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single, State As Integer)
On Error Resume Next
    If Data.Files.Count > 0 Then
        Effect = vbDropEffectCopy
    End If
End Sub

Private Sub RefreshNotice(Optional ByVal Reposition As Boolean = True)
On Error Resume Next
Dim l_lngTopColor As Long, l_lngBottomColor As Long
Dim l_lngOption As Long
Dim l_lngLeftSpace As Long, l_lngRightSpace As Long
Dim l_lngTopSpace As Long, l_lngBottomSpace As Long
Dim l_lngTextHeight As Long, l_lngTitleHeight As Long
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_rctWindow As Win32.RECT
Dim l_lngWindowWidth As Long, l_lngWindowHeight As Long
Dim l_rctTextSize As Win32.RECT, l_rctText As Win32.RECT
Dim l_sngCloseTime As Single
Dim l_strWaitingNotices As String
    GetClientRect Me.hwnd, l_rctWindow
    l_lngWindowWidth = (l_rctWindow.Right - l_rctWindow.Left)
    l_lngWindowHeight = (l_rctWindow.Bottom - l_rctWindow.Top)
    l_lngLeftSpace = 4
    l_lngTopSpace = 4
    l_lngRightSpace = 4
    If m_notNotice.CloseTime <> -1 Then
        l_lngRightSpace = l_lngRightSpace + 8
    End If
    If m_notNotice.Icon Is Nothing Then
    Else
        l_lngLeftSpace = l_lngLeftSpace + m_notNotice.Icon.Width + 4
    End If
    If Reposition Then
        tbrNotice.EnableTheme = False
        tbrNotice.DisableUpdates = True
        tbrNotice.Buttons.Clear
        tbrNotice.Font.Name = picNotice.Font.Name
        tbrNotice.Font.Size = picNotice.Font.Size
        tbrNotice.Font.Bold = True
        For l_lngOption = LBound(m_notNotice.Options) To UBound(m_notNotice.Options)
            With tbrNotice.Buttons.AddNew(" " & CStr(m_notNotice.Options(l_lngOption)(0)) & " ")
                Set .ClickEvent = m_notNotice.Options(l_lngOption)(1)
            End With
        Next l_lngOption
        tbrNotice.DisableUpdates = False
        tbrNotice.Reflow
    End If
    l_lngBottomSpace = tbrNotice.IdealHeight + 8
    l_lngWidth = l_lngWindowWidth - 50
    l_rctTextSize.Right = l_lngWidth - (l_lngLeftSpace + l_lngRightSpace)
    l_rctTextSize.Bottom = 10000
    picNotice.Font.Bold = False
    Win32.DrawText picNotice.hdc, m_notNotice.Text, Len(m_notNotice.Text), l_rctTextSize, DrawText_CalculateRect Or DrawText_Wrap_WordBreak
    l_lngTextHeight = l_rctTextSize.Bottom
    l_rctTextSize.Right = l_lngWidth - (l_lngLeftSpace + l_lngRightSpace)
    l_rctTextSize.Bottom = 10000
    picNotice.Font.Bold = True
    Win32.DrawText picNotice.hdc, m_notNotice.Title, Len(m_notNotice.Title), l_rctTextSize, DrawText_CalculateRect Or DrawText_Wrap_WordBreak
    l_lngTitleHeight = l_rctTextSize.Bottom + 4
    l_lngHeight = (l_lngTextHeight + l_lngTitleHeight + l_lngTopSpace + l_lngBottomSpace)
    
    If Reposition Then
        SetParent picNotice.hwnd, picHiddenControls.hwnd
        picNotice.Move (l_lngWindowWidth - l_lngWidth) / 2, l_lngWindowHeight - l_lngHeight - (picStatus.Height / Screen.TwipsPerPixelY), l_lngWidth, l_lngHeight
        SetParent picNotice.hwnd, GetParent(picFileSidebar.hwnd)
        m_imgNotice.Resize picNotice.ScaleWidth, picNotice.ScaleHeight
    End If
    l_lngBottomColor = SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
    l_lngTopColor = BlendColors(l_lngBottomColor, SwapChannels(GetSystemColor(SystemColor_Button_Highlight), Red, Blue), 127)
    m_imgNotice.Clear l_lngBottomColor
    m_imgNotice.GradientFill F2Rect(0, 0, m_imgNotice.Width, m_imgNotice.Height - tbrNotice.IdealHeight, False), Array(l_lngTopColor, l_lngTopColor, l_lngBottomColor, l_lngBottomColor), RenderMode_Normal
    If m_notNotice.Icon Is Nothing Then
    Else
        m_notNotice.Icon.Draw m_imgNotice, (l_lngLeftSpace - 4) / 2 + 4, m_imgNotice.Height / 2, 1, , , BlitMode_SourceAlpha
    End If
    m_imgNotice.Box m_imgNotice.Rectangle, F2RGB(0, 0, 0, IIf(m_booNoticeFocused, 192, 63)), RenderMode_SourceAlpha
    If m_notNotice.CloseTime <> -1 Then
        l_sngCloseTime = (HiResTimer - m_notNotice.OpenTime) / (m_notNotice.CloseTime - m_notNotice.OpenTime)
        l_lngBottomColor = F2RGB(0, 0, 0, 63)
        l_lngTopColor = F2RGB(0, 0, 0, 63 + (128 * (1 - l_sngCloseTime)))
        m_imgNotice.GradientFill F2Rect(m_imgNotice.Width - 9, 1 + ((m_imgNotice.Height - 2) * l_sngCloseTime), m_imgNotice.Width - 1, m_imgNotice.Height - 1, True), Array(l_lngTopColor, l_lngTopColor, l_lngBottomColor, l_lngBottomColor), RenderMode_SourceAlpha
    End If
    CopyImageToDC picNotice.hdc, m_imgNotice.Rectangle, m_imgNotice
    picNotice.Font.Bold = True
    With l_rctText
        .Left = l_lngLeftSpace
        .Top = l_lngTopSpace
        .Right = picNotice.ScaleWidth - l_lngRightSpace
        .Bottom = .Top + l_lngTitleHeight
    End With
    DrawText picNotice.hdc, m_notNotice.Title, Len(m_notNotice.Title), l_rctText, DrawText_Wrap_WordBreak
    picNotice.Font.Bold = False
    With l_rctText
        .Left = l_lngLeftSpace
        .Top = l_lngTopSpace + l_lngTitleHeight
        .Right = picNotice.ScaleWidth - l_lngRightSpace
        .Bottom = .Top + l_lngTextHeight
    End With
    DrawText picNotice.hdc, m_notNotice.Text, Len(m_notNotice.Text), l_rctText, DrawText_Wrap_WordBreak
    If m_colNoticeQueue.Count > 0 Then
        picNotice.Font.Size = picNotice.Font.Size - 2
        With l_rctText
            .Left = l_lngLeftSpace
            .Top = l_lngTopSpace
            .Right = picNotice.ScaleWidth - l_lngRightSpace
            .Bottom = .Top + l_lngTitleHeight
        End With
        l_strWaitingNotices = CStr(m_colNoticeQueue.Count) & " more notice(s) waiting"
        DrawText picNotice.hdc, l_strWaitingNotices, Len(l_strWaitingNotices), l_rctText, DrawText_Align_Right
        picNotice.Font.Size = picNotice.Font.Size + 2
    End If
    tbrNotice.Move (picNotice.ScaleWidth - tbrNotice.IdealWidth) / 2, picNotice.ScaleHeight - tbrNotice.IdealHeight - 4, tbrNotice.IdealWidth, tbrNotice.IdealHeight
    picNotice.Refresh
End Sub

Friend Sub ActivateNotice()
On Error Resume Next
    Set m_notNotice = m_colNoticeQueue(1)
    m_colNoticeQueue.Remove 1
    tmrNotice.Enabled = False
    m_booNoticeFocused = False
    picNotice.Visible = False
    SetParent picNotice.hwnd, picHiddenControls.hwnd
    If m_notNotice Is Nothing Then Exit Sub
    RefreshNotice
    With m_notNotice
        .OpenTime = HiResTimer()
        If .CloseTime <> -1 Then
            .CloseTime = HiResTimer() + .CloseTime
        End If
        If .CloseTime <> -1 Then tmrNotice.Enabled = True
    End With
    tbrNotice.EnableTheme = False
    picNotice.Visible = True
    tbrNotice.EnableTheme = False
End Sub

Public Sub ShowNotice(Optional Title As String = "", Optional Text As String = "", Optional Icon As Fury2Image = Nothing, Optional Options As Variant = Nothing, Optional AutoClose As Boolean = True)
On Error Resume Next
Dim l_notNew As cNotice
    tbrNotice.EnableTheme = False
    Set l_notNew = New cNotice
    With l_notNew
        If (VarType(Options) And vbArray) = vbArray Then
            .Options = Options
        Else
            .Options = Array(Array("OK"))
        End If
        .Title = Title
        .Text = Text
        .CloseTime = IIf(AutoClose, g_edEditor.Options.NoticeAutoCloseTime, -1)
        Set .Icon = Icon
    End With
    m_colNoticeQueue.Add l_notNew
    If m_notNotice Is Nothing Then
        ActivateNotice
    Else
        RefreshNotice
    End If
End Sub

Public Sub DismissNotice()
On Error Resume Next
    tmrNotice.Enabled = False
    m_booNoticeFocused = False
    picNotice.Visible = False
    SetParent picNotice.hwnd, picHiddenControls.hwnd
    Set m_notNotice = Nothing
    If m_colNoticeQueue.Count > 0 Then
        ActivateNotice
    End If
End Sub
