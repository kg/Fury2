VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Global"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements Fury2Object
Implements Fury2Plugin
Public Engine As Fury2Engine
Public Skins As Fury2Collection
Public Fonts As Fury2Collection
Public Forms As Fury2Collection
Public Desktop As Container
Public DrawBehind As Boolean
Public Backbuffer As Fury2Image

Sub UnloadForm(Name)
On Error Resume Next
    Desktop.Children.Remove Name
    Forms(Name).Visible = False
    Forms.Remove Name
    Set Engine.IObjThis = Nothing
    Engine.RunCode Engine.ScriptEngine.Language.GenerateAssignment(CStr(Name), "IObjThis", True)
End Sub

Sub HideForm(Name)
On Error Resume Next
    Desktop.Children.Remove Name
    Forms(Name).Visible = False
End Sub

Sub ShowContextMenu(Menu, Optional ByVal X As Long = -32767, Optional ByVal Y As Long = -32767)
On Error Resume Next
    Desktop.Children.Add Menu
    Menu.Popup = True
    If (X = -32767) Then
        X = Engine.Mouse.X
    End If
    If (Y = -32767) Then
        Y = Engine.Mouse.Y
    End If
    Menu.Show X, Y
End Sub

Sub ShowForm(Name, Optional ByVal X As Long = -32767, Optional ByVal Y As Long = -32767)
On Error Resume Next
    Desktop.Children.Add Forms(Name).This, Name
    Desktop.Container.Sync
    With Forms(Name)
        .This.Visible = True
        If (X = -32767) Then
            X = (Desktop.Rectangle.Width - .This.Rectangle.Width) / 2
        End If
        If (Y = -32767) Then
            Y = (Desktop.Rectangle.Height - .This.Rectangle.Height) / 2
        End If
        .This.Rectangle.RelLeft = X
        .This.Rectangle.RelTop = Y
    End With
    Desktop.Container.SetFocus Forms(Name)
End Sub

Function LoadForm(Path) As Object
On Error Resume Next
Dim l_strName As String
Dim l_strScript As String
Dim l_strChosenName As String
    If Engine.FileSystem.FileExists(Path) = False Then
        Engine.CriticalError "UIKit", "LoadForm", "File not found: " & Path
    End If
    l_strName = Engine.VariableName(Path)
    l_strScript = Engine.FileSystem.File(Path).Text
    Engine.CacheScript l_strName, Engine.ScriptEngine.Language.GenerateClass(l_strName, l_strScript)
    Set LoadForm = Engine.ScriptEngine.Eval(Engine.ScriptEngine.Language.GenerateInstantiation(l_strName))
    l_strChosenName = l_strName
    l_strChosenName = CStr(LoadForm.Name)
    Forms.Add LoadForm, l_strChosenName
    Set Engine.IObjThis = LoadForm
    Engine.RunCode Engine.ScriptEngine.Language.GenerateAssignment(l_strChosenName, "IObjThis", True)
    Set Engine.IObjThis = Nothing
End Function

Sub LoadSkinDef(ByRef Skin As SkinState, ByRef Path As String)
On Error Resume Next
Dim l_strProps() As String, l_lngProps As Long
Dim l_strKey As String, l_strValue As String, l_strParts() As String
Dim l_strChannels() As String
    If Engine.FileSystem.FileExists(Path) Then
        l_strProps = Split(Engine.FileSystem.File(Path).Text, ";")
        For l_lngProps = LBound(l_strProps) To UBound(l_strProps)
            If InStr(l_strProps(l_lngProps), ":") Then
                l_strParts = Split(l_strProps(l_lngProps), ":")
                l_strKey = Replace(l_strParts(0), vbCrLf, "")
                l_strValue = l_strParts(1)
                Select Case LCase(Trim(l_strKey))
                Case "textcolor", "highlightcolor"
                    l_strChannels = Split(l_strValue, ",")
                    CallByName Skin, LCase(Trim(l_strKey)), VbLet, F2RGB(CLng(l_strChannels(0)), CLng(l_strChannels(1)), CLng(l_strChannels(2)), CLng(l_strChannels(3)))
                Case "edgeoffset"
                    Skin.EdgeOffset = CLng(l_strValue)
                Case "font"
                    Set Skin.Font = Fonts(LCase(Trim(l_strValue)))
                Case Else
                End Select
            End If
        Next l_lngProps
    End If
End Sub

Sub LoadSkins(ByVal Path)
On Error Resume Next
Dim l_fldFolders As Fury2Folders
Dim l_filFiles As Fury2Files
Dim l_lngFiles As Long, l_lngFolders As Long
Dim l_sknSkin As Skin
Dim l_sksState As SkinState
Dim l_wskWindow As Fury2WindowSkin
Dim l_imgImage As Fury2Image
Dim l_strFile As String, l_strFolder As String
    Path = CStr(Path)
    If Right(Path, 1) <> "/" Then Path = Path + "/"
    Set l_fldFolders = Engine.FileSystem.EnumFolders(Path & "/*", False)
    For l_lngFolders = 1 To l_fldFolders.Count
        l_strFolder = l_fldFolders.Folder(l_lngFolders).Name
        Set l_sknSkin = New Skin
        Set l_filFiles = Engine.FileSystem.EnumFiles(l_strFolder, "*.txt")
        For l_lngFiles = 1 To l_filFiles.Count
            l_strFile = l_filFiles.File(l_lngFiles)
            Set l_sksState = New SkinState
            Set l_wskWindow = Nothing
            If (Engine.FileSystem.FileExists(Replace(l_strFile, ".txt", ".png"))) Then
                Set l_wskWindow = Engine.LoadWindowSkin(Replace(l_strFile, ".txt", ".png"))
            End If
            Set l_sksState.WindowSkin = l_wskWindow
            Set l_sksState.Font = Fonts("Normal")
            l_sksState.TextColor = F2White
            l_sksState.HighlightColor = F2RGB(32, 96, 220, 127)
            LoadSkinDef l_sksState, l_strFile
            l_sknSkin.States.Add l_sksState, LCase(Trim(Replace(Replace(l_strFile, l_strFolder & "/", ""), ".txt", "")))
        Next l_lngFiles
        Set l_filFiles = Engine.FileSystem.EnumFiles(l_strFolder + "/glyphs/", "*.png")
        For l_lngFiles = 1 To l_filFiles.Count
            l_strFile = l_filFiles.File(l_lngFiles)
            l_sknSkin.Glyphs.Add Engine.LoadImage(l_strFile), LCase(Trim(Replace(Replace(l_strFile, l_strFolder & "/glyphs/", ""), ".png", "")))
        Next l_lngFiles
        Skins.Add l_sknSkin, LCase(Trim(Replace(Replace(l_strFolder, Path, ""), "/", "")))
    Next
End Sub

Sub LoadFonts(ByVal Path)
On Error Resume Next
Dim l_filFiles As Fury2Files
Dim l_lngFiles As Long
Dim l_strFile As String
Dim l_strProps() As String, l_lngProps As Long
Dim l_strKey As String, l_strValue As String, l_strParts() As String
Dim l_strName As String, l_fntFont As Fury2Font
    Path = CStr(Path)
    If Right(Path, 1) <> "/" Then Path = Path + "/"
    Set l_filFiles = Engine.FileSystem.EnumFiles(Path, "*.txt")
    For l_lngFiles = 1 To l_filFiles.Count
        l_strFile = l_filFiles.File(l_lngFiles)
        l_strProps = Split(Engine.FileSystem.File(l_strFile).Text, ";")
        For l_lngProps = LBound(l_strProps) To UBound(l_strProps)
            If InStr(l_strProps(l_lngProps), ":") Then
                l_strParts = Split(l_strProps(l_lngProps), ":")
                l_strKey = Replace(l_strParts(0), vbCrLf, "")
                l_strValue = l_strParts(1)
                Select Case LCase(Trim(l_strKey))
                Case "name"
                    l_strName = Trim(l_strValue)
                Case "file"
                    Set l_fntFont = Engine.LoadFont(Path + Trim(l_strValue))
                Case Else
                End Select
            End If
        Next l_lngProps
        Fonts.Add l_fntFont, l_strName
    Next l_lngFiles
End Sub

Public Function GetScreenLocation(Widget, Optional ByVal Center As Boolean = False)
On Error Resume Next
Dim l_objParent As Object
Dim l_lngX As Long, l_lngY As Long
    If Widget Is Nothing Then
        GetScreenLocation = Array(0, 0)
        Exit Function
    End If
    With Widget.Rectangle
        l_lngX = .Left
        l_lngY = .Top
    End With
    Set l_objParent = Widget.Parent
    Do Until l_objParent Is Nothing
        With l_objParent.Rectangle
            l_lngX = l_lngX + .Left
            l_lngY = l_lngY + .Top
        End With
        Err.Clear
        Set l_objParent = l_objParent.Parent
        If Err <> 0 Then Exit Do
    Loop
    If Center Then
        With Widget.Rectangle
            l_lngX = l_lngX + (.Width / 2)
            l_lngY = l_lngY + (.Height / 2)
        End With
    End If
    GetScreenLocation = Array(l_lngX, l_lngY)
End Function

Public Sub Sync()
On Error Resume Next
    Desktop.Container.Sync
    Desktop.Container.SetFocus Desktop.GetFocusableChild(1)
End Sub

Public Function CreateSkin() As Skin
On Error Resume Next
    Set CreateSkin = New Skin
End Function

Public Function CreateWidget(ByRef WidgetType As String, Optional ByVal Name As String = "") As Object
On Error Resume Next
Dim l_ctrContainer As UIKit.Container
Dim l_btnButton As UIKit.Button
Dim l_wndWindow As UIKit.Window
Dim l_txfTextField As UIKit.TextField
Dim l_gphGraphic As UIKit.Graphic
Dim l_lbxListBox As UIKit.ListBox
Dim l_mnuMenu As UIKit.Menu
Dim l_lblLabel As UIKit.Label
Dim l_sbrScrollBar As UIKit.ScrollBar
    Select Case LCase(Trim(WidgetType))
    Case "container"
        Set l_ctrContainer = New Container
        With l_ctrContainer
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_ctrContainer
    Case "button"
        Set l_btnButton = New Button
        With l_btnButton
            Set .Skin = Skins("button")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_btnButton
    Case "window"
        Set l_wndWindow = New Window
        With l_wndWindow
            Set .Skin = Skins("window")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_wndWindow
    Case "textfield"
        Set l_txfTextField = New TextField
        With l_txfTextField
            Set .Skin = Skins("textfield")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_txfTextField
    Case "label"
        Set l_lblLabel = New Label
        With l_lblLabel
            Set .Skin = Skins("label")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_lblLabel
    Case "graphic"
        Set l_gphGraphic = New Graphic
        With l_gphGraphic
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_gphGraphic
    Case "listbox"
        Set l_lbxListBox = New ListBox
        With l_lbxListBox
            Set .Skin = Skins("listbox")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_lbxListBox
    Case "menu"
        Set l_mnuMenu = New Menu
        With l_mnuMenu
            Set .Skin = Skins("menu")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_mnuMenu
    Case "scrollbar"
        Set l_sbrScrollBar = New ScrollBar
        With l_sbrScrollBar
            Set .Skin = Skins("scrollbar")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
            .Refresh
        End With
        Set CreateWidget = l_sbrScrollBar
    Case Else
    End Select
End Function

Private Sub Class_Initialize()
On Error Resume Next
    Set Globals = Me
    Set Skins = New Fury2Collection
    Set Fonts = New Fury2Collection
    Set Forms = New Fury2Collection
    Set Desktop = New Container
    Desktop.Name = "Desktop"
    Set Desktop.Parent = Me
    Set NullCapture = New NullWidget
    ButtonCount = 0
    TextFieldCount = 0
    WindowCount = 0
    ContainerCount = 0
    GraphicCount = 0
    ListBoxCount = 0
    MenuCount = 0
End Sub

Private Sub Fury2Plugin_ControlActivate(Control As Engine.Fury2InputControl)
On Error Resume Next
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Engine.Fury2InputControl)
On Error Resume Next
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
    Fury2Plugin_DrawBehind = Me.DrawBehind
End Property

Private Property Set Fury2Plugin_Engine(ByVal RHS As Engine.Fury2Engine)
On Error Resume Next
    Set Engine = RHS
    Set Backbuffer = RHS.Backbuffer
    Set Desktop.Rectangle = Backbuffer.Rectangle
    Set mdlGlobal.Engine = RHS
    Engine.AddObject "UIKit", Me
End Property

Private Property Get Fury2Plugin_Engine() As Engine.Fury2Engine
    Set Fury2Plugin_Engine = Engine
End Property

Private Sub Fury2Plugin_Init()
On Error Resume Next
    Desktop.Container.Sync
End Sub

Private Sub Fury2Plugin_KeyDown(KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    Desktop.Widget.KeyDown Nothing, KeyCode, Shift
End Sub

Private Sub Fury2Plugin_KeyPress(KeyAscii As Long)
On Error Resume Next
    Desktop.Widget.KeyPress Nothing, KeyAscii
End Sub

Private Sub Fury2Plugin_KeyUp(KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    Desktop.Widget.KeyUp Nothing, KeyCode, Shift
End Sub

Private Sub Fury2Plugin_MapLoad(Filename As String)
On Error Resume Next
End Sub

Private Sub Fury2Plugin_MouseDown(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    Desktop.Widget.MouseDown Nothing, Button, Shift, X, Y
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    Desktop.Widget.MouseMove Nothing, Button, Shift, X, Y
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    Desktop.Widget.MouseUp Nothing, Button, Shift, X, Y
End Sub

Private Sub Fury2Plugin_Redraw()
On Error Resume Next
    Desktop.Widget.Redraw Nothing, Backbuffer, 0, 0
End Sub

Private Sub Fury2Plugin_Shutdown()
On Error Resume Next
End Sub

Private Sub Fury2Plugin_Startup()
On Error Resume Next
End Sub

Private Sub Fury2Plugin_SyncUpdate()
On Error Resume Next
End Sub

Private Sub Fury2Plugin_Unload()
On Error Resume Next
End Sub

Private Sub Fury2Plugin_Update()
On Error Resume Next
End Sub

