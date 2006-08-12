VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Global"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private Type WidgetGroup
    Variable As String
    Values() As String
End Type

Implements Fury2Object
Implements Fury2Plugin
Public DebugForms As Boolean
Public Engine As Fury2Engine
Public Skins As Fury2Collection
Public Fonts As Fury2Collection
Public Forms As Fury2Collection
Public Desktop As Container
Public DrawBehind As Boolean
Public Buffer As Fury2Image
Private m_rctRectangle As Fury2Rect

Public Property Get Rectangle() As Fury2Rect
    Set Rectangle = m_rctRectangle
End Property

Public Property Set Rectangle(NewRect As Fury2Rect)
    Set m_rctRectangle = NewRect.Copy
    Set Desktop.Rectangle = NewRect.Copy.Translate(-NewRect.Left, -NewRect.Top)
End Property

Sub SetUIKitTintColor(ByVal Color As Long)
On Error Resume Next
Dim l_sknSkin As Skin
Dim l_sksState As SkinState
    If Color = F2White Then
        For Each l_sknSkin In Skins
            For Each l_sksState In l_sknSkin.States
                If l_sksState.WindowSkin Is Nothing Then
                Else
                    With l_sksState.WindowSkin
                        .BlitMode = BlitMode_Merge
                        .Tint = Color
                    End With
                End If
                l_sksState.TintColor = Color
            Next l_sksState
        Next l_sknSkin
    Else
        For Each l_sknSkin In Skins
            For Each l_sksState In l_sknSkin.States
                If l_sksState.WindowSkin Is Nothing Then
                Else
                    With l_sksState.WindowSkin
                        .BlitMode = BlitMode_Font_Merge
                        .Tint = Color
                    End With
                End If
                l_sksState.TintColor = Color
            Next l_sksState
        Next l_sknSkin
    End If
End Sub

Sub UnloadForm(Name)
On Error Resume Next
    Desktop.Children.Remove Name
    Forms(Name).Visible = False
    Forms.Remove Name
    Set Engine.IObjThis = Nothing
    Engine.RunCode Engine.ScriptEngine.Language.GenerateAssignment(CStr(Name), "IObjThis", True)
End Sub

Sub UnloadAllForms()
On Error Resume Next
    Forms.Clear
    Desktop.Children.Clear
End Sub

Sub HideForm(Name)
On Error Resume Next
    Desktop.Children.Remove Name
    Forms(Name).Visible = False
End Sub

Sub ShowContextMenu(Menu, Optional ByVal X As Long = -32767, Optional ByVal Y As Long = -32767)
On Error Resume Next
    Desktop.Children.Remove Desktop.Children.Find(Menu)
    Desktop.Children.Add Menu, , 1
    Menu.Popup = True
    If (X = -32767) Then
        X = Engine.Mouse.X
    End If
    If (Y = -32767) Then
        Y = Engine.Mouse.Y
    End If
    Menu.Show X - Rectangle.Left, Y - Rectangle.Top
End Sub

Sub ShowForm(Name, Optional ByVal X As Long = -32767, Optional ByVal Y As Long = -32767)
On Error Resume Next
    Desktop.Children.Add Forms(Name).This, Name, 1
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
    Desktop.Container.SetFocus Forms(Name).This
End Sub

Function GetLevelVar(ByVal Level As Long) As String
On Error Resume Next
    If Level = 1 Then
        GetLevelVar = "This"
    Else
        GetLevelVar = "NewWidget" & (Level - 1)
    End If
End Function

Function ParseFormScript(ByRef Text As String) As String
On Error Resume Next
Dim l_strScript As String
Dim l_strConstructor As String
Dim l_strBlock As String
Dim l_strOutput As String
Dim l_strLines() As String
Dim Lang As IScriptLanguage
Dim l_lngWidget As Long
Dim l_wgWidgetGroups() As WidgetGroup
Dim l_lngWidgetGroups As Long
Dim l_strName As String, l_strType As String
Dim l_lngPos As Long
Dim l_strIndent As String
Dim l_booOmitLine As Boolean
Dim l_strVar As String
Dim l_lngLine As Long, l_strLine As String, l_strToken As String, l_strArgs As String
Dim l_lngNames As Long
Dim l_lngWidgetCount As Long
    Set Lang = Engine.ScriptEngine.Language
    l_lngWidgetGroups = 0
    l_strLines = Lang.SplitLines(Text)
    l_strOutput = "Public This" & Lang.LineTerminator & _
        "Public Name" & Lang.LineTerminator
    l_strIndent = String(4, " ")
    l_strConstructor = "On Error Resume Next" & Lang.LineTerminator & _
        "Dim NewWidget1, NewWidget2, NewWidget3, NewWidget4, NewWidget5, NewWidget6, NewWidget7, NewWidget8, NewWidget9" & Lang.LineTerminator
    For l_lngLine = LBound(l_strLines) To UBound(l_strLines)
        l_strLine = Trim(l_strLines(l_lngLine))
        l_booOmitLine = False
        If (Mid(l_strLine, 1, 1) = "'") Then
        Else
            If InStr(l_strLine, " ") Then
                l_strToken = Left(l_strLine, InStr(l_strLine, " ") - 1)
                l_strArgs = Mid(l_strLine, InStr(l_strLine, " ") + 1)
                Select Case LCase(l_strToken)
                Case "widgets"
                    l_booOmitLine = True
                    l_lngPos = InStr(l_strArgs, "(")
                    If (l_lngPos > 1) Then
                        l_strName = Trim(Left(l_strArgs, l_lngPos - 1))
                        l_strType = Trim(Mid(l_strArgs, l_lngPos + 1))
                        l_strType = Trim(Left(l_strType, Len(l_strType) - 1))
                        
                        l_lngWidgetGroups = l_lngWidgetGroups + 1
                        ReDim Preserve l_wgWidgetGroups(0 To l_lngWidgetGroups - 1)
                        With l_wgWidgetGroups(l_lngWidgetGroups - 1)
                            .Variable = l_strName
                            .Values = Split(l_strType, " ")
                        End With
                        
                        l_strIndent = String((l_lngWidget + l_lngWidgetGroups + 1) * 4, " ")
                    
                    End If
                Case "widget"
                    l_lngWidgetCount = l_lngWidgetCount + 1
                    l_strConstructor = l_strConstructor & l_strBlock
                    l_strBlock = ""
                    l_booOmitLine = True
                    l_lngPos = InStr(l_strArgs, "(")
                    If (l_lngPos > 1) Then
                        l_lngWidget = l_lngWidget + 1
                        l_strName = Trim(Left(l_strArgs, l_lngPos - 1))
                        l_strType = Trim(Mid(l_strArgs, l_lngPos + 1))
                        l_strType = Trim(Left(l_strType, Len(l_strType) - 1))
                        
                        If l_lngWidgetGroups = 0 Then
                            l_strOutput = l_strOutput & "Public " & l_strName & Lang.LineTerminator
                        Else
                            With l_wgWidgetGroups(l_lngWidgetGroups - 1)
                                For l_lngNames = LBound(.Values) To UBound(.Values)
                                    l_strOutput = l_strOutput & "Public " & _
                                        Replace(l_strName, .Variable, .Values(l_lngNames)) & _
                                        Lang.LineTerminator
                                Next l_lngNames
                            End With
                        End If
                        
                        l_strName = """" + l_strName + """"
                        l_strType = """" + l_strType + """"
                        l_strVar = GetLevelVar(l_lngWidget)
                        
                        If (l_lngWidget = 1) Then
                            l_strBlock = l_strBlock & _
                                l_strIndent & Lang.GenerateAssignment("Name", l_strName, False) & Lang.LineTerminator
                        End If
                        
                        l_strBlock = l_strBlock & _
                            l_strIndent & Lang.GenerateAssignment(l_strVar, _
                                Lang.GenerateFunctionCall("CreateWidget", _
                                    Array(l_strType, l_strName)), _
                                True) & Lang.LineTerminator
                        l_strBlock = l_strBlock & _
                            l_strIndent & "With " & l_strVar & Lang.LineTerminator
                        
                        l_strIndent = String((l_lngWidget + l_lngWidgetGroups + 1) * 4, " ")
                        
                        ' Assign the eventhandler at the start of any given constructor; set it to the form by default
                        l_strBlock = l_strBlock & _
                            l_strIndent & Lang.GenerateAssignment(".EventHandler", "Me", True) & Lang.LineTerminator
                    
                    End If
                Case "end"
                    If Trim(LCase(l_strArgs)) = "widget" Then
                        l_booOmitLine = True
                        
                        If (l_lngWidget = 1) Then
                            ' Call Container.Sync at the end of a top-level constructor
                            l_strBlock = l_strBlock & _
                                l_strIndent & ".Container.Sync" & Lang.LineTerminator
                        End If
                                               
                        l_lngWidget = l_lngWidget - 1
                        l_strIndent = String((l_lngWidget + l_lngWidgetGroups + 1) * 4, " ")
                        
                        l_strBlock = l_strBlock & _
                            l_strIndent & "End With" & Lang.LineTerminator
                        If (l_lngWidget > 0) Then
                            l_strBlock = l_strBlock & _
                                l_strIndent & ".Children.Add " & l_strVar & Lang.LineTerminator
                        End If
                        
                        If l_lngWidgetGroups = 0 Then
                            l_strConstructor = l_strConstructor & _
                                l_strBlock
                        Else
                            With l_wgWidgetGroups(l_lngWidgetGroups - 1)
                                l_strConstructor = l_strConstructor & _
                                    Left(l_strIndent, Len(l_strIndent) - 4) & "' Widgets " & .Variable & Lang.LineTerminator
                                For l_lngNames = LBound(.Values) To UBound(.Values)
                                    l_strConstructor = l_strConstructor & _
                                        Replace(l_strBlock, .Variable, .Values(l_lngNames))
                                Next l_lngNames
                                l_strConstructor = l_strConstructor & _
                                    Left(l_strIndent, Len(l_strIndent) - 4) & "' End Widgets" & Lang.LineTerminator
                            End With
                        End If
                        l_strBlock = ""
                        
                        l_strVar = GetLevelVar(l_lngWidget)
                    ElseIf (Trim(LCase(l_strArgs))) = "widgets" Then
                        l_booOmitLine = True
                        
                        l_lngWidgetGroups = l_lngWidgetGroups - 1
                        If l_lngWidgetGroups > 0 Then
                            ReDim Preserve l_wgWidgetGroups(0 To l_lngWidgetGroups - 1)
                        Else
                            Erase l_wgWidgetGroups
                        End If
                        l_strIndent = String((l_lngWidget + l_lngWidgetGroups + 1) * 4, " ")
                    End If
                End Select
            Else
            End If
        End If
        If l_booOmitLine Then
        Else
            If l_lngWidget > 0 Then
                l_strBlock = l_strBlock & l_strIndent & l_strLine & vbCrLf
            Else
                l_strScript = l_strScript & l_strLines(l_lngLine) & vbCrLf
            End If
        End If
    Next l_lngLine
    If (l_lngWidgetCount = 0) Then
        ParseFormScript = Text
    Else
        l_strConstructor = l_strConstructor & _
            "On Error Goto 0" & Lang.LineTerminator & _
            l_strIndent & "Form_Initialize"
        l_strOutput = l_strOutput & _
            l_strScript & Lang.LineTerminator & _
            Lang.GenerateClassMethod("Class_Initialize", "", l_strConstructor) & Lang.LineTerminator
        ParseFormScript = l_strOutput
    End If
End Function

Function LoadForm(ByVal Path As String) As Object
On Error Resume Next
Dim l_strName As String
Dim l_strScript As String
Dim l_strChosenName As String
    If Engine.FileSystem.FileExists(Path) = False Then
        Engine.CriticalError "UIKit", "LoadForm", "File not found: " & Path
    End If
    l_strScript = Engine.FileSystem.File(Path).Text
    l_strScript = ParseFormScript(l_strScript)
    If DebugForms Then
        Engine.FileSystem.CreateFile(Path & ".f2script").Text = l_strScript
    End If
    Engine.ContextLevelAdd Path
    Set LoadForm = Engine.GenerateScriptObject(l_strScript)
    l_strChosenName = l_strName
    l_strChosenName = CStr(LoadForm.Name)
    Forms.Remove l_strChosenName
    Forms.Add LoadForm, l_strChosenName
    Set Engine.IObjThis = LoadForm
    Engine.RunCode Engine.ScriptEngine.Language.GenerateAssignment(l_strChosenName, "IObjThis", True)
    Set Engine.IObjThis = Nothing
    Engine.ContextLevelRemove
End Function

Function DuplicateForm(ByVal Form As Object) As Object
On Error Resume Next
    Set DuplicateForm = Engine.ScriptEngine.EvalObject(Engine.ScriptEngine.Language.GenerateInstantiation(TypeName(Form)))
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
                Case "file", "skinfile", "skin"
                    Skin.SkinFile = CStr(l_strValue)
                Case "textcolor", "highlightcolor", "tintcolor"
                    l_strChannels = Split(l_strValue, ",")
                    CallByName Skin, LCase(Trim(l_strKey)), VbLet, F2RGB(CLng(l_strChannels(0)), CLng(l_strChannels(1)), CLng(l_strChannels(2)), CLng(l_strChannels(3)))
                Case "edgeoffset"
                    Skin.EdgeOffset = CLng(l_strValue)
                Case "width"
                    Skin.Width = CLng(l_strValue)
                Case "font"
                    Set Skin.Font = Fonts(LCase(Trim(l_strValue)))
                Case Else
                End Select
            End If
        Next l_lngProps
    End If
End Sub

Sub LoadSkins(ByVal Path As String)
On Error Resume Next
Dim l_fldFolders As Fury2Folders
Dim l_filFiles As Fury2Files
Dim l_lngFiles As Long, l_lngFolders As Long
Dim l_sknSkin As Skin, l_strSkin As String
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
            Set l_sksState.Font = Fonts("Normal")
            l_sksState.TextColor = F2White
            l_sksState.HighlightColor = F2RGB(32, 96, 220, 127)
            l_sksState.SkinFile = Replace(l_strFile, ".txt", ".png")
            LoadSkinDef l_sksState, l_strFile
            Set l_wskWindow = Nothing
            Set l_wskWindow = Engine.LoadWindowSkin(l_sksState.SkinFile)
            Set l_sksState.WindowSkin = l_wskWindow
            l_sknSkin.States.Add l_sksState, LCase(Trim(Replace(Replace(l_strFile, l_strFolder & "/", ""), ".txt", "")))
        Next l_lngFiles
        Set l_filFiles = Engine.FileSystem.EnumFiles(l_strFolder + "/glyphs/", "*.png")
        For l_lngFiles = 1 To l_filFiles.Count
            l_strFile = l_filFiles.File(l_lngFiles)
            l_sknSkin.Glyphs.Add Engine.LoadImage(l_strFile, True), LCase(Trim(Replace(Replace(l_strFile, l_strFolder & "/glyphs/", ""), ".png", "")))
        Next l_lngFiles
        Skins.Add l_sknSkin, LCase(Trim(Replace(Replace(l_strFolder, Path, ""), "/", "")))
    Next
End Sub

Sub LoadFonts(ByVal Path As String)
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
    Set Buffer = Engine.Backbuffer
    Set Me.Rectangle = Buffer.Rectangle
    Set Desktop.Rectangle = Me.Rectangle.Copy
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
Dim l_sbrScrollBar As UIKit.Scrollbar
Dim l_wdgNull As UIKit.NullWidget
    Select Case LCase(Trim(WidgetType))
    Case "container"
        Set l_ctrContainer = New Container
        With l_ctrContainer
            .SetEngine Engine
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_ctrContainer
    Case "button"
        Set l_btnButton = New Button
        With l_btnButton
            .SetEngine Engine
            Set .Skin = Skins("button")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_btnButton
    Case "window"
        Set l_wndWindow = New Window
        With l_wndWindow
            .SetEngine Engine
            Set .Skin = Skins("window")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_wndWindow
    Case "textfield"
        Set l_txfTextField = New TextField
        With l_txfTextField
            .SetEngine Engine
            Set .Skin = Skins("textfield")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_txfTextField
    Case "label"
        Set l_lblLabel = New Label
        With l_lblLabel
            .SetEngine Engine
            Set .Skin = Skins("label")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_lblLabel
    Case "graphic"
        Set l_gphGraphic = New Graphic
        With l_gphGraphic
            .SetEngine Engine
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_gphGraphic
    Case "listbox"
        Set l_lbxListBox = New ListBox
        With l_lbxListBox
            .SetEngine Engine
            Set .Skin = Skins("listbox")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_lbxListBox
    Case "menu"
        Set l_mnuMenu = New Menu
        With l_mnuMenu
            .SetEngine Engine
            Set .Skin = Skins("menu")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
        End With
        Set CreateWidget = l_mnuMenu
    Case "scrollbar"
        Set l_sbrScrollBar = New Scrollbar
        With l_sbrScrollBar
            .SetEngine Engine
            Set .Skin = Skins("scrollbar")
            If Len(Trim(Name)) > 0 Then
                .Name = Name
            End If
            .Refresh
        End With
        Set CreateWidget = l_sbrScrollBar
    Case Else
        Set l_wdgNull = New NullWidget
        With l_wdgNull
            .SetEngine Engine
        End With
        Set CreateWidget = l_wdgNull
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

Public Sub Plugin_Load()
On Error Resume Next
    Engine.HookEvents Me
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
    Set Buffer = RHS.Backbuffer
    Set Me.Rectangle = Buffer.Rectangle
    Set Desktop.Rectangle = Me.Rectangle.Copy
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
    If Desktop.Visible Then
        Desktop.Widget.KeyDown Nothing, KeyCode, Shift
    End If
End Sub

Private Sub Fury2Plugin_KeyPress(KeyAscii As Long)
On Error Resume Next
    If Desktop.Visible Then
        Desktop.Widget.KeyPress Nothing, KeyAscii
    End If
End Sub

Private Sub Fury2Plugin_KeyUp(KeyCode As Long, ByVal Shift As Long)
On Error Resume Next
    If Desktop.Visible Then
        Desktop.Widget.KeyUp Nothing, KeyCode, Shift
    End If
End Sub

Private Sub Fury2Plugin_MapLoad(Filename As String)
On Error Resume Next
End Sub

Private Sub Fury2Plugin_MouseDown(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If Desktop.Visible Then
        Desktop.Widget.MouseDown Nothing, Button, Shift, X - Me.Rectangle.Left, Y - Me.Rectangle.Top
        If Desktop.Rectangle.PointInside(X - Me.Rectangle.Left, Y - Me.Rectangle.Top) Then
            X = -32767
            Y = -32767
        End If
    End If
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If Desktop.Visible Then
        Desktop.Widget.MouseMove Nothing, Button, Shift, X - Me.Rectangle.Left, Y - Me.Rectangle.Top
        If Desktop.Rectangle.PointInside(X - Me.Rectangle.Left, Y - Me.Rectangle.Top) Then
            If Button = 0 Then
                X = -32767
                Y = -32767
            End If
        End If
    End If
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, ByRef X As Long, ByRef Y As Long)
On Error Resume Next
    If Desktop.Visible Then
        Desktop.Widget.MouseUp Nothing, Button, Shift, X - Me.Rectangle.Left, Y - Me.Rectangle.Top
        If Desktop.Rectangle.PointInside(X - Me.Rectangle.Left, Y - Me.Rectangle.Top) Then
            X = -32767
            Y = -32767
        End If
    End If
End Sub

Private Sub Fury2Plugin_OutputPluginChanged()

End Sub

Private Sub Fury2Plugin_Redraw()
On Error Resume Next
    If Buffer Is Nothing Then Exit Sub
    Desktop.Widget.Redraw Nothing, Buffer, 0, 0, 1#
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

