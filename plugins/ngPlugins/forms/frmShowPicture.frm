VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Begin VB.Form frmShowPicture 
   BorderStyle     =   0  'None
   ClientHeight    =   7335
   ClientLeft      =   0
   ClientTop       =   15
   ClientWidth     =   9195
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmShowPicture.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   489
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   613
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picViewport 
      AutoRedraw      =   -1  'True
      Height          =   7290
      Left            =   0
      ScaleHeight     =   482
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   464
      TabIndex        =   1
      Top             =   0
      Width           =   7020
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   -15
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin ngPlugins.ObjectInspector insProperties 
      Height          =   7230
      Left            =   6165
      TabIndex        =   0
      Top             =   60
      Width           =   3375
      _ExtentX        =   5953
      _ExtentY        =   12753
   End
End
Attribute VB_Name = "frmShowPicture"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
   
Implements iExtendedForm
Implements iEditingCommands
Implements iCustomMenus
Implements iDocument

Public Design As ShowPictureDesign

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private m_lngSelectedPicture As Long
Private m_imgViewport As Fury2Image

Private m_booDraggingPicture As Boolean
Private m_rctPictureStart As Fury2Rect

Private m_lngMouseX As Long, m_lngMouseY As Long
Private m_lngStartMouseX As Long, m_lngStartMouseY As Long

Private m_colUndo As New Engine.Fury2Collection
Private m_colRedo As New Engine.Fury2Collection

Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Private m_booVisible As Boolean

Private m_tbhHandler As iToolbarHandler

'Public Sub CutPicture()
'On Error Resume Next
'    CopyPicture
'    DeletePicture
'End Sub

Public Sub DeletePicture()
On Error Resume Next
    Design.Pictures.Remove m_lngSelectedPicture
    RefreshAll
End Sub

Public Sub RefreshMouse()
On Error Resume Next
    Editor.SetLocation CStr(m_lngMouseX) & ", " & CStr(m_lngMouseY)
End Sub

Public Function PictureFromPoint(ByVal X As Long, ByVal Y As Long) As Picture
On Error Resume Next
Dim l_lngPictures As Long
Dim l_picPicture As Picture
    For l_lngPictures = Design.Pictures.Count To 1 Step -1
        Set l_picPicture = Design.Pictures(l_lngPictures)
        If l_picPicture.Rectangle.PointInside(X, Y) Then
            Set PictureFromPoint = l_picPicture
            Exit For
        End If
    Next l_lngPictures
End Function

Public Sub ExportImage()
On Error Resume Next
Dim l_fpgPlugin As iFileTypePlugin
Dim l_frmImage As frmImage
Dim l_imgImage As Fury2Image
Dim l_rctImage As Fury2Rect
    Set l_fpgPlugin = Editor.FindFileTypePlugin("Image")
    Set l_rctImage = Design.Rectangle
    Set l_imgImage = F2Image(l_rctImage.Right, l_rctImage.Bottom)
    l_imgImage.Clear 0
    Design.Render l_imgImage
    l_imgImage.FillChannel l_imgImage.Rectangle, 3, 255
    Set l_frmImage = l_fpgPlugin.CreateNew()
    l_frmImage.SetImage l_imgImage
    Editor.NewDocument l_frmImage
End Sub

Public Sub ExportScript()
On Error Resume Next
Dim l_fpgPlugin As iFileTypePlugin
Dim l_frmScript As frmScriptFile
    Set l_fpgPlugin = Editor.FindFileTypePlugin("Script")
    Set l_frmScript = l_fpgPlugin.CreateNew()
    l_frmScript.scScript.Text = Design.Export()
    Editor.NewDocument l_frmScript
End Sub

Public Sub InsertPicture(Optional ByRef Filename As String = "")
On Error Resume Next
Dim l_strFilename As String
Dim l_picNew As New Picture
    l_strFilename = Filename
    If l_strFilename = "" Then
        l_strFilename = SelectFiles("Images|" + libGraphics.SupportedGraphicsFormats, "Select Image...", False)
        If Trim(l_strFilename) <> "" Then
            If InStr(l_strFilename, Engine.Engine.FileSystem.Root) Then
                l_strFilename = Replace(l_strFilename, Engine.Engine.FileSystem.Root, "/")
                l_strFilename = Replace(l_strFilename, "\", "/")
            Else
                Select Case MsgBox("The file you have selected is not inside your game folder. The engine will not be able to load it. Continue?", vbInformation Or vbYesNo, "Warning")
                Case vbNo
                    Exit Sub
                End Select
            End If
        Else
            Exit Sub
        End If
    End If
    With l_picNew
        .Filename = l_strFilename
        Set .Rectangle = .Image.Rectangle
        Set .SourceRectangle = .Image.Rectangle
        .Opacity = 1
    End With
    Design.Pictures.Add l_picNew
    m_lngSelectedPicture = Design.Pictures.Count
    RefreshAll
End Sub

Public Property Get SelectedPicture() As Picture
On Error Resume Next
    Set SelectedPicture = Design.Pictures(m_lngSelectedPicture)
End Property

Public Property Get ActiveType() As String
On Error Resume Next
    Select Case LCase(Trim(Me.ActiveControl.Name))
    Case "picViewport"
        ActiveType = "Pictures"
    Case Else
    End Select
End Property

Public Sub LockRedraw(Window As Long, State As Boolean)
On Error Resume Next
    SendMessage Window, WM_SETREDRAW, Abs(CLng(State)), 0
End Sub

Public Sub AllocateBuffers()
On Error Resume Next
    Set m_imgViewport = F2DIBSection(1, 1, picViewport.hdc)
    SelectObject picViewport.hdc, m_imgViewport.DIBHandle
End Sub

Public Sub DeallocateBuffers()
On Error Resume Next
    Set m_imgViewport = Nothing
End Sub

Public Sub Redraw()
On Error Resume Next
    If Design Is Nothing Then Exit Sub
    If m_imgViewport Is Nothing Then Exit Sub
    If (m_imgViewport.Width <> picViewport.ScaleWidth) Or (m_imgViewport.Height <> picViewport.ScaleHeight) Then
        Set m_imgViewport = F2DIBSection(picViewport.ScaleWidth, picViewport.ScaleHeight, picViewport.hdc)
        SelectObject picViewport.hdc, m_imgViewport.DIBHandle
    End If
    m_imgViewport.Clear
    Design.Render m_imgViewport
    If m_lngSelectedPicture > 0 Then
        m_imgViewport.Box SelectedPicture.Rectangle, F2White
        m_imgViewport.Box SelectedPicture.Rectangle.Copy().Adjust(-1, -1), F2Black
    End If
    picViewport.Refresh
End Sub

Private Sub FixRectCoords(ByRef X1 As Long, ByRef Y1 As Long, ByRef X2 As Long, ByRef Y2 As Long)
On Error Resume Next
Dim Temp As Long
    If X1 > X2 Then
        Temp = X1
        X1 = X2
        X2 = Temp
    End If
    If Y1 > Y2 Then
        Temp = Y1
        Y1 = Y2
        Y2 = Temp
    End If
End Sub

Private Function SmartApply(Obj As iUndoEntry) As Boolean
On Error Resume Next
Dim l_ouUndo As cObjectUndoEntry
Dim l_prUndo As cPropertyUndoEntry
    SmartApply = Obj.Apply()
    If TypeOf Obj Is cTileUndoEntry Then
    ElseIf TypeOf Obj Is cObjectUndoEntry Then
    ElseIf TypeOf Obj Is cPropertyUndoEntry Then
    ElseIf TypeOf Obj Is cMultiUndoEntry Then
    End If
End Function

Public Function Redo() As Boolean
On Error Resume Next
Dim l_undRedo As iUndoEntry
Dim l_undUndo As iUndoEntry
    If m_colRedo.Count > 0 Then
        BeginProcess "Preparing to Redo..."
        Set l_undRedo = m_colRedo.Item(1)
        Set l_undUndo = l_undRedo.CreateReverse()
        m_colRedo.Remove 1
        m_colUndo.Add l_undUndo
        UpdateProcess 0, "Performing Redo..."
        Redo = SmartApply(l_undRedo)
        EndProcess
        Editor.ActionUpdate
    End If
End Function

Public Function Undo() As Boolean
On Error Resume Next
Dim l_undRedo As iUndoEntry
Dim l_undUndo As iUndoEntry
    If m_colUndo.Count > 0 Then
        BeginProcess "Preparing to Undo..."
        Set l_undUndo = m_colUndo.Item(m_colUndo.Count)
        Set l_undRedo = l_undUndo.CreateReverse()
        m_colUndo.Remove m_colUndo.Count
        m_colRedo.Add l_undRedo, , 1
        UpdateProcess 0, "Performing Undo..."
        Undo = SmartApply(l_undUndo)
        EndProcess
        Editor.ActionUpdate
    End If
End Function

Public Sub MultiPropertyUndoPush(ByRef Obj As Object, ByRef Methods As Variant, ByRef Values As Variant)
On Error Resume Next
Dim l_undUndo As cMultiUndoEntry
Dim l_undProp As cPropertyUndoEntry
Dim l_lngIndex As Long
    If Obj Is Nothing Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cMultiUndoEntry
    With l_undUndo.Entries
        For l_lngIndex = LBound(Methods) To UBound(Methods)
            Set l_undProp = New cPropertyUndoEntry
            With l_undProp
                Set .Object = Obj
                .MethodName = CStr(Methods(l_lngIndex))
                If (VarType(Values(l_lngIndex)) And vbObject) = vbObject Then
                    Set .Value = Values(l_lngIndex)
                Else
                    .Value = Values(l_lngIndex)
                End If
            End With
            .Add l_undProp
        Next l_lngIndex
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub PropertyUndoPush(ByRef Obj As Object, ByRef Method As String, ByRef Value As Variant)
On Error Resume Next
Dim l_undUndo As cPropertyUndoEntry
    If Obj Is Nothing Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cPropertyUndoEntry
    With l_undUndo
        Set .Object = Obj
        .MethodName = Method
        If (VarType(Value) And vbObject) = vbObject Then
            Set .Value = Value
        Else
            .Value = Value
        End If
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub ObjectUndoPush(ByRef Container As Object, ByRef Value As Object, ByVal Index As Long, ByVal Operation As ObjectUndoOperations)
On Error Resume Next
Dim l_undUndo As cObjectUndoEntry
    If Container Is Nothing Then Exit Sub
    If Value Is Nothing Then Exit Sub
    If Index < 1 Then Exit Sub
    BeginProcess "Storing Undo Data..."
    Set l_undUndo = New cObjectUndoEntry
    With l_undUndo
        Set .Container = Container
        Set .Value = Value
        .Index = Index
        .Operation = Operation
    End With
    m_colUndo.Add l_undUndo
    m_colRedo.Clear
    If m_colUndo.Count > c_lngUndoStackLength Then
        m_colUndo.Remove 1
    End If
    EndProcess
End Sub

Public Sub Cleanup()
On Error Resume Next
    Set m_fpgPlugin = Nothing
End Sub

Public Sub RefreshAll()
On Error Resume Next
    Redraw
    RefreshInspector
End Sub

Public Sub RefreshInspector()
On Error Resume Next
    insProperties.Inspect SelectedPicture, "Picture #" & m_lngSelectedPicture, True
End Sub

Private Function ClipboardContainsFormat(Format As ShowPictureDesignerClipboardFormats) As Boolean
On Error Resume Next
Dim l_objPlugin As ShowPictureDesigner
    Set l_objPlugin = m_fpgPlugin
    With l_objPlugin.CustomClipboard
        .GetCurrentFormats Me.hwnd
        ClipboardContainsFormat = .HasCurrentFormat(l_objPlugin.ClipboardFormat(Format))
    End With
End Function

Private Function ClipboardFormat(Format As ShowPictureDesignerClipboardFormats) As Long
On Error Resume Next
Dim l_objPlugin As ShowPictureDesigner
    Set l_objPlugin = m_fpgPlugin
    ClipboardFormat = l_objPlugin.ClipboardFormat(Format)
End Function

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Private Function CustomClipboard() As cCustomClipboard
On Error Resume Next
Dim l_objPlugin As ShowPictureDesigner
    Set l_objPlugin = m_fpgPlugin
    Set CustomClipboard = l_objPlugin.CustomClipboard
End Function

Private Function Editor() As Object
On Error Resume Next
Dim l_objPlugin As ShowPictureDesigner
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Function

Public Sub Form_Activate()
On Error GoTo 0
    AllocateBuffers
    Form_Resize
    Redraw
End Sub

Private Sub Form_Deactivate()
On Error Resume Next
    DeallocateBuffers
End Sub

Private Sub Form_Load()
On Error Resume Next
    Form_Activate
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    Cleanup
End Sub

Private Sub Form_Resize()
On Error Resume Next
    If WindowState = 2 And m_booVisible = False Then
        m_booVisible = True
        RefreshAll
    End If
    picViewport.Move 2, 2, Me.ScaleWidth - 6 - insProperties.Width, Me.ScaleHeight - 4
    insProperties.Move picViewport.Left + picViewport.Width + 2, picViewport.Top, Me.ScaleWidth - 6 - picViewport.Width, picViewport.Height
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DestroyMenu "ExportImage"
        .DestroyMenu "ExportScript"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DefineMenu "Export Image", "ExportImage"
        .DefineMenu "Export Script", "ExportScript"
    End With
End Sub

Private Sub iCustomMenus_MenuClick(key As String)
On Error Resume Next
Dim l_strParameter As String, l_lngParameter As Long
Dim CommandName As String
    CommandName = key
    If InStr(CommandName, "(") Then
        l_strParameter = Trim(Mid(CommandName, InStr(CommandName, "(") + 1))
        If Right(l_strParameter, 1) = ")" Then l_strParameter = Trim(Left(l_strParameter, Len(l_strParameter) - 1))
        CommandName = Left(CommandName, InStr(CommandName, "(") - 1)
        CommandName = Replace(CommandName, ":", "_")
        If Left(l_strParameter, 1) = """" Then
            ' String
            l_strParameter = CStr(Mid(l_strParameter, 2, Len(l_strParameter) - 2))
            CallByName Me, CommandName, VbMethod, l_strParameter
        Else
            ' Integer
            l_lngParameter = CLng(l_strParameter)
            CallByName Me, CommandName, VbMethod, l_lngParameter
        End If
    Else
        CommandName = Replace(CommandName, ":", "_")
        CallByName Me, CommandName, VbMethod
    End If
    Err.Clear
End Sub

Private Property Get iDocument_CanSave() As Boolean
On Error Resume Next
    iDocument_CanSave = True
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = m_strFilename
End Property

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = m_fpgPlugin
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
    Set m_fpgPlugin = RHS
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
Dim l_vfFile As VirtualFile
    Kill Filename
    Err.Clear
    Set l_vfFile = F2File()
    SaveToFile Design, l_vfFile
    l_vfFile.SaveFile Filename
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
    Redraw
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "ShowPicture Designer"
End Property

Private Sub iEditingCommands_CanCopy(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Pictures"
        If m_lngSelectedPicture > 0 Then
            'NewValue = True
        End If
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanCut(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Pictures"
        If m_lngSelectedPicture > 0 Then
            'NewValue = True
        End If
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanDelete(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Pictures"
        If m_lngSelectedPicture > 0 Then
            NewValue = True
        End If
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanPaste(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanRedo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
        NewValue = (m_colRedo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_CanSelectAll(NewValue As Boolean)
End Sub

Private Sub iEditingCommands_CanSelectNone(NewValue As Boolean)
End Sub

Private Sub iEditingCommands_CanUndo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
        NewValue = (m_colUndo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_Copy()
On Error Resume Next
    Select Case ActiveType
    Case "Pictures"
        'CopyPicture
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Cut()
On Error Resume Next
    Select Case ActiveType
    Case "Pictures"
        'CutPicture
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Delete()
On Error Resume Next
    Select Case ActiveType
    Case "Pictures"
        DeletePicture
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Paste()
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Redo()
On Error Resume Next
'    Me.Redo
    Redraw
End Sub

Private Sub iEditingCommands_SelectAll()
End Sub

Private Sub iEditingCommands_SelectNone()
End Sub

Private Sub iEditingCommands_Undo()
On Error Resume Next
'    Me.Undo
    Redraw
End Sub

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Private Sub insOverview_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    Redraw
End Sub

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2spd", GetTitle(Name))
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

Function ListContext() As Variant
    ListContext = Menus(MenuString("Insert &New", , , "NEW"), _
        MenuString("-"), _
        MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete))
End Function

Private Sub insProperties_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
    Redraw
End Sub

Private Sub picViewport_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_lngStartMouseX = X
    m_lngStartMouseY = Y
    m_lngSelectedPicture = Design.Pictures.Find(PictureFromPoint(X, Y))
    Redraw
    RefreshInspector
    Editor.ActionUpdate
    If Button = 2 Then
        Select Case QuickShowMenu(picViewport, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
        ListContext(), _
        frmIcons.ilContextMenus)
        Case 1
            InsertPicture
        Case 3
            iEditingCommands_Cut
        Case 4
            iEditingCommands_Copy
        Case 5
            iEditingCommands_Paste
        Case 6
            iEditingCommands_Delete
        Case Else
        End Select
    ElseIf Button = 1 Then
        m_booDraggingPicture = True
        Set m_rctPictureStart = SelectedPicture.Rectangle.Copy
    End If
End Sub

Private Sub picViewport_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_lngMouseX = X
    m_lngMouseY = Y
    RefreshMouse
    If Button = 1 Then
        If m_booDraggingPicture Then
            Set SelectedPicture.Rectangle = m_rctPictureStart.Copy.Translate(X - m_lngStartMouseX, Y - m_lngStartMouseY)
            Redraw
        End If
    End If
End Sub

Private Sub picViewport_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_booDraggingPicture = False
    RefreshInspector
End Sub

Private Sub picViewport_Resize()
    Redraw
End Sub
