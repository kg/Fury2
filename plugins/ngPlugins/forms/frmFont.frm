VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#12.11#0"; "ngUI.ocx"
Begin VB.Form frmFont 
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
   Icon            =   "frmFont.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   489
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   613
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picPreview 
      Height          =   1485
      Left            =   0
      ScaleHeight     =   95
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   143
      TabIndex        =   1
      Top             =   0
      Visible         =   0   'False
      Width           =   2205
   End
   Begin ngPlugins.ObjectInspector insOverview 
      Height          =   3045
      Left            =   855
      TabIndex        =   0
      Top             =   1170
      Visible         =   0   'False
      Width           =   1710
      _ExtentX        =   3016
      _ExtentY        =   5371
   End
   Begin VB.PictureBox picCharacters 
      BackColor       =   &H80000014&
      BorderStyle     =   0  'None
      Height          =   5580
      Left            =   2850
      ScaleHeight     =   372
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   393
      TabIndex        =   2
      Top             =   1020
      Visible         =   0   'False
      Width           =   5895
      Begin VB.PictureBox picImage 
         Height          =   1485
         Left            =   1905
         ScaleHeight     =   95
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   143
         TabIndex        =   6
         Top             =   1740
         Visible         =   0   'False
         Width           =   2205
      End
      Begin cFScroll.FlatScrollBar hsCharacters 
         Height          =   210
         Left            =   30
         TabIndex        =   4
         Top             =   1500
         Width           =   5835
         _ExtentX        =   10292
         _ExtentY        =   370
         Max             =   100
         LargeChange     =   64
         SmallChange     =   16
         Style           =   -1
      End
      Begin VB.PictureBox picCharacterList 
         AutoRedraw      =   -1  'True
         Height          =   1500
         Left            =   30
         ScaleHeight     =   96
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   385
         TabIndex        =   3
         Top             =   15
         Width           =   5835
      End
      Begin ngPlugins.ObjectInspector insCharacter 
         Height          =   3435
         Left            =   780
         TabIndex        =   5
         Top             =   2100
         Visible         =   0   'False
         Width           =   5745
         _ExtentX        =   10134
         _ExtentY        =   6059
      End
      Begin ngUI.ngTabStrip tsCharacter 
         Height          =   2145
         Left            =   225
         Top             =   2145
         Width           =   1965
         _ExtentX        =   3466
         _ExtentY        =   3784
      End
   End
   Begin ngUI.ngTabStrip tsViews 
      Height          =   375
      Left            =   2595
      Top             =   405
      Width           =   720
      _ExtentX        =   1270
      _ExtentY        =   661
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   -15
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmFont"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
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
Private Declare Function LBItemFromPt Lib "comctl32.dll" _
  (ByVal hwnd As Long, ByVal ptx As Long, ByVal pty As Long, ByVal bAutoScroll As Long) As Long
   
Implements iExtendedForm
Implements iEditingCommands
Implements iCustomMenus
Implements iDocument

Private Enum FontEditorViews
    View_Overview
    View_Characters
    View_Preview
End Enum

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private m_imgPreview As Fury2Image
Private m_imgImage As Fury2Image
Private m_imgCharacterList As Fury2Image

Private m_lngSelectedCharacter As Long

Private m_colUndo As New Engine.Fury2Collection
Private m_colRedo As New Engine.Fury2Collection

Private m_fntFont As Fury2Font
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Private m_booVisible As Boolean

Private WithEvents m_tbrToolbar As ngToolbar
Attribute m_tbrToolbar.VB_VarHelpID = -1
Private m_lngCurrentView As FontEditorViews

Public Property Get SelectedCharacter() As CharacterProxy
On Error Resume Next
Dim l_prxProxy As CharacterProxy
    Set l_prxProxy = New CharacterProxy
    Set l_prxProxy.Font = m_fntFont
    l_prxProxy.Character = m_lngSelectedCharacter
    Set SelectedCharacter = l_prxProxy
End Property

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iDocument_DocumentIcon = Editor.LoadResources("ng").ItemData("icons\font.png")
End Property

Private Sub picImage_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    Editor.ActionUpdate
    Select Case QuickShowMenu2(picImage, X, Y, _
        Menus(MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
        frmIcons.ilContextMenus)
    Case 1
        CopyImage
    Case 2
        PasteImage
    Case Else
    End Select
End Sub

Private Sub picImage_Paint()
On Error Resume Next
Dim l_sngXRatio As Single, l_sngYRatio As Single, l_sngRatio As Single
    l_sngXRatio = picImage.ScaleWidth / m_imgImage.Width
    l_sngYRatio = picImage.ScaleHeight / m_imgImage.Height
    If (l_sngXRatio < l_sngYRatio) Then l_sngRatio = l_sngXRatio Else l_sngRatio = l_sngYRatio
    DrawImageToDC picImage.hdc, F2Rect(0, 0, m_imgImage.Width * l_sngRatio, m_imgImage.Height * l_sngRatio, False), m_imgImage.Rectangle, m_imgImage
    picImage.Line (m_imgImage.Width * l_sngRatio, 0)-(picImage.ScaleWidth, picImage.ScaleHeight), picImage.BackColor, BF
    picImage.Line (0, m_imgImage.Height * l_sngRatio)-(picImage.ScaleWidth, picImage.ScaleHeight), picImage.BackColor, BF
End Sub

Private Sub picImage_Resize()
On Error Resume Next
    RedrawImage
End Sub

Private Sub tsCharacter_Resize()
On Error Resume Next
    insCharacter.Move (2) + tsCharacter.Left, tsCharacter.Top + tsCharacter.IdealHeight + 1, tsCharacter.Width - 4, tsCharacter.Height - (tsCharacter.IdealHeight + 3)
    picImage.Move insCharacter.Left, insCharacter.Top, insCharacter.Width, insCharacter.Height
End Sub

Private Sub hsCharacters_Change()
    RedrawCharacterList
End Sub

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Public Sub Recolor()
On Error Resume Next
    m_fntFont.Recolor SelectColor(F2White)
    Redraw
End Sub

Public Sub AddDropShadow()
On Error Resume Next
    m_fntFont.AddDropShadow SelectColor(F2Black), 1, 1, 1
    Redraw
End Sub

Public Sub AddOutline()
On Error Resume Next
    m_fntFont.AddOutline SelectColor(F2Black), 1
    Redraw
End Sub

Public Property Get ActiveType() As String
On Error Resume Next
    Select Case LCase(Trim(Me.ActiveControl.Name))
    Case "piccharacters", "piccharacterlist", "inscharacter", "tscharacter"
        ActiveType = "Characters"
    Case "picimage"
        ActiveType = "Image"
    Case Else
    End Select
End Property

Public Sub LockRedraw(Window As Long, State As Boolean)
On Error Resume Next
    SendMessage Window, WM_SETREDRAW, Abs(CLng(State)), 0
End Sub

Public Property Get Font_() As Fury2Font
    Set Font = m_fntFont
End Property

Public Sub AllocateBuffers()
On Error Resume Next
    Set m_imgPreview = F2Image(1, 1)
    Set m_imgImage = F2Image(1, 1)
End Sub

Public Sub DeallocateBuffers()
On Error Resume Next
    Set m_imgPreview = Nothing
    Set m_imgImage = Nothing
End Sub

Public Sub Redraw()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Inspect m_fntFont, "Font", True
    Case View_Characters
        RedrawCharacters
    Case View_Preview
        RedrawPreview
    Case Else
    End Select
End Sub

Public Sub RedrawCharacters()
On Error Resume Next
    RedrawCharacterList
    RedrawSelectedCharacter
End Sub

Public Sub RedrawSelectedCharacter()
On Error Resume Next
    Select Case LCase(Trim(tsCharacter.SelectedTab.Text))
    Case "properties"
        insCharacter.ClearStack
        insCharacter.AddToStack m_fntFont, "Font"
        insCharacter.Inspect SelectedCharacter, "Character " & m_lngSelectedCharacter
    Case "image"
        RedrawImage
    Case Else
    End Select
End Sub

Public Sub RedrawCharacterList()
On Error Resume Next
Dim l_imgCharacter As Fury2Image
Dim l_lngCharacter As Long
Dim l_lngX As Long
Dim l_lngWidth As Long
Dim l_rctCharacter As Fury2Rect
    m_imgCharacterList.Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
    l_lngX = -hsCharacters.Value
    For l_lngCharacter = 1 To m_fntFont.CharacterCount
        Set l_imgCharacter = m_fntFont.Character(l_lngCharacter)
        If m_lngSelectedCharacter = l_lngCharacter Then
            m_imgCharacterList.Fill F2Rect(l_lngX, 0, ClipValue(l_imgCharacter.Width, 6, 999), picCharacterList.ScaleHeight, False), SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue)
        End If
'        m_imgCharacterList.Blit F2Rect(l_lngX, m_fntFont.CharacterYOffset(l_lngCharacter) + (m_fntFont.Height - l_imgCharacter.Height), l_imgCharacter.Width, l_imgCharacter.Height, False), , l_imgCharacter, , BlitMode_Normal, SetAlpha(SwapChannels(IIf(m_lngSelectedCharacter = l_lngCharacter, GetSystemColor(SystemColor_Highlight_Text), GetSystemColor(SystemColor_Button_Text)), Red, Blue), 255)
        m_imgCharacterList.Blit F2Rect(l_lngX, m_fntFont.CharacterYOffset(l_lngCharacter) + (m_fntFont.Height - l_imgCharacter.Height), l_imgCharacter.Width, l_imgCharacter.Height, False), , l_imgCharacter, , BlitMode_Font_SourceAlpha, SetAlpha(SwapChannels(IIf(m_lngSelectedCharacter = l_lngCharacter, GetSystemColor(SystemColor_Highlight_Text), GetSystemColor(SystemColor_Button_Text)), Red, Blue), 255)
        l_lngX = l_lngX + ClipValue(l_imgCharacter.Width, 6, 999) + 1
        l_lngWidth = l_lngWidth + ClipValue(l_imgCharacter.Width, 6, 999) + 1
    Next l_lngCharacter
    picCharacterList.Refresh
    hsCharacters.Max = l_lngWidth - picCharacterList.ScaleWidth
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

Public Sub InitViews()
On Error Resume Next
    tsViews.Tabs.AddNew "Overview", "t" & CStr(View_Overview)
    tsViews.Tabs.AddNew "Characters", "t" & CStr(View_Characters)
    tsViews.Tabs.AddNew "Preview", "t" & CStr(View_Preview)
    tsCharacter.Tabs.AddNew "Properties"
    tsCharacter.Tabs.AddNew "Image"
End Sub

Public Sub RefreshAll()
On Error Resume Next
    Redraw
    ViewChanged
End Sub

Public Sub ViewChanged()
On Error Resume Next
Dim l_objObject As Object
    Screen.MousePointer = 11
    insOverview.Visible = False
    picPreview.Visible = False
    picCharacters.Visible = False
    tsViews_Resize
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Visible = True
    Case View_Preview
        picPreview.Visible = True
    Case View_Characters
        picCharacters.Visible = True
    Case Else
    End Select
    Redraw
    Screen.MousePointer = 0
End Sub

Public Sub RedrawPreview()
On Error Resume Next
    If (m_imgPreview.Width <> picPreview.ScaleWidth) Or (m_imgPreview.Height <> picPreview.ScaleHeight) Then
        m_imgPreview.Resize picPreview.ScaleWidth, picPreview.ScaleHeight
    End If
    With m_imgPreview
        .Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        m_fntFont.Draw m_imgPreview, "The quick brown fox jumped over the lazy dogs." & vbCrLf & _
            "My sphinx is made of quartz." & vbCrLf & _
            "1234567890-= !@#$%^&*()_+", m_imgPreview.Rectangle
    End With
    picPreview_Paint
End Sub

Public Sub RedrawImage()
On Error Resume Next
Dim l_imgImage As Fury2Image
    Set l_imgImage = SelectedCharacter.Image
    If (m_imgImage.Width <> l_imgImage.Width) Or (m_imgImage.Height <> l_imgImage.Height) Then
        m_imgImage.Resize l_imgImage.Width, l_imgImage.Height
    End If
    With m_imgImage
        .Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        .Blit , , l_imgImage, , BlitMode_SourceAlpha
    End With
    picImage_Paint
End Sub

Private Function ClipboardContainsFormat(Format As FontEditorClipboardFormats) As Boolean
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    With l_objPlugin.CustomClipboard
        .GetCurrentFormats Me.hwnd
        ClipboardContainsFormat = .HasCurrentFormat(l_objPlugin.ClipboardFormat(Format))
    End With
End Function

Private Function ClipboardFormat(Format As FontEditorClipboardFormats) As Long
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    ClipboardFormat = l_objPlugin.ClipboardFormat(Format)
End Function

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Private Function CustomClipboard() As cCustomClipboard
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    Set CustomClipboard = l_objPlugin.CustomClipboard
End Function

Private Function Editor() As Object
On Error Resume Next
Dim l_objPlugin As FontEditor
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Function

Private Sub tsCharacter_TabSelected(TheTab As ngUI.ngTab)
On Error Resume Next
    insCharacter.Visible = False
    picImage.Visible = False
    tsCharacter_Resize
    Select Case LCase(Trim(TheTab.Text))
    Case "properties"
        insCharacter.Visible = True
    Case "image"
        picImage.Visible = True
    Case Else
    End Select
    RedrawSelectedCharacter
End Sub

Private Sub tsViews_Resize()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Move (2) + tsViews.Left, tsViews.Top + tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - (tsViews.IdealHeight + 3)
    Case View_Preview
        picPreview.Move (2) + tsViews.Left, tsViews.Top + tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - (tsViews.IdealHeight + 3)
    Case View_Characters
        picCharacters.Move (2) + tsViews.Left, tsViews.Top + tsViews.IdealHeight + 1, tsViews.Width - 4, tsViews.Height - (tsViews.IdealHeight + 3)
    Case Else
    End Select
End Sub

Public Sub Form_Activate()
On Error Resume Next
    Set insOverview.Editor = Editor
    AllocateBuffers
    Form_Resize
    tsViews_Resize
    ViewChanged
    Redraw
End Sub

Private Sub Form_Deactivate()
On Error Resume Next
    DeallocateBuffers
End Sub

Private Sub Form_Load()
On Error Resume Next
'    vsFont.Width = GetScrollbarSize(vsFont)
    hsCharacters.Height = GetScrollbarSize(hsCharacters) + 1
    InitViews
    Form_Activate
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    Cleanup
End Sub

Private Sub Form_Resize()
On Error GoTo 0
    If WindowState = 2 And m_booVisible = False Then
        m_booVisible = True
        RefreshAll
    End If
    tsViews.Move 2, 2, Me.ScaleWidth - 4, Me.ScaleHeight - 4
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu
        .Items.Remove "Tools"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu.Items
        With .AddNew("&Tools", , "Tools", , , , , , , , 1)
            Set .ChildMenu = CreateMenu()
            With .ChildMenu.Items
                .AddNew "Add Drop Shadow", , , , , , , , BindEvent(Me, "AddDropShadow")
                .AddNew "Add Outline", , , , , , , , BindEvent(Me, "AddOutline")
                .AddNew "Recolor", , , , , , , , BindEvent(Me, "Recolor")
            End With
        End With
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
    Err.Clear
    Set l_vfFile = F2File()
    SaveToFile m_fntFont, l_vfFile
    l_vfFile.SaveFile Filename
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
    Redraw
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Font"
End Property

Private Sub iEditingCommands_CanCopy(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Characters"
        NewValue = (m_lngSelectedCharacter >= 1)
    Case "Image"
        NewValue = True
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanCut(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Characters"
        NewValue = (m_lngSelectedCharacter >= 1)
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanDelete(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Characters"
        NewValue = (m_lngSelectedCharacter >= 1)
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanPaste(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Characters"
        NewValue = ClipboardContainsFormat(FCF_Character)
    Case "Image"
        NewValue = ClipboardContainsImage(CustomClipboard, Me.hwnd)
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
    Case "Characters"
        CopyCharacter
    Case "Image"
        CopyImage
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Cut()
On Error Resume Next
    Select Case ActiveType
    Case "Characters"
        CutCharacter
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Delete()
On Error Resume Next
    Select Case ActiveType
    Case "Characters"
        DeleteCharacter
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Paste()
On Error Resume Next
    Select Case ActiveType
    Case "Characters"
        PasteCharacter
    Case "Image"
        PasteImage
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
    ' lol
    ' Redraw
End Sub

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2Font", GetTitle(Name))
End Sub

Friend Sub SetFont(Font As Fury2Font)
On Error Resume Next
    Set m_fntFont = Font
End Sub

Private Sub picCharacterList_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_imgCharacter As Fury2Image
Dim l_lngCharacter As Long
Dim l_lngX As Long
Dim l_lngWidth As Long
Dim l_rctCharacter As Fury2Rect
    l_lngX = -hsCharacters.Value
    For l_lngCharacter = 1 To m_fntFont.CharacterCount
        Set l_imgCharacter = m_fntFont.Character(l_lngCharacter)
        If l_imgCharacter Is Nothing Then
        Else
            Set l_rctCharacter = F2Rect(l_lngX, 0, ClipValue(l_imgCharacter.Width, 6, 999) + 1, picCharacterList.ScaleHeight, False)
            If l_rctCharacter.PointInside(X, Y) Then
                m_lngSelectedCharacter = l_lngCharacter
                RedrawCharacterList
                RedrawSelectedCharacter
                If Button = 2 Then
                    Editor.ActionUpdate
                    Select Case QuickShowMenu2(picCharacterList, X, Y, _
                        Menus(MenuString("&Insert New"), "-", MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete)), _
                        frmIcons.ilContextMenus)
                    Case 1
                        InsertCharacter
                    Case 3
                        CutCharacter
                    Case 4
                        CopyCharacter
                    Case 5
                        PasteCharacter
                    Case 6
                        DeleteCharacter
                    Case Else
                    End Select
                End If
                Exit Sub
            End If
            l_lngX = l_lngX + ClipValue(l_imgCharacter.Width, 6, 999) + 1
        End If
    Next l_lngCharacter
    If Button = 2 Then
        Editor.ActionUpdate
        Select Case QuickShowMenu2(picCharacterList, X, Y, _
            Menus(MenuString("&Insert New"), "-", MenuString("&Paste", , , "PASTE", , , Editor.CanPaste)), _
            frmIcons.ilContextMenus)
        Case 1
            InsertCharacter
        Case 3
            PasteCharacter
        Case Else
        End Select
    End If
End Sub

Private Sub picCharacterList_Resize()
On Error Resume Next
    Set m_imgCharacterList = F2DIBSection(picCharacterList.ScaleWidth, picCharacterList.ScaleHeight, picCharacterList.hdc)
    SelectObject picCharacterList.hdc, m_imgCharacterList.DIBHandle
    RedrawCharacterList
End Sub

Private Sub picCharacters_Resize()
On Error Resume Next
    picCharacterList.Move 2, 2, picCharacters.ScaleWidth - 4, m_fntFont.FullHeight + 16
    hsCharacters.Move 2, picCharacterList.Top + picCharacterList.Height + 2, picCharacterList.Width, hsCharacters.Height
    tsCharacter.Move 2, hsCharacters.Top + hsCharacters.Height + 2, picCharacterList.Width, picCharacters.ScaleHeight - hsCharacters.Height - picCharacterList.Height - 8
End Sub

Private Sub picPreview_Paint()
On Error Resume Next
    CopyImageToDC picPreview.hdc, m_imgPreview.Rectangle, m_imgPreview
End Sub

Private Sub picPreview_Resize()
On Error Resume Next
    RedrawPreview
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

Private Sub tsViews_TabSelected(TheTab As ngUI.ngTab)
On Error Resume Next
    m_lngCurrentView = CLng(Mid(TheTab.key, 2))
    ViewChanged
End Sub

Public Sub CutCharacter()
On Error Resume Next
    CopyCharacter
    DeleteCharacter
End Sub

Public Sub CopyCharacter()
On Error Resume Next
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(FCF_Character), SelectedCharacter
    CustomClipboard.ClipboardClose
End Sub

Public Sub CopyImage()
On Error Resume Next
    ClipboardSerializeImage CustomClipboard, Me.hwnd, SelectedCharacter.Image
End Sub

Public Function PasteCharacter() As CharacterProxy
On Error Resume Next
Dim l_chCharacter As CharacterProxy
    Set l_chCharacter = New CharacterProxy
    Set l_chCharacter.Font = m_fntFont
    m_fntFont.AddCharacter F2Image(1, 1), 0
    l_chCharacter.Character = m_fntFont.CharacterCount
    m_lngSelectedCharacter = m_fntFont.CharacterCount
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(FCF_Character), l_chCharacter) Then
        CustomClipboard.ClipboardClose
        RedrawCharacterList
        RedrawSelectedCharacter
        Editor.ToolbarUpdate
        Set PasteCharacter = l_chCharacter
    Else
        CustomClipboard.ClipboardClose
    End If
End Function

Public Sub PasteImage()
On Error Resume Next
Dim l_imgImage As Fury2Image
    Set l_imgImage = ClipboardDeserializeImage(CustomClipboard, Me.hwnd)
    If Not (l_imgImage Is Nothing) Then
        Set SelectedCharacter.Image = l_imgImage
        RedrawCharacterList
        RedrawSelectedCharacter
        Editor.ToolbarUpdate
    End If
End Sub

Public Sub InsertCharacter()
On Error Resume Next
    m_fntFont.AddCharacter F2Image(4, 4), 0
    m_lngSelectedCharacter = m_fntFont.CharacterCount
    RedrawCharacterList
    RedrawSelectedCharacter
    Editor.ToolbarUpdate
End Sub

Public Sub DeleteCharacter()
On Error Resume Next
    m_fntFont.RemoveCharacter m_lngSelectedCharacter
    m_lngSelectedCharacter = ClipValue(m_lngSelectedCharacter, 1, m_fntFont.CharacterCount)
    RedrawCharacterList
    RedrawSelectedCharacter
    Editor.ToolbarUpdate
End Sub

