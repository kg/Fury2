VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Begin VB.Form frmScriptFile 
   BorderStyle     =   0  'None
   Caption         =   "Untitled"
   ClientHeight    =   3180
   ClientLeft      =   0
   ClientTop       =   -15
   ClientWidth     =   4680
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
   Icon            =   "frmScriptFile.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   212
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   ShowInTaskbar   =   0   'False
   Begin ngPlugins.Script scScript 
      Height          =   3015
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4560
      _ExtentX        =   8043
      _ExtentY        =   5318
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   30
      Top             =   30
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmScriptFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements iExtendedForm
Implements iDocument
Implements iEditingCommands
Implements iCustomAccelerators
Implements iCustomMenus

Private m_colAccelerators As New Fury2Collection
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Public Sub Show_FindDialog()
On Error Resume Next
Static s_booHere As Boolean
    If s_booHere Then Exit Sub
    s_booHere = True
    scScript.Control.ExecuteCmd cmCmdFind
    Err.Clear
    s_booHere = False
End Sub

Public Sub Show_ReplaceDialog()
On Error Resume Next
Static s_booHere As Boolean
    If s_booHere Then Exit Sub
    s_booHere = True
    scScript.Control.ExecuteCmd cmCmdFindReplace
    Err.Clear
    s_booHere = False
End Sub

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Private Function Editor() As Object
On Error Resume Next
Dim l_objPlugin As ScriptFile
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Function

Private Sub Form_Load()
On Error Resume Next
End Sub

Private Sub iCustomAccelerators_DefineAccelerators(Manager As ngInterfaces.cAcceleratorManager)
On Error Resume Next
Dim l_aclAccelerator As cAccelerator
    For Each l_aclAccelerator In m_colAccelerators
        Manager.RemoveAccelerator l_aclAccelerator
    Next l_aclAccelerator
    m_colAccelerators.Clear
    m_colAccelerators.Add Manager.AddAccelerator(vbKeyF, Me, "Show_FindDialog", True)
    m_colAccelerators.Add Manager.AddAccelerator(vbKeyH, Me, "Show_ReplaceDialog", True)
End Sub

Private Sub iCustomAccelerators_UndefineAccelerators(Manager As ngInterfaces.cAcceleratorManager)
On Error Resume Next
Dim l_aclAccelerator As cAccelerator
    For Each l_aclAccelerator In m_colAccelerators
        Manager.RemoveAccelerator l_aclAccelerator
    Next l_aclAccelerator
    m_colAccelerators.Clear
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DestroyMenu "Find"
        .DestroyMenu "Replace"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DefineMenu "&Find...", "Find", , "Ctrl+F", ContextMenuIcon("FIND")
        .DefineMenu "R&eplace...", "Replace", , "Ctrl+H", ContextMenuIcon("REPLACE")
    End With
End Sub

Private Sub iCustomMenus_MenuClick(key As String)
On Error Resume Next
    Select Case LCase(Trim(key))
    Case "find"
        Show_FindDialog
    Case "replace"
        Show_ReplaceDialog
    Case Else
    End Select
End Sub

Private Property Get iDocument_Plugin() As ngInterfaces.iFileTypePlugin
On Error Resume Next
    Set iDocument_Plugin = m_fpgPlugin
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iFileTypePlugin)
On Error Resume Next
    Set m_fpgPlugin = RHS
End Property

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2script", GetTitle(Name))
End Sub

Private Sub Form_Resize()
On Error Resume Next
    scScript.Move 0, 0, Me.ScaleWidth, Me.ScaleHeight
End Sub

Private Property Get iDocument_CanSave() As Boolean
On Error Resume Next
    iDocument_CanSave = True
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = m_strFilename
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
Dim l_lngFileHandle As Long
    m_strFilename = Filename
    Err.Clear
    scScript.Control.SaveFile m_strFilename, False
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Script"
End Property

Private Sub iEditingCommands_CanCopy(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = .CanCopy
    End With
End Sub

Private Sub iEditingCommands_CanCut(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = .CanCut
    End With
End Sub

Private Sub iEditingCommands_CanDelete(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = True
    End With
End Sub

Private Sub iEditingCommands_CanPaste(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = .CanPaste
    End With
End Sub

Private Sub iEditingCommands_CanRedo(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = .CanRedo
    End With
End Sub

Private Sub iEditingCommands_CanSelectAll(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = (.SelLength < .TextLength) And (.TextLength > 0)
    End With
End Sub

Private Sub iEditingCommands_CanSelectNone(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = .SelLength > 0
    End With
End Sub

Private Sub iEditingCommands_CanUndo(NewValue As Boolean)
On Error Resume Next
    With scScript.Control
        NewValue = .CanUndo
    End With
End Sub

Private Sub iEditingCommands_Copy()
On Error Resume Next
    With scScript.Control
        .ExecuteCmd cmCmdCopy
    End With
End Sub

Private Sub iEditingCommands_Cut()
On Error Resume Next
    With scScript.Control
        .ExecuteCmd cmCmdCut
    End With
End Sub

Private Sub iEditingCommands_Delete()
On Error Resume Next
Dim l_rngSel As New Range
    With scScript.Control
        If .SelLength > 0 Then
            .SelText = ""
        Else
            Set l_rngSel = .GetSel(False)
            l_rngSel.EndColNo = l_rngSel.StartColNo + 1
            .SetSel l_rngSel, False
            .SelText = ""
        End If
    End With
End Sub

Private Sub iEditingCommands_Paste()
On Error Resume Next
    With scScript.Control
        .ExecuteCmd cmCmdPaste
    End With
End Sub

Private Sub iEditingCommands_Redo()
On Error Resume Next
    With scScript.Control
        .ExecuteCmd cmCmdRedo
    End With
End Sub

Private Sub iEditingCommands_SelectAll()
On Error Resume Next
    With scScript.Control
        .ExecuteCmd cmCmdSelectAll
    End With
End Sub

Private Sub iEditingCommands_SelectNone()
On Error Resume Next
Dim l_rngSel As New Range
    With scScript.Control
        .SetSel l_rngSel, True
    End With
End Sub

Private Sub iEditingCommands_Undo()
On Error Resume Next
    With scScript.Control
        .ExecuteCmd cmCmdUndo
    End With
End Sub

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

