VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#13.5#0"; "ngUI.ocx"
Begin VB.Form frmSequence 
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
   Icon            =   "frmSequence.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   489
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   613
   ShowInTaskbar   =   0   'False
   Begin ngUI.ngViewport vpTimeline 
      Height          =   1755
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   9195
      _ExtentX        =   16219
      _ExtentY        =   3096
   End
   Begin VB.Timer tmrRedraw 
      Enabled         =   0   'False
      Interval        =   25
      Left            =   8685
      Top             =   6870
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin VB.PictureBox picDisplay 
      BorderStyle     =   0  'None
      Height          =   5505
      Left            =   -15
      ScaleHeight     =   367
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   610
      TabIndex        =   1
      Top             =   1785
      Width           =   9150
      Begin VB.Timer tmrRedrawOnce 
         Enabled         =   0   'False
         Interval        =   1
         Left            =   8220
         Top             =   5070
      End
      Begin ngPlugins.ObjectInspector insProperties 
         Height          =   3600
         Left            =   4410
         TabIndex        =   2
         Top             =   90
         Width           =   4740
         _ExtentX        =   8361
         _ExtentY        =   6350
      End
      Begin ngUI.ngViewport vpDisplay 
         Height          =   3930
         Left            =   45
         TabIndex        =   3
         Top             =   75
         Width           =   4320
         _ExtentX        =   7620
         _ExtentY        =   6932
      End
   End
End
Attribute VB_Name = "frmSequence"
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
Implements iToolbar

Private Enum SequenceEditorViews
    View_Sequence
End Enum

Public EnableProperties As Boolean

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private m_fntFont As Fury2RasterFont

Private m_imgCustomDisplay As Fury2Image

Private m_imgEvent As Fury2Image
Private m_imgEventTriggered As Fury2Image

Private m_colUndo As New Engine.Fury2Collection
Private m_colRedo As New Engine.Fury2Collection

Private m_sngTimelineZoom As Single

Private m_lngEventCount As Long
Private m_lngTimelineDrag As Long
Private m_sngTimelineHeaderWidth As Single
Private m_sngTimelineHeaderHeight As Single
Private m_sngTime As Single
Private m_seqSequence As Fury2Sequence
Private m_colElements As Fury2Collection
Private m_colTweens As Fury2Collection
Private m_booSelected As Boolean
Private m_booStoredData As Boolean
Private m_colHooks As Fury2Collection
Private m_picPictures() As Fury2Picture
Private m_lngPictureCount As Long
Private m_hsHotspots() As Fury2Hotspot
Private m_engEngine As Fury2Engine
Private m_imgFramebuffer As Fury2Image
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin
Private m_lngOldBitmap As Long
Private m_booPlaying As Boolean

Private m_booVisible As Boolean

Private m_splMain As New cSplitter
Private m_splDisplay As New cSplitter

Private WithEvents m_tbrToolbar As ngToolbar
Attribute m_tbrToolbar.VB_VarHelpID = -1
Private m_lngCurrentView As SequenceEditorViews

Public Property Get CurrentKeyframe() As Fury2SequenceKeyframe
On Error Resume Next
Dim l_kfCurrentKeyframe As Fury2SequenceKeyframe
Dim l_kfNextKeyframe As Fury2SequenceKeyframe
    If m_seqSequence Is Nothing Then Exit Property
    Set l_kfCurrentKeyframe = m_seqSequence.CurrentKeyframe
    Set l_kfNextKeyframe = m_seqSequence.NextKeyframe
    If m_seqSequence.Time >= l_kfNextKeyframe.Time Then
        Set CurrentKeyframe = l_kfNextKeyframe
    Else
        Set CurrentKeyframe = l_kfCurrentKeyframe
    End If
End Property

Public Property Get NextKeyframe() As Fury2SequenceKeyframe
On Error Resume Next
Dim l_kfNextKeyframe As Fury2SequenceKeyframe
    If m_seqSequence Is Nothing Then Exit Property
    Set l_kfNextKeyframe = m_seqSequence.NextKeyframe
    If m_seqSequence.Time >= l_kfNextKeyframe.Time Then
        Set NextKeyframe = m_seqSequence.Keyframes(m_seqSequence.Keyframes.Find(l_kfNextKeyframe) + 1)
    Else
        Set NextKeyframe = l_kfNextKeyframe
    End If
End Property

Private Sub iToolbar_HideToolbar(Toolbar As Object)
On Error Resume Next
    Set m_tbrToolbar = Toolbar
    m_tbrToolbar.Visible = False
    Set m_tbrToolbar = Nothing
End Sub

Private Sub iToolbar_ShowToolbar(Toolbar As Object)
On Error Resume Next
    Set m_tbrToolbar = Toolbar
    m_tbrToolbar.Visible = True
    RefreshTools
End Sub

Public Property Get Sequence() As Fury2Sequence
On Error Resume Next
    Set Sequence = m_seqSequence
End Property

Public Property Set Sequence(ByVal NewValue As Fury2Sequence)
On Error Resume Next
    Set m_seqSequence = NewValue
    InitSequence
    Form_Activate
End Property

Public Property Get Filename() As String
On Error Resume Next
    Filename = m_strFilename
End Property

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iDocument_DocumentIcon = Editor.LoadResources("ng").ItemData("icons\sequence.png")
End Property

Public Property Get RedrawRate() As Long
On Error Resume Next
    RedrawRate = tmrRedraw.Interval
End Property

Public Property Let RedrawRate(ByVal Rate As Long)
On Error Resume Next
    tmrRedraw.Interval = Rate
End Property

Public Property Get Document() As iDocument
On Error Resume Next
    Set Document = Me
End Property

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Public Property Get ActiveType() As String
On Error Resume Next
    Select Case LCase(Trim(Me.ActiveControl.Name))
    Case Else
    End Select
End Property

Public Sub LockRedraw(Window As Long, State As Boolean)
On Error Resume Next
    SendMessage Window, WM_SETREDRAW, Abs(CLng(State)), 0
End Sub

Public Sub Redraw()
On Error Resume Next
    RedrawTimeline
    RedrawDisplay
End Sub

Public Sub RefreshTimeline()
On Error Resume Next
Dim l_lngTimelineWidth As Long, l_lngTimelineHeight As Long
Dim l_sngLength As Single
Dim l_strTween As String
Dim l_lngWidth As Long
Dim l_eleElement As Fury2SequenceElement
Dim l_evtEvent As Fury2SequenceEvent
Dim l_kfKeyframe As Fury2SequenceKeyframe
Dim l_twTween As Fury2SequenceTween
Dim l_colTweens As Fury2Collection
Dim l_lngSum() As Long
Dim l_lngElement As Long, l_lngTween As Long, l_lngElements As Long, l_lngCount As Long
Dim l_lngEvents As Long, l_colEvents As Fury2Collection
    Set m_colElements = New Fury2Collection
    Set m_colTweens = New Fury2Collection
    l_sngLength = m_seqSequence.Length + 10
    l_lngTimelineWidth = (l_sngLength * m_sngTimelineZoom) + m_sngTimelineHeaderWidth
    m_sngTimelineHeaderWidth = 0
    l_lngCount = 2
    For Each l_eleElement In m_seqSequence.Elements
        l_lngWidth = m_fntFont.TextWidth(l_eleElement.Id) + 2
        If l_lngWidth > m_sngTimelineHeaderWidth Then
            m_sngTimelineHeaderWidth = l_lngWidth
        End If
        If Not m_colElements.Contains(l_eleElement.Id) Then
            m_colElements.Add New Fury2Collection, l_eleElement.Id
            l_lngCount = l_lngCount + 1
        End If
    Next l_eleElement
    m_colElements.Add New Fury2Collection, "Events"
    Set l_colEvents = m_colElements("Events")
    For Each l_kfKeyframe In m_seqSequence.Keyframes
        For Each l_twTween In l_kfKeyframe.Tweens
            l_lngWidth = m_fntFont.TextWidth(l_twTween.PropertyName) + 10
            If l_lngWidth > m_sngTimelineHeaderWidth Then
                m_sngTimelineHeaderWidth = l_lngWidth
            End If
            Set l_colTweens = Nothing
            l_lngElement = m_seqSequence.Elements.Find(m_seqSequence.Elements(l_twTween.ElementID))
            Set l_colTweens = m_colElements(l_twTween.ElementID)
            If l_colTweens.Find(l_twTween.PropertyName) <= 0 Then
                l_colTweens.Add l_twTween.PropertyName
                l_lngCount = l_lngCount + 1
            End If
        Next l_twTween
    Next l_kfKeyframe
    ReDim l_lngSum(1 To m_seqSequence.Elements.Count)
    If m_seqSequence.Elements.Count > 1 Then
        For l_lngElements = 2 To m_seqSequence.Elements.Count
            l_lngSum(l_lngElements) = l_lngSum(l_lngElements - 1) + m_colElements(l_lngElements - 1).Count + 1
        Next l_lngElements
    End If
    For Each l_kfKeyframe In m_seqSequence.Keyframes
        For Each l_twTween In l_kfKeyframe.Tweens
            l_lngElement = m_seqSequence.Elements.Find(m_seqSequence.Elements(l_twTween.ElementID))
            l_lngTween = l_lngSum(l_lngElement) + m_colElements(l_twTween.ElementID).Find(l_twTween.PropertyName)
            m_colTweens.Add l_lngTween, l_twTween.ElementID & "." & l_twTween.PropertyName
        Next l_twTween
        l_lngEvents = 1
        For Each l_evtEvent In l_kfKeyframe.Events
            If l_lngEvents > l_colEvents.Count Then
                l_lngCount = l_lngCount + 1
                l_colEvents.Add l_lngCount, "Events." & l_lngEvents
                m_colTweens.Add l_lngCount, "Events." & l_lngEvents
            End If
            l_lngEvents = l_lngEvents + 1
        Next l_evtEvent
    Next l_kfKeyframe
    m_lngEventCount = l_colEvents.Count
    m_sngTimelineHeaderHeight = m_fntFont.FullHeight
    l_lngTimelineHeight = m_sngTimelineHeaderHeight + ((m_fntFont.FullHeight + 2) * l_lngCount)
    vpTimeline.SetVirtualSize l_lngTimelineWidth, l_lngTimelineHeight
End Sub

Public Sub RedrawTimeline()
On Error Resume Next
    vpTimeline.RedrawAndPaint
End Sub

Public Sub RedrawDisplay()
On Error Resume Next
    vpDisplay.RedrawAndPaint
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
    m_seqSequence.Free
    Set m_seqSequence = Nothing
    Set m_fpgPlugin = Nothing
End Sub

Public Sub InitSplitters()
On Error Resume Next
    With m_splMain
        .Orientation = cSPLTOrientationHorizontal
        .Bind vpTimeline, picDisplay
        .Orientation = cSPLTOrientationHorizontal
        .MinimumSize(cSPLTLeftOrTopPanel) = 50
        .MaximumSize(cSPLTLeftOrTopPanel) = 450
        .KeepProportion = True
        .Position = 150
    End With
    With m_splDisplay
        .Orientation = cSPLTOrientationVertical
        .Bind vpDisplay, insProperties, picDisplay
        .Orientation = cSPLTOrientationVertical
        .MinimumSize(cSPLTRightOrBottomPanel) = 50
        .MaximumSize(cSPLTRightOrBottomPanel) = 450
        .KeepProportion = True
        .Position = picDisplay.Width - 200
    End With
End Sub

Public Sub InitViewport()
On Error Resume Next
    Set m_imgFramebuffer = F2Image(Engine.ScreenWidth, Engine.ScreenHeight)
    vpDisplay.MaxWidth = Engine.ScreenWidth
    vpDisplay.MaxHeight = Engine.ScreenHeight
    vpDisplay.SetVirtualSize Engine.ScreenWidth, Engine.ScreenHeight
End Sub

Public Sub RefreshAll()
On Error Resume Next
    Redraw
End Sub

Private Function ClipboardContainsFormat(Format As TilesetEditorClipboardFormats) As Boolean
On Error Resume Next
Dim l_objPlugin As TilesetEditor
    Set l_objPlugin = m_fpgPlugin
    With l_objPlugin.CustomClipboard
        .GetCurrentFormats Me.hwnd
        ClipboardContainsFormat = .HasCurrentFormat(l_objPlugin.ClipboardFormat(Format))
    End With
End Function

Private Function ClipboardFormat(Format As TilesetEditorClipboardFormats) As Long
On Error Resume Next
Dim l_objPlugin As TilesetEditor
    Set l_objPlugin = m_fpgPlugin
    ClipboardFormat = l_objPlugin.ClipboardFormat(Format)
End Function

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Private Function CustomClipboard() As cCustomClipboard
On Error Resume Next
Dim l_objPlugin As TilesetEditor
    Set l_objPlugin = m_fpgPlugin
    Set CustomClipboard = l_objPlugin.CustomClipboard
End Function

Public Property Get Engine() As Fury2Engine
On Error Resume Next
    If m_engEngine Is Nothing Then Set m_engEngine = Editor.Engine
    Set Engine = m_engEngine
End Property

Public Property Get Editor() As Object
On Error Resume Next
Dim l_objPlugin As SequenceEditor
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Property

Public Sub InitSequence()
On Error Resume Next
    Engine.UnHookAll
    Engine.HideAllPictures
    Engine.DisableAllHotspots
    Engine.HookEvents m_seqSequence
    m_seqSequence.Play 0
    m_seqSequence.Update
    SetTime m_sngTime
    insProperties.InspectAny = True
    insProperties.Inspect m_seqSequence, "Sequence", True, , True
    RefreshTimeline
    Redraw
End Sub

Public Sub SetTime(ByVal Time As Double)
On Error Resume Next
    If m_booPlaying Then Exit Sub
    m_sngTime = Time
    m_seqSequence.Time = Engine.Math.ClipNumber(m_sngTime, 0, m_seqSequence.Length)
    m_seqSequence.Update
    insProperties.RefreshValues
End Sub

Public Sub SelectSequence()
On Error Resume Next
    If m_booSelected Then Exit Sub
    m_booSelected = True
    If m_booStoredData Then
        Engine.SetPictures m_picPictures, m_lngPictureCount
        Engine.SetHotspots m_hsHotspots
        Engine.SetHookedObjects m_colHooks
    End If
End Sub

Public Sub DeselectSequence()
On Error Resume Next
    If Not m_booSelected Then Exit Sub
    m_booSelected = False
    m_booStoredData = True
    m_lngPictureCount = Engine.ActivePictureSlots
    m_picPictures = Engine.GetPictures()
    m_hsHotspots = Engine.GetHotspots()
    Set m_colHooks = Engine.GetHookedObjects()
End Sub

Public Sub Form_Activate()
On Error Resume Next
    SelectSequence
    Set insProperties.Editor = Editor
    Form_Resize
    InitViewport
    SetTime m_sngTime
    Redraw
End Sub

Private Sub Form_Deactivate()
On Error Resume Next
    DeselectSequence
End Sub

Private Sub Form_Load()
On Error Resume Next
'    vsTileset.Width = GetScrollbarSize(vsTileset)
    EnableProperties = True
    InitSplitters
    Form_Activate
    m_sngTimelineZoom = 50
    Set m_fntFont = New Fury2RasterFont
    m_fntFont.ImportTTF Me.Font, False
    m_fntFont.Color = SoftFX.SetAlpha(SoftFX.SwapChannels(GetSystemColor(SystemColor_Button_Text), Red, Blue), 255)
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
    m_splMain.Resize
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu
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
    Set m_imgEvent = Editor.LoadResources("ng").ItemData("sequence editor\event.png")
    Set m_imgEventTriggered = Editor.LoadResources("ng").ItemData("sequence editor\inactive event.png")
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
Dim l_vfFile As VirtualFile
    Err.Clear
    Set l_vfFile = F2File()
    If Engine.SaveToFile(m_seqSequence, l_vfFile) Then
        l_vfFile.SaveFile Filename
        iDocument_Save = True
    End If
    If iDocument_Save Then
        SetFilename Filename
    End If
    Redraw
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "User Data"
End Property

Private Sub iEditingCommands_CanCopy(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanCut(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanDelete(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
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
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Cut()
On Error Resume Next
    Select Case ActiveType
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Delete()
On Error Resume Next
    Select Case ActiveType
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

Private Sub insProperties_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    RefreshTimeline
    SetTime m_sngTime
    Redraw
End Sub

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2seq", GetTitle(Name))
End Sub

Private Sub m_tbrToolbar_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
Dim l_kfKeyframe As Fury2SequenceKeyframe
Dim l_twTween As Fury2SequenceTween
    Select Case LCase(Trim(Button.key))
    Case "play"
        If Button.Checked Then
            StopSequence
        Else
            PlaySequence
        End If
    Case "pause"
        If Button.Checked Then
            UnpauseSequence
        Else
            PauseSequence
        End If
    Case "previouskeyframe"
        If m_sngTime = CurrentKeyframe.Time Then
            SetTime m_sngTime - 0.01
        End If
        SetTime CurrentKeyframe.Time
        Redraw
    Case "nextkeyframe"
        If m_sngTime = NextKeyframe.Time Then
            SetTime NextKeyframe.Time + 0.01
        Else
            SetTime NextKeyframe.Time
        End If
        Redraw
    Case "insertkeyframe"
        m_seqSequence.AddNewKeyframe m_sngTime
        m_seqSequence.Rewind
        SetTime m_sngTime
        Redraw
    Case "splicekeyframes"
        Set l_kfKeyframe = NextKeyframe
        With m_seqSequence.AddNewKeyframe(m_sngTime)
            For Each l_twTween In l_kfKeyframe.Tweens
                With .AddNewTween(l_twTween.ElementID & "." & l_twTween.PropertyName, l_twTween.GetCurrentValue(), l_twTween.ValueType, l_twTween.Variable)
                    .TweenMode = l_twTween.TweenMode
                    .TweenParameter = l_twTween.TweenParameter
                End With
            Next l_twTween
        End With
        m_seqSequence.Rewind
        SetTime m_sngTime
        Redraw
    Case "removekeyframe"
        m_seqSequence.Keyframes.Remove CurrentKeyframe
        m_seqSequence.Rewind
        SetTime m_sngTime
        Redraw
    End Select
End Sub

Private Sub picDisplay_Resize()
On Error Resume Next
    m_splDisplay.Resize
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

Private Sub tmrRedraw_Timer()
On Error Resume Next
Static l_booHere As Boolean
    Do While l_booHere
        DoEvents
    Loop
    l_booHere = True
    m_seqSequence.Update
    RedrawDisplay
    RedrawTimeline
    If m_seqSequence.Playing <> m_booPlaying Then StopSequence
    l_booHere = False
End Sub

Private Sub tmrRedrawOnce_Timer()
On Error Resume Next
    tmrRedrawOnce.Enabled = False
    RedrawDisplay
End Sub

Private Sub vpDisplay_Redraw(ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Single, ByVal YOffset As Single)
On Error Resume Next
Dim l_imgFramebuffer As Fury2Image
    If m_seqSequence Is Nothing Then Exit Sub
    Set l_imgFramebuffer = Engine.Backbuffer
    Set Engine.Backbuffer = m_imgFramebuffer
    Engine.ClearEveryFrame = True
    Engine.Redraw
    Set Engine.Backbuffer = l_imgFramebuffer
    Surface.Blit F2Rect(-XOffset, -YOffset, m_imgFramebuffer.Width, m_imgFramebuffer.Height, False), , m_imgFramebuffer
End Sub

Private Sub vpDisplay_SurfaceChanged()
On Error Resume Next
End Sub

Private Sub vpTimeline_MouseDown(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    If m_seqSequence Is Nothing Then Exit Sub
    If Y <= m_sngTimelineHeaderHeight Then
        m_lngTimelineDrag = 2
    ElseIf X <= m_sngTimelineHeaderWidth Then
        m_lngTimelineDrag = 1
    Else
        m_lngTimelineDrag = 0
    End If
    vpTimeline_MouseMove Button, Shift, X, Y
End Sub

Private Sub vpTimeline_MouseMove(ByVal Button As Integer, ByVal Shift As Integer, ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    If m_seqSequence Is Nothing Then Exit Sub
    If Button = 1 Then
        If m_lngTimelineDrag = 0 Then
            SetTime (X - m_sngTimelineHeaderWidth) / m_sngTimelineZoom
            If Not m_booPlaying Then
                Redraw
                'tmrRedrawOnce.Enabled = True
            End If
        Else
        End If
    End If
End Sub

Private Sub vpTimeline_Redraw(ByVal Surface As libGraphics.Fury2Image, ByVal XOffset As Single, ByVal YOffset As Single)
On Error Resume Next
Dim l_sngX As Single, l_sngY As Single, l_sngLastX As Single
Dim l_eleElement As Fury2SequenceElement
Dim l_colElement As Fury2Collection
Dim l_kfKeyframe As Fury2SequenceKeyframe
Dim l_sngTime As Single, l_sngEndTime As Single
Dim l_sngStep As Single, l_strNumberFormat As String
Dim l_kfCurrentKeyframe As Fury2SequenceKeyframe
Dim l_twTween As Fury2SequenceTween, l_evtEvent As Fury2SequenceEvent
Dim l_lngElement As Long, l_lngProperties As Long, l_lngEvents As Long
Dim l_strTween As String
Dim l_lngTweenColor As Long
    If m_seqSequence Is Nothing Then Exit Sub
    Set l_kfCurrentKeyframe = CurrentKeyframe
    l_lngTweenColor = F2RGB(0, 127, 0, 63)
    l_sngY = m_sngTimelineHeaderHeight
    l_sngX = 0 - XOffset + m_sngTimelineHeaderWidth
    Surface.PushClipRectangle F2Rect(m_sngTimelineHeaderWidth, m_sngTimelineHeaderHeight, vpTimeline.ViewportWidth, vpTimeline.ViewportHeight)
    Surface.Fill Surface.ClipRectangle, SwapChannels(GetSystemColor(SystemColor_Window_Background), Red, Blue)
    Surface.Fill F2Rect(0, l_sngY, (m_seqSequence.Length * m_sngTimelineZoom) - XOffset + m_sngTimelineHeaderWidth, vpTimeline.ViewportHeight), SwapChannels(GetSystemColor(SystemColor_Window), Red, Blue)
    For Each l_kfKeyframe In m_seqSequence.Keyframes
        l_sngLastX = l_sngX
        l_sngX = (l_kfKeyframe.Time * m_sngTimelineZoom) - XOffset + m_sngTimelineHeaderWidth
        If (l_sngX >= Surface.ClipRectangle.Left) And ((l_sngLastX <= Surface.ClipRectangle.Right) Or (l_sngX <= Surface.ClipRectangle.Right)) Then
            For Each l_twTween In l_kfKeyframe.Tweens
                l_strTween = l_twTween.ElementID & "." & l_twTween.PropertyName
                l_lngElement = CLng(m_colTweens(l_strTween))
                Surface.Fill F2Rect(l_sngLastX, l_sngY + (l_lngElement * (m_fntFont.FullHeight + 2)) + 1 - YOffset, l_sngX, l_sngY + ((l_lngElement + 1) * (m_fntFont.FullHeight + 2) - YOffset)), l_lngTweenColor, RenderMode_SourceAlpha
            Next l_twTween
            If l_kfCurrentKeyframe Is l_kfKeyframe Then
                Surface.AntiAliasConvexPolygon Array(Array(l_sngX + 0.5, l_sngY + 16), Array(l_sngX + 4.5, l_sngY - 1), Array(l_sngX - 4, l_sngY - 1)), F2RGB(0, 0, 127, 255), RenderMode_SourceAlpha
                Surface.AntiAliasLine Array(l_sngX, l_sngY, l_sngX, vpTimeline.ViewportHeight), F2RGB(0, 0, 127, 255)
                Surface.AntiAliasConvexPolygon Array(Array(l_sngX + 0.5, vpTimeline.ViewportHeight - 16), Array(l_sngX + 4.5, vpTimeline.ViewportHeight + 1), Array(l_sngX - 4, vpTimeline.ViewportHeight + 1)), F2RGB(0, 0, 127, 255), RenderMode_SourceAlpha
            Else
                Surface.AntiAliasLine Array(l_sngX, l_sngY, l_sngX, vpTimeline.ViewportHeight), F2RGB(0, 0, 0, 255)
            End If
            l_lngEvents = 1
            For Each l_evtEvent In l_kfKeyframe.Events
                l_strTween = "Events." & l_lngEvents
                l_lngElement = CLng(m_colTweens(l_strTween))
                If (l_evtEvent.Triggered) Then
                    m_imgEventTriggered.Draw Surface, l_sngX, l_sngY + ((l_lngElement - 1.5) * (m_fntFont.FullHeight + 2)) - YOffset + 1, 1, 1, , BlitMode_SourceAlpha
                Else
                    m_imgEvent.Draw Surface, l_sngX, l_sngY + ((l_lngElement - 1.5) * (m_fntFont.FullHeight + 2)) - YOffset + 1, 1, 1, , BlitMode_SourceAlpha
                End If
                l_lngEvents = l_lngEvents + 1
            Next l_evtEvent
        End If
    Next l_kfKeyframe
    If m_booPlaying Then
        l_sngTime = m_seqSequence.Time
    Else
        l_sngTime = m_sngTime
    End If
    l_sngX = (l_sngTime * m_sngTimelineZoom) - XOffset + m_sngTimelineHeaderWidth
    Surface.AntiAliasLine Array(l_sngX, l_sngY, l_sngX, vpTimeline.ViewportHeight), F2RGB(255, 0, 0, 255)
    l_sngY = -YOffset + m_sngTimelineHeaderHeight
    l_sngX = 0
    Surface.PopClipRectangle
    Surface.PushClipRectangle F2Rect(0, m_sngTimelineHeaderHeight, m_sngTimelineHeaderWidth, vpTimeline.ViewportHeight)
    Surface.AntiAliasLine Array(m_sngTimelineHeaderWidth - 1, l_sngY, m_sngTimelineHeaderWidth - 1, vpTimeline.ViewportHeight), F2RGB(0, 0, 0, 63)
    For Each l_eleElement In m_seqSequence.Elements
        Set l_colElement = Nothing
        Set l_colElement = m_colElements(l_eleElement.Id)
        m_fntFont.Draw Surface, l_eleElement.Id, F2Rect(l_sngX, l_sngY, m_sngTimelineHeaderWidth, m_fntFont.FullHeight, False), m_fntFont.Color
        Surface.AntiAliasLine Array(l_sngX, l_sngY + m_fntFont.FullHeight + 2, m_sngTimelineHeaderWidth - 1, l_sngY + m_fntFont.FullHeight + 2), F2RGB(0, 0, 0, 63)
        l_sngY = l_sngY + m_fntFont.FullHeight + 2
        If Not (l_colElement Is Nothing) Then
            If l_colElement.Count > 0 Then
                For l_lngProperties = 1 To l_colElement.Count
                    m_fntFont.Draw Surface, l_colElement(l_lngProperties), F2Rect(l_sngX + 8, l_sngY, m_sngTimelineHeaderWidth, m_fntFont.FullHeight, False), m_fntFont.Color
                    Surface.AntiAliasLine Array(l_sngX + 6, l_sngY + 1, l_sngX + 6, l_sngY + m_fntFont.FullHeight + 1), F2RGB(0, 0, 0, 63)
                    Surface.AntiAliasLine Array(l_sngX + 6, l_sngY + m_fntFont.FullHeight + 2, m_sngTimelineHeaderWidth - 1, l_sngY + m_fntFont.FullHeight + 2), F2RGB(0, 0, 0, 63)
                    l_sngY = l_sngY + m_fntFont.FullHeight + 2
                Next l_lngProperties
            End If
        End If
    Next l_eleElement
    m_fntFont.Draw Surface, "Events", F2Rect(l_sngX, l_sngY, m_sngTimelineHeaderWidth, m_fntFont.FullHeight, False), m_fntFont.Color
    Surface.AntiAliasLine Array(l_sngX, l_sngY + m_fntFont.FullHeight + 2, m_sngTimelineHeaderWidth - 1, l_sngY + m_fntFont.FullHeight + 2), F2RGB(0, 0, 0, 63)
    l_sngY = l_sngY + m_fntFont.FullHeight + 2
    If m_lngEventCount > 0 Then
        For l_lngProperties = 1 To m_lngEventCount
            m_fntFont.Draw Surface, "Event " & l_lngProperties, F2Rect(l_sngX + 8, l_sngY, m_sngTimelineHeaderWidth, m_fntFont.FullHeight, False), m_fntFont.Color
            Surface.AntiAliasLine Array(l_sngX + 6, l_sngY + 1, l_sngX + 6, l_sngY + m_fntFont.FullHeight + 1), F2RGB(0, 0, 0, 63)
            Surface.AntiAliasLine Array(l_sngX + 6, l_sngY + m_fntFont.FullHeight + 2, m_sngTimelineHeaderWidth - 1, l_sngY + m_fntFont.FullHeight + 2), F2RGB(0, 0, 0, 63)
            l_sngY = l_sngY + m_fntFont.FullHeight + 2
        Next l_lngProperties
    End If
    Surface.PopClipRectangle
    l_sngX = -XOffset
    l_sngY = 0
    l_sngEndTime = Sequence.Length + 10
    If m_sngTimelineZoom >= 50 Then
        l_sngStep = m_sngTimelineZoom \ 50
    Else
        l_sngStep = 1 / Floor((1 / m_sngTimelineZoom) * 10)
    End If
    If l_sngStep = 0 Then l_sngStep = 1
    Surface.PushClipRectangle F2Rect(m_sngTimelineHeaderWidth, 0, vpTimeline.ViewportWidth, m_sngTimelineHeaderHeight)
    l_strNumberFormat = String(Floor(Log(l_sngEndTime) / Log(10)) + 1, "0")
    For l_sngTime = 0 To l_sngEndTime Step l_sngStep
        l_sngX = (l_sngTime * m_sngTimelineZoom) - XOffset + m_sngTimelineHeaderWidth
        Surface.AntiAliasLine Array(l_sngX, l_sngY, l_sngX, m_sngTimelineHeaderHeight), F2Black
        m_fntFont.Draw Surface, Format(l_sngTime, IIf(Abs(l_sngTime - Floor(l_sngTime)) < 0.01, l_strNumberFormat, l_strNumberFormat + ".00")), F2Rect(l_sngX + 3, l_sngY, vpTimeline.ViewportWidth, m_sngTimelineHeaderHeight, False), m_fntFont.Color
    Next l_sngTime
    Surface.PopClipRectangle
End Sub

Public Sub PlaySequence()
On Error Resume Next
    If m_booPlaying Then Exit Sub
    m_booPlaying = True
    m_seqSequence.Play 1
    tmrRedraw.Enabled = True
    RefreshToolButtons
End Sub

Public Sub PauseSequence()
On Error Resume Next
    If Not m_booPlaying Then Exit Sub
    If m_seqSequence.Paused Then Exit Sub
    m_seqSequence.Pause
    tmrRedraw.Enabled = False
    RefreshToolButtons
    Redraw
End Sub

Public Sub UnpauseSequence()
On Error Resume Next
    If Not m_booPlaying Then Exit Sub
    If Not m_seqSequence.Paused Then Exit Sub
    m_seqSequence.Unpause
    tmrRedraw.Enabled = True
    RefreshToolButtons
End Sub

Public Sub StopSequence()
On Error Resume Next
    If Not m_booPlaying Then Exit Sub
    m_booPlaying = False
    m_seqSequence.Play 0
    m_seqSequence.Update
    tmrRedraw.Enabled = False
    RefreshToolButtons
    SetTime m_sngTime
    Redraw
End Sub

Public Sub RefreshToolButtons()
On Error Resume Next
    With m_tbrToolbar.Buttons
        .Item("Play").Checked = m_booPlaying
        .Item("Pause").Checked = m_booPlaying And (m_seqSequence.Paused)
    End With
End Sub

Public Sub RefreshTools()
On Error Resume Next
Dim l_lngButtons As Long
    If m_tbrToolbar Is Nothing Then Exit Sub
    Set m_tbrToolbar.ResourceFile = Editor.LoadResources("ng")
    m_tbrToolbar.DisableUpdates = True
    m_tbrToolbar.Buttons.Clear
    m_tbrToolbar.ResourcePattern = "sequence editor\*.png"
    With m_tbrToolbar.Buttons
        .AddNew , "Play", "play", "Play"
        .AddNew , "Pause", "pause", "Pause"
        .AddNew , "PreviousKeyframe", "previous keyframe", "Previous Keyframe"
        .AddNew , "NextKeyframe", "next keyframe", "Next Keyframe"
        .AddNew "-"
        .AddNew , "InsertKeyframe", "add new keyframe", "Insert Empty Keyframe"
        .AddNew , "SpliceKeyframes", "add duplicate keyframe", "Splice Keyframes"
        .AddNew , "RemoveKeyframe", "delete keyframe", "Remove Keyframe"
        .AddNew "-"
        .AddNew , "InsertElement", "add new element", "Add New Element"
        .AddNew , "RemoveElement", "delete element", "Remove Element"
    End With
    RefreshToolButtons
    m_tbrToolbar.DisableUpdates = False
    m_tbrToolbar.Reflow
    m_tbrToolbar.Width = m_tbrToolbar.IdealVerticalWidth
    m_tbrToolbar.Visible = True
    m_tbrToolbar.Reflow
End Sub
