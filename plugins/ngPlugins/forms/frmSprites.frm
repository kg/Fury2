VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#12.13#0"; "ngUI.ocx"
Begin VB.Form frmSprites 
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
   Icon            =   "frmSprites.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   489
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   613
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picHarmless 
      BorderStyle     =   0  'None
      Height          =   15
      Left            =   0
      ScaleHeight     =   1
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   1
      TabIndex        =   19
      Top             =   0
      Width           =   15
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   -15
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin VB.PictureBox picContainer 
      BorderStyle     =   0  'None
      Height          =   7275
      Left            =   30
      ScaleHeight     =   485
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   622
      TabIndex        =   0
      Top             =   15
      Width           =   9330
      Begin ngPlugins.Script scSprite 
         Height          =   750
         Left            =   105
         TabIndex        =   13
         Top             =   6285
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   1323
      End
      Begin VB.PictureBox picPoses 
         BackColor       =   &H80000016&
         BorderStyle     =   0  'None
         Height          =   5760
         Left            =   510
         ScaleHeight     =   384
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   396
         TabIndex        =   7
         Top             =   1470
         Visible         =   0   'False
         Width           =   5940
         Begin VB.TextBox txtPoseProperty 
            Height          =   285
            Left            =   375
            TabIndex        =   20
            Top             =   2400
            Visible         =   0   'False
            Width           =   2565
         End
         Begin VB.PictureBox picPoseDisplay 
            Height          =   1485
            Left            =   420
            ScaleHeight     =   95
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   143
            TabIndex        =   21
            Top             =   885
            Visible         =   0   'False
            Width           =   2205
         End
         Begin VB.PictureBox picFrames 
            BackColor       =   &H80000016&
            BorderStyle     =   0  'None
            Height          =   5760
            Left            =   2445
            ScaleHeight     =   384
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   396
            TabIndex        =   8
            Top             =   315
            Visible         =   0   'False
            Width           =   5940
            Begin ngUI.ngListBox lstFrames 
               Height          =   1140
               Left            =   210
               TabIndex        =   16
               Top             =   120
               Width           =   4830
               _ExtentX        =   8520
               _ExtentY        =   2011
               BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
                  Name            =   "Tahoma"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   400
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               AllowReorder    =   -1  'True
               AllowMultiSelect=   -1  'True
               AllowNullSelection=   0   'False
            End
            Begin VB.PictureBox picFrameDisplay 
               Height          =   1485
               Left            =   975
               ScaleHeight     =   95
               ScaleMode       =   3  'Pixel
               ScaleWidth      =   143
               TabIndex        =   15
               Top             =   1905
               Visible         =   0   'False
               Width           =   2205
            End
            Begin VB.TextBox txtFrameProperty 
               Height          =   285
               Left            =   930
               TabIndex        =   14
               Top             =   3420
               Visible         =   0   'False
               Width           =   2565
            End
            Begin ngPlugins.ObjectInspector insFrameOptions 
               Height          =   1725
               Left            =   2250
               TabIndex        =   9
               Top             =   3045
               Visible         =   0   'False
               Width           =   2310
               _ExtentX        =   4075
               _ExtentY        =   3043
            End
            Begin ngPlugins.Script scFrame 
               Height          =   975
               Left            =   3960
               TabIndex        =   10
               Top             =   2115
               Visible         =   0   'False
               Width           =   810
               _ExtentX        =   1429
               _ExtentY        =   1720
            End
            Begin ngUI.ngTabStrip tsFrames 
               Height          =   3195
               Left            =   315
               Top             =   1530
               Width           =   3450
               _ExtentX        =   6085
               _ExtentY        =   5636
            End
         End
         Begin ngPlugins.ObjectInspector insPoseOptions 
            Height          =   1725
            Left            =   315
            TabIndex        =   11
            Top             =   1950
            Visible         =   0   'False
            Width           =   2310
            _ExtentX        =   4075
            _ExtentY        =   3043
         End
         Begin ngUI.ngListBox lstPoses 
            Height          =   1140
            Left            =   15
            TabIndex        =   17
            Top             =   15
            Width           =   4830
            _ExtentX        =   8520
            _ExtentY        =   2011
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            AllowReorder    =   -1  'True
            AllowMultiSelect=   -1  'True
            AllowNullSelection=   0   'False
         End
         Begin ngUI.ngTabStrip tsPoses 
            Height          =   645
            Left            =   135
            Top             =   1800
            Width           =   450
            _ExtentX        =   794
            _ExtentY        =   1138
         End
      End
      Begin VB.PictureBox picStates 
         BackColor       =   &H80000016&
         BorderStyle     =   0  'None
         Height          =   3630
         Left            =   2190
         ScaleHeight     =   242
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   341
         TabIndex        =   4
         Top             =   1275
         Visible         =   0   'False
         Width           =   5115
         Begin VB.ListBox lstStatePoses 
            Height          =   1680
            IntegralHeight  =   0   'False
            Left            =   2895
            MultiSelect     =   1  'Simple
            TabIndex        =   5
            Top             =   1905
            Visible         =   0   'False
            Width           =   2175
         End
         Begin ngPlugins.ObjectInspector insStateOptions 
            Height          =   1725
            Left            =   540
            TabIndex        =   6
            Top             =   1950
            Visible         =   0   'False
            Width           =   2355
            _ExtentX        =   4154
            _ExtentY        =   3043
         End
         Begin ngUI.ngListBox lstStates 
            Height          =   1140
            Left            =   120
            TabIndex        =   18
            Top             =   105
            Width           =   4830
            _ExtentX        =   8520
            _ExtentY        =   2011
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            AllowReorder    =   -1  'True
            AllowMultiSelect=   -1  'True
            AllowNullSelection=   0   'False
         End
         Begin ngUI.ngTabStrip tsStates 
            Height          =   645
            Left            =   4305
            Top             =   1590
            Width           =   825
            _ExtentX        =   1455
            _ExtentY        =   1138
         End
      End
      Begin VB.PictureBox picSidebar 
         BorderStyle     =   0  'None
         Height          =   4920
         Left            =   6660
         ScaleHeight     =   328
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   180
         TabIndex        =   1
         Top             =   0
         Width           =   2700
         Begin cFScroll.FlatScrollBar vsSprites 
            Height          =   3510
            Left            =   1950
            TabIndex        =   3
            Top             =   255
            Width           =   285
            _ExtentX        =   503
            _ExtentY        =   6191
            Orientation     =   1
            Max             =   100
            LargeChange     =   16
            SmallChange     =   4
            Style           =   -1
         End
         Begin VB.PictureBox picSprites 
            AutoRedraw      =   -1  'True
            BorderStyle     =   0  'None
            Height          =   4065
            Left            =   0
            ScaleHeight     =   271
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   131
            TabIndex        =   2
            Top             =   0
            Width           =   1965
         End
      End
      Begin ngPlugins.ObjectInspector insOverview 
         Height          =   3045
         Left            =   180
         TabIndex        =   12
         Top             =   1965
         Visible         =   0   'False
         Width           =   1710
         _ExtentX        =   3016
         _ExtentY        =   5371
      End
      Begin ngUI.ngTabStrip tsViews 
         Height          =   435
         Left            =   1305
         Top             =   15
         Width           =   330
         _ExtentX        =   582
         _ExtentY        =   767
      End
   End
End
Attribute VB_Name = "frmSprites"
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

Private Enum SpriteEditorViews
    View_Overview
    View_States
    View_Poses
    View_Script
    View_Data
End Enum

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private m_imgFrameDisplay As Fury2Image
Private m_imgPoseDisplay As Fury2Image
Private m_imgSpriteBuffer As Fury2Image

Private m_lngSelectedSprite As Long
Private m_lngSelectedState As Long
Private m_lngSelectedPose As Long
Private m_lngSelectedFrame As Long

Private m_booStatePosesDragging As Boolean
Private m_lngStatePosesStart As Long

Private m_colUndo As New Engine.Fury2Collection
Private m_colRedo As New Engine.Fury2Collection

Private m_splSidebar As New cSplitter
Private m_scSprites As Fury2Sprites
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Private m_booVisible As Boolean

Private WithEvents m_tbrToolbar As ngToolbar
Attribute m_tbrToolbar.VB_VarHelpID = -1
Private m_lngCurrentView As SpriteEditorViews

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
On Error Resume Next
    Set iDocument_DocumentIcon = Editor.LoadResources("ng").ItemData("icons\sprites.png")
End Property

Private Sub InspectListItems(Inspector As ObjectInspector, Items() As ngListItem)
On Error Resume Next
Dim l_objObjects() As Object
Dim l_lngIndex As Long
    ReDim l_objObjects(LBound(Items) To UBound(Items))
    For l_lngIndex = LBound(Items) To UBound(Items)
        Set l_objObjects(l_lngIndex) = Items(l_lngIndex).Tag
    Next l_lngIndex
    Inspector.InspectMultiple l_objObjects
End Sub

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Public Sub CutSprite()
On Error Resume Next
    CopySprite
    DeleteSprite
End Sub

Public Sub CopySprite()
On Error Resume Next
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(SCF_Sprite), SelectedSprite
    CustomClipboard.ClipboardClose
End Sub

Public Function PasteSprite(Optional ByVal AtIndex As Long = -1) As Fury2Sprite
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    If AtIndex < 1 Then
        AtIndex = m_scSprites.Count + 1
    End If
    Set l_sprSprite = New Fury2Sprite
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(SCF_Sprite), l_sprSprite) Then
        CustomClipboard.ClipboardClose
        m_scSprites.Add l_sprSprite
        l_sprSprite.Initialize
        l_sprSprite.Load
        m_lngSelectedSprite = AtIndex
        ViewChanged
        RedrawSprites
        Editor.ToolbarUpdate
        Set PasteSprite = l_sprSprite
    Else
        CustomClipboard.ClipboardClose
    End If
End Function

Public Function InsertSprite(Optional ByVal Index As Long = -1) As Fury2Sprite
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_staState As Fury2State
    If Index = -1 Then Index = m_scSprites.Count + 1
    Set l_sprSprite = New Fury2Sprite
    l_sprSprite.Initialize
    l_sprSprite.Load
    l_sprSprite.Name = "New Sprite"
    Set l_staState = New Fury2State
    l_staState.Name = "New State"
    l_sprSprite.States.Add l_staState
    m_scSprites.Add l_sprSprite, , Index
    m_lngSelectedSprite = Index
    ViewChanged
    RedrawSprites
    Editor.ToolbarUpdate
    Set InsertSprite = l_sprSprite
End Function

Public Sub CutStates()
On Error Resume Next
    CopyStates
    DeleteStates
End Sub

Public Sub CopyStates()
On Error Resume Next
Dim l_vfData As VirtualFile
Dim l_liItem As ngListItem
Dim l_staState As Fury2State
    CustomClipboard.ClipboardOpen Me.hwnd
    Set l_vfData = New VirtualFile
    l_vfData.Save lstStates.SelectedItemCount
    For Each l_liItem In lstStates.SelectedItemCollection
        Set l_staState = l_liItem.Tag
        l_vfData.Save l_staState
    Next l_liItem
    CustomClipboard.ClearClipboard
    CustomClipboard.SetBinaryData ClipboardFormat(SCF_SpriteStates), l_vfData.Data
    Set l_vfData = Nothing
    CustomClipboard.ClipboardClose
End Sub

Public Sub PasteStates(Optional ByVal AtIndex As Long = -1)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngIndex As Long
Dim l_staState As Fury2State
Dim l_vfFile As VirtualFile
    If AtIndex = -1 Then
        AtIndex = SelectedSprite.States.Count + 1
    End If
    If AtIndex < 1 Then
        AtIndex = 1
    End If
    CustomClipboard.ClipboardOpen Me.hwnd
    Set l_vfFile = New VirtualFile
    If ClipboardDeserializeFile(CustomClipboard, ClipboardFormat(SCF_SpriteStates), l_vfFile) Then
        CustomClipboard.ClipboardClose
        l_vfFile.Load l_lngCount
        If l_lngCount > 0 Then
            For l_lngIndex = 1 To l_lngCount
                Set l_staState = New Fury2State
                l_vfFile.Load l_staState
                SelectedSprite.States.Add l_staState, , AtIndex + (l_lngIndex - 1)
            Next l_lngIndex
        End If
        lstStates.SelectItems AtIndex, AtIndex + l_lngCount - 1
    Else
        CustomClipboard.ClipboardClose
    End If
End Sub

Public Sub InsertState(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_staState As Fury2State
    If Index = -1 Then Index = SelectedSprite.States.Count + 1
    Set l_staState = New Fury2State
    SelectedSprite.States.Add l_staState, , Index
    m_lngSelectedState = Index
    StatesViewChanged
    lstStates.SelectItems Index
    Editor.ToolbarUpdate
End Sub

Public Sub CutPoses()
On Error Resume Next
    CopyPoses
    DeletePoses
End Sub

Public Sub CopyPoses()
On Error Resume Next
Dim l_vfData As VirtualFile
Dim l_liItem As ngListItem
Dim l_posPose As Fury2Pose
    CustomClipboard.ClipboardOpen Me.hwnd
    Set l_vfData = New VirtualFile
    l_vfData.Save lstPoses.SelectedItemCount
    For Each l_liItem In lstPoses.SelectedItemCollection
        Set l_posPose = l_liItem.Tag
        l_vfData.Save l_posPose
    Next l_liItem
    CustomClipboard.ClearClipboard
    CustomClipboard.SetBinaryData ClipboardFormat(SCF_SpritePoses), l_vfData.Data
    Set l_vfData = Nothing
    CustomClipboard.ClipboardClose
End Sub

Public Sub PastePoses(Optional ByVal AtIndex As Long = -1)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngIndex As Long
Dim l_posPose As Fury2Pose
Dim l_fraFrame As Fury2PoseFrame
Dim l_vfFile As VirtualFile
    If AtIndex = -1 Then
        AtIndex = SelectedSprite.Poses.Count + 1
    End If
    If AtIndex < 1 Then
        AtIndex = 1
    End If
    CustomClipboard.ClipboardOpen Me.hwnd
    Set l_vfFile = New VirtualFile
    If ClipboardDeserializeFile(CustomClipboard, ClipboardFormat(SCF_SpritePoses), l_vfFile) Then
        CustomClipboard.ClipboardClose
        l_vfFile.Load l_lngCount
        If l_lngCount > 0 Then
            For l_lngIndex = 1 To l_lngCount
                Set l_posPose = New Fury2Pose
                l_vfFile.Load l_posPose
                SelectedSprite.Poses.Add l_posPose, , AtIndex + (l_lngIndex - 1)
                For Each l_fraFrame In l_posPose.Frames
                    l_fraFrame.LoadGraphics
                Next l_fraFrame
            Next l_lngIndex
        End If
        lstPoses.SelectItems AtIndex, AtIndex + l_lngCount - 1
    Else
        CustomClipboard.ClipboardClose
    End If
End Sub

Public Sub InsertPose(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_posPose As Fury2Pose
    If Index = -1 Then Index = SelectedSprite.Poses.Count + 1
    Set l_posPose = New Fury2Pose
    SelectedSprite.Poses.Add l_posPose, , Index
    PosesViewChanged
    lstPoses.SelectItems Index
    Editor.ToolbarUpdate
End Sub

Public Sub CutFrames()
On Error Resume Next
    CopyFrames
    DeleteFrames
End Sub

Public Sub CopyFrames()
On Error Resume Next
Dim l_vfData As VirtualFile
Dim l_liItem As ngListItem
Dim l_fraFrame As Fury2PoseFrame
    CustomClipboard.ClipboardOpen Me.hwnd
    Set l_vfData = New VirtualFile
    l_vfData.Save lstFrames.SelectedItemCount
    For Each l_liItem In lstFrames.SelectedItemCollection
        Set l_fraFrame = l_liItem.Tag
        l_vfData.Save l_fraFrame
    Next l_liItem
    CustomClipboard.ClearClipboard
    CustomClipboard.SetBinaryData ClipboardFormat(SCF_SpriteFrames), l_vfData.Data
    Set l_vfData = Nothing
    CustomClipboard.ClipboardClose
End Sub

Public Sub PasteFrames(Optional ByVal AtIndex As Long = -1)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngIndex As Long
Dim l_fraFrame As Fury2PoseFrame
Dim l_vfFile As VirtualFile
    If AtIndex = -1 Then
        AtIndex = SelectedPose.Frames.Count + 1
    End If
    If AtIndex < 1 Then
        AtIndex = 1
    End If
    CustomClipboard.ClipboardOpen Me.hwnd
    Set l_vfFile = New VirtualFile
    If ClipboardDeserializeFile(CustomClipboard, ClipboardFormat(SCF_SpriteFrames), l_vfFile) Then
        CustomClipboard.ClipboardClose
        l_vfFile.Load l_lngCount
        If l_lngCount > 0 Then
            For l_lngIndex = 1 To l_lngCount
                Set l_fraFrame = New Fury2PoseFrame
                l_vfFile.Load l_fraFrame
                SelectedPose.Frames.Add l_fraFrame, , AtIndex + (l_lngIndex - 1)
                l_fraFrame.LoadGraphics
            Next l_lngIndex
        End If
        lstFrames.SelectItems AtIndex, AtIndex + l_lngCount - 1
    Else
        CustomClipboard.ClipboardClose
    End If
End Sub

Public Sub InsertFrame(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame
    If Index = -1 Then Index = SelectedPose.Frames.Count + 1
    Set l_fraFrame = New Fury2PoseFrame
    SelectedPose.Frames.Add l_fraFrame, , Index
    FramesViewChanged
    lstFrames.SelectItems Index
    Editor.ToolbarUpdate
End Sub

Public Sub DeleteFrames()
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_fraFrame As Fury2PoseFrame
    For Each l_liItem In lstFrames.SelectedItemCollection
        Set l_fraFrame = l_liItem.Tag
        SelectedPose.Frames.Remove SelectedPose.Frames.Find(l_fraFrame)
    Next l_liItem
    FramesViewChanged
    Editor.ToolbarUpdate
End Sub

Public Sub DeletePoses()
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_posPose As Fury2Pose
    For Each l_liItem In lstPoses.SelectedItemCollection
        Set l_posPose = l_liItem.Tag
        SelectedSprite.Poses.Remove SelectedSprite.Poses.Find(l_posPose)
    Next l_liItem
    PosesViewChanged
    Select Case LCase(Trim(tsPoses.SelectedTab.Text))
    Case "frames"
        FramesViewChanged
    Case Else
    End Select
    Editor.ToolbarUpdate
End Sub

Public Sub DeleteStates()
On Error Resume Next
Dim l_liItem As ngListItem
Dim l_staState As Fury2State
    For Each l_liItem In lstStates.SelectedItemCollection
        Set l_staState = l_liItem.Tag
        SelectedSprite.States.Remove SelectedSprite.States.Find(l_staState)
    Next l_liItem
    StatesViewChanged
    Editor.ToolbarUpdate
End Sub

Public Sub DeleteSprite()
On Error Resume Next
    m_scSprites.Remove m_lngSelectedSprite
    RedrawSprites
    ViewChanged
    Redraw
    Editor.ToolbarUpdate
End Sub

Function ListContext(Optional ByVal ItemSelected As Boolean = True) As Variant
On Error Resume Next
    If ItemSelected Then
        ListContext = Menus(MenuString("Insert &New", , , "NEW"), _
            MenuString("-"), _
            MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete))
    Else
        ListContext = Menus(MenuString("Insert &New", , , "NEW"), _
            MenuString("-"), _
            MenuString("Cu&t", , , "CUT", , , False), MenuString("&Copy", , , "COPY", , , False), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , False))
    End If
End Function

Public Property Get ActiveType() As String
On Error Resume Next
    Select Case LCase(Trim(Me.ActiveControl.Name))
    Case "lstposes"
        ActiveType = "Poses"
    Case "lstframes"
        ActiveType = "Frames"
    Case "lststates"
        ActiveType = "States"
    Case "picsprites"
        ActiveType = "Sprites"
    Case "scsprite"
        ActiveType = "Sprite Script"
    Case "scframe"
        ActiveType = "Frame Script"
    Case Else
    End Select
End Property

Public Sub LockRedraw(Window As Long, State As Boolean)
On Error Resume Next
    SendMessage Window, WM_SETREDRAW, Abs(CLng(State)), 0
End Sub

Public Property Get SelectedFrame() As Fury2PoseFrame
On Error Resume Next
Dim l_posPose As Fury2Pose
    Set l_posPose = SelectedPose
    If l_posPose Is Nothing Then
    Else
        If m_lngSelectedFrame > 0 And m_lngSelectedFrame <= l_posPose.Frames.Count Then
            Set SelectedFrame = l_posPose.Frames(m_lngSelectedFrame)
        End If
    End If
End Property

Public Property Get SelectedPose() As Fury2Pose
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Set l_sprSprite = SelectedSprite
    If l_sprSprite Is Nothing Then
    Else
        If m_lngSelectedPose > 0 And m_lngSelectedPose <= l_sprSprite.Poses.Count Then
            Set SelectedPose = l_sprSprite.Poses(m_lngSelectedPose)
        End If
    End If
End Property

Public Property Get SelectedState() As Fury2State
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Set l_sprSprite = SelectedSprite
    If l_sprSprite Is Nothing Then
    Else
        If m_lngSelectedState > 0 And m_lngSelectedState <= l_sprSprite.States.Count Then
            Set SelectedState = l_sprSprite.States(m_lngSelectedState)
        End If
    End If
End Property

Public Property Get SelectedSprite() As Fury2Sprite
On Error Resume Next
    If m_lngSelectedSprite > 0 And m_lngSelectedSprite <= m_scSprites.Count Then
        Set SelectedSprite = m_scSprites(m_lngSelectedSprite)
    End If
End Property

Public Property Get Sprites() As Fury2Sprites
    Set Sprites = m_scSprites
End Property

Public Sub AllocateBuffers()
On Error Resume Next
    Set m_imgSpriteBuffer = F2Image(1, 1)
    Set m_imgFrameDisplay = F2Image(1, 1)
    Set m_imgPoseDisplay = F2Image(1, 1)
End Sub

Public Sub DeallocateBuffers()
On Error Resume Next
    Set m_imgSpriteBuffer = Nothing
    Set m_imgFrameDisplay = Nothing
    Set m_imgPoseDisplay = Nothing
End Sub

Public Sub RedrawSprites()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_rctSprite As Fury2Rect
Dim l_rctText As Win32.RECT
Dim l_lngY As Long
Dim l_lngHeight As Long
Dim l_lngTotalHeight As Long
Dim l_lngIndex As Long
    picSprites.Cls
    l_lngIndex = 1
    l_lngY = -vsSprites.Value
    For Each l_sprSprite In m_scSprites
        l_lngHeight = 0
        Set l_rctSprite = l_sprSprite.Rectangle(True)
        If (m_imgSpriteBuffer.Width <> picSprites.ScaleWidth) Or (m_imgSpriteBuffer.Height <> l_rctSprite.Height) Then
            m_imgSpriteBuffer.Resize picSprites.ScaleWidth, l_rctSprite.Height + 2
        End If
        l_lngHeight = l_rctSprite.Height + 4 + picSprites.TextHeight(l_sprSprite.Name)
        If m_lngSelectedSprite = l_lngIndex Then
            m_imgSpriteBuffer.Clear SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue)
            picSprites.ForeColor = GetSystemColor(SystemColor_Highlight_Text)
        Else
            m_imgSpriteBuffer.Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
            picSprites.ForeColor = GetSystemColor(SystemColor_Button_Text)
        End If
        With l_sprSprite
            .Refresh
            .Render m_imgSpriteBuffer, (m_imgSpriteBuffer.Width - l_rctSprite.Width) / 2, 0
        End With
        If m_lngSelectedSprite = l_lngIndex Then
            m_imgSpriteBuffer.Fill m_imgSpriteBuffer.Rectangle, SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
        End If
        CopyImageToDC picSprites.hdc, F2Rect(0, l_lngY, m_imgSpriteBuffer.Width, m_imgSpriteBuffer.Height, False), m_imgSpriteBuffer
        If m_lngSelectedSprite = l_lngIndex Then
            picSprites.Line (0, l_lngY + m_imgSpriteBuffer.Height)-(picSprites.ScaleWidth, l_lngY + l_lngHeight), vbHighlight, BF
        End If
        With l_rctText
            .Left = 0
            .Top = l_lngY + l_rctSprite.Height + 2
            .Right = picSprites.ScaleWidth
            .Bottom = l_lngY + l_lngHeight
        End With
        Win32.DrawText picSprites.hdc, l_sprSprite.Name, Len(l_sprSprite.Name), l_rctText, DrawText_Align_Center_Horizontal Or DrawText_Align_Top Or DrawText_Wrap_WordBreak
        l_lngTotalHeight = l_lngTotalHeight + l_lngHeight
        l_lngY = l_lngY + l_lngHeight
        l_lngIndex = l_lngIndex + 1
    Next l_sprSprite
    If l_lngTotalHeight > picSprites.ScaleHeight Then
        If vsSprites.Enabled = False Then
            vsSprites.Enabled = True
        End If
        If vsSprites.Max <> (l_lngTotalHeight - picSprites.ScaleHeight) Then
            vsSprites.Max = (l_lngTotalHeight - picSprites.ScaleHeight)
        End If
        If vsSprites.Value > vsSprites.Max Then vsSprites.Value = vsSprites.Max
    Else
        If vsSprites.Enabled Then
            vsSprites.Enabled = False
        End If
        If vsSprites.Value <> 0 Then vsSprites.Value = 0
    End If
    If picSprites.AutoRedraw Then picSprites.Refresh
End Sub

Public Sub RedrawFrames()
On Error Resume Next
Dim l_lngItems As Long
Dim l_fraFrame As Fury2PoseFrame
Dim l_liItem As ngListItem
    If SelectedPose Is Nothing Then Exit Sub
    If lstPoses.SelectedItemCount > 1 Then
        lstFrames.AllowReorder = False
        lstFrames.DisableUpdates = True
        With lstFrames.ListItems
            .Clear
            .AddNew "[multiple poses]"
        End With
    Else
        lstFrames.AllowReorder = True
        lstFrames.DisableUpdates = True
        With lstFrames.ListItems
            If .Count <> SelectedPose.Frames.Count Then
                .Clear
                l_lngItems = 1
                For Each l_fraFrame In SelectedPose.Frames
                    Set .AddNew(IIf(l_lngItems = 1, "Stopped Frame", "Animation Frame " & l_lngItems - 1)).Tag = l_fraFrame
                    If l_lngItems = m_lngSelectedFrame Then
                        lstFrames.ListItems(l_lngItems).Selected = True
                    Else
                        lstFrames.ListItems(l_lngItems).Selected = False
                    End If
                    l_lngItems = l_lngItems + 1
                Next l_fraFrame
                lstFrames_SelectionChange
            Else
                l_lngItems = 1
                For Each l_liItem In lstFrames.ListItems
                    l_liItem.Text = IIf(l_lngItems = 1, "Stopped Frame", "Animation Frame " & l_lngItems - 1)
                    Set l_liItem.Tag = SelectedPose.Frames(l_lngItems)
'                    If l_lngItems = m_lngSelectedFrame Then
'                        l_liItem.Selected = True
'                    Else
'                        l_liItem.Selected = False
'                    End If
                    l_lngItems = l_lngItems + 1
                Next l_liItem
            End If
        End With
    End If
    lstFrames.DisableUpdates = False
    lstFrames.Reflow
End Sub

Public Sub RedrawPoses()
On Error Resume Next
Dim l_lngItems As Long
Dim l_posPose As Fury2Pose
Dim l_liItem As ngListItem
    If SelectedSprite Is Nothing Then Exit Sub
    lstPoses.AllowReorder = True
    lstPoses.DisableUpdates = True
    With lstPoses.ListItems
        If .Count <> SelectedSprite.Poses.Count Then
            .Clear
            l_lngItems = 1
            For Each l_posPose In SelectedSprite.Poses
                Set .AddNew(l_posPose.Name).Tag = l_posPose
                l_lngItems = l_lngItems + 1
            Next l_posPose
        Else
            l_lngItems = 1
            For Each l_liItem In lstPoses.ListItems
                l_liItem.Text = SelectedSprite.Poses(l_lngItems).Name
                Set l_liItem.Tag = SelectedSprite.Poses(l_lngItems)
                l_lngItems = l_lngItems + 1
            Next l_liItem
        End If
    End With
    lstPoses.DisableUpdates = False
    lstPoses.Reflow
End Sub

Public Sub RedrawStates()
On Error Resume Next
Dim l_lngItems As Long
Dim l_staState As Fury2State
Dim l_liItem As ngListItem
    If SelectedSprite Is Nothing Then Exit Sub
    lstStates.AllowReorder = True
    lstStates.DisableUpdates = True
    With lstStates.ListItems
        If .Count <> SelectedSprite.States.Count Then
            .Clear
            l_lngItems = 1
            For Each l_staState In SelectedSprite.States
                Set .AddNew(l_staState.Name).Tag = l_staState
                l_lngItems = l_lngItems + 1
            Next l_staState
        Else
            l_lngItems = 1
            For Each l_liItem In lstStates.ListItems
                l_liItem.Text = SelectedSprite.States(l_lngItems).Name
                Set l_liItem.Tag = SelectedSprite.States(l_lngItems)
                l_lngItems = l_lngItems + 1
            Next l_liItem
        End If
    End With
    lstStates.DisableUpdates = False
    lstStates.Reflow
End Sub

Public Sub RedrawStatePoses()
On Error Resume Next
Dim l_lngItems As Long
    If SelectedState Is Nothing Then Exit Sub
    LockRedraw lstStatePoses.hwnd, False
    For l_lngItems = 1 To SelectedSprite.Poses.Count
        If lstStatePoses.ListCount < l_lngItems Then
            lstStatePoses.AddItem SelectedSprite.Poses(l_lngItems).Name
        Else
            lstStatePoses.List(l_lngItems - 1) = SelectedSprite.Poses(l_lngItems).Name
        End If
        lstStatePoses.Selected(l_lngItems - 1) = ((l_lngItems > SelectedState.PoseOffset) And (l_lngItems <= (SelectedState.PoseOffset + SelectedState.DirectionPoses)))
    Next l_lngItems
    Do While lstStatePoses.ListCount > SelectedSprite.Poses.Count
        lstStatePoses.RemoveItem lstStatePoses.ListCount - 1
    Loop
    LockRedraw lstStatePoses.hwnd, True
    lstStatePoses.Refresh
End Sub

Public Sub RedrawFrame()
End Sub

Public Sub Redraw()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Overview, View_Data
        insOverview.RefreshValues
        insOverview.Redraw
    Case View_States
        RedrawStates
        Select Case LCase(Trim(tsStates.SelectedTab.Text))
        Case "options"
            insStateOptions.RefreshValues
            insStateOptions.Redraw
        Case "poses"
            RedrawStatePoses
        Case Else
        End Select
    Case View_Poses
        RedrawPoses
        Select Case LCase(Trim(tsPoses.SelectedTab.Text))
        Case "options"
            insPoseOptions.RefreshValues
            insPoseOptions.Redraw
        Case "frames"
            RedrawFrames
            Select Case LCase(Trim(tsFrames.SelectedTab.Text))
            Case "options"
                insFrameOptions.RefreshValues
                insFrameOptions.Redraw
            Case "script"
                scFrame.Text = SelectedFrame.Script
            Case Else
            End Select
        Case Else
        End Select
    Case View_Script
        scSprite.Text = SelectedSprite.Script
    Case Else
    End Select
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
    m_scSprites.Free
    Set m_fpgPlugin = Nothing
    Set m_scSprites = Nothing
    Set m_splSidebar = Nothing
End Sub

'Public Sub CopySprite()
'On Error Resume Next
'Dim l_sprSprite As Fury2Sprite
'    BeginProcess "Performing Copy..."
'    Set l_sprSprite = m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite)
'    CustomClipboard.ClipboardOpen Me.hWnd
'    ClipboardSerialize CustomClipboard, ClipboardFormat(CF_Sprite), l_sprSprite
'    CustomClipboard.ClipboardClose
'    EndProcess
'End Sub
'
'Public Sub CutSprite()
'On Error Resume Next
'    CopySprite
'    DeleteSprite
'End Sub
'
'Public Sub DeleteSprite()
'On Error Resume Next
'    BeginProcess "Performing Delete..."
'    ObjectUndoPush m_mapMap.Layers(m_lngSelectedLayer).Sprites, m_mapMap.Layers(m_lngSelectedLayer).Sprites(m_lngSelectedSprite), m_lngSelectedSprite, OUO_Add
'    m_mapMap.Layers(m_lngSelectedLayer).Sprites.Remove m_lngSelectedSprite
'    RefreshSprites
'    EndProcess
'End Sub

Public Sub InitSplitters()
On Error Resume Next
    With m_splSidebar
        .Orientation = cSPLTOrientationVertical
        .Bind tsViews, picSidebar
        .Orientation = cSPLTOrientationVertical
        .MinimumSize(cSPLTRightOrBottomPanel) = 75
        .MaximumSize(cSPLTRightOrBottomPanel) = 200
        .KeepProportion = True
        .Position = picContainer.ScaleWidth - 150
    End With
End Sub

Public Sub InitViews()
On Error Resume Next
    tsViews.Tabs.AddNew "Overview", "t" & CStr(View_Overview)
    tsViews.Tabs.AddNew "States", "t" & CStr(View_States)
    tsViews.Tabs.AddNew "Poses", "t" & CStr(View_Poses)
    tsViews.Tabs.AddNew "Script", "t" & CStr(View_Script)
    tsViews.Tabs.AddNew "Data", "t" & CStr(View_Data)
    tsStates.Tabs.AddNew "Options"
    tsStates.Tabs.AddNew "Poses"
    tsPoses.Tabs.AddNew "Options"
    tsPoses.Tabs.AddNew "Frames"
    tsPoses.Tabs.AddNew "Blocking"
    tsFrames.Tabs.AddNew "Options"
    tsFrames.Tabs.AddNew "Rectangle"
    tsFrames.Tabs.AddNew "Alignment"
    tsFrames.Tabs.AddNew "Script"
    tsFrames.Tabs.AddNew "Secondary Images"
End Sub

'Public Function PasteSprite(Optional ByVal AtIndex As Long = -1, Optional ByVal DoRedraw As Boolean = True) As Fury2Sprite
'On Error Resume Next
'Dim l_sprSprite As Fury2Sprite
'    With m_mapMap.Layers(m_lngSelectedLayer).Sprites
'        BeginProcess "Performing Paste..."
'        If AtIndex < 1 Then
'            AtIndex = .Count + 1
'        End If
'        Set l_sprSprite = New Fury2Sprite
'        CustomClipboard.ClipboardOpen Me.hWnd
'        If ClipboardDeserialize(CustomClipboard, ClipboardFormat(CF_Sprite), l_sprSprite) Then
'            CustomClipboard.ClipboardClose
'            l_sprSprite.Initialize
'            l_sprSprite.Load
'            ObjectUndoPush m_mapMap.Layers(m_lngSelectedLayer).Sprites, l_sprSprite, AtIndex, OUO_Remove
'            .Add l_sprSprite, , AtIndex
'            m_lngSelectedSprite = AtIndex
'            If DoRedraw Then
'                RefreshSprites
'                Redraw
'            End If
'            Editor.ToolbarUpdate
'            Set PasteSprite = l_sprSprite
'        Else
'            CustomClipboard.ClipboardClose
'        End If
'        EndProcess
'    End With
'End Function

Public Sub RefreshAll()
On Error Resume Next
    RedrawSprites
    ViewChanged
End Sub

Public Sub ViewChanged()
On Error Resume Next
Dim l_objObject As Object
    Screen.MousePointer = 11
    insOverview.Visible = False
    picStates.Visible = False
    picPoses.Visible = False
    scSprite.Visible = False
    m_splSidebar.Resize
    tsViews_Resize
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Visible = True
        insOverview.InspectAny = False
        insOverview.ZOrder
        insOverview.Inspect SelectedSprite, "Sprite", True
    Case View_States
        StatesViewChanged
        picStates.Visible = True
        picStates.ZOrder
    Case View_Poses
        PosesViewChanged
        picPoses.Visible = True
        picPoses.ZOrder
    Case View_Script
        scSprite.Text = SelectedSprite.ScriptSource
        scSprite.Visible = True
        scSprite.ZOrder
    Case View_Data
        SelectedSprite.ReloadScript
        insOverview.Visible = True
        insOverview.InspectAny = True
        insOverview.ZOrder
        insOverview.ClearStack
        insOverview.AddToStack SelectedSprite, "Sprite"
        insOverview.Inspect SelectedSprite.Script, "Data", False, False, True
    Case Else
    End Select
    Screen.MousePointer = 0
End Sub

Public Sub StatesViewChanged()
On Error Resume Next
    Screen.MousePointer = 11
    insStateOptions.Visible = False
    lstStatePoses.Visible = False
    tsStates_Resize
    Select Case LCase(Trim(tsStates.SelectedTab.Text))
    Case "options"
        insStateOptions.ShowHierarchy = False
        If lstStates.SelectedItemCount > 1 Then
            InspectListItems insStateOptions, lstStates.SelectedItems
        Else
            insStateOptions.Inspect SelectedState, "State #" & m_lngSelectedState, True
        End If
        insStateOptions.Visible = True
        insStateOptions.ZOrder
    Case "poses"
        RedrawStatePoses
        lstStatePoses.Visible = True
        lstStatePoses.ZOrder
    Case Else
    End Select
    Redraw
    Screen.MousePointer = 0
End Sub

Public Sub PosesViewChanged()
On Error Resume Next
    Screen.MousePointer = 11
    insPoseOptions.Visible = False
    picFrames.Visible = False
    picPoseDisplay.Visible = False
    txtPoseProperty.Visible = False
    tsPoses_Resize
    Select Case LCase(Trim(tsPoses.SelectedTab.Text))
    Case "options"
        insPoseOptions.ShowHierarchy = True
        If lstPoses.SelectedItemCount > 1 Then
            InspectListItems insPoseOptions, lstPoses.SelectedItems
        Else
            insPoseOptions.Inspect SelectedPose, "Pose #" & m_lngSelectedPose, True
        End If
        insPoseOptions.Visible = True
        insPoseOptions.ZOrder
    Case "frames"
        FramesViewChanged
        picFrames.Visible = True
        picFrames.ZOrder
    Case "blocking"
        picPoseDisplay.Visible = True
        picPoseDisplay.ZOrder
        txtPoseProperty.Visible = True
        txtPoseProperty.ZOrder
        RedrawPoseView
    Case Else
    End Select
    Redraw
    RedrawSprites
    Screen.MousePointer = 0
End Sub

Public Function GetSelectedFrames() As Object()
Dim l_lngItemCount As Long, l_lngIndex As Long, l_lngFrame As Long
Dim l_liItems() As ngListItem
Dim l_fraFrames() As Object, l_fraFrame As Fury2PoseFrame
    If lstPoses.SelectedItemCount > 0 Then
        l_liItems = lstPoses.SelectedItems
        For l_lngIndex = LBound(l_liItems) To UBound(l_liItems)
            l_lngItemCount = l_lngItemCount + SelectedSprite.Poses(l_liItems(l_lngIndex).Index).Frames.Count
        Next l_lngIndex
        ReDim l_fraFrames(0 To l_lngItemCount - 1)
        l_lngFrame = 0
        For l_lngIndex = LBound(l_liItems) To UBound(l_liItems)
            With SelectedSprite.Poses(l_liItems(l_lngIndex).Index)
                For Each l_fraFrame In .Frames
                    Set l_fraFrames(l_lngFrame) = l_fraFrame
                    l_lngFrame = l_lngFrame + 1
                Next l_fraFrame
            End With
        Next l_lngIndex
        GetSelectedFrames = l_fraFrames
    End If
End Function

Public Sub FramesViewChanged()
On Error Resume Next
Dim l_lngItemCount As Long, l_lngIndex As Long, l_lngFrame As Long
Dim l_liItems() As ngListItem
Dim l_fraFrames() As Object, l_fraFrame As Fury2PoseFrame
    Screen.MousePointer = 11
    insFrameOptions.Visible = False
    scFrame.Visible = False
    picFrameDisplay.Visible = False
    txtFrameProperty.Visible = False
    tsFrames_Resize
    Select Case LCase(Trim(tsFrames.SelectedTab.Text))
    Case "options"
        insFrameOptions.ShowHierarchy = True
        If lstPoses.SelectedItemCount > 1 Then
            l_fraFrames = GetSelectedFrames
            insFrameOptions.InspectMultiple l_fraFrames
        Else
            If lstFrames.SelectedItemCount > 1 Then
                InspectListItems insFrameOptions, lstFrames.SelectedItems
            Else
                insFrameOptions.Inspect SelectedFrame, "Frame #" & m_lngSelectedFrame, True
            End If
        End If
        insFrameOptions.Visible = True
        insFrameOptions.ZOrder
    Case "secondary images"
        insFrameOptions.ShowHierarchy = True
        If lstPoses.SelectedItemCount > 1 Then
'            l_fraFrames = GetSelectedFrames
'            insFrameOptions.InspectMultiple l_fraFrames
            insFrameOptions.Inspect Nothing
        Else
            If lstFrames.SelectedItemCount > 1 Then
'                InspectListItems insFrameOptions, lstFrames.SelectedItems
                insFrameOptions.Inspect Nothing
            Else
                insFrameOptions.ClearStack
                insFrameOptions.AddToStack SelectedFrame, "Frame #" & m_lngSelectedFrame
                insFrameOptions.Inspect SelectedFrame.SecondaryImages, "Secondary Images", False, False
            End If
        End If
        insFrameOptions.Visible = True
        insFrameOptions.ZOrder
    Case "script"
        scFrame.Text = SelectedFrame.Script
        scFrame.Visible = True
        scFrame.ZOrder
    Case "rectangle", "alignment"
        picFrameDisplay.Visible = True
        picFrameDisplay.ZOrder
        txtFrameProperty.Visible = True
        txtFrameProperty.ZOrder
        RedrawFrameView
    Case Else
    End Select
    Redraw
    RedrawSprites
    Screen.MousePointer = 0
End Sub

Public Sub RedrawFrameView(Optional ByVal UpdateTextbox As Boolean = True)
On Error Resume Next
Dim l_lngIndex As Long
Dim l_liItems() As ngListItem
Dim l_objFrames() As Object
Dim l_fraFrame As Fury2PoseFrame
Dim l_itType As IInspectorType
Dim l_strText As String, l_strCurrent As String
Dim l_lngX As Long, l_lngY As Long, l_lngRowHeight As Long
    l_liItems = lstFrames.SelectedItems
    l_objFrames = GetSelectedFrames()
    If (m_imgFrameDisplay.Width <> picFrameDisplay.ScaleWidth) Or (m_imgFrameDisplay.Height <> picFrameDisplay.ScaleHeight) Then
        m_imgFrameDisplay.Resize picFrameDisplay.ScaleWidth, picFrameDisplay.ScaleHeight
    End If
    With m_imgFrameDisplay
        .Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        SelectedFrame.Image.MatteColor = SelectedFrame.MatteColor
        Select Case tsFrames.SelectedTab.Text
        Case "Rectangle"
            .Blit , , SelectedFrame.Image, 0.5, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
            Err.Clear
            l_lngIndex = -1
            l_lngIndex = UBound(l_objFrames)
            If l_lngIndex = -1 Or Err <> 0 Then
            Else
                For l_lngIndex = LBound(l_objFrames) To UBound(l_objFrames)
                    Set l_fraFrame = l_objFrames(l_lngIndex)
                    .Fill l_fraFrame.Rectangle, SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
                    .Fill l_fraFrame.Rectangle, SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
                    .Blit l_fraFrame.Rectangle, l_fraFrame.Rectangle, l_fraFrame.Image, 1, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
                    Set l_itType = l_fraFrame.Rectangle
                    l_strCurrent = l_itType.ToString
                    If Len(l_strText) = 0 Then
                        l_strText = l_strCurrent
                    Else
                        If l_strCurrent = l_strText Then
                        Else
                            l_strText = "[multiple values]"
                        End If
                    End If
                Next l_lngIndex
            End If
            If UpdateTextbox Then txtFrameProperty.Text = l_strText
        Case "Alignment"
            Err.Clear
            l_lngIndex = -1
            l_lngIndex = UBound(l_objFrames)
            If l_lngIndex = -1 Or Err <> 0 Then
                .Blit F2Rect(0, 0, SelectedFrame.Rectangle.Width, SelectedFrame.Rectangle.Height, False), SelectedFrame.Rectangle, SelectedFrame.Image, 1, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
                .AntiAliasLine Array(SelectedFrame.XCenter, 0, SelectedFrame.XCenter, SelectedFrame.Rectangle.Height - 1), F2RGB(255, 0, 0, 255)
                .AntiAliasLine Array(0, SelectedFrame.YCenter, SelectedFrame.Rectangle.Width - 1, SelectedFrame.YCenter), F2RGB(255, 0, 0, 255)
                l_strText = SelectedFrame.XCenter & ", " & SelectedFrame.YCenter
            Else
                For l_lngIndex = LBound(l_objFrames) To UBound(l_objFrames)
                    Set l_fraFrame = l_objFrames(l_lngIndex)
                    If (l_lngX + l_fraFrame.Rectangle.Width) > .Width Then
                        l_lngX = 0
                        l_lngY = l_lngY + l_lngRowHeight
                        l_lngRowHeight = 0
                    End If
                    l_lngRowHeight = IIf(l_fraFrame.Rectangle.Height > l_lngRowHeight, l_fraFrame.Rectangle.Height, l_lngRowHeight)
                    .Blit F2Rect(l_lngX, l_lngY, l_fraFrame.Rectangle.Width, l_fraFrame.Rectangle.Height, False), l_fraFrame.Rectangle, l_fraFrame.Image, 1, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
                    .AntiAliasLine Array(l_fraFrame.XCenter + l_lngX, l_lngY, l_fraFrame.XCenter + l_lngX, l_fraFrame.Rectangle.Height - 1 + l_lngY), F2RGB(255, 0, 0, 255)
                    .AntiAliasLine Array(l_lngX, l_fraFrame.YCenter + l_lngY, l_fraFrame.Rectangle.Width - 1 + l_lngX, l_fraFrame.YCenter + l_lngY), F2RGB(255, 0, 0, 255)
                    l_strCurrent = l_fraFrame.XCenter & ", " & l_fraFrame.YCenter
                    If Len(l_strText) = 0 Then
                        l_strText = l_strCurrent
                    Else
                        If l_strCurrent = l_strText Then
                        Else
                            l_strText = "[multiple values]"
                        End If
                    End If
                    l_lngX = l_lngX + l_fraFrame.Rectangle.Width
                Next l_lngIndex
            End If
            If UpdateTextbox Then txtFrameProperty.Text = l_strText
        Case Else
        End Select
    End With
    picFrameDisplay_Paint
End Sub

Public Sub RedrawPoseView(Optional ByVal UpdateTextbox As Boolean = True)
On Error Resume Next
Dim l_lngIndex As Long
Dim l_posPose As Fury2Pose
Dim l_fraFrame As Fury2PoseFrame
Dim l_rctBlocking As Fury2Rect
Dim l_liItems() As ngListItem
Dim l_strText As String, l_strCurrent As String
Dim l_lngX As Long, l_lngY As Long, l_lngRowHeight As Long
    l_liItems = lstPoses.SelectedItems
    If (m_imgPoseDisplay.Width <> picPoseDisplay.ScaleWidth) Or (m_imgPoseDisplay.Height <> picPoseDisplay.ScaleHeight) Then
        m_imgPoseDisplay.Resize picPoseDisplay.ScaleWidth, picPoseDisplay.ScaleHeight
    End If
    With m_imgPoseDisplay
        .Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        Select Case tsPoses.SelectedTab.Text
        Case "Blocking"
            l_lngIndex = -1
            l_lngIndex = UBound(l_liItems)
            If l_lngIndex = -1 Or Err <> 0 Then
            Else
                For l_lngIndex = LBound(l_liItems) To UBound(l_liItems)
                    Set l_posPose = SelectedSprite.Poses(l_liItems(l_lngIndex).Index)
                    Set l_fraFrame = l_posPose.Frames(1)
                    If (l_lngX + l_fraFrame.Rectangle.Width) > .Width Then
                        l_lngX = 0
                        l_lngY = l_lngY + l_lngRowHeight
                        l_lngRowHeight = 0
                    End If
                    l_lngRowHeight = IIf(l_fraFrame.Rectangle.Height > l_lngRowHeight, l_fraFrame.Rectangle.Height, l_lngRowHeight)
                    .Blit l_fraFrame.Rectangle.Copy.Translate(-l_fraFrame.Rectangle.Left + l_lngX, -l_fraFrame.Rectangle.Top + l_lngY), l_fraFrame.Rectangle, l_fraFrame.Image, 1, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
                    Set l_rctBlocking = F2Rect(Ceil(l_fraFrame.XCenter - (l_posPose.Blocking.Width / 2)) + l_lngX, l_fraFrame.YCenter - (l_posPose.Blocking.Height) + l_lngY, l_posPose.Blocking.Width, l_posPose.Blocking.Height, False)
                    .Box l_rctBlocking, F2RGB(255, 0, 0, 192), RenderMode_SourceAlpha
                    l_strCurrent = l_posPose.Blocking.Width & "x" & l_posPose.Blocking.Height
                    If Len(l_strText) = 0 Then
                        l_strText = l_strCurrent
                    Else
                        If l_strCurrent = l_strText Then
                        Else
                            l_strText = "[multiple values]"
                        End If
                    End If
                    l_lngX = l_lngX + l_fraFrame.Rectangle.Width
                Next l_lngIndex
            End If
            If UpdateTextbox Then txtPoseProperty.Text = l_strText
        Case Else
        End Select
    End With
    picPoseDisplay_Paint
End Sub

Private Function ClipboardContainsFormat(Format As SpriteEditorClipboardFormats) As Boolean
On Error Resume Next
Dim l_objPlugin As SpriteEditor
    Set l_objPlugin = m_fpgPlugin
    With l_objPlugin.CustomClipboard
        .GetCurrentFormats Me.hwnd
        ClipboardContainsFormat = .HasCurrentFormat(l_objPlugin.ClipboardFormat(Format))
    End With
End Function

Private Function ClipboardFormat(Format As SpriteEditorClipboardFormats) As Long
On Error Resume Next
Dim l_objPlugin As SpriteEditor
    Set l_objPlugin = m_fpgPlugin
    ClipboardFormat = l_objPlugin.ClipboardFormat(Format)
End Function

Private Function ContextMenuIcon(key As String) As IPictureDisp
On Error Resume Next
    Set ContextMenuIcon = frmIcons.ilContextMenus.ItemPicture(frmIcons.ilContextMenus.ItemIndex(key))
End Function

Private Function CustomClipboard() As cCustomClipboard
On Error Resume Next
Dim l_objPlugin As SpriteEditor
    Set l_objPlugin = m_fpgPlugin
    Set CustomClipboard = l_objPlugin.CustomClipboard
End Function

Private Function Editor() As Object
On Error Resume Next
Dim l_objPlugin As SpriteEditor
    Set l_objPlugin = m_fpgPlugin
    Set Editor = l_objPlugin.Editor
End Function

Private Sub tsFrames_Resize()
On Error Resume Next
    Select Case LCase(Trim(tsFrames.SelectedTab.Text))
    Case "options", "secondary images"
        insFrameOptions.Move (2) + tsFrames.Left, tsFrames.Top + 24, tsFrames.Width - 4, tsFrames.Height - 26
    Case "script"
        scFrame.Move (2) + tsFrames.Left, tsFrames.Top + 24, tsFrames.Width - 4, tsFrames.Height - 26
    Case "alignment", "rectangle"
        picFrameDisplay.Move (2) + tsFrames.Left, tsFrames.Top + 24, tsFrames.Width - 4, tsFrames.Height - 26 - txtFrameProperty.Height
        txtFrameProperty.Move picFrameDisplay.Left, picFrameDisplay.Top + picFrameDisplay.Height, picFrameDisplay.Width, txtFrameProperty.Height
    Case Else
    End Select
End Sub

Private Sub tsFrames_TabSelected(TheTab As ngTab)
On Error Resume Next
    FramesViewChanged
End Sub

Private Sub tsPoses_Resize()
On Error Resume Next
    Select Case LCase(Trim(tsPoses.SelectedTab.Text))
    Case "options"
        insPoseOptions.Move (2) + tsPoses.Left, tsPoses.Top + 24, tsPoses.Width - 4, tsPoses.Height - 26
    Case "frames"
        picFrames.Move (2) + tsPoses.Left, tsPoses.Top + 24, tsPoses.Width - 4, tsPoses.Height - 26
    Case "blocking"
        picPoseDisplay.Move (2) + tsPoses.Left, tsPoses.Top + 24, tsPoses.Width - 4, tsPoses.Height - 26 - txtPoseProperty.Height
        txtPoseProperty.Move picPoseDisplay.Left, picPoseDisplay.Top + picPoseDisplay.Height, picPoseDisplay.Width, txtPoseProperty.Height
    Case Else
    End Select
End Sub

Private Sub tsPoses_TabSelected(TheTab As ngTab)
On Error Resume Next
    PosesViewChanged
End Sub

Private Sub tsStates_Resize()
On Error Resume Next
    Select Case LCase(Trim(tsStates.SelectedTab.Text))
    Case "options"
        insStateOptions.Move (2) + tsStates.Left, tsStates.Top + 24, tsStates.Width - 4, tsStates.Height - 26
    Case "poses"
        lstStatePoses.Move (2) + tsStates.Left, tsStates.Top + 24, tsStates.Width - 4, tsStates.Height - 26
    Case Else
    End Select
End Sub

Private Sub tsStates_TabSelected(TheTab As ngTab)
On Error Resume Next
    StatesViewChanged
End Sub

Private Sub tsViews_Resize()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Overview, View_Data
        insOverview.Move (2) + tsViews.Left, tsViews.Top + 24, tsViews.Width - 4, tsViews.Height - 26
    Case View_States
        picStates.Move (2) + tsViews.Left, tsViews.Top + 24, tsViews.Width - 4, tsViews.Height - 26
    Case View_Poses
        picPoses.Move (2) + tsViews.Left, tsViews.Top + 24, tsViews.Width - 4, tsViews.Height - 26
    Case View_Script
        scSprite.Move (2) + tsViews.Left, tsViews.Top + 24, tsViews.Width - 4, tsViews.Height - 26
    Case Else
    End Select
End Sub

Private Sub tsViews_TabSelected(TheTab As ngTab)
On Error Resume Next
    m_lngCurrentView = CLng(Mid(TheTab.key, 2))
    ViewChanged
End Sub

Public Sub Form_Activate()
On Error Resume Next
    Set insFrameOptions.Editor = Editor
    Set insOverview.Editor = Editor
    Set insPoseOptions.Editor = Editor
    Set insStateOptions.Editor = Editor
    AllocateBuffers
    Form_Resize
    tsViews_Resize
    picSidebar_Resize
    ViewChanged
    RedrawSprites
    Redraw
End Sub

Private Sub Form_Deactivate()
On Error Resume Next
    picHarmless.SetFocus
    DeallocateBuffers
End Sub

Private Sub Form_Load()
On Error Resume Next
'    vsSprites.Width = GetScrollbarSize(vsSprites)
    Set m_scSprites = New Fury2Sprites
    InitViews
    InitSplitters
    Form_Activate
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    picHarmless.SetFocus
    Cleanup
End Sub

Private Sub Form_Resize()
On Error GoTo 0
    If WindowState = 2 And m_booVisible = False Then
        m_booVisible = True
        RefreshAll
    End If
    picContainer.Move 2, 2, Me.ScaleWidth - 4, Me.ScaleHeight - 4
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu
        ' .DefineMenu "Reload Images", "ReloadGraphics"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler.GetMenu
        ' .DestroyMenu "ReloadGraphics"
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
    SaveToFile m_scSprites, l_vfFile
    l_vfFile.SaveFile Filename
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
    Redraw
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Sprite Collection"
End Property

Private Sub iEditingCommands_CanCopy(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Frames"
        NewValue = (Not (lstFrames.FirstSelectedItem Is Nothing))
    Case "Poses"
        NewValue = (Not (lstPoses.FirstSelectedItem Is Nothing))
    Case "States"
        NewValue = (Not (lstStates.FirstSelectedItem Is Nothing))
    Case "Sprites"
        NewValue = Not (SelectedSprite Is Nothing)
    Case "Sprite Script"
        NewValue = scSprite.Control.CanCopy
    Case "Frame Script"
        NewValue = scFrame.Control.CanCopy
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanCut(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Frames"
        NewValue = (Not (lstFrames.FirstSelectedItem Is Nothing)) And (SelectedPose.Frames.Count > 1)
    Case "Poses"
        NewValue = (Not (lstPoses.FirstSelectedItem Is Nothing)) And (SelectedSprite.Poses.Count > 1)
    Case "States"
        NewValue = (Not (lstStates.FirstSelectedItem Is Nothing)) And (SelectedSprite.States.Count > 1)
    Case "Sprites"
        NewValue = Not (SelectedSprite Is Nothing)
    Case "Sprite Script"
        NewValue = scSprite.Control.CanCut
    Case "Frame Script"
        NewValue = scFrame.Control.CanCut
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanDelete(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Frames"
        NewValue = (Not (lstFrames.FirstSelectedItem Is Nothing)) And (SelectedPose.Frames.Count > 1)
    Case "Poses"
        NewValue = (Not (lstPoses.FirstSelectedItem Is Nothing)) And (SelectedSprite.Poses.Count > 1)
    Case "States"
        NewValue = (Not (lstStates.FirstSelectedItem Is Nothing)) And (SelectedSprite.States.Count > 1)
    Case "Sprites"
        NewValue = (Not (SelectedSprite Is Nothing)) And (m_scSprites.Count > 1)
    Case "Sprite Script"
        NewValue = scSprite.Control.CanCut
    Case "Frame Script"
        NewValue = scFrame.Control.CanCut
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanPaste(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Sprite Script"
        NewValue = scSprite.Control.CanPaste
    Case "Frame Script"
        NewValue = scFrame.Control.CanPaste
    Case "Poses"
        NewValue = ClipboardContainsFormat(SCF_SpritePoses)
    Case "Frames"
        NewValue = ClipboardContainsFormat(SCF_SpriteFrames)
    Case "States"
        NewValue = ClipboardContainsFormat(SCF_SpriteStates)
    Case "Sprites"
        NewValue = ClipboardContainsFormat(SCF_Sprite)
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanRedo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Sprite Script"
        NewValue = scSprite.Control.CanRedo
    Case "Frame Script"
        NewValue = scFrame.Control.CanRedo
    Case Else
        NewValue = (m_colRedo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_CanSelectAll(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Frames"
        NewValue = True
    Case "Poses"
        NewValue = True
    Case "States"
        NewValue = True
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanSelectNone(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Frames"
        NewValue = True
    Case "Poses"
        NewValue = True
    Case "States"
        NewValue = True
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_CanUndo(NewValue As Boolean)
On Error Resume Next
    Select Case ActiveType
    Case "Sprite Script"
        NewValue = scSprite.Control.CanUndo
    Case "Frame Script"
        NewValue = scFrame.Control.CanUndo
    Case Else
        NewValue = (m_colUndo.Count > 0)
    End Select
End Sub

Private Sub iEditingCommands_Copy()
On Error Resume Next
    Select Case ActiveType
    Case "Sprite Script"
        scSprite.Control.Copy
    Case "Frame Script"
        scFrame.Control.Copy
    Case "States"
        CopyStates
    Case "Poses"
        CopyPoses
    Case "Frames"
        CopyFrames
    Case "Sprites"
        CopySprite
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Cut()
On Error Resume Next
    Select Case ActiveType
    Case "Sprite Script"
        scSprite.Control.Cut
    Case "Frame Script"
        scFrame.Control.Cut
    Case "States"
        CutStates
    Case "Poses"
        CutPoses
    Case "Frames"
        CutFrames
    Case "Sprites"
        CutSprite
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Delete()
On Error Resume Next
    Select Case ActiveType
    Case "Sprites"
        DeleteSprite
    Case "Frames"
        DeleteFrames
    Case "Poses"
        DeletePoses
    Case "States"
        DeleteStates
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Paste()
On Error Resume Next
    Select Case ActiveType
    Case "Sprite Script"
        scSprite.Control.Paste
    Case "Frame Script"
        scFrame.Control.Paste
    Case "States"
        PasteStates
    Case "Poses"
        PastePoses
    Case "Frames"
        PasteFrames
    Case "Sprites"
        PasteSprite
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_Redo()
On Error Resume Next
'    Me.Redo
    Redraw
End Sub

Private Sub iEditingCommands_SelectAll()
On Error Resume Next
    Select Case ActiveType
    Case "Frames"
        lstFrames.SelectAll
    Case "Poses"
        lstPoses.SelectAll
    Case "States"
        lstStates.SelectAll
    Case Else
    End Select
End Sub

Private Sub iEditingCommands_SelectNone()
On Error Resume Next
    Select Case ActiveType
    Case "Frames"
        lstFrames.SelectNone
    Case "Poses"
        lstPoses.SelectNone
    Case "States"
        lstStates.SelectNone
    Case Else
    End Select
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

Private Sub insFrameOptions_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    SelectedSprite.Refresh
    Redraw
    RedrawSprites
End Sub

Private Sub insOverview_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    SelectedSprite.Refresh
    Redraw
    RedrawSprites
End Sub

Private Sub insPoseOptions_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    SelectedSprite.Refresh
    Redraw
    RedrawSprites
End Sub

Private Sub insStateOptions_AfterItemChange(ByVal OldValue As Variant, ByVal NewValue As Variant)
On Error Resume Next
    SelectedSprite.Refresh
    Redraw
    RedrawSprites
End Sub

Private Sub lstFrames_DragComplete()
On Error Resume Next
    m_lngSelectedFrame = lstFrames.FirstSelectedItem.Index
    FramesViewChanged
End Sub

Private Sub lstFrames_DragMoveItem(Item As ngUI.ngListItem, ByVal OldIndex As Long, ByVal NewIndex As Long)
On Error Resume Next
    SelectedPose.Frames.Move OldIndex, NewIndex
End Sub

Private Sub lstFrames_ItemContextMenu(Item As ngUI.ngListItem)
On Error Resume Next
Dim l_ptMouse As POINTAPI
    If lstPoses.SelectedItemCount > 1 Then Exit Sub
    Editor.ActionUpdate
    GetCursorPos l_ptMouse
    ScreenToClient lstFrames.hwnd, l_ptMouse
    Select Case QuickShowMenu2(lstFrames, l_ptMouse.X, l_ptMouse.Y, _
    ListContext(Not (Item Is Nothing)), _
    frmIcons.ilContextMenus)
    Case 1
        If Item Is Nothing Then
            InsertFrame
        Else
            InsertFrame Item.Index
        End If
    Case 3
        CutFrames
    Case 4
        CopyFrames
    Case 5
        If Item Is Nothing Then
            PasteFrames
        Else
            PasteFrames Item.Index
        End If
    Case 6
        DeleteFrames
    Case Else
    End Select
End Sub

Private Sub lstFrames_SelectionChange()
On Error Resume Next
    m_lngSelectedFrame = lstFrames.FirstSelectedItem.Index
    FramesViewChanged
    Editor.ToolbarUpdate
End Sub

Private Sub lstPoses_DragComplete()
On Error Resume Next
    m_lngSelectedPose = lstPoses.FirstSelectedItem.Index
    PosesViewChanged
End Sub

Private Sub lstPoses_DragMoveItem(Item As ngUI.ngListItem, ByVal OldIndex As Long, ByVal NewIndex As Long)
On Error Resume Next
    SelectedSprite.Poses.Move OldIndex, NewIndex
End Sub

Private Sub lstPoses_ItemContextMenu(Item As ngUI.ngListItem)
On Error Resume Next
Dim l_ptMouse As POINTAPI
    Editor.ActionUpdate
    GetCursorPos l_ptMouse
    ScreenToClient lstPoses.hwnd, l_ptMouse
    Select Case QuickShowMenu2(lstPoses, l_ptMouse.X, l_ptMouse.Y, _
    ListContext(Not (Item Is Nothing)), _
    frmIcons.ilContextMenus)
    Case 1
        If Item Is Nothing Then
            InsertPose
        Else
            InsertPose Item.Index
        End If
    Case 3
        CutPoses
    Case 4
        CopyPoses
    Case 5
        If Item Is Nothing Then
            PastePoses
        Else
            PastePoses Item.Index
        End If
    Case 6
        DeletePoses
    Case Else
    End Select
End Sub

Private Sub lstPoses_SelectionChange()
On Error Resume Next
    m_lngSelectedPose = lstPoses.FirstSelectedItem.Index
    PosesViewChanged
    Editor.ToolbarUpdate
End Sub

Private Sub lstStatePoses_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_ptMouse As POINTAPI
Dim l_lngItems As Long
    If SelectedState Is Nothing Then Exit Sub
    If Button = 1 Then
        GetCursorPos l_ptMouse
        m_booStatePosesDragging = True
        m_lngStatePosesStart = LBItemFromPt(lstStatePoses.hwnd, l_ptMouse.X, l_ptMouse.Y, 0)
        If m_lngStatePosesStart = -1 Then
            If Y < 0 Then
                m_lngStatePosesStart = 0
            Else
                m_lngStatePosesStart = lstStatePoses.ListCount - 1
            End If
        End If
        SelectedState.PoseOffset = m_lngStatePosesStart
        SelectedState.DirectionPoses = 1
        For l_lngItems = 1 To SelectedSprite.Poses.Count
            lstStatePoses.Selected(l_lngItems - 1) = ((l_lngItems > SelectedState.PoseOffset) And (l_lngItems <= (SelectedState.PoseOffset + SelectedState.DirectionPoses)))
        Next l_lngItems
    End If
End Sub

Private Sub lstStatePoses_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_lngStatePosesEnd As Long
Dim l_ptMouse As POINTAPI
Dim l_lngItems As Long
    If SelectedState Is Nothing Then Exit Sub
    If ((Button And 1) = 1) And m_booStatePosesDragging Then
        GetCursorPos l_ptMouse
        SelectedState.PoseOffset = m_lngStatePosesStart
        l_lngStatePosesEnd = LBItemFromPt(lstStatePoses.hwnd, l_ptMouse.X, l_ptMouse.Y, 0)
        If l_lngStatePosesEnd = -1 Then
            If Y < 0 Then
                l_lngStatePosesEnd = 0
            Else
                l_lngStatePosesEnd = lstStatePoses.ListCount - 1
            End If
        End If
        If l_lngStatePosesEnd > m_lngStatePosesStart Then
            SelectedState.DirectionPoses = l_lngStatePosesEnd - m_lngStatePosesStart + 1
        Else
            SelectedState.DirectionPoses = 1
        End If
        For l_lngItems = 1 To SelectedSprite.Poses.Count
            lstStatePoses.Selected(l_lngItems - 1) = ((l_lngItems > SelectedState.PoseOffset) And (l_lngItems <= (SelectedState.PoseOffset + SelectedState.DirectionPoses)))
        Next l_lngItems
    End If
End Sub

Private Sub lstStates_DragComplete()
On Error Resume Next
    m_lngSelectedState = lstStates.FirstSelectedItem.Index
    StatesViewChanged
End Sub

Private Sub lstStates_DragMoveItem(Item As ngUI.ngListItem, ByVal OldIndex As Long, ByVal NewIndex As Long)
On Error Resume Next
    SelectedSprite.States.Move OldIndex, NewIndex
End Sub

Private Sub lstStates_ItemContextMenu(Item As ngUI.ngListItem)
On Error Resume Next
Dim l_ptMouse As POINTAPI
    Editor.ActionUpdate
    GetCursorPos l_ptMouse
    ScreenToClient lstStates.hwnd, l_ptMouse
    Select Case QuickShowMenu2(lstStates, l_ptMouse.X, l_ptMouse.Y, _
    ListContext(Not (Item Is Nothing)), _
    frmIcons.ilContextMenus)
    Case 1
        If Item Is Nothing Then
            InsertState
        Else
            InsertState Item.Index
        End If
    Case 3
        CutStates
    Case 4
        CopyStates
    Case 5
        If Item Is Nothing Then
            PasteStates
        Else
            PasteStates Item.Index
        End If
    Case 6
        DeleteStates
    Case Else
    End Select
End Sub

Private Sub lstStates_SelectionChange()
On Error Resume Next
    m_lngSelectedState = lstStates.FirstSelectedItem.Index
    StatesViewChanged
    Editor.ToolbarUpdate
End Sub

Private Sub picContainer_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebar.MouseDown Button, Shift, X, Y
End Sub

Private Sub picContainer_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebar.MouseMove Button, Shift, X, Y
End Sub

Private Sub picContainer_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_splSidebar.MouseUp Button, Shift, X, Y
End Sub

Private Sub picContainer_Resize()
On Error GoTo 0
    m_splSidebar.Resize
End Sub

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.f2sprites", GetTitle(Name))
End Sub

Friend Sub SetSprites(Sprites As Fury2Sprites)
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    Set m_scSprites = Sprites
    For Each l_sprSprite In m_scSprites
        l_sprSprite.Load
        l_sprSprite.Poses.LoadGraphics
    Next l_sprSprite
End Sub

Public Sub ReloadGraphics()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    For Each l_sprSprite In m_scSprites
        l_sprSprite.Load
        l_sprSprite.Poses.LoadGraphics
    Next l_sprSprite
End Sub

Private Sub picPoseDisplay_Paint()
On Error Resume Next
    CopyImageToDC picPoseDisplay.hdc, m_imgPoseDisplay.Rectangle, m_imgPoseDisplay
End Sub

Private Sub picPoseDisplay_Resize()
On Error Resume Next
    If picPoseDisplay.Visible Then RedrawPoseView
End Sub

Private Sub picFrameDisplay_Paint()
On Error Resume Next
    CopyImageToDC picFrameDisplay.hdc, m_imgFrameDisplay.Rectangle, m_imgFrameDisplay
End Sub

Private Sub picFrameDisplay_Resize()
On Error Resume Next
    If picFrameDisplay.Visible Then RedrawFrameView
End Sub

Private Sub picFrames_Resize()
On Error Resume Next
    lstFrames.Move 2, 2, picFrames.ScaleWidth - 4, ClipValue(picFrames.ScaleHeight / 4, 32, 150)
    tsFrames.Move 2, lstFrames.Top + lstFrames.Height + 2, picFrames.ScaleWidth - 4, picFrames.ScaleHeight - (lstFrames.Top + lstFrames.Height + 4)
End Sub

Private Sub picSidebar_Resize()
On Error GoTo 0
    picSprites.Move 0, 0, picSidebar.ScaleWidth - vsSprites.Width, picSidebar.ScaleHeight
End Sub

Private Sub picSprites_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_rctSprite As Fury2Rect
Dim l_rctText As Win32.RECT
Dim l_lngY As Long
Dim l_lngHeight As Long
Dim l_lngTotalHeight As Long
Dim l_lngIndex As Long
    l_lngIndex = 1
    l_lngY = -vsSprites.Value
    For Each l_sprSprite In m_scSprites
        l_lngHeight = 0
        Set l_rctSprite = l_sprSprite.Rectangle(True)
        l_lngHeight = l_rctSprite.Height + 4 + picSprites.TextHeight(l_sprSprite.Name)
        If (Y >= l_lngY) And (Y < (l_lngY + l_lngHeight)) Then
            If l_lngIndex <> m_lngSelectedSprite Then
                m_lngSelectedSprite = l_lngIndex
                ViewChanged
            End If
            RedrawSprites
            Exit For
        End If
        l_lngY = l_lngY + l_lngHeight
        l_lngIndex = l_lngIndex + 1
    Next l_sprSprite
    Editor.ActionUpdate
    If Button = 2 Then
        Select Case QuickShowMenu2(picSprites, X, Y, _
        ListContext(), _
        frmIcons.ilContextMenus)
        Case 1
            InsertSprite
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
    End If
End Sub

Private Sub picSprites_Paint()
On Error Resume Next
    RedrawSprites
End Sub

Private Sub picSprites_Resize()
On Error GoTo 0
    vsSprites.Move picSprites.Width, 0, GetScrollbarSize(vsSprites), picSprites.Height
    RedrawSprites
End Sub

Private Sub picPoses_Resize()
On Error Resume Next
    lstPoses.Move 2, 2, picPoses.ScaleWidth - 4, ClipValue(picPoses.ScaleHeight / 4, 32, 150)
    tsPoses.Move 2, lstPoses.Top + lstPoses.Height + 2, picPoses.ScaleWidth - 4, picPoses.ScaleHeight - (lstPoses.Top + lstPoses.Height + 4)
End Sub

Private Sub picStates_Resize()
On Error Resume Next
    lstStates.Move 2, 2, picStates.ScaleWidth - 4, ClipValue(picStates.ScaleHeight / 4, 32, 150)
    tsStates.Move 2, lstStates.Top + lstStates.Height + 2, picStates.ScaleWidth - 4, picStates.ScaleHeight - (lstStates.Top + lstStates.Height + 4)
End Sub

Private Sub scFrame_Change()
On Error Resume Next
    SelectedFrame.Script = scFrame.Text
End Sub

Private Sub scSprite_Change()
On Error Resume Next
    If scSprite.Visible Then
        SelectedSprite.ScriptSource = scSprite.Text
    End If
End Sub

Private Sub txtFrameProperty_Change()
On Error Resume Next
Dim l_itType As IInspectorType
Dim l_objFrames() As Object
Dim l_lngIndex As Long
Dim l_fraFrame As Fury2PoseFrame
Dim l_booChanged As Boolean
    If Left(txtFrameProperty.Text, 1) = "[" Then Exit Sub
    l_objFrames = GetSelectedFrames()
    Err.Clear
    l_lngIndex = -1
    l_lngIndex = UBound(l_objFrames)
    If l_lngIndex = -1 Or Err <> 0 Then
    Else
        For l_lngIndex = LBound(l_objFrames) To UBound(l_objFrames)
            Set l_fraFrame = l_objFrames(l_lngIndex)
            Select Case tsFrames.SelectedTab.Text
            Case "Rectangle"
                Err.Clear
                Set l_itType = l_fraFrame.Rectangle
                l_itType.FromString txtFrameProperty.Text
                If Err = 0 Then l_booChanged = True
            Case "Alignment"
                Err.Clear
                l_fraFrame.XCenter = CSng(Split(txtFrameProperty.Text, ",")(0))
                l_fraFrame.YCenter = CSng(Split(txtFrameProperty.Text, ",")(1))
                If Err = 0 Then l_booChanged = True
            Case Else
            End Select
        Next l_lngIndex
    End If
    If l_booChanged Then RedrawFrameView False
End Sub

Private Sub txtPoseProperty_Change()
On Error Resume Next
Dim l_liItems() As ngListItem
Dim l_lngIndex As Long
Dim l_posPose As Fury2Pose
Dim l_booChanged As Boolean
    If Left(txtPoseProperty.Text, 1) = "[" Then Exit Sub
    l_liItems = lstPoses.SelectedItems
    Err.Clear
    l_lngIndex = -1
    l_lngIndex = UBound(l_liItems)
    If l_lngIndex = -1 Or Err <> 0 Then
    Else
        For l_lngIndex = LBound(l_liItems) To UBound(l_liItems)
            Set l_posPose = SelectedSprite.Poses(l_liItems(l_lngIndex).Index)
            Select Case tsPoses.SelectedTab.Text
            Case "Blocking"
                Err.Clear
                l_posPose.Blocking.SetValues CLng(Split(txtPoseProperty.Text, "x")(0)), CLng(Split(txtPoseProperty.Text, "x")(1))
                If Err = 0 Then l_booChanged = True
            Case Else
            End Select
        Next l_lngIndex
    End If
    If l_booChanged Then RedrawPoseView False
End Sub

Private Sub vsSprites_Change()
    RedrawSprites
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

