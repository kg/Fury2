VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{9DC93C3A-4153-440A-88A7-A10AEDA3BAAA}#3.7#0"; "vbalDTab6.ocx"
Object = "{801EF197-C2C5-46DA-BA11-46DBBD0CD4DF}#1.1#0"; "cFScroll.ocx"
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
         TabIndex        =   20
         Top             =   6285
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   1323
      End
      Begin VB.PictureBox picPoses 
         BorderStyle     =   0  'None
         Height          =   5760
         Left            =   495
         ScaleHeight     =   384
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   396
         TabIndex        =   10
         Top             =   1470
         Visible         =   0   'False
         Width           =   5940
         Begin VB.PictureBox picFrames 
            BorderStyle     =   0  'None
            Height          =   5760
            Left            =   855
            ScaleHeight     =   384
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   396
            TabIndex        =   12
            Top             =   480
            Visible         =   0   'False
            Width           =   5940
            Begin VB.PictureBox picFrameDisplay 
               Height          =   1485
               Left            =   975
               ScaleHeight     =   95
               ScaleMode       =   3  'Pixel
               ScaleWidth      =   143
               TabIndex        =   22
               Top             =   1905
               Visible         =   0   'False
               Width           =   2205
            End
            Begin VB.TextBox txtFrameProperty 
               Height          =   285
               Left            =   930
               TabIndex        =   21
               Top             =   3420
               Visible         =   0   'False
               Width           =   2565
            End
            Begin VB.ListBox lstFrames 
               Height          =   1245
               IntegralHeight  =   0   'False
               ItemData        =   "frmSprites.frx":2372
               Left            =   510
               List            =   "frmSprites.frx":2379
               TabIndex        =   13
               Top             =   315
               Width           =   4665
            End
            Begin ngPlugins.ObjectInspector insFrameOptions 
               Height          =   1725
               Left            =   2250
               TabIndex        =   15
               Top             =   3045
               Visible         =   0   'False
               Width           =   2310
               _ExtentX        =   4075
               _ExtentY        =   3043
            End
            Begin ngPlugins.Script scFrame 
               Height          =   975
               Left            =   3960
               TabIndex        =   16
               Top             =   2115
               Visible         =   0   'False
               Width           =   810
               _ExtentX        =   1429
               _ExtentY        =   1720
            End
            Begin vbalDTab6.vbalDTabControl dtFrames 
               Height          =   4350
               Left            =   225
               TabIndex        =   14
               Top             =   1365
               Width           =   5685
               _ExtentX        =   10028
               _ExtentY        =   7673
               AllowScroll     =   0   'False
               TabAlign        =   0
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
               MoveableTabs    =   0   'False
            End
         End
         Begin VB.ListBox lstPoses 
            Height          =   1245
            IntegralHeight  =   0   'False
            ItemData        =   "frmSprites.frx":2387
            Left            =   510
            List            =   "frmSprites.frx":238E
            TabIndex        =   11
            Top             =   315
            Width           =   4665
         End
         Begin ngPlugins.ObjectInspector insPoseOptions 
            Height          =   1725
            Left            =   315
            TabIndex        =   18
            Top             =   1950
            Visible         =   0   'False
            Width           =   2310
            _ExtentX        =   4075
            _ExtentY        =   3043
         End
         Begin vbalDTab6.vbalDTabControl dtPoses 
            Height          =   4350
            Left            =   210
            TabIndex        =   17
            Top             =   1365
            Width           =   5685
            _ExtentX        =   10028
            _ExtentY        =   7673
            AllowScroll     =   0   'False
            TabAlign        =   0
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
            MoveableTabs    =   0   'False
         End
      End
      Begin VB.PictureBox picStates 
         BorderStyle     =   0  'None
         Height          =   3630
         Left            =   2190
         ScaleHeight     =   242
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   341
         TabIndex        =   5
         Top             =   1275
         Visible         =   0   'False
         Width           =   5115
         Begin VB.ListBox lstStates 
            Height          =   1245
            IntegralHeight  =   0   'False
            ItemData        =   "frmSprites.frx":239C
            Left            =   510
            List            =   "frmSprites.frx":239E
            TabIndex        =   6
            Top             =   315
            Width           =   4665
         End
         Begin VB.ListBox lstStatePoses 
            Height          =   1680
            IntegralHeight  =   0   'False
            Left            =   2895
            MultiSelect     =   1  'Simple
            TabIndex        =   8
            Top             =   1905
            Visible         =   0   'False
            Width           =   2175
         End
         Begin ngPlugins.ObjectInspector insStateOptions 
            Height          =   1725
            Left            =   540
            TabIndex        =   9
            Top             =   1950
            Visible         =   0   'False
            Width           =   2355
            _ExtentX        =   4154
            _ExtentY        =   3043
         End
         Begin vbalDTab6.vbalDTabControl dtStates 
            Height          =   2205
            Left            =   135
            TabIndex        =   7
            Top             =   1395
            Width           =   4980
            _ExtentX        =   8784
            _ExtentY        =   3889
            AllowScroll     =   0   'False
            TabAlign        =   0
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
            MoveableTabs    =   0   'False
         End
      End
      Begin VB.PictureBox picSidebar 
         BorderStyle     =   0  'None
         Height          =   4920
         Left            =   6660
         ScaleHeight     =   328
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   180
         TabIndex        =   2
         Top             =   0
         Width           =   2700
         Begin cFScroll.FlatScrollBar vsSprites 
            Height          =   3510
            Left            =   1950
            TabIndex        =   4
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
            TabIndex        =   3
            Top             =   0
            Width           =   1965
         End
      End
      Begin ngPlugins.ObjectInspector insOverview 
         Height          =   3045
         Left            =   180
         TabIndex        =   19
         Top             =   1965
         Visible         =   0   'False
         Width           =   1710
         _ExtentX        =   3016
         _ExtentY        =   5371
      End
      Begin vbalDTab6.vbalDTabControl dtViews 
         Height          =   4920
         Left            =   0
         TabIndex        =   1
         Top             =   0
         Width           =   6585
         _ExtentX        =   11615
         _ExtentY        =   8678
         TabAlign        =   0
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
         MoveableTabs    =   0   'False
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
End Enum

Private Const c_lngUndoStackLength As Long = 50
Private Const c_lngRedoStackLength As Long = 25

Private m_imgFrameDisplay As Fury2Image
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

Public Sub InsertSprite(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
    If Index = -1 Then Index = m_scSprites.Count + 1
    Set l_sprSprite = New Fury2Sprite
    l_sprSprite.Initialize
    l_sprSprite.Load
    l_sprSprite.Name = "New Sprite"
    m_scSprites.Add l_sprSprite, , Index
    m_lngSelectedSprite = Index
    ViewChanged
    RedrawSprites
    Editor.ToolbarUpdate
End Sub

Public Sub CutState()
On Error Resume Next
    CopyState
    DeleteState
End Sub

Public Sub CopyState()
On Error Resume Next
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(SCF_SpriteState), SelectedState
    CustomClipboard.ClipboardClose
End Sub

Public Function PasteState(Optional ByVal AtIndex As Long = -1) As Fury2State
On Error Resume Next
Dim l_staState As Fury2State
    If AtIndex < 1 Then
        AtIndex = SelectedSprite.States.Count + 1
    End If
    Set l_staState = New Fury2State
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(SCF_SpriteState), l_staState) Then
        CustomClipboard.ClipboardClose
        SelectedSprite.States.Add l_staState
        m_lngSelectedState = AtIndex
        StatesViewChanged
        Editor.ToolbarUpdate
        Set PasteState = l_staState
    Else
        CustomClipboard.ClipboardClose
    End If
End Function

Public Sub InsertState(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_staState As Fury2State
    If Index = -1 Then Index = m_lngSelectedState
    Set l_staState = New Fury2State
    SelectedSprite.States.Add l_staState ', , Index
    StatesViewChanged
    Editor.ToolbarUpdate
End Sub

Public Sub CutPose()
On Error Resume Next
    CopyPose
    DeletePose
End Sub

Public Sub CopyPose()
On Error Resume Next
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(SCF_SpritePose), SelectedPose
    CustomClipboard.ClipboardClose
End Sub

Public Function PastePose(Optional ByVal AtIndex As Long = -1) As Fury2Pose
On Error Resume Next
Dim l_posPose As Fury2Pose
Dim l_fraFrame As Fury2PoseFrame
    If AtIndex < 1 Then
        AtIndex = SelectedSprite.Poses.Count + 1
    End If
    Set l_posPose = New Fury2Pose
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(SCF_SpritePose), l_posPose) Then
        CustomClipboard.ClipboardClose
        For Each l_fraFrame In l_posPose.Frames
            l_fraFrame.LoadGraphics
        Next l_fraFrame
        SelectedSprite.Poses.Add l_posPose, , AtIndex
        m_lngSelectedPose = AtIndex
        PosesViewChanged
        Editor.ToolbarUpdate
        Set PastePose = l_posPose
    Else
        CustomClipboard.ClipboardClose
    End If
End Function

Public Sub InsertPose(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_posPose As Fury2Pose
    If Index = -1 Then Index = SelectedSprite.Poses.Count + 1
    Set l_posPose = New Fury2Pose
    SelectedSprite.Poses.Add l_posPose, , Index
    PosesViewChanged
    Editor.ToolbarUpdate
End Sub

Public Sub CutFrame()
On Error Resume Next
    CopyFrame
    DeleteFrame
End Sub

Public Sub CopyFrame()
On Error Resume Next
    CustomClipboard.ClipboardOpen Me.hwnd
    ClipboardSerialize CustomClipboard, ClipboardFormat(SCF_SpriteFrame), SelectedFrame
    CustomClipboard.ClipboardClose
End Sub

Public Function PasteFrame(Optional ByVal AtIndex As Long = -1) As Fury2PoseFrame
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame
    If AtIndex < 1 Then
        AtIndex = SelectedPose.Frames.Count + 1
    End If
    Set l_fraFrame = New Fury2PoseFrame
    CustomClipboard.ClipboardOpen Me.hwnd
    If ClipboardDeserialize(CustomClipboard, ClipboardFormat(SCF_SpriteFrame), l_fraFrame) Then
        CustomClipboard.ClipboardClose
        l_fraFrame.LoadGraphics
        SelectedPose.Frames.Add l_fraFrame, , AtIndex
        m_lngSelectedFrame = AtIndex
        FramesViewChanged
        Editor.ToolbarUpdate
        Set PasteFrame = l_fraFrame
    Else
        CustomClipboard.ClipboardClose
    End If
End Function

Public Sub InsertFrame(Optional ByVal Index As Long = -1)
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame
    If Index = -1 Then Index = SelectedPose.Frames.Count + 1
    Set l_fraFrame = New Fury2PoseFrame
    SelectedPose.Frames.Add l_fraFrame, , Index
    m_lngSelectedFrame = Index
    FramesViewChanged
    Editor.ToolbarUpdate
End Sub

Public Sub DeleteFrame()
On Error Resume Next
    SelectedPose.Frames.Remove m_lngSelectedFrame
    ViewChanged
    Redraw
    Editor.ToolbarUpdate
End Sub

Public Sub DeletePose()
On Error Resume Next
    SelectedSprite.Poses.Remove m_lngSelectedPose
    ViewChanged
    Redraw
    Editor.ToolbarUpdate
End Sub

Public Sub DeleteState()
On Error Resume Next
    SelectedSprite.States.Remove m_lngSelectedState
    ViewChanged
    Redraw
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

Function ListContext() As Variant
    ListContext = Menus(MenuString("Insert &New", , , "NEW"), _
        MenuString("-"), _
        MenuString("Cu&t", , , "CUT", , , Editor.CanCut), MenuString("&Copy", , , "COPY", , , Editor.CanCopy), MenuString("&Paste", , , "PASTE", , , Editor.CanPaste), MenuString("&Delete", , , "DELETE", , , Editor.CanDelete))
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
End Sub

Public Sub DeallocateBuffers()
On Error Resume Next
    Set m_imgSpriteBuffer = Nothing
    Set m_imgFrameDisplay = Nothing
End Sub

Public Sub RedrawSprites()
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_rctSprite As Fury2Rect
Dim l_rctText As Win32.Rect
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
        Else
            m_imgSpriteBuffer.Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        End If
        With l_sprSprite
            .Refresh
            .Render m_imgSpriteBuffer, (m_imgSpriteBuffer.Width - l_rctSprite.Width) / 2, 1
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
    If SelectedPose Is Nothing Then Exit Sub
    LockRedraw lstFrames.hwnd, False
    m_lngSelectedFrame = lstFrames.ListIndex + 1
    For l_lngItems = 1 To SelectedPose.Frames.Count
        If lstFrames.ListCount < l_lngItems Then
            lstFrames.AddItem IIf(l_lngItems = 1, "Stopped Frame", "Animation Frame " & l_lngItems - 1)
        Else
            lstFrames.List(l_lngItems - 1) = IIf(l_lngItems = 1, "Stopped Frame", "Animation Frame " & l_lngItems - 1)
        End If
    Next l_lngItems
    Do While lstFrames.ListCount > SelectedPose.Frames.Count
        lstFrames.RemoveItem lstFrames.ListCount - 1
    Loop
    lstFrames.ListIndex = m_lngSelectedFrame - 1
    LockRedraw lstFrames.hwnd, True
    lstFrames.Refresh
End Sub

Public Sub RedrawPoses()
On Error Resume Next
Dim l_lngItems As Long
    If SelectedSprite Is Nothing Then Exit Sub
    LockRedraw lstPoses.hwnd, False
    For l_lngItems = 1 To SelectedSprite.Poses.Count
        If lstPoses.ListCount < l_lngItems Then
            lstPoses.AddItem SelectedSprite.Poses(l_lngItems).Name
        Else
            lstPoses.List(l_lngItems - 1) = SelectedSprite.Poses(l_lngItems).Name
        End If
    Next l_lngItems
    Do While lstPoses.ListCount > SelectedSprite.Poses.Count
        lstPoses.RemoveItem lstPoses.ListCount - 1
    Loop
    lstPoses.ListIndex = m_lngSelectedPose - 1
    LockRedraw lstPoses.hwnd, True
    lstPoses.Refresh
End Sub

Public Sub RedrawStates()
On Error Resume Next
Dim l_lngItems As Long
    If SelectedSprite Is Nothing Then Exit Sub
    LockRedraw lstStates.hwnd, False
    For l_lngItems = 1 To SelectedSprite.States.Count
        If lstStates.ListCount < l_lngItems Then
            lstStates.AddItem SelectedSprite.States(l_lngItems).Name
        Else
            lstStates.List(l_lngItems - 1) = SelectedSprite.States(l_lngItems).Name
        End If
    Next l_lngItems
    Do While lstStates.ListCount > SelectedSprite.States.Count
        lstStates.RemoveItem lstStates.ListCount - 1
    Loop
    lstStates.ListIndex = m_lngSelectedState - 1
    LockRedraw lstStates.hwnd, True
    lstStates.Refresh
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
    Case View_Overview
        insOverview.RefreshValues
        insOverview.Redraw
    Case View_States
        RedrawStates
        Select Case LCase(Trim(dtStates.SelectedTab.key))
        Case "options"
            insStateOptions.RefreshValues
            insStateOptions.Redraw
        Case "poses"
            RedrawStatePoses
        Case Else
        End Select
    Case View_Poses
        RedrawPoses
        Select Case LCase(Trim(dtPoses.SelectedTab.key))
        Case "options"
            insPoseOptions.RefreshValues
            insPoseOptions.Redraw
        Case "frames"
            RedrawFrames
            Select Case LCase(Trim(dtFrames.SelectedTab.key))
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
        .Bind dtViews, picSidebar
        .Orientation = cSPLTOrientationVertical
        .MinimumSize(cSPLTRightOrBottomPanel) = 75
        .MaximumSize(cSPLTRightOrBottomPanel) = 200
        .KeepProportion = True
        .Position = picContainer.ScaleWidth - 150
    End With
End Sub

Public Sub InitViews()
On Error Resume Next
    dtViews.Tabs.Add "t" & CStr(View_Overview), , "Overview"
    dtViews.Tabs.Add "t" & CStr(View_States), , "States"
    dtViews.Tabs.Add "t" & CStr(View_Poses), , "Poses"
    dtViews.Tabs.Add "t" & CStr(View_Script), , "Script"
    dtStates.Tabs.Add "Options", , "Options"
    dtStates.Tabs.Add "Poses", , "Poses"
    dtPoses.Tabs.Add "Options", , "Options"
    dtPoses.Tabs.Add "Frames", , "Frames"
    dtPoses.Tabs.Add "Blocking", , "Blocking"
    dtFrames.Tabs.Add "Options", , "Options"
    dtFrames.Tabs.Add "Rectangle", , "Rectangle"
    dtFrames.Tabs.Add "Alignment", , "Alignment"
    dtFrames.Tabs.Add "Script", , "Script"
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
    dtViews_Resize
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Visible = True
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
    Case Else
    End Select
    Screen.MousePointer = 0
End Sub

Public Sub StatesViewChanged()
On Error Resume Next
    Screen.MousePointer = 11
    insStateOptions.Visible = False
    lstStatePoses.Visible = False
    dtStates_Resize
    Select Case LCase(Trim(dtStates.SelectedTab.key))
    Case "options"
        insStateOptions.ShowHierarchy = False
        insStateOptions.Inspect SelectedState, "State", True
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
    dtPoses_Resize
    Select Case LCase(Trim(dtPoses.SelectedTab.key))
    Case "options"
        insPoseOptions.ShowHierarchy = True
        insPoseOptions.Inspect SelectedPose, "Pose", True
        insPoseOptions.Visible = True
        insPoseOptions.ZOrder
    Case "frames"
        FramesViewChanged
        picFrames.Visible = True
        picFrames.ZOrder
    Case Else
    End Select
    Redraw
    RedrawSprites
    Screen.MousePointer = 0
End Sub

Public Sub FramesViewChanged()
On Error Resume Next
    Screen.MousePointer = 11
    insFrameOptions.Visible = False
    scFrame.Visible = False
    picFrameDisplay.Visible = False
    txtFrameProperty.Visible = False
    dtFrames_Resize
    Select Case LCase(Trim(dtFrames.SelectedTab.key))
    Case "options"
        insFrameOptions.ShowHierarchy = True
        insFrameOptions.Inspect SelectedFrame, "Frame", True
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

Public Sub RedrawFrameView()
On Error Resume Next
    If (m_imgFrameDisplay.Width <> picFrameDisplay.ScaleWidth) Or (m_imgFrameDisplay.Height <> picFrameDisplay.ScaleHeight) Then
        m_imgFrameDisplay.Resize picFrameDisplay.ScaleWidth, picFrameDisplay.ScaleHeight
    End If
    With m_imgFrameDisplay
        .Clear SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
        SelectedFrame.Image.MatteColor = SelectedFrame.MatteColor
        Select Case dtFrames.SelectedTab.key
        Case "Rectangle"
            .Blit , , SelectedFrame.Image, 0.5, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
            .Fill SelectedFrame.Rectangle, SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
            .Fill SelectedFrame.Rectangle, SetAlpha(SwapChannels(GetSystemColor(SystemColor_Highlight), Red, Blue), 127), RenderMode_SourceAlpha
            .Blit SelectedFrame.Rectangle, SelectedFrame.Rectangle, SelectedFrame.Image, 1, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
            txtFrameProperty.Text = SelectedFrame.Rectangle.Class_ToString()
        Case "Alignment"
            .Blit F2Rect(0, 0, SelectedFrame.Rectangle.Width, SelectedFrame.Rectangle.Height, False), SelectedFrame.Rectangle, SelectedFrame.Image, 1, IIf(SelectedSprite.Effect = F2SB_Matte, BlitMode_Matte, BlitMode_SourceAlpha)
            .AntiAliasLine Array(SelectedFrame.XCenter, 0, SelectedFrame.XCenter, SelectedFrame.Rectangle.Height - 1), F2RGB(255, 0, 0, 255)
            .AntiAliasLine Array(0, SelectedFrame.YCenter, SelectedFrame.Rectangle.Width - 1, SelectedFrame.YCenter), F2RGB(255, 0, 0, 255)
            txtFrameProperty.Text = SelectedFrame.XCenter & ", " & SelectedFrame.YCenter
        Case Else
        End Select
    End With
    picFrameDisplay_Paint
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

Private Sub dtFrames_Resize()
On Error Resume Next
    Select Case LCase(Trim(dtFrames.SelectedTab.key))
    Case "options"
        insFrameOptions.Move (2) + dtFrames.Left, dtFrames.Top + 24, dtFrames.Width - 4, dtFrames.Height - 26
    Case "script"
        scFrame.Move (2) + dtFrames.Left, dtFrames.Top + 24, dtFrames.Width - 4, dtFrames.Height - 26
    Case "alignment", "rectangle"
        picFrameDisplay.Move (2) + dtFrames.Left, dtFrames.Top + 24, dtFrames.Width - 4, dtFrames.Height - 26 - txtFrameProperty.Height
        txtFrameProperty.Move picFrameDisplay.Left, picFrameDisplay.Top + picFrameDisplay.Height, picFrameDisplay.Width, txtFrameProperty.Height
    Case Else
    End Select
End Sub

Private Sub dtFrames_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    FramesViewChanged
End Sub

Private Sub dtPoses_Resize()
On Error Resume Next
    Select Case LCase(Trim(dtPoses.SelectedTab.key))
    Case "options"
        insPoseOptions.Move (2) + dtPoses.Left, dtPoses.Top + 24, dtPoses.Width - 4, dtPoses.Height - 26
    Case "frames"
        picFrames.Move (2) + dtPoses.Left, dtPoses.Top + 24, dtPoses.Width - 4, dtPoses.Height - 26
    Case Else
    End Select
End Sub

Private Sub dtPoses_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    PosesViewChanged
End Sub

Private Sub dtStates_Resize()
On Error Resume Next
    Select Case LCase(Trim(dtStates.SelectedTab.key))
    Case "options"
        insStateOptions.Move (2) + dtStates.Left, dtStates.Top + 24, dtStates.Width - 4, dtStates.Height - 26
    Case "poses"
        lstStatePoses.Move (2) + dtStates.Left, dtStates.Top + 24, dtStates.Width - 4, dtStates.Height - 26
    Case Else
    End Select
End Sub

Private Sub dtStates_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    StatesViewChanged
End Sub

Private Sub dtViews_Resize()
On Error Resume Next
    Select Case m_lngCurrentView
    Case View_Overview
        insOverview.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case View_States
        picStates.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case View_Poses
        picPoses.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case View_Script
        scSprite.Move (2) + dtViews.Left, dtViews.Top + 24, dtViews.Width - 4, dtViews.Height - 26
    Case Else
    End Select
End Sub

Private Sub dtViews_TabSelected(theTab As vbalDTab6.cTab)
On Error Resume Next
    m_lngCurrentView = CLng(Mid(theTab.key, 2))
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
    dtViews_Resize
    picSidebar_Resize
    ViewChanged
    RedrawSprites
    Redraw
End Sub

Private Sub Form_Deactivate()
On Error Resume Next
    DeallocateBuffers
End Sub

Private Sub Form_Load()
On Error Resume Next
'    vsSprites.Width = GetScrollbarSize(vsSprites)
    Set m_scSprites = New Fury2Sprites
    InitViews
    InitSplitters
    Form_Activate
    lstPoses.Clear
    lstStates.Clear
    lstStatePoses.Clear
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
    picContainer.Move 2, 2, Me.ScaleWidth - 4, Me.ScaleHeight - 4
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DefineMenu "Reload Images", "ReloadGraphics"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DestroyMenu "ReloadGraphics"
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
        NewValue = (lstFrames.ListIndex >= 0)
    Case "Poses"
        NewValue = (lstPoses.ListIndex >= 0)
    Case "States"
        NewValue = (lstStates.ListIndex >= 0)
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
        NewValue = (lstFrames.ListIndex >= 0) And (SelectedPose.Frames.Count > 1)
    Case "Poses"
        NewValue = (lstPoses.ListIndex >= 0) And (SelectedSprite.Poses.Count > 1)
    Case "States"
        NewValue = (lstStates.ListIndex >= 0) And (SelectedSprite.States.Count > 1)
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
        NewValue = (lstFrames.ListIndex >= 0) And (SelectedPose.Frames.Count > 1)
    Case "Poses"
        NewValue = (lstPoses.ListIndex >= 0) And (SelectedSprite.Poses.Count > 1)
    Case "States"
        NewValue = (lstStates.ListIndex >= 0) And (SelectedSprite.States.Count > 1)
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
        NewValue = ClipboardContainsFormat(SCF_SpritePose)
    Case "Frames"
        NewValue = ClipboardContainsFormat(SCF_SpriteFrame)
    Case "States"
        NewValue = ClipboardContainsFormat(SCF_SpriteState)
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
End Sub

Private Sub iEditingCommands_CanSelectNone(NewValue As Boolean)
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
        CopyState
    Case "Poses"
        CopyPose
    Case "Frames"
        CopyFrame
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
        CutState
    Case "Poses"
        CutPose
    Case "Frames"
        CutFrame
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
        DeleteFrame
    Case "Poses"
        DeletePose
    Case "States"
        DeleteState
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
        PasteState
    Case "Poses"
        PastePose
    Case "Frames"
        PasteFrame
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

Private Sub lstFrames_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyUp Or KeyCode = vbKeyDown Then
        m_lngSelectedFrame = lstFrames.ListIndex + 1
        FramesViewChanged
    End If
End Sub

Private Sub lstFrames_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_ptMouse As POINTAPI
Dim l_lngIndex As Long
    If Y < 0 Then Exit Sub
    If Y >= (lstFrames.Height * Screen.TwipsPerPixelY) Then Exit Sub
    lstFrames.SetFocus
    GetCursorPos l_ptMouse
    l_lngIndex = LBItemFromPt(lstFrames.hwnd, l_ptMouse.X, l_ptMouse.Y, False)
    If l_lngIndex <> m_lngSelectedFrame - 1 Then
        If l_lngIndex > -1 Then
            If Button = 1 Then lstFrames.ListIndex = l_lngIndex
            m_lngSelectedFrame = lstFrames.ListIndex + 1
        End If
        FramesViewChanged
    End If
    Editor.ActionUpdate
    If Button = 2 Then
        Select Case QuickShowMenu(lstFrames, X, Y, _
        ListContext(), _
        frmIcons.ilContextMenus)
        Case 1
            InsertFrame l_lngIndex
        Case 3
            CutFrame
        Case 4
            CopyFrame
        Case 5
            PasteFrame l_lngIndex
        Case 6
            DeleteFrame
        Case Else
        End Select
    End If
End Sub

Private Sub lstFrames_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button = 1 Then
        If m_lngSelectedFrame <> lstFrames.ListIndex + 1 Then
            lstFrames_MouseDown Button, Shift, X, Y
            DoEvents
        End If
    End If
End Sub

Private Sub lstPoses_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    If KeyCode = vbKeyUp Or KeyCode = vbKeyDown Then
        m_lngSelectedPose = lstPoses.ListIndex + 1
        PosesViewChanged
    End If
End Sub

Private Sub lstPoses_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_ptMouse As POINTAPI
Dim l_lngIndex As Long
    If Y < 0 Then Exit Sub
    If Y >= (lstPoses.Height * Screen.TwipsPerPixelY) Then Exit Sub
    lstPoses.SetFocus
    GetCursorPos l_ptMouse
    l_lngIndex = LBItemFromPt(lstPoses.hwnd, l_ptMouse.X, l_ptMouse.Y, False)
    If l_lngIndex <> m_lngSelectedPose - 1 Then
        If Button = 1 Then lstPoses.ListIndex = l_lngIndex
        m_lngSelectedPose = lstPoses.ListIndex + 1
        PosesViewChanged
    End If
    Editor.ActionUpdate
    If Button = 2 Then
        Select Case QuickShowMenu(lstPoses, X, Y, _
        ListContext(), _
        frmIcons.ilContextMenus)
        Case 1
            InsertPose l_lngIndex
        Case 3
            CutPose
        Case 4
            CopyPose
        Case 5
            PastePose l_lngIndex
        Case 6
            DeletePose
        Case Else
        End Select
    End If
End Sub

Private Sub lstPoses_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button = 1 Then
        lstPoses_MouseDown Button, Shift, X, Y
    End If
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

Private Sub lstStates_KeyUp(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    m_lngSelectedState = lstStates.ListIndex + 1
    StatesViewChanged
End Sub

Private Sub lstStates_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_ptMouse As POINTAPI
Dim l_lngIndex As Long
    If Y < 0 Then Exit Sub
    If Y >= (lstStates.Height * Screen.TwipsPerPixelY) Then Exit Sub
    lstStates.SetFocus
    GetCursorPos l_ptMouse
    l_lngIndex = LBItemFromPt(lstStates.hwnd, l_ptMouse.X, l_ptMouse.Y, False)
    If l_lngIndex <> m_lngSelectedState - 1 Then
        If Button = 1 Then lstStates.ListIndex = l_lngIndex
        m_lngSelectedState = lstStates.ListIndex + 1
        StatesViewChanged
    End If
    Editor.ActionUpdate
    If Button = 2 Then
        Select Case QuickShowMenu(lstStates, X, Y, _
        ListContext(), _
        frmIcons.ilContextMenus)
        Case 1
            InsertState l_lngIndex
        Case 3
            CutState
        Case 4
            CopyState
        Case 5
            PasteState l_lngIndex
        Case 6
            DeleteState
        Case Else
        End Select
    End If
End Sub

Private Sub lstStates_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    If Button = 1 Then
        lstStates_MouseDown Button, Shift, X, Y
    End If
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

Private Sub picFrameDisplay_Paint()
On Error Resume Next
    CopyImageToDC picFrameDisplay.hdc, m_imgFrameDisplay.Rectangle, m_imgFrameDisplay
End Sub

Private Sub picFrameDisplay_Resize()
On Error Resume Next
    RedrawFrameView
End Sub

Private Sub picFrames_Resize()
On Error Resume Next
    lstFrames.Move 2, 2, picFrames.ScaleWidth - 4, ClipValue(picFrames.ScaleHeight / 4, 32, 150)
    dtFrames.Move 2, lstFrames.Top + lstFrames.Height + 2, picFrames.ScaleWidth - 4, picFrames.ScaleHeight - (lstFrames.Top + lstFrames.Height + 4)
End Sub

Private Sub picSidebar_Resize()
On Error GoTo 0
    picSprites.Move 0, 0, picSidebar.ScaleWidth - vsSprites.Width, picSidebar.ScaleHeight
End Sub

Private Sub picSprites_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
Dim l_sprSprite As Fury2Sprite
Dim l_rctSprite As Fury2Rect
Dim l_rctText As Win32.Rect
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
        Select Case QuickShowMenu(picSprites, X * Screen.TwipsPerPixelX, Y * Screen.TwipsPerPixelY, _
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
    dtPoses.Move 2, lstPoses.Top + lstPoses.Height + 2, picPoses.ScaleWidth - 4, picPoses.ScaleHeight - (lstPoses.Top + lstPoses.Height + 4)
End Sub

Private Sub picStates_Resize()
On Error Resume Next
    lstStates.Move 2, 2, picStates.ScaleWidth - 4, ClipValue(picStates.ScaleHeight / 4, 32, 150)
    dtStates.Move 2, lstStates.Top + lstStates.Height + 2, picStates.ScaleWidth - 4, picStates.ScaleHeight - (lstStates.Top + lstStates.Height + 4)
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

Private Sub vsSprites_Change()
    RedrawSprites
End Sub


Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

