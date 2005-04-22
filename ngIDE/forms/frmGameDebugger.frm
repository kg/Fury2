VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{EF59A10B-9BC4-11D3-8E24-44910FC10000}#11.0#0"; "vbalEdit.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#8.10#0"; "ngUI.ocx"
Begin VB.Form frmGameDebugger 
   BorderStyle     =   0  'None
   Caption         =   "Game Debugger"
   ClientHeight    =   5910
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   7050
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   ScaleHeight     =   394
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   470
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picConsole 
      BackColor       =   &H80000014&
      BorderStyle     =   0  'None
      Height          =   2385
      Left            =   90
      ScaleHeight     =   159
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   130
      TabIndex        =   1
      Top             =   435
      Width           =   1950
      Begin VB.TextBox txtConsoleInput 
         BeginProperty Font 
            Name            =   "Courier New"
            Size            =   11.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   45
         TabIndex        =   3
         Top             =   1215
         Width           =   1860
      End
      Begin vbalEdit.vbalRichEdit reConsole 
         Height          =   1170
         Left            =   45
         TabIndex        =   2
         Top             =   30
         Width           =   1800
         _ExtentX        =   3175
         _ExtentY        =   2064
         Version         =   1
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Courier New"
            Size            =   11.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         BackColor       =   -2147483628
         ForeColor       =   -2147483630
         ViewMode        =   1
         Border          =   0   'False
         TextLimit       =   16777216
         AutoURLDetect   =   0   'False
         ScrollBars      =   3
      End
   End
   Begin VB.Timer tmrRefreshValues 
      Interval        =   5000
      Left            =   2925
      Top             =   2715
   End
   Begin ngIDE.ObjectInspector insVariables 
      Height          =   3600
      Left            =   2070
      TabIndex        =   0
      Top             =   2130
      Visible         =   0   'False
      Width           =   4800
      _ExtentX        =   8467
      _ExtentY        =   6350
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin ngUI.ngTabStrip tsViews 
      Height          =   4350
      Left            =   1335
      TabIndex        =   4
      Top             =   75
      Width           =   4755
      _ExtentX        =   8387
      _ExtentY        =   7673
   End
End
Attribute VB_Name = "frmGameDebugger"
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
Implements iExtendedForm
Implements iDocument

Public Enum TextColors
    TextColors_Message
    TextColors_DebugMessage
    TextColors_CriticalError
    TextColors_Error
End Enum

Dim m_strCurrentView As String
Dim m_ctlCurrentView As Control

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
End Property

Public Function TextColor(Color As TextColors) As Long
On Error Resume Next
    Select Case Color
    Case TextColors_Message
        TextColor = vbButtonText
    Case TextColors_DebugMessage
        TextColor = BlendColors(GetSystemColor(SystemColor_Button_Text), GetSystemColor(SystemColor_Button_Face), 127)
    Case TextColors_CriticalError
        TextColor = BlendColors(GetSystemColor(SystemColor_Button_Text), RGB(255, 0, 0), 192)
    Case TextColors_Error
        TextColor = BlendColors(GetSystemColor(SystemColor_Button_Text), RGB(255, 0, 0), 96)
    Case Else
    End Select
End Function

Public Sub ShowConsole()
On Error Resume Next
    tsViews.SelectTab 1
End Sub

Public Sub ConsoleAppend(ByRef Text As String, Optional ByVal Color As Long = vbButtonText)
On Error Resume Next
Dim l_lngOldLength As Long, l_lngLength As Long
    reConsole.Redraw = False
    l_lngLength = Len(reConsole.Contents(SF_TEXT))
    reConsole.SetSelection l_lngOldLength - 1, l_lngOldLength - 1
    reConsole.SetFont reConsole.Font, Color, ercTextNormal, False, ercSetFormatSelection
    reConsole.InsertContents SF_TEXT, Text & vbCrLf
    l_lngLength = Len(reConsole.Contents(SF_TEXT))
    reConsole.Redraw = True
    reConsole.SetSelection l_lngLength, l_lngLength
End Sub

Public Sub InitViews()
On Error Resume Next
    tsViews.Tabs.AddNew "Console", "Console"
    tsViews.Tabs.AddNew "Variables", "Variables"
End Sub

Private Sub tsViews_Resize()
On Error Resume Next
    m_ctlCurrentView.Move (2) + tsViews.Left, tsViews.IdealHeight + tsViews.Top + 1, tsViews.Width - 4, tsViews.Height - (tsViews.IdealHeight + 3)
End Sub

Public Sub ViewChanged()
On Error Resume Next
    m_ctlCurrentView.Visible = False
    Set m_ctlCurrentView = Nothing
    Select Case LCase(Trim(m_strCurrentView))
    Case "variables"
        Set m_ctlCurrentView = insVariables
    Case "console"
        Set m_ctlCurrentView = picConsole
    Case Else
    End Select
    tsViews_Resize
    RefreshView
    m_ctlCurrentView.Visible = True
    m_ctlCurrentView.ZOrder
End Sub

Public Sub RefreshView()
On Error Resume Next
Dim l_seEngine As ScriptEngine
    Select Case LCase(Trim(m_strCurrentView))
    Case "variables"
        Set l_seEngine = g_dbgDebugger.GameEngine.ScriptEngine
        insVariables.Inspect l_seEngine.Namespace, "Variables", True, False, True
    Case Else
    End Select
End Sub

Private Sub tsViews_TabSelected(theTab As ngTab)
On Error Resume Next
    m_strCurrentView = theTab.key
    ViewChanged
End Sub

Private Sub Form_Load()
On Error Resume Next
    InitViews
    insVariables.InspectAny = True
End Sub

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Private Sub Form_Activate()
On Error Resume Next
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
End Sub

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = Nothing
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
End Property

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
End Sub

Private Sub Form_Resize()
On Error Resume Next
    tsViews.Move 2, 2, Me.ScaleWidth - 4, Me.ScaleHeight - 4
End Sub

Private Property Get iDocument_CanSave() As Boolean
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = g_dbgDebugger.GameEngine.GameName
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Game Debugger"
End Property

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = False
End Property

Private Sub picConsole_Resize()
On Error Resume Next
    reConsole.Move 0, 0, picConsole.ScaleWidth, picConsole.ScaleHeight - txtConsoleInput.Height
    txtConsoleInput.Move 0, reConsole.Top + reConsole.Height, reConsole.Width, txtConsoleInput.Height
End Sub

Private Sub tmrRefreshValues_Timer()
On Error Resume Next
    If m_strCurrentView = "Variables" Then
        If Not GameIsPaused Then
'            If GetActiveWindow() = frmMain.hwnd Then
                insVariables.RefreshValues
'            End If
        End If
    End If
End Sub

Private Sub txtConsoleInput_KeyPress(KeyAscii As Integer)
On Error Resume Next
    If KeyAscii = 13 Then
        KeyAscii = 0
        Err.Clear
        g_dbgDebugger.Engine.ConsoleCommand txtConsoleInput.Text
        If Err = 0 Then
            txtConsoleInput.Text = ""
        End If
    End If
End Sub
