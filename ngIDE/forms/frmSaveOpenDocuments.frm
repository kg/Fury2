VERSION 5.00
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Object = "{462EF1F4-16AF-444F-9DEE-F41BEBEC2FD8}#1.1#0"; "vbalodcl6.ocx"
Begin VB.Form frmSaveOpenDocuments 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Confirm Close"
   ClientHeight    =   3060
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   5880
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmSaveOpenDocuments.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   204
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   392
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   4335
      TabIndex        =   4
      Top             =   960
      Width           =   1500
   End
   Begin vbalIml6.vbalImageList ilDocumentIcons 
      Left            =   5295
      Top             =   1380
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   24
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "Don't Save"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   4335
      TabIndex        =   3
      Top             =   540
      Width           =   1500
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "Save Selected"
      Default         =   -1  'True
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   4335
      TabIndex        =   2
      Top             =   120
      Width           =   1500
   End
   Begin VB.Frame fraDocuments 
      Caption         =   "Currently Open Documents"
      Height          =   3000
      Left            =   30
      TabIndex        =   0
      Top             =   30
      Width           =   4275
      Begin ODCboLst6.OwnerDrawComboList lstDocuments 
         Height          =   2700
         Left            =   75
         TabIndex        =   1
         Top             =   225
         Width           =   4110
         _ExtentX        =   7250
         _ExtentY        =   4763
         ExtendedUI      =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   -2147483630
         Style           =   5
         FullRowSelect   =   -1  'True
         MaxLength       =   0
         NoGrayWhenDisabled=   -1  'True
         NoDimWhenOutOfFocus=   -1  'True
      End
   End
End
Attribute VB_Name = "frmSaveOpenDocuments"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Cancelled As Boolean

Public Sub RefreshDocumentList()
On Error Resume Next
Dim l_docDocument As cChildManager, l_lngDocumentIndex As Long
Dim l_plgPlugin As iPlugin, l_lngIcon As Long
Dim l_icnIcon As IPictureDisp
    ilDocumentIcons.Clear
    With lstDocuments
        .ImageList = ilDocumentIcons.hIml
        .Clear
        l_lngDocumentIndex = 1
        For Each l_docDocument In frmMain.Documents
            With l_docDocument
                Set l_icnIcon = Nothing
                Set l_plgPlugin = Nothing
                Set l_plgPlugin = .Document.Plugin
                Set l_icnIcon = l_plgPlugin.Icon
                If l_icnIcon Is Nothing Then
                    l_lngIcon = 0
                Else
                    ilDocumentIcons.AddFromHandle l_icnIcon.Handle, Image_Icon, "ICON_" & l_icnIcon.Handle
                    l_lngIcon = ilDocumentIcons.ItemIndex("ICON_" & l_icnIcon.Handle) - 1
                End If
                lstDocuments.AddItemAndData " " & IIf(Trim(.Document.Filename) = "", .Form.Caption, GetTitle(.Document.Filename)), l_lngIcon, 2, , , l_lngDocumentIndex, , 18, eixLeft, eixVCentre
                lstDocuments.Selected(lstDocuments.ListCount - 1) = True
            End With
            l_lngDocumentIndex = l_lngDocumentIndex + 1
        Next l_docDocument
    End With
End Sub

Private Sub cmdCancel_Click()
On Error Resume Next
    Cancelled = True
    Me.Hide
End Sub

Private Sub cmdClose_Click()
On Error Resume Next
    Cancelled = False
    Me.Hide
End Sub

Private Sub cmdOK_Click()
On Error Resume Next
Dim l_docDocument As cChildManager
Dim l_lngDocuments As Long
    Cancelled = False
    For l_lngDocuments = 0 To lstDocuments.ListCount
        If lstDocuments.Selected(l_lngDocuments) Then
            frmMain.Documents(l_lngDocuments + 1).Save
        End If
    Next l_lngDocuments
    Me.Hide
End Sub

Private Sub Form_Activate()
On Error Resume Next
End Sub

Private Sub Form_Load()
On Error Resume Next
    RefreshDocumentList
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    lstDocuments.Clear
    lstDocuments.ImageList = 0
    ilDocumentIcons.Destroy
End Sub

Private Sub lstDocuments_Change()
On Error Resume Next
End Sub
