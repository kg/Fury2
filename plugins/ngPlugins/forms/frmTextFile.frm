VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Begin VB.Form frmTextFile 
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
   Icon            =   "frmTextFile.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   212
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtText 
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3135
      HideSelection   =   0   'False
      Left            =   0
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   0
      Top             =   0
      Width           =   4680
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   30
      Top             =   30
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmTextFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements iExtendedForm
Implements iDocument

Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = m_fpgPlugin
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
    Set m_fpgPlugin = RHS
End Property

Friend Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.txt", GetTitle(Name))
End Sub

Private Sub Form_Resize()
On Error Resume Next
    txtText.Move 0, 0, Me.ScaleWidth, Me.ScaleHeight
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
    Kill Filename
    Err.Clear
    l_lngFileHandle = FreeFile
    Open Filename For Binary Access Write As #l_lngFileHandle
        Put #l_lngFileHandle, 1, txtText.Text
    Close #l_lngFileHandle
    iDocument_Save = (Err.Number = 0)
    If iDocument_Save Then
        SetFilename Filename
    End If
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Text File"
End Property

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = True
End Property

Private Sub txtText_Change()

End Sub
