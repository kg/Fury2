VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Begin VB.Form frmCommandBrowser 
   BorderStyle     =   0  'None
   Caption         =   "Command Browser"
   ClientHeight    =   3180
   ClientLeft      =   0
   ClientTop       =   -15
   ClientWidth     =   4665
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
   Icon            =   "frmCommandBrowser.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   212
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   311
   ShowInTaskbar   =   0   'False
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   15
      Top             =   2670
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmCommandBrowser"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements iExtendedForm
Implements iDocument

Public Plugin As CommandBrowser

Private Sub Form_Activate()
On Error Resume Next
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    Plugin.Notify_Close
End Sub

Private Property Get iDocument_Plugin() As ngInterfaces.iFileTypePlugin
On Error Resume Next
    Set iDocument_Plugin = Nothing
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iFileTypePlugin)
On Error Resume Next
'    Set m_fpgPlugin = RHS
End Property

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
End Sub

Private Sub Form_Resize()
On Error Resume Next
End Sub

Private Property Get iDocument_CanSave() As Boolean
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = "Command Browser"
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Command Browser"
End Property

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property
