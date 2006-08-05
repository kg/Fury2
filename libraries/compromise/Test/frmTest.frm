VERSION 5.00
Begin VB.Form frmTest 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Compromise Test"
   ClientHeight    =   3615
   ClientLeft      =   45
   ClientTop       =   465
   ClientWidth     =   3045
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
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   241
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   203
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdTest 
      Caption         =   "Test"
      Height          =   360
      Left            =   1530
      TabIndex        =   6
      Top             =   795
      Width           =   1500
   End
   Begin VB.TextBox txtLog 
      Height          =   2415
      Left            =   15
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   5
      Top             =   1185
      Width           =   3015
   End
   Begin VB.CommandButton cmdCreate 
      Caption         =   "Create"
      Height          =   360
      Left            =   15
      TabIndex        =   4
      Top             =   795
      Width           =   1500
   End
   Begin VB.CommandButton cmdUnregister 
      Caption         =   "Unregister"
      Height          =   360
      Left            =   1530
      TabIndex        =   3
      Top             =   405
      Width           =   1500
   End
   Begin VB.CommandButton cmdRegister 
      Caption         =   "Register"
      Height          =   360
      Left            =   15
      TabIndex        =   2
      Top             =   405
      Width           =   1500
   End
   Begin VB.CommandButton cmdUnInit 
      Caption         =   "Uninitialize"
      Height          =   360
      Left            =   1530
      TabIndex        =   1
      Top             =   15
      Width           =   1500
   End
   Begin VB.CommandButton cmdInit 
      Caption         =   "Initialize"
      Height          =   360
      Left            =   15
      TabIndex        =   0
      Top             =   15
      Width           =   1500
   End
End
Attribute VB_Name = "frmTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Sub LogPrint(Text)
On Error Resume Next
Dim l_strText As String
    l_strText = CStr(Text) + vbCrLf
    txtLog.SelStart = Len(txtLog.Text)
    txtLog.SelLength = 0
    txtLog.SelText = l_strText
    txtLog.SelStart = Len(txtLog.Text)
    txtLog.SelLength = 0
End Sub

Private Sub cmdCreate_Click()
On Error Resume Next
Dim X As Object
    Err.Clear
    Set X = CreateObject("VBScript.VBScript")
    If Err.Number = 0 Then
        LogPrint "Create Successful: " & ObjPtr(X)
    Else
        LogPrint "Create Failed: " & Err.Description & " (" & Err.Number & ")"
    End If
    Err.Clear
    Set X = Nothing
End Sub

Private Sub cmdInit_Click()
On Error Resume Next
Dim l_lngResult As Long
    l_lngResult = Compromise.Initialize
    LogPrint "Initialize()=" & l_lngResult
End Sub

Private Sub cmdRegister_Click()
On Error Resume Next
Dim l_lngResult As Long
    l_lngResult = Compromise.Register("C:\Windows\System32\vbscript.dll")
    LogPrint "Register()=" & l_lngResult
End Sub

Private Sub cmdTest_Click()
On Error Resume Next
    Compromise.Register "C:\Windows\System32\vbscript.dll"
    Compromise.Register "J:\development\binary\sys\scriptengine.dll"
    Compromise.Register "J:\development\binary\sys\script2.dll"
End Sub

Private Sub cmdUnInit_Click()
On Error Resume Next
Dim l_lngResult As Long
    l_lngResult = Compromise.UnInitialize
    LogPrint "Uninitialize()=" & l_lngResult
End Sub

Private Sub cmdUnregister_Click()
On Error Resume Next
Dim l_lngResult As Long
    l_lngResult = Compromise.Unregister("C:\Windows\System32\vbscript.dll")
    LogPrint "Unregister()=" & l_lngResult
End Sub

Private Sub Form_Load()
    LogPrint "Ready"
End Sub
