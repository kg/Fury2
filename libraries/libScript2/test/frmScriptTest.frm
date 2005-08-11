VERSION 5.00
Begin VB.Form frmScriptTest 
   Caption         =   "Form1"
   ClientHeight    =   3150
   ClientLeft      =   60
   ClientTop       =   390
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   3150
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   1740
      TabIndex        =   0
      Top             =   1335
      Width           =   1215
   End
End
Attribute VB_Name = "frmScriptTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim WithEvents m_seEngine As ScriptEngine
Attribute m_seEngine.VB_VarHelpID = -1

Private Sub Command1_Click()
    Set m_seEngine = New ScriptEngine
    Set m_seEngine.language = m_seEngine.LoadLanguage("VBScript")
    m_seEngine.Init Me.hWnd
    m_seEngine.AddObject "Form", Me
    m_seEngine.AddCode "' Hello, world!" & vbCrLf & _
        "' Class ThisIsAComment" & vbCrLf & _
        "Class ThisIsAClass " & vbCrLf & _
        "  Public ThisIsAVariable" & vbCrLf & _
        "  Public ThisIsVar2, _" & vbCrLf & _
        "  ThisIsVar3" & vbCrLf & _
        "End Class" & vbCrLf & _
        "Sub ThisIsASub(Arg1, Arg2  ,Arg3  )" & vbCrLf & _
        "  MsgBox ""Hello!""" & vbCrLf & _
        "End Sub" & vbCrLf & _
        "Function ThisIsAFunction(Arg)" & vbCrLf & _
        "  ThisIsAFunction = 42" & vbCrLf & _
        "End Function" & vbCrLf & _
        "MsgBox ""Script Loaded"""
    DoEvents
End Sub

Private Sub m_seEngine_Error(ByVal LineNumber As Long, ByVal Character As Long, Description As String)
    Debug.Print Description & " at line " & LineNumber
End Sub
