VERSION 5.00
Object = "*\A..\..\..\CREATE~1\CONTRO~2\MDIActiveX.vbp"
Begin VB.Form Form1 
   Caption         =   "EURO Rechner 1.0"
   ClientHeight    =   3135
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   4905
   Icon            =   "Test.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   ScaleHeight     =   3135
   ScaleWidth      =   4905
   StartUpPosition =   3  'Windows-Standard
   Begin sMDIinActiveX.MDIActiveX MDIActiveX1 
      Left            =   3120
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Verkleinern..."
      Height          =   330
      Left            =   2295
      TabIndex        =   8
      Top             =   1215
      Width           =   1635
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Vergrößern..."
      Height          =   330
      Left            =   315
      TabIndex        =   7
      Top             =   1215
      Width           =   1635
   End
   Begin VB.CommandButton Command3 
      Caption         =   "EURO Rechner beenden..."
      Height          =   375
      Left            =   315
      TabIndex        =   5
      Top             =   1665
      Width           =   3615
   End
   Begin VB.CommandButton Command2 
      Caption         =   "&Caption ändern..."
      Height          =   330
      Left            =   2295
      TabIndex        =   4
      Top             =   765
      Width           =   1635
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&Icon ändern..."
      Height          =   330
      Left            =   315
      TabIndex        =   3
      Top             =   765
      Width           =   1635
   End
   Begin VB.PictureBox Picture1 
      Height          =   330
      Left            =   4005
      Picture         =   "Test.frx":0442
      ScaleHeight     =   270
      ScaleWidth      =   270
      TabIndex        =   2
      Top             =   225
      Visible         =   0   'False
      Width           =   330
   End
   Begin VB.TextBox Text1 
      Height          =   330
      Left            =   810
      TabIndex        =   0
      Text            =   "100.00"
      Top             =   90
      Width           =   2130
   End
   Begin VB.Label Label2 
      Caption         =   "Dieses Fenster wurde in einer ActiveX-DLL erstellt und besitzt ein eigenes Menü!"
      ForeColor       =   &H000000C0&
      Height          =   465
      Left            =   225
      TabIndex        =   6
      Top             =   2160
      Width           =   4020
   End
   Begin VB.Label Label1 
      Caption         =   "Betrag"
      Height          =   240
      Left            =   180
      TabIndex        =   1
      Top             =   135
      Width           =   600
   End
   Begin VB.Menu Active 
      Caption         =   "ActiveX-DLL"
      Begin VB.Menu Laden 
         Caption         =   "&Laden"
         Shortcut        =   ^L
      End
      Begin VB.Menu löschen 
         Caption         =   "Lö&schen"
         Shortcut        =   ^S
      End
      Begin VB.Menu Editor 
         Caption         =   "&Editor"
      End
      Begin VB.Menu trennung 
         Caption         =   "-"
      End
      Begin VB.Menu Normal 
         Caption         =   "Normales MDIChild"
      End
   End
   Begin VB.Menu umrechnen 
      Caption         =   "Umrechnen"
      Begin VB.Menu ats 
         Caption         =   "In ATS"
         Shortcut        =   {F11}
      End
      Begin VB.Menu EURO 
         Caption         =   "In EURO"
         Shortcut        =   {F12}
      End
   End
   Begin VB.Menu Fenster 
      Caption         =   "Fenster"
      WindowList      =   -1  'True
      Begin VB.Menu Cascade 
         Caption         =   "Cascade"
         Shortcut        =   {F5}
      End
      Begin VB.Menu Tile 
         Caption         =   "Tile"
         Shortcut        =   {F6}
      End
      Begin VB.Menu dummy 
         Caption         =   "-"
      End
      Begin VB.Menu felder 
         Caption         =   "EURO Felder Löschen"
      End
   End
   Begin VB.Menu Hilfe 
      Caption         =   "Hilfe"
      Begin VB.Menu About 
         Caption         =   "About"
         Shortcut        =   {F1}
      End
      Begin VB.Menu copyright 
         Caption         =   "Copyright"
      End
      Begin VB.Menu VersionOCX 
         Caption         =   "Version OCX"
      End
      Begin VB.Menu VersionDLL 
         Caption         =   "Version DLL"
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub About_Click()
  MsgBox "EURO Rechner 1.0"
End Sub

Private Sub ats_Click()
  MsgBox "Betrag in ATS = " & Val(Me.Text1) * 13.7603
  Me.Text1 = ""
End Sub

Private Sub Cascade_Click()
'Zugriff auf die MDIForm!
  Me.MDIActiveX1.MDIForm.Arrange vbCascade
End Sub

Private Sub Command1_Click()
  Me.Icon = Me.Picture1.Picture
End Sub

Private Sub Command2_Click()
  Me.Caption = "EURO Rechner / " & Now
End Sub

Private Sub Command3_Click()
  Unload Me
End Sub

Private Sub Command4_Click()
  Me.Width = Me.Width * 1.3
  Me.Height = Me.Height * 1.3
End Sub

Private Sub Command5_Click()
  Me.Width = Me.Width * 0.8
  Me.Height = Me.Height * 0.8
  
End Sub

Private Sub copyright_Click()
  MsgBox Me.MDIActiveX1.copyright
End Sub

Private Sub Editor_Click()
  
  Me.MDIActiveX1.MDIForm.ladeeditor
 
End Sub

Private Sub EURO_Click()
  MsgBox "Betrag in EURO = " & Val(Me.Text1) / 13.7603
  Me.Text1 = ""
End Sub

Private Sub felder_Click()
  Me.MDIActiveX1.MDIForm.enumchild
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
'Seit Version 1.0.2 unnötig!
'need not to do this in Version 1.0.2
''z.B. Strg+F6 verarbeiten....
'
'   Me.MDIActiveX1.EvalKeyDown KeyCode, Shift
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
'Sollte im Textfeld etwas stehen, so nachfragen!
'if there is something in text1 --> ask
  If Me.Text1 <> "" Then
    If MsgBox("Wirklich beenden?", vbYesNo, "EURO Rechner") <> vbYes Then
      Cancel = True
    End If
  End If
  
End Sub

Private Sub Laden_Click()
'Eine Public Sub in MDIForm1 aufrufen..
'Call a Public Sub of MDIForm1
  Me.MDIActiveX1.MDIForm.LadeRechner
End Sub

Private Sub Löschen_Click()
'Eine Public Sub in MDIForm1 aufrufen..
'Call a Public Sub of MDIForm1
  Me.MDIActiveX1.MDIForm.entladen
End Sub

Private Sub Normal_Click()
'Eine Public Sub in MDIForm1 aufrufen..
'Call a Public Sub of MDIForm1
  Me.MDIActiveX1.MDIForm.LadeNormal
End Sub

Private Sub Tile_Click()
'Zugriff auf die MDIForm!
  Me.MDIActiveX1.MDIForm.Arrange vbTileHorizontal
End Sub

Private Sub VersionDLL_Click()
'Eigene Menüpunkte
'Menus only in this form
  MsgBox "EURO Rechner Version " & App.Major & "." & App.Minor & "." & App.Revision
End Sub

Private Sub VersionOCX_Click()
  MsgBox "MDIActiveX.ocx Version " & Me.MDIActiveX1.Version
End Sub
