VERSION 5.00
Object = "*\A..\..\..\CREATE~1\CONTRO~2\MDIActiveX.vbp"
Begin VB.MDIForm MDIForm1 
   BackColor       =   &H8000000C&
   Caption         =   "MDIActiveX Demo Programm"
   ClientHeight    =   6645
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   10200
   Icon            =   "MDIForm1.frx":0000
   LinkTopic       =   "MDIForm1"
   StartUpPosition =   3  'Windows-Standard
   Begin sMDIinActiveX.MDIActiveX MDIActiveX1 
      Left            =   840
      Top             =   600
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin VB.Menu Active 
      Caption         =   "ActiveX-DLL"
      Begin VB.Menu Laden 
         Caption         =   "&Laden"
         Shortcut        =   ^L
      End
      Begin VB.Menu Löschen 
         Caption         =   "Lö&schen"
         Shortcut        =   ^S
      End
      Begin VB.Menu editor 
         Caption         =   "&Editor"
      End
      Begin VB.Menu trennung 
         Caption         =   "-"
      End
      Begin VB.Menu Normal 
         Caption         =   "Normales MDIChild"
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
      Begin VB.Menu del 
         Caption         =   "EURO Felder löschen"
      End
   End
   Begin VB.Menu Hilfe 
      Caption         =   "Hilfe"
      Begin VB.Menu About 
         Caption         =   "About"
         Shortcut        =   {F1}
      End
      Begin VB.Menu Copyright 
         Caption         =   "Copyright"
      End
      Begin VB.Menu Version 
         Caption         =   "Version"
      End
   End
End
Attribute VB_Name = "MDIForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
' ----------------------------------------------------------- '
'                                                             '
' Demo Programm zum sMDIActiveX.ocx Control                   '
'                                                             '
' Copyright (c)2000 by Michael Wallner                        '
' http;//wallner.homepage.com                                 '
' eMail: michael.wallner@iname.com                            '
'                                                             '
' ----------------------------------------------------------- '

Public Sub LadeEditor()
Dim editor As TestMDIActiveX
 
   Set editor = New TestMDIActiveX
   editor.ShowForm2

End Sub

Public Sub LadeRechner()
Dim Rechner As TestMDIActiveX
 
   Set Rechner = New TestMDIActiveX
   Rechner.ShowForm1

End Sub

Public Sub LadeNormal()
Dim f As MDIChild
  
  Set f = New MDIChild
  f.Show

End Sub

Public Sub Entladen()
Dim f As Form
On Error Resume Next
  
    Set f = Me.MDIActiveX1.ActiveForm
    Unload f

End Sub

Public Sub ShowAbout()
  MsgBox "MDIActiveX.ocx Demo Programm" & vbCrLf & "(c)2000 Michael Wallner"
End Sub

Public Sub enumChild()
Dim f As Form

    For Each f In Me.MDIActiveX1.Forms
      If Left(f.Caption, 4) = "EURO" Then
        f.text1.Text = ""
      End If
    Next
    
End Sub

Private Sub About_Click()
  ShowAbout
End Sub

Private Sub Cascade_Click()
   Me.Arrange vbCascade
End Sub

Private Sub Copyright_Click()
  MsgBox Me.MDIActiveX1.Copyright
End Sub

Private Sub del_Click()
  Me.enumChild
End Sub

Private Sub editor_Click()
  Me.LadeEditor
End Sub

Private Sub Laden_Click()
  LadeRechner
End Sub


Private Sub Löschen_Click()
  Entladen
End Sub

Private Sub Normal_Click()
  LadeNormal
End Sub

Private Sub Tile_Click()
  Me.Arrange vbTileHorizontal
End Sub

Private Sub Version_Click()
  MsgBox "MDIActiveX.ocx Version " & Me.MDIActiveX1.Version
End Sub
