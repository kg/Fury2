VERSION 5.00
Object = "*\A..\..\..\CREATE~1\CONTRO~2\MDIActiveX.vbp"
Begin VB.Form Form2 
   Caption         =   "Editor ohne eigenen Menü"
   ClientHeight    =   3195
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows-Standard
   Begin sMDIinActiveX.MDIActiveX MDIActiveX1 
      Left            =   2640
      Top             =   120
      _ExtentX        =   847
      _ExtentY        =   794
   End
   Begin VB.TextBox Text1 
      Height          =   825
      Left            =   0
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Beides
      TabIndex        =   0
      Text            =   "Form2.frx":0000
      Top             =   0
      Width           =   1590
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Private Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

Private Sub Form_Resize()
Dim r As RECT

Debug.Print "Resize!!!"

  GetClientRect Me.hwnd, r
  Me.Text1.Width = Me.ScaleX(r.Right, vbPixels, Me.ScaleMode)
  Me.Text1.Height = Me.ScaleY(r.Bottom, vbPixels, Me.ScaleMode) - Me.Text1.Top
  
End Sub
