VERSION 5.00
Begin VB.Form frmInvoke 
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   30
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   30
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   2
   ShowInTaskbar   =   0   'False
   Visible         =   0   'False
   Begin VB.Timer tmrInvoke 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   -585
      Top             =   -225
   End
End
Attribute VB_Name = "frmInvoke"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Queue As New Collection

Private Sub tmrInvoke_Timer()
On Error Resume Next
Dim l_evtItem As ngEvent
    If Queue.Count <= 1 Then
        tmrInvoke.Enabled = False
    End If
    Set l_evtItem = Queue(1)
    Queue.Remove 1
    l_evtItem.Invoke
End Sub
