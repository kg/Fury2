VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SkinState"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public WindowSkin As Fury2WindowSkin
Public Font As Fury2Font
Public TextColor As Long
Public HighlightColor As Long
Public EdgeOffset As Long
Public Width As Long
Public TintColor As Long

Private Sub Class_Initialize()
On Error Resume Next
    TextColor = F2White
    HighlightColor = F2RGB(0, 0, 220, 127)
    EdgeOffset = 0
    Width = 15
    TintColor = F2White
End Sub
