VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngMenuParent"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Property Get TextWidth() As Long
End Property

Public Property Get TextHeight() As Long
End Property

Public Property Get Form() As Object
End Property

Public Property Get MenuItem() As ngMenuItem
End Property

Public Property Get Children() As ngMenuItems
End Property

Public Sub Reflow()
End Sub

Public Sub Redraw()
End Sub
