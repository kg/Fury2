VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iEditingCommands"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Sub CanPaste(ByRef NewValue As Boolean)
End Sub

Public Sub CanCopy(ByRef NewValue As Boolean)
End Sub

Public Sub CanCut(ByRef NewValue As Boolean)
End Sub

Public Sub CanDelete(ByRef NewValue As Boolean)
End Sub

Public Sub CanSelectAll(ByRef NewValue As Boolean)
End Sub

Public Sub CanSelectNone(ByRef NewValue As Boolean)
End Sub

Public Sub CanUndo(ByRef NewValue As Boolean)
End Sub

Public Sub CanRedo(ByRef NewValue As Boolean)
End Sub

Public Sub Paste()
End Sub

Public Sub Copy()
End Sub

Public Sub Cut()
End Sub

Public Sub Delete()
End Sub

Public Sub SelectAll()
End Sub

Public Sub SelectNone()
End Sub

Public Sub Undo()
End Sub

Public Sub Redo()
End Sub

