VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iToolbarHandler"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Enum ToolbarPositions
    TBP_Top = 0
    TBP_Right = 1
    TBP_Bottom = 2
    TBP_Left = 3
End Enum

Public Type ToolbarOptions
    Title As String
    XDockable As Boolean
    YDockable As Boolean
    Undockable As Boolean
    Closable As Boolean
    FullRow As Boolean
    ShowChevron As Boolean
    XWidth As Long
    XHeight As Long
    YWidth As Long
    YHeight As Long
    DefaultPosition As ToolbarPositions
End Type

Public Sub AddToolbar(ByVal Name As String, ByVal Window As Long, ByRef Options As ToolbarOptions)
On Error Resume Next
End Sub

Public Sub RemoveToolbar(ByVal Name As String)
On Error Resume Next
End Sub

Public Sub ResizeToolbar(ByVal Name As String, ByVal Window As Long, ByRef Options As ToolbarOptions)
On Error Resume Next
End Sub
