VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cBlockingUndoEntry"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements iUndoEntry

Public Map As Fury2Map
Public Layer As Long
Private m_lnLines() As FLine

Public Property Let Lines(NewLines() As FLine)
    m_lnLines = NewLines()
End Property

Public Property Get Lines() As FLine()
    Lines = m_lnLines
End Property

Private Function iUndoEntry_Apply() As Boolean
On Error Resume Next
    If Map Is Nothing Then Exit Function
    If Layer < 1 Then Exit Function
    If Layer > Map.Layers.Count Then Exit Function
    Map.Layers(Layer).CollisionLines = m_lnLines
    Screen.MousePointer = 11
    Screen.MousePointer = 0
    iUndoEntry_Apply = True
End Function

Private Function iUndoEntry_CreateReverse() As iUndoEntry
On Error Resume Next
Dim l_undReverser As New cBlockingUndoEntry
    With l_undReverser
        .Layer = Layer
        Set .Map = Me.Map
        .Lines = .Map.Layers(.Layer).CollisionLines
    End With
    Set iUndoEntry_CreateReverse = l_undReverser
End Function

