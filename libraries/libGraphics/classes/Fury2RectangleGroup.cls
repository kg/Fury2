VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2RectangleGroup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private mCol As Collection

Public Function Intersect(OtherRect) As Boolean
On Error Resume Next
Dim m_rctOther As Fury2Rect
Dim m_rctCurrent As Fury2Rect
    Set OtherRect = m_rctOther
    If OtherRect Is Nothing Then Exit Function
    For Each m_rctCurrent In mCol
        If m_rctCurrent.Intersect(m_rctOther) Then
            Intersect = True
            Exit Function
        End If
    Next m_rctCurrent
End Function

Public Sub Add(Obj)
On Error Resume Next
Dim m_rctRect As Fury2Rect
    Set m_rctRect = Obj
    If m_rctRect Is Nothing Then Exit Sub
    mCol.Add m_rctRect
End Sub

Public Sub Remove(Index)
On Error Resume Next
    mCol.Remove CLng(Index)
End Sub

Public Property Get Item(Index) As Fury2Rect
Attribute Item.VB_UserMemId = 0
Attribute Item.VB_MemberFlags = "200"
On Error Resume Next
    Set Item = mCol.Item(CLng(Index))
End Property

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
    Set mCol = Nothing
End Sub
