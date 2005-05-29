VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2DeformationMesh"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_mpPoints() As MeshPoint
Private m_mshParam As MeshParam

Public Function GetParam() As MeshParam
On Error Resume Next
    GetParam = m_mshParam
End Function

Private Sub Class_Initialize()
On Error Resume Next
    ReDim m_mpPoints(0 To 1, 0 To 1)
    With m_mshParam
        .Width = 2
        .Height = 2
        .pData = VarPtr(m_mpPoints(0, 0))
    End With
End Sub

Public Sub Resize(ByVal Width As Long, ByVal Height As Long)
On Error Resume Next
    Width = ClipValue(Width, 1, 1024)
    Height = ClipValue(Height, 1, 1024)
    ReDim m_mpPoints(0 To Width - 1, 0 To Height - 1)
    With m_mshParam
        .Width = Width
        .Height = Height
        .pData = VarPtr(m_mpPoints(0, 0))
    End With
End Sub

Public Property Get Width() As Long
    Width = m_mshParam.Width
End Property

Public Property Get Height() As Long
    Height = m_mshParam.Height
End Property

Public Property Get Values() As Variant
On Error Resume Next
Dim l_varValues As Variant
Dim l_lngX As Long, l_lngY As Long
    ReDim l_varValues(0 To Width - 1, 0 To Height - 1)
    For l_lngY = LBound(m_mpPoints, 2) To UBound(m_mpPoints, 2)
        For l_lngX = LBound(m_mpPoints, 1) To UBound(m_mpPoints, 1)
            With m_mpPoints(l_lngX, l_lngY)
                l_varValues(l_lngX, l_lngY) = Array(.X, .Y)
            End With
        Next l_lngX
    Next l_lngY
    Values = l_varValues
End Property

Public Property Let Values(ByRef NewValues As Variant)
On Error Resume Next
Dim l_varValues As Variant
Dim l_lngX As Long, l_lngY As Long
    Resize UBound(NewValues, 1) + 1, UBound(NewValues, 2) + 1
    For l_lngY = LBound(m_mpPoints, 2) To UBound(m_mpPoints, 2)
        For l_lngX = LBound(m_mpPoints, 1) To UBound(m_mpPoints, 1)
            With m_mpPoints(l_lngX, l_lngY)
                .X = NewValues(l_lngX, l_lngY)(0)
                .Y = NewValues(l_lngX, l_lngY)(1)
            End With
        Next l_lngX
    Next l_lngY
End Property

Public Sub RandomizeAll(ByVal XOffset As Single, ByVal YOffset As Single, ByVal XScale As Single, ByVal YScale As Single)
On Error Resume Next
    RandomizeMesh m_mshParam, XOffset, YOffset, XScale, YScale
End Sub

Public Sub ScaleAll(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    ScaleMesh m_mshParam, X, Y
End Sub

Public Sub TranslateAll(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    TranslateMesh m_mshParam, X, Y
End Sub

Public Sub SetAll(ByVal X As Single, ByVal Y As Single)
On Error Resume Next
    SetMesh m_mshParam, X, Y
End Sub

Public Property Get XValue(ByVal X As Long, ByVal Y As Long) As Single
On Error Resume Next
    If X < 0 Or Y < 0 Or X >= Width Or Y >= Height Then
        XValue = 0
    Else
        XValue = m_mpPoints(X, Y).X
    End If
End Property

Public Property Let XValue(ByVal X As Long, ByVal Y As Long, ByVal NewValue As Single)
On Error Resume Next
    If X < 0 Or Y < 0 Or X >= Width Or Y >= Height Then Exit Property
    m_mpPoints(X, Y).X = NewValue
End Property

Public Property Get YValue(ByVal X As Long, ByVal Y As Long) As Single
On Error Resume Next
    If X < 0 Or Y < 0 Or X >= Width Or Y >= Height Then
        YValue = 0
    Else
        YValue = m_mpPoints(X, Y).Y
    End If
End Property

Public Property Let YValue(ByVal X As Long, ByVal Y As Long, ByVal NewValue As Single)
On Error Resume Next
    If X < 0 Or Y < 0 Or X >= Width Or Y >= Height Then Exit Property
    m_mpPoints(X, Y).Y = NewValue
End Property

Public Property Get Value(ByVal X As Long, ByVal Y As Long) As Variant
Attribute Value.VB_UserMemId = 0
On Error Resume Next
    If X < 0 Or Y < 0 Or X >= Width Or Y >= Height Then
        Value = Array(0, 0)
    Else
        With m_mpPoints(X, Y)
            Value = Array(.X, .Y)
        End With
    End If
End Property

Public Property Let Value(ByVal X As Long, ByVal Y As Long, ByRef NewValue As Variant)
On Error Resume Next
    If X < 0 Or Y < 0 Or X >= Width Or Y >= Height Then Exit Property
    With m_mpPoints(X, Y)
        .X = CSng(NewValue(0))
        .Y = CSng(NewValue(1))
    End With
End Property

