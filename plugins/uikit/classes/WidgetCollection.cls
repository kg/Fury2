VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "WidgetCollection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
'
'    Engine (Fury² Game Creation System Runtime Engine)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Softwarse Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Implements Fury2Object
Implements IVirtualFileSaveable

Private mCol As Collection
Public Parent As Object

Public Function FindByMethod(ByVal MethodName As String, ParamArray Parameters() As Variant) As Object
On Error Resume Next
Dim l_objObject As Object
Dim l_booResult As Boolean
    If mCol.Count < 1 Then Exit Function
    For Each l_objObject In mCol
        Err.Clear
        l_booResult = False
        Select Case UBound(Parameters)
        Case 0
            l_booResult = VBA.CallByName(l_l_objObjectl_objObjectect, MethodName, VbMethod, Parameters(0))
        Case 1
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1))
        Case 2
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2))
        Case 3
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3))
        Case 4
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4))
        Case 5
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5))
        Case 6
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6))
        Case 7
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6), Parameters(7))
        Case Else
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod)
        End Select
        If l_booResult Then
            Set FindByMethod = l_objObject
            Exit For
        End If
    Next l_objObject
    Err.Clear
End Function

Public Sub CallMethodWithParameters(ByVal MethodName As String, ParamArray Parameters() As Variant)
On Error Resume Next
Dim l_objObject As Object
    If mCol.Count < 1 Then Exit Sub
    For Each l_objObject In mCol
        Err.Clear
        Select Case UBound(Parameters)
        Case 0
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0))
        Case 1
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1))
        Case 2
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2))
        Case 3
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3))
        Case 4
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4))
        Case 5
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5))
        Case 6
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6))
        Case 7
            Call VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5), Parameters(6), Parameters(7))
        Case Else
            Call VBA.CallByName(l_objObject, MethodName, VbMethod)
        End Select
    Next l_objObject
    Err.Clear
End Sub

Public Sub CallMethod(MethodName As String, Optional IgnoreErrors As Boolean = False)
On Error Resume Next
Dim l_objObject As Object
    If mCol.Count < 1 Then Exit Sub
    For Each l_objObject In mCol
        Err.Clear
        CallByName l_objObject, MethodName, VbMethod
        If IgnoreErrors Then
        Else
            If Err <> 0 Then Exit Sub
        End If
    Next l_objObject
    Err.Clear
End Sub

Public Sub Add(Obj, Optional ByVal sKey, Optional ByVal AtIndex As Long = -1)
On Error Resume Next
Dim l_wdgObj As iWidget
    If Find(Obj) > 0 Then Exit Sub
    If IsMissing(sKey) Then
        If AtIndex = -1 Then
            mCol.Add Obj
        Else
            If mCol.Count = 0 Then
                mCol.Add Obj
            ElseIf AtIndex <= 1 Then
                mCol.Add Obj, , 1
            ElseIf mCol.Count > AtIndex Then
                mCol.Add Obj, , AtIndex
            Else
                mCol.Add Obj, , , AtIndex - 1
            End If
        End If
    Else
        sKey = LCase(Trim(CStr(sKey)))
        If AtIndex = -1 Then
            mCol.Add Obj, sKey
        Else
            If mCol.Count = 0 Then
                mCol.Add Obj, sKey
            ElseIf AtIndex <= 1 Then
                mCol.Add Obj, sKey, 1
            ElseIf mCol.Count > AtIndex Then
                mCol.Add Obj, sKey, AtIndex
            Else
                mCol.Add Obj, sKey, , AtIndex - 1
            End If
        End If
    End If
    Set Obj.Parent = Parent
    Err.Clear
End Sub

Public Property Get Item(ByVal vntIndexKey) As Object
Attribute Item.VB_UserMemId = 0
Attribute Item.VB_MemberFlags = "200"
On Error Resume Next
    If VarType(vntIndexKey) = vbString Then
        vntIndexKey = LCase(Trim(CStr(vntIndexKey)))
    End If
    Err.Clear
    Set Item = mCol(vntIndexKey)
    Err.Clear
End Property

Public Function Find(Var) As Long
On Error Resume Next
Dim m_lngItems As Long, m_objItem As Object
    If mCol.Count <= 0 Then Exit Function
    If Var Is Nothing Then Exit Function
    If IsObject(Var) Then
        m_lngItems = 1
        For Each m_objItem In mCol
            If m_objItem Is Var Then
                Find = m_lngItems
                Exit Function
            End If
            m_lngItems = m_lngItems + 1
        Next m_objItem
    Else
        For m_lngItems = 1 To mCol.Count
            If mCol(m_lngItems) = Var Then
                Find = m_lngItems
                Exit Function
            End If
        Next m_lngItems
    End If
End Function

Public Property Get Count() As Long
On Error Resume Next
    Count = mCol.Count
End Property

Public Sub Remove(ByVal vntIndexKey)
On Error Resume Next
    If VarType(vntIndexKey) = vbString Then
        vntIndexKey = LCase(Trim(CStr(vntIndexKey)))
    ElseIf VarType(vntIndexKey) = vbObject Then
        vntIndexKey = Find(vntIndexKey)
    Else
        vntIndexKey = CLng(vntIndexKey)
    End If
    mCol(vntIndexKey).Parent.ChildRemoved mCol(vntIndexKey)
    Set mCol(vntIndexKey).Parent = Nothing
    mCol.Remove vntIndexKey
End Sub

Public Sub Move(ByVal FromIndex As Long, ByVal ToIndex As Long)
On Error Resume Next
Dim l_objObject As Object
    If FromIndex < 1 Or FromIndex > mCol.Count Then Exit Sub
    If ToIndex < 1 Or ToIndex > (mCol.Count + 1) Then Exit Sub
    If FromIndex = ToIndex Then Exit Sub
    Set l_objObject = mCol.Item(FromIndex)
    Remove FromIndex
    If (FromIndex < ToIndex) Then
'        Trust me, this works - even though it doesn't make sense unless you think about it
'        ToIndex = ToIndex - 1
    End If
    Add l_objObject, , ToIndex
End Sub

Public Sub Clear()
On Error Resume Next
    Set mCol = New Collection
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
On Error Resume Next
    Set NewEnum = mCol.[_NewEnum]
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set mCol = Nothing
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
        .WriteSegment_End
    End With
End Sub



