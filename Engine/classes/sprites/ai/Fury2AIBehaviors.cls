VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2AIBehaviors"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² AI Behavior Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    Engine (Fury² Game Creation System Runtime Engine)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
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

Option Explicit
Implements Fury2Object
Implements IVirtualFileCollection
Private mCol As Collection
Private m_objParent As Object

Public Function Find(Obj) As Long
On Error Resume Next
Dim m_lngItems As Long
    If mCol.Count <= 0 Then Exit Function
    For m_lngItems = 1 To mCol.Count
        If mCol(m_lngItems) Is Obj Then
            Find = m_lngItems
            Exit Function
        End If
    Next m_lngItems
End Function

Public Function Duplicate() As Fury2AIBehaviors
On Error Resume Next
Dim m_lngAI As Long
    Set Duplicate = New Fury2AIBehaviors
    With Duplicate
        If mCol.Count > 0 Then
            For m_lngAI = 1 To mCol.Count
                Duplicate.Add mCol.Item(m_lngAI).Duplicate
            Next m_lngAI
        End If
    End With
End Function

Sub Load()
Dim m_lngBehaviors As Long
    If mCol.Count <= 0 Then Exit Sub
    For m_lngBehaviors = mCol.Count To 1 Step -1
        Set mCol(m_lngBehaviors).Target = m_objParent
        mCol(m_lngBehaviors).RecreateScriptObject
    Next m_lngBehaviors
End Sub

Sub SetParent(Obj As Object)
    Set m_objParent = Obj
End Sub

Sub Update(Obj As Fury2Sprite)
On Error Resume Next
Dim m_lngBehaviors As Long
    If mCol.Count <= 0 Then Exit Sub
    For m_lngBehaviors = mCol.Count To 1 Step -1
        mCol(m_lngBehaviors).Update Obj
    Next m_lngBehaviors
End Sub

Public Function AddNew() As Fury2AIBehavior
On Error Resume Next
    Set AddNew = New Fury2AIBehavior
    Set AddNew.Chain = Me
    Set AddNew.Target = m_objParent
    mCol.Add AddNew
End Function

Public Sub Add(Obj As Object, Optional sKey, Optional AtIndex)
On Error Resume Next
    If IsMissing(sKey) Then
        If IsMissing(AtIndex) Then
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
        If IsMissing(AtIndex) Then
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
    Set Obj.Chain = Me
    Set Obj.Target = m_objParent
    Err.Clear
End Sub

Public Property Get Item(vntIndexKey) As Fury2AIBehavior
Attribute Item.VB_UserMemId = 0
Attribute Item.VB_MemberFlags = "200"
On Error Resume Next
  Set Item = mCol(vntIndexKey)
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = mCol.Count
End Property

Sub Reset()
On Error Resume Next
    Set mCol = New Collection
End Sub

Public Sub Remove(vntIndexKey)
On Error Resume Next
    mCol.Remove vntIndexKey
End Sub

Public Sub RemoveObject(Obj As Object)
On Error Resume Next
Dim m_lngItems As Long
    If mCol.Count <= 0 Then Exit Sub
    For m_lngItems = 1 To mCol.Count
        If mCol(m_lngItems) Is Obj Then mCol.Remove m_lngItems
    Next m_lngItems
End Sub

Public Property Get NewEnum() As IUnknown
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

Private Sub IVirtualFileCollection_AddItem(Item As Object)
On Error Resume Next
    mCol.Add Item
    Set Item.Chain = Me
    Err.Clear
End Sub

Private Property Get IVirtualFileCollection_Count() As Long
On Error Resume Next
    IVirtualFileCollection_Count = mCol.Count
End Property

Private Function IVirtualFileCollection_CreateNew() As Object
On Error Resume Next
    Set IVirtualFileCollection_CreateNew = New Fury2AIBehavior
End Function

Private Function IVirtualFileCollection_GetItem(ByVal Index As Long) As Object
On Error Resume Next
    Set IVirtualFileCollection_GetItem = mCol.Item(Index)
End Function

Private Sub IVirtualFileCollection_LoadHeader(File As libGraphics.VirtualFile)
End Sub

Private Sub IVirtualFileCollection_Reset()
On Error Resume Next
    Set mCol = New Collection
End Sub

Private Sub IVirtualFileCollection_SaveHeader(File As libGraphics.VirtualFile)
End Sub
