VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Collection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Generic Collection Object"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Obj"
Attribute VB_Ext_KEY = "Member0" ,"Obj"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    libFilesystem (Fury² Virtual Filesystem Library)
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
Implements IVirtualFileSaveable
Private mCol As Collection

Public Sub Add(Obj, Optional sKey, Optional AtIndex)
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
    Err.Clear
End Sub

Public Property Get Item(vntIndexKey)
Attribute Item.VB_UserMemId = 0
On Error Resume Next
    'used when referencing an element in the collection
    'vntIndexKey contains either the Index or Key to the collection,
    'this is why it is declared as a Variant
    'Syntax: Set foo = X.Item(xyz) or Set foo = X.Item(5)
    If VarType(mCol(vntIndexKey)) = vbObject Then
        Set Item = mCol(vntIndexKey)
    Else
        Item = mCol(vntIndexKey)
    End If
End Property

Public Function Find(Var) As Long
On Error Resume Next
Dim m_lngItems As Long
    If mCol.Count <= 0 Then Exit Function
    If IsObject(Var) Then
        For m_lngItems = 1 To mCol.Count
            If mCol(m_lngItems) Is Var Then
                Find = m_lngItems
                Exit Function
            End If
        Next m_lngItems
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
    'used when retrieving the number of elements in the
    'collection. Syntax: Debug.Print X.Count
    Count = mCol.Count
End Property

Public Sub Remove(vntIndexKey)
On Error Resume Next
    'used when removing an element from the collection
    'vntIndexKey contains either the Index or Key, which is why
    'it is declared as a Variant
    'Syntax: X.Remove(xyz)
    mCol.Remove vntIndexKey
End Sub

Public Sub RemoveObject(Obj)
On Error Resume Next
Dim m_lngItems As Long
    If mCol.Count <= 0 Then Exit Sub
    For m_lngItems = 1 To mCol.Count
        If mCol(m_lngItems) Is Obj Then mCol.Remove m_lngItems
    Next m_lngItems
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
On Error Resume Next
    'this property allows you to enumerate
    'this collection with the For...Each syntax
    Set NewEnum = mCol.[_NewEnum]
End Property

Private Sub Class_Initialize()
On Error Resume Next
    'creates the collection when this class is created
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    'destroys collection when this class is terminated
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

