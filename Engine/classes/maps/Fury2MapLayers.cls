VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2MapLayers"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Map Layer Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Fury2MapLayer"
Attribute VB_Ext_KEY = "Member0" ,"Fury2MapLayer"
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

Implements Fury2Object
Implements IVirtualFileCollection
Implements IInspectableCollection
Implements IVirtualFileSaveable
Private mCol As Collection
Public Parent As Fury2Map

Private Property Get IInspectableCollection_ItemCount() As Long
On Error Resume Next
    IInspectableCollection_ItemCount = mCol.Count
End Property

Private Property Set IInspectableCollection_ItemValue(ByVal Index As Long, ByVal RHS As Variant)
On Error Resume Next
End Property

Private Property Let IInspectableCollection_ItemValue(ByVal Index As Long, ByVal RHS As Variant)
On Error Resume Next
End Property

Private Property Get IInspectableCollection_ItemValue(ByVal Index As Long) As Variant
On Error Resume Next
    Err.Clear
    Set IInspectableCollection_ItemValue = mCol(Index)
    If Err <> 0 Then
        IInspectableCollection_ItemValue = mCol(Index)
    End If
End Property

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
    Add l_objObject, ToIndex
End Sub

Public Function Find(Obj) As Long
On Error Resume Next
Dim l_objItem As Object, l_lngItems As Long
    If mCol.Count <= 0 Then Exit Function
    l_lngItems = 1
    For Each l_objItem In mCol
        If l_objItem Is Obj Then
            Find = l_lngItems
            Exit Function
        End If
        l_lngItems = l_lngItems + 1
    Next l_objItem
End Function

Sub Clear()
On Error Resume Next
Dim m_lyrLayer As Fury2MapLayer
    For Each m_lyrLayer In mCol
        m_lyrLayer.Clear m_lyrLayer.Tileset.TransparentTile
    Next m_lyrLayer
    Err.Clear
End Sub

Public Sub Add(ByVal Obj As Fury2MapLayer, Optional ByVal AtIndex As Long = -1)
On Error Resume Next
    Obj.SetParent Parent
    If AtIndex = -1 Then
        mCol.Add Obj
    Else
        If (mCol.Count = 0) Or (AtIndex > mCol.Count) Then
            mCol.Add Obj
        ElseIf AtIndex <= 1 Then
            mCol.Add Obj, , 1
        ElseIf mCol.Count > AtIndex Then
            mCol.Add Obj, , AtIndex
        Else
            mCol.Add Obj, , , AtIndex - 1
        End If
    End If
    Err.Clear
End Sub

Public Property Get Item(ByVal vntIndexKey As Long) As Fury2MapLayer
Attribute Item.VB_UserMemId = 0
On Error Resume Next
  Set Item = mCol(vntIndexKey)
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = mCol.Count
End Property

Public Sub Remove(vntIndexKey As Variant)
On Error Resume Next
    mCol.Remove vntIndexKey
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    Set NewEnum = mCol.[_NewEnum]
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
End Sub

Private Sub IVirtualFileCollection_AddItem(Item As Object)
On Error Resume Next
    Add Item
End Sub

Private Property Get IVirtualFileCollection_Count() As Long
On Error Resume Next
    IVirtualFileCollection_Count = mCol.Count
End Property

Private Function IVirtualFileCollection_CreateNew() As Object
On Error Resume Next
Dim l_lyrNew As Fury2MapLayer
    Set l_lyrNew = New Fury2MapLayer
    l_lyrNew.SetParent Parent
    l_lyrNew.Sprites.SetParent Parent
    Set IVirtualFileCollection_CreateNew = l_lyrNew
End Function

Private Function IVirtualFileCollection_GetItem(ByVal Index As Long) As Object
On Error Resume Next
    Set IVirtualFileCollection_GetItem = mCol.Item(Index)
End Function

Private Sub IVirtualFileCollection_Reset()
On Error Resume Next
    Set mCol = New Collection
End Sub

Private Sub IVirtualFileCollection_SaveHeader(File As libGraphics.VirtualFile)
On Error Resume Next
End Sub

Private Sub IVirtualFileCollection_LoadHeader(File As libGraphics.VirtualFile)
On Error Resume Next
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
End Sub

