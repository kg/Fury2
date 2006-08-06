VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2LightSources"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Area Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Fury2Area"
Attribute VB_Ext_KEY = "Member0" ,"Fury2Area"
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
Implements IVirtualFileSaveable

Private mHook As Object
Private mCol As Collection
Private mvarParent As Fury2LayerLighting

Public Sub Refresh()
On Error Resume Next
Dim l_litLight As Fury2LightSource
    For Each l_litLight In mCol
        l_litLight.UpdateCache
        l_litLight.UpdateImage
        l_litLight.UpdateAttachment
    Next l_litLight
End Sub

Public Sub Free()
On Error Resume Next
    Set mvarParent = Nothing
    Set mCol = Nothing
End Sub

Public Sub Hook(Obj As Object)
On Error Resume Next
    Set mHook = Obj
End Sub

Public Function FindByNames(ByVal Match As String) As Fury2Collection
On Error Resume Next
Dim l_colItems As Fury2Collection
Dim l_litLight As Fury2LightSource
    If mvarParent Is Nothing Then Stop
    If mvarParent.Layer.Map.Engine Is Nothing Then Stop
    Set l_colItems = mvarParent.Layer.Map.Engine.F2Collection()
    For Each l_litLight In mCol
        If l_litLight.Name Like Match Then
            l_colItems.Add l_litLight
        End If
    Next l_litLight
    Set FindByNames = l_colItems
End Function

Public Function Find(ByVal vntIndexKey As Variant) As Long
On Error Resume Next
Dim l_lngLightSources As Long
Dim l_objObject As Object, l_strKey As String
Dim l_litLightSource As Fury2LightSource
    If mCol.Count < 1 Then Exit Function
    If VarType(vntIndexKey) = vbObject Then
        Set l_objObject = vntIndexKey
        l_lngLightSources = 1
        For Each l_litLightSource In mCol
            If l_litLightSource Is l_objObject Then
                Find = l_lngLightSources
                Exit For
            End If
            l_lngLightSources = l_lngLightSources + 1
        Next l_litLightSource
    ElseIf VarType(vntIndexKey) = vbString Then
        l_strKey = CStr(vntIndexKey)
        l_lngLightSources = 1
        For Each l_litLightSource In mCol
            If l_litLightSource.Name Like l_strKey Then
                Find = l_lngLightSources
                Exit For
            End If
            l_lngLightSources = l_lngLightSources + 1
        Next l_litLightSource
    Else
        Find = CLng(vntIndexKey)
    End If
End Function

Friend Sub SetParent(Parent As Fury2LayerLighting)
    Set mvarParent = Parent
End Sub

Public Function AddNew(X As Long, Y As Long) As Fury2LightSource
On Error Resume Next
    If mCol.Count >= c_lngMaxLightSources Then Exit Function
    mCol.Add New Fury2LightSource
    With Item(mCol.Count)
        .X = X - 32
        .Y = Y - 32
        .FalloffDistance = 50
        .Color = F2White
        .SetEngine mvarParent.Layer.Map.Engine
        .SetParent mvarParent
    End With
End Function

Public Sub Add(Obj As Fury2LightSource, Optional ByVal sKey, Optional ByVal AtIndex)
On Error Resume Next
    If IsMissing(sKey) Then
        If IsMissing(AtIndex) Then
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
    Else
        sKey = LCase(Trim(CStr(sKey)))
        If IsMissing(AtIndex) Then
            mCol.Add Obj, sKey
        Else
            If (mCol.Count = 0) Or (AtIndex > mCol.Count) Then
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
    Obj.SetEngine mvarParent.Layer.Map.Engine
    Obj.SetParent mvarParent
    If mHook Is Nothing Then
    Else
        mHook.Changed
    End If
End Sub

Public Property Get Item(ByVal vntIndexKey As Variant) As Fury2LightSource
Attribute Item.VB_UserMemId = 0
Attribute Item.VB_MemberFlags = "200"
On Error Resume Next
  Set Item = mCol(vntIndexKey)
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = mCol.Count
End Property

Public Sub Remove(vntIndexKey As Variant)
On Error Resume Next
    Item(vntIndexKey).Free
    mCol.Remove vntIndexKey
    If mHook Is Nothing Then
    Else
        mHook.Changed
    End If
End Sub

Public Sub Clear()
On Error Resume Next
    Do While mCol.Count > 0
        Item(1).Free
        mCol.Remove 1
    Loop
    Set mCol = New Collection
    If mHook Is Nothing Then
    Else
        mHook.Changed
    End If
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
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
    Add Item
End Sub

Private Property Get IVirtualFileCollection_Count() As Long
On Error Resume Next
    IVirtualFileCollection_Count = mCol.Count
End Property

Private Function IVirtualFileCollection_CreateNew() As Object
On Error Resume Next
Dim l_lsLight As Fury2LightSource
    Set l_lsLight = New Fury2LightSource
    l_lsLight.SetParent mvarParent
    Set IVirtualFileCollection_CreateNew = l_lsLight
End Function

Private Function IVirtualFileCollection_GetItem(ByVal Index As Long) As Object
On Error Resume Next
    Set IVirtualFileCollection_GetItem = mCol.Item(Index)
End Function

Private Sub IVirtualFileCollection_Reset()
On Error Resume Next
    Clear
End Sub

Private Sub IVirtualFileCollection_SaveHeader(File As libGraphics.VirtualFile)
On Error Resume Next
End Sub

Private Sub IVirtualFileCollection_LoadHeader(File As libGraphics.VirtualFile)
On Error Resume Next
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

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
End Sub

