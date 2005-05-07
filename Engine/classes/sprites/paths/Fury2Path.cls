VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Path"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Path/Waypoint Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Fury2Waypoint"
Attribute VB_Ext_KEY = "Member0" ,"Fury2Waypoint"
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
Implements IInspectable
Implements IInspectableCollection
Implements Fury2Object
Implements IVirtualFileCollection

Private mCol As Collection
Private mvarLooping As Boolean
Private mvarParent As Fury2Sprite

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

Friend Sub SetParent(ByRef Parent As Fury2Sprite)
On Error Resume Next
    Set Parent = mvarParent
End Sub

Public Sub Completed()
On Error Resume Next
Dim l_objWaypoint As Fury2Waypoint
    Set l_objWaypoint = mCol.Item(1)
    mCol.Remove 1
    If mvarLooping Then
        mCol.Add l_objWaypoint
        l_objWaypoint.WaitRemainder = l_objWaypoint.Wait
    End If
    If l_objWaypoint.ReachedEvent <> "" Then
        mvarParent.Map.Engine.ScriptEngine.AddCode l_objWaypoint.ReachedEvent
    End If
End Sub

Public Function Find(ByVal Obj) As Long
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
    Set mCol = New Collection
End Sub

Sub Reset()
Attribute Reset.VB_Description = "Deprecated. Use Clear."
Attribute Reset.VB_MemberFlags = "40"
On Error Resume Next
    Set mCol = New Collection
End Sub

Public Property Get Looping() As Boolean
Attribute Looping.VB_Description = "If True, the path will continue forever."
On Error Resume Next
    Looping = mvarLooping
End Property

Public Property Let Looping(NewLooping As Boolean)
On Error Resume Next
    mvarLooping = NewLooping
End Property

Public Function Add(ByVal X As Single, ByVal Y As Single, Optional ReachedEvent As String = "", Optional FailEvent As String = "", Optional Wait As Long = 0, Optional ByVal AtIndex) As Fury2Waypoint
On Error Resume Next
    If mCol.Count >= c_lngMaxWaypoints Then Exit Function
    Dim objNewMember As Fury2Waypoint
    Set objNewMember = New Fury2Waypoint
    
    objNewMember.X = X
    objNewMember.Y = Y
    objNewMember.ReachedEvent = ReachedEvent
    objNewMember.FailEvent = FailEvent
    If IsMissing(AtIndex) Then
        mCol.Add objNewMember
    Else
        If mCol.Count = 0 Then
            mCol.Add objNewMember
        ElseIf AtIndex <= 1 Then
            mCol.Add objNewMember, , 1
        ElseIf mCol.Count > AtIndex Then
            mCol.Add objNewMember, , AtIndex
        Else
            mCol.Add objNewMember, , , AtIndex - 1
        End If
    End If
    
    Set Add = objNewMember
    Set objNewMember = Nothing
End Function

Public Sub AddObject(ByVal Obj As Fury2Waypoint, Optional ByVal AtIndex)
On Error Resume Next
    If mCol.Count >= c_lngMaxWaypoints Then Exit Sub
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
End Sub

Public Property Get Item(ByVal vntIndexKey As Variant) As Fury2Waypoint
Attribute Item.VB_Description = "*"
Attribute Item.VB_UserMemId = 0
On Error Resume Next
  Set Item = mCol(vntIndexKey)
End Property

Public Property Get Count() As Long
Attribute Count.VB_Description = "The number of nodes in the path."
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
End Sub

Private Property Get IVirtualFileCollection_Count() As Long
On Error Resume Next
    IVirtualFileCollection_Count = mCol.Count
End Property

Private Function IVirtualFileCollection_CreateNew() As Object
On Error Resume Next
    Set IVirtualFileCollection_CreateNew = New Fury2Waypoint
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
    File.Save mvarLooping
End Sub

Private Sub IVirtualFileCollection_LoadHeader(File As libGraphics.VirtualFile)
On Error Resume Next
    File.Load mvarLooping
End Sub

