VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2JoystickBindings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"ControlBinding"
Attribute VB_Ext_KEY = "Member0" ,"ControlBinding"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
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
Implements IVirtualFileCollection

Public Parent As Fury2Joystick

Private mCol As Collection

Sub Rebind()
On Error Resume Next
Dim m_bndBind As Fury2JoystickBinding
    For Each m_bndBind In mCol
        m_bndBind.Rebind
    Next m_bndBind
End Sub

Function Update() As Long
On Error Resume Next
Dim m_lngIndex As Long
Dim m_booFound As Boolean
Dim m_bndBind As Fury2JoystickBinding
    If mCol.Count <= 0 Then Exit Function
    m_lngIndex = 1
    For Each m_bndBind In mCol
        With m_bndBind
            If m_booFound Then
                .Update
            Else
                If .Update Then
                    Update = m_lngIndex
                    m_booFound = True
                End If
            End If
        End With
        m_lngIndex = m_lngIndex + 1
    Next m_bndBind
    Err.Clear
End Function

Sub Clear()
On Error Resume Next
    Set mCol = New Collection
End Sub

Public Sub Add(Obj As Fury2JoystickBinding, Optional sKey As String)
On Error Resume Next
    If Len(sKey) = 0 Then
        mCol.Add Obj
    Else
        mCol.Add Obj, sKey
    End If
    Set Obj.Parent = Parent
    If Obj.BindControl Is Nothing Then Obj.Rebind
End Sub

Public Property Get Item(vntIndexKey As Variant) As Fury2JoystickBinding
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
    Set IVirtualFileCollection_CreateNew = New Fury2JoystickBinding
End Function

Private Function IVirtualFileCollection_GetItem(ByVal Index As Long) As Object
On Error Resume Next
    Set IVirtualFileCollection_GetItem = mCol.Item(Index)
End Function

Private Sub IVirtualFileCollection_LoadHeader(File As libGraphics.VirtualFile)
    With File
    End With
End Sub

Private Sub IVirtualFileCollection_Reset()
On Error Resume Next
    Set mCol = New Collection
End Sub

Private Sub IVirtualFileCollection_SaveHeader(File As libGraphics.VirtualFile)
    With File
    End With
End Sub
