VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2MapObjects"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
'    MERCHANTABILITY or FITNESS FOR A PARTI4CULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Implements Fury2Object
Implements IVirtualFileSaveable

Private mCol As Collection
Private mvarParent As Fury2Map

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

Friend Sub SetParent(Parent As Fury2Map)
    Set mvarParent = Parent
End Sub

Public Sub Add(ByVal Obj As Fury2MapObject, Optional ByVal sKey, Optional ByVal AtIndex)
On Error Resume Next
    Obj.Load m_Engine, mvarParent
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
        sKey = LCase(Trim(CStr(sKey)))
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

Public Property Get GenericItem(vntIndexKey As Variant) As Fury2MapObject
Attribute GenericItem.VB_UserMemId = 0
On Error Resume Next
    Set GenericItem = mCol(vntIndexKey)
End Property

Public Property Get Item(vntIndexKey As Variant)
On Error Resume Next
Dim l_mobItem As Fury2MapObject
Dim l_sndItem As Fury2SoundObject
    Set l_mobItem = mCol(vntIndexKey)
    If TypeOf l_mobItem Is Fury2SoundObject Then
        Set l_sndItem = l_mobItem
        Set Item = l_sndItem
    End If
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
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngItem As Long
Dim l_objItem As Fury2MapObject
Dim l_lngType As Long
    Set mCol = New Collection
    With File
        .ReadSegment_Begin
            .Load l_lngCount
            If l_lngCount > 0 Then
                .ReadSegment_Begin
                    For l_lngItem = 1 To l_lngCount
                        Set l_objItem = Nothing
                        .ReadSegment_Begin
                            .Load l_lngType
                            Select Case l_lngType
                            Case 1
                                Set l_objItem = New Fury2SoundObject
                            Case Else
                                Set l_objItem = New Fury2MapObject
                            End Select
                            .Load l_objItem
                            Add l_objItem
                        .ReadSegment_End
                    Next l_lngItem
                .ReadSegment_End
            End If
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngItem As Long
Dim l_objItem As Fury2MapObject
    With File
        .WriteSegment_Begin
            .Save mCol.Count
            If mCol.Count > 0 Then
                .WriteSegment_Begin
                    For l_lngItem = 1 To mCol.Count
                        .WriteSegment_Begin
                            Set l_objItem = mCol(l_lngItem)
                            If TypeOf l_objItem Is Fury2SoundObject Then
                                .Save CLng(1)
                            Else
                                .Save CLng(0)
                            End If
                            .Save l_objItem
                        .WriteSegment_End
                    Next l_lngItem
                .WriteSegment_End
            End If
        .WriteSegment_End
    End With
End Sub
