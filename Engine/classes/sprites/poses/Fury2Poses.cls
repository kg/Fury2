VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Poses"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Pose Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Fury2Pose"
Attribute VB_Ext_KEY = "Member0" ,"Fury2Pose"
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

Private mCol As Collection
Private m_sprParent As Fury2Sprite

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

Public Property Get Parent() As Fury2Sprite
    Set Parent = m_sprParent
End Property

Friend Sub SetParent(Parent As Fury2Sprite)
On Error Resume Next
    Set m_sprParent = Parent
End Sub

Public Sub ModifyGraphics(ByVal FunctionName As String, Optional ByRef Parameters As Variant)
On Error Resume Next
Dim l_posPose As Fury2Pose
    For Each l_posPose In mCol
        l_posPose.ModifyGraphics FunctionName, Parameters
    Next l_posPose
End Sub

Public Sub LoadGraphics()
On Error Resume Next
Dim l_posPose As Fury2Pose
    For Each l_posPose In mCol
        l_posPose.LoadGraphics
    Next l_posPose
End Sub

Sub Free()
On Error Resume Next
Dim l_posPose As Fury2Pose
    For Each l_posPose In mCol
        l_posPose.Free
    Next l_posPose
End Sub

Sub TranslateAlignment(X, Y)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Frames.TranslateAlignment X, Y
    Next m_lngPoses
End Sub

Sub TranslateRectangles(X, Y)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Frames.TranslateRectangles X, Y
    Next m_lngPoses
End Sub

Sub SetFrameWidth(Width)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Frames.SetFrameWidth Width
    Next m_lngPoses
End Sub

Sub SetFrameHeight(Height)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Frames.SetFrameHeight Height
    Next m_lngPoses
End Sub

Sub SetMaskColor(Color)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Frames.SetMaskColor Color
    Next m_lngPoses
End Sub

Sub SetAnimationDelay(Delay)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Frames.SetAnimationDelay Delay
    Next m_lngPoses
End Sub

Sub SetFilename(Filename)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Frames.SetFilename Filename
    Next m_lngPoses
End Sub

Sub SetBlocking(Width, Height)
On Error Resume Next
Dim m_lngPoses As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngPoses = 1 To mCol.Count
        mCol.Item(m_lngPoses).Blocking.Width = CLng(Width)
        mCol.Item(m_lngPoses).Blocking.Height = CLng(Height)
    Next m_lngPoses
End Sub

Public Function Duplicate() As Fury2Poses
On Error Resume Next
Dim m_lngPoses As Long
    Set Duplicate = New Fury2Poses
    With Duplicate
        If mCol.Count > 0 Then
            For m_lngPoses = 1 To mCol.Count
                Duplicate.Add mCol.Item(m_lngPoses).Duplicate
            Next m_lngPoses
        End If
    End With
End Function

Public Sub AddPose(XVelocity, YVelocity, Optional AnimType = 1, Optional Name As String = "")
On Error Resume Next
Dim m_posPose As Fury2Pose
    If mCol.Count >= c_lngMaxPoses Then Exit Sub
    Set m_posPose = New Fury2Pose
    With m_posPose
        .XVelocity = XVelocity
        .YVelocity = YVelocity
        .LoopMode = CLng(AnimType)
        .Name = Name
    End With
    Add m_posPose
    Set m_posPose = Nothing
End Sub

Public Sub Add(Obj As Fury2Pose, Optional ByVal sKey, Optional ByVal AtIndex)
On Error Resume Next
    Obj.SetParent m_sprParent
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
End Sub

Public Property Get Item(vntIndexKey As Variant) As Fury2Pose
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

Public Sub Clear()
    Set mCol = New Collection
End Sub

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
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
Dim l_posNew As Fury2Pose
    Set l_posNew = New Fury2Pose
    l_posNew.SetParent m_sprParent
    Set IVirtualFileCollection_CreateNew = l_posNew
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

Public Function Find(ByVal Var As Fury2Pose) As Long
On Error Resume Next
Dim m_lngItems As Long, m_objItem As Fury2Pose
    If mCol.Count <= 0 Then Exit Function
    m_lngItems = 1
    For Each m_objItem In mCol
        If m_objItem Is Var Then
            Find = m_lngItems
            Exit Function
        End If
        m_lngItems = m_lngItems + 1
    Next m_objItem
End Function


