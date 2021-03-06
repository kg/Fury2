VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2PoseFrames"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury� Pose::Frame Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    Engine (Fury� Game Creation System Runtime Engine)
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
Implements IVirtualFileSaveable
Implements IInspectableCollection
Private m_posParent As WeakReference
Private mCol As Collection

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

Public Property Get Parent() As Fury2Pose
    Set Parent = WRGetPose(m_posParent)
End Property

Friend Sub SetParent(Pose As Fury2Pose)
On Error Resume Next
    WRSet m_posParent, Pose
End Sub

Friend Sub Reindex()
On Error Resume Next
Dim l_lngIndex As Long
Dim l_fraFrame As Fury2PoseFrame
    For Each l_fraFrame In mCol
        l_lngIndex = l_lngIndex + 1
        l_fraFrame.SetIndex l_lngIndex
    Next l_fraFrame
End Sub

Public Sub ModifyGraphics(ByVal FunctionName As String, Optional ByRef Parameters As Variant)
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame, l_lngParameterCount As Long
Dim l_rctOld As Fury2Rect
    l_lngParameterCount = 0
    l_lngParameterCount = UBound(Parameters) + 1
    Err.Clear
    For Each l_fraFrame In mCol
        With l_fraFrame
            .Image.PushClipRectangle .Rectangle
            Select Case l_lngParameterCount
            Case 1
                CallByName .Image, FunctionName, VbMethod, Parameters(0)
            Case 2
                CallByName .Image, FunctionName, VbMethod, Parameters(0), Parameters(1)
            Case 3
                CallByName .Image, FunctionName, VbMethod, Parameters(0), Parameters(1), Parameters(2)
            Case 4
                CallByName .Image, FunctionName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3)
            Case 5
                CallByName .Image, FunctionName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4)
            Case 6
                CallByName .Image, FunctionName, VbMethod, Parameters(0), Parameters(1), Parameters(2), Parameters(3), Parameters(4), Parameters(5)
            Case Else
                CallByName .Image, FunctionName, VbMethod
            End Select
            .Image.PopClipRectangle
        End With
    Next l_fraFrame
    Err.Clear
End Sub

Sub TranslateAlignment(ByVal X, ByVal Y)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    X = CSng(X)
    Y = CSng(Y)
    For m_lngFrames = 1 To mCol.Count
        With mCol.Item(m_lngFrames)
            .XCenter = .XCenter + X
            .YCenter = .YCenter + Y
        End With
    Next m_lngFrames
End Sub

Sub TranslateRectangles(ByVal X, ByVal Y)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    X = CLng(X)
    Y = CLng(Y)
    For m_lngFrames = 1 To mCol.Count
        With mCol.Item(m_lngFrames)
            .Rectangle.Translate X, Y
        End With
    Next m_lngFrames
End Sub

Sub SetFrameWidth(Width)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngFrames = 1 To mCol.Count
        mCol.Item(m_lngFrames).Rectangle.Width = CLng(Width)
    Next m_lngFrames
End Sub

Sub SetFrameHeight(Height)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngFrames = 1 To mCol.Count
        mCol.Item(m_lngFrames).Rectangle.Height = CLng(Height)
    Next m_lngFrames
End Sub

Sub TranslateRectangle(X, Y)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngFrames = 1 To mCol.Count
        mCol.Item(m_lngFrames).Rectangle.Translate CLng(X), CLng(Y)
    Next m_lngFrames
End Sub

Sub SetMaskColor(Color)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngFrames = 1 To mCol.Count
        mCol.Item(m_lngFrames).MaskColor = CLng(Color)
    Next m_lngFrames
End Sub

Sub SetAnimationDelay(Delay)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngFrames = 1 To mCol.Count
        mCol.Item(m_lngFrames).Delay = CLng(Delay)
    Next m_lngFrames
End Sub

Sub SetFilename(Filename)
On Error Resume Next
Dim m_lngFrames As Long
    If mCol.Count = 0 Then Exit Sub
    For m_lngFrames = 1 To mCol.Count
        mCol.Item(m_lngFrames).Filename = CStr(Filename)
    Next m_lngFrames
End Sub

Public Sub LoadGraphics()
On Error Resume Next
Dim l_fraFrame As Fury2PoseFrame
    For Each l_fraFrame In mCol
        l_fraFrame.LoadGraphics
    Next l_fraFrame
End Sub

Public Function Duplicate() As Fury2PoseFrames
On Error Resume Next
Dim m_fraFrame As Fury2PoseFrame
    Set Duplicate = New Fury2PoseFrames
    With Duplicate
        For Each m_fraFrame In mCol
            .AddFast m_fraFrame
        Next m_fraFrame
        .Reindex
    End With
End Function

Public Sub AddFrame(Filename, Rectangle, Optional XCenter = 0, Optional YCenter = 0, Optional Delay = 1, Optional MaskColor = -32767)
On Error Resume Next
Dim m_fraFrame As Fury2PoseFrame
    If mCol.Count >= c_lngMaxPoseFrames Then Exit Sub
    Set m_fraFrame = New Fury2PoseFrame
    With m_fraFrame
        .Filename = CStr(Filename)
        Set .Rectangle = Rectangle
        .XCenter = CSng(XCenter)
        .YCenter = CSng(YCenter)
        .Delay = CLng(Delay)
        .MaskColor = CLng(MaskColor)
        Set .Parent = Parent
    End With
    Add m_fraFrame
End Sub

Public Sub AddFast(ByVal Obj As Fury2PoseFrame)
On Error Resume Next
    Obj.SetParent Parent
    mCol.Add Obj
End Sub

Public Sub Add(Obj As Fury2PoseFrame, Optional ByVal sKey, Optional ByVal AtIndex)
On Error Resume Next
    Obj.SetParent Parent
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
    Reindex
End Sub

Public Property Get Item(ByVal vntIndexKey As Variant) As Fury2PoseFrame
Attribute Item.VB_UserMemId = 0
Attribute Item.VB_MemberFlags = "200"
On Error Resume Next
  Set Item = mCol(ClipValue(CLng(vntIndexKey), 1, mCol.Count))
End Property

Public Property Get Count() As Long
On Error Resume Next
    Count = mCol.Count
End Property

Public Sub Remove(vntIndexKey As Variant)
On Error Resume Next
    mCol.Remove vntIndexKey
    Reindex
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
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
    Reindex
End Sub

Private Property Get IVirtualFileCollection_Count() As Long
On Error Resume Next
    IVirtualFileCollection_Count = mCol.Count
End Property

Private Function IVirtualFileCollection_CreateNew() As Object
On Error Resume Next
Dim l_fraNew As Fury2PoseFrame
    Set l_fraNew = New Fury2PoseFrame
    l_fraNew.SetParent Parent
    Set IVirtualFileCollection_CreateNew = l_fraNew
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

Public Function Find(ByVal Var As Fury2PoseFrame) As Long
On Error Resume Next
Dim m_lngItems As Long, m_objItem As Fury2PoseFrame
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


Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
End Sub

