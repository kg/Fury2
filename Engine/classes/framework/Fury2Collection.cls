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
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Generic Collection Object"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Obj"
Attribute VB_Ext_KEY = "Member0" ,"Obj"
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
#If ExternalModule = 0 Then
Implements Fury2Object
#End If
Implements IVirtualFileSaveable

Private mCol As Collection
Private mHook As Object
#If ExternalModule = 0 Then
Private m_engEngine As Fury2Engine

Public Function Duplicate() As Fury2Collection
On Error Resume Next
Dim l_colNew As Fury2Collection
Dim l_lngIndex As Long
    Set l_colNew = m_engEngine.F2Collection()
    If mCol.Count > 0 Then
        For l_lngIndex = 1 To mCol.Count
            l_colNew.Add mCol(l_lngIndex)
        Next l_lngIndex
    End If
    Set Duplicate = l_colNew
End Function

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property
#End If

Public Sub Hook(Obj As Object)
On Error Resume Next
    Set mHook = Obj
End Sub

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
            l_booResult = VBA.CallByName(l_objObject, MethodName, VbMethod, Parameters(0))
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

Public Sub Add(Obj, Optional ByVal sKey, Optional ByVal AtIndex)
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
    If mHook Is Nothing Then
    Else
        mHook.Changed
    End If
End Sub

Public Property Get Item(ByVal vntIndexKey)
Attribute Item.VB_UserMemId = 0
On Error Resume Next
    If VarType(vntIndexKey) = vbString Then
        vntIndexKey = LCase(Trim(CStr(vntIndexKey)))
    End If
    Err.Clear
    Set Item = mCol(vntIndexKey)
    If Err <> 0 Then
        Item = mCol(vntIndexKey)
    End If
    Err.Clear
End Property

Public Function Find(Var) As Long
On Error Resume Next
Dim m_lngItems As Long, m_objItem As Object
    If mCol.Count <= 0 Then Exit Function
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
    mCol.Remove vntIndexKey
    If mHook Is Nothing Then
    Else
        mHook.Changed
    End If
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
    Set mHook = Nothing
    Set mCol = Nothing
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngIndex As Long
Dim l_strType As String
Dim l_objItem As Object, l_strItem As String
    Set mCol = New Collection
    With File
        .ReadSegment_Begin
            .Load l_lngCount
            If l_lngCount > 0 Then
                For l_lngIndex = 1 To l_lngCount
                    .ReadSegment_Begin
                    Set l_objItem = Nothing
                    l_strItem = vbNullString
                    .Load l_strType
                    If l_strType = "String" Then
                        .Load l_strItem
                        mCol.Add l_strItem
                    ElseIf l_strType = "Nothing" Then
                        mCol.Add Nothing
                    Else
                        #If ExternalModule = 0 Then
                        Err.Clear
                        Set l_objItem = Engine.ScriptEngine.Eval(Engine.ScriptEngine.Language.GenerateInstantiation(l_strType))
                        If l_objItem Is Nothing Then
                            Err.Clear
                            Set l_objItem = CreateObject("Engine." & l_strType)
                            If l_objItem Is Nothing Then
                                Err.Clear
                                Set l_objItem = CreateObject("libGraphics." & l_strType)
                            End If
                        End If
                        .Load l_objItem
                        mCol.Add l_objItem
                        #End If
                    End If
                    .ReadSegment_End
                Next l_lngIndex
            End If
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_objItem As Object
Dim l_strItem As String
Dim l_strType As String
Dim l_lngIndex As Long
    With File
        .WriteSegment_Begin
            .Save CLng(mCol.Count)
            If mCol.Count > 0 Then
                For l_lngIndex = 1 To mCol.Count
                    .WriteSegment_Begin
                        Set l_objItem = Nothing
                        l_strItem = vbNullString
                        Set l_objItem = mCol(l_lngIndex)
                        If l_objItem Is Nothing Then
                            l_strItem = mCol(l_lngIndex)
                        End If
                        If l_strItem <> vbNullString Then
                            .Save "String"
                            .Save l_strItem
                        Else
                            l_strType = TypeName(l_objItem)
                            .Save l_strType
                            If Not (l_objItem Is Nothing) Then .Save l_objItem
                        End If
                    .WriteSegment_End
                Next l_lngIndex
            End If
        .WriteSegment_End
    End With
End Sub

