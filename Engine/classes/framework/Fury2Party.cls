VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Party"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Party/Party Member Collection"
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
Implements Fury2TrackedObject
Implements IVirtualFileCollection

Private mvarRevision As Fury2RevisionData
Private mvarMembers As Collection
Public Money As Long
Public Leader As Long
Public Controller As Fury2Controller
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Property Get Revision() As Fury2RevisionData
    Set Revision = mvarRevision
End Property

Public Sub InitMap(ByVal Index As Long, ByVal StartX As Single, ByVal StartY As Single, ByVal Layer As Long)
On Error Resume Next
Dim m_lngItems As Long
    If mvarMembers.Count > 0 Then
        For m_lngItems = 1 To mvarMembers.Count
            mvarMembers(m_lngItems).InitMap Index, StartX, StartY, Layer
            mvarMembers(m_lngItems).InitMap Index, StartX, StartY
        Next m_lngItems
    End If
    If mvarMembers.Count > 0 Then
        If Controller Is Nothing Then
        Else
            Engine.UnHookEvents Controller
            Set Controller = Nothing
        End If
        Set Controller = Engine.F2Controller(mvarMembers(Leader).Sprite, Engine.Cameras(1))
        If Controller Is Nothing Then
            Set Controller = New Fury2Controller
            Set Controller.Camera = Engine.Cameras(1)
        End If
        Engine.HookEvents Controller
    End If
End Sub

Public Function CreateMember() As Object
On Error Resume Next
    Err.Clear
    With Engine.ScriptEngine
        .AddCode .Language.GenerateAssignment("NewPartyMember", .Language.GenerateInstantiation("PartyMember"), True)
        Set CreateMember = .Eval("NewPartyMember")
    End With
    Err.Clear
End Function

Public Sub Add(ByVal Obj As Object, Optional ByVal sKey As String = "")
On Error Resume Next
    If Len(sKey) = 0 Then
        mvarMembers.Add Obj
    Else
        mvarMembers.Add Obj, sKey
    End If
End Sub

Public Property Get Item(vntIndexKey As Variant) As Object
Attribute Item.VB_UserMemId = 0
Attribute Item.VB_MemberFlags = "200"
On Error Resume Next
Dim m_lngMembers As Long, m_strName As String
  Set Item = mvarMembers(vntIndexKey)
  If VarType(vntIndexKey) = vbString And Item Is Nothing Then
        m_strName = LCase(Trim(CStr(vntIndexKey)))
        If mvarMembers.Count > 0 Then
            For m_lngMembers = 1 To mvarMembers.Count
                If LCase(Trim(mvarMembers(m_lngMembers).Name)) = m_strName Then
                    Set Item = mvarMembers(m_lngMembers)
                    Exit For
                End If
            Next m_lngMembers
        End If
  End If
End Property

Public Property Get Count() As Long
    Count = mvarMembers.Count
End Property

Public Sub Remove(vntIndexKey As Variant)
On Error Resume Next
    mvarMembers.Remove vntIndexKey
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    Set NewEnum = mvarMembers.[_NewEnum]
End Property

Private Sub Class_Initialize()
    Set mvarRevision = New Fury2RevisionData
    Set mvarMembers = New Collection
    Leader = 1
End Sub

Private Property Get Fury2TrackedObject_Revision() As Fury2RevisionData
    Set Fury2TrackedObject_Revision = mvarRevision
End Property

Private Sub IVirtualFileCollection_AddItem(Item As Object)
On Error Resume Next
    mvarMembers.Add Item
End Sub

Private Property Get IVirtualFileCollection_Count() As Long
On Error Resume Next
    IVirtualFileCollection_Count = mvarMembers.Count
End Property

Private Function IVirtualFileCollection_CreateNew() As Object
On Error Resume Next
    Set IVirtualFileCollection_CreateNew = CreateMember
End Function

Private Function IVirtualFileCollection_GetItem(ByVal Index As Long) As Object
On Error Resume Next
    Set IVirtualFileCollection_GetItem = mvarMembers.Item(Index)
End Function

Private Sub IVirtualFileCollection_Reset()
On Error Resume Next
    Set mvarMembers = New Collection
End Sub

Private Sub IVirtualFileCollection_SaveHeader(File As libGraphics.VirtualFile)
On Error Resume Next
    File.WriteSegment_Begin
        File.Save Money
    File.WriteSegment_End
End Sub

Private Sub IVirtualFileCollection_LoadHeader(File As libGraphics.VirtualFile)
On Error Resume Next
    File.ReadSegment_Begin
        File.Load Money
    File.ReadSegment_End
End Sub

