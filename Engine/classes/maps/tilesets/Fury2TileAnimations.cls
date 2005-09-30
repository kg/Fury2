VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2TileAnimations"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Tile Animation Collection"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Fury2TileAnimation"
Attribute VB_Ext_KEY = "Member0" ,"Fury2TileAnimation"
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
Private mCol As Collection
Private mvarMapTable() As Integer

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

Public Sub MultiAddInteger(Delay, Tiles() As Integer)
On Error Resume Next
Dim m_lngTiles As Long
Dim m_anmNew As Fury2TileAnimation
Dim m_lngMap As Long
Dim m_intSubtiles() As Integer
    For m_lngTiles = LBound(Tiles) To UBound(Tiles)
        Set m_anmNew = New Fury2TileAnimation
        m_anmNew.Tile = CInt(Tiles(m_lngTiles))
        m_anmNew.Resize (UBound(Tiles) - LBound(Tiles)) + 1
        For m_lngMap = LBound(Tiles) To UBound(Tiles)
            m_anmNew.Frame(m_lngMap) = Tiles((m_lngMap + m_lngTiles) Mod (UBound(Tiles) + 1))
            m_anmNew.Delay(m_lngMap) = CInt(Delay)
        Next m_lngMap
        m_anmNew.Tile = CInt(Tiles(m_lngTiles))
        mCol.Add m_anmNew
        Set m_anmNew = Nothing
    Next m_lngTiles
End Sub

Public Sub MultiAdd(Delay, ParamArray Tiles())
On Error Resume Next
Dim m_lngTiles As Long
Dim m_anmNew As Fury2TileAnimation
Dim m_lngMap As Long
Dim m_intSubtiles() As Integer
    For m_lngTiles = LBound(Tiles) To UBound(Tiles)
        Set m_anmNew = New Fury2TileAnimation
        m_anmNew.Tile = CInt(Tiles(m_lngTiles))
        m_anmNew.Resize (UBound(Tiles) - LBound(Tiles)) + 1
        For m_lngMap = LBound(Tiles) To UBound(Tiles)
            m_anmNew.Frame(m_lngMap) = Tiles((m_lngMap + m_lngTiles) Mod (UBound(Tiles) + 1))
            m_anmNew.Delay(m_lngMap) = CInt(Delay)
        Next m_lngMap
        m_anmNew.Tile = CInt(Tiles(m_lngTiles))
        mCol.Add m_anmNew
        Set m_anmNew = Nothing
    Next m_lngTiles
End Sub

Public Function Pointer(Optional Index As Long = 0)
On Error Resume Next
    Pointer = VarPtr(mvarMapTable(Index))
End Function

Public Sub RedimMapTable(TileCount As Long)
On Error Resume Next
Dim m_lngIndex As Long
    ReDim mvarMapTable(0 To (TileCount * 2) - 1)
    For m_lngIndex = 0 To (TileCount * 2) - 1
        mvarMapTable(m_lngIndex) = ClipValue(m_lngIndex, 0, TileCount - 1)
    Next m_lngIndex
End Sub

Sub Update()
On Error Resume Next
    If mCol.Count < 1 Then Exit Sub
Dim l_tanAnimation As Fury2TileAnimation
    For Each l_tanAnimation In mCol
        l_tanAnimation.Update
        mvarMapTable(l_tanAnimation.Tile) = l_tanAnimation.Frame(l_tanAnimation.CurrentFrame)
    Next l_tanAnimation
End Sub

Public Function Add(Obj As Fury2TileAnimation, Optional sKey As String)
On Error Resume Next
    If mCol.Count >= c_lngMaxAnimations Then Exit Function
    If Len(sKey) = 0 Then
        mCol.Add Obj
    Else
        mCol.Add Obj, sKey
    End If
End Function

Public Property Get Item(ByVal vntIndexKey As Variant) As Fury2TileAnimation
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
    ReDim mvarMapTable(0 To 0)
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set mCol = Nothing
    Erase mvarMapTable
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
    Set IVirtualFileCollection_CreateNew = New Fury2TileAnimation
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

