VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cTileUndoEntry"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
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
Implements iUndoEntry

Public X As Long, Y As Long
Public Brush As Fury2Brush
Public Map As Fury2Map
Public Layer As Long

Private Function iUndoEntry_Apply() As Boolean
On Error Resume Next
    If Map Is Nothing Then Exit Function
    If Brush Is Nothing Then Exit Function
    If Layer < 1 Then Exit Function
    If Layer > Map.Layers.Count Then Exit Function
    Screen.MousePointer = 11
    Brush.Draw Map, Layer, X, Y
    Screen.MousePointer = 0
    iUndoEntry_Apply = True
End Function

Private Function iUndoEntry_CreateReverse() As iUndoEntry
On Error Resume Next
Dim l_undReverser As New cTileUndoEntry
Dim l_brsReverse As New Fury2Brush
    With l_undReverser
        .X = X
        .Y = Y
        .Layer = Layer
        Set .Map = Me.Map
        With l_brsReverse
            Screen.MousePointer = 11
            .Grab Map, Layer, X, Y, Brush.Width, Brush.Height, , , True
            Screen.MousePointer = 0
        End With
        Set .Brush = l_brsReverse
    End With
    Set iUndoEntry_CreateReverse = l_undReverser
End Function
