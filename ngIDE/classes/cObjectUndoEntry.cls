VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cObjectUndoEntry"
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

Public Enum ObjectUndoOperations
    OUO_Add
    OUO_Remove
End Enum

Public Container As Object
Public Value As Object
Public Operation As ObjectUndoOperations
Public Index As Long

Private Sub Class_Terminate()
On Error Resume Next
    Value.Free
End Sub

Private Function iUndoEntry_Apply() As Boolean
On Error Resume Next
Dim l_colContainer As Collection
    If Operation = OUO_Add Then
        If TypeOf Container Is Collection Then
            Set l_colContainer = Container
            If (Index > 1) Then
                l_colContainer.Add Value, , , Index - 1
            Else
                l_colContainer.Add Value
            End If
        Else
            Container.Add Value, , Index
        End If
    ElseIf Operation = OUO_Remove Then
        Err.Clear
        If TypeOf Container Is Collection Then
            Set l_colContainer = Container
            l_colContainer.Remove Index
        Else
            Container.Remove Index
        End If
    End If
End Function

Private Function iUndoEntry_CreateReverse() As iUndoEntry
On Error Resume Next
Dim l_undReverser As New cObjectUndoEntry
    With l_undReverser
        Set .Container = Me.Container
        .Index = Me.Index
        .Operation = IIf(Me.Operation = OUO_Add, OUO_Remove, OUO_Add)
        Set .Value = Me.Value
    End With
    Set iUndoEntry_CreateReverse = l_undReverser
End Function

