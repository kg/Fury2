VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cPropertyUndoEntry"
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

Public Object As Object
Public MethodName As String
Public Value As Variant

Private Function iUndoEntry_Apply() As Boolean
On Error Resume Next
Dim l_vtType As VbVarType
    l_vtType = VarType(Value)
    If (l_vtType And vbObject) = vbObject Then
        CallByName Object, MethodName, VbSet, Value
    ElseIf l_vtType = vbString Then
        CallByName Object, MethodName, VbLet, CStr(Value)
    ElseIf l_vtType = vbLong Then
        CallByName Object, MethodName, VbLet, CLng(Value)
    ElseIf l_vtType = vbInteger Then
        CallByName Object, MethodName, VbLet, CInt(Value)
    ElseIf l_vtType = vbSingle Then
        CallByName Object, MethodName, VbLet, CSng(Value)
    ElseIf l_vtType = vbDouble Then
        CallByName Object, MethodName, VbLet, CDbl(Value)
    ElseIf l_vtType = vbByte Then
        CallByName Object, MethodName, VbLet, CByte(Value)
    ElseIf l_vtType = vbBoolean Then
        CallByName Object, MethodName, VbLet, CBool(Value)
    Else
        CallByName Object, MethodName, VbLet, Value
    End If
End Function

Private Function iUndoEntry_CreateReverse() As iUndoEntry
On Error Resume Next
Dim l_undReverser As New cPropertyUndoEntry
    With l_undReverser
        Set .Object = Me.Object
        .MethodName = Me.MethodName
        If (VarType(Me.Value) And vbObject) = vbObject Then
            Set .Value = CallByName(Object, MethodName, VbGet)
        Else
            .Value = CallByName(Object, MethodName, VbGet)
        End If
    End With
    Set iUndoEntry_CreateReverse = l_undReverser
End Function

