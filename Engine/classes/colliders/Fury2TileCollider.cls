VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2TileCollider"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Per-tile Collision Handler"
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

Implements Fury2Object
Implements Fury2Collider

Private mvarHookedMap As Fury2Map

Public Property Set Fury2Collider_HookedMap(vData As Fury2Map)
'used when assigning an Object to the property, on the left side of a Set statement.
'Syntax: Set X.HookedMap = Form1
    Set mvarHookedMap = vData
End Property

Public Property Get Fury2Collider_HookedMap() As Fury2Map
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.HookedMap
    Set HookedMap = mvarHookedMap
End Property

Public Function Fury2Collider_Blocked(ByVal X As Single, ByVal y As Single, ByVal Layer As Long) As Byte
On Error Resume Next
Dim m_lngTileX As Long, m_lngTileY As Long
Dim m_lngSprites As Long
Dim m_rctRect As Win32.Rect
    With mvarHookedMap.Layers(Layer)
        If (X < 0) Or (X > mvarHookedMap.MaxX) Then
            If mvarHookedMap.Layers(Layer).WrapX Then
            Else
                Fury2Collider_Blocked = True
            End If
        ElseIf (y < 0) Or (y > mvarHookedMap.MaxY) Then
            If mvarHookedMap.Layers(Layer).WrapY Then
            Else
                Fury2Collider_Blocked = True
            End If
        Else
            If .DisableBlocking Then
                Fury2Collider_Blocked = False
            Else
                m_lngTileX = X \ .BlockingGridWidth
                m_lngTileY = y \ .BlockingGridHeight
                Fury2Collider_Blocked = .Blocking(m_lngTileX, m_lngTileY)
            End If
        End If
    End With
End Function
