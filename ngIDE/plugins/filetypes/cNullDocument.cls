VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cNullDocument"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'
'    ngIDE (Fury² Game Creation System Next-Generation Editor)
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
Implements iDocument

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Private Property Get iDocument_CanSave() As Boolean

End Property

Private Property Get iDocument_Filename() As String

End Property

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin

End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)

End Property

Private Function iDocument_Save(Filename As String) As Boolean

End Function

Private Property Get iDocument_Typename() As String

End Property

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = False
End Property
