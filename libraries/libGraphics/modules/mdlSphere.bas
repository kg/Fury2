Attribute VB_Name = "mdlSphere"
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

Public Type RWS_Header
    Signature As String * 4
    Version As Integer
    EdgeWidth As Byte
    BackgroundMode As Byte
    CornerColor1 As Long
    CornerColor2 As Long
    CornerColor3 As Long
    CornerColor4 As Long
    BorderOffsetLeft As Byte
    BorderOffsetTop As Byte
    BorderOffsetRight As Byte
    BorderOffsetBottom As Byte
    Reserved As String * 36
End Type

Public Type RTS_Header
    Signature As String * 4 ' assume .rts
    Version As Integer ' assume 1
    NumberOfTiles As Integer
    TileWidth As Integer
    TileHeight As Integer
    TileBPP As Integer ' 24 or 32
    CompressionMode As Byte ' assume 0
    Reserved As String * 241
End Type
