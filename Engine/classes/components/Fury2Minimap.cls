VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Minimap"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
'ng ;
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Implements Fury2Object
Public Engine As Fury2Engine
Public X As Single
Public Y As Single
Public MapImage As Object
Public MarkerImage As Object
Public ShowMonsters As Boolean
Public ShowNPCs As Boolean
Public Sprite As Object
Public Border As Boolean
Public Alpha As Single
Public MarkerAlpha As Single
Public MarkerFadeSpeed As Single
Public MarkerBlitMode As Long
Private m_sngX As Single, m_sngY As Single

Public Property Get DrawBehind() As Boolean
    DrawBehind = False
End Property

Public Sub Update()
On Error Resume Next
    m_sngX = (((Sprite.X / Sprite.Parent.MaxX) * MapImage.Width) + X) - (MarkerImage.Width / 2)
    m_sngY = (((Sprite.Y / Sprite.Parent.MaxY) * MapImage.Height) + Y) - (MarkerImage.Height / 2)
    If MarkerFadeSpeed = 0 Then Exit Sub
    MarkerAlpha = MarkerAlpha + MarkerFadeSpeed
    If (MarkerAlpha <= 0) Or (MarkerAlpha >= 1) Then
        MarkerFadeSpeed = -MarkerFadeSpeed
    End If
End Sub

Public Sub Redraw()
On Error Resume Next
    With Engine.Backbuffer
        If Border Then
'            .DrawBox F2Rect(x, y, MapImage.Width + 2, MapImage.Height + 2, False), F2RGB(255, 255, 255, Alpha * 255)
            .Blit F2Rect(X + 1, Y + 1, MapImage.Width, MapImage.Height, False), MapImage.Rectangle, MapImage, Alpha, BlitMode_Matte
            .Blit F2Rect(m_sngX + 1, m_sngY + 1, MarkerImage.Width, MarkerImage.Height, False), MarkerImage.Rectangle, MarkerImage, MarkerAlpha, MarkerBlitMode
        Else
            .Blit F2Rect(X, Y, MapImage.Width, MapImage.Height, False), MapImage.Rectangle, MapImage, Alpha, BlitMode_Matte
            .Blit F2Rect(m_sngX, m_sngY, MarkerImage.Width, MarkerImage.Height, False), MarkerImage.Rectangle, MarkerImage, MarkerAlpha, MarkerBlitMode
        End If
    End With
    Err.Clear
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    MarkerBlitMode = BlitMode_Additive
    Alpha = 0.66
    MarkerAlpha = 0.7
    MarkerFadeSpeed = -((0.7) / 20)
End Sub
