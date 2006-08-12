VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ScriptConstants"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
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

Public Property Get Alphabet() As String
    Alphabet = "abcdefghijklmnopqrstuvwxyz"
End Property

Public Property Get Digits() As String
    Digits = "0123456789"
End Property

Public Property Get BinaryDigits() As String
    BinaryDigits = "01"
End Property

Public Property Get HexDigits() As String
    HexDigits = "0123456789ABCDEF"
End Property

Public Property Get OctalDigits() As String
    OctalDigits = "01234567"
End Property

Public Property Get Key(ByVal KeyCode As String) As Long
On Error Resume Next
    Select Case LCase(Trim(KeyCode))
    Case "~", "`"
        Key = 192
    Case "-", "_"
        Key = 189
    Case "=", "+"
        Key = 187
    Case "[", "{"
        Key = 219
    Case "]", "}"
        Key = 221
    Case "\", "|"
        Key = 220
    Case ";", ":"
        Key = 186
    Case "'", """"
        Key = 222
    Case ",", "<"
        Key = 188
    Case ".", ">"
        Key = 190
    Case "/", "?"
        Key = 191
    Case "esc", "escape"
        Key = vbKeyEscape
    Case "bksp", "back", "backspace"
        Key = vbKeyBack
    Case "ret", "return", "enter"
        Key = vbKeyReturn
    Case "ctl", "ctrl", "control"
        Key = vbKeyControl
    Case "tab"
        Key = vbKeyTab
    Case "pause"
        Key = vbKeyPause
    Case "shift"
        Key = vbKeyShift
    Case "caps", "caps lock", "capslock", "cap lock", "cap"
        Key = vbKeyCapital
    Case "spc", "space", "space bar", "spacebar"
        Key = vbKeySpace
    Case "pgup", "pg up", "pageup", "page up"
        Key = vbKeyPageUp
    Case "pgdn", "pg dn", "pagedown", "page down"
        Key = vbKeyPageDown
    Case "end"
        Key = vbKeyEnd
    Case "home"
        Key = vbKeyHome
    Case "ins", "insert"
        Key = vbKeyInsert
    Case "del", "delete"
        Key = vbKeyDelete
    Case "left", "left arrow"
        Key = 37
    Case "up", "up arrow"
        Key = 38
    Case "right", "right arrow"
        Key = 39
    Case "down", "down arrow"
        Key = 40
    Case Else
        If Len(KeyCode) = 1 Then
            Key = Asc(UCase(KeyCode))
            Exit Property
        End If
        If LCase(Left(KeyCode, 1)) = "f" Then
            Key = vbKeyF1 + CLng(Mid(KeyCode, 2)) - 1
        End If
    End Select
End Property

Public Property Get SoundType_Sample() As Long
    SoundType_Sample = 0
End Property
Public Property Get SoundType_Stream() As Long
    SoundType_Stream = 1
End Property
Public Property Get SoundType_Music() As Long
    SoundType_Music = 2
End Property

Public Property Get F2LM_PlayOnce() As Long
    F2LM_PlayOnce = 0
End Property
Public Property Get F2LM_Restart() As Long
    F2LM_Restart = 1
End Property
Public Property Get F2LM_PingPong() As Long
    F2LM_PingPong = 2
End Property

Public Property Get Transition_None() As Long
    Transition_None = 0
End Property
Public Property Get Transition_Fade() As Long
    Transition_Fade = 1
End Property
Public Property Get Transition_Crossfade() As Long
    Transition_Crossfade = 2
End Property

Public Property Get ControllerStyle_Normal() As Long
    ControllerStyle_Normal = 0
End Property
Public Property Get ControllerStyle_ResidentEvil() As Long
    ControllerStyle_ResidentEvil = 1
End Property
Public Property Get ControllerStyle_Platformer() As Long
    ControllerStyle_Platformer = 2
End Property
Public Property Get ControllerStyle_HorizontalShooter() As Long
    ControllerStyle_HorizontalShooter = 3
End Property
Public Property Get ControllerStyle_VerticalShooter() As Long
    ControllerStyle_VerticalShooter = 4
End Property

Public Property Get ResampleMode_Default() As Long
    ResampleMode_Default = -1
End Property
Public Property Get ResampleMode_None() As Long
    ResampleMode_None = 0
End Property
Public Property Get ResampleMode_Linear() As Long
    ResampleMode_Linear = 1
End Property
Public Property Get ResampleMode_Bilinear() As Long
    ResampleMode_Bilinear = 2
End Property
Public Property Get ResampleMode_Bilinear_High_Quality() As Long
    ResampleMode_Bilinear_High_Quality = 3
End Property
Public Property Get ResampleMode_Linear_Wrap() As Long
    ResampleMode_Linear_Wrap = 4
End Property
Public Property Get ResampleMode_Bilinear_Wrap() As Long
    ResampleMode_Bilinear_Wrap = 5
End Property
Public Property Get ResampleMode_Linear_Clamp() As Long
    ResampleMode_Linear_Clamp = 6
End Property
Public Property Get ResampleMode_Bilinear_Clamp() As Long
    ResampleMode_Bilinear_Clamp = 7
End Property

Public Property Get BlitMode_Default() As Long
    BlitMode_Default = -1
End Property
Public Property Get BlitMode_Normal() As Long
    BlitMode_Normal = 0
End Property
Public Property Get BlitMode_Matte() As Long
    BlitMode_Matte = 1
End Property
Public Property Get BlitMode_SourceAlpha() As Long
    BlitMode_SourceAlpha = 2
End Property
Public Property Get BlitMode_Additive() As Long
    BlitMode_Additive = 3
End Property
Public Property Get BlitMode_Subtractive() As Long
    BlitMode_Subtractive = 4
End Property
Public Property Get BlitMode_AND() As Long
    BlitMode_AND = 5
End Property
Public Property Get BlitMode_OR() As Long
    BlitMode_OR = 6
End Property
Public Property Get BlitMode_XOR() As Long
    BlitMode_XOR = 7
End Property
Public Property Get BlitMode_Lightmap() As Long
    BlitMode_Lightmap = 8
End Property
Public Property Get BlitMode_Lightmap_RGB() As Long
    BlitMode_Lightmap_RGB = 9
End Property
Public Property Get BlitMode_Matte_Tint() As Long
    BlitMode_Matte_Tint = 10
End Property
Public Property Get BlitMode_SourceAlpha_Tint() As Long
    BlitMode_SourceAlpha_Tint = 11
End Property
Public Property Get BlitMode_Font() As Long
    BlitMode_Font = 12
End Property
Public Property Get BlitMode_Font_SourceAlpha() As Long
    BlitMode_Font_SourceAlpha = 13
End Property
Public Property Get BlitMode_Dither() As Long
    BlitMode_Dither = 14
End Property
Public Property Get BlitMode_Screen() As Long
    BlitMode_Screen = 15
End Property
Public Property Get BlitMode_Multiply() As Long
    BlitMode_Multiply = 16
End Property
Public Property Get BlitMode_Merge() As Long
    BlitMode_Merge = 17
End Property
Public Property Get BlitMode_Unerase() As Long
    BlitMode_Unerase = 18
End Property
Public Property Get BlitMode_Erase() As Long
    BlitMode_Erase = 19
End Property
Public Property Get BlitMode_Font_Merge() As Long
    BlitMode_Font_Merge = 20
End Property
Public Property Get BlitMode_Behind() As Long
    BlitMode_Behind = 21
End Property
Public Property Get BlitMode_Dodge() As Long
    BlitMode_Dodge = 22
End Property
Public Property Get BlitMode_Burn() As Long
    BlitMode_Burn = 23
End Property
Public Property Get BlitMode_Normal_Tint() As Long
    BlitMode_Normal_Tint = 24
End Property
Public Property Get BlitMode_Additive_SourceAlpha() As Long
    BlitMode_Additive_SourceAlpha = 25
End Property
Public Property Get BlitMode_Subtractive_SourceAlpha() As Long
    BlitMode_Subtractive_SourceAlpha = 26
End Property
Public Property Get BlitMode_SourceAlpha_ColorMask() As Long
    BlitMode_SourceAlpha_ColorMask = 27
End Property
Public Property Get BlitMode_Difference() As Long
    BlitMode_Difference = 28
End Property
Public Property Get BlitMode_SourceAlpha_Premultiplied() As Long
    BlitMode_SourceAlpha_Premultiplied = 29
End Property

Public Property Get RenderMode_Default() As Long
    RenderMode_Default = -1
End Property
Public Property Get RenderMode_Normal() As Long
    RenderMode_Normal = 0
End Property
Public Property Get RenderMode_SourceAlpha() As Long
    RenderMode_SourceAlpha = 1
End Property
Public Property Get RenderMode_Additive() As Long
    RenderMode_Additive = 2
End Property
Public Property Get RenderMode_Subtractive() As Long
    RenderMode_Subtractive = 3
End Property
Public Property Get RenderMode_Merge() As Long
    RenderMode_Merge = 4
End Property
Public Property Get RenderMode_Shadow() As Long
    RenderMode_Shadow = 5
End Property
Public Property Get RenderMode_Screen() As Long
    RenderMode_Screen = 6
End Property
Public Property Get RenderMode_Additive_SourceAlpha() As Long
    RenderMode_Additive_SourceAlpha = 7
End Property
Public Property Get RenderMode_Subtractive_SourceAlpha() As Long
    RenderMode_Subtractive_SourceAlpha = 8
End Property
Public Property Get RenderMode_Font_SourceAlpha() As Long
    RenderMode_Font_SourceAlpha = 9
End Property
Public Property Get RenderMode_Unerase() As Long
    RenderMode_Unerase = 10
End Property
Public Property Get RenderMode_Erase() As Long
    RenderMode_Erase = 11
End Property

Public Property Get CallType_Set() As Long
    CallType_Set = 8
End Property
Public Property Get CallType_Let() As Long
    CallType_Let = 4
End Property
Public Property Get CallType_Get() As Long
    CallType_Get = 2
End Property
Public Property Get CallType_Call() As Long
    CallType_Call = 1
End Property

Public Property Get Red() As Long
    Red = 2
End Property
Public Property Get Green() As Long
    Green = 1
End Property
Public Property Get Blue() As Long
    Blue = 0
End Property
Public Property Get Alpha() As Long
    Alpha = 3
End Property

Public Property Get Radian() As Double
    Radian = c_dblRadian
End Property
Public Property Get Pi() As Double
    Pi = c_dblPi
End Property
Public Property Get Sqrt2() As Double
    Sqrt2 = c_dblSqrt2
End Property

Public Property Get F2TM_Instant() As Long
    F2TM_Instant = 0
End Property
Public Property Get F2TM_Linear() As Long
    F2TM_Linear = 1
End Property

Public Property Get F2TT_Number() As Long
    F2TT_Number = 0
End Property
Public Property Get F2TT_Integer() As Long
    F2TT_Integer = 1
End Property
Public Property Get F2TT_Color() As Long
    F2TT_Color = 2
End Property

