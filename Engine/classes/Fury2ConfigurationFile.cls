VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ConfigurationFile"
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
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Option Compare Text

Implements Fury2Object

Private Type Setting
    Name As String
    Value As String
End Type

Private m_setSettings() As Setting
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Function ToBoolean(ByVal SettingName As String) As Boolean
On Error Resume Next
    Select Case LCase(Trim(Settings(SettingName)))
    Case "yes", "true", "1", "-1", "on", "ok", "enabled", "enable", "allow", "allowed"
        ToBoolean = True
    Case Else
        ToBoolean = False
    End Select
End Function

Public Function Load(Filename As String) As Boolean
On Error Resume Next
Dim l_varSettings As Variant
Dim l_strSettings As String
Dim l_lngSettings As Long
Dim l_strSetting As String
Dim l_varSetting As Variant
    If Engine.Filesystem.FileExists(Filename) Then
        l_strSettings = Engine.Filesystem.File(Filename).Text
        l_varSettings = Split(l_strSettings, vbCrLf)
        For l_lngSettings = LBound(l_varSettings) To UBound(l_varSettings)
            l_strSetting = l_varSettings(l_lngSettings)
            If InStr(l_strSetting, "//") Then l_strSetting = Left(l_strSetting, InStr(l_strSetting, "//") - 1)
            If InStr(l_strSetting, ":") Then
                Settings(Left(l_strSetting, InStr(l_strSetting, ":") - 1)) = Mid(l_strSetting, InStr(l_strSetting, ":") + 1)
            End If
        Next l_lngSettings
    End If
End Function

Public Sub Save(Filename As String)
On Error Resume Next
Dim l_strSettings As String
Dim l_lngSettings As Long
    For l_lngSettings = LBound(m_setSettings) To UBound(m_setSettings)
        With m_setSettings(l_lngSettings)
            l_strSettings = l_strSettings & .Name & ":" & .Value & vbCrLf
        End With
    Next l_lngSettings
    Engine.Filesystem.CreateFile(Filename).Text = l_strSettings
End Sub

Public Sub DefineSettings(Settings As Variant)
On Error Resume Next
Dim l_lngSettings As Long
Dim l_varSetting As Variant
    If (VarType(Settings) And vbArray) = vbArray Then
        For l_lngSettings = LBound(Settings) To UBound(Settings)
            l_varSetting = Settings(l_lngSettings)
            If (VarType(l_varSetting) And vbArray) = vbArray Then
                DefineSetting CStr(l_varSetting(0)), CStr(l_varSetting(1))
            Else
                DefineSetting CStr(l_varSetting)
            End If
        Next l_lngSettings
    End If
End Sub

Public Sub DefineSetting(Name As String, Optional Default As String = "")
On Error Resume Next
Dim l_lngUBound As Long
    Err.Clear
    l_lngUBound = UBound(m_setSettings)
    If Err <> 0 Or l_lngUBound < 0 Then
        ReDim m_setSettings(0 To 0)
    Else
        ReDim Preserve m_setSettings(0 To UBound(m_setSettings) + 1)
    End If
    With m_setSettings(UBound(m_setSettings))
        .Name = Trim(Name)
        .Value = Default
    End With
End Sub

Public Sub ClearSettings()
On Error Resume Next
    Erase m_setSettings
End Sub

Public Property Let Settings(ByVal Name As String, ByVal Value As String)
On Error Resume Next
Dim l_lngSettings As Long
    Name = Trim(Name)
    For l_lngSettings = LBound(m_setSettings) To UBound(m_setSettings)
        With m_setSettings(l_lngSettings)
            If .Name = Name Then
                .Value = Value
                Exit Property
            End If
        End With
    Next l_lngSettings
End Property

Public Property Get Settings(ByVal Name As String) As String
On Error Resume Next
Dim l_lngSettings As Long
    Name = Trim(Name)
    For l_lngSettings = LBound(m_setSettings) To UBound(m_setSettings)
        With m_setSettings(l_lngSettings)
            If .Name = Name Then
                Settings = .Value
                Exit Property
            End If
        End With
    Next l_lngSettings
End Property

Public Sub InitDefaultSettings()
On Error Resume Next
    ClearSettings
    DefineSettings Array(Array("Show Configuration Dialog", "Yes"), Array("Title", "Untitled Game"), Array("Screen Mode", "320x240 @ 32"), Array("Framerate", "60fps"), _
    Array("Special FX", "Yes"), Array("Output Plugin", "GDI"), Array("Vertical Sync", "No"), Array("Show Framerate", "Yes"), Array("Show Titlebar", "Yes"), _
    Array("Default Transition", "Crossfade"), Array("Default Transition Length", "1.0"), "Default Map", Array("Default Font", "TTF;Tahoma"), "Default Window Skin", _
    Array("Default Start Position", "0 @ 0, 0"), Array("Enable Sound", "Yes"), Array("Music Volume", "0.65"), Array("Language", "VBScript"), Array("Icon", "/icon.ico"), _
    Array("Default Cursor", ""), Array("Screen Scale Ratio", "1.0"), Array("Output Scaling Mode", "Fast"), Array("Fullscreen", "No"))
End Sub
