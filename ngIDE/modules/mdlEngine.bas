Attribute VB_Name = "mdlEngine"
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
Private m_booEngineInitialized As Boolean
Global g_engEngine As Fury2Engine

Sub InitEngine(ByVal GamePath As String)
On Error Resume Next
    If m_booEngineInitialized Then ShutdownEngine
    SetStatus "Initializing Engine"
    g_edEditor.GamePath = GamePath
    Set g_engEngine = Fury2Load(GamePath, EM_Library, g_edEditor)
    If g_engEngine Is Nothing Then
        g_edEditor.ShowNotice "Error", "Unable to initialize engine.", g_edEditor.NoticeIcon("error")
    Else
        m_booEngineInitialized = True
    End If
    Set g_edEditor.ProgressCallback = g_engEngine.BindEvent("Fury2SetProgress", g_edEditor)
    SetStatus
End Sub

Sub ShutdownEngine()
On Error Resume Next
    If m_booEngineInitialized = False Then Exit Sub
    m_booEngineInitialized = False
    'g_engEngine.Quit
    Set g_engEngine = Nothing
End Sub

Sub InstallEngine()
On Error Resume Next
    RegisterServer App.Path & "\graphics.dll", True
    RegisterServer App.Path & "\sound.dll", True
    RegisterServer App.Path & "\packages.dll", True
    RegisterServer App.Path & "\filesystem.dll", True
    RegisterServer App.Path & "\engine.dll", True
    RegisterServer App.Path & "\debugger.dll", True
End Sub
