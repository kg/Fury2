Attribute VB_Name = "mdlEngine"
Option Explicit
Private m_booEngineInitialized As Boolean

Sub InitEngine(ByVal GamePath As String)
On Error Resume Next
    If m_booEngineInitialized Then ShutdownEngine
    SetStatus "Initializing Engine"
    g_edEditor.GamePath = GamePath
    With Fury2Load(GamePath, EM_Library, g_edEditor)
    End With
    If Engine.Engine Is Nothing Then
        MsgBox "Unable to initialize engine.", vbCritical, "Error"
    Else
        m_booEngineInitialized = True
    End If
    SetStatus
End Sub

Sub ShutdownEngine()
On Error Resume Next
    If m_booEngineInitialized = False Then Exit Sub
    Fury2Shutdown
    m_booEngineInitialized = False
End Sub

Sub InstallEngine()
On Error Resume Next
    RegisterServer App.Path & "\graphics.dll", True
    RegisterServer App.Path & "\sound.dll", True
    RegisterServer App.Path & "\packages.dll", True
    RegisterServer App.Path & "\filesystem.dll", True
    RegisterServer App.Path & "\engine.dll", True
End Sub
