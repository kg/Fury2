VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SoundFader"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements Fury2Plugin
Public Handle As SoundHandle
Public VolumeStart As Single
Public VolumeEnd As Single
Public Slider As Fury2Slider
Public FreeWhenDone As Boolean
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Private Sub Fury2Plugin_ControlActivate(Control As Fury2InputControl)
End Sub

Private Sub Fury2Plugin_ControlDeactivate(Control As Fury2InputControl)
End Sub

Private Property Get Fury2Plugin_DrawBehind() As Boolean
End Property

Private Property Set Fury2Plugin_Engine(ByVal RHS As Fury2Engine)
    Set m_engEngine = RHS
End Property

Private Property Get Fury2Plugin_Engine() As Fury2Engine
    Set Fury2Plugin_Engine = m_engEngine
End Property

Private Sub Fury2Plugin_Init()
End Sub

Private Sub Fury2Plugin_KeyDown(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_KeyPress(KeyAscii As Long)
End Sub

Private Sub Fury2Plugin_KeyUp(KeyCode As Long, ByVal Shift As Long)
End Sub

Private Sub Fury2Plugin_MapLoad(Filename As String)
End Sub

Private Sub Fury2Plugin_MouseDown(ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
End Sub

Private Sub Fury2Plugin_MouseMove(ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
End Sub

Private Sub Fury2Plugin_MouseUp(ByVal Button As Long, ByVal Shift As Long, X As Long, Y As Long)
End Sub

Private Sub Fury2Plugin_OutputPluginChanged()
End Sub

Private Sub Fury2Plugin_Redraw()
End Sub

Private Sub Fury2Plugin_Shutdown()
End Sub

Private Sub Fury2Plugin_Startup()
End Sub

Private Sub Fury2Plugin_SyncUpdate()
On Error Resume Next
Dim l_sngVolume As Single
    l_sngVolume = VolumeStart + ((VolumeEnd - VolumeStart) * Slider.Progress)
    Handle.Volume = l_sngVolume
    If Slider.Finished Then
        If FreeWhenDone Then
            Handle.Kill
            Handle.Free
            Set Handle = Nothing
        End If
        Engine.UnHookEvents Me
    End If
End Sub

Private Sub Fury2Plugin_Unload()
End Sub

Private Sub Fury2Plugin_Update()
End Sub
