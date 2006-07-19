Attribute VB_Name = "mdlParticles"
Option Explicit

Public Sub ParticleDieCallback(ByRef Evt As ParticleDieEvent)
On Error Resume Next
Dim l_typType As Fury2ParticleType
Dim l_evtEvent As Fury2Event
Dim l_lngNull As Long
    CopyBytes VarPtr(l_typType), VarPtr(Evt.UserData), 4
    Set l_evtEvent = l_typType.DieEvent
    If Not (l_evtEvent Is Nothing) Then l_evtEvent.Invoke
    CopyBytes VarPtr(l_typType), VarPtr(l_lngNull), 4
    Err.Clear
End Sub

Public Sub ParticleCollideCallback(ByRef Evt As ParticleCollideEvent)
On Error Resume Next
Dim l_typType As Fury2ParticleType
Dim l_evtEvent As Fury2Event
Dim l_lngNull As Long
    CopyBytes VarPtr(l_typType), VarPtr(Evt.UserData), 4
    Set l_evtEvent = l_typType.CollideEvent
    If Not (l_evtEvent Is Nothing) Then l_evtEvent.Invoke
    CopyBytes VarPtr(l_typType), VarPtr(l_lngNull), 4
    Err.Clear
End Sub

