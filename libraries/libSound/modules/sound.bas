Attribute VB_Name = "mdlSound"
Option Explicit

'
'   ::fury² sound engine::
'
Public m_FModHandle As Long
Public Declare Function FSOUND_GetVersion Lib "fmod.dll" Alias "_FSOUND_GetVersion@0" () As Single

Sub InitSE()
On Error Resume Next
#If F2GC Then
#Else
    If m_Engine.DisableSound Then
    Else
#End If
        Err.Clear
#If F2GC Then
        m_FModHandle = FMod_Utils.Load(App.Path + IIf(Right(App.Path, 1) = "\", "", "\") + "fmod.dll")
#Else
        m_FModHandle = FMod_Utils.Load(RootPath + "fmod.dll")
#End If
        If m_FModHandle = 0 Then MsgBox "Unable to load FMod.dll!": Exit Sub
        Call FSOUND_GetVersion ' Trick VB into loading the DLL properly
#If F2GC Then
        FSound.Init 44100, 64, 0
#Else
        FSound.Init m_Engine.SamplingRate, c_lngSFXChannelCount, 2 ' Initialize
#End If
#If F2GC Then
#Else
    End If
#End If
End Sub

Sub ShutdownSE()
On Error Resume Next
#If F2GC Then
#Else
    If m_Engine.DisableSound Then
    Else
#End If
        FSound.Close
        FMod_Utils.Unload m_FModHandle
        m_FModHandle = 0
#If F2GC Then
#Else
    End If
#End If
End Sub
