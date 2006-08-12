Attribute VB_Name = "mdlSound"
Option Explicit
Public EnableCallbacks As Boolean

Public Function FMod_Channel_Callback(ByVal Channel As Long, ByVal CallbackType As FMOD_CHANNEL_CALLBACKTYPE, ByVal Command As Long, ByVal Data1 As Long, ByVal Data2 As Long) As Long
On Error Resume Next
Dim l_objRef As Object
Dim l_chnChannel As Channel
Dim l_lngSound As Long
Dim l_lngSyncPoint As Long
Dim l_lngUserData As Long
Dim l_bytName(0 To 256) As Byte
Dim l_strName As String
Dim l_lngOffset As Long
'Dim l_lngHandle As Long
    If Not EnableCallbacks Then Exit Function
    Debug.Print "FMod_Channel_Callback"
    ' Grab object handle
    FMOD_Channel_GetUserData Channel, l_lngUserData
    FMOD_Channel_GetCurrentSound Channel, l_lngSound
    CopyMemory ByVal VarPtr(l_chnChannel), ByVal VarPtr(l_lngUserData), 4
    
    ' Can't do anything without a channel handle
    If Not (l_chnChannel Is Nothing) Then
        Select Case CallbackType
        Case FMOD_CHANNEL_CALLBACKTYPE_END
            l_chnChannel.FireStopCallback
        Case FMOD_CHANNEL_CALLBACKTYPE_VIRTUALVOICE
        Case FMOD_CHANNEL_CALLBACKTYPE_SYNCPOINT
            FMOD_Sound_GetSyncPoint l_lngSound, Data1, l_lngSyncPoint
            FMOD_Sound_GetSyncPointInfo l_lngSound, l_lngSyncPoint, l_bytName(0), 256, l_lngOffset, FMOD_TIMEUNIT_MS
            l_strName = StrConv(l_bytName, vbUnicode)
            l_strName = Left(l_strName, InStr(l_strName, Chr(0)) - 1)
'            l_lngHandle = FreeFile
'            Open "C:\fmod.log" For Append As #l_lngHandle
'            Print #l_lngHandle, "Sync point """ & l_strName & """ @ " & l_lngOffset & "ms in sound " & l_lngSound
'            Close #l_lngHandle
            Select Case LCase(Trim(l_strName))
            Case "start"
                l_chnChannel.FireStartCallback
            Case "end-1"
                l_chnChannel.FireBeforeEndCallback
            Case "end"
                l_chnChannel.FireEndCallback
            End Select
'        Case FMOD_CHANNEL_CALLBACKTYPE_MODZXX
'        Case FMOD_CHANNEL_CALLBACKTYPE_MODROW
'        Case FMOD_CHANNEL_CALLBACKTYPE_MODORDER
'        Case FMOD_CHANNEL_CALLBACKTYPE_MODINST
        End Select
    End If
    
    ' Free object handle
    l_lngUserData = 0
    CopyMemory ByVal VarPtr(l_chnChannel), ByVal VarPtr(l_lngUserData), 4
    
    FMod_Channel_Callback = FMOD_OK
End Function

Public Function FModVector(ByRef Values As Variant) As FMOD_VECTOR
On Error Resume Next
Dim l_vecResult As FMOD_VECTOR
    l_vecResult.X = CSng(Values(0))
    l_vecResult.Y = CSng(Values(1))
    l_vecResult.Z = CSng(Values(2))
    Err.Clear
    FModVector = l_vecResult
End Function
