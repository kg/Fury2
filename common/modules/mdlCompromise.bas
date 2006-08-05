Attribute VB_Name = "Compromise"
Option Explicit
Public Declare Function Initialize Lib "Compromise" Alias "_Initialize@0" () As Long
Public Declare Function UnInitialize Lib "Compromise" Alias "_Uninitialize@0" () As Long

Public Declare Function Register_ Lib "Compromise" Alias "_Register@4" (ByVal Filename As Long) As Long
Public Declare Function Unregister_ Lib "Compromise" Alias "_Unregister@4" (ByVal Filename As Long) As Long

Public Declare Function IsSupported Lib "Compromise" Alias "_IsSupported@0" () As Long

Public Declare Function GetEnabled Lib "Compromise" Alias "_GetEnabled@0" () As Long
Public Declare Sub SetEnabled Lib "Compromise" Alias "_SetEnabled@4" (ByVal State As Long)

Private m_lngCompromiseLibrary As Long

Public Sub LoadCompromise()
On Error Resume Next
    m_lngCompromiseLibrary = LoadLibrary("compromise.dll")
    If m_lngCompromiseLibrary = 0 Then
        Err.Raise GetLastError, "Unable to load Compromise.dll"
    End If
End Sub

Public Function Register(ByRef Filename As String) As Long
On Error Resume Next
    Register = Register_(ByVal StrPtr(Filename))
End Function

Public Function Unregister(ByRef Filename As String) As Long
On Error Resume Next
    Unregister = Unregister_(ByVal StrPtr(Filename))
End Function

