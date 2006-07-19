VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Strings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Function PadString(ByRef Text As String, ByVal Length As Long, Optional ByVal PaddingCharacter As String = " ") As String
On Error Resume Next
    PadString = PadLeft(Text, Length, PaddingCharacter)
End Function

Public Function PadStringR(ByRef Text As String, ByVal Length As Long, Optional ByVal PaddingCharacter As String = " ") As String
On Error Resume Next
    PadStringR = PadRight(Text, Length, PaddingCharacter)
End Function

Public Function PadLeft(ByRef Text As String, ByVal Length As Long, Optional ByRef PaddingCharacter As String = " ") As String
On Error Resume Next
Dim m_lngLength As Long
    m_lngLength = Len(Text)
    If m_lngLength < Length Then
        PadLeft = String(Length - m_lngLength, PaddingCharacter) + Text
    ElseIf m_lngLength > Length Then
        PadLeft = right(Text, Length)
    Else
        PadLeft = Text
    End If
End Function

Public Function PadRight(ByRef Text As String, ByVal Length As Long, Optional ByRef PaddingCharacter As String = " ") As String
On Error Resume Next
Dim m_lngLength As Long
    m_lngLength = Len(Text)
    If m_lngLength < Length Then
        PadRight = Text + String(Length - m_lngLength, PaddingCharacter)
    ElseIf m_lngLength > Length Then
        PadRight = left(Text, Length)
    Else
        PadRight = Text
    End If
End Function

Public Function PatternMatch(ByVal Text As String, ByVal Pattern As String) As Boolean
On Error Resume Next
    PatternMatch = Text Like Pattern
End Function

Public Function Match(ByRef Text As String, ByRef Pattern As String) As Boolean
On Error Resume Next
    Match = Text Like Pattern
End Function

Public Function Reverse(ByRef Text As String) As String
On Error Resume Next
    Reverse = StrReverse(Text)
End Function
