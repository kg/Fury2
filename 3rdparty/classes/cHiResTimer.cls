VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cHiResTimer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

' No need to use GetPerformanceCounter() because this
' will be accurate to the highest timer resolution:
Private Declare Function timeGetTime Lib "winmm.dll" () As Long
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, lpvSource As Any, ByVal cbCopy As Long)

Private Type tTimerData
   lInterval As Long
   sKey As String
   bEnabled As Boolean
   lLastTick As Long
End Type
Private m_tT() As tTimerData
Private m_iCount As Long
Private m_lPtr As Long

Public Event Timer(ByVal sKey As String)

Public Sub Connect(iTmr As ITimer)
   ' Do this is you want to get an implemented
   ' call:
   m_lPtr = ObjPtr(iTmr)
End Sub

Private Property Get ObjectFromPtr(ByVal lPtr As Long) As ITimer
Dim oThis As ITimer
    CopyMemory oThis, lPtr, 4
    Set ObjectFromPtr = oThis
    CopyMemory oThis, 0&, 4
End Property

Friend Sub FireTimer()
Dim i As Long
Dim lTick As Long
Dim lAmount As Long
Dim iTmr As ITimer
   
   If m_iCount > 0 Then
      If m_lPtr <> 0 Then
         ' Using implements
         Set iTmr = ObjectFromPtr(m_lPtr)
         lTick = timeGetTime()
         For i = 1 To m_iCount
            If m_tT(i).bEnabled Then
               lAmount = (lTick - m_tT(i).lLastTick)
               If lAmount >= m_tT(i).lInterval Or lAmount < 0 Then
                  m_tT(i).lLastTick = lTick
                  iTmr.Timer m_tT(i).sKey
               End If
            End If
         Next i
      Else
         ' using events
         lTick = timeGetTime()
         For i = 1 To m_iCount
            If m_tT(i).bEnabled Then
               lAmount = (lTick - m_tT(i).lLastTick)
               If lAmount >= m_tT(i).lInterval Or lAmount < 0 Then
                  m_tT(i).lLastTick = lTick
                  RaiseEvent Timer(m_tT(i).sKey)
               End If
            End If
         Next i
      End If
   End If
End Sub

Public Property Get Count() As Long
   Count = m_iCount
End Property

Public Function Add(ByVal sKey As String, Optional ByVal lInterval As Long = 10, Optional ByVal bEnabled As Boolean = False) As Long
   If Not (Exists(sKey)) Then
      m_iCount = m_iCount + 1
      ReDim Preserve m_tT(1 To m_iCount) As tTimerData
      With m_tT(m_iCount)
         .sKey = sKey
         .lInterval = lInterval
         .bEnabled = bEnabled
      End With
      If m_iCount = 1 Then
         StartTimer
         AddObject Me
      End If
   End If
End Function

Public Sub Remove(ByVal vKey As Variant)
Dim lIndex As Long
Dim i As Long
   lIndex = Index(vKey)
   If (lIndex > 0) Then
      If (m_iCount > 1) Then
         For i = lIndex To m_iCount - 1
            LSet m_tT(i) = m_tT(i + 1)
         Next i
         m_iCount = m_iCount - 1
         ReDim Preserve m_tT(1 To m_iCount) As tTimerData
      Else
         Erase m_tT
         m_iCount = 0
         StopTimer
         RemoveObject Me
      End If
   End If
End Sub
Public Property Get Exists(ByVal sKey As String) As Boolean
Dim i As Long
   For i = 1 To m_iCount
      If (m_tT(i).sKey = sKey) Then
         Exists = True
         Exit For
      End If
   Next i
End Property
Public Property Get Interval(ByVal vKey As Variant) As Long
Dim lIndex As Long
   lIndex = Index(vKey)
   If (lIndex > 0) Then
      Interval = m_tT(lIndex).lInterval
   End If
End Property
Public Property Let Interval(ByVal vKey As Variant, ByVal lInterval As Long)
Dim lIndex As Long
   lIndex = Index(vKey)
   If (lIndex > 0) Then
      m_tT(lIndex).lInterval = lInterval
   End If
End Property
Public Property Get Enabled(ByVal vKey As Variant) As Boolean
Dim lIndex As Long
   lIndex = Index(vKey)
   If (lIndex > 0) Then
      Enabled = m_tT(lIndex).bEnabled
   End If
End Property
Public Property Let Enabled(ByVal vKey As Variant, ByVal bEnabled As Boolean)
Dim lIndex As Long
   lIndex = Index(vKey)
   If (lIndex > 0) Then
      m_tT(lIndex).bEnabled = bEnabled
      If (bEnabled) Then
         m_tT(lIndex).lLastTick = timeGetTime()
      End If
   End If
End Property
Public Property Get Index(ByVal vKey As Variant) As Long
Dim i As Long
   If IsNumeric(vKey) Then
      If vKey > 0 And vKey <= m_iCount Then
         Index = vKey
         Exit Property
      End If
   Else
      For i = 1 To m_iCount
         If m_tT(i).sKey = vKey Then
            Index = i
            Exit Property
         End If
      Next i
   End If
   Err.Raise 9, App.EXEName & ".cHiResTimer"
End Property

Private Sub Class_Terminate()
   StopTimer
   RemoveObject Me
End Sub
