VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Channel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_seEngine As SoundEngine
Private m_lngHandle As Long

Public Property Get Parent() As SoundEngine
On Error Resume Next
    Set Parent = m_seEngine
End Property

Public Property Get Handle() As Long
On Error Resume Next
    Handle = m_lngHandle
End Property

Friend Sub Init(ByVal Parent As SoundEngine, ByVal Handle As Long)
On Error Resume Next
    Set m_seEngine = Parent
    m_lngHandle = Handle
End Sub

Friend Sub Free()
On Error Resume Next
    Set m_seEngine = Nothing
    m_lngHandle = 0
End Sub

