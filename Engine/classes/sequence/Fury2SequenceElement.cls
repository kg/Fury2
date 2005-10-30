VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SequenceElement"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Object As Object
Public Expression As String
Public ID As String
Private m_seqSequence As Fury2Sequence

Friend Sub SetSequence(Sequence As Fury2Sequence)
On Error Resume Next
    Set m_seqSequence = Sequence
End Sub

Public Property Get Sequence() As Fury2Sequence
On Error Resume Next
    Set Sequence = m_seqSequence
End Property

Public Sub Initialize()
On Error Resume Next
    Set Object = Nothing
    Set Object = m_seqSequence.Engine.Evaluate(Expression)
End Sub

Public Sub Reinitialize()
On Error Resume Next
    If Object Is Nothing Then
        Set Object = m_seqSequence.Engine.Evaluate(Expression)
    End If
End Sub
