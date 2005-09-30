VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2ParticleGenerator"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_genGenerator As ParticleGenerator
Private m_sprAttached As Fury2Sprite
Private m_sngAttachX As Single, m_sngAttachY As Single, m_sngAttachH As Single, m_sngAttachV As Single

Private Sub Class_Initialize()
On Error Resume Next
    m_genGenerator.Type = 0
    m_genGenerator.GenerateRate = 1
    m_genGenerator.NewA = 1
    m_genGenerator.NewL = 1
    m_genGenerator.Life = -1
End Sub

Friend Sub Update()
On Error Resume Next
    m_genGenerator.NewX = m_sprAttached.X + m_sngAttachX
    m_genGenerator.NewY = m_sprAttached.Y + m_sngAttachY
End Sub

Public Sub Attach(ByRef Sprite, Optional ByVal XOffset As Single = 0, Optional ByVal YOffset As Single = 0)
On Error Resume Next
    Set m_sprAttached = Sprite
    m_sngAttachX = XOffset
    m_sngAttachY = YOffset
End Sub

Public Property Let AttachX(ByVal vData As Single)
    m_sngAttachX = vData
End Property

Public Property Get AttachX() As Single
    AttachX = m_sngAttachX
End Property

Public Property Let AttachY(ByVal vData As Single)
    m_sngAttachY = vData
End Property

Public Property Get AttachY() As Single
    AttachY = m_sngAttachY
End Property

Friend Sub AddTo(ByVal Engine As Long)
On Error Resume Next
    AddParticleGenerator Engine, m_genGenerator
End Sub

Friend Sub RemoveFrom(ByVal Engine As Long)
On Error Resume Next
    RemoveParticleGenerator Engine, m_genGenerator
End Sub

Public Property Get Handle() As Long
On Error Resume Next
    Handle = VarPtr(m_genGenerator)
End Property

Public Property Get GenerateDelay() As Long
On Error Resume Next
    GenerateDelay = m_genGenerator.GenerateDelay
End Property

Public Property Let GenerateDelay(ByVal NewValue As Long)
On Error Resume Next
    m_genGenerator.GenerateDelay = NewValue
End Property

Public Property Get GenerateRate() As Long
On Error Resume Next
    GenerateRate = m_genGenerator.GenerateRate
End Property

Public Property Let GenerateRate(ByVal NewValue As Long)
On Error Resume Next
    m_genGenerator.GenerateRate = NewValue
End Property

Public Property Get CurrentRotation() As Single
On Error Resume Next
    CurrentRotation = m_genGenerator.CurrentRotation
End Property

Public Property Let CurrentRotation(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.CurrentRotation = NewValue
End Property

Public Property Get GenerateRotation() As Single
On Error Resume Next
    GenerateRotation = m_genGenerator.GenerateRotation
End Property

Public Property Let GenerateRotation(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.GenerateRotation = NewValue
End Property

Public Property Get Life() As Long
On Error Resume Next
    Life = m_genGenerator.Life
End Property

Public Property Let Life(ByVal NewValue As Long)
On Error Resume Next
    m_genGenerator.Life = NewValue
End Property

Public Property Get RandomL() As Single
On Error Resume Next
    RandomL = m_genGenerator.RandomL
End Property

Public Property Let RandomL(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomL = NewValue
End Property

Public Property Get RandomA() As Single
On Error Resume Next
    RandomA = m_genGenerator.RandomA
End Property

Public Property Let RandomA(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomA = NewValue
End Property

Public Property Get RandomX() As Single
On Error Resume Next
    RandomX = m_genGenerator.RandomX
End Property

Public Property Let RandomX(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomX = NewValue
End Property

Public Property Get RandomY() As Single
On Error Resume Next
    RandomY = m_genGenerator.RandomY
End Property

Public Property Let RandomY(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomY = NewValue
End Property

Public Property Get NewL() As Single
On Error Resume Next
    NewL = m_genGenerator.NewL
End Property

Public Property Let NewL(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewL = NewValue
End Property

Public Property Get NewA() As Single
On Error Resume Next
    NewA = m_genGenerator.NewA
End Property

Public Property Let NewA(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewA = NewValue
End Property

Public Property Get NewX() As Single
On Error Resume Next
    NewX = m_genGenerator.NewX
End Property

Public Property Let NewX(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewX = NewValue
End Property

Public Property Get NewY() As Single
On Error Resume Next
    NewY = m_genGenerator.NewY
End Property

Public Property Let NewY(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewY = NewValue
End Property

Public Property Get NewR() As Single
On Error Resume Next
    NewR = m_genGenerator.NewR
End Property

Public Property Let NewR(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewR = NewValue
End Property

Public Property Get NewRV() As Single
On Error Resume Next
    NewRV = m_genGenerator.NewRV
End Property

Public Property Let NewRV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewRV = NewValue
End Property

Public Property Get RandomR() As Single
On Error Resume Next
    RandomR = m_genGenerator.RandomR
End Property

Public Property Let RandomR(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomR = NewValue
End Property

Public Property Get RandomRV() As Single
On Error Resume Next
    RandomRV = m_genGenerator.RandomRV
End Property

Public Property Let RandomRV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomRV = NewValue
End Property

Public Property Get RandomLV() As Single
On Error Resume Next
    RandomLV = m_genGenerator.RandomLV
End Property

Public Property Let RandomLV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomLV = NewValue
End Property

Public Property Get RandomAV() As Single
On Error Resume Next
    RandomAV = m_genGenerator.RandomAV
End Property

Public Property Let RandomAV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomAV = NewValue
End Property

Public Property Get RandomXV() As Single
On Error Resume Next
    RandomXV = m_genGenerator.RandomXV
End Property

Public Property Let RandomXV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomXV = NewValue
End Property

Public Property Get RandomYV() As Single
On Error Resume Next
    RandomYV = m_genGenerator.RandomYV
End Property

Public Property Let RandomYV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.RandomYV = NewValue
End Property

Public Property Get NewLV() As Single
On Error Resume Next
    NewLV = m_genGenerator.NewLV
End Property

Public Property Let NewLV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewLV = NewValue
End Property

Public Property Get NewAV() As Single
On Error Resume Next
    NewAV = m_genGenerator.NewAV
End Property

Public Property Let NewAV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewAV = NewValue
End Property

Public Property Get NewXV() As Single
On Error Resume Next
    NewXV = m_genGenerator.NewXV
End Property

Public Property Let NewXV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewXV = NewValue
End Property

Public Property Get NewYV() As Single
On Error Resume Next
    NewYV = m_genGenerator.NewYV
End Property

Public Property Let NewYV(ByVal NewValue As Single)
On Error Resume Next
    m_genGenerator.NewYV = NewValue
End Property

Public Property Get ParticleType() As Long
On Error Resume Next
    ParticleType = m_genGenerator.Type
End Property

Public Property Let ParticleType(ByVal NewValue As Long)
On Error Resume Next
    m_genGenerator.Type = NewValue
End Property


