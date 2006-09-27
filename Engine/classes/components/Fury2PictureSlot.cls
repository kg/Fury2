VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2PictureSlot"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Private m_engEngine As Fury2Engine
Private m_lngIndex As Long

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Property Get Index() As Long
    Index = m_lngIndex
End Property

Friend Sub SetEngine(ByVal Engine As Fury2Engine)
    Set m_engEngine = Engine
End Sub

Friend Sub SetIndex(ByVal Index As Long)
    m_lngIndex = Index
End Sub

Public Function Show(ByVal Pic As Object, Optional ByVal X As Long = 0, Optional ByVal Y As Long = 0, Optional ByVal Width As Long = -1, Optional ByVal Height As Long = -1, Optional ByVal x2 As Long = 0, Optional ByVal y2 As Long = 0, Optional ByVal EffectMode As SFXBlitModes = BlitMode_Default, Optional ByVal Alpha As Single = 1, Optional ByVal EffectColor As Long = 0, Optional ByVal Tile As Boolean = False) As Fury2PictureSlot
    m_engEngine.ShowPicture m_lngIndex, Pic, X, Y, Width, Height, x2, y2, EffectMode, Alpha, EffectColor, Tile
    Set Show = Me
End Function

Public Sub Hide()
    m_engEngine.HidePicture m_lngIndex
End Sub

Public Sub Play(Optional ByVal OnFinish As Fury2Event = Nothing)
On Error Resume Next
    m_engEngine.GetPictureGraphic(m_lngIndex).Play OnFinish
End Sub

Public Sub Free()
On Error Resume Next
    m_engEngine.PictureSlotDestroyed m_lngIndex
    Set m_engEngine = Nothing
    m_lngIndex = 0
End Sub

Public Property Get X() As Long
On Error Resume Next
    X = m_engEngine.GetPictureX(m_lngIndex)
End Property

Public Property Get Y() As Long
On Error Resume Next
    Y = m_engEngine.GetPictureY(m_lngIndex)
End Property

