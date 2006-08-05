VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SpritePainterOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IInspectable

Private m_booTemplate As Boolean
Private m_strSprites As String
Private m_objSprites As Fury2Sprites
Private m_sngDrawRate As Single
Private m_sngDrawRadius As Single

Public Property Get DrawRate() As Single
    DrawRate = m_sngDrawRate
End Property

Public Property Let DrawRate(ByVal NewRate As Single)
    m_sngDrawRate = NewRate
End Property

Public Property Get DrawRadius() As Single
    DrawRadius = m_sngDrawRadius
End Property

Public Property Let DrawRadius(ByVal NewRadius As Single)
    m_sngDrawRadius = NewRadius
End Property

Public Property Get SpritesFilename() As String
    SpritesFilename = m_strSprites
End Property

Public Property Let SpritesFilename(ByVal NewName As String)
Attribute SpritesFilename.VB_Description = "*Name"
Attribute SpritesFilename.VB_MemberFlags = "40"
    m_strSprites = NewName
End Property

Public Property Set SpriteList(ByVal NewSprites As Fury2Sprites)
Attribute SpriteList.VB_Description = "*"
    Set m_objSprites = NewSprites
End Property

Public Property Get SpriteList() As Fury2Sprites
    Set SpriteList = m_objSprites
End Property

Public Property Get UseTemplate() As Boolean
    UseTemplate = m_booTemplate
End Property

Public Property Let UseTemplate(ByVal NewValue As Boolean)
    m_booTemplate = NewValue
End Property

' Tricky tricky tricky
Public Property Get Sprites() As String
Attribute Sprites.VB_Description = "~"
    Sprites = m_strSprites
End Property

Private Sub Class_Initialize()
    m_sngDrawRate = 20
    m_sngDrawRadius = 5
End Sub
