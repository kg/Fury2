VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CharacterProxy"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IVirtualFileSaveable
Private m_fntFont As Fury2Font
Private m_lngCharacter As Long

Friend Property Get Font() As Fury2Font
On Error Resume Next
    Set Font = m_fntFont
End Property

Friend Property Set Font(ByVal NewFont As Fury2Font)
On Error Resume Next
    Set m_fntFont = NewFont
End Property

Friend Property Get Character() As Long
On Error Resume Next
    Character = m_lngCharacter
End Property

Friend Property Let Character(ByVal NewCharacter As Long)
On Error Resume Next
    m_lngCharacter = NewCharacter
End Property

Public Property Get Width() As Long
On Error Resume Next
    Width = m_fntFont.CharacterWidth(m_lngCharacter)
End Property

Public Property Let Width(ByVal NewWidth As Long)
On Error Resume Next
    m_fntFont.CharacterWidth(m_lngCharacter) = NewWidth
End Property

Public Property Get Height() As Long
On Error Resume Next
    Height = m_fntFont.CharacterHeight(m_lngCharacter)
End Property

Public Property Let Height(ByVal NewHeight As Long)
On Error Resume Next
    m_fntFont.CharacterHeight(m_lngCharacter) = NewHeight
End Property

Public Property Get XOffset() As Long
On Error Resume Next
    XOffset = m_fntFont.CharacterXOffset(m_lngCharacter)
End Property

Public Property Let XOffset(ByVal NewXOffset As Long)
On Error Resume Next
    m_fntFont.CharacterXOffset(m_lngCharacter) = NewXOffset
End Property

Public Property Get YOffset() As Long
On Error Resume Next
    YOffset = m_fntFont.CharacterYOffset(m_lngCharacter)
End Property

Public Property Let YOffset(ByVal NewYOffset As Long)
On Error Resume Next
    m_fntFont.CharacterYOffset(m_lngCharacter) = NewYOffset
End Property

Public Property Get Image() As Fury2Image
On Error Resume Next
    Set Image = m_fntFont.Character(m_lngCharacter)
End Property

Public Property Set Image(ByVal NewImage As Fury2Image)
On Error Resume Next
    Set m_fntFont.Character(m_lngCharacter) = NewImage
End Property

Public Property Get Value() As Long
On Error Resume Next
    Value = m_fntFont.CharacterValue(m_lngCharacter)
End Property

Public Property Let Value(ByVal NewValue As Long)
On Error Resume Next
    m_fntFont.CharacterValue(m_lngCharacter) = NewValue
End Property

Public Property Get ValueString() As String
On Error Resume Next
    ValueString = Chr(m_fntFont.CharacterValue(m_lngCharacter))
End Property

Public Property Let ValueString(ByVal NewValue As String)
On Error Resume Next
    m_fntFont.CharacterValue(m_lngCharacter) = Asc(Left(NewValue, 1))
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngValue As Long
Dim l_imgImage As Fury2Image
    With File
        Set l_imgImage = F2Image(1, 1)
        .Load l_imgImage
        Set Image = l_imgImage
        .Load l_lngValue
        Value = l_lngValue
        .Load l_lngValue
        Width = l_lngValue
        .Load l_lngValue
        Height = l_lngValue
        .Load l_lngValue
        XOffset = l_lngValue
        .Load l_lngValue
        YOffset = l_lngValue
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save Image
        .Save Value
        .Save Width
        .Save Height
        .Save XOffset
        .Save YOffset
    End With
End Sub
