VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngResource"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_rcfParent As ngResourceFile
Private m_strFilename As String
Private m_rtyType As ngResourceTypes

Friend Sub Free()
On Error Resume Next
    Set m_rcfParent = Nothing
End Sub

Public Property Get Parent() As ngResourceFile
    Set Parent = m_rcfParent
End Property

Friend Sub SetParent(ByRef NewParent As ngResourceFile)
    Set m_rcfParent = NewParent
End Sub

Public Property Get Filename() As String
    Filename = m_strFilename
End Property

Friend Property Let Filename(ByRef NewValue As String)
    m_strFilename = NewValue
End Property

Public Property Get ResourceType() As ngResourceTypes
    ResourceType = m_rtyType
End Property

Friend Property Let ResourceType(ByVal NewValue As ngResourceTypes)
    m_rtyType = NewValue
End Property

Public Property Get Data() As Variant
Attribute Data.VB_UserMemId = 0
Attribute Data.VB_MemberFlags = "200"
On Error Resume Next
    Select Case m_rtyType
    Case rtyImage, rtyIcon
        Set Data = Load
    Case rtyText, rtyScript
        Data = Load
    End Select
End Property

Public Function Load() As Variant
On Error Resume Next
    Select Case m_rtyType
    Case rtyImage
        Set Load = Me.LoadImage()
    Case rtyIcon
        Set Load = Me.LoadIcon()
    Case rtyText, rtyScript
        Load = Me.LoadText()
    End Select
End Function

Public Function LoadIcon() As StdPicture
On Error Resume Next
Dim l_strFilename As String
    l_strFilename = m_rcfParent.ExtractFile(m_strFilename)
    FileCopy l_strFilename, l_strFilename & ".ico"
    Set LoadIcon = LoadPicture(l_strFilename & ".ico")
    Kill l_strFilename
    Kill l_strFilename & ".ico"
End Function

Public Function LoadImage() As Fury2Image
On Error Resume Next
Dim l_bytImage() As Byte
    l_bytImage = m_rcfParent.ReadFile(m_strFilename)
    Set LoadImage = F2ImageFromByteArray(l_bytImage)
End Function

Public Function LoadText() As String
On Error Resume Next
Dim l_strFilename As String
    l_strFilename = m_rcfParent.ExtractFile(m_strFilename)
    LoadText = ReadTextFile(l_strFilename)
    Kill l_strFilename
End Function
