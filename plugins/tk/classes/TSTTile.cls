VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TSTTile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

'
'   TSX Tile Class
'   Copyright 2002, Kevin Gadd
'

Private m_lngPixels() As Long

Sub Load(File As VirtualFile, Width As Long, Height As Long)
Dim m_lngX As Long, m_lngY As Long
    ReDim m_lngPixels(0 To Width - 1, 0 To Height - 1)
    For m_lngX = 0 To Width - 1
        For m_lngY = 0 To Height - 1
            File.RawLoad VarPtr(m_lngPixels(m_lngX, m_lngY)) + 2, 1
            File.RawLoad VarPtr(m_lngPixels(m_lngX, m_lngY)) + 1, 1
            File.RawLoad VarPtr(m_lngPixels(m_lngX, m_lngY)), 1
        Next m_lngY
    Next m_lngX
End Sub

Sub Save(File As VirtualFile)
Dim m_lngX As Long, m_lngY As Long
    For m_lngX = 0 To UBound(m_lngPixels, 1)
        For m_lngY = 0 To UBound(m_lngPixels, 1)
            File.RawSave VarPtr(m_lngPixels(m_lngX, m_lngY)) + 2, 1
            File.RawSave VarPtr(m_lngPixels(m_lngX, m_lngY)) + 1, 1
            File.RawSave VarPtr(m_lngPixels(m_lngX, m_lngY)), 1
        Next m_lngY
    Next m_lngX
End Sub

Public Property Get Pixels() As Long()
On Error Resume Next
    Pixels = m_lngPixels()
End Property

Public Property Let Pixels(NewPixels() As Long)
On Error Resume Next
    m_lngPixels() = NewPixels()
End Property

Public Property Get Image() As Fury2Image
On Error Resume Next
    Set Image = F2Image(UBound(m_lngPixels, 1) + 1, UBound(m_lngPixels, 2) + 1)
    CopyMemory ByVal Image.Pointer(0, 0), ByVal VarPtr(m_lngPixels(0, 0)), 4 * Image.Width * Image.Height
End Property
