VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2GETypes"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Public Type F2GraphicHeaderv1dot00 ' 10 bytes
    Version As Byte ' always set to 100 (version 1.00)
    CompressionMode As Byte ' always set to 0 (no compression)
    Width As Integer ' pixels
    Height As Integer ' pixels
    Pitch As Byte ' what was i thinking?
    ExpansionSpace1 As Integer
    ExpansionSpace2 As Byte
End Type

Public Type F2GraphicHeaderv1dot01 ' 10 bytes
    Version As Byte ' always set to 101 (version 1.01)
    CompressionMode As Byte ' always set to 0 (no compression)
    Width As Integer ' pixels
    Height As Integer ' pixels
    MaskColor As Long ' 32-bit BGRA
End Type

Public Type F2Filter
    RedMinimum As Long
    RedMinimumOut As Long
    GreenMinimum As Long
    GreenMinimumOut As Long
    BlueMinimum As Long
    BlueMinimumOut As Long
    RedMaximum As Long
    RedMaximumOut As Long
    GreenMaximum As Long
    GreenMaximumOut As Long
    BlueMaximum As Long
    BlueMaximumOut As Long
    RedAdjust As Long
    GreenAdjust As Long
    BlueAdjust As Long
End Type

Public Type Fury2Particle
    x As Single
    y As Single
    XVelocity As Single
    YVelocity As Single
    Color As Long
    Life As Long
    Reserved As Single
End Type

Public Type RFN_Header
    Signature(0 To 3) As Byte
    Version As Integer
    NumberOfChars As Integer
    Reserved(0 To 247) As Byte
End Type

Public Type RFN_CharHeader
    Width As Integer
    Height As Integer
End Type

Public Type RFN_CharFile
    Header As RFN_Header
    Chars() As RFN_CharHeader
End Type

