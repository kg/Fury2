Attribute VB_Name = "mdlGlobal"
Option Explicit
Public Engine As Fury2Engine
Public NullCapture As NullWidget
Public GraphicCount As Long
Public ButtonCount As Long
Public TextFieldCount As Long
Public WindowCount As Long
Public ContainerCount As Long
Public ListBoxCount As Long
Public MenuCount As Long
Public LabelCount As Long
Public ScrollBarCount As Long
Public Globals As UIKit.Global
Public IndentLevel As Long

Public Sub Push(Name As String)
    Debug.Print String(IndentLevel, Asc(" ")) & Name
    IndentLevel = IndentLevel + 2
End Sub

Public Sub Pop()
    IndentLevel = IndentLevel - 2
End Sub
