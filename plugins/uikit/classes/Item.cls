VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Item"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Text As String
Public Image As Fury2Image
Public Font As Fury2Font
Public TextColor As Long

Public Property Get Width() As Long
    If Image Is Nothing Then
        Width = Font.TextWidth(Text)
    Else
        Width = Image.Width + Font.TextWidth(Text)
    End If
End Property

Public Property Get Height() As Long
    If Image Is Nothing Then
        Height = Font.FullHeight
    Else
        If (Image.Height > Font.FullHeight) Then
            Height = Image.Height
        Else
            Height = Font.FullHeight
        End If
    End If
End Property

Public Sub Draw(Surface As Fury2Image, Rectangle As Fury2Rect, Selected As Boolean, Opacity As Single)
On Error Resume Next
Dim l_rctText As Fury2Rect
    If Image Is Nothing Then
        Set l_rctText = Rectangle
    Else
        If Image.AlphaChannel Then
            Surface.Blit Rectangle, , Image, Opacity, BlitMode_SourceAlpha
        Else
            Surface.Blit Rectangle, , Image, Opacity, BlitMode_Matte
        End If
        Set l_rctText = F2Rect(Rectangle.Left + Image.Width + 1, Rectangle.Top, Rectangle.Width, Rectangle.Height, False)
    End If
    Font.Draw Surface, Text, l_rctText, TextColor, Opacity
End Sub
