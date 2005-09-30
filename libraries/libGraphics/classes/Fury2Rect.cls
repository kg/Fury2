VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Rect"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements IInspectable
Implements IInspectorType
Implements IVirtualFileSaveable
Private mvarRect As SoftFX.Rectangle

Public Sub Accomodate(NewRect)
On Error Resume Next
Dim l_rctRect As Fury2Rect
Dim l_rcRect As SoftFX.Rectangle
    Set l_rctRect = NewRect
    If l_rctRect Is Nothing Then Exit Sub
    l_rcRect = l_rctRect.GetRectangle
    With mvarRect
        If (l_rcRect.Left < .Left) Then
            .Width = .Width + (.Left - l_rcRect.Left)
            .Left = l_rcRect.Left
        End If
        If (l_rcRect.Top < .Top) Then
            .Height = .Height + (.Top - l_rcRect.Top)
            .Top = l_rcRect.Top
        End If
        If (l_rcRect.Left + l_rcRect.Width) > (.Left + .Width) Then
            .Width = .Width + ((l_rcRect.Left + l_rcRect.Width) - (.Left + .Width))
        End If
        If (l_rcRect.Top + l_rcRect.Height) > (.Top + .Height) Then
            .Height = .Height + ((l_rcRect.Top + l_rcRect.Height) - (.Top + .Height))
        End If
    End With
End Sub

Public Sub SetRectangle(Rect As SoftFX.Rectangle)
On Error Resume Next
    mvarRect = Rect
End Sub

Public Function GetRectangle() As SoftFX.Rectangle
On Error Resume Next
    GetRectangle = mvarRect
End Function

Public Sub GetValues(ByRef x1 As Long, ByRef y1 As Long, ByRef x2 As Long, ByRef y2 As Long)
On Error Resume Next
    With mvarRect
        x1 = .Left
        y1 = .Top
        x2 = .Left + .Width
        y2 = .Top + .Height
    End With
End Sub

Public Function SetValues(ByVal x1 As Long, ByVal y1 As Long, ByVal x2 As Long, ByVal y2 As Long) As Fury2Rect
On Error Resume Next
    Err.Clear
    With mvarRect
        .Left = x1
        .Top = y1
        .Width = x2 - x1
        .Height = y2 - y1
    End With
    Set SetValues = Me
End Function

Public Property Get Left() As Long
    Left = mvarRect.Left
End Property

Public Property Let Left(ByVal NewLeft As Long)
    mvarRect.Width = mvarRect.Width + (mvarRect.Left - NewLeft)
    mvarRect.Left = NewLeft
End Property

Public Property Get Top() As Long
    Top = mvarRect.Top
End Property

Public Property Let Top(ByVal NewTop As Long)
    mvarRect.Height = mvarRect.Height + (mvarRect.Top - NewTop)
    mvarRect.Top = NewTop
End Property

Public Property Get RelLeft() As Long
    RelLeft = mvarRect.Left
End Property

Public Property Let RelLeft(ByVal NewLeft As Long)
    mvarRect.Left = CLng(NewLeft)
End Property

Public Property Get RelTop() As Long
    RelTop = mvarRect.Top
End Property

Public Property Let RelTop(ByVal NewTop As Long)
    mvarRect.Top = CLng(NewTop)
End Property

Public Property Get Right() As Long
    Right = mvarRect.Width + mvarRect.Left
End Property

Public Property Let Right(ByVal NewRight As Long)
    mvarRect.Width = NewRight - mvarRect.Left
End Property

Public Property Get Bottom() As Long
    Bottom = mvarRect.Height + mvarRect.Top
End Property

Public Property Let Bottom(ByVal NewBottom As Long)
    mvarRect.Height = NewBottom - mvarRect.Top
End Property

Public Property Get Width() As Long
    Width = mvarRect.Width
End Property

Public Property Let Width(ByVal NewWidth As Long)
    mvarRect.Width = NewWidth
End Property

Public Property Get Height() As Long
    Height = mvarRect.Height
End Property

Public Property Let Height(ByVal NewHeight As Long)
    mvarRect.Height = NewHeight
End Property

Public Property Get XCenter() As Long
    XCenter = mvarRect.Left + (mvarRect.Width \ 2)
End Property

Public Property Get YCenter() As Long
    YCenter = mvarRect.Top + (mvarRect.Height \ 2)
End Property

Public Property Get GetRect() As Win32.Rect
    With GetRect
        .Left = mvarRect.Left
        .Top = mvarRect.Top
        .Right = mvarRect.Left + mvarRect.Width
        .Bottom = mvarRect.Top + mvarRect.Height
    End With
End Property

Public Function Copy() As Fury2Rect
On Error Resume Next
    Set Copy = New Fury2Rect
    With Copy
        .SetRectangle mvarRect
    End With
End Function

Public Function Translate(ByVal X As Long, ByVal Y As Long) As Fury2Rect
On Error Resume Next
    mvarRect.Left = mvarRect.Left + X
    mvarRect.Top = mvarRect.Top + Y
    Set Translate = Me
End Function

Public Function PointInside(ByVal X As Long, ByVal Y As Long) As Boolean
    If X < Left Then Exit Function
    If Y < Top Then Exit Function
    If X >= Right Then Exit Function
    If Y >= Bottom Then Exit Function
    PointInside = True
End Function

Public Function Intersect(OtherRect) As Boolean
On Error Resume Next
Dim l_rctRect As Fury2Rect
    Set l_rctRect = OtherRect
    If l_rctRect Is Nothing Then Exit Function
    With l_rctRect
        If Left > .Right Then Exit Function
        If Top > .Bottom Then Exit Function
        If Right < .Left Then Exit Function
        If Bottom < .Top Then Exit Function
        Intersect = True
    End With
End Function

Public Function Interpolate(NewRect, ByVal Progress As Single) As Fury2Rect
On Error Resume Next
Dim l_rctRect As Fury2Rect
    Set l_rctRect = NewRect
    If l_rctRect Is Nothing Then Exit Function
    With l_rctRect
        Left = Left + ((.Left - Left) * Progress)
        Top = Top + ((.Top - Top) * Progress)
        Right = Right + ((.Right - Right) * Progress)
        Bottom = Bottom + ((.Bottom - Bottom) * Progress)
    End With
End Function

Public Function AdjustScale(ByVal X As Single, ByVal Y As Single) As Fury2Rect
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long, l_lngW As Long, l_lngH As Long
    l_lngX = XCenter
    l_lngY = YCenter
    l_lngW = Width
    l_lngH = Height
    mvarRect.Left = l_lngX - Ceil(l_lngW * X / 2)
    mvarRect.Width = Ceil(l_lngW * X)
    mvarRect.Top = l_lngY - Ceil(l_lngH * Y / 2)
    mvarRect.Height = Ceil(l_lngH * Y)
    Set AdjustScale = Me
End Function

Public Function Adjust(ByVal X As Long, ByVal Y As Long) As Fury2Rect
On Error Resume Next
    mvarRect.Left = mvarRect.Left - X
    mvarRect.Width = mvarRect.Width + (X * 2)
    mvarRect.Top = mvarRect.Top - Y
    mvarRect.Height = mvarRect.Height + (Y * 2)
    Set Adjust = Me
End Function

Public Function Rationalize() As Fury2Rect
On Error Resume Next
    SoftFX.NormalizeRectangle mvarRect
    Set Rationalize = Me
End Function

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Rectangle: " & Left & ", " & Top & " - " & Right & ", " & Bottom
End Function

Public Sub FromString(ByVal Data As String)
    IInspectorType_FromString Data
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    g_lngRectCount = g_lngRectCount + 1
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    g_lngRectCount = g_lngRectCount - 1
End Sub

Private Sub IInspectorType_FromString(ByVal Data As String)
On Error Resume Next
Dim l_strStart As String, l_strEnd As String
Dim l_lngPos As Long
Dim l_varParts As Variant
    l_lngPos = InStr(Data, "-")
    If l_lngPos Then
        l_strStart = Left(Data, l_lngPos - 1)
        l_strEnd = Mid(Data, l_lngPos + 1)
        If StripParens(l_strStart) And StripParens(l_strEnd) Then
            l_varParts = Split(l_strStart, ",")
            If UBound(l_varParts) = 1 Then
                mvarRect.Left = CLng(l_varParts(0))
                mvarRect.Top = CLng(l_varParts(1))
            End If
            l_varParts = Split(l_strEnd, ",")
            If UBound(l_varParts) = 1 Then
                mvarRect.Width = CLng(l_varParts(0)) - mvarRect.Left
                mvarRect.Height = CLng(l_varParts(1)) - mvarRect.Top
            End If
        End If
    End If
End Sub

Private Function IInspectorType_ToString() As Variant
On Error Resume Next
    IInspectorType_ToString = "(" & Left & ", " & Top & ") - (" & Right & ", " & Bottom & ")"
End Function

Private Sub IVirtualFileSaveable_Deserialize(File As VirtualFile)
On Error Resume Next
Dim l_lngX As Long, l_lngY As Long, l_lngX2 As Long, l_lngY2 As Long
    With File
        .ReadSegment_Begin
            .Load l_lngX
            .Load l_lngY
            .Load l_lngX2
            .Load l_lngY2
            SetValues l_lngX, l_lngY, l_lngX2, l_lngY2
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
            .Save Left
            .Save Top
            .Save Right
            .Save Bottom
        .WriteSegment_End
    End With
End Sub
