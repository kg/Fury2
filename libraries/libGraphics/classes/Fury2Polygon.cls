VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Polygon"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements IInspectable
Implements IInspectableCollection
Implements IInspectorType
Implements IVirtualFileSaveable
Private m_plyPolygon As GenericPolygon
Private m_fptPoints() As FPoint

Public Property Get Count() As Long
On Error Resume Next
Dim l_lngCount As Long
    Err.Clear
    l_lngCount = UBound(m_fptPoints) - LBound(m_fptPoints) + 1
    If (Err <> 0) Then
        l_lngCount = 0
    End If
    Err.Clear
    Count = l_lngCount
End Property

Public Property Let Count(ByVal NewCount As Long)
On Error Resume Next
    If (NewCount > 0) Then
        ReDim Preserve m_fptPoints(0 To NewCount - 1)
        m_plyPolygon.VertexPointer = VarPtr(m_fptPoints(0))
    Else
        Erase m_fptPoints
        m_plyPolygon.VertexPointer = 0
    End If
    m_plyPolygon.VertexCount = Count
    m_plyPolygon.InactiveVertexCount = 0
End Property

Public Sub Clear()
On Error Resume Next
    Count = -1
End Sub

Public Sub Add(ByVal NewPoint As Variant)
On Error Resume Next
    Count = Count + 1
    Me.Point(Count - 1) = NewPoint
End Sub

Public Sub AddFPoint(ByRef NewPoint As FPoint)
On Error Resume Next
    Count = Count + 1
    Me.FPoint(Count - 1) = NewPoint
End Sub

Public Property Get Handle() As Long
Attribute Handle.VB_Description = "*"
On Error Resume Next
    If (Count > 0) Then
        Handle = VarPtr(m_plyPolygon)
    Else
        Handle = 0
    End If
End Property

Public Property Get FPoints() As FPoint()
Attribute FPoints.VB_Description = "*"
On Error Resume Next
    FPoints = m_fptPoints
End Property

Public Property Let FPoints(ByRef NewValue() As FPoint)
On Error Resume Next
    m_fptPoints = NewValue
    m_plyPolygon.VertexPointer = VarPtr(m_fptPoints(0))
    m_plyPolygon.VertexCount = Count
    m_plyPolygon.InactiveVertexCount = 0
End Property

Public Property Get FPoint(ByVal Index As Long) As FPoint
On Error Resume Next
    FPoint = m_fptPoints(Index)
End Property

Public Property Let FPoint(ByVal Index As Long, ByRef NewPoint As FPoint)
On Error Resume Next
    m_fptPoints(Index) = NewPoint
End Property

Public Property Get Point(ByVal Index As Long) As Variant
On Error Resume Next
    Point = Array(m_fptPoints(Index).X, m_fptPoints(Index).Y)
End Property

Public Property Let Point(ByVal Index As Long, ByVal NewPoint As Variant)
On Error Resume Next
    If (VarType(NewPoint) And vbArray) = vbArray Then
        m_fptPoints(Index).X = NewPoint(0)
        m_fptPoints(Index).Y = NewPoint(1)
    ElseIf (VarType(NewPoint) And vbString) = vbString Then
        Dim l_strParts() As String
        l_strParts = Split(NewPoint, ",")
        m_fptPoints(Index).X = CSng(l_strParts(0))
        m_fptPoints(Index).Y = CSng(l_strParts(1))
    End If
End Property

Public Function Translate(ByVal X As Single, ByVal Y As Single) As Fury2Polygon
On Error Resume Next
Dim l_lngIndex As Long
    For l_lngIndex = LBound(m_fptPoints) To UBound(m_fptPoints)
        m_fptPoints(l_lngIndex).X = m_fptPoints(l_lngIndex).X + X
        m_fptPoints(l_lngIndex).Y = m_fptPoints(l_lngIndex).Y + Y
    Next l_lngIndex
    Set Translate = Me
End Function

Public Function Multiply(ByVal X As Single, ByVal Y As Single) As Fury2Polygon
On Error Resume Next
Dim l_lngIndex As Long
    For l_lngIndex = LBound(m_fptPoints) To UBound(m_fptPoints)
        m_fptPoints(l_lngIndex).X = m_fptPoints(l_lngIndex).X * X
        m_fptPoints(l_lngIndex).Y = m_fptPoints(l_lngIndex).Y * Y
    Next l_lngIndex
    Set Multiply = Me
End Function

Public Sub Draw(ByVal Surface As Fury2Image, ByVal X As Single, ByVal Y As Single, ByVal Color As Long, Optional ByVal Mode As SFXRenderModes = RenderMode_Default, Optional ByVal AntiAlias As Boolean = False)
On Error Resume Next
Dim l_fptPoints() As FPoint
Dim l_lngIndex As Long
    If Surface Is Nothing Then Exit Sub
    If Count < 1 Then Exit Sub
    If Mode = RenderMode_Default Then Mode = Surface.RenderMode
    If ((X <> 0) Or (Y <> 0)) Then
        l_fptPoints = m_fptPoints
        For l_lngIndex = LBound(l_fptPoints) To UBound(l_fptPoints)
            l_fptPoints(l_lngIndex).X = l_fptPoints(l_lngIndex).X + X
            l_fptPoints(l_lngIndex).Y = l_fptPoints(l_lngIndex).Y + Y
        Next l_lngIndex
        m_plyPolygon.VertexPointer = VarPtr(l_fptPoints(0))
        If AntiAlias Then
            Primitive_ConvexPolygon_AntiAlias Surface.Handle, m_plyPolygon, Color, Surface.GetRenderer(Mode), 0
        Else
            Primitive_ConvexPolygon Surface.Handle, m_plyPolygon, Color, Surface.GetRenderer(Mode), 0
        End If
        m_plyPolygon.VertexPointer = VarPtr(m_fptPoints(0))
    Else
        If AntiAlias Then
            Primitive_ConvexPolygon_AntiAlias Surface.Handle, m_plyPolygon, Color, Surface.GetRenderer(Mode), 0
        Else
            Primitive_ConvexPolygon Surface.Handle, m_plyPolygon, Color, Surface.GetRenderer(Mode), 0
        End If
    End If
End Sub

Public Sub DrawOutline(ByVal Surface As Fury2Image, ByVal X As Single, ByVal Y As Single, ByVal Color As Long, Optional ByVal AntiAlias As Boolean = False)
On Error Resume Next
Dim l_fptPoints() As FPoint
Dim l_lngIndex As Long
    If Surface Is Nothing Then Exit Sub
    If Count < 1 Then Exit Sub
    If ((X <> 0) Or (Y <> 0)) Then
        l_fptPoints = m_fptPoints
        For l_lngIndex = LBound(l_fptPoints) To UBound(l_fptPoints)
            l_fptPoints(l_lngIndex).X = l_fptPoints(l_lngIndex).X + X
            l_fptPoints(l_lngIndex).Y = l_fptPoints(l_lngIndex).Y + Y
        Next l_lngIndex
        m_plyPolygon.VertexPointer = VarPtr(l_fptPoints(0))
'        If AntiAlias Then
            Primitive_ConvexPolygon_Outline Surface.Handle, m_plyPolygon, Color
'        Else
'            Primitive_ConvexPolygon Surface.Handle, m_plyPolygon, Color, Surface.GetRenderer(Mode), 0
'        End If
        m_plyPolygon.VertexPointer = VarPtr(m_fptPoints(0))
    Else
'        If AntiAlias Then
            Primitive_ConvexPolygon_Outline Surface.Handle, m_plyPolygon, Color
'        Else
'            Primitive_ConvexPolygon Surface.Handle, m_plyPolygon, Color, Surface.GetRenderer(Mode), 0
'        End If
    End If
End Sub

Public Function PointInside(ByVal X As Single, ByVal Y As Single) As Boolean
On Error Resume Next
Dim m_fptPoint As FPoint
    m_fptPoint.X = X
    m_fptPoint.Y = Y
    PointInside = SoftFX.PointIntersectsPolygon(ByVal Handle, m_fptPoint) = 1
End Function

Public Function IntersectsPolygon(ByVal Other As Fury2Polygon) As Boolean
On Error Resume Next
    If Other Is Nothing Then Exit Function
    IntersectsPolygon = SoftFX.PolygonIntersectsPolygon(Handle, Other.Handle) = 1
End Function

Public Function Duplicate() As Fury2Polygon
On Error Resume Next
Dim l_plyPolygon As Fury2Polygon
    Set l_plyPolygon = New Fury2Polygon
    l_plyPolygon.FPoints = m_fptPoints
    Set Duplicate = l_plyPolygon
End Function

Private Sub Class_Initialize()
On Error Resume Next
    Count = -1
End Sub

Private Property Get IInspectableCollection_ItemCount() As Long
On Error Resume Next
    IInspectableCollection_ItemCount = Count
End Property

Private Property Set IInspectableCollection_ItemValue(ByVal Index As Long, ByVal RHS As Variant)
On Error Resume Next
    Point(Index - 1) = RHS
End Property

Private Property Let IInspectableCollection_ItemValue(ByVal Index As Long, ByVal RHS As Variant)
On Error Resume Next
    Point(Index - 1) = RHS
End Property

Private Property Get IInspectableCollection_ItemValue(ByVal Index As Long) As Variant
On Error Resume Next
    IInspectableCollection_ItemValue = Point(Index - 1)
End Property

Private Sub IInspectorType_FromString(ByVal Data As String)
On Error Resume Next
Dim l_strItems() As String
Dim l_strParts() As String
Dim l_lngIndex As Long
    If InStr(Data, "Polygon(") Then
        Data = Mid(Data, 9, Len(Data) - 10)
        If Len(Trim(Data)) = 0 Then
            Count = 0
            Exit Sub
        End If
        l_strItems = Split(Data, ",")
        Count = UBound(l_strItems) - LBound(l_strItems) + 1
        For l_lngIndex = 0 To Count - 1
            l_strParts = Split(Trim(l_strItems(l_lngIndex)), " ")
            m_fptPoints(l_lngIndex).X = CSng(l_strParts(0))
            m_fptPoints(l_lngIndex).Y = CSng(l_strParts(1))
        Next l_lngIndex
    Else
    End If
End Sub

Private Function IInspectorType_ToString() As Variant
On Error Resume Next
Dim l_lngIndex As Long
Dim l_ptPoint As FPoint
    IInspectorType_ToString = "Polygon("
    For l_lngIndex = 0 To Count - 1
        If (l_lngIndex > 0) Then
            IInspectorType_ToString = IInspectorType_ToString + ","
        End If
        l_ptPoint = m_fptPoints(l_lngIndex)
        IInspectorType_ToString = IInspectorType_ToString & Format(l_ptPoint.X, "######0.0##") & " " & Format(l_ptPoint.Y, "######0.0##")
    Next l_lngIndex
    IInspectorType_ToString = IInspectorType_ToString + ")"
End Function

Public Function Class_ToString() As String
On Error Resume Next
Dim l_lngIndex As Long
Dim l_ptPoint As FPoint
    Class_ToString = "Polygon("
    For l_lngIndex = 0 To Count - 1
        If (l_lngIndex > 0) Then
            Class_ToString = Class_ToString + ", "
        End If
        l_ptPoint = m_fptPoints(l_lngIndex)
        Class_ToString = Class_ToString & "(" & Format(l_ptPoint.X, "######0") & ", " & Format(l_ptPoint.Y, "######0") & ")"
    Next l_lngIndex
    Class_ToString = Class_ToString + ")"
End Function

Private Sub IVirtualFileSaveable_Deserialize(File As VirtualFile)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngIndex As Long
Dim l_fptPoint As FPoint
    With File
        .ReadSegment_Begin
            .Load l_lngCount
            Count = l_lngCount
            If (l_lngCount > 0) Then
                For l_lngIndex = 0 To l_lngCount - 1
                    .RawLoad VarPtr(l_fptPoint), 8
                    m_fptPoints(l_lngIndex) = l_fptPoint
                Next l_lngIndex
            End If
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As VirtualFile)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngIndex As Long
    With File
        .WriteSegment_Begin
            l_lngCount = Count
            .Save l_lngCount
            If (l_lngCount > 0) Then
                For l_lngIndex = 0 To l_lngCount - 1
                    .RawSave VarPtr(m_fptPoints(l_lngIndex)), 8
                Next l_lngIndex
            End If
        .WriteSegment_End
    End With
End Sub
