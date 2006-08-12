VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SequenceTween"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements Fury2Object
Implements IVirtualFileSaveable
Implements IInspectable
Public Element As Fury2SequenceElement
Attribute Element.VB_VarDescription = "*"
Public ElementID As String
Attribute ElementID.VB_VarDescription = "The name of the element this tween applies to."
Public PropertyName As String
Attribute PropertyName.VB_VarDescription = "The name of the property this tween applies to."
Public ValueType As Fury2TweenTypes
Public TweenMode As Fury2TweenModes
Attribute TweenMode.VB_VarDescription = "The type of tweening to apply."
Public TweenParameter As Double
Attribute TweenParameter.VB_VarDescription = "The parameter for the tweening function."
Public Variable As String
Attribute Variable.VB_VarDescription = "The variable to add to the value of this tween."
Public MethodID As Long
Attribute MethodID.VB_VarDescription = "*"
Private m_varValue As Variant
Private m_vtValueType As VbVarType
Private m_dblPreviousValue As Double
Private m_dblNextValue As Double
Private m_lngPreviousValue As Long
Private m_lngNextValue As Long
Private m_skfKeyframe As WeakReference
Private m_seEngine As ScriptEngine

Public Property Get Value() As Variant
On Error Resume Next
    Select Case ValueType
    Case F2TT_Number
        Value = CDbl(m_varValue)
    Case F2TT_Integer, F2TT_Color
        Value = CLng(m_varValue)
    End Select
End Property

Public Property Let Value(ByVal NewValue As Variant)
On Error Resume Next
    Select Case ValueType
    Case F2TT_Number
        m_varValue = CDbl(NewValue)
    Case F2TT_Integer, F2TT_Color
        m_varValue = CLng(NewValue)
    End Select
End Property

Public Function Duplicate() As Fury2SequenceTween
On Error Resume Next
Dim l_twNew As Fury2SequenceTween
    Set l_twNew = New Fury2SequenceTween
    With l_twNew
        .ElementID = ElementID
        .PropertyName = PropertyName
        .ValueType = ValueType
        .Value = Value
        .TweenMode = TweenMode
        .TweenParameter = TweenParameter
    End With
    Set Duplicate = l_twNew
End Function

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Tween: " & ElementID & "." & PropertyName
End Function

Public Sub Free()
On Error Resume Next
    Set Element = Nothing
    Set m_seEngine = Nothing
    MethodID = -1
    WRFree m_skfKeyframe
End Sub

Public Sub Bind()
On Error Resume Next
    If Element Is Nothing Then
        Element.Sequence.Engine.DebugOut "Unable to bind tween to null element " & ElementID
        Exit Sub
    End If
    If Element.Object Is Nothing Then
        Element.Sequence.Engine.DebugOut "Unable to bind tween to null element object " & ElementID
        Exit Sub
    End If
    Set m_seEngine = Element.Sequence.Engine.ScriptEngine
    With m_seEngine
        MethodID = -1
        MethodID = .ResolveMember(Element.Object, PropertyName)
        If MethodID = -1 Then
            Element.Sequence.Engine.DebugOut "Unable to bind tween to property " & PropertyName & " of object " & Element.Sequence.Engine.ToString(Element.Object)
        End If
    End With
End Sub

Public Property Get PreviousTween() As Fury2SequenceTween
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
Dim l_stwTween As Fury2SequenceTween
    Set l_skfKeyframe = Keyframe.PreviousKeyframe
    If l_skfKeyframe Is Nothing Then
    Else
        For Each l_stwTween In l_skfKeyframe.Tweens
            If (l_stwTween.ElementID = Me.ElementID) And (l_stwTween.PropertyName = Me.PropertyName) Then
                Set PreviousTween = l_stwTween
                Exit For
            End If
        Next l_stwTween
    End If
End Property

Friend Sub SetKeyframe(Keyframe As Fury2SequenceKeyframe)
On Error Resume Next
    WRSet m_skfKeyframe, Keyframe
End Sub

Public Property Get Keyframe() As Fury2SequenceKeyframe
Attribute Keyframe.VB_Description = "*"
On Error Resume Next
    Set Keyframe = WRGetKeyframe(m_skfKeyframe)
End Property

Public Function GetCurrentValue() As Variant
On Error Resume Next
Dim l_varArgs() As Variant
    l_varArgs = Array()
    GetCurrentValue = m_seEngine.InvokeMember(Element.Object, MethodID, VbGet, l_varArgs)
End Function

Friend Sub FetchStartValue()
On Error Resume Next
Dim l_stwPrevious As Fury2SequenceTween
Dim l_varArgs() As Variant
Dim l_varValue As Variant
    Set l_stwPrevious = PreviousTween
    If l_stwPrevious Is Nothing Then
        l_varArgs = Array()
        l_varValue = m_seEngine.InvokeMember(Element.Object, MethodID, VbGet, l_varArgs)
        If ValueType = F2TT_Number Then
            m_lngPreviousValue = CDbl(l_varValue)
        Else
            m_lngPreviousValue = CLng(l_varValue)
        End If
    Else
        If ValueType = F2TT_Number Then
            m_lngPreviousValue = CDbl(l_stwPrevious.Value)
        Else
            m_lngPreviousValue = CLng(l_stwPrevious.Value)
        End If
    End If
    If ValueType = F2TT_Number Then
        m_lngNextValue = Value
    Else
        m_lngNextValue = Value
    End If
    If Len(Variable) > 0 Then
        If ValueType = F2TT_Number Then
            m_lngNextValue = m_lngNextValue + CDbl(Element.Sequence.Variable(Variable))
        Else
            m_lngNextValue = m_lngNextValue + CLng(Element.Sequence.Variable(Variable))
        End If
    End If
    Err.Clear
End Sub

Friend Sub FetchValueType()
On Error Resume Next
Dim l_varValue As Variant
    Err.Clear
    l_varValue = VBA.CallByName(Element.Object, PropertyName, VbGet)
    If Err <> 0 Then
        m_vtValueType = vbNull
    Else
        m_vtValueType = VarType(l_varValue)
    End If
End Sub

Public Sub Initialize()
On Error Resume Next
    m_vtValueType = vbNull
    Set Element = Nothing
    Set Element = Keyframe.Sequence.Elements.FindByProperty("ID", ElementID)
    Element.Reinitialize
End Sub

Public Sub Reinitialize()
On Error Resume Next
    If Element Is Nothing Then
        m_vtValueType = vbNull
        Set Element = Keyframe.Sequence.Elements.FindByProperty("ID", ElementID)
    End If
    Element.Reinitialize
    Bind
    FetchStartValue
End Sub

Public Sub Update(ByVal Progress As Double)
On Error Resume Next
Dim l_lngValue As Double
    Select Case TweenMode
    Case F2TM_Instant
        If Progress >= 1 Then
            Select Case ValueType
            Case F2TT_Number
                SetValue m_lngNextValue
            Case F2TT_Integer, F2TT_Color
                SetValue m_lngNextValue
            End Select
        End If
    Case F2TM_Linear
        Select Case ValueType
        Case F2TT_Number
            SetValue (m_lngPreviousValue) + ((m_lngNextValue - m_lngPreviousValue) * Progress)
        Case F2TT_Integer
            SetValue (m_lngPreviousValue) + ((m_lngNextValue - m_lngPreviousValue) * Progress)
        Case F2TT_Color
            SetValue BlendColors(m_lngPreviousValue, m_lngNextValue, Progress * 255)
        End Select
    Case Else
    End Select
End Sub

Friend Sub SetValue(ByVal Value As Double)
On Error Resume Next
Dim l_varArgs() As Variant
    If m_vtValueType = vbNull Then
        FetchValueType
    End If
    Select Case m_vtValueType
    Case vbInteger
        l_varArgs = Array(CInt(Value))
    Case vbLong
        l_varArgs = Array(CLng(Value))
    Case vbSingle
        l_varArgs = Array(CSng(Value))
    Case vbDouble
        l_varArgs = Array(CDbl(Value))
    Case vbCurrency
        l_varArgs = Array(CCur(Value))
    Case vbString
        l_varArgs = Array(CStr(Value))
    Case vbBoolean
        l_varArgs = Array(CBool(Value > 0))
    Case vbVariant
        l_varArgs = Array(CDbl(Value))
    Case vbDecimal
        l_varArgs = Array(CDec(Value))
    Case vbByte
        l_varArgs = Array(CByte(Value))
    Case Else
    End Select
    If MethodID <> -1 Then
        m_seEngine.InvokeMember Element.Object, MethodID, VbLet, l_varArgs
    End If
    Err.Clear
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    TweenMode = F2TM_Linear
    TweenParameter = 1
    ValueType = F2TT_Number
    Value = 0
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngValue As Long
    With File
        .Load ElementID
        .Load PropertyName
        .Load Variable
        .Load l_lngValue
        TweenMode = l_lngValue
        .Load TweenParameter
        MethodID = -1
        .Load l_lngValue
        ValueType = l_lngValue
        m_varValue = .LoadVariant
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save ElementID
        .Save PropertyName
        .Save Variable
        .Save CLng(TweenMode)
        .Save TweenParameter
        .Save CLng(ValueType)
        .SaveVariant m_varValue
    End With
End Sub

