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
Public Element As Fury2SequenceElement
Public ElementID As String
Public PropertyName As String
Public Value As Double
Public TweenMode As Fury2TweenModes
Public TweenParameter As Double
Private m_vtValueType As VbVarType
Private m_dblPreviousValue As Double
Private m_skfKeyframe As Fury2SequenceKeyframe

Public Property Get PreviousTween() As Fury2SequenceTween
On Error Resume Next
Dim l_skfKeyframe As Fury2SequenceKeyframe
Dim l_stwTween As Fury2SequenceTween
    Set l_skfKeyframe = m_skfKeyframe.PreviousKeyframe
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
    Set m_skfKeyframe = Keyframe
End Sub

Public Property Get Keyframe() As Fury2SequenceKeyframe
On Error Resume Next
    Set Keyframe = m_skfKeyframe
End Property

Friend Sub FetchStartValue()
On Error Resume Next
Dim l_stwPrevious As Fury2SequenceTween
Dim l_varValue As Variant
    Set l_stwPrevious = PreviousTween
    If l_stwPrevious Is Nothing Then
        l_varValue = VBA.CallByName(Element.Object, PropertyName, VbGet)
        m_dblPreviousValue = CDbl(l_varValue)
    Else
        m_dblPreviousValue = l_stwPrevious.Value
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
    Set Element = m_skfKeyframe.Sequence.Elements.FindByProperty("ID", ElementID)
    Element.Reinitialize
End Sub

Public Sub Reinitialize()
On Error Resume Next
    If Element Is Nothing Then
        m_vtValueType = vbNull
        Set Element = m_skfKeyframe.Sequence.Elements.FindByProperty("ID", ElementID)
    End If
    Element.Reinitialize
    FetchStartValue
End Sub

Public Sub Update(ByVal Progress As Double)
On Error Resume Next
Dim l_dblValue As Double
    Select Case TweenMode
    Case F2TM_Instant
        If Progress >= 1 Then
            SetValue Value
        End If
    Case F2TM_Linear
        SetValue (m_dblPreviousValue) + ((Value - m_dblPreviousValue) * Progress)
    Case F2TM_Exponential
        l_dblValue = (m_dblPreviousValue) + ((Value - m_dblPreviousValue) * Progress)
        If l_dblValue < 0 Then l_dblValue = 0
        If l_dblValue > 1 Then l_dblValue = 1
        SetValue l_dblValue ^ TweenParameter
    Case F2TM_Sinusoidal
        l_dblValue = (m_dblPreviousValue) + ((Value - m_dblPreviousValue) * Progress)
        If l_dblValue < 0 Then l_dblValue = 0
        If l_dblValue > 1 Then l_dblValue = 1
        SetValue Sin(l_dblValue) * c_dblSineScale
    Case Else
    End Select
End Sub

Friend Sub SetValue(ByVal Value As Double)
On Error Resume Next
    If m_vtValueType = vbNull Then
        FetchValueType
    End If
    Select Case m_vtValueType
    Case vbInteger
        VBA.CallByName Element.Object, PropertyName, VbLet, CInt(Value)
    Case vbLong
        VBA.CallByName Element.Object, PropertyName, VbLet, CLng(Value)
    Case vbSingle
        VBA.CallByName Element.Object, PropertyName, VbLet, CSng(Value)
    Case vbDouble
        VBA.CallByName Element.Object, PropertyName, VbLet, CDbl(Value)
    Case vbCurrency
        VBA.CallByName Element.Object, PropertyName, VbLet, CCur(Value)
    Case vbString
        VBA.CallByName Element.Object, PropertyName, VbLet, CStr(Value)
    Case vbBoolean
        VBA.CallByName Element.Object, PropertyName, VbLet, CBool(Value)
    Case vbVariant
        VBA.CallByName Element.Object, PropertyName, VbLet, CDbl(Value)
    Case vbDecimal
        VBA.CallByName Element.Object, PropertyName, VbLet, CDec(Value)
    Case vbByte
        VBA.CallByName Element.Object, PropertyName, VbLet, CByte(Value)
    Case Else
    End Select
    Err.Clear
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    TweenMode = F2TM_Linear
    TweenParameter = 1
End Sub
