VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2SequenceElement"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Implements Fury2Object
Implements IVirtualFileSaveable
Implements IInspectable
Public Object As Object
Attribute Object.VB_VarDescription = "*"
Public Expression As String
Attribute Expression.VB_VarDescription = "The default object for the element."
Private m_strID As String
Attribute m_strID.VB_VarDescription = "The name of the element."
Private m_booVisible As Boolean
Private m_seqSequence As WeakReference
Private m_strShowScript As String
Private m_strHideScript As String
Private m_evtShow As Fury2Event
Private m_evtHide As Fury2Event

Public Property Get ID() As String
    ID = m_strID
End Property

Public Property Let ID(ByVal NewID As String)
On Error Resume Next
    If NewID <> m_strID Then
        If Sequence.Elements.Contains(NewID) Then Exit Property
        m_strID = NewID
        Sequence.Elements.Remove Me
        Sequence.AddElement Me
    End If
End Property

Friend Sub SetID(ByVal NewID As String)
On Error Resume Next
    m_strID = NewID
End Sub

Public Property Get ShowScript() As String
    ShowScript = m_strShowScript
End Property

Public Property Let ShowScript(ByRef NewScript As String)
On Error Resume Next
    If NewScript <> m_strShowScript Then
        m_strShowScript = NewScript
        Set m_evtShow = Nothing
    End If
End Property

Public Property Get HideScript() As String
    HideScript = m_strHideScript
End Property

Public Property Let HideScript(ByRef NewScript As String)
On Error Resume Next
    If NewScript <> m_strHideScript Then
        m_strHideScript = NewScript
        Set m_evtHide = Nothing
    End If
End Property

Public Function Duplicate() As Fury2SequenceElement
On Error Resume Next
Dim l_eleNew As Fury2SequenceElement
    Set l_eleNew = New Fury2SequenceElement
    With l_eleNew
        .ShowScript = ShowScript
        .HideScript = HideScript
        .Expression = Expression
        .SetID ID
    End With
    Set Duplicate = l_eleNew
End Function

Public Function Class_ToString() As String
On Error Resume Next
    Class_ToString = "Element: " & ID
End Function

Public Property Get Visible() As Boolean
Attribute Visible.VB_Description = "If True, this element has been shown."
On Error Resume Next
    Visible = m_booVisible
End Property

Public Sub Free()
On Error Resume Next
    Hide
    Set Object = Nothing
    Set m_evtShow = Nothing
    Set m_evtHide = Nothing
    WRFree m_seqSequence
End Sub

Friend Sub SetSequence(Sequence As Fury2Sequence)
On Error Resume Next
    WRSet m_seqSequence, Sequence
End Sub

Public Property Get Sequence() As Fury2Sequence
Attribute Sequence.VB_Description = "*"
On Error Resume Next
    Set Sequence = WRGetSequence(m_seqSequence)
End Property

Public Property Get Element() As Fury2SequenceElement
Attribute Element.VB_Description = "*"
    Set Element = Me
End Property

Public Sub Hide()
On Error Resume Next
    If m_booVisible Then
        m_booVisible = False
        If m_evtHide Is Nothing Then
            Object.Hide
        Else
            m_evtHide.Invoke Sequence, Element
        End If
    End If
End Sub

Public Sub Show()
On Error Resume Next
    If Not m_booVisible Then
        m_booVisible = True
        m_evtShow.Invoke Sequence, Element
    End If
End Sub

Friend Sub CompileScripts()
On Error Resume Next
    If Len(HideScript) = 0 Then
        Set m_evtHide = Nothing
    Else
        Set m_evtHide = Sequence.Engine.CompileScriptBlock(HideScript, "Sequence, Element")
    End If
    If Len(ShowScript) = 0 Then
        Set m_evtShow = Nothing
    Else
        Set m_evtShow = Sequence.Engine.CompileScriptBlock(ShowScript, "Sequence, Element")
    End If
End Sub

Public Sub Initialize()
On Error Resume Next
    CompileScripts
    If Len(Expression) > 0 Then
        m_booVisible = False
        Set Object = Nothing
        Set Object = Sequence.Variable(Expression)
        If Object Is Nothing Then Set Object = Sequence.Engine.Evaluate(Expression)
    End If
End Sub

Public Sub Reinitialize()
On Error Resume Next
    If (Object Is Nothing) And Len(Expression) > 0 Then
        m_booVisible = False
        Set Object = Nothing
        Set Object = Sequence.Variable(Expression)
        If Object Is Nothing Then Set Object = Sequence.Engine.Evaluate(Expression)
    End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load m_strID
        .Load Expression
        .Load m_strShowScript
        .Load m_strHideScript
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save m_strID
        .Save Expression
        .Save m_strShowScript
        .Save m_strHideScript
    End With
End Sub


