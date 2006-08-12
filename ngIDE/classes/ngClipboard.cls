VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngClipboard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private m_edEditor As cEditor
Private m_clpClipboard As cCustomClipboard

Friend Sub SetEditor(ByVal Editor As cEditor)
On Error Resume Next
    Set m_edEditor = Editor
    Set m_clpClipboard = New cCustomClipboard
End Sub

Public Function RegisterFormat(ByVal Name As String) As Long
On Error Resume Next
    RegisterFormat = m_clpClipboard.AddFormat(Name)
End Function

Public Property Get ContainsFormat(ByVal Format As Long) As Boolean
On Error Resume Next
    With m_clpClipboard
        .GetCurrentFormats m_edEditor.hWnd
        ContainsFormat = .HasCurrentFormat(Format)
    End With
End Property

Public Function Save(ByVal Obj As Object, ByVal Format As Long) As Boolean
On Error Resume Next
    Save = mdlClipboard.ClipboardSerialize(m_edEditor.Clipboard, Format, Obj)
End Function

Public Function Load(ByVal Obj As Object, ByVal Format As Long) As Boolean
On Error Resume Next
    Load = mdlClipboard.ClipboardDeserialize(m_edEditor.Clipboard, Format, Obj)
End Function

Public Property Get Text() As String
On Error Resume Next
    Text = Clipboard.GetText()
End Property

Public Property Let Text(ByVal NewValue As String)
On Error Resume Next
    Clipboard.Clear
    Clipboard.SetText NewValue
End Property
