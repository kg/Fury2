VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Globals"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Private m_objHook As Object
Private m_colProcesses As New Collection

Public Sub SetHook(Obj As Object)
    Set m_objHook = Obj
End Sub

Public Sub BeginProcess(Optional ByRef Caption As String = "Busy...")
On Error Resume Next
    m_colProcesses.Add Caption
    m_objHook.SetStatus Caption
    m_objHook.SetProgress 0
End Sub

Public Sub UpdateProcess(Optional ByVal Progress As Single = 0, Optional ByRef NewCaption As String = "")
On Error Resume Next
    If Len(NewCaption) > 0 Then
        m_objHook.SetStatus NewCaption
    End If
    m_objHook.SetProgress Progress / 100
End Sub

Public Sub EndProcess()
On Error Resume Next
    m_colProcesses.Remove m_colProcesses.Count
    If m_colProcesses.Count > 0 Then
        m_objHook.SetStatus CStr(m_colProcesses.Item(1))
    Else
        m_objHook.SetStatus
    End If
    m_objHook.SetProgress
End Sub

Public Function SelectColor(Optional ByVal Color As Long = -16777216, Optional ByRef Image As Fury2Image = Nothing, Optional ByRef Owner As Object = Nothing) As Long
Dim l_frmDialog As New frmSelectColor
    F2Init
    Load l_frmDialog
    Set l_frmDialog.PickerImage = Image
    l_frmDialog.OldColor = Color
    l_frmDialog.NewColor = Color
    l_frmDialog.Cancelled = False
    l_frmDialog.Done = False
    l_frmDialog.Show vbModal, Owner
    l_frmDialog.RefreshAll
    Do While Not l_frmDialog.Done
        DoEvents
    Loop
    If l_frmDialog.Cancelled Then
        SelectColor = Color
    Else
        SelectColor = l_frmDialog.NewColor
    End If
    Unload l_frmDialog
End Function
