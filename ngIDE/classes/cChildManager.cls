VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cChildManager"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
' Handles notifying the main form of child changes

Option Explicit
Private WithEvents m_frmForm As Form
Attribute m_frmForm.VB_VarHelpID = -1

Public Property Let Visible(NewValue As Boolean)
On Error Resume Next
    With m_frmForm
        .Visible = NewValue
    End With
    With extender
        .Visible = NewValue
    End With
End Property

Public Sub Activate()
On Error Resume Next
Dim l_objForm As Object
    SetBusyState True
    Err.Clear
    With extender
        .SetFocus
    End With
    m_frmForm.Form_Activate
    g_edEditor.Event_DocumentActivate Me
    If g_booMainWindowLoaded Then frmMain.RefreshWindows
    SetBusyState False
End Sub

Public Sub Hide()
On Error Resume Next
    If g_edEditor.Event_DocumentClose(Me) Then Exit Sub
    SetBusyState True
    If Not (Form Is Nothing) Then
        Form.Hide
        Unload Form
    End If
    Detach
    SetBusyState False
End Sub

Public Property Get extender() As MDIActiveX
On Error Resume Next
Dim l_objForm As iExtendedForm
    Set l_objForm = m_frmForm
    Set extender = l_objForm.extender
    Err.Clear
End Property

Public Property Get Menus() As iCustomMenus
On Error Resume Next
    Set Menus = m_frmForm
End Property

Public Property Get Accelerators() As iCustomAccelerators
On Error Resume Next
    Set Accelerators = m_frmForm
End Property

Public Property Get Toolbars() As iToolbars
On Error Resume Next
    Set Toolbars = m_frmForm
End Property

Public Property Get Document() As iDocument
On Error Resume Next
    Set Document = m_frmForm
End Property

Public Property Get Form() As Form
On Error Resume Next
    Set Form = m_frmForm
End Property

Public Sub Attach(ByRef NewForm As Form)
On Error Resume Next
    Set m_frmForm = NewForm
End Sub

Public Sub Detach()
On Error Resume Next
    Set m_frmForm = Nothing
    If g_booMainWindowLoaded Then frmMain.DereferenceChildManager Me
End Sub

Private Sub m_frmForm_Activate()
On Error Resume Next
    If m_frmForm.WindowState = 0 Then
        m_frmForm.WindowState = 2
    End If
End Sub

Private Sub m_frmForm_Unload(Cancel As Integer)
On Error Resume Next
    Detach
    If g_booMainWindowLoaded Then frmMain.RefreshWindows
End Sub
