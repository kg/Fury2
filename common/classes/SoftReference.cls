VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SoftReference"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const TraceReferences = 0
Private m_lngReference As Long
Private m_objReference As Object
Private m_booReferenced As Boolean

Public Property Get Referenced() As Boolean
    Referenced = m_booReferenced
End Property

Public Property Get Value() As Object
Attribute Value.VB_UserMemId = 0
Attribute Value.VB_MemberFlags = "200"
Dim l_objValue As Object
Dim l_lngNull As Long
    If Not m_booReferenced Then Exit Property
    #If TraceReferences Then
        Debug.Print "SoftReference::Get"
    #End If
    Set Value = m_objReference
'    CopyMemory l_objValue, m_lngReference, 4
'    Set Value = l_objValue
'    CopyMemory l_objValue, l_lngNull, 4
End Property

Public Property Set Value(ByVal NewValue As Object)
On Error Resume Next
Dim l_lngNull As Long
    If NewValue Is Nothing Then
        #If TraceReferences Then
            Debug.Print "SoftReference::Unset"
        #End If
'        CopyMemory m_lngReference, l_lngNull, 4
        Set m_objReference = Nothing
        m_booReferenced = False
    Else
        #If TraceReferences Then
            Debug.Print "SoftReference::Set"
        #End If
'        CopyMemory m_lngReference, NewValue, 4
        Set m_objReference = NewValue
        m_booReferenced = True
    End If
End Property

Public Property Let Value(ByVal NewValue As Object)
On Error Resume Next
Dim l_lngNull As Long
    If NewValue Is Nothing Then
        #If TraceReferences Then
            Debug.Print "SoftReference::Unset"
        #End If
'        CopyMemory m_lngReference, l_lngNull, 4
        Set m_objReference = Nothing
        m_booReferenced = False
    Else
        #If TraceReferences Then
            Debug.Print "SoftReference::Set"
        #End If
'        CopyMemory m_lngReference, NewValue, 4
        Set m_objReference = NewValue
        m_booReferenced = True
    End If
End Property

Private Sub Class_Initialize()
On Error Resume Next
    #If TraceReferences Then
        Debug.Print "SoftReference::Create"
    #End If
End Sub

Private Sub Class_Terminate()
On Error Resume Next
Dim l_lngNull As Long
    #If TraceReferences Then
        Debug.Print "SoftReference::Terminate"
    #End If
    Set m_objReference = Nothing
'    CopyMemory m_lngReference, l_lngNull, 4
    m_booReferenced = False
End Sub
