VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cNullDocument"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Implements iDocument

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Private Property Get iDocument_CanSave() As Boolean

End Property

Private Property Get iDocument_Filename() As String

End Property

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin

End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)

End Property

Private Function iDocument_Save(Filename As String) As Boolean

End Function

Private Property Get iDocument_Typename() As String

End Property

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = False
End Property
