VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IInspectableCollection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Property Get ItemCount() As Long
End Property

Public Property Get ItemValue(ByVal Index As Long) As Variant
End Property

Public Property Let ItemValue(ByVal Index As Long, ByVal Value As Variant)
End Property

Public Property Set ItemValue(ByVal Index As Long, ByVal Value As Variant)
End Property

