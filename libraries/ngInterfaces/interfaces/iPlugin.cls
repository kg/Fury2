VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "iPlugin"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Sub ShowConfiguration()
End Sub

Public Property Get CanConfigure() As Boolean
End Property

Public Property Get PluginName() As String
End Property

Public Property Get PluginPath() As String
End Property

Public Property Get PluginDescription() As String
End Property

Public Property Get ShowInPluginMenu() As Boolean
End Property

Public Property Get Icon() As IPictureDisp
End Property

Public Property Set Editor(NewEditor As Object)
End Property

Public Sub Initialize()
End Sub

Public Sub Activate()
End Sub

Public Sub Shutdown()
End Sub
