VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ImportTTFOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Implements IVirtualFileSaveable
Implements IInspectable

Private mvarName As String
Private mvarSize As Single
Private mvarBold As Boolean
Private mvarItalic As Boolean

Public Property Get Name() As String
    Name = mvarName
End Property

Public Property Let Name(ByRef vData As String)
    mvarName = vData
End Property

Public Property Get Size() As Single
    Size = mvarSize
End Property

Public Property Let Size(ByVal vData As Single)
    mvarSize = vData
End Property

Public Property Get Bold() As Boolean
    Bold = mvarBold
End Property

Public Property Let Bold(ByVal vData As Boolean)
    mvarBold = vData
End Property

Public Property Get Italic() As Boolean
    Italic = mvarItalic
End Property

Public Property Let Italic(ByVal vData As Boolean)
    mvarItalic = vData
End Property

Private Sub Class_Initialize()
    mvarName = "Tahoma"
    mvarSize = 8
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
    With File
        .Load mvarName
        .Load mvarSize
        .Load mvarBold
        .Load mvarItalic
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
    With File
        .Save mvarName
        .Save mvarSize
        .Save mvarBold
        .Save mvarItalic
    End With
End Sub

