VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2PixelFormat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
'local variable(s) to hold property value(s)
Private mvarRedBits As Long 'local copy
Private mvarGreenBits As Long 'local copy
Private mvarBlueBits As Long 'local copy
Private mvarAlphaBits As Long 'local copy
Private mvarRedOffset As Long 'local copy
Private mvarGreenOffset As Long 'local copy
Private mvarBlueOffset As Long 'local copy
Private mvarAlphaOffset As Long 'local copy
Private mvarPitch As Long 'local copy

Public Function GetPF() As PixelFormat
Dim PF As PixelFormat
    With PF
        .RedLength = mvarRedBits
        .RedStart = mvarRedOffset
        .GreenLength = mvarGreenBits
        .GreenStart = mvarGreenOffset
        .BlueLength = mvarBlueBits
        .BlueStart = mvarBlueOffset
        .AlphaLength = mvarAlphaBits
        .AlphaStart = mvarAlphaOffset
        .RedMask = Bitmask(mvarRedOffset, mvarRedBits)
        .GreenMask = Bitmask(mvarGreenOffset, mvarGreenBits)
        .BlueMask = Bitmask(mvarBlueOffset, mvarBlueBits)
        .AlphaMask = Bitmask(mvarAlphaOffset, mvarAlphaBits)
        .Pitch = mvarPitch
    End With
    GetPF = PF
End Function

Public Sub SetPixelFormatPreset(Preset As F2PixelFormats)
    Select Case Preset
    Case BGRA32
        SetPixelFormat 16, 8, 8, 8, 0, 8, 24, 8, 32
    Case RGBA32
        SetPixelFormat 0, 8, 8, 8, 16, 8, 24, 8, 32
    Case ABGR32
        SetPixelFormat 24, 8, 16, 8, 8, 8, 0, 8, 32
    Case ARGB32
        SetPixelFormat 8, 8, 16, 8, 24, 8, 0, 8, 32
    Case RGB24
        SetPixelFormat 0, 8, 8, 8, 16, 8, 24, 0, 24
    Case BGR24
        SetPixelFormat 16, 8, 8, 8, 0, 8, 24, 0, 24
    Case RGB16
        SetPixelFormat 0, 5, 5, 6, 11, 5, 16, 0, 16
    Case BGR16
        SetPixelFormat 11, 5, 5, 6, 0, 5, 16, 0, 16
    Case RGB15
        SetPixelFormat 0, 5, 5, 5, 10, 5, 15, 0, 16
    Case BGR15
        SetPixelFormat 10, 5, 5, 5, 0, 5, 15, 0, 16
    Case Else
        SetPixelFormat 16, 8, 8, 8, 0, 8, 24, 8, 32
    End Select
End Sub

Public Sub SetPixelFormat(RedOffset As Long, RedBits As Long, GreenOffset As Long, GreenBits As Long, BlueOffset As Long, BlueBits As Long, AlphaOffset As Long, AlphaBits As Long, Pitch As Long)
    mvarRedBits = RedBits
    mvarRedOffset = RedOffset
    mvarGreenBits = GreenBits
    mvarGreenOffset = GreenOffset
    mvarBlueBits = BlueBits
    mvarBlueOffset = BlueOffset
    mvarAlphaBits = AlphaBits
    mvarAlphaOffset = AlphaOffset
    mvarPitch = Pitch
End Sub

Public Property Let Pitch(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.Pitch = 5
    mvarPitch = vData
End Property


Public Property Get Pitch() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.Pitch
    Pitch = mvarPitch
End Property



Public Property Let AlphaOffset(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.AlphaOffset = 5
    mvarAlphaOffset = vData
End Property


Public Property Get AlphaOffset() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.AlphaOffset
    AlphaOffset = mvarAlphaOffset
End Property



Public Property Let BlueOffset(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.BlueOffset = 5
    mvarBlueOffset = vData
End Property


Public Property Get BlueOffset() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.BlueOffset
    BlueOffset = mvarBlueOffset
End Property



Public Property Let GreenOffset(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.GreenOffset = 5
    mvarGreenOffset = vData
End Property


Public Property Get GreenOffset() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.GreenOffset
    GreenOffset = mvarGreenOffset
End Property



Public Property Let RedOffset(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.RedOffset = 5
    mvarRedOffset = vData
End Property


Public Property Get RedOffset() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.RedOffset
    RedOffset = mvarRedOffset
End Property



Public Property Let AlphaBits(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.AlphaBits = 5
    mvarAlphaBits = vData
End Property


Public Property Get AlphaBits() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.AlphaBits
    AlphaBits = mvarAlphaBits
End Property



Public Property Let BlueBits(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.BlueBits = 5
    mvarBlueBits = vData
End Property


Public Property Get BlueBits() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.BlueBits
    BlueBits = mvarBlueBits
End Property



Public Property Let GreenBits(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.GreenBits = 5
    mvarGreenBits = vData
End Property


Public Property Get GreenBits() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.GreenBits
    GreenBits = mvarGreenBits
End Property



Public Property Let RedBits(ByVal vData As Long)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.RedBits = 5
    mvarRedBits = vData
End Property


Public Property Get RedBits() As Long
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.RedBits
    RedBits = mvarRedBits
End Property



