VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2GEEnums"
Attribute VB_GlobalNameSpace = True
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Public Enum SFXLockingModes
    LockingMode_Default = 0
    LockingMode_AutoUnlock = 1
    LockingMode_AutoUnlock_AutoLock = 2
End Enum

Public Enum SFXRenderModes
    RenderMode_Default = -1
    RenderMode_Normal = 0
    RenderMode_SourceAlpha = 1
    RenderMode_Additive = 2
    RenderMode_Subtractive = 3
    RenderMode_Merge = 4
    RenderMode_Shadow = 5
End Enum

Public Enum SFXResampleModes
    ResampleMode_Default = -1
    ResampleMode_None = 0
    ResampleMode_Linear = 1
    ResampleMode_Bilinear = 2
    ResampleMode_Bilinear_High_Quality = 3
    ResampleMode_Bicubic = 4
    ResampleMode_Gaussian = 5
    ResampleMode_2xSaI = 6
End Enum

Public Enum SFXBlitModes
    BlitMode_Default = -1
    BlitMode_Normal = 0
    BlitMode_Matte = 1
    BlitMode_SourceAlpha = 2
    BlitMode_Additive = 3
    BlitMode_Subtractive = 4
    BlitMode_AND = 5
    BlitMode_OR = 6
    BlitMode_XOR = 7
    BlitMode_Lightmap = 8
    BlitMode_Lightmap_RGB = 9
    BlitMode_Matte_Tint = 10
    BlitMode_SourceAlpha_Tint = 11
    BlitMode_Font = 12
    BlitMode_Font_SourceAlpha = 13
    BlitMode_Dither = 14
    BlitMode_Screen = 15
    BlitMode_Multiply = 16
    BlitMode_Merge = 17
    BlitMode_Unerase = 18
    BlitMode_Erase = 19
    BlitMode_Font_Merge = 20
    BlitMode_Behind = 21
    BlitMode_Dodge = 22
    BlitMode_Burn = 23
    BlitMode_Normal_Tint = 24
End Enum

Public Enum F2PixelFormats
    BGRA32 = 0&
    RGBA32 = 1&
    ABGR32 = 2&
    ARGB32 = 3&
    BGR24 = 4&
    RGB24 = 5&
    RGB16 = 6&
    BGR16 = 7&
    RGB15 = 8&
    BGR15 = 9&
    RGBA16 = 10&
    BGRA16 = 11&
    RGB8 = 12&
    BGR8 = 13&
    RGBA8 = 14&
    BGRA8 = 15&
End Enum

Public Enum SFXChannels
    Red = 2&
    Green = 1&
    Blue = 0&
    Alpha = 3&
End Enum