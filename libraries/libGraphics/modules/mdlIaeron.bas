Attribute VB_Name = "mdlIaeron"
'
'   Iaeron VB Bindings
'   Copyright 2002 Kevin Gadd
'

Option Explicit

Public Const c_strImageFilter As String = "*.bmp;*.gif;*.jpg;*.png;*.tga;*.pcx;*.jpeg"

Public Declare Function iaBlendColors5050 Lib "Iaeron" Alias "_iaBlendColors5050@8" (ByVal Color1 As Long, ByVal Color2 As Long) As Long
Public Declare Function iaSaveImage Lib "Iaeron" Alias "_iaSaveImage@16" (ByVal filename As String, ByVal Pointer As Long, ByVal Width As Long, ByVal Height As Long) As Long
Public Declare Function iaSaveImageP Lib "Iaeron" Alias "_iaSaveImageP@20" (ByVal filename As String, ByVal Pointer As Long, ByVal PalettePointer As Long, ByVal Width As Long, ByVal Height As Long) As Long
Public Declare Function iaLoadImage Lib "Iaeron" Alias "_iaLoadImage@4" (ByVal filename As String) As Long
Public Declare Function iaLoadImageA Lib "Iaeron" Alias "_iaLoadImageA@8" (ByVal filename As String, ByRef OutAlpha As Long) As Long
Public Declare Function iaLoadImageP Lib "Iaeron" Alias "_iaLoadImageP@4" (ByVal filename As String) As Long
Public Declare Function iaGetPaletteLength Lib "Iaeron" Alias "_iaGetPaletteLength@4" (ByVal Handle As Long) As Long
Public Declare Function iaGetWidth Lib "Iaeron" Alias "_iaGetWidth@4" (ByVal Handle As Long) As Long
Public Declare Function iaGetHeight Lib "Iaeron" Alias "_iaGetHeight@4" (ByVal Handle As Long) As Long
Public Declare Sub iaSwapRedBlue Lib "Iaeron" Alias "_iaSwapRedBlue@4" (ByVal Handle As Long)
Public Declare Sub iaComposite Lib "Iaeron" Alias "_iaComposite@8" (ByVal Handle As Long, ByVal Color As Long)
Public Declare Sub iaCompositeAlpha Lib "Iaeron" Alias "_iaCompositeAlpha@12" (ByVal Handle As Long, ByVal Color As Long, ByVal Alpha As Long)
Public Declare Function iaGetPointer Lib "Iaeron" Alias "_iaGetPointer@4" (ByVal Handle As Long) As Long
Public Declare Sub iaGetPixels Lib "Iaeron" Alias "_iaGetPixels@8" (ByVal Handle As Long, ByVal DestinationPointer As Long)
Public Declare Sub iaGetPalette Lib "Iaeron" Alias "_iaGetPalette@8" (ByVal Handle As Long, ByVal DestinationPointer As Long)
Public Declare Sub iaFreeImage Lib "Iaeron" Alias "_iaFreeImage@4" (ByRef Handle As Long)
Public Declare Sub iaInit Lib "Iaeron" Alias "_iaInit@0" ()
Public Declare Sub iaShutdown Lib "Iaeron" Alias "_iaShutdown@0" ()
