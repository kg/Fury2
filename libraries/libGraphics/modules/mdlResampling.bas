Attribute VB_Name = "mdlResampling"
'
'   ::fury2 resampling code::
'

Option Explicit

Sub ResamplePoint(SourceImage As Fury2Image, DestImage As Fury2Image, SourceRect As Fury2Rect, DestRect As Fury2Rect)
'Dim DestParam As BlitParam, SourceParam As BlitParam
'    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestImage.Width, DestImage.Pointer(0, 0)
'    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceImage.Width, SourceImage.Pointer(0, 0)
'    SoftFX.Resample_Integer DestParam, SourceParam
End Sub

Sub ResampleBiLinear(SourceImage As Fury2Image, DestImage As Fury2Image, SourceRect As Fury2Rect, DestRect As Fury2Rect)
'Dim DestParam As BlitParam, SourceParam As BlitParam
'    FillParam DestParam, DestRect.left, DestRect.top, DestRect.Right, DestRect.Bottom, DestImage.Width, DestImage.Pointer(0, 0)
'    FillParam SourceParam, SourceRect.left, SourceRect.top, SourceRect.Right, SourceRect.Bottom, SourceImage.Width, SourceImage.Pointer(0, 0)
'    Call SoftFX.Resample_Bilinear(DestParam, SourceParam)
End Sub
