Attribute VB_Name = "mdlDX8Override"
Option Explicit

Private l_imgRadialGradient As Fury2Image

Private l_devRTDevice As Direct3DDevice8

Public Const RTIdentifierTag As Long = &H44334437 ' D3D8 -1
Public Const IdentifierTag As Long = &H44334438 ' D3D8
Public Const IdentifierTagIndex As Long = 7
Public Const DeviceTagIndex As Long = 6
Public Const TextureTagIndex As Long = 5
Public Const RenderTargetTagIndex As Long = 4

Public Sub SetFormSize(ByRef Frm As Form, X As Long, Y As Long, Optional ByVal Center As Boolean = False)
On Error Resume Next
Dim BW As Long, TH As Long, OldMode As Long
    OldMode = Frm.ScaleMode
    Frm.ScaleMode = 1
    BW = (Frm.Width - Frm.ScaleWidth) \ 2
    TH = (Frm.Height - Frm.ScaleHeight) - (BW * 2)
    If Center Then
        Frm.Move (Screen.Width - (CLng(X * Screen.TwipsPerPixelX) + (BW * 2))) \ 2, (Screen.Height - (CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2))) \ 2, CLng(X * Screen.TwipsPerPixelX) + (BW * 2), CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    Else
        Frm.Move Frm.Left, Frm.Top, CLng(X * Screen.TwipsPerPixelX) + (BW * 2), CLng(Y * Screen.TwipsPerPixelY) + TH + (BW * 2)
    End If
    Frm.ScaleMode = OldMode
End Sub

Public Sub DerefRectangle(ByVal Pointer As Long, ByRef Output As Rectangle, ByVal Image As Long)
On Error Resume Next
'    Too tricky to ever work right
'    If Pointer Then CopyMemory DerefRectangle, ByVal VarPtr(Pointer), 4
    If Pointer <> 0 Then
        CopyMemory ByVal VarPtr(Output), ByVal Pointer, LenB(Output)
    Else
        Output.Left = 0
        Output.Top = 0
        Output.Width = GetImageWidth(Image)
        Output.Height = GetImageHeight(Image)
    End If
End Sub

'
'   DX8Override
'   Creates a SoftFX image that is attached to a D3D8 render target
'

'Public Function o_SetPixel(ByRef Parameters As Image_Operation_Parameters) As Long
'On Error Resume Next
'Dim l_lngTag As Long
'Dim l_lngDC As Long
'    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
'    If l_lngTag = IdentifierTag Then
'        If GetImageLocked(Parameters.Image) Then
'            o_SetPixel = 1
'            l_lngDC = GetImageTag(Parameters.Image, 1)
'            SetPixelV l_lngDC, Parameters.P2, Parameters.P3, SFXColorToGDI(Parameters.P4)
'        Else
'        End If
'    End If
'End Function

Public Function o_Box(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_rctParameter As Rectangle
Dim l_verVertexes(0 To 4) As D3DVertex
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_Box = 1
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Image
        With Parameters.Header
            If KeyMatches(.KeyPointer, "FilterSimple_Box_SourceAlpha") Then
                SetBlendMode l_devDevice, BlitMode_SourceAlpha
            ElseIf KeyMatches(.KeyPointer, "FilterSimple_Box_Additive") Then
                SetBlendMode l_devDevice, BlitMode_Additive
            Else
                SetBlendMode l_devDevice, BlitMode_Normal
            End If
        End With
        l_verVertexes(0) = MakeVertex(l_rctParameter.Left, l_rctParameter.Top, 1, Parameters.P3)
        l_verVertexes(1) = MakeVertex(l_rctParameter.Left + l_rctParameter.Width - 1, l_rctParameter.Top, 1, Parameters.P3)
        l_verVertexes(2) = MakeVertex(l_rctParameter.Left + l_rctParameter.Width - 1, l_rctParameter.Top + l_rctParameter.Height - 1, 1, Parameters.P3)
        l_verVertexes(3) = MakeVertex(l_rctParameter.Left, l_rctParameter.Top + l_rctParameter.Height - 1, 1, Parameters.P3)
        l_verVertexes(4) = MakeVertex(l_rctParameter.Left, l_rctParameter.Top, 1, Parameters.P3)
        l_devDevice.SetTexture 0, Nothing
        l_devDevice.DrawPrimitiveUP D3DPT_LINESTRIP, 4, ByVal VarPtr(l_verVertexes(0)), Len(l_verVertexes(0))
'        ChangeRenderTarget l_devDevice, l_srfRenderTarget
    End If
End Function

Public Function o_ConvexPolygon(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_verVertexes() As D3DVertex
Dim l_fpPoint As FPoint
Dim l_lngVertexes As Long
Dim l_lngVertexCount As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_ConvexPolygon = 1
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        l_lngVertexCount = GetPolygonVertexCount(Parameters.P2)
        ReDim l_verVertexes(0 To l_lngVertexCount)
        For l_lngVertexes = 0 To l_lngVertexCount - 1
            CopyMemory l_fpPoint, ByVal GetPolygonVertexPointer(Parameters.P2, l_lngVertexes), Len(l_fpPoint)
            With l_verVertexes(l_lngVertexes)
                .X = l_fpPoint.X
                .Y = l_fpPoint.Y
                .RHW = 1
                .Color = Parameters.P3
            End With
        Next l_lngVertexes
        l_verVertexes(l_lngVertexCount) = l_verVertexes(0)
        SetBlendMode l_devDevice, BlitMode_Normal
        l_devDevice.SetTexture 0, Nothing
        l_devDevice.DrawPrimitiveUP D3DPT_TRIANGLEFAN, l_lngVertexCount - 1, ByVal VarPtr(l_verVertexes(0)), Len(l_verVertexes(0))
'        ChangeRenderTarget l_devDevice, l_srfRenderTarget
    End If
End Function

Public Function o_ConvexPolygon_Textured(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_verVertexes() As D3DVertex
Dim l_tvVertex As TexturedVertex
Dim l_lngVertexes As Long
Dim l_lngVertexCount As Long
Dim l_sdDesc As D3DSURFACE_DESC, l_texTexture As Direct3DTexture8
Dim l_sngXR As Single, l_sngYR As Single
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_ConvexPolygon_Textured = 1
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        UseImageAsTextureH l_devDevice, Parameters.P2, 0
        Set l_texTexture = l_devDevice.GetTexture(0)
        l_texTexture.GetLevelDesc 0, l_sdDesc
        l_sngXR = 1 / l_sdDesc.Width
        l_sngYR = 1 / l_sdDesc.Height
        l_lngVertexCount = GetPolygonVertexCount(Parameters.P3)
        ReDim l_verVertexes(0 To l_lngVertexCount)
        For l_lngVertexes = 0 To l_lngVertexCount - 1
            CopyMemory l_tvVertex, ByVal GetTexturedPolygonVertexPointer(Parameters.P3, l_lngVertexes), Len(l_tvVertex)
            With l_verVertexes(l_lngVertexes)
                .X = l_tvVertex.X
                .Y = l_tvVertex.Y
                .U1 = (l_tvVertex.U + 0.5) * l_sngXR
                .V1 = (l_tvVertex.V + 0.5) * l_sngYR
                .Color = -1
                .RHW = 1
            End With
        Next l_lngVertexes
        l_verVertexes(l_lngVertexCount) = l_verVertexes(0)
        Select Case Parameters.P5
        Case GetAdditiveRenderer()
            SetBlendMode l_devDevice, BlitMode_Additive
        Case GetScreenRenderer()
            SetBlendMode l_devDevice, BlitMode_Screen
        Case GetSourceAlphaRenderer()
            SetBlendMode l_devDevice, BlitMode_SourceAlpha
        Case Else
            SetBlendMode l_devDevice, BlitMode_Normal
        End Select
        Select Case Parameters.P4
        Case GetBilinearScaler
            l_devDevice.SetTextureStageState 0, D3DTSS_MINFILTER, D3DTEXF_LINEAR
            l_devDevice.SetTextureStageState 0, D3DTSS_MAGFILTER, D3DTEXF_LINEAR
        Case Else
            l_devDevice.SetTextureStageState 0, D3DTSS_MINFILTER, D3DTEXF_POINT
            l_devDevice.SetTextureStageState 0, D3DTSS_MAGFILTER, D3DTEXF_POINT
        End Select
        l_devDevice.DrawPrimitiveUP D3DPT_TRIANGLEFAN, l_lngVertexCount - 1, ByVal VarPtr(l_verVertexes(0)), Len(l_verVertexes(0))
'        ChangeRenderTarget l_devDevice, l_srfRenderTarget
    End If
End Function

Public Function o_Line(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_rctParameter As Rectangle
Dim l_verVertexes(0 To 1) As D3DVertex
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_Line = 1
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Image
        With Parameters.Header
            If KeyMatches(.KeyPointer, "FilterSimple_Line_SourceAlpha") Then
                SetBlendMode l_devDevice, BlitMode_SourceAlpha
            ElseIf KeyMatches(.KeyPointer, "FilterSimple_Line_Additive") Then
                SetBlendMode l_devDevice, BlitMode_Additive
            Else
                SetBlendMode l_devDevice, BlitMode_Normal
            End If
        End With
        l_verVertexes(0) = MakeVertex(l_rctParameter.Left, l_rctParameter.Top, 1, Parameters.P3)
        l_verVertexes(1) = MakeVertex(l_rctParameter.Left + l_rctParameter.Width, l_rctParameter.Top + l_rctParameter.Height, 1, Parameters.P3)
        l_devDevice.SetTexture 0, Nothing
        l_devDevice.DrawPrimitiveUP D3DPT_LINESTRIP, 1, ByVal VarPtr(l_verVertexes(0)), Len(l_verVertexes(0))
'        ChangeRenderTarget l_devDevice, l_srfRenderTarget
    End If
End Function

Public Function o_Adjust(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_rctParameter As Rectangle
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_Adjust = 1
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Image
'        SoftFX.ClipRectangle_ImageClipRect l_rctParameter, Parameters.Image
        If Parameters.P3 > 0 Then
            SetBlendMode l_devDevice, BlitMode_Additive
            l_devDevice.SetTexture 0, Nothing
            mdlDX8.DrawFilledRect l_devDevice, l_rctParameter.Left, l_rctParameter.Top, l_rctParameter.Width, l_rctParameter.Height, F2RGB(Parameters.P3, Parameters.P3, Parameters.P3, 0)
        ElseIf Parameters.P3 < 0 Then
            SetBlendMode l_devDevice, BlitMode_Subtractive
            l_devDevice.SetTexture 0, Nothing
            mdlDX8.DrawFilledRect l_devDevice, l_rctParameter.Left, l_rctParameter.Top, l_rctParameter.Width, l_rctParameter.Height, F2RGB(-Parameters.P3, -Parameters.P3, -Parameters.P3, 0)
        End If
'        ChangeRenderTarget l_devDevice, l_srfRenderTarget
    End If
End Function

Public Function o_Gradient_Radial(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_rctParameter As Rectangle
Dim l_lngColor As Long
Dim l_sngR As Single
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        o_Gradient_Radial = 1
        If l_imgRadialGradient Is Nothing Then
            Set l_imgRadialGradient = F2Image(256, 256)
            l_imgRadialGradient.Clear 0
            l_imgRadialGradient.RadialGradientFill F2Rect(0, 0, 512, 512, False), Array(F2RGB(0, 0, 0, 0), F2RGB(255, 255, 255, 255)), RenderMode_Default
        End If
        DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Image
'        SoftFX.ClipRectangle_ImageClipRect l_rctParameter, Parameters.Image
        With l_devDevice
            .SetTexture 0, Nothing
            
            .SetRenderState D3DRS_BLENDOP, D3DBLENDOP_ADD
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_ZERO
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_ONE
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_SELECTARG2
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_DISABLE
            mdlDX8.DrawFilledRect l_devDevice, l_rctParameter.Left, l_rctParameter.Top, l_rctParameter.Width, l_rctParameter.Height, Parameters.P3
            
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1
            .SetTextureStageState 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE
            UseImageAsTexture l_devDevice, l_imgRadialGradient, 0
            l_sngR = 0.5 * (1 / 256)
            mdlDX8.DrawTexturedQuad l_devDevice, l_rctParameter.Left, l_rctParameter.Top, Ceil(l_rctParameter.Width / 2), Ceil(l_rctParameter.Height / 2), l_sngR, l_sngR, 1 - l_sngR, 1 - l_sngR, Parameters.P4
            mdlDX8.DrawTexturedQuad l_devDevice, l_rctParameter.Left, l_rctParameter.Top + Ceil(l_rctParameter.Height / 2), Ceil(l_rctParameter.Width / 2), Ceil(l_rctParameter.Height / 2), l_sngR, 1 - l_sngR, 1 - l_sngR, l_sngR, Parameters.P4
            mdlDX8.DrawTexturedQuad l_devDevice, l_rctParameter.Left + Ceil(l_rctParameter.Width / 2), l_rctParameter.Top, Ceil(l_rctParameter.Width / 2), Ceil(l_rctParameter.Height / 2), 1 - l_sngR, l_sngR, l_sngR, 1 - l_sngR, Parameters.P4
            mdlDX8.DrawTexturedQuad l_devDevice, l_rctParameter.Left + Ceil(l_rctParameter.Width / 2), l_rctParameter.Top + Ceil(l_rctParameter.Height / 2), Ceil(l_rctParameter.Width / 2), Ceil(l_rctParameter.Height / 2), 1 - l_sngR, 1 - l_sngR, l_sngR, l_sngR, Parameters.P4
            
        End With
'        ChangeRenderTarget l_devDevice, l_srfRenderTarget
    End If
End Function

Public Function o_Fill(ByRef Parameters As FilterSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_rctParameter As Rectangle
Dim l_lngColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        o_Fill = 1
        DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Image
        SoftFX.ClipRectangle_ImageClipRect l_rctParameter, Parameters.Image
        l_lngColor = Parameters.P3
        With Parameters.Header
            If KeyMatches(.KeyPointer, "FilterSimple_Fill_SourceAlpha") Then
                SetBlendMode l_devDevice, BlitMode_SourceAlpha
            ElseIf KeyMatches(.KeyPointer, "FilterSimple_Fill_Additive") Then
                SetBlendMode l_devDevice, BlitMode_Additive
            Else
                SetBlendMode l_devDevice, BlitMode_Normal
            End If
        End With
        l_devDevice.SetTexture 0, Nothing
        mdlDX8.DrawFilledRect l_devDevice, l_rctParameter.Left, l_rctParameter.Top, l_rctParameter.Width, l_rctParameter.Height, l_lngColor
'        ChangeRenderTarget l_devDevice, l_srfRenderTarget
    End If
End Function
'
'Public Function o_Fill_Channel(ByRef Parameters As FilterSimple_Parameters) As Long
'On Error Resume Next
'Dim l_lngTag As Long
'Dim l_lngDC As Long
'Dim l_rctArea As Rect, l_lngBrush As Long
'Dim l_rctParameter As Rectangle, l_lngROP As Long
'Dim l_lngRegion As Long
'    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
'    If l_lngTag = IdentifierTag Then
'        If GetImageLocked(Parameters.Image) Then
'            o_Fill_Channel = 1
'            l_lngDC = GetImageTag(Parameters.Image, 1)
'            DerefRectangle Parameters.Rectangle, l_rctParameter
'            With l_rctArea
'                .Left = l_rctParameter.Left
'                .Top = l_rctParameter.Top
'                .Right = .Left + l_rctParameter.Width
'                .Bottom = .Top + l_rctParameter.Height
'            End With
'            l_lngRegion = CreateRectRegion(l_rctArea)
'            l_lngROP = SetROP2(l_lngDC, ROP_AND)
'            l_lngBrush = CreateSolidBrush(RGB(IIf(Parameters.P3 = Red, 0, 255), IIf(Parameters.P3 = Green, 0, 255), IIf(Parameters.P3 = Blue, 0, 255)))
'            FillRegion l_lngDC, l_lngRegion, l_lngBrush
'            DeleteObject l_lngBrush
'            SetROP2 l_lngDC, ROP_OR
'            l_lngBrush = CreateSolidBrush(RGB(IIf(Parameters.P3 = Red, 255, 0), IIf(Parameters.P3 = Green, 255, 0), IIf(Parameters.P3 = Blue, 255, 0)))
'            FillRegion l_lngDC, l_lngRegion, l_lngBrush
'            DeleteObject l_lngBrush
'            SetROP2 l_lngDC, l_lngROP
'            DeleteObject l_lngRegion
'            DeleteObject l_lngBrush
'        Else
'        End If
'    End If
'End Function
'
'Public Function o_Invert(ByRef Parameters As FilterSimple_Parameters) As Long
'On Error Resume Next
'Dim l_lngTag As Long
'Dim l_lngDC As Long
'Dim l_rctArea As Rect, l_lngBrush As Long
'Dim l_rctParameter As Rectangle, l_lngROP As Long
'Dim l_lngRegion As Long
'    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
'    If l_lngTag = IdentifierTag Then
'        If GetImageLocked(Parameters.Image) Then
'            o_Invert = 1
'            l_lngDC = GetImageTag(Parameters.Image, 1)
'            DerefRectangle Parameters.Rectangle, l_rctParameter
'            With l_rctArea
'                .Left = l_rctParameter.Left
'                .Top = l_rctParameter.Top
'                .Right = .Left + l_rctParameter.Width
'                .Bottom = .Top + l_rctParameter.Height
'            End With
'            If Parameters.Header.ParameterCount = 3 Then
'                ' Invert_Channel
'                l_lngBrush = CreateSolidBrush(RGB(IIf(Parameters.P3 = Red, 255, 0), IIf(Parameters.P3 = Green, 255, 0), IIf(Parameters.P3 = Blue, 255, 0)))
'                l_lngROP = SetROP2(l_lngDC, ROP_XOR)
'            ElseIf Parameters.Header.ParameterCount = 2 Then
'                ' Invert or Invert_Color. Doesn't matter which; GDI doesn't care about alpha
'                l_lngBrush = CreateSolidBrush(RGB(255, 255, 255))
'                l_lngROP = SetROP2(l_lngDC, ROP_NOT)
'            End If
'            l_lngRegion = CreateRectRegion(l_rctArea)
'            FillRegion l_lngDC, l_lngRegion, l_lngBrush
'            SetROP2 l_lngDC, l_lngROP
'            DeleteObject l_lngRegion
'            DeleteObject l_lngBrush
'        Else
'        End If
'    End If
'End Function
'

Public Function o_Clear(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_Clear = 1
        Dim l_srfRenderTarget As Direct3DSurface8
        Set l_srfRenderTarget = ChangeRenderTarget(l_devDevice, GetImageRenderTargetH(Parameters.Image))
        If KeyMatches(Parameters.Header.KeyPointer, "Fill") Then
            l_devDevice.Clear 0, ByVal 0, D3DCLEAR_TARGET, Parameters.P2, 0, 0
        ElseIf KeyMatches(Parameters.Header.KeyPointer, "Clear") Then
            l_devDevice.Clear 0, ByVal 0, D3DCLEAR_TARGET, 0, 0, 0
        End If
    End If
End Function

' OK
Public Function o_Unlock(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_rctLock As D3DLOCKED_RECT
Dim l_rctLockArea As DxVBLibA.Rect
Dim l_sdDesc As D3DSURFACE_DESC
Dim l_devDevice As Direct3DDevice8
Dim l_srfDevice As Direct3DSurface8
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_Unlock = 1
        If GetImageLocked(Parameters.Image) <> 0 Then
            l_devDevice.EndScene
            Set l_srfDevice = GetImageRenderTargetH(Parameters.Image)
            l_srfDevice.GetDesc l_sdDesc
            With l_rctLockArea
                .Left = 0
                .Top = 0
                .Right = l_sdDesc.Width
                .Bottom = l_sdDesc.Height
            End With
            Err.Clear
            l_srfDevice.LockRect l_rctLock, l_rctLockArea, D3DLOCK_NOSYSLOCK
            If l_rctLock.pBits Then
                Debug.Print "Unlocked image " & Parameters.Image
                SetImageLocked Parameters.Image, 0
                SetImagePointer Parameters.Image, l_rctLock.pBits
                SetImagePitch Parameters.Image, l_rctLock.Pitch - (l_sdDesc.Width * 4)
            Else
                'Debug.Print "Couldn't unlock image " & Parameters.Image
            End If
        End If
    End If
End Function
' OK

' OK
Public Function o_Lock(ByRef Parameters As Image_Operation_Parameters) As Long
On Error Resume Next
Dim l_rctLock As D3DLOCKED_RECT
Dim l_rctLockArea As DxVBLibA.Rect
Dim l_sdDesc As D3DSURFACE_DESC
Dim l_devDevice As Direct3DDevice8
Dim l_srfDevice As Direct3DSurface8
    Set l_devDevice = GetImageDeviceH(Parameters.Image)
    If l_devDevice Is Nothing Then
    Else
        o_Lock = 1
        If GetImageLocked(Parameters.Image) = 0 Then
            Debug.Print "Locking image " & Parameters.Image
            Set l_srfDevice = GetImageRenderTargetH(Parameters.Image)
            l_srfDevice.UnlockRect
            SetImageLocked Parameters.Image, 1
            SetImagePointer Parameters.Image, 0
            SetImagePitch Parameters.Image, 0
            l_devDevice.BeginScene
        End If
    End If
End Function
' OK

' OK
Public Function o_Allocate_RenderTarget(ByRef Parameters As Image_Allocate_Parameters) As Long
On Error Resume Next
Dim l_texTexture As Direct3DTexture8
Dim l_unkTexture As IShellFolderEx_TLB.IUnknown
Dim l_lngTexture As Long, l_lngDevice As Long
Dim l_lngImage As Long
    ' Allocate overrides always consume the operation
    o_Allocate_RenderTarget = 1
    If (Parameters.Width < 1) Then Exit Function
    If (Parameters.Height < 1) Then Exit Function
    If (Parameters.Image = 0) Then Exit Function
    l_lngImage = Parameters.Image
    Set l_texTexture = CreateRenderTarget(Parameters.Width, Parameters.Height, l_devRTDevice)
    If l_texTexture Is Nothing Then
    Else
        Set l_unkTexture = l_texTexture
        l_unkTexture.AddRef
        Set l_unkTexture = Nothing
    End If
    CopyMemory l_lngTexture, l_texTexture, 4
    CopyMemory l_lngDevice, l_devRTDevice, 4
    SetImageTag l_lngImage, RenderTargetTagIndex, ObjPtr(l_texTexture.GetSurfaceLevel(0))
    SetImageTag l_lngImage, TextureTagIndex, l_lngTexture
    SetImageTag l_lngImage, DeviceTagIndex, l_lngDevice
    SetImageTag l_lngImage, IdentifierTagIndex, RTIdentifierTag
    SetImageWidth l_lngImage, Parameters.Width
    SetImageHeight l_lngImage, Parameters.Height
    SetImagePitch l_lngImage, 0
    SetImagePointer l_lngImage, 0
    SetImageLocked l_lngImage, True
    Debug.Print "Allocated Render Target :" & l_lngImage
End Function
' OK

' OK
Public Function o_Allocate_Device(ByRef Parameters As Image_Allocate_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_unkDevice As IShellFolderEx_TLB.IUnknown
Dim l_lngDevice As Long
Dim l_lngImage As Long
    ' Allocate overrides always consume the operation
    o_Allocate_Device = 1
    If (Parameters.Width < 1) Then Exit Function
    If (Parameters.Height < 1) Then Exit Function
    If (Parameters.Image = 0) Then Exit Function
    l_lngImage = Parameters.Image
    Set l_devDevice = CreateDevice(Parameters.Width, Parameters.Height)
    If l_devDevice Is Nothing Then
    Else
        Set l_unkDevice = l_devDevice
        l_unkDevice.AddRef
    '    l_unkDevice.AddRef
    '    l_unkDevice.AddRef
        Set l_unkDevice = Nothing
    End If
    CopyMemory l_lngDevice, l_devDevice, 4
    SetImageTag l_lngImage, RenderTargetTagIndex, ObjPtr(l_devDevice.GetRenderTarget)
    SetImageTag l_lngImage, DeviceTagIndex, l_lngDevice
    SetImageTag l_lngImage, IdentifierTagIndex, IdentifierTag
    SetImageWidth l_lngImage, Parameters.Width
    SetImageHeight l_lngImage, Parameters.Height
    SetImagePitch l_lngImage, 0
    SetImagePointer l_lngImage, 0
    SetImageLocked l_lngImage, True
    Debug.Print "Allocated Device: " & l_lngImage
End Function
' OK

' OK
Public Function o_Deallocate(ByRef Parameters As Image_Allocate_Parameters) As Long
On Error Resume Next
Dim l_lngTag As Long
Dim l_texTexture As Direct3DTexture8
Dim l_unkTexture As IShellFolderEx_TLB.IUnknown
Dim l_devDevice As Direct3DDevice8
Dim l_unkDevice As IShellFolderEx_TLB.IUnknown
Dim l_booHandled As Boolean
    o_Deallocate = 0
    If (Parameters.Image = 0) Then Exit Function
    l_lngTag = GetImageTag(Parameters.Image, IdentifierTagIndex)
    If l_lngTag = IdentifierTag Then
        l_lngTag = GetImageTag(Parameters.Image, DeviceTagIndex)
        CopyMemory l_devDevice, l_lngTag, 4
        Set l_unkDevice = l_devDevice
        l_lngTag = 0
        CopyMemory l_devDevice, l_lngTag, 4
'        l_unkDevice.Release
        Set l_unkDevice = Nothing
        l_booHandled = True
    ElseIf l_lngTag = RTIdentifierTag Then
        l_lngTag = GetImageTag(Parameters.Image, RenderTargetTagIndex)
        SetImageTag Parameters.Image, TextureTagIndex, 0
        CopyMemory l_texTexture, l_lngTag, 4
        Set l_unkTexture = l_texTexture
        l_lngTag = 0
        CopyMemory l_texTexture, l_lngTag, 4
'        l_unkTexture.Release
        Set l_unkTexture = Nothing
        l_booHandled = True
    End If
    l_lngTag = GetImageTag(Parameters.Image, TextureTagIndex)
    If l_lngTag <> 0 Then
        ' Decrement the reference count
        Debug.Print "Deallocating texture for image " & Parameters.Image
        SetImageTag Parameters.Image, TextureTagIndex, 0
        CopyMemory l_texTexture, l_lngTag, 4
        Set l_unkTexture = l_texTexture
'        l_unkTexture.Release
        Set l_texTexture = Nothing
    End If
    If l_booHandled Then
        SetImageWidth Parameters.Image, 0
        SetImageHeight Parameters.Image, 0
        ClearImageTags Parameters.Image
        o_Deallocate = 1
    End If
End Function
' OK

'Public Function o_ResampleBlit(ByRef Parameters As BlitResample_Parameters) As Long
'On Error Resume Next
'Dim l_lngTag As Long
'Dim l_lngSourceDC As Long, l_lngDestDC As Long
'Dim l_rctDest As Rectangle, l_rctSource As Rectangle
'Dim l_lngROP As Long
'    If (Parameters.SourceImage = 0) Then Exit Function
'    If (Parameters.DestImage = 0) Then Exit Function
'    l_lngTag = GetImageTag(Parameters.SourceImage, IdentifierTagIndex)
'    If l_lngTag = IdentifierTag Then
'        With Parameters.Header
'            If KeyMatches(.KeyPointer, "BlitResample_Normal") Then
'                l_lngROP = vbSrcCopy
'            Else
'                l_lngROP = 0
'            End If
'        End With
'        l_lngSourceDC = GetImageTag(Parameters.SourceImage, 1)
'        DerefRectangle Parameters.DestRectangle, l_rctDest
'        DerefRectangle Parameters.SourceRectangle, l_rctSource
'        If l_lngROP <> 0 Then
'            If GetImageTag(Parameters.DestImage, IdentifierTagIndex) = IdentifierTag Then
'                o_ResampleBlit = 1
'                ' GDI to GDI
'                l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
'                SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
'                With l_rctDest
'                    StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
'                End With
'            ElseIf GetImageDIBHandle(Parameters.DestImage) <> 0 Then
'                o_ResampleBlit = 1
'                ' GDI to DIBSection
'                l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
'                If l_lngDestDC Then
'                    SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
'                    With l_rctDest
'                        StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
'                    End With
'                End If
'            End If
'        End If
'        If GetImageLocked(Parameters.SourceImage) Then
'            o_ResampleBlit = 1
'        End If
'    End If
'End Function
'
'Public Function o_ResampleBlit_Opacity(ByRef Parameters As BlitResample_Parameters) As Long
'On Error Resume Next
'Dim l_lngTag As Long
'Dim l_lngSourceDC As Long, l_lngDestDC As Long
'Dim l_rctDest As Rectangle, l_rctSource As Rectangle
'Dim l_bfBlend As BlendFunction
'    If (Parameters.SourceImage = 0) Then Exit Function
'    If (Parameters.DestImage = 0) Then Exit Function
'    l_lngTag = GetImageTag(Parameters.SourceImage, IdentifierTagIndex)
'    If l_lngTag = IdentifierTag Then
'        With l_bfBlend
'            .SourceConstantAlpha = Parameters.P6
'            .BlendOperation = AlphaBlend_Source_Over
'        End With
'        l_lngSourceDC = GetImageTag(Parameters.SourceImage, 1)
'        DerefRectangle Parameters.DestRectangle, l_rctDest
'        DerefRectangle Parameters.SourceRectangle, l_rctSource
'        If GetImageTag(Parameters.DestImage, IdentifierTagIndex) = IdentifierTag Then
'            o_ResampleBlit_Opacity = 1
'            ' GDI to GDI
'            l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
'            SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
'            With l_rctDest
'                'StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
'                AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, BFToLong(l_bfBlend)
'            End With
'        ElseIf GetImageDIBHandle(Parameters.DestImage) <> 0 Then
'            o_ResampleBlit_Opacity = 1
'            ' GDI to DIBSection
'            l_lngDestDC = GetImageTag(Parameters.DestImage, 1)
'            If l_lngDestDC Then
'                SetStretchBltMode l_lngDestDC, StretchBlt_ColorOnColor
'                With l_rctDest
'                    'StretchBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, l_lngROP
'                    AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, l_rctSource.Left, l_rctSource.Top, l_rctSource.Width, l_rctSource.Height, BFToLong(l_bfBlend)
'                End With
'            End If
'        End If
'        If GetImageLocked(Parameters.SourceImage) Then
'            o_ResampleBlit_Opacity = 1
'        End If
'    End If
'End Function
'
Private Sub g_Blit(ByRef Device As Direct3DDevice8, ByRef Parameters As BlitSimple_Parameters, ByVal TintColor As Long, ByVal FBTex As Boolean)
On Error Resume Next
Dim l_rctParameter As Rectangle, l_rctCopy As Rectangle
Dim l_sngU2 As Single, l_sngV2 As Single
Dim l_sngXR As Single, l_sngYR As Single
Dim l_lngSX As Long, l_lngSY As Long
Dim l_sngSX As Single, l_sngSY As Single
Dim l_sngU(0 To 3) As Single, l_sngV(0 To 3) As Single
Dim l_sdDesc As D3DSURFACE_DESC, l_texTexture As Direct3DTexture8
    Dim l_srfRenderTarget As Direct3DSurface8
    Set l_srfRenderTarget = ChangeRenderTarget(Device, GetImageRenderTargetH(Parameters.Dest))
    DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Dest
    l_rctCopy = l_rctParameter
    l_lngSX = Parameters.SX
    l_lngSY = Parameters.SY
    SoftFX.Clip2D_SimpleRect l_rctParameter, Parameters.Dest, Parameters.Source, l_rctCopy, l_lngSX, l_lngSY
    mdlDX8.UseImageAsTextureH Device, Parameters.Source, 0
    Set l_texTexture = Device.GetTexture(0)
    l_texTexture.GetLevelDesc 0, l_sdDesc
    l_sngXR = 1 / l_sdDesc.Width
    l_sngYR = 1 / l_sdDesc.Height
    l_sngSX = (l_lngSX + 0.5) * l_sngXR
    l_sngSY = (l_lngSY + 0.5) * l_sngYR
    l_sngU2 = (l_sngSX) + (l_rctParameter.Width * l_sngXR)
    l_sngV2 = (l_sngSY) + (l_rctParameter.Height * l_sngYR)
    If FBTex Then
        l_sngU(0) = l_sngSX
        l_sngV(0) = l_sngSY
        l_sngU(1) = l_sngU2
        l_sngV(1) = l_sngV2
        Set l_texTexture = Device.GetTexture(1)
        l_texTexture.GetLevelDesc 0, l_sdDesc
        l_sngXR = 1 / l_sdDesc.Width
        l_sngYR = 1 / l_sdDesc.Height
        l_sngU(2) = (l_rctParameter.Left + 0.5) * l_sngXR
        l_sngV(2) = (l_rctParameter.Top + 0.5) * l_sngYR
        l_sngU(3) = (l_rctParameter.Left + l_rctParameter.Width + 0.5) * l_sngXR
        l_sngV(3) = (l_rctParameter.Top + l_rctParameter.Height + 0.5) * l_sngYR
        mdlDX8.DrawMultiTexturedQuad Device, l_rctParameter.Left, l_rctParameter.Top, l_rctParameter.Width, l_rctParameter.Height, l_sngU, l_sngV, TintColor
    Else
        mdlDX8.DrawTexturedQuad Device, l_rctParameter.Left, l_rctParameter.Top, l_rctParameter.Width, l_rctParameter.Height, l_sngSX, l_sngSY, l_sngU2, l_sngV2, TintColor
    End If
'    ChangeRenderTarget Device, l_srfRenderTarget
End Sub

Private Sub g_Blit_Resample(ByRef Device As Direct3DDevice8, ByRef InParameters As BlitSimple_Parameters, ByVal TintColor As Long, ByVal FBTex As Boolean)
On Error Resume Next
Dim Parameters As BlitResample_Parameters
Dim l_rctDest As Rectangle, l_rctDestCopy As Rectangle, l_rctSource As Rectangle, l_rctSourceCopy As Rectangle
Dim l_sngU2 As Single, l_sngV2 As Single
Dim l_sngXR As Single, l_sngYR As Single
Dim l_sngSX As Single, l_sngSY As Single
Dim l_sngU(0 To 3) As Single, l_sngV(0 To 3) As Single
Dim l_sdDesc As D3DSURFACE_DESC, l_texTexture As Direct3DTexture8
    LSet Parameters = InParameters
    Dim l_srfRenderTarget As Direct3DSurface8
    Set l_srfRenderTarget = ChangeRenderTarget(Device, GetImageRenderTargetH(Parameters.DestImage))
    DerefRectangle Parameters.DestRectangle, l_rctDest, Parameters.DestImage
    DerefRectangle Parameters.SourceRectangle, l_rctSource, Parameters.SourceImage
    l_rctDestCopy = l_rctDest
    l_rctSourceCopy = l_rctSource
    SoftFX.Clip2D_PairedRect l_rctDestCopy, l_rctSourceCopy, Parameters.DestImage, Parameters.SourceImage, l_rctDest, l_rctSource, 0
    mdlDX8.UseImageAsTextureH Device, Parameters.SourceImage, 0
    Set l_texTexture = Device.GetTexture(0)
    l_texTexture.GetLevelDesc 0, l_sdDesc
    l_sngXR = 1 / l_sdDesc.Width
    l_sngYR = 1 / l_sdDesc.Height
    l_sngSX = (l_rctSource.Left + 0.5) * l_sngXR
    l_sngSY = (l_rctSource.Top + 0.5) * l_sngYR
    l_sngU2 = (l_rctSource.Left + l_rctSource.Width + 0.5) * l_sngXR
    l_sngV2 = (l_rctSource.Top + l_rctSource.Height + 0.5) * l_sngYR
    Select Case Parameters.Scaler
    Case GetBilinearScaler
        Device.SetTextureStageState 0, D3DTSS_MINFILTER, D3DTEXF_LINEAR
        Device.SetTextureStageState 0, D3DTSS_MAGFILTER, D3DTEXF_LINEAR
    Case Else
        Device.SetTextureStageState 0, D3DTSS_MINFILTER, D3DTEXF_POINT
        Device.SetTextureStageState 0, D3DTSS_MAGFILTER, D3DTEXF_POINT
    End Select
    If FBTex Then
        l_sngU(0) = l_sngSX
        l_sngV(0) = l_sngSY
        l_sngU(1) = l_sngU2
        l_sngV(1) = l_sngV2
        Set l_texTexture = Device.GetTexture(1)
        l_texTexture.GetLevelDesc 0, l_sdDesc
        l_sngXR = 1 / l_sdDesc.Width
        l_sngYR = 1 / l_sdDesc.Height
        l_sngU(2) = (l_rctDest.Left + 0.5) * l_sngXR
        l_sngV(2) = (l_rctDest.Top + 0.5) * l_sngYR
        l_sngU(3) = (l_rctDest.Left + l_rctDest.Width + 0.5) * l_sngXR
        l_sngV(3) = (l_rctDest.Top + l_rctDest.Height + 0.5) * l_sngYR
        mdlDX8.DrawMultiTexturedQuad Device, l_rctDest.Left, l_rctDest.Top, l_rctDest.Width, l_rctDest.Height, l_sngU, l_sngV, TintColor
    Else
        mdlDX8.DrawTexturedQuad Device, l_rctDest.Left, l_rctDest.Top, l_rctDest.Width, l_rctDest.Height, l_sngSX, l_sngSY, l_sngU2, l_sngV2, TintColor
    End If
'    ChangeRenderTarget Device, l_srfRenderTarget
End Sub

Public Function o_Blit_Additive(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Additive = 1
        SetBlendMode l_devDevice, BlitMode_Additive
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Additive_Opacity")
            End If
            If .Index = l_lngOpacityIndex Then
                l_lngTintColor = F2RGB(Parameters.P6, Parameters.P6, Parameters.P6, 0)
            Else
                l_lngTintColor = F2RGB(255, 255, 255, 0)
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

Public Function o_Blit_Subtractive(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Subtractive = 1
        SetBlendMode l_devDevice, BlitMode_Subtractive
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Subtractive_Opacity")
            End If
            If .Index = l_lngOpacityIndex Then
                l_lngTintColor = F2RGB(Parameters.P6, Parameters.P6, Parameters.P6, 0)
            Else
                l_lngTintColor = F2RGB(255, 255, 255, 0)
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

Public Function o_Blit_Unerase(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Unerase = 1
        SetBlendMode l_devDevice, BlitMode_Unerase
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Unerase_Opacity")
            End If
            If .Index = l_lngOpacityIndex Then
                l_lngTintColor = F2RGB(Parameters.P6, Parameters.P6, Parameters.P6, 0)
            Else
                l_lngTintColor = F2RGB(255, 255, 255, 0)
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

Public Function o_Blit_Erase(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Erase = 1
        SetBlendMode l_devDevice, BlitMode_Erase
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Erase_Opacity")
            End If
            If .Index = l_lngOpacityIndex Then
                l_lngTintColor = F2RGB(Parameters.P6, Parameters.P6, Parameters.P6, 0)
            Else
                l_lngTintColor = F2RGB(255, 255, 255, 0)
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

Public Function o_Blit_Lightmap(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Lightmap = 1
        SetBlendMode l_devDevice, BlitMode_Lightmap
        With Parameters.Header
            Static l_lngOpacityIndex As Long, l_lngRGBOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Lightmap_Opacity")
            End If
            If l_lngRGBOpacityIndex = 0 Then
                l_lngRGBOpacityIndex = GetOverrideIndex("BlitSimple_Lightmap_RGB_Opacity")
            End If
            If .Index = l_lngOpacityIndex Or .Index = l_lngRGBOpacityIndex Then
                l_lngTintColor = F2RGB(255, 255, 255, Parameters.P6)
            End If
        End With
        UseImageAsTextureH l_devDevice, Parameters.Dest, 1
        g_Blit l_devDevice, Parameters, l_lngTintColor, True
        UseImageAsTextureH l_devDevice, 0, 1
    End If
End Function

Public Function o_Blit_Normal(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Normal = 1
        SetBlendMode l_devDevice, BlitMode_Normal
        With Parameters.Header
            Static l_lngOpacityIndex As Long, l_lngResampleIndex As Long, l_lngResampleOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Normal_Opacity")
            End If
            If l_lngResampleIndex = 0 Then
                l_lngResampleIndex = GetOverrideIndex("BlitResample_Normal")
            End If
            If l_lngResampleOpacityIndex = 0 Then
                l_lngResampleOpacityIndex = GetOverrideIndex("BlitResample_Normal_Opacity")
            End If
            If (.Index = l_lngOpacityIndex) Or (.Index = l_lngResampleOpacityIndex) Then
                l_devDevice.SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
                l_devDevice.SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
                l_lngTintColor = F2RGB(255, 255, 255, Parameters.P6)
                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, l_lngTintColor
            End If
        End With
        If (Parameters.Header.Index = l_lngResampleIndex) Or (Parameters.Header.Index = l_lngResampleOpacityIndex) Then
            g_Blit_Resample l_devDevice, Parameters, l_lngTintColor, False
        Else
            g_Blit l_devDevice, Parameters, l_lngTintColor, False
        End If
    End If
End Function

Public Function o_Blit_Normal_Tint(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Normal_Tint = 1
        SetBlendMode l_devDevice, BlitMode_Normal_Tint
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Normal_Tint_Opacity")
            End If
            If .Index = l_lngOpacityIndex Then
                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2RGB(Parameters.P7, Parameters.P7, Parameters.P7, Parameters.P7)
            Else
                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, -1
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

Public Function o_Blit_Multiply(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Multiply = 1
        SetBlendMode l_devDevice, BlitMode_Multiply
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Multiply_Opacity")
            End If
            If .Index = l_lngOpacityIndex Then
                l_lngTintColor = F2RGB(255, 255, 255, 255 - Parameters.P6)
            Else
                l_lngTintColor = F2RGB(255, 255, 255, 0)
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

Public Function o_Blit_Font_SourceAlpha(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_Font_SourceAlpha = 1
        SetBlendMode l_devDevice, BlitMode_Font_SourceAlpha
        With Parameters.Header
            Static l_lngOpacityIndex As Long, l_lngRGBOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_Font_SourceAlpha_Opacity")
            End If
            If l_lngRGBOpacityIndex = 0 Then
                l_lngRGBOpacityIndex = GetOverrideIndex("BlitSimple_Font_SourceAlpha_RGB_Opacity")
            End If
            If .Index = l_lngOpacityIndex Or .Index = l_lngRGBOpacityIndex Then
                l_lngTintColor = SetAlpha(Parameters.P6, 255 - (GetAlpha(Parameters.P6) * Parameters.P7 / 255))
            Else
                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

Public Function o_Blit_SourceAlpha(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_SourceAlpha = 1
        SetBlendMode l_devDevice, BlitMode_SourceAlpha
        With Parameters.Header
            Static l_lngOpacityIndex As Long, l_lngResampleIndex As Long, l_lngResampleOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_SourceAlpha_Opacity")
            End If
            If l_lngResampleIndex = 0 Then
                l_lngResampleIndex = GetOverrideIndex("BlitResample_SourceAlpha")
            End If
            If l_lngResampleOpacityIndex = 0 Then
                l_lngResampleOpacityIndex = GetOverrideIndex("BlitResample_SourceAlpha_Opacity")
            End If
            If (.Index = l_lngOpacityIndex) Or (.Index = l_lngResampleOpacityIndex) Then
                l_lngTintColor = BGRA(255, 255, 255, Parameters.P6)
            End If
        End With
        If (Parameters.Header.Index = l_lngResampleIndex) Or (Parameters.Header.Index = l_lngResampleOpacityIndex) Then
            g_Blit_Resample l_devDevice, Parameters, l_lngTintColor, False
        Else
            g_Blit l_devDevice, Parameters, l_lngTintColor, False
        End If
    End If
End Function

Public Function o_Blit_SourceAlpha_Tint(ByRef Parameters As BlitSimple_Parameters) As Long
On Error Resume Next
Dim l_devDevice As Direct3DDevice8
Dim l_lngTintColor As Long
    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
    If l_devDevice Is Nothing Then
    Else
        l_lngTintColor = -1
        o_Blit_SourceAlpha_Tint = 1
        SetBlendMode l_devDevice, BlitMode_SourceAlpha_Tint
        With Parameters.Header
            Static l_lngOpacityIndex As Long
            If l_lngOpacityIndex = 0 Then
                l_lngOpacityIndex = GetOverrideIndex("BlitSimple_SourceAlpha_Tint_Opacity")
            End If
            If .Index = l_lngOpacityIndex Then
                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2RGB(Parameters.P7, Parameters.P7, Parameters.P7, Parameters.P7)
            Else
                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, -1
            End If
        End With
        g_Blit l_devDevice, Parameters, l_lngTintColor, False
    End If
End Function

'Public Function o_Blit_Normal(ByRef Parameters As BlitSimple_Parameters) As Long
'On Error Resume Next
'Dim l_devDevice As Direct3DDevice8
'Dim l_lngTintColor As Long
'    Set l_devDevice = GetImageDeviceH(Parameters.Dest)
'    If l_devDevice Is Nothing Then
'    Else
'        l_lngTintColor = -1
'        o_Blit = 1
'        DerefRectangle Parameters.Rectangle, l_rctParameter, Parameters.Dest
'        If l_rctParameter.Width > GetImageWidth(Parameters.Source) Then
'            l_rctParameter.Width = GetImageWidth(Parameters.Source)
'        End If
'        If l_rctParameter.Height > GetImageHeight(Parameters.Source) Then
'            l_rctParameter.Height = GetImageHeight(Parameters.Source)
'        End If
'        With Parameters.Header
'            If KeyMatches(.KeyPointer, "BlitSimple_Normal_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_Normal
'                l_lngTintColor = F2RGB(255, 255, 255, Parameters.P6)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_SourceAlpha") Then
'                SetBlendMode l_devDevice, BlitMode_SourceAlpha
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_SourceAlpha_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_SourceAlpha
'                l_lngTintColor = F2RGB(255, 255, 255, Parameters.P6)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_SourceAlphaMatte") Then
'                SetBlendMode l_devDevice, BlitMode_SourceAlpha
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_SourceAlphaMatte_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_SourceAlpha
'                l_lngTintColor = F2RGB(255, 255, 255, Parameters.P6)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_SourceAlpha_Solid_Tint") Then
'                SetBlendMode l_devDevice, BlitMode_SourceAlpha_Tint
'                l_lngTintColor = SetAlpha(Parameters.P6, 0)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_SourceAlpha_Tint") Then
'                SetBlendMode l_devDevice, BlitMode_SourceAlpha_Tint
'                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2White()
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_SourceAlpha_Tint_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_SourceAlpha_Tint
'                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2RGB(Parameters.P7, Parameters.P7, Parameters.P7, Parameters.P7)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Additive") Then
'                SetBlendMode l_devDevice, BlitMode_Additive
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Additive_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_Additive
'                l_lngTintColor = F2RGB(255, 255, 255, Parameters.P6)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Lightmap") Then
'                SetBlendMode l_devDevice, BlitMode_Lightmap
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2White()
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Lightmap_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_Lightmap
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2RGB(Parameters.P6, Parameters.P6, Parameters.P6, Parameters.P6)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Lightmap_RGB") Then
'                SetBlendMode l_devDevice, BlitMode_Lightmap_RGB
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2White()
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Lightmap_RGB_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_Lightmap_RGB
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2RGB(Parameters.P6, Parameters.P6, Parameters.P6, Parameters.P6)
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Font") Then
'                SetBlendMode l_devDevice, BlitMode_Font_SourceAlpha
'                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2White()
'            ElseIf KeyMatches(.KeyPointer, "BlitSimple_Font_Opacity") Then
'                SetBlendMode l_devDevice, BlitMode_Font_SourceAlpha
'                l_lngTintColor = SetAlpha(Parameters.P6, 255 - GetAlpha(Parameters.P6))
'                l_devDevice.SetRenderState D3DRS_TEXTUREFACTOR, F2RGB(Parameters.P7, Parameters.P7, Parameters.P7, Parameters.P7)
'            Else
'                SetBlendMode l_devDevice, BlitMode_Normal
'            End If
'        End With
'        l_sngU1 = (Parameters.SX + 0.5) / NextPowerOf2(GetImageWidth(Parameters.Source))
'        l_sngV1 = (Parameters.SY + 0.5) / NextPowerOf2(GetImageHeight(Parameters.Source))
'        l_sngU2 = (Parameters.SX + l_rctParameter.Width + 0.5) / NextPowerOf2(GetImageWidth(Parameters.Source))
'        l_sngV2 = (Parameters.SY + l_rctParameter.Height + 0.5) / NextPowerOf2(GetImageHeight(Parameters.Source))
'        mdlDX8.UseImageAsTextureH l_devDevice, Parameters.Source
'        mdlDX8.DrawTexturedQuad l_devDevice, l_rctParameter.Left, l_rctParameter.Top, l_rctParameter.Width, l_rctParameter.Height, l_sngU1, l_sngV1, l_sngU2, l_sngV2, l_lngTintColor
'    End If
'End Function

'
'Public Function o_Blit_Opacity(ByRef Parameters As BlitSimple_Parameters) As Long
'On Error Resume Next
'Dim l_lngTag As Long
'Dim l_lngSourceDC As Long, l_lngDestDC As Long
'Dim l_rctDest As Rectangle
'Dim l_bfBlend As BlendFunction
'    If (Parameters.Source = 0) Then Exit Function
'    If (Parameters.Dest = 0) Then Exit Function
'    l_lngTag = GetImageTag(Parameters.Source, IdentifierTagIndex)
'    If l_lngTag = IdentifierTag Then
'        With l_bfBlend
'            .SourceConstantAlpha = Parameters.P6
'            .BlendOperation = AlphaBlend_Source_Over
'        End With
'        l_lngSourceDC = GetImageTag(Parameters.Source, 1)
'        DerefRectangle Parameters.Rectangle, l_rctDest
'        If GetImageTag(Parameters.Dest, IdentifierTagIndex) = IdentifierTag Then
'            o_Blit_Opacity = 1
'            ' GDI to GDI
'            l_lngDestDC = GetImageTag(Parameters.Dest, 1)
'            With l_rctDest
'                'BitBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, l_lngROP
'                AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, .Width, .Height, BFToLong(l_bfBlend)
'            End With
'        ElseIf GetImageDIBHandle(Parameters.Dest) <> 0 Then
'            o_Blit_Opacity = 1
'            ' GDI to DIBSection
'            l_lngDestDC = GetImageTag(Parameters.Dest, 1)
'            If l_lngDestDC Then
'                With l_rctDest
'                    'BitBlt l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, l_lngROP
'                    AlphaBlend l_lngDestDC, .Left, .Top, .Width, .Height, l_lngSourceDC, Parameters.SX, Parameters.SY, .Width, .Height, BFToLong(l_bfBlend)
'                End With
'            End If
'        End If
'        If GetImageLocked(Parameters.Source) Then
'            o_Blit_Opacity = 1
'        End If
'    End If
'End Function

Public Sub InitDX8Override()
On Error Resume Next
    AddOverride "Deallocate", AddressOf o_Deallocate
    AddOverride "FilterSimple_Fill", AddressOf o_Fill
    AddOverride "FilterSimple_Fill_SourceAlpha", AddressOf o_Fill
    AddOverride "FilterSimple_Fill_Additive", AddressOf o_Fill
    AddOverride "FilterSimple_Line", AddressOf o_Line
    AddOverride "FilterSimple_Line_SourceAlpha", AddressOf o_Line
    AddOverride "FilterSimple_Box", AddressOf o_Box
    AddOverride "FilterSimple_Box_SourceAlpha", AddressOf o_Box
    AddOverride "Lock", AddressOf o_Lock
    AddOverride "Unlock", AddressOf o_Unlock
    AddOverride "Clear", AddressOf o_Clear
    AddOverride "Fill", AddressOf o_Clear
    AddOverride "BlitSimple_Normal", AddressOf o_Blit_Normal
    AddOverride "BlitSimple_Normal_Opacity", AddressOf o_Blit_Normal
    AddOverride "BlitResample_Normal", AddressOf o_Blit_Normal
    AddOverride "BlitResample_Normal_Opacity", AddressOf o_Blit_Normal
    AddOverride "BlitSimple_Normal_Tint", AddressOf o_Blit_Normal_Tint
    AddOverride "BlitSimple_Normal_Tint_Opacity", AddressOf o_Blit_Normal_Tint
    AddOverride "BlitSimple_SourceAlpha", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitSimple_SourceAlpha_Opacity", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitResample_SourceAlpha", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitResample_SourceAlpha_Opacity", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitSimple_SourceAlpha_Tint", AddressOf o_Blit_SourceAlpha_Tint
    AddOverride "BlitSimple_SourceAlpha_Solid_Tint", AddressOf o_Blit_SourceAlpha_Tint
    AddOverride "BlitSimple_SourceAlpha_Tint_Opacity", AddressOf o_Blit_SourceAlpha_Tint
    AddOverride "BlitSimple_SourceAlphaMatte", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitSimple_SourceAlphaMatte_Opacity", AddressOf o_Blit_SourceAlpha
    AddOverride "BlitSimple_Additive", AddressOf o_Blit_Additive
    AddOverride "BlitSimple_Additive_Opacity", AddressOf o_Blit_Additive
    AddOverride "BlitSimple_Subtractive", AddressOf o_Blit_Subtractive
    AddOverride "BlitSimple_Subtractive_Opacity", AddressOf o_Blit_Subtractive
    AddOverride "BlitSimple_Unerase", AddressOf o_Blit_Unerase
    AddOverride "BlitSimple_Unerase_Opacity", AddressOf o_Blit_Unerase
    AddOverride "BlitSimple_Erase", AddressOf o_Blit_Erase
    AddOverride "BlitSimple_Erase_Opacity", AddressOf o_Blit_Erase
    AddOverride "BlitSimple_Lightmap", AddressOf o_Blit_Lightmap
    AddOverride "BlitSimple_Lightmap_Opacity", AddressOf o_Blit_Lightmap
    AddOverride "BlitSimple_Lightmap_RGB", AddressOf o_Blit_Lightmap
    AddOverride "BlitSimple_Lightmap_RGB_Opacity", AddressOf o_Blit_Lightmap
    AddOverride "BlitSimple_Multiply", AddressOf o_Blit_Multiply
    AddOverride "BlitSimple_Multiply_Opacity", AddressOf o_Blit_Multiply
'    AddOverride "BlitSimple_Font", AddressOf o_Blit_Font
'    AddOverride "BlitSimple_Font_Opacity", AddressOf o_Blit_Font
    AddOverride "BlitSimple_Font_SourceAlpha", AddressOf o_Blit_Font_SourceAlpha
    AddOverride "BlitSimple_Font_SourceAlpha_Opacity", AddressOf o_Blit_Font_SourceAlpha
    AddOverride "BlitSimple_Font_SourceAlpha_RGB", AddressOf o_Blit_Font_SourceAlpha
    AddOverride "BlitSimple_Font_SourceAlpha_RGB_Opacity", AddressOf o_Blit_Font_SourceAlpha
'    AddOverride "BlitSimple_Merge", AddressOf o_Blit
'    AddOverride "BlitSimple_Merge_Opacity", AddressOf o_Blit
'    AddOverride "BlitResample_Normal", AddressOf o_ResampleBlit
'    AddOverride "BlitResample_Normal_Opacity", AddressOf o_ResampleBlit_Opacity
    AddOverride "FilterSimple_Adjust", AddressOf o_Adjust
'    AddOverride "FilterSimple_Invert_Color", AddressOf o_Invert
'    AddOverride "FilterSimple_Invert_Channel", AddressOf o_Invert
'    AddOverride "FilterSimple_Fill_Channel", AddressOf o_Fill_Channel
'    AddOverride "SetPixel(image, x, y, value)", AddressOf o_SetPixel
    AddOverride "FilterSimple_ConvexPolygon", AddressOf o_ConvexPolygon
    AddOverride "FilterSimple_ConvexPolygon_Textured", AddressOf o_ConvexPolygon_Textured
    AddOverride "FilterSimple_Gradient_Radial", AddressOf o_Gradient_Radial
End Sub

Public Function CreateDX8RenderTarget(ByVal Width As Long, ByVal Height As Long, ByRef Device As Direct3DDevice8) As Fury2Image
On Error Resume Next
    ' Install the allocator
    Set l_devRTDevice = Device
    AddOverride "Allocate", AddressOf o_Allocate_RenderTarget
    Set CreateDX8RenderTarget = F2Image(Width, Height)
    ' Uninstall the allocator
    RemoveOverride "Allocate", AddressOf o_Allocate_RenderTarget
    Set l_devRTDevice = Nothing
End Function

Public Function CreateDX8Device(ByVal Width As Long, ByVal Height As Long) As Fury2Image
On Error Resume Next
    ' Install the allocator
    AddOverride "Allocate", AddressOf o_Allocate_Device
    Set CreateDX8Device = F2Image(Width, Height)
    ' Uninstall the allocator
    RemoveOverride "Allocate", AddressOf o_Allocate_Device
End Function

'Public Function CreateDX8ImageFromFile(ByVal Filename As String) As Fury2Image
'On Error Resume Next
'    ' Install the allocator
'    AddOverride "Allocate(image, width, height)", AddressOf o_Allocate
'    m_lngAllocationBitmap = 0
'    m_lngAllocationBitmap = LoadImage(0, Filename, Image_Bitmap, 0, 0, LR_LOADFROMFILE Or LR_DEFAULTSIZE Or LR_DEFAULTCOLOR)
'    If m_lngAllocationBitmap = 0 Then
'    Else
'        Set CreateGDIImageFromFile = F2Image(1, 1) ' Size will be overridden by the allocator on success
'    End If
'    m_lngAllocationBitmap = 0
'    ' Uninstall the allocator
'    RemoveOverride "Allocate(image, width, height)", AddressOf o_Allocate
'End Function

' OK
Public Function IsImageDX8(ByRef Image As Fury2Image) As Boolean
On Error Resume Next
#If (DEIC <> 1) And (DEI1 <> 1) And (DEI1G <> 1) Then
    IsImageDX8 = (GetImageTag(Image.Handle, IdentifierTagIndex) = IdentifierTag)
#End If
End Function
' OK
