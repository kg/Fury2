Attribute VB_Name = "mdlDX8"
Option Explicit
Public Type D3DVertex
    X As Single
    Y As Single
    Z As Single
    RHW As Single
    Color As Long
    U1 As Single
    V1 As Single
    U2 As Single
    V2 As Single
End Type

Public m_dxDX8 As DirectX8
Public m_dxD3D8 As Direct3D8
Public m_lngAdapter As Long
Public m_lngDXWindow As Long

Public Function SetBlendMode(ByRef Device As Direct3DDevice8, ByVal Mode As SFXBlitModes)
On Error Resume Next
    With Device
        .SetRenderState D3DRS_ALPHABLENDENABLE, 1
        .SetRenderState D3DRS_BLENDOP, D3DBLENDOP_ADD
        .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE
        .SetTextureStageState 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE
        .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE
        .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_MODULATE
        .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
        .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
        .SetTextureStageState 1, D3DTSS_COLOROP, D3DTOP_DISABLE
        .SetTextureStageState 1, D3DTSS_ALPHAOP, D3DTOP_DISABLE
        Select Case Mode
        Case BlitMode_Normal
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_ZERO
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_ONE
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG2
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_TFACTOR
        Case BlitMode_Additive
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_ONE
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_ONE
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_DISABLE
        Case BlitMode_Subtractive
            .SetRenderState D3DRS_BLENDOP, D3DBLENDOP_REVSUBTRACT
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_ONE
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_ONE
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_DISABLE
        Case BlitMode_SourceAlpha
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
        Case BlitMode_SourceAlpha_Tint
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_BLENDDIFFUSEALPHA
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_TFACTOR
        Case BlitMode_Lightmap, BlitMode_Lightmap_RGB
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_SELECTARG1
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG2
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE
            .SetTextureStageState 1, D3DTSS_COLOROP, D3DTOP_MODULATE2X
            .SetTextureStageState 1, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 1, D3DTSS_COLORARG2, D3DTA_CURRENT
            .SetTextureStageState 1, D3DTSS_ALPHAOP, D3DTOP_DISABLE
        Case BlitMode_Multiply
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_ZERO
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_BLENDDIFFUSEALPHA
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_DISABLE
        Case BlitMode_Font_SourceAlpha
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE
        Case BlitMode_Normal_Tint
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_BLENDDIFFUSEALPHA
            .SetTextureStageState 0, D3DTSS_COLORARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG2
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_TFACTOR
        Case BlitMode_Unerase
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_ONE
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_ONE
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_DISABLE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE
        Case BlitMode_Erase
            .SetRenderState D3DRS_BLENDOP, D3DBLENDOP_REVSUBTRACT
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_ONE
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_ONE
            .SetTextureStageState 0, D3DTSS_COLOROP, D3DTOP_DISABLE
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE
            .SetTextureStageState 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE
        Case Else
            .SetRenderState D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA
            .SetRenderState D3DRS_SRCBLEND, D3DBLEND_SRCALPHA
            .SetTextureStageState 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG2
            .SetTextureStageState 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE
        End Select
    End With
End Function

Public Function GetImageRenderTargetH(ByVal Image As Long) As Direct3DSurface8
On Error Resume Next
Dim l_unkTarget As IShellFolderEx_TLB.IUnknown
Dim l_srfTarget As Direct3DSurface8
Dim l_lngTag As Long
    If Image = 0 Then
    Else
        l_lngTag = GetImageTag(Image, IdentifierTagIndex)
        If (l_lngTag = IdentifierTag) Or (l_lngTag = RTIdentifierTag) Then
            l_lngTag = GetImageTag(Image, RenderTargetTagIndex)
            CopyMemory l_srfTarget, l_lngTag, 4
            Set GetImageRenderTargetH = l_srfTarget
            Set l_unkTarget = l_srfTarget
            l_unkTarget.AddRef
            Set l_unkTarget = Nothing
            l_lngTag = 0
            CopyMemory l_srfTarget, l_lngTag, 4
        End If
    End If
End Function

Public Function GetImageRenderTarget(ByRef Image As Fury2Image) As Direct3DSurface8
On Error Resume Next
    If Image Is Nothing Then
    Else
        Set GetImageRenderTarget = GetImageRenderTargetH(Image.Handle)
    End If
End Function

Public Function GetImageDeviceH(ByVal Image As Long) As Direct3DDevice8
On Error Resume Next
Dim l_unkDevice As IShellFolderEx_TLB.IUnknown
Dim l_devDevice As Direct3DDevice8
Dim l_lngTag As Long
    If Image = 0 Then
    Else
        l_lngTag = GetImageTag(Image, IdentifierTagIndex)
        If (l_lngTag = IdentifierTag) Or (l_lngTag = RTIdentifierTag) Then
            l_lngTag = GetImageTag(Image, DeviceTagIndex)
            CopyMemory l_devDevice, l_lngTag, 4
            Set GetImageDeviceH = l_devDevice
            Set l_unkDevice = l_devDevice
            l_unkDevice.AddRef
            Set l_unkDevice = Nothing
            l_lngTag = 0
            CopyMemory l_devDevice, l_lngTag, 4
        End If
    End If
End Function

Public Function GetImageDevice(ByRef Image As Fury2Image) As Direct3DDevice8
On Error Resume Next
    If Image Is Nothing Then
    Else
        Set GetImageDevice = GetImageDeviceH(Image.Handle)
    End If
End Function

Public Sub DrawDeviceToWindow(ByRef Device As Direct3DDevice8, ByVal hWnd As Long)
On Error Resume Next
    Device.EndScene
    Device.Present ByVal 0, ByVal 0, hWnd, ByVal 0
    Device.BeginScene
End Sub

Public Sub UseImageAsTexture(ByRef Device As Direct3DDevice8, ByRef Image As Fury2Image, ByVal Stage As Long)
On Error Resume Next
    If Image Is Nothing Then
        UseImageAsTextureH Device, 0, Stage
    Else
        UseImageAsTextureH Device, Image.Handle, Stage
    End If
End Sub

Public Sub UseImageAsTextureH(ByRef Device As Direct3DDevice8, ByVal Image As Long, ByVal Stage As Long)
On Error Resume Next
Dim l_lngTexture As Long
Dim l_texTexture As Direct3DTexture8
Dim l_unkTexture As IShellFolderEx_TLB.IUnknown
Dim l_booSame As Boolean
    If Image = 0 Then
        Device.SetTexture Stage, Nothing
    Else
        l_lngTexture = GetImageTag(Image, 5)
        If l_lngTexture = 0 Then
            Set l_texTexture = CreateTextureFromImageH(Device, Image)
            CopyMemory l_lngTexture, l_texTexture, 4
            Set l_unkTexture = l_texTexture
            l_unkTexture.AddRef
            Set l_unkTexture = Nothing
            Call SetImageTag(Image, 5, l_lngTexture)
            SetImageDirty Image, 0
        Else
            Set l_texTexture = Nothing
            CopyMemory l_texTexture, l_lngTexture, 4
            ' This shouldn't be necessary, but it is.
            ' I think either D3D8 or VB is doing a Release() somewhere that it shouldn't be
            Set l_unkTexture = l_texTexture
            l_unkTexture.AddRef
            Set l_unkTexture = Nothing
            If GetImageTag(Image, IdentifierTagIndex) = IdentifierTag Then
                ' Device as texture
                CopyImageToTextureH l_texTexture, Image
                SetImageDirty Image, 0
                l_booSame = False
            ElseIf GetImageTag(Image, IdentifierTagIndex) = IdentifierTag Then
                ' Render Target as texture
                CopyImageToTextureH l_texTexture, Image
                SetImageDirty Image, 0
                l_booSame = False
            ElseIf (GetImageDirty(Image) <> 0) Then
                ' Image has changed since the texture was last uploaded. Reupload!
                CopyImageToTextureH l_texTexture, Image
                SetImageDirty Image, 0
                l_booSame = False
            End If
        End If
        ProfileStart "Texture Switches"
        Device.SetTexture Stage, l_texTexture
        ProfileStop "Texture Switches"
        l_lngTexture = 0
        CopyMemory l_texTexture, l_lngTexture, 4
        CopyMemory l_unkTexture, l_lngTexture, 4
    End If
End Sub

Public Sub InitQuad(ByRef Vertexes() As D3DVertex, ByVal x1 As Long, ByVal y1 As Long, ByVal x2 As Long, ByVal y2 As Long)
On Error Resume Next
    With Vertexes(0)
        .X = x1: .Y = y1
        .RHW = 1#
    End With
    With Vertexes(1)
        .X = x2: .Y = y1
        .RHW = 1#
    End With
    With Vertexes(2)
        .X = x1: .Y = y2
        .RHW = 1#
    End With
    With Vertexes(3)
        .X = x2: .Y = y1
        .RHW = 1#
    End With
    With Vertexes(4)
        .X = x2: .Y = y2
        .RHW = 1#
    End With
    With Vertexes(5)
        .X = x1: .Y = y2
        .RHW = 1#
    End With
End Sub

Public Sub InitQuadUV(ByRef Vertexes() As D3DVertex, ByVal U1 As Single, ByVal V1 As Single, ByVal U2 As Single, ByVal V2 As Single)
On Error Resume Next
    With Vertexes(0)
        .U1 = U1: .V1 = V1
        .U2 = U1: .V2 = V1
    End With
    With Vertexes(1)
        .U1 = U2: .V1 = V1
        .U2 = U2: .V2 = V1
    End With
    With Vertexes(2)
        .U1 = U1: .V1 = V2
        .U2 = U1: .V2 = V2
    End With
    With Vertexes(3)
        .U1 = U2: .V1 = V1
        .U2 = U2: .V2 = V1
    End With
    With Vertexes(4)
        .U1 = U2: .V1 = V2
        .U2 = U2: .V2 = V2
    End With
    With Vertexes(5)
        .U1 = U1: .V1 = V2
        .U2 = U1: .V2 = V2
    End With
End Sub

Public Sub InitQuadUVMT(ByRef Vertexes() As D3DVertex, ByRef U() As Single, ByRef V() As Single)
On Error Resume Next
    With Vertexes(0)
        .U1 = U(0): .V1 = V(0)
        .U2 = U(2): .V2 = V(2)
    End With
    With Vertexes(1)
        .U1 = U(1): .V1 = V(0)
        .U2 = U(3): .V2 = V(2)
    End With
    With Vertexes(2)
        .U1 = U(0): .V1 = V(1)
        .U2 = U(2): .V2 = V(3)
    End With
    With Vertexes(3)
        .U1 = U(1): .V1 = V(0)
        .U2 = U(3): .V2 = V(2)
    End With
    With Vertexes(4)
        .U1 = U(1): .V1 = V(1)
        .U2 = U(3): .V2 = V(3)
    End With
    With Vertexes(5)
        .U1 = U(0): .V1 = V(1)
        .U2 = U(2): .V2 = V(3)
    End With
End Sub

Public Sub InitQuadColor(ByRef Vertexes() As D3DVertex, Optional ByVal ColorTL As Long = -1, Optional ByVal ColorTR As Long = -1, Optional ByVal ColorBL As Long = -1, Optional ByVal ColorBR As Long = -1)
On Error Resume Next
    With Vertexes(0)
        .Color = ColorTL
    End With
    With Vertexes(1)
        .Color = ColorTR
    End With
    With Vertexes(2)
        .Color = ColorBL
    End With
    With Vertexes(3)
        .Color = ColorTR
    End With
    With Vertexes(4)
        .Color = ColorBR
    End With
    With Vertexes(5)
        .Color = ColorBL
    End With
End Sub

Public Function NextPowerOf2(ByVal Number As Long) As Long
On Error Resume Next
    NextPowerOf2 = 2 ^ Ceil(Log(Number) / Log(2))
End Function

Public Sub CopyImageToTextureH(ByRef Texture As Direct3DTexture8, ByVal Image As Long)
On Error Resume Next
Dim l_rctLock As D3DLOCKED_RECT
Dim l_rctLockArea As DxVBLibA.Rect
Dim l_lngRows As Long
Dim l_lngRowCount As Long, l_lngRowSize As Long
Dim l_booLocked As Boolean
Dim l_lngWidth As Long, l_lngHeight As Long
Dim l_sdDesc As D3DSURFACE_DESC
Dim l_devDevice As Direct3DDevice8
Dim l_srfTarget As Direct3DSurface8
    Set l_devDevice = GetImageDeviceH(Image)
    Set l_srfTarget = GetImageRenderTargetH(Image)
    If l_devDevice Is Nothing Then
        ' Image
        ProfileStart "Texture Uploads"
        Texture.GetLevelDesc 0, l_sdDesc
        l_lngWidth = l_sdDesc.Width
        l_lngHeight = l_sdDesc.Height
        With l_rctLockArea
            .Left = 0
            .Top = 0
            .Right = l_lngWidth
            .Bottom = l_lngHeight
        End With
        Err.Clear
        Texture.LockRect 0, l_rctLock, l_rctLockArea, D3DLOCK_DISCARD
        If l_rctLock.pBits Then
            l_booLocked = GetImageLocked(Image) <> 0
            SetImageLocked Image, 0
            If GetImagePointer(Image, 0, 0) Then
                l_lngRowCount = GetImageHeight(Image)
                l_lngRowSize = GetImageWidth(Image) * 4
                For l_lngRows = 0 To l_lngRowCount - 1
                    CopyMemory ByVal (l_rctLock.pBits + (l_rctLock.Pitch * l_lngRows)), ByVal GetImagePointer(Image, 0, l_lngRows), l_lngRowSize
                Next l_lngRows
            End If
            SetImageLocked Image, CLng(Abs(l_booLocked))
        End If
        Texture.UnlockRect 0
        ProfileStop "Texture Uploads"
    ElseIf l_srfTarget Is l_devDevice.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO) Then
        ' Device
        ProfileStart "Texture Copies"
        l_devDevice.EndScene
        l_devDevice.CopyRects l_devDevice.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO), ByVal 0, 0, Texture.GetSurfaceLevel(0), ByVal 0
        l_devDevice.BeginScene
        ProfileStop "Texture Copies"
    Else
        ' Render Target
    End If
End Sub

Public Sub CopyImageToTexture(ByRef Texture As Direct3DTexture8, ByRef Image As Fury2Image)
On Error Resume Next
    CopyImageToTextureH Texture, Image.Handle
End Sub

Public Function CreateTextureFromImage(ByRef Device As Direct3DDevice8, ByRef Image As Fury2Image) As Direct3DTexture8
On Error Resume Next
    Set CreateTextureFromImage = CreateTextureFromImageH(Device, Image.Handle)
End Function

Public Function CreateTextureFromImageH(ByRef Device As Direct3DDevice8, ByVal Image As Long) As Direct3DTexture8
On Error Resume Next
Dim l_texTexture As Direct3DTexture8
Dim l_lngWidth As Long, l_lngHeight As Long
    l_lngWidth = NextPowerOf2(GetImageWidth(Image))
    l_lngHeight = NextPowerOf2(GetImageHeight(Image))
    Err.Clear
    ProfileStart "Texture Allocation"
    Set l_texTexture = Device.CreateTexture(l_lngWidth, l_lngHeight, 0, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED)
    ProfileStop "Texture Allocation"
    If Not (l_texTexture Is Nothing) Then
        CopyImageToTextureH l_texTexture, Image
    End If
    Set CreateTextureFromImageH = l_texTexture
End Function

Public Function MakeVertex(ByVal X As Single, ByVal Y As Single, Optional ByVal Z As Single = 1#, Optional ByVal Color As Long = -1, Optional ByVal U As Single = 0, Optional ByVal V As Single = 0) As D3DVertex
On Error Resume Next
    With MakeVertex
        .X = X
        .Y = Y
        .Z = Z
        .RHW = 1
        .Color = Color
        .U1 = U
        .V1 = V
    End With
End Function

Public Sub DrawTexturedQuad(ByRef Device As Direct3DDevice8, ByVal Left As Long, ByVal Top As Long, ByVal Width As Long, ByVal Height As Long, ByVal U1 As Single, ByVal V1 As Single, ByVal U2 As Single, ByVal V2 As Single, ByVal Color As Long)
On Error Resume Next
Dim l_verVertexes(0 To 5) As D3DVertex
Dim l_lngPtr As Long
    ProfileStart "Textured Quad Rendering"
    InitQuad l_verVertexes, Left, Top, Left + Width, Top + Height
    InitQuadUV l_verVertexes, U1, V1, U2, V2
    InitQuadColor l_verVertexes, Color, Color, Color, Color
    Device.DrawPrimitiveUP D3DPT_TRIANGLELIST, 2, ByVal VarPtr(l_verVertexes(0)), Len(l_verVertexes(0))
    ProfileStop "Textured Quad Rendering"
End Sub

Public Sub DrawMultiTexturedQuad(ByRef Device As Direct3DDevice8, ByVal Left As Long, ByVal Top As Long, ByVal Width As Long, ByVal Height As Long, ByRef U() As Single, ByRef V() As Single, ByVal Color As Long)
On Error Resume Next
Dim l_verVertexes(0 To 5) As D3DVertex
Dim l_lngPtr As Long
    ProfileStart "Textured Quad Rendering"
    InitQuad l_verVertexes, Left, Top, Left + Width, Top + Height
    InitQuadUVMT l_verVertexes, U, V
    InitQuadColor l_verVertexes, Color, Color, Color, Color
    Device.DrawPrimitiveUP D3DPT_TRIANGLELIST, 2, ByVal VarPtr(l_verVertexes(0)), Len(l_verVertexes(0))
    ProfileStop "Textured Quad Rendering"
End Sub

Public Sub DrawFilledRect(ByRef Device As Direct3DDevice8, ByVal Left As Long, ByVal Top As Long, ByVal Width As Long, ByVal Height As Long, ByVal Color As Long)
On Error Resume Next
Dim l_verVertexes(0 To 5) As D3DVertex
    ProfileStart "Filled Quad Rendering"
    InitQuad l_verVertexes, Left, Top, Left + Width, Top + Height
    InitQuadColor l_verVertexes, Color, Color, Color, Color
    Device.DrawPrimitiveUP D3DPT_TRIANGLELIST, 2, ByVal VarPtr(l_verVertexes(0)), Len(l_verVertexes(0))
    ProfileStop "Filled Quad Rendering"
End Sub

Public Function ChangeRenderTarget(ByRef Device As Direct3DDevice8, ByRef NewTarget As Direct3DSurface8) As Direct3DSurface8
On Error Resume Next
    Set ChangeRenderTarget = Device.GetRenderTarget()
    If ChangeRenderTarget Is NewTarget Then
    Else
        Device.EndScene
        Device.SetRenderTarget NewTarget, Nothing, 0
        Device.BeginScene
    End If
End Function

Public Function CreateRenderTarget(ByVal Width As Long, ByVal Height As Long, ByRef Device As Direct3DDevice8) As Direct3DTexture8
On Error Resume Next
Dim l_texTexture As Direct3DTexture8
Dim l_lngWidth As Long, l_lngHeight As Long
    l_lngWidth = NextPowerOf2(Width)
    l_lngHeight = NextPowerOf2(Height)
    Err.Clear
    ProfileStart "Texture Allocation"
    Set l_texTexture = Device.CreateTexture(l_lngWidth, l_lngHeight, 1, D3DUSAGE_RENDERTARGET, D3DFMT_A8R8G8B8, D3DPOOL_DEFAULT)
    ProfileStop "Texture Allocation"
    Set CreateRenderTarget = l_texTexture
End Function

Public Function CreateDevice(ByVal Width As Long, ByVal Height As Long, Optional ByVal hWnd As Long = -1) As Direct3DDevice8
On Error Resume Next
Dim l_dmDisplayMode As D3DDISPLAYMODE
Dim l_ppPresent As D3DPRESENT_PARAMETERS
Dim l_verVertex As D3DVertex
    If m_dxD3D8 Is Nothing Then Exit Function
    Call m_dxD3D8.GetAdapterDisplayMode(m_lngAdapter, l_dmDisplayMode)
    With l_ppPresent
        .Windowed = True
        .BackBufferWidth = Width
        .BackBufferHeight = Height
        .BackBufferCount = 0
        .BackBufferFormat = D3DFMT_A8R8G8B8
        .SwapEffect = D3DSWAPEFFECT_DISCARD
        .MultiSampleType = D3DMULTISAMPLE_NONE
        .flags = D3DPRESENTFLAG_LOCKABLE_BACKBUFFER
    End With
    If hWnd = -1 Then hWnd = m_lngDXWindow
    Set CreateDevice = m_dxD3D8.CreateDevice(m_lngAdapter, D3DDEVTYPE_HAL, hWnd, D3DCREATE_SOFTWARE_VERTEXPROCESSING, l_ppPresent)
    If CreateDevice Is Nothing Then Exit Function
    With CreateDevice
        .SetRenderState D3DRS_CULLMODE, D3DCULL_NONE
        .SetRenderState D3DRS_ZENABLE, 0
        .SetRenderState D3DRS_LIGHTING, 0
        .SetTextureStageState 0, D3DTSS_MINFILTER, D3DTEXF_LINEAR
        .SetTextureStageState 0, D3DTSS_MAGFILTER, D3DTEXF_LINEAR
        .SetVertexShader D3DFVF_TEX1 Or D3DFVF_TEX2 Or D3DFVF_XYZRHW Or D3DFVF_DIFFUSE
        .BeginScene
    End With
End Function

Public Sub Initialize()
On Error Resume Next
    m_lngAdapter = D3DADAPTER_DEFAULT
    Set m_dxDX8 = New DirectX8
    Set m_dxD3D8 = m_dxDX8.Direct3DCreate()
End Sub

Public Sub Uninitialize()
On Error Resume Next
    Set m_dxD3D8 = Nothing
    Set m_dxDX8 = Nothing
End Sub
