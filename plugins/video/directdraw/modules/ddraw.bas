Attribute VB_Name = "mdlDirectDraw"
'
' DirectDraw Plugin
'
Global m_DX7 As DirectX7
Global m_DD7 As DirectDraw7
Global m_ddsScreen As DirectDrawSurface7
Global m_ddsFlip As DirectDrawSurface7
Global m_sdScreen As DDSURFACEDESC2
Global m_lngHWnd As Long

Public Sub DDInitScreen(Optional ByVal Fullscreen As Boolean = False, Optional ByVal Width As Long = 0, Optional ByVal Height As Long = 0, Optional ByVal BitDepth As Long = 32, Optional ByVal HardwareFlip As Boolean = False)
On Error Resume Next
Dim l_sdDesc As DDSURFACEDESC2
    If Fullscreen Then
        m_DD7.SetCooperativeLevel m_lngHWnd, DDSCL_EXCLUSIVE Or DDSCL_FULLSCREEN
        m_DD7.SetDisplayMode Width, Height, BitDepth, 0, DDSDM_DEFAULT
    Else
        m_DD7.SetCooperativeLevel m_lngHWnd, DDSCL_NORMAL
    End If
    m_DD7.GetDisplayMode m_sdScreen
    If Width = 0 Then Width = m_sdScreen.lWidth
    If Height = 0 Then Height = m_sdScreen.lHeight
    If HardwareFlip Then
        With l_sdDesc
            .lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
            .ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_FLIP Or DDSCAPS_COMPLEX
            .lBackBufferCount = 1
        End With
    Else
        With l_sdDesc
            .lFlags = DDSD_CAPS
            .ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE
        End With
    End If
    Set m_ddsScreen = m_DD7.CreateSurface(l_sdDesc)
    If HardwareFlip Then
        l_sdDesc.ddsCaps.lCaps = DDSCAPS_BACKBUFFER
        Set m_ddsFlip = m_ddsScreen.GetAttachedSurface(l_sdDesc.ddsCaps)
    End If
End Sub

Public Sub DDUnInitScreen()
On Error Resume Next
    Set m_ddsFlip = Nothing
    Set m_ddsScreen = Nothing
    m_DD7.RestoreDisplayMode
    m_DD7.SetCooperativeLevel m_lngHWnd, DDSCL_NORMAL
End Sub

Public Function DDCreateSurface(Width As Long, Height As Long, Optional ByVal Hardware As Boolean = False) As DirectDrawSurface7
On Error Resume Next
Dim l_ddsSurface As DirectDrawSurface7
Dim l_sdDesc As DDSURFACEDESC2
    With l_sdDesc
        .lWidth = Width
        .lHeight = Height
        .lFlags = DDSD_WIDTH Or DDSD_HEIGHT Or DDSD_CAPS Or DDSD_PIXELFORMAT
        If Hardware Then
            .ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_VIDEOMEMORY
            .ddpfPixelFormat.lRGBBitCount = 32
            .ddpfPixelFormat.lRBitMask = F2RGB(255, 0, 0, 0)
            .ddpfPixelFormat.lGBitMask = F2RGB(0, 255, 0, 0)
            .ddpfPixelFormat.lBBitMask = F2RGB(0, 0, 255, 0)
            .ddpfPixelFormat.lRGBAlphaBitMask = F2RGB(255, 255, 255, 255)
            .ddpfPixelFormat.lFlags = DDPF_RGB
        Else
            .ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN Or DDSCAPS_SYSTEMMEMORY
            .ddpfPixelFormat.lRGBBitCount = 32
            .ddpfPixelFormat.lRBitMask = F2RGB(255, 0, 0, 0)
            .ddpfPixelFormat.lGBitMask = F2RGB(0, 255, 0, 0)
            .ddpfPixelFormat.lBBitMask = F2RGB(0, 0, 255, 0)
            .ddpfPixelFormat.lRGBAlphaBitMask = F2RGB(255, 255, 255, 255)
            .ddpfPixelFormat.lFlags = DDPF_RGB
        End If
    End With
    Set l_ddsSurface = m_DD7.CreateSurface(l_sdDesc)
    Set DDCreateSurface = l_ddsSurface
End Function

Public Sub DDInit(ByVal hwnd As Long)
On Error Resume Next
    m_lngHWnd = hwnd
    Set m_DX7 = New DirectX7
    Set m_DD7 = m_DX7.DirectDrawCreate("")
    m_DD7.GetDisplayMode m_sdScreen
End Sub

Public Sub DDShutdown()
On Error Resume Next
    Set m_DD7 = Nothing
    Set m_DX7 = Nothing
End Sub

Public Function StripParens(ByRef Text As String) As Boolean
On Error Resume Next
    If InStr(Text, "(") >= 0 And InStr(Text, ")") >= 0 Then
        Text = Trim(Text)
        If Left(Text, 1) = "(" And Right(Text, 1) = ")" Then
            Text = Mid(Text, 2, Len(Text) - 2)
            StripParens = True
        End If
    End If
End Function

Public Sub brSwap(ByRef v1 As Long, ByRef v2 As Long)
Dim v3 As Long
    v3 = v1
    v1 = v2
    v2 = v3
End Sub

Public Function vbMax(ParamArray Values() As Variant)
On Error Resume Next
Dim biggestValue, biggestIndex As Long, checkAll As Long
    biggestValue = 0
    For checkAll = LBound(Values) To UBound(Values)
        If Values(checkAll) > biggestValue Then biggestIndex = checkAll: biggestValue = Values(checkAll)
    Next checkAll
    vbMax = biggestValue
End Function

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

