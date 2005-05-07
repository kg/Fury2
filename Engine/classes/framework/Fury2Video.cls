VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Video"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit
Public Image As Fury2Image
Public Filename As String
Private m_infFile As AVI_FILE_INFO
Private m_infStream As AVI_STREAM_INFO
Private m_bihHeader As mAVIDecs.BitmapInfoHeader
Private m_booPlaying As Boolean
Private m_booLoop As Boolean
Private m_lngFrameCount As Long
Private m_dblFrameLength As Double
Private m_dblPlaybackSpeed As Double
Private m_dblLastFrame As Double
Private m_lngFirstFrame As Long
Private m_lngFrame As Long
Private m_lngFile As Long
Private m_lngStream As Long
Private m_lngGetFrameObj As Long
Private m_strCodec As String
Private m_engEngine As Fury2Engine

Friend Sub SetEngine(Engine As Fury2Engine)
On Error Resume Next
    Set m_engEngine = Engine
End Sub

Public Property Get Engine() As Fury2Engine
    Set Engine = m_engEngine
End Property

Public Function Load(ByVal Filename As String) As Boolean
On Error Resume Next
Dim l_lngResult As Long
Dim l_strFilename As String
Dim l_bytFourCC(0 To 3) As Byte
    Load = False
    If m_engEngine Is Nothing Then Exit Function
    Filename = Engine.Filesystem.AutoSelectExtension(Filename, "avi")
    l_strFilename = Engine.Filesystem.File(Filename).GetRealFilename

    'Open the AVI File and get a file interface pointer (m_lngFile)
    l_lngResult = AVIFileOpen(m_lngFile, l_strFilename, OF_SHARE_DENY_WRITE, 0&)
    If l_lngResult <> AVIERR_OK Then GoTo ErrorOut
    If m_lngFile = 0 Then GoTo ErrorOut
 
    'Get the first available video stream (m_lngStream)
    l_lngResult = AVIFileGetStream(m_lngFile, m_lngStream, streamtypeVIDEO, 0)
    If l_lngResult <> AVIERR_OK Then GoTo ErrorOut
    If m_lngStream = 0 Then GoTo ErrorOut
    
    'get the starting position of the stream (some streams may not start simultaneously)
    m_lngFirstFrame = AVIStreamStart(m_lngStream)
    If m_lngFirstFrame = -1 Then GoTo ErrorOut 'this function returns -1 on error
    
    'get the length of video stream in frames
    m_lngFrameCount = AVIStreamLength(m_lngStream)
    If m_lngFrameCount = -1 Then GoTo ErrorOut ' this function returns -1 on error
    
    'get file info struct (UDT)
    l_lngResult = AVIFileInfo(m_lngFile, m_infFile, Len(m_infFile))
    If l_lngResult <> AVIERR_OK Then GoTo ErrorOut
        
    'get stream info struct (UDT)
    l_lngResult = AVIStreamInfo(m_lngStream, m_infStream, Len(m_infStream))
    If l_lngResult <> AVIERR_OK Then GoTo ErrorOut
    
'    'check to see if decoder is installed because if it is missing VFW will mangle the stack :(
'    CopyMemory l_bytFourCC(0), m_infStream.fccHandler, 4
'    m_strCodec = StrConv(l_bytFourCC, vbUnicode)
'    If CheckForRegKey("SYSTEM\CurrentControlSet\Control\MediaResources\icm\vidc." & m_strCodec) Or _
'        CheckForRegKey("SOFTWARE\Microsoft\Windows NT\CurrentVersion\Drivers32") Then
'        Engine.TextOut "Video codec not found: " & m_strCodec
'    End If
    
    m_dblFrameLength = 1 / (CSng(m_infStream.dwRate) / CSng(m_infStream.dwScale))
    
    'set bih attributes which we want GetFrame functions to return
    With m_bihHeader
        .biBitCount = 32
        .biClrImportant = 0
        .biClrUsed = 0
        .biCompression = BI_RGB
        .biHeight = m_infStream.rcFrame.bottom - m_infStream.rcFrame.top
        .biPlanes = 1
        .biSize = 40
        .biWidth = m_infStream.rcFrame.right - m_infStream.rcFrame.left
        .biXPelsPerMeter = 0
        .biYPelsPerMeter = 0
        .biSizeImage = .biWidth * 4 * .biHeight
    End With
    
    m_lngGetFrameObj = AVIStreamGetFrameOpen(m_lngStream, m_bihHeader)
    If m_lngGetFrameObj = 0 Then
        GoTo ErrorOut
    End If
    
    Set Image = F2Image(Abs(m_bihHeader.biWidth), Abs(m_bihHeader.biHeight))
    
    m_lngFrame = 1
    Refresh
    
    Load = True
    Err.Clear
    Exit Function
    
ErrorOut:
    Err.Clear
    Free
End Function

Public Property Get Codec() As String
On Error Resume Next
    Codec = m_strCodec
End Property

Public Property Get Length() As Double
On Error Resume Next
    Length = (m_lngFrameCount + m_lngFirstFrame - 1) * m_dblFrameLength
End Property

Public Property Get FrameLength() As Double
On Error Resume Next
    FrameLength = m_dblFrameLength
End Property

Public Property Get Frame() As Long
On Error Resume Next
    Frame = m_lngFrame
End Property

Public Property Let Frame(ByVal NewFrame As Long)
On Error Resume Next
    m_lngFrame = NewFrame
    Refresh
End Property

Public Property Get Time() As Double
On Error Resume Next
    Time = (m_lngFrame + m_lngFirstFrame - 1) * m_dblFrameLength
End Property

Public Property Get FrameCount() As Long
On Error Resume Next
    FrameCount = m_lngFrameCount
End Property

Public Property Get PlaybackSpeed() As Double
On Error Resume Next
    PlaybackSpeed = m_dblPlaybackSpeed
End Property

Public Property Let PlaybackSpeed(ByVal NewValue As Double)
On Error Resume Next
    m_dblPlaybackSpeed = NewValue
End Property

Public Property Get Playing() As Boolean
On Error Resume Next
    Playing = m_booPlaying
End Property

Public Property Get Looping() As Boolean
On Error Resume Next
    Looping = m_booLoop
End Property

Public Property Let Looping(ByVal NewValue As Boolean)
On Error Resume Next
    m_booLoop = NewValue
End Property

Friend Sub Update()
On Error Resume Next
Dim l_dblFrameLength As Double
Dim l_dblElapsedFrames As Double
Dim l_lngElapsedFrames As Long
Dim l_dblDelta As Double
Dim l_dblNow As Double
Dim l_booUpdate As Boolean
    If m_lngFile = 0 Then Exit Sub
    If m_lngStream = 0 Then Exit Sub
    If m_booPlaying Then
        If m_dblPlaybackSpeed <= 0 Then Exit Sub
        If m_dblPlaybackSpeed > 16 Then m_dblPlaybackSpeed = 16
        l_dblFrameLength = (m_dblFrameLength / m_dblPlaybackSpeed)
        l_dblNow = Engine.HiTimer
        l_dblDelta = l_dblNow - m_dblLastFrame
        l_dblElapsedFrames = l_dblDelta / l_dblFrameLength
        l_lngElapsedFrames = Floor(l_dblElapsedFrames)
        If l_lngElapsedFrames > 0 Then
            m_lngFrame = m_lngFrame + l_lngElapsedFrames
            m_dblLastFrame = m_dblLastFrame + (l_dblFrameLength * CDbl(l_lngElapsedFrames))
            If m_booLoop Then
                m_lngFrame = WrapValue(m_lngFrame, 1, m_lngFrameCount)
            Else
                If m_lngFrame <= 1 Or m_lngFrame >= m_lngFrameCount Then
                    Pause
                End If
            End If
            l_booUpdate = True
        End If
        If l_booUpdate Then Refresh
    End If
    Err.Clear
End Sub

Public Sub Refresh()
On Error Resume Next
    m_lngFrame = ClipValue(m_lngFrame, 1, m_lngFrameCount)
    ReadFrame m_lngFrame
End Sub

Friend Sub ReadFrame(ByVal Index As Long)
On Error Resume Next
Dim l_lngDIB As Long
Dim l_bihHeader As mAVIDecs.BitmapInfoHeader
    If m_lngFile = 0 Then Exit Sub
    If m_lngStream = 0 Then Exit Sub
    If m_lngGetFrameObj = 0 Then Exit Sub
    l_lngDIB = AVIStreamGetFrame(m_lngGetFrameObj, Index + m_lngFirstFrame - 1)
    
    CopyMemory ByVal VarPtr(m_bihHeader.biSize), ByVal l_lngDIB, Len(m_bihHeader)
    If m_bihHeader.biBitCount < 32 Then Exit Sub
    If m_bihHeader.biSizeImage < 1 Then Exit Sub
    
    CopyMemory ByVal Image.Pointer(0, 0), ByVal l_lngDIB + 40, m_bihHeader.biSizeImage
    Image.Flip
    SoftFX.SetImageDirty Image.Handle, 1
    Err.Clear
End Sub

Public Sub Play(Optional ByVal Speed As Double = 1)
On Error Resume Next
    Frame = 1
    m_dblLastFrame = Engine.HiTimer
    m_dblPlaybackSpeed = Speed
    m_booPlaying = True
End Sub

Public Sub Pause()
On Error Resume Next
    m_booPlaying = False
End Sub

Public Sub Free()
On Error Resume Next
    If m_lngGetFrameObj <> 0 Then
        Call AVIStreamGetFrameClose(m_lngGetFrameObj)
        m_lngGetFrameObj = 0
    End If
    If m_lngStream <> 0 Then
        Call AVIStreamRelease(m_lngStream)
        m_lngStream = 0
    End If
    If m_lngFile <> 0 Then
        Call AVIFileRelease(m_lngFile)
        m_lngFile = 0
    End If
    Err.Clear
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Free
End Sub
