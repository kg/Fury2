VERSION 5.00
Begin VB.Form frmTest 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "SoftFX Override Demonstration"
   ClientHeight    =   3600
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4800
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer tmrRedraw 
      Interval        =   200
      Left            =   75
      Top             =   90
   End
End
Attribute VB_Name = "frmTest"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private m_lngOldBitmap As Long
Dim m_imgBehindTexture As Fury2Image, m_imgInFrontTexture As Fury2Image
Dim m_imgGDISurface As Fury2Image
Dim m_imgText As Fury2Image
Dim m_imgBuffer As Fury2Image
Dim m_imgRenderTarget As Fury2Image
Dim m_lngX As Long, m_lngY As Long
Dim m_varPoly() As Variant

Public Sub Redraw()
On Error Resume Next
Dim l_filFilter As Fury2ConvolutionFilter
Dim l_devDevice As Direct3DDevice8
    F2LockingMode = LockingMode_Default
    Set l_devDevice = GetImageDevice(m_imgBuffer)
    With m_imgBuffer
        Set .ClipRectangle = .Rectangle
        .Clear F2RGB(63, 63, 63, 255)
        .ClippedSetClipRectangle F2Rect(50, 50, 200, 150, False)
        .Fill .Rectangle, F2Black
        .ClippedSetClipRectangle F2Rect(0, 0, 240, 150, False)
        .Fill .Rectangle, F2RGB(255, 0, 0, 255)
        .ClippedSetClipRectangle F2Rect(-50, 40, 100, 100, False)
        .Fill .Rectangle, F2RGB(0, 255, 0, 255)
        If PointInPolygon(m_varPoly, Array(m_lngX, m_lngY)) Then
            .ConvexPolygon m_varPoly, F2RGB(0, 255, 0, 255)
            .Fill F2Rect(m_lngX - 2, m_lngY - 2, 5, 5, False), F2RGB(0, 0, 255, 255)
        Else
            .ConvexPolygon m_varPoly, F2RGB(255, 0, 0, 255)
            .Fill F2Rect(m_lngX - 2, m_lngY - 2, 5, 5, False), F2RGB(0, 0, 255, 255)
        End If
        .Locked = True
        mdlDX8.DrawDeviceToWindow l_devDevice, Me.hWnd
    End With
    Set l_devDevice = Nothing
End Sub

Private Sub Form_Load()
On Error Resume Next
    Randomize Timer
    F2Init
    m_lngDXWindow = Me.hWnd
    mdlDX8.Initialize
    InitDX8Override
    m_varPoly = Array( _
    Array(25, 10), Array(50, 50), Array(10, 50))
'    Set m_imgBuffer = F2DIBSection(320, 240, Me.hDC)
    Set m_imgBuffer = mdlDX8Override.CreateDX8Device(320, 240)
'    Set m_imgRenderTarget = mdlDX8Override.CreateDX8RenderTarget(128, 128, GetImageDevice(m_imgBuffer))
    Set m_imgRenderTarget = F2Image(150, 150)
    Set m_imgBehindTexture = F2LoadImage("J:\test_behind_2.png")
    Set m_imgInFrontTexture = F2LoadImage("J:\windowskin.png")
'    m_imgInFrontTexture.Box m_imgInFrontTexture.Rectangle, F2RGB(255, 255, 255, 0)
'    m_imgInFrontTexture.[Line] Array(0, 0, 63, 0), F2RGB(255, 0, 0, 0)
'    m_imgInFrontTexture.[Line] Array(0, 0, 0, 63), F2RGB(0, 0, 255, 0)
'    InitGDIOverride
'    Set m_imgGDISurface = CreateGDIImageFromFile("J:\test.bmp")
'    Debug.Print "IsGDI: " & IsImageGDI(m_imgGDISurface)
'    Debug.Print "Size: " & m_imgGDISurface.Width & "x" & m_imgGDISurface.Height
    Redraw
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
On Error Resume Next
    m_lngX = X
    m_lngY = Y
End Sub

Private Sub Form_Resize()
On Error Resume Next
    Redraw
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Set m_imgRenderTarget = Nothing
    Set m_imgBuffer = Nothing
    F2Shutdown
End Sub

Private Sub tmrRedraw_Timer()
    Redraw
End Sub
