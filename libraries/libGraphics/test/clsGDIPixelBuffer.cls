VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsGDIPixelBuffer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private m_lngPixels() As Long
Private m_lngBitmap As Long

Public Sub Attach(Bitmap As Long)
On Error Resume Next
    ReDim m_lngPixels(0 To GetBitmapWidth(Bitmap) - 1, 0 To GetBitmapHeight(Bitmap) - 1)
    m_lngBitmap = Bitmap
    Call mdlGDI.GetBitmapArrayPtr(Bitmap, Me.Pointer)
End Sub

Public Property Get Pointer() As Long
On Error Resume Next
    If m_lngBitmap = 0 Then Exit Sub
    Pointer = VarPtr(m_lngPixels(0, 0))
End Property

Public Sub Detach()
On Error Resume Next
    If m_lngBitmap = 0 Then Exit Sub
    mdlGDI.BytePtrToBitmap m_lngBitmap, Me.Pointer
    m_lngBitmap = 0
    Erase m_lngPixels
End Sub

Private Sub Class_Initialize()
On Error Resume Next
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Detach
End Sub
