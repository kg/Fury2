VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cSplitter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Private Type POINTAPI
   X As Long
   Y As Long
End Type
Private Type RECT
   Left As Long
   Top As Long
   Right As Long
   Bottom As Long
End Type
Private Type BITMAP '24 bytes
   bmType As Long
   bmWidth As Long
   bmHeight As Long
   bmWidthBytes As Long
   bmPlanes As Integer
   bmBitsPixel As Integer
   bmBits As Long
End Type

Private Const IDC_SIZENS = 32645&
Private Const IDC_SIZEWE = 32644&
Private Const IDC_NO = 32648&

Private Const R2_NOTXORPEN = 10  '  DPxn

Private Const PATINVERT = &H5A0049       ' (DWORD) dest = pattern XOR dest
Private Const DSTINVERT = &H550009       ' (DWORD) dest = (NOT dest)

Private Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Private Declare Function ScreenToClient Lib "user32" (ByVal hwnd As Long, lpPoint As POINTAPI) As Long
Private Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Private Declare Sub ClipCursorRect Lib "user32" Alias "ClipCursor" (lpRect As RECT)
Private Declare Sub ClipCursorClear Lib "user32" Alias "ClipCursor" (ByVal lpRect As Long)
Private Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function CreateDCAsNull Lib "gdi32" Alias "CreateDCA" (ByVal lpDriverName As String, lpDeviceName As Any, lpOutput As Any, lpInitData As Any) As Long
Private Declare Function SetROP2 Lib "gdi32" (ByVal hdc As Long, ByVal nDrawMode As Long) As Long
Private Declare Function CreateBitmapIndirect Lib "gdi32" (lpBitmap As BITMAP) As Long
Private Declare Function CreatePatternBrush Lib "gdi32" (ByVal hBitmap As Long) As Long
Private Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Private Declare Function FillRect Lib "user32" (ByVal hdc As Long, lpRect As RECT, ByVal hBrush As Long) As Long
Private Declare Function PatBlt Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal dwRop As Long) As Long
Private Declare Function LoadCursorLong Lib "user32" Alias "LoadCursorA" (ByVal hInstance As Long, ByVal lpCursorName As Long) As Long
Private Declare Function DestroyCursor Lib "user32" (ByVal hCursor As Long) As Long
Private Declare Function SetCursor Lib "user32" (ByVal hCursor As Long) As Long

Public Enum ESPLTOrientationConstants
    cSPLTOrientationHorizontal = 1
    cSPLTOrientationVertical = 2
End Enum

Public Enum ESPLTPanelConstants
   cSPLTLeftOrTopPanel = 1
   cSPLTRightOrBottomPanel = 2
End Enum

Private m_bKeepProportionsWhenResizing As Boolean
Private m_fProportion As Single
Private m_lSplitPos As Long
Private m_lSplitSize As Long
Private m_lMinSize(1 To 2) As Long
Private m_lMaxSize(1 To 2) As Long
Private m_bFullDrag As Boolean
Private m_bInDrag As Boolean
Private m_tPInitial As POINTAPI
Private m_lSplitInitial  As Long
Private m_hBrush As Long
Private m_lPattern(0 To 3) As Long
Private m_tSplitR As RECT
Private m_hCursor As Long

Private m_oContainer As Object
Private m_oLeftTop As Object
Private m_oRightBottom As Object

Private m_eOrientation As ESPLTOrientationConstants

Public Event Split(X As Single, Y As Single, bCancel As Boolean)

Public Property Get FullDrag() As Boolean
   FullDrag = m_bFullDrag
End Property
Public Property Let FullDrag(ByVal bState As Boolean)
   If Not (m_bFullDrag = bState) Then
      m_bFullDrag = bState
      If Not m_bFullDrag Then
         CreateBrush
      Else
         DestroyBrush
      End If
   End If
End Property

Public Property Get Orientation() As ESPLTOrientationConstants
   Orientation = m_eOrientation
End Property
Public Property Let Orientation(ByVal eOrientation As ESPLTOrientationConstants)
   If Not (m_eOrientation = eOrientation) Then
      m_eOrientation = eOrientation
      If Not (m_hCursor = 0) Then
         DestroyCursor m_hCursor
      End If
      If (m_eOrientation = cSPLTOrientationHorizontal) Then
         m_hCursor = LoadCursorLong(0, IDC_SIZENS)
      Else
         m_hCursor = LoadCursorLong(0, IDC_SIZEWE)
      End If
      Resize
   End If
End Property

Public Property Get Proportion() As Single
   If (m_fProportion > 1) Then
      m_fProportion = 1
   End If
   Proportion = m_fProportion * 100
End Property
Public Property Let Proportion(ByVal fProportion As Single)
   If (fProportion > 100#) Or (fProportion < 0#) Then
      Err.Raise 380, App.EXEName & ".cSplitter"
   Else
      m_fProportion = fProportion / 100#
      Resize
   End If
End Property

Public Property Get Position() As Long
   Position = m_lSplitPos
End Property
Public Property Let Position(ByVal lPosition As Long)
   If (lPosition <> m_lSplitPos) Then
      m_lSplitPos = lPosition
      pValidatePosition
      pSetProportion
      Resize
   End If
End Property

Public Property Get KeepProportion() As Boolean
   KeepProportion = m_bKeepProportionsWhenResizing
End Property
Public Property Let KeepProportion(ByVal bState As Boolean)
   m_bKeepProportionsWhenResizing = bState
End Property

Public Property Let Container(oContainer As Object)
   Set m_oContainer = oContainer
End Property
Public Property Get Container() As Object
   Set Container = m_oContainer
End Property

Public Property Get SplitterSize() As Long
   SplitterSize = m_lSplitSize
End Property
Public Property Let SplitterSize(ByVal lSize As Long)
   If Not (m_lSplitSize = lSize) Then
      If (lSize < 0) Then
         Err.Raise 380, App.EXEName & ".cSplitter"
      Else
         m_lSplitSize = lSize
         Resize
      End If
   End If
End Property

Public Property Get MinimumSize( _
      ByVal ePanel As ESPLTPanelConstants _
   ) As Long
   MinimumSize = m_lMinSize(ePanel)
End Property
Public Property Let MinimumSize( _
      ByVal ePanel As ESPLTPanelConstants, _
      ByVal lSize As Long _
   )
   If Not (m_lMinSize(ePanel) = lSize) Then
      m_lMinSize(ePanel) = lSize
      Resize
   End If
End Property

Public Property Get MaximumSize( _
      ByVal ePanel As ESPLTPanelConstants _
   ) As Long
   MaximumSize = m_lMaxSize(ePanel)
End Property
Public Property Let MaximumSize( _
      ByVal ePanel As ESPLTPanelConstants, _
      ByVal lSize As Long _
   )
   If Not (m_lMaxSize(ePanel) = lSize) Then
      m_lMaxSize(ePanel) = lSize
   End If
End Property


Public Sub Bind(oLeftTop As Object, oRightBottom As Object, Optional ByRef Container As Object = Nothing)
   Set m_oContainer = Container
   
   If (m_oContainer Is Nothing) Then
      Set m_oContainer = oLeftTop.Container
   End If
   
   Set m_oLeftTop = oLeftTop
   Set m_oLeftTop.Container = m_oContainer
   Set m_oRightBottom = oRightBottom
   Set m_oRightBottom.Container = m_oContainer
      
   Resize
   
End Sub

Private Function pbConfigured() As Boolean
   If Not m_oContainer Is Nothing Then
      If Not m_oLeftTop Is Nothing Then
         If Not m_oRightBottom Is Nothing Then
            pbConfigured = True
         End If
      End If
   End If
End Function

Public Sub MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If m_oLeftTop.Visible And m_oRightBottom.Visible Then
        If (Button = vbLeftButton) Then
           Dim bCancel As Boolean
           RaiseEvent Split(X, Y, bCancel)
           If Not bCancel Then
              m_bInDrag = True
           
              Dim tP As POINTAPI
              GetCursorPos tP
              LSet m_tPInitial = tP
              m_lSplitInitial = m_lSplitPos
                 
              Dim tR As RECT
              GetWindowRect m_oContainer.hwnd, tR
              ClipCursorRect tR
              
              If Not (m_bFullDrag) Then
                 If (m_eOrientation = cSPLTOrientationVertical) Then
                    m_tSplitR.Left = tR.Left + m_lSplitPos
                    m_tSplitR.Right = m_tSplitR.Left + m_lSplitSize
                    m_tSplitR.Top = tR.Top
                    m_tSplitR.Bottom = tR.Bottom
                 Else
                    m_tSplitR.Left = tR.Left
                    m_tSplitR.Right = tR.Right
                    m_tSplitR.Top = tR.Top + m_lSplitPos
                    m_tSplitR.Bottom = m_tSplitR.Top + m_lSplitSize
                 End If
                 
                 pDrawSplitter
                 
              End If
              
           End If
        End If
    End If
End Sub
Public Sub MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
        If m_oLeftTop.Visible And m_oRightBottom.Visible Then
            If (pbConfigured) Then
               SetCursor m_hCursor
            
               If (m_bInDrag) And (Button) Then
                  
                  Dim tP As POINTAPI
                  GetCursorPos tP
                  
                  If Not (m_bFullDrag) Then
                     pDrawSplitter
                  End If
                  
                  If (m_eOrientation = cSPLTOrientationVertical) Then
                     m_lSplitPos = m_lSplitInitial + (tP.X - m_tPInitial.X)
                  Else
                     m_lSplitPos = m_lSplitInitial + (tP.Y - m_tPInitial.Y)
                  End If
                  pValidatePosition
                  
                  If (m_bFullDrag) Then
                     pResizePanels
                  Else
                     Dim tR As RECT
                     GetWindowRect m_oContainer.hwnd, tR
                     
                     If (m_eOrientation = cSPLTOrientationVertical) Then
                        m_tSplitR.Left = tR.Left + m_lSplitPos
                        m_tSplitR.Right = m_tSplitR.Left + m_lSplitSize
                        m_tSplitR.Top = tR.Top
                        m_tSplitR.Bottom = tR.Bottom
                     Else
                        m_tSplitR.Left = tR.Left
                        m_tSplitR.Right = tR.Right
                        m_tSplitR.Top = tR.Top + m_lSplitPos
                        m_tSplitR.Bottom = m_tSplitR.Top + m_lSplitSize
                     End If
                        
                     pDrawSplitter
            
                  End If
                  
               End If
            End If
        End If
End Sub
Public Sub MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If m_oLeftTop.Visible And m_oRightBottom.Visible Then
        If (pbConfigured()) Then
           If (m_bInDrag) Then
              ClipCursorClear 0&
              
              Dim tP As POINTAPI
              GetCursorPos tP
              
              If Not m_bFullDrag Then
                 pDrawSplitter
              End If
              
              If (m_eOrientation = cSPLTOrientationVertical) Then
                 m_lSplitPos = m_lSplitInitial + (tP.X - m_tPInitial.X)
              Else
                 m_lSplitPos = m_lSplitInitial + (tP.Y - m_tPInitial.Y)
              End If
              pValidatePosition
                 
              pResizePanels
              
              pSetProportion
              m_bInDrag = False
           End If
        End If
    End If
End Sub

Private Sub pDrawSplitter()
Dim lhDC As Long
Dim hOldBrush As Long
   lhDC = CreateDCAsNull("DISPLAY", ByVal 0&, ByVal 0&, ByVal 0&)
   hOldBrush = SelectObject(lhDC, m_hBrush)
   PatBlt lhDC, m_tSplitR.Left, m_tSplitR.Top, m_tSplitR.Right - m_tSplitR.Left, m_tSplitR.Bottom - m_tSplitR.Top, PATINVERT
   SelectObject lhDC, hOldBrush
   DeleteDC lhDC
End Sub

Private Sub pSetProportion()
   If (m_eOrientation = cSPLTOrientationVertical) Then
      m_fProportion = (m_lSplitPos * 1#) / m_oContainer.ScaleX(m_oContainer.ScaleWidth, m_oContainer.ScaleMode, vbPixels)
   Else
      m_fProportion = (m_lSplitPos * 1#) / m_oContainer.ScaleY(m_oContainer.ScaleHeight, m_oContainer.ScaleMode, vbPixels)
   End If
End Sub

Private Sub pValidatePosition()
   
   Dim tR As RECT
   GetClientRect m_oContainer.hwnd, tR
   
   If (m_eOrientation = cSPLTOrientationVertical) Then
      ' Check right too big:
      If (m_lMaxSize(2) > 0) Then
         If ((tR.Right - m_lSplitPos - m_lSplitSize) > m_lMaxSize(2)) Then
            m_lSplitPos = tR.Right - m_lMaxSize(2) - m_lSplitSize
         End If
      End If
      ' Check left too big:
      If (m_lMaxSize(1) > 0) Then
         If (m_lSplitPos > m_lMaxSize(1)) Then
            m_lSplitPos = m_lMaxSize(1)
         End If
      End If
      ' Check right too small:
      If (m_lMinSize(2) > 0) Then
         If ((tR.Right - m_lSplitPos - m_lSplitSize) < m_lMinSize(2)) Then
            m_lSplitPos = tR.Right - m_lMinSize(2) - m_lSplitSize
         End If
      End If
      ' Check left too small:
      If (m_lMinSize(1) > 0) Then
         If (m_lSplitPos < m_lMinSize(1)) Then
            m_lSplitPos = m_lMinSize(1)
         End If
      End If
   Else
      ' Check bottom too big:
      If (m_lMaxSize(2) > 0) Then
         If ((tR.Bottom - m_lSplitPos - m_lSplitSize) > m_lMaxSize(2)) Then
            m_lSplitPos = tR.Bottom - m_lMaxSize(2) - m_lSplitSize
         End If
      End If
      ' Check top too big:
      If (m_lMaxSize(1) > 0) Then
         If (m_lSplitPos > m_lMaxSize(1)) Then
            m_lSplitPos = m_lMaxSize(1)
         End If
      End If
      ' Bottom too small:
      If (m_lMinSize(2) > 0) Then
         If ((tR.Bottom - m_lSplitPos - m_lSplitSize) < m_lMinSize(2)) Then
            m_lSplitPos = tR.Bottom - m_lMinSize(2) - m_lSplitSize
         End If
      End If
      ' Top too small:
      If (m_lMinSize(1) > 0) Then
         If (m_lSplitPos < m_lMinSize(1)) Then
            m_lSplitPos = m_lMinSize(1)
         End If
      End If
   End If
End Sub

Public Sub Resize()
   If pbConfigured() Then
            
      ' Get the container's size:
      Dim tR As RECT
      GetClientRect m_oContainer.hwnd, tR
      
      If (m_bKeepProportionsWhenResizing) Then
         ' attempt to keep the proportions of the two parts:
         If (m_eOrientation = cSPLTOrientationVertical) Then
            m_lSplitPos = (tR.Right - tR.Left) * m_fProportion
         Else
            m_lSplitPos = (tR.Bottom - tR.Top) * m_fProportion
         End If
      End If
            
      pValidatePosition
      pResizePanels
      
   End If
End Sub

Private Sub pResizePanels()
   Dim f As Single
   On Error Resume Next
'   This doesn't work right at all
    If (m_oContainer.Visible = True) And (m_oLeftTop.Visible = False) Then
        m_oRightBottom.Move 0, 0, m_oContainer.ScaleWidth, m_oContainer.ScaleHeight
    ElseIf (m_oContainer.Visible = True) And (m_oRightBottom.Visible = False) Then
        m_oLeftTop.Move 0, 0, m_oContainer.ScaleWidth, m_oContainer.ScaleHeight
    ElseIf (m_eOrientation = cSPLTOrientationHorizontal) Then
       f = m_oContainer.ScaleY(m_lSplitPos, vbPixels, m_oContainer.ScaleMode)
       m_oLeftTop.Move 0, 0, m_oContainer.ScaleWidth, f
       f = f + m_oContainer.ScaleY(m_lSplitSize, vbPixels, m_oContainer.ScaleMode)
       m_oRightBottom.Move 0, f, m_oContainer.ScaleWidth, m_oContainer.ScaleHeight - f
    Else
       f = m_oContainer.ScaleX(m_lSplitPos, vbPixels, m_oContainer.ScaleMode)
       m_oLeftTop.Move 0, 0, f, m_oContainer.ScaleHeight
       f = f + m_oContainer.ScaleX(m_lSplitSize, vbPixels, m_oContainer.ScaleMode)
       m_oRightBottom.Move f, 0, m_oContainer.ScaleWidth - f, m_oContainer.ScaleHeight
    End If

End Sub

Private Function CreateBrush() As Boolean
Dim tbm As BITMAP
Dim hBm As Long

   DestroyBrush
      
   ' Create a monochrome bitmap containing the desired pattern:
   tbm.bmType = 0
   tbm.bmWidth = 16
   tbm.bmHeight = 8
   tbm.bmWidthBytes = 2
   tbm.bmPlanes = 1
   tbm.bmBitsPixel = 1
   tbm.bmBits = VarPtr(m_lPattern(0))
   hBm = CreateBitmapIndirect(tbm)

   ' Make a brush from the bitmap bits
   m_hBrush = CreatePatternBrush(hBm)

   '// Delete the useless bitmap
   DeleteObject hBm

End Function
Private Sub DestroyBrush()
   If Not (m_hBrush = 0) Then
      DeleteObject m_hBrush
      m_hBrush = 0
   End If
End Sub

Private Sub Class_Initialize()
   
   m_fProportion = 0.5
   m_eOrientation = cSPLTOrientationHorizontal
      m_hCursor = LoadCursorLong(0, IDC_SIZENS)
   m_lSplitSize = 4
   m_lMinSize(1) = 8
   m_lMaxSize(1) = -1
   m_lMinSize(2) = 8
   m_lMaxSize(2) = -1
   m_bFullDrag = True
   m_lSplitPos = 128
   
   Dim i As Long
   For i = 0 To 3
      m_lPattern(i) = &HAAAA5555
   Next i
   
End Sub

Private Sub Class_Terminate()
   DestroyBrush
   If Not (m_hCursor = 0) Then
      DestroyCursor m_hCursor
   End If
End Sub
