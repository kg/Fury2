VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Begin VB.Form frmImage 
   BorderStyle     =   0  'None
   Caption         =   "Loading"
   ClientHeight    =   3180
   ClientLeft      =   0
   ClientTop       =   -15
   ClientWidth     =   4680
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmImage.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   212
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   312
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtZoom 
      Alignment       =   1  'Right Justify
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3870
      TabIndex        =   3
      Text            =   "100"
      ToolTipText     =   "Zoom (%)"
      Top             =   2880
      Width           =   555
   End
   Begin VB.HScrollBar hsImage 
      Height          =   255
      LargeChange     =   64
      Left            =   -15
      SmallChange     =   16
      TabIndex        =   2
      Top             =   2880
      Width           =   3885
   End
   Begin VB.VScrollBar vsImage 
      Height          =   2880
      LargeChange     =   64
      Left            =   4425
      SmallChange     =   16
      TabIndex        =   1
      Top             =   0
      Width           =   255
   End
   Begin VB.PictureBox picImage 
      AutoRedraw      =   -1  'True
      BorderStyle     =   0  'None
      Height          =   2880
      Left            =   0
      ScaleHeight     =   192
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   295
      TabIndex        =   0
      Top             =   0
      Width           =   4425
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   30
      Top             =   30
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmImage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements iCustomMenus
Implements iExtendedForm
Implements iDocument

Private Declare Function SetStretchBltMode Lib "gdi32" (ByVal hdc As Long, ByVal nStretchMode As Long) As Long
Private Const COLORONCOLOR = 3

Private m_sngZoom As Single
Private m_imgImage As Fury2Image
Private m_imgImageCache As Fury2Image
Private m_strFilename As String
Private m_fpgPlugin As iFileTypePlugin

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Private Sub Form_Activate()
On Error Resume Next
    picImage.AutoRedraw = True
    picImage_Paint
End Sub

Private Sub Form_Deactivate()
    picImage.AutoRedraw = False
End Sub

Private Sub Form_Load()
    m_sngZoom = 1
End Sub

Private Sub iCustomMenus_DestroyMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DestroyMenu "Filters"
    End With
End Sub

Private Sub iCustomMenus_InitializeMenus(Handler As ngInterfaces.iCustomMenuHandler)
On Error Resume Next
    With Handler
        .DefineMenu "Filters", "Filters"
    End With
End Sub

Private Sub iCustomMenus_MenuClick(key As String)
On Error Resume Next
Dim l_strParameter As String, l_lngParameter As Long
Dim CommandName As String
    CommandName = key
    If InStr(CommandName, "(") Then
        l_strParameter = Trim(Mid(CommandName, InStr(CommandName, "(") + 1))
        If Right(l_strParameter, 1) = ")" Then l_strParameter = Trim(Left(l_strParameter, Len(l_strParameter) - 1))
        CommandName = Left(CommandName, InStr(CommandName, "(") - 1)
        CommandName = Replace(CommandName, ":", "_")
        If Left(l_strParameter, 1) = """" Then
            ' String
            l_strParameter = CStr(Mid(l_strParameter, 2, Len(l_strParameter) - 2))
            CallByName Me, CommandName, VbMethod, l_strParameter
        Else
            ' Integer
            l_lngParameter = CLng(l_strParameter)
            CallByName Me, CommandName, VbMethod, l_lngParameter
        End If
    Else
        CommandName = Replace(CommandName, ":", "_")
        CallByName Me, CommandName, VbMethod
    End If
    Err.Clear
End Sub

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = m_fpgPlugin
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
    Set m_fpgPlugin = RHS
End Property

Public Sub SetImage(Image As Fury2Image)
On Error Resume Next
Dim l_sngXRatio As Single, l_sngYRatio As Single
    Set m_imgImage = Image
    If m_imgImage.AlphaChannel Then
        Set m_imgImageCache = Image.Duplicate
        m_imgImageCache.Composite SwapChannels(GetSystemColor(SystemColor_Button_Face), Red, Blue)
    Else
        Set m_imgImageCache = Image
    End If
    l_sngXRatio = (Screen.Width * 0.75 / Screen.TwipsPerPixelX) / m_imgImage.Width
    l_sngYRatio = (Screen.Height * 0.75 / Screen.TwipsPerPixelY) / m_imgImage.Height
    If l_sngXRatio > 1.5 Or l_sngYRatio > 1.5 Then
        If (l_sngXRatio < l_sngYRatio) And (l_sngXRatio > 1) Then
            m_sngZoom = l_sngXRatio
        Else
            m_sngZoom = l_sngYRatio
        End If
    ElseIf l_sngXRatio < 0.5 Or l_sngYRatio < 0.5 Then
        If (l_sngXRatio > l_sngYRatio) And (l_sngXRatio < 1) Then
            m_sngZoom = 1 / l_sngXRatio
        Else
            m_sngZoom = 1 / l_sngYRatio
        End If
    End If
    If m_sngZoom < 1 Then
        m_sngZoom = (CLng(m_sngZoom * 100) \ 25) * 25
    ElseIf m_sngZoom > 1 Then
        m_sngZoom = Floor(m_sngZoom)
    End If
    txtZoom.Text = CStr(CLng(m_sngZoom * 100))
End Sub

Public Sub SetFilename(Name As String)
On Error Resume Next
    m_strFilename = Name
    Me.Caption = IIf(Trim(Name) = "", "Untitled.png", GetTitle(Name))
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    Select Case KeyCode
    Case vbKeyUp
        vsImage.Value = ClipValue(vsImage.Value - vsImage.SmallChange, vsImage.Min, vsImage.Max)
    Case vbKeyDown
        vsImage.Value = ClipValue(vsImage.Value + vsImage.SmallChange, vsImage.Min, vsImage.Max)
    Case vbKeyLeft
        hsImage.Value = ClipValue(hsImage.Value - hsImage.SmallChange, hsImage.Min, hsImage.Max)
    Case vbKeyRight
        hsImage.Value = ClipValue(hsImage.Value + hsImage.SmallChange, hsImage.Min, hsImage.Max)
    End Select
End Sub

Private Sub Form_Resize()
On Error Resume Next
    hsImage.Move 0, Me.ScaleHeight - hsImage.Height, Me.ScaleWidth - (vsImage.Width + txtZoom.Width), hsImage.Height
    txtZoom.Move hsImage.Width, hsImage.Top, txtZoom.Width, hsImage.Height
    vsImage.Move Me.ScaleWidth - vsImage.Width, 0, vsImage.Width, Me.ScaleHeight - (hsImage.Height)
    picImage.Move 0, 0, Me.ScaleWidth - vsImage.Width, Me.ScaleHeight - (hsImage.Height)
End Sub

Private Sub hsImage_Change()
On Error Resume Next
    picImage_Paint
End Sub

Private Sub hsImage_Scroll()
On Error Resume Next
    hsImage_Change
End Sub

Private Property Get iDocument_CanSave() As Boolean
On Error Resume Next
    iDocument_CanSave = Not (m_imgImage Is Nothing)
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = m_strFilename
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
    iDocument_Save = m_imgImage.SavePNG(Filename)
    If iDocument_Save Then
        SetFilename Filename
    End If
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Image"
End Property

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Private Sub picImage_Paint()
On Error Resume Next
Dim l_rctDest As Fury2Rect, l_rctSource As Fury2Rect
Dim l_lngXMax As Long, l_lngYMax As Long
    If m_imgImage Is Nothing Then Exit Sub
    With m_imgImage
        l_lngXMax = (.Width) - (picImage.ScaleWidth / m_sngZoom)
        l_lngYMax = (.Height) - (picImage.ScaleHeight / m_sngZoom)
        If l_lngXMax > 0 Then
            If Not hsImage.Enabled Then
                hsImage.Enabled = True
            End If
            If hsImage.Max <> l_lngXMax Then
                hsImage.Max = l_lngXMax
            End If
        Else
            If hsImage.Enabled Then
                hsImage.Enabled = False
                hsImage.Value = 0
            End If
        End If
        If l_lngYMax > 0 Then
            If Not vsImage.Enabled Then
                vsImage.Enabled = True
            End If
            If vsImage.Max <> l_lngYMax Then
                vsImage.Max = l_lngYMax
            End If
        Else
            If vsImage.Enabled Then
                vsImage.Enabled = False
                vsImage.Value = 0
            End If
        End If
        Set l_rctSource = .Rectangle
        Set l_rctDest = libGraphics.F2Rect(-hsImage.Value * m_sngZoom, -vsImage.Value * m_sngZoom, .Width * m_sngZoom, .Height * m_sngZoom, False)
        If l_rctDest.Left < 0 Then
            l_rctSource.Left = l_rctSource.Left + ((-l_rctDest.Left) / m_sngZoom)
            l_rctDest.Left = 0
        End If
        If l_rctDest.Top < 0 Then
            l_rctSource.Top = l_rctSource.Top + ((-l_rctDest.Top) / m_sngZoom)
            l_rctDest.Top = 0
        End If
        If l_rctDest.Width > picImage.ScaleWidth Then
            l_rctSource.Width = (picImage.ScaleWidth / m_sngZoom)
            l_rctDest.Width = l_rctSource.Width * m_sngZoom
        End If
        If l_rctDest.Height > picImage.ScaleHeight Then
            l_rctSource.Height = (picImage.ScaleHeight / m_sngZoom)
            l_rctDest.Height = l_rctSource.Height * m_sngZoom
        End If
        SetStretchBltMode picImage.hdc, COLORONCOLOR
        DrawImageToDC picImage.hdc, l_rctDest, l_rctSource, m_imgImageCache
        If ((.Width - hsImage.Value) * m_sngZoom) < picImage.ScaleWidth Then
            picImage.Line ((.Width - hsImage.Value) * m_sngZoom, 0)-(picImage.ScaleWidth, picImage.ScaleHeight), picImage.BackColor, BF
        End If
        If ((.Height - vsImage.Value) * m_sngZoom) < picImage.ScaleHeight Then
            picImage.Line (0, (.Height - vsImage.Value) * m_sngZoom)-(picImage.ScaleWidth, picImage.ScaleHeight), picImage.BackColor, BF
        End If
    End With
    If picImage.AutoRedraw Then picImage.Refresh
End Sub

Private Sub picImage_Resize()
On Error Resume Next
    picImage_Paint
End Sub

Private Sub txtZoom_Change()
On Error Resume Next
    m_sngZoom = CSng(ClipValue(CLng(txtZoom.Text), 1, 1600)) / 100
    picImage_Paint
End Sub

Private Sub vsImage_Change()
On Error Resume Next
    picImage_Paint
End Sub

Private Sub vsImage_Scroll()
On Error Resume Next
    vsImage_Change
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = False
End Property

