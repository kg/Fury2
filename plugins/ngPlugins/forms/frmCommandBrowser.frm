VERSION 5.00
Object = "{F588DF24-2FB2-4956-9668-1BD0DED57D6C}#1.4#0"; "MDIActiveX.ocx"
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Object = "{CA5A8E1E-C861-4345-8FF8-EF0A27CD4236}#2.0#0"; "vbalTreeView6.ocx"
Object = "{DBCEA9F3-9242-4DA3-9DB7-3F59DB1BE301}#8.9#0"; "ngUI.ocx"
Begin VB.Form frmCommandBrowser 
   BorderStyle     =   0  'None
   Caption         =   "Command Browser"
   ClientHeight    =   4935
   ClientLeft      =   0
   ClientTop       =   -15
   ClientWidth     =   5520
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
   Icon            =   "frmCommandBrowser.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   329
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   368
   ShowInTaskbar   =   0   'False
   Begin vbalIml6.vbalImageList ilObjects 
      Left            =   420
      Top             =   1920
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   32
      Size            =   4592
      Images          =   "frmCommandBrowser.frx":038A
      Version         =   131072
      KeyCount        =   4
      Keys            =   "BACKÿFORWARDÿRESETÿFILTER"
   End
   Begin VB.PictureBox picObjects 
      Height          =   4935
      Left            =   0
      ScaleHeight     =   325
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   129
      TabIndex        =   1
      Top             =   0
      Width           =   1995
      Begin ngUI.ngToolbar tbrObjects 
         Height          =   270
         Left            =   360
         TabIndex        =   8
         Top             =   180
         Width           =   405
         _ExtentX        =   714
         _ExtentY        =   476
      End
      Begin VB.TextBox txtFilterObjects 
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   1290
         TabIndex        =   3
         Top             =   0
         Width           =   105
      End
      Begin vbalTreeViewLib6.vbalTreeView tvObjects 
         Height          =   4080
         Left            =   165
         TabIndex        =   2
         Top             =   765
         Width           =   1695
         _ExtentX        =   2990
         _ExtentY        =   7197
         BorderStyle     =   0
         NoCustomDraw    =   0   'False
         HistoryStyle    =   -1  'True
         FullRowSelect   =   -1  'True
         SingleSel       =   -1  'True
         Style           =   1
         ScaleMode       =   3
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Tahoma"
            Size            =   9
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
      End
   End
   Begin VB.PictureBox picObjectMembers 
      BorderStyle     =   0  'None
      Height          =   4935
      Left            =   2025
      ScaleHeight     =   4935
      ScaleWidth      =   3495
      TabIndex        =   0
      Top             =   0
      Width           =   3495
      Begin VB.TextBox txtInfo 
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1890
         Left            =   0
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         TabIndex        =   7
         Top             =   3000
         Width           =   3450
      End
      Begin VB.PictureBox picMembers 
         Height          =   2910
         Left            =   0
         ScaleHeight     =   190
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   226
         TabIndex        =   4
         Top             =   0
         Width           =   3450
         Begin VB.TextBox txtFilterMembers 
            BorderStyle     =   0  'None
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   9
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   240
            Left            =   1290
            TabIndex        =   5
            Top             =   -15
            Width           =   105
         End
         Begin vbalIml6.vbalImageList ilMembers 
            Left            =   705
            Top             =   1305
            _ExtentX        =   953
            _ExtentY        =   953
            ColourDepth     =   32
            Size            =   4592
            Images          =   "frmCommandBrowser.frx":159A
            Version         =   131072
            KeyCount        =   4
            Keys            =   "BACKÿFORWARDÿRESETÿFILTER"
         End
         Begin vbalTreeViewLib6.vbalTreeView tvMembers 
            Height          =   4080
            Left            =   0
            TabIndex        =   6
            Top             =   0
            Width           =   1695
            _ExtentX        =   2990
            _ExtentY        =   7197
            BorderStyle     =   0
            NoCustomDraw    =   0   'False
            HistoryStyle    =   -1  'True
            FullRowSelect   =   -1  'True
            SingleSel       =   -1  'True
            Style           =   1
            ScaleMode       =   3
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "Tahoma"
               Size            =   9
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
         End
         Begin ngUI.ngToolbar tbrMembers 
            Height          =   270
            Left            =   0
            TabIndex        =   9
            Top             =   0
            Width           =   405
            _ExtentX        =   714
            _ExtentY        =   476
         End
      End
   End
   Begin sMDIinActiveX.MDIActiveX extender 
      Left            =   0
      Top             =   4485
      _ExtentX        =   847
      _ExtentY        =   794
   End
End
Attribute VB_Name = "frmCommandBrowser"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
'    ngPlugins (Fury² Game Creation System Next-Generation Editor Standard Plugin Set)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit
Implements iExtendedForm
Implements iDocument

Private Const WM_SETREDRAW = &HB
Private Declare Function InvalidateRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT, ByVal bErase As Long) As Long

Public Plugin As CommandBrowser
Dim m_miSelectedMember As MemberInfo
Dim m_tiSelectedObject As TypeInfo
Dim m_tlbEngine As TypeLibInfo
Dim m_tlbGraphics As TypeLibInfo
Dim m_tlbSound As TypeLibInfo
Dim m_tlbFilesystem As TypeLibInfo
Dim m_splSplitMain As New cSplitter
Dim m_splSplitMembers As New cSplitter

Private Property Get iDocument_DocumentIcon() As libGraphics.Fury2Image
End Property

Private Property Get iDocument_Object() As Object
    Set iDocument_Object = Me
End Property

Public Sub InitToolbars()
    Set tbrObjects.ResourceFile = Plugin.Editor.Resources
    tbrObjects.ResourcePattern = "toolbar\*.png"
    With tbrObjects.Buttons
        .AddNew , "Back", "undo", "Go Back"
        .AddNew , "Forward", "redo", "Go Forward"
        .AddNew "-"
        .AddNew , "ResetFilter", "delete", "Reset Filter"
        .AddNew "Filter:", , , , , , False
    End With
    Set tbrMembers.ResourceFile = Plugin.Editor.Resources
    tbrMembers.ResourcePattern = "toolbar\*.png"
    With tbrMembers.Buttons
        .AddNew , "Back", "undo", "Go Back"
        .AddNew , "Forward", "redo", "Go Forward"
        .AddNew "-"
        .AddNew , "ResetFilter", "delete", "Reset Filter"
        .AddNew "Filter:", , , , , , False
    End With
End Sub

Public Function TLIToString(ByRef VT As VarTypeInfo, ByRef Value) As String
On Error Resume Next
Dim l_memMember As MemberInfo
    With VT
        If .TypeInfo Is Nothing Then
            If VarType(Value) = vbObject Then
                If Value Is Nothing Then
                    TLIToString = "Nothing"
                End If
            ElseIf VarType(Value) = vbString Then
                TLIToString = """" & Replace(Value, """", """""") & """"
            Else
                TLIToString = CStr(Value)
            End If
        Else
            If .TypeInfo.TypeKind = TKIND_ENUM Then
                For Each l_memMember In .TypeInfo.Members
                    With l_memMember
                        If .Value = Value Then
                            TLIToString = .Name
                            Exit Function
                        End If
                    End With
                Next l_memMember
            End If
            TLIToString = CStr(Value)
        End If
    End With
End Function

Public Function VarTypeSignature(ByRef VT As VarTypeInfo) As String
On Error Resume Next
Dim l_vtType As TliVarType
Dim l_booArray As Boolean
Dim l_booByref As Boolean
    With VT
        l_vtType = .VarType
        If ((l_vtType And VT_ARRAY) = VT_ARRAY) Then
            l_booArray = True
            l_vtType = l_vtType And (Not VT_ARRAY)
        End If
        If ((l_vtType And VT_BYREF) = VT_BYREF) Then
            l_booByref = True
            l_vtType = l_vtType And (Not VT_BYREF)
        End If
        VarTypeSignature = "Unknown"
        If Not (.TypeInfo Is Nothing) Then
            VarTypeSignature = IIf(Left(.TypeInfo.Name, 1) = "_", Mid(.TypeInfo.Name, 2), .TypeInfo.Name)
        Else
            Select Case l_vtType
            Case VT_EMPTY
                VarTypeSignature = "Empty"
            Case VT_NULL
                VarTypeSignature = "Null"
            Case VT_I2
                VarTypeSignature = "Integer"
            Case VT_I4
                VarTypeSignature = "Long"
            Case VT_R4
                VarTypeSignature = "Single"
            Case VT_R8
                VarTypeSignature = "Double"
            Case VT_CY
            Case VT_DATE
                VarTypeSignature = "Date"
            Case VT_BSTR
                VarTypeSignature = "String"
            Case VT_DISPATCH
                VarTypeSignature = "Object"
            Case VT_ERROR
                VarTypeSignature = "Error"
            Case VT_BOOL
                VarTypeSignature = "Boolean"
            Case VT_VARIANT
                VarTypeSignature = "Variant"
            Case VT_UNKNOWN
            Case VT_DECIMAL
            Case VT_I1
            Case VT_UI1
                VarTypeSignature = "Byte"
            Case VT_UI2
            Case VT_UI4
            Case VT_I8
            Case VT_UI8
            Case VT_INT
                VarTypeSignature = "Long"
            Case VT_UINT
            Case VT_VOID
            Case VT_HRESULT
            Case VT_PTR
            Case VT_SAFEARRAY
            Case VT_CARRAY
            Case VT_USERDEFINED
            Case VT_LPSTR
            Case VT_LPWSTR
            Case VT_RECORD
            Case VT_FILETIME
            Case VT_BLOB
            Case VT_STREAM
            Case VT_STORAGE
            Case VT_STREAMED_OBJECT
            Case VT_STORED_OBJECT
            Case VT_BLOB_OBJECT
            Case VT_CF
            Case VT_CLSID
            Case VT_VECTOR
            Case Else
            End Select
        End If
        If l_booArray Then VarTypeSignature = VarTypeSignature + "()"
        If l_booByref Then VarTypeSignature = "Byref" + VarTypeSignature
    End With
End Function

Public Function ParameterList(ByRef Member As MemberInfo, Optional ByVal ParameterTypes As Boolean = False, Optional ByVal Parens As Boolean = False) As String
On Error Resume Next
Dim l_strParameters As String
Dim l_lngParameters As Long
    With Member
        If .Parameters.Count > 0 Then
            For l_lngParameters = 1 To .Parameters.Count
                With .Parameters(l_lngParameters)
                    If (ParameterTypes) Then
                        If .Default Then
                            l_strParameters = l_strParameters & "Optional " & .Name & " As " & VarTypeSignature(.VarTypeInfo) & " = " & TLIToString(.VarTypeInfo, .DefaultValue)
                        ElseIf .Optional Then
                            l_strParameters = l_strParameters & "Optional " & .Name & " As " & VarTypeSignature(.VarTypeInfo)
                        Else
                            l_strParameters = l_strParameters & .Name & " As " & VarTypeSignature(.VarTypeInfo)
                        End If
                    Else
                        If .Default Or .Optional Then
                            l_strParameters = l_strParameters & "Optional " & .Name
                        Else
                            l_strParameters = l_strParameters & .Name
                        End If
                    End If
                    If (l_lngParameters < Member.Parameters.Count) Then
                        l_strParameters = l_strParameters & ", "
                    End If
                End With
            Next l_lngParameters
        End If
    End With
    If Parens And Len(l_strParameters) > 0 Then
        ParameterList = "(" & l_strParameters & ")"
    Else
        ParameterList = l_strParameters
    End If
End Function

Public Function MemberSignature(ByRef Member As MemberInfo, Optional ByVal ParameterTypes As Boolean = False, Optional ByVal ShortForm As Boolean) As String
On Error Resume Next
Dim l_strParameters As String
Dim l_lngParameters As Long
Dim l_strReturnType As String
Dim l_strInvokeType As String
Dim l_booProperty As Boolean
Dim l_booSub As Boolean
    With Member
        Select Case .InvokeKind
        Case INVOKE_FUNC
            If .ReturnType.VarType = VT_VOID Then
                l_strInvokeType = "Sub "
                l_booSub = True
            Else
                l_strInvokeType = "Function "
            End If
        Case INVOKE_PROPERTYGET
            l_booProperty = True
            l_strInvokeType = IIf(ShortForm, "Property ", "Property Get ")
        Case INVOKE_PROPERTYPUT
            l_booProperty = True
            l_strInvokeType = IIf(ShortForm, "Property ", "Property Let ")
        Case INVOKE_PROPERTYPUTREF
            l_booProperty = True
            l_strInvokeType = IIf(ShortForm, "Property ", "Property Set ")
        End Select
        l_strParameters = ParameterList(Member, ParameterTypes)
        If .ReturnType.VarType <> VT_VOID Then
            l_strReturnType = " As " & VarTypeSignature(.ReturnType)
        End If
        If (ShortForm) Then
            If l_booProperty Then
                MemberSignature = "Property " & .Name
            ElseIf l_booSub Then
                MemberSignature = "Sub " & .Name
            Else
                MemberSignature = "Function " & .Name
            End If
        Else
            MemberSignature = l_strInvokeType & .Name & "(" & l_strParameters & ")" & l_strReturnType
        End If
    End With
End Function

Public Function TypeFromName(ByRef TypeLib As TypeLibInfo, ByRef Name As String) As TypeInfo
    Set TypeFromName = TypeLib.GetTypeInfo(TypeLib.GetTypeInfoNumber(Name))
End Function

Public Function TypeLibFromName(ByRef Name As String) As TypeLibInfo
    Select Case LCase(Name)
    Case "engine"
        Set TypeLibFromName = m_tlbEngine
    Case "libgraphics"
        Set TypeLibFromName = m_tlbGraphics
    Case "libsound"
        Set TypeLibFromName = m_tlbSound
    Case "libfilesystem"
        Set TypeLibFromName = m_tlbFilesystem
    Case Else
    End Select
End Function

Public Sub InitSplitters()
On Error Resume Next
    With m_splSplitMain
        .Orientation = cSPLTOrientationVertical
        .Bind picObjects, picObjectMembers, Me
        .Orientation = cSPLTOrientationVertical
        .MinimumSize(cSPLTLeftOrTopPanel) = 75
        .MaximumSize(cSPLTLeftOrTopPanel) = 300
        .KeepProportion = True
        .Position = 150
    End With
    With m_splSplitMembers
        .Orientation = cSPLTOrientationHorizontal
        .Bind picMembers, txtInfo, picObjectMembers
        .Orientation = cSPLTOrientationHorizontal
        .MinimumSize(cSPLTRightOrBottomPanel) = 50
        .MaximumSize(cSPLTRightOrBottomPanel) = 250
        .KeepProportion = True
        .Position = Me.ScaleHeight - 100
    End With
End Sub

Public Sub InitBrowser()
On Error Resume Next
    Set m_tlbEngine = InterfaceInfoFromObject(Engine.Fury2Globals).Parent
    Set m_tlbGraphics = InterfaceInfoFromObject(libGraphics.Fury2GEGlobal).Parent
    Set m_tlbSound = InterfaceInfoFromObject(libSound.Fury2SEGlobals).Parent
    Set m_tlbFilesystem = InterfaceInfoFromObject(libFilesystem.Fury2FSGlobals).Parent
End Sub

Public Sub EnumObjects(ByVal TypeLib As TypeLibInfo, ByVal Target As vbalTreeView)
On Error Resume Next
Dim l_srResults As SearchResults
Dim l_siItem As SearchItem
Dim l_tnParent As cTreeViewNode
    Target.Refresh
    Set l_srResults = TypeLib.GetTypes(, tliStAppObject Or tliStClasses Or tliStConstants Or tliStDeclarations Or tliStEvents Or tliStIntrinsicAliases, True)
    If (Len(Trim(txtFilterObjects.Text)) > 0) Then
        For Each l_siItem In l_srResults
            If (InStr(1, l_siItem.Name, Trim(txtFilterObjects.Text), vbTextCompare)) Then
                Target.Nodes.Add , , TypeLib.Name & ":" & l_siItem.Name, l_siItem.Name
            End If
        Next l_siItem
    Else
        Set l_tnParent = Target.Nodes.Add(, , TypeLib.Name, TypeLib.Name)
        For Each l_siItem In l_srResults
            l_tnParent.Children.Add l_tnParent, etvwChild, TypeLib.Name & ":" & l_siItem.Name, l_siItem.Name
        Next l_siItem
    End If
End Sub

Public Sub RefreshObjects()
On Error Resume Next
    SendMessage tvObjects.hWndTreeView, WM_SETREDRAW, 0, 0
    tvObjects.Nodes.Clear
    EnumObjects m_tlbEngine, tvObjects
    EnumObjects m_tlbGraphics, tvObjects
    EnumObjects m_tlbSound, tvObjects
    EnumObjects m_tlbFilesystem, tvObjects
    tvObjects.Sorted = True
    SendMessage tvObjects.hWndTreeView, WM_SETREDRAW, 1, 0
End Sub

Public Sub RefreshMembers()
On Error Resume Next
Dim l_strOldItem As String
Dim l_lngMembers As Long
Dim l_intInterface As InterfaceInfo
Dim l_nodNode As cTreeViewNode
Dim l_memMember As MemberInfo
Dim l_booFiltered As Boolean
    l_booFiltered = Len(Trim(txtFilterMembers.Text)) > 0
    SendMessage tvMembers.hWndTreeView, WM_SETREDRAW, 0, 0
    tvMembers.Nodes.Clear
    With m_tiSelectedObject
        Select Case .TypeKind
        Case TKIND_ENUM
            With .Members
                For l_lngMembers = 1 To .Count
                    If (Not l_booFiltered) Or (InStr(1, .Item(l_lngMembers).Name, txtFilterMembers.Text, vbTextCompare)) Then
                        Set l_nodNode = tvMembers.Nodes.Add(, , .Item(l_lngMembers).Name, .Item(l_lngMembers).Name & " = " & CStr(.Item(l_lngMembers).Value))
                    End If
                Next l_lngMembers
            End With
        Case TKIND_COCLASS
            With .DefaultInterface.Members
                For l_lngMembers = 1 To .Count
                    Set l_memMember = .Item(l_lngMembers)
                    With l_memMember
                        If ((Not l_booFiltered) Or (InStr(1, .Name, txtFilterMembers.Text, vbTextCompare))) Then
                        
                        
                        
                            Select Case LCase(Trim(.Name))
                            Case "addref", "release", "queryinterface", "gettypeinfo", "gettypeinfocount", "getidsofnames", "invoke", "class_tostring"
                            Case Else
                                If (tvMembers.Nodes.Exists(.Name)) Then
                                    Set l_nodNode = tvMembers.Nodes(.Name)
                                Else
                                    Set l_nodNode = tvMembers.Nodes.Add(, , .Name, MemberSignature(l_memMember, , True))
                                End If
                                Select Case .InvokeKind
                                Case INVOKE_FUNC
                                    l_nodNode.AddChildNode .Name & ":Call", MemberSignature(l_memMember, True)
                                Case INVOKE_PROPERTYGET
                                    l_nodNode.AddChildNode .Name & ":Get", VarTypeSignature(.ReturnType) & " = " & "Object." & .Name & ParameterList(l_memMember, , True)
                                Case INVOKE_PROPERTYPUT
                                    l_nodNode.AddChildNode .Name & ":Let", "Object." & .Name & ParameterList(l_memMember, , True) & " = " & VarTypeSignature(.ReturnType)
                                Case INVOKE_PROPERTYPUTREF
                                    l_nodNode.AddChildNode .Name & ":Set", "Set " & m_tiSelectedObject.Name & "." & .Name & " = " & VarTypeSignature(.ReturnType)
                                Case Else
                                End Select
                            End Select
                        End If
                    End With
                Next l_lngMembers
            End With
        Case Else
        End Select
    End With
    tvMembers.Sorted = True
    SendMessage tvMembers.hWndTreeView, WM_SETREDRAW, 1, 0
End Sub

Public Sub RefreshInfo()
On Error Resume Next
Dim l_strInfo As String
    l_strInfo = m_miSelectedMember.HelpString
    If InStr(l_strInfo, "{") Then
        l_strInfo = Left(l_strInfo, InStr(l_strInfo, "{") - 1)
    End If
    If Left(l_strInfo, 1) = "*" Then
        l_strInfo = Mid(l_strInfo, 2)
    End If
    If Left(l_strInfo, 1) = "~" Then
        l_strInfo = Mid(l_strInfo, 2)
    End If
    txtInfo.Text = l_strInfo
End Sub

Private Sub Form_Activate()
On Error Resume Next
End Sub

Private Sub Form_Load()
On Error Resume Next
    InitToolbars
    InitBrowser
    InitSplitters
    RefreshObjects
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_splSplitMain.MouseDown Button, Shift, X, Y
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_splSplitMain.MouseMove Button, Shift, X, Y
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_splSplitMain.MouseUp Button, Shift, X, Y
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
On Error Resume Next
    Plugin.Notify_Close
End Sub

Private Property Get iDocument_Plugin() As ngInterfaces.iPlugin
On Error Resume Next
    Set iDocument_Plugin = Nothing
End Property

Private Property Set iDocument_Plugin(RHS As ngInterfaces.iPlugin)
On Error Resume Next
'    Set m_fpgPlugin = RHS
End Property

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
On Error Resume Next
    Select Case KeyCode
    Case vbKeyF5
        RefreshObjects
        RefreshMembers
        RefreshInfo
    Case Else
    End Select
End Sub

Private Sub Form_Resize()
On Error Resume Next
    m_splSplitMain.Resize
End Sub

Private Property Get iDocument_CanSave() As Boolean
End Property

Private Property Get iDocument_Filename() As String
On Error Resume Next
    iDocument_Filename = "Command Browser"
End Property

Private Function iDocument_Save(Filename As String) As Boolean
On Error Resume Next
End Function

Private Property Get iDocument_Typename() As String
On Error Resume Next
    iDocument_Typename = "Command Browser"
End Property

Private Property Get iExtendedForm_Extender() As Object
On Error Resume Next
    Set iExtendedForm_Extender = Me.extender
End Property

Private Sub picMembers_Resize()
On Error Resume Next
Dim l_rctArea As RECT
    tbrMembers.Move 0, 0, tbrMembers.IdealWidth, tbrMembers.IdealHeight
    txtFilterMembers.Move tbrMembers.Width, 3, picMembers.ScaleWidth - tbrMembers.Width - 3, tbrMembers.Height - 6
    tvMembers.Move 0, tbrMembers.IdealHeight, picMembers.ScaleWidth, picMembers.ScaleHeight - tbrMembers.IdealHeight
    l_rctArea.Right = tvMembers.Width
    l_rctArea.Bottom = tvMembers.Height
    InvalidateRect tvMembers.hWndTreeView, l_rctArea, 0
End Sub

Private Sub picObjectMembers_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_splSplitMembers.MouseDown Button, Shift, X, Y
End Sub

Private Sub picObjectMembers_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_splSplitMembers.MouseMove Button, Shift, X, Y
End Sub

Private Sub picObjectMembers_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    m_splSplitMembers.MouseUp Button, Shift, X, Y
End Sub

Private Sub picObjectMembers_Resize()
    m_splSplitMembers.Resize
End Sub

Private Sub picObjects_Resize()
On Error Resume Next
Dim l_rctArea As RECT
    tbrObjects.Move 0, 0, tbrObjects.IdealWidth, tbrObjects.IdealHeight
    txtFilterObjects.Move tbrObjects.Width, 3, picObjects.ScaleWidth - tbrObjects.Width - 3, tbrObjects.Height - 6
    tvObjects.Move 0, tbrObjects.IdealHeight, picObjects.ScaleWidth, picObjects.ScaleHeight - tbrObjects.IdealHeight
    l_rctArea.Right = tvObjects.Width
    l_rctArea.Bottom = tvObjects.Height
    InvalidateRect tvObjects.hWndTreeView, l_rctArea, 0
End Sub

Private Sub tbrMembers_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
    Select Case LCase(Trim(Button.key))
    Case "resetfilter"
        txtFilterMembers.Text = ""
    Case Else
    End Select
End Sub

Private Sub tbrObjects_ButtonClick(Button As ngUI.ngToolButton)
On Error Resume Next
    Select Case LCase(Trim(Button.key))
    Case "resetfilter"
        txtFilterObjects.Text = ""
    Case Else
    End Select
End Sub

Private Sub tvMembers_NodeClick(node As vbalTreeViewLib6.cTreeViewNode)
On Error Resume Next
Dim l_nodNode As cTreeViewNode
    Set l_nodNode = node
    Do Until l_nodNode.Parent Is Nothing
        Set l_nodNode = l_nodNode.Parent
    Loop
    Set m_miSelectedMember = m_tiSelectedObject.DefaultInterface.GetMember(l_nodNode.key)
    RefreshInfo
End Sub

Private Sub tvObjects_NodeClick(node As vbalTreeViewLib6.cTreeViewNode)
On Error Resume Next
    If InStr(node.key, ":") Then
        Set m_tiSelectedObject = TypeFromName(TypeLibFromName(CStr(Split(node.key, ":")(0))), node.Text)
    Else
        Set m_tiSelectedObject = Nothing
    End If
    RefreshMembers
    RefreshInfo
End Sub

Private Sub txtFilterMembers_Change()
On Error Resume Next
    RefreshMembers
    RefreshInfo
End Sub

Private Sub txtFilterObjects_Change()
On Error Resume Next
    RefreshObjects
    Set m_tiSelectedObject = Nothing
    RefreshMembers
    RefreshInfo
End Sub

Private Property Get iDocument_Modified() As Boolean
On Error Resume Next
    iDocument_Modified = False
End Property

