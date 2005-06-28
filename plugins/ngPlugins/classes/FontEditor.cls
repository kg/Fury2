VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "FontEditor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
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
Implements iPlugin
Implements iFileTypePlugin
Public Enum FontEditorClipboardFormats
    FCF_MIN_
    FCF_Character
    FCF_Image
    FCF_MAX_
End Enum
Private m_lngClipboardFormats(FCF_MIN_ To FCF_MAX_)
Private m_objEditor As Object
Private m_clpClipboard As cCustomClipboard

Friend Property Get CustomClipboard() As cCustomClipboard
On Error Resume Next
    Set CustomClipboard = m_clpClipboard
End Property

Public Property Get ClipboardFormat(Format As FontEditorClipboardFormats) As Long
On Error Resume Next
    ClipboardFormat = m_lngClipboardFormats(Format)
End Property

Public Property Get Editor() As Object
    Set Editor = m_objEditor
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_clpClipboard = New cCustomClipboard
    m_lngClipboardFormats(FCF_Character) = m_clpClipboard.AddFormat("Fury2FontCharacter")
    m_lngClipboardFormats(FCF_Image) = m_clpClipboard.AddFormat("Fury2Image")
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    m_clpClipboard.ClipboardClose
End Sub

Private Sub iPlugin_ShowConfiguration()
End Sub

Private Property Get iPlugin_CanConfigure() As Boolean
On Error Resume Next
    iPlugin_CanConfigure = False
End Property

Private Sub iFileTypePlugin_FixUpSaveFilename(ByRef Filename As String)
On Error Resume Next
    If Len(Trim(GetExtension(Filename))) > 0 Then
    Else
        Filename = Filename + ".f2font"
    End If
End Sub

Private Function iFileTypePlugin_CanOpenFile(Filename As String) As Boolean
On Error Resume Next
    Select Case LCase(Trim(GetExtension(Filename)))
    Case "f2font", "f2fnt", "rfn"
        iFileTypePlugin_CanOpenFile = True
    Case Else
    End Select
End Function

Private Function iFileTypePlugin_CreateNew(Optional ByVal EnableWizard As Boolean = True) As iDocument
On Error Resume Next
Dim l_frmNew As New frmFont
Dim l_fntFont As Fury2Font
    If EnableWizard Then
        Load frmImportTTF
        m_objEditor.LoadSettings "FontEditor\DefaultSettings", frmImportTTF.Options
        frmImportTTF.RefreshSettings
        frmImportTTF.Show vbModal
        If Not (frmImportTTF.Options Is Nothing) Then
            m_objEditor.SaveSettings "FontEditor\DefaultSettings", frmImportTTF.Options
            With frmImportTTF.Options
                l_frmNew.SetFilename ""
                l_frmNew.RefreshAll
                Set l_fntFont = DefaultEngine.F2Font()
                l_fntFont.ImportTTF DefaultEngine.TTFont(.Name, .Size, .Bold, .Italic)
                l_frmNew.SetFont l_fntFont
                Set iFileTypePlugin_CreateNew = l_frmNew
                Set iFileTypePlugin_CreateNew.Plugin = Me
            End With
        End If
        Unload frmImportTTF
        Err.Clear
    Else
        Set l_fntFont = DefaultEngine.F2Font()
        l_frmNew.SetFont l_fntFont
        Set iFileTypePlugin_CreateNew = l_frmNew
        Set iFileTypePlugin_CreateNew.Plugin = Me
        Err.Clear
    End If
End Function

Private Property Get iFileTypePlugin_FileTypeName() As String
On Error Resume Next
    iFileTypePlugin_FileTypeName = "Font"
End Property

Private Function iFileTypePlugin_LoadFromFile(File As libGraphics.VirtualFile) As iDocument
On Error Resume Next
End Function

Private Function iFileTypePlugin_LoadFromFilename(Filename As String) As iDocument
On Error Resume Next
Dim l_frmNew As frmFont
Dim l_fntFont As Fury2Font
    Set l_frmNew = New frmFont
    Set l_fntFont = DefaultEngine.F2Font()
    If LoadFromFile(l_fntFont, F2File(Filename)) Then
        l_frmNew.SetFilename Filename
        l_frmNew.SetFont l_fntFont
        Set iFileTypePlugin_LoadFromFilename = l_frmNew
        Set iFileTypePlugin_LoadFromFilename.Plugin = Me
    Else
        l_fntFont.ImportRFN F2File(Filename)
        If l_fntFont.CharacterCount > 0 Then
            l_frmNew.SetFilename Filename
            l_frmNew.SetFont l_fntFont
            Set iFileTypePlugin_LoadFromFilename = l_frmNew
            Set iFileTypePlugin_LoadFromFilename.Plugin = Me
        End If
    End If
End Function

Private Property Get iFileTypePlugin_ShowInNewMenu() As Boolean
On Error Resume Next
    iFileTypePlugin_ShowInNewMenu = True
End Property

Private Sub iPlugin_Activate()
End Sub

Private Property Set iPlugin_Editor(RHS As Object)
    Set m_objEditor = RHS
End Property

Private Property Get iPlugin_Icon() As Fury2Image
On Error Resume Next
    Set iPlugin_Icon = m_objEditor.LoadResources("ng").ItemData("icons\font.png")
End Property

Private Sub iPlugin_Initialize()
End Sub

Private Property Get iPlugin_PluginDescription() As String
On Error Resume Next
    iPlugin_PluginDescription = "Provides support for creating, loading, editing, and saving Fury² and Sphere fonts."
End Property

Private Property Get iPlugin_PluginName() As String
On Error Resume Next
    iPlugin_PluginName = "Font Editor Plugin"
End Property

Private Property Get iPlugin_PluginPath() As String
On Error Resume Next
    iPlugin_PluginPath = App.Path & IIf(Right(App.Path, 1) = "\", "ng.dll", "\ng.dll")
End Property

Private Property Get iPlugin_ShowInPluginMenu() As Boolean
    iPlugin_ShowInPluginMenu = False
End Property

Private Sub iPlugin_Shutdown()
End Sub

Private Property Get iFileTypePlugin_FilterString() As String
On Error Resume Next
    iFileTypePlugin_FilterString = "Fonts|*.f2font;*.f2fnt;*.rfn"
End Property

Private Property Get iPlugin_ToolbarIcon() As libGraphics.Fury2Image
On Error Resume Next
End Property

Private Property Get iFileTypePlugin_SupportedExtensions() As Variant
On Error Resume Next
    iFileTypePlugin_SupportedExtensions = Array("f2font", "f2fnt", "rfn")
End Property

