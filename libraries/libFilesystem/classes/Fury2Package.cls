VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fury2Package"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "A virtual filesystem package."
'
'    libFilesystem (Fury² Virtual Filesystem Library)
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
Option Compare Text
Private m_strFilename As String
#If SawZIP = 1 Then
Private m_arcArchive As Archive
#End If
Private m_fsFilesystem As Fury2Filesystem
Private m_strMountPoint As String

Public Property Get MountPoint() As String
    MountPoint = m_strMountPoint
End Property

Public Property Let MountPoint(NewPoint As String)
    m_strMountPoint = NewPoint
End Property

#If SawZIP = 1 Then
Friend Property Get Archive() As Archive
    Set Archive = m_arcArchive
End Property
#End If

Public Property Get Name() As String
Attribute Name.VB_UserMemId = 0
Attribute Name.VB_MemberFlags = "200"
On Error Resume Next
    Name = m_strFilename
End Property

Public Property Get Loaded() As Boolean
On Error Resume Next
    #If SawZIP = 1 Then
        Loaded = Trim(m_arcArchive.Name) <> ""
    #End If
End Property

Private Sub Initialize()
On Error Resume Next
#If SawZIP = 1 Then
    With m_arcArchive
        .AllowDirtyRead = True
        .StoreRelativePaths = True
        .OverwriteReadOnlyFiles = m_fsFilesystem.OverwriteReadOnlyFiles
    End With
#End If
End Sub

Friend Sub SetFilesystem(FS As Fury2Filesystem)
On Error Resume Next
    Set m_fsFilesystem = FS
    Initialize
End Sub

Friend Sub ClosePackage()
On Error Resume Next
#If SawZIP = 1 Then
    If Loaded Then m_arcArchive.Close
    m_arcArchive.ClearWarnings
#End If
End Sub

Friend Sub OpenPackage(Filename As String)
#If SawZIP = 1 Then
On Error Resume Next
Dim l_filPackage As Fury2File
Dim l_strFilename As String
Dim l_strSettings As String
Dim l_varSettings As Variant, l_varSetting As Variant
Dim l_lngSettings As Long, l_filSettings As SAWZIPLib.File
    ClosePackage
    m_strFilename = LCase(Trim(Filename))
    If m_fsFilesystem.FileExists(m_strFilename + ".f2pack") Then
        Set l_filPackage = m_fsFilesystem.File(m_strFilename + ".f2pack")
    ElseIf m_fsFilesystem.FileExists(m_strFilename + ".zip") Then
        Set l_filPackage = m_fsFilesystem.File(m_strFilename + ".zip")
    Else
        m_fsFilesystem.LogError "Package """ + Filename + """ not found."
        Exit Sub
    End If
    Err.Clear
    l_strFilename = l_filPackage.GetRealFilename
    If Len(l_strFilename) > 0 Then m_arcArchive.Open l_strFilename
    If Loaded Then
    
        ' load settings
        Err.Clear
        Set l_filSettings = m_arcArchive.Files.Get("package.f2cfg")
        If l_filSettings Is Nothing Then
        Else
            l_strSettings = l_filSettings.ExtractToString()
            If Trim(l_strSettings) = "" Then
            Else
                l_varSettings = Split(l_strSettings, vbCrLf)
                For l_lngSettings = LBound(l_varSettings) To UBound(l_varSettings)
                    l_varSetting = Split(l_varSettings(l_lngSettings), ":")
                    Select Case LCase(Trim(l_varSetting(0)))
                    Case "mount point", "mountpoint", "mount"
                        m_strMountPoint = Trim(l_varSetting(1))
                    Case Else
                    End Select
                Next l_lngSettings
            End If
        End If
        
        m_fsFilesystem.LogMessage "Package """ + Filename + """ mounted at " & m_strMountPoint & "."
    Else
        m_fsFilesystem.LogError "Package """ + Filename + """ not loaded."
    End If
#End If
End Sub

Friend Sub CreatePackage(Filename As String)
On Error Resume Next
#If SawZIP = 1 Then
    ClosePackage
    m_strFilename = LCase(Trim(Filename))
    m_arcArchive.Create m_fsFilesystem.TranslateFilename(Filename + ".f2pack")
    Err.Clear
#End If
End Sub

Private Sub Class_Initialize()
On Error Resume Next
#If SawZIP = 1 Then
    Set m_arcArchive = New Archive
#End If
    m_strMountPoint = "/"
End Sub

Private Sub Class_Terminate()
On Error Resume Next
#If SawZIP = 1 Then
    ClosePackage
    Set m_arcArchive = Nothing
#End If
End Sub
