VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "GamePackerOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
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
Implements IVirtualFileSaveable
Implements IInspectable
Private mvarIcon As String
Private mvarStub As String
Private mvarOutputFilename As String
Private mvarGameName As String
Private mvarCopyright As String
Private mvarProductVersion As String
Private mvarProductName As String
Private mvarCompany As String
Private mvarInputFiles As String
Private mvarEngine As Fury2Engine
Private mvarLibraryFolder As String
Private mvarCompressData As Boolean

Friend Sub SetEngine(ByRef Engine As Fury2Engine)
On Error Resume Next
    Set mvarEngine = Engine
End Sub

Private Sub Class_Initialize()
On Error Resume Next
    mvarInputFiles = "*.*"
    mvarOutputFilename = "game.exe"
    mvarGameName = ""
    mvarStub = ""
    mvarIcon = ""
    mvarLibraryFolder = ""
    mvarCompressData = True
    mvarProductVersion = ""
    mvarProductName = ""
    mvarCompany = ""
    mvarCopyright = ""
End Sub

Public Property Get CompressData() As Boolean
Attribute CompressData.VB_Description = "If True, your game's files will be compressed before being packed into the executable."
On Error Resume Next
    CompressData = mvarCompressData
End Property

Public Property Let CompressData(ByVal NewValue As Boolean)
On Error Resume Next
    mvarCompressData = NewValue
End Property

Public Property Get Icon() As String
Attribute Icon.VB_Description = "Specifies the icon to embed into the executable."
On Error Resume Next
    Icon = mvarIcon
End Property

Public Property Let Icon(ByRef NewValue As String)
On Error Resume Next
    mvarIcon = NewValue
End Property

Public Property Get LibraryFolder() As String
On Error Resume Next
    LibraryFolder = mvarLibraryFolder
End Property

Public Property Let LibraryFolder(ByRef NewValue As String)
On Error Resume Next
    mvarLibraryFolder = NewValue
End Property

Public Property Get Stub() As String
Attribute Stub.VB_Description = "Specifies a stub executable to pack the game into. If blank, the default engine stub will be used."
On Error Resume Next
    Stub = mvarStub
End Property

Public Property Let Stub(ByRef NewValue As String)
On Error Resume Next
    mvarStub = NewValue
End Property

Public Property Get GameName() As String
Attribute GameName.VB_Description = "Specifies the title of the game. If left blank, the name specified in game.f2config will be used."
On Error Resume Next
    GameName = mvarGameName
End Property

Public Property Let GameName(ByRef NewValue As String)
On Error Resume Next
    mvarGameName = NewValue
End Property

Public Property Get Company() As String
Attribute Company.VB_Description = "Specifies a company name to place in the executable's version information. If empty, the default will be used."
On Error Resume Next
    Company = mvarCompany
End Property

Public Property Let Company(ByRef NewValue As String)
On Error Resume Next
    mvarCompany = NewValue
End Property

Public Property Get Copyright() As String
On Error Resume Next
    Copyright = mvarCopyright
End Property

Public Property Let Copyright(ByRef NewValue As String)
On Error Resume Next
    mvarCopyright = NewValue
End Property

Public Property Get ProductVersion() As String
Attribute ProductVersion.VB_Description = "Specifies a product version number (a.b.c.d) to place in the executable's version information. If empty, the default will be used."
On Error Resume Next
    ProductVersion = mvarProductVersion
End Property

Public Property Let ProductVersion(ByRef NewValue As String)
On Error Resume Next
    mvarProductVersion = NewValue
End Property

Public Property Get ProductName() As String
Attribute ProductName.VB_Description = "Specifies a product name to place in the executable's version information. If empty, the GameName will be used."
On Error Resume Next
    ProductName = mvarProductName
End Property

Public Property Let ProductName(ByRef NewValue As String)
On Error Resume Next
    mvarProductName = NewValue
End Property

Public Property Get InputFiles() As String
Attribute InputFiles.VB_Description = "Specifies one or more file sets to be included in the packed game. Seperate individual filesets with ;."
On Error Resume Next
    InputFiles = mvarInputFiles
End Property

Public Property Let InputFiles(ByRef NewValue As String)
On Error Resume Next
    mvarInputFiles = NewValue
End Property

Public Property Get OutputFilename() As String
Attribute OutputFilename.VB_Description = "Specifies the name of the output executable."
On Error Resume Next
    OutputFilename = mvarOutputFilename
End Property

Public Property Let OutputFilename(ByRef NewValue As String)
On Error Resume Next
    mvarOutputFilename = NewValue
End Property

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Load mvarInputFiles
        .Load mvarOutputFilename
        .Load mvarGameName
        .Load mvarStub
        .Load mvarIcon
        .Load mvarCompressData
        .Load mvarLibraryFolder
        .Load mvarCompany
        .Load mvarProductName
        .Load mvarProductVersion
        .Load mvarCopyright
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .Save mvarInputFiles
        .Save mvarOutputFilename
        .Save mvarGameName
        .Save mvarStub
        .Save mvarIcon
        .Save mvarCompressData
        .Save mvarLibraryFolder
        .Save mvarCompany
        .Save mvarProductName
        .Save mvarProductVersion
        .Save mvarCopyright
    End With
End Sub


