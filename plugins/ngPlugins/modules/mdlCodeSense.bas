Attribute VB_Name = "mdlCodeSense"
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
Private m_booInitialized As Boolean

Public Function EnumLibrary(Obj As Object) As String
On Error Resume Next
Dim l_tliTypelib As TypeLibInfo
Dim l_cclCoClass As CoClassInfo
Dim l_lngInterfaces As Long
    Set l_tliTypelib = InterfaceInfoFromObject(Obj).Parent
    For Each l_cclCoClass In l_tliTypelib.CoClasses
        EnumLibrary = EnumLibrary & vbLf & EnumKeywords(l_cclCoClass)
    Next l_cclCoClass
End Function

Public Function EnumKeywords(ByRef Object As TLI.CoClassInfo) As String
On Error Resume Next
Dim l_lngMembers As Long
Dim l_strResult() As String
Dim l_iniInterface As InterfaceInfo
Dim l_mbrMember As MemberInfo
    With Object.DefaultInterface
        ReDim l_strResult(0 To .Members.Count - 1)
        For Each l_mbrMember In .Members
            l_strResult(l_lngMembers) = l_mbrMember.Name
            l_lngMembers = l_lngMembers + 1
        Next l_mbrMember
    End With
    EnumKeywords = Join(l_strResult, vbLf)
End Function

Public Function LoadTLI(ByRef Filename As String) As TLI.TypeLibInfo
On Error Resume Next
Dim l_tliApp As New TLIApplication
    Set LoadTLI = l_tliApp.TypeLibInfoFromFile(Filename)
End Function

Public Function InterfaceInfoFromObject(ByRef Object As Object) As TLI.InterfaceInfo
On Error Resume Next
Dim l_tliApp As New TLIApplication
    Set InterfaceInfoFromObject = l_tliApp.InterfaceInfoFromObject(Object)
End Function

Public Sub InitializeF2Script()
On Error Resume Next
Dim l_lngLanguage As CodeSenseCtl.Language
Dim l_gblGlobals As CodeSenseCtl.Globals
Dim l_strPrivateKeywords As String, l_strPublicKeywords As String
Dim l_hkKey As HotKey
    If m_booInitialized Then Exit Sub
    Set l_gblGlobals = New CodeSenseCtl.Globals
    Set l_lngLanguage = New Language
    With l_lngLanguage
        .CaseSensitive = False
        l_strPrivateKeywords = Join(Array( _
        "Public", "Private", "Dim", "ReDim", "Array", "Join", "Split", "LBound", "UBound", "Left", "Right", "Mid", "InStr", "LCase", "UCase", _
        "On", "Error", "Resume", "Next", "For", "If", "Else", "ElseIf", "Select", "Case", "End", "Sub", "Function", "Property", "Class", _
        "CLng", "CInt", "CSng", "CDbl", "CStr", "Then", "True", "False", "Nothing", "Empty", "Null", "With", "Do", "Loop", "Each", "In", _
        "LTrim", "RTrim", "Trim", "Exit", "Eval", "Class_Initialize", "Class_Terminate", "Class_ToString", "Class_ToNumber", _
        "Class_Load", "Class_Save" _
        ), vbLf)
        l_strPublicKeywords = EnumLibrary(Fury2Globals) & vbLf & EnumLibrary(Fury2GEGlobal) & vbLf & EnumLibrary(New Fury2File) & vbLf & EnumLibrary(New SoundEngine)
        .Keywords = l_strPrivateKeywords & vbLf & l_strPublicKeywords
        .ScopeKeywords1 = Join(Array("If", "Do", "For", "Select Case", "Sub", "Function", "Class", "Property", "With"), vbLf)
        .ScopeKeywords2 = Join(Array("End If", "Loop", "Next", "End Select", "End Sub", "End Function", "End Class", "End Property", "End With"), vbLf)
        .Operators = Join(Array("+", "-", "*", "/", "\", "^", "&", "And", "Or", "Xor", "Not", "Mod", ",", ".", "<", ">", "<=", ">=", "=", "<>", "(", ")", "Set", "Is", "New", "Me"), vbLf)
        .SingleLineComments = "'"
        .StringDelims = """"
        .Style = cmLangStyleProcedural
    End With
    l_gblGlobals.RegisterLanguage "F2Script", l_lngLanguage
    Set l_hkKey = New HotKey
    With l_hkKey
        .Modifiers1 = vbCtrlMask
        .VirtKey1 = "H"
    End With
    l_gblGlobals.RegisterHotKey l_hkKey, cmCmdFindReplace
    m_booInitialized = True
End Sub
