VERSION 5.00
Begin VB.Form frmDocGen 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Documentation Page Generator"
   ClientHeight    =   5370
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   8430
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   9
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   358
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   562
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer tmrStartup 
      Interval        =   50
      Left            =   3615
      Top             =   2445
   End
   Begin VB.CommandButton cmdRun 
      Caption         =   "&Run"
      Height          =   495
      Left            =   7020
      TabIndex        =   1
      Top             =   120
      Width           =   1380
   End
   Begin VB.Frame fraOutput 
      Caption         =   "&Output"
      Height          =   5325
      Left            =   30
      TabIndex        =   0
      Top             =   15
      Width           =   6960
      Begin VB.TextBox txtOutput 
         BackColor       =   &H8000000F&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Courier New"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   5010
         Left            =   45
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   2  'Vertical
         TabIndex        =   2
         Top             =   255
         Width           =   6855
      End
   End
End
Attribute VB_Name = "frmDocGen"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public Enums As New Collection
Public Classes As New Collection
Public Variables As New Collection
Public Templates As New Collection

Sub ArraySort(Arr As Variant, Optional ByVal Descending As Boolean = False)
On Error Resume Next
Dim m_lngLB As Long, m_lngUB As Long, m_lngCount As Long
Dim P As Long, K As Long, H As Long, I As Long, J As Long
Dim Temp As Variant
    Err.Clear
    If (VarType(Arr) And vbArray) <> vbArray Then Exit Sub
    m_lngLB = LBound(Arr)
    m_lngUB = UBound(Arr)
    m_lngCount = (m_lngUB - m_lngLB) + 1
    If Err <> 0 Or m_lngUB < 0 Then Exit Sub
    Err.Clear
    If m_lngCount < 2 Then Exit Sub
    For P = m_lngLB To ClipValue(m_lngUB - 1, m_lngLB, m_lngUB)
        H = P
        If Descending Then
            For K = P + 1 To m_lngUB
                If LCase(Arr(K)) > LCase(Arr(H)) Then H = K
            Next K
        Else
            For K = P + 1 To m_lngUB
                If LCase(Arr(K)) < LCase(Arr(H)) Then H = K
            Next K
        End If
        If P <> H Then
            I = H
            J = P
            Temp = Arr(I)
            Arr(I) = Arr(J)
            Arr(J) = Temp
        End If
    Next P
    Err.Clear
End Sub

Public Function TLIToString(ByRef VT As VarTypeInfo, ByRef Value, Optional ByVal WikiFormat As Boolean = True) As String
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
                            If WikiFormat Then
                                TLIToString = "Documentation." & VT.TypeInfo.Name & ".[" & .Name & "]"
                            Else
                                TLIToString = .Name
                            End If
                            Exit Function
                        End If
                    End With
                Next l_memMember
            End If
            If Value Is Nothing Then
                TLIToString = "Nothing"
            Else
                TLIToString = CStr(Value)
            End If
        End If
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
                            l_strParameters = l_strParameters & "Optional " & .Name & " As " & VarTypeSignature(.VarTypeInfo, False) & " = " & TLIToString(.VarTypeInfo, .DefaultValue, False)
                        ElseIf .Optional Then
                            l_strParameters = l_strParameters & "Optional " & .Name & " As " & VarTypeSignature(.VarTypeInfo, False)
                        Else
                            l_strParameters = l_strParameters & .Name & " As " & VarTypeSignature(.VarTypeInfo, False)
                        End If
                    Else
                        If .Default Or .Optional Then
                            l_strParameters = l_strParameters & "[" & .Name & "]"
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

Public Function VarTypeSignature(ByRef VT As VarTypeInfo, Optional ByVal WikiFormat As Boolean = True) As String
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
            VarTypeSignature = "Documentation." & IIf(Left(.TypeInfo.Name, 1) = "_", Mid(.TypeInfo.Name, 2), .TypeInfo.Name) & ".[" & IIf(Left(.TypeInfo.Name, 1) = "_", Mid(.TypeInfo.Name, 2), .TypeInfo.Name) & "]"
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
                VarTypeSignature = "None"
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
        If l_booArray Then VarTypeSignature = VarTypeSignature + " ()"
        If l_booByref Then VarTypeSignature = "ByRef " + VarTypeSignature
    End With
End Function

Sub DefineVariable(ByRef Name As String)
On Error Resume Next
Dim l_varVariable As New Variable
    l_varVariable.Name = Name
    Variables.Add l_varVariable, Name
    Log "+ $" & Name & "$"
End Sub

Sub SetVariable(ByRef Name As String, ByRef Value As String)
On Error Resume Next
    Variables(Name).Value = Value
End Sub

Sub LoadTemplate(ByRef FileName As String)
On Error Resume Next
Dim l_temTemplate As New Template
    l_temTemplate.Load FileName
    Templates.Add l_temTemplate, FileName
    Log "<- " & FileName
End Sub

Sub LoadTemplateFolder(ByRef Path As String)
On Error Resume Next
Dim l_strFile As String
    Log ": " & Path
    l_strFile = Dir("Templates\" & Path & "*.*")
    Do Until Len(l_strFile) < 1
        LoadTemplate Path & l_strFile
        l_strFile = Dir
    Loop
End Sub

Sub Init()
On Error Resume Next
    Log App.Path
    ChDrive Left(App.Path, 2)
    ChDir App.Path
    Log "Initializing Variables"
    DefineVariable "Namespaces"
    DefineVariable "ClassLinks"
    DefineVariable "EnumLinks"
    DefineVariable "ClassName"
    DefineVariable "EnumName"
    DefineVariable "MethodName"
    DefineVariable "PropertyName"
    DefineVariable "ValueName"
    DefineVariable "Type"
    DefineVariable "Value"
    DefineVariable "ReturnType"
    DefineVariable "Parameters"
    DefineVariable "Summary"
    DefineVariable "Properties"
    DefineVariable "Methods"
    DefineVariable "Interfaces"
    DefineVariable "Values"
    DefineVariable "Example"
    DefineVariable "Date"
    DefineVariable "Version"
    Log "Initializing Templates"
    LoadTemplate "NamespaceMap.xml"
    LoadTemplate "ClassReference.wiki"
    LoadTemplateFolder "$ClassName$\"
    LoadTemplateFolder "$EnumName$\"
End Sub

Sub BuildVariables()
On Error Resume Next
Dim l_strVariable As String
Dim l_strLibrary As String
Dim l_strName As String
Dim l_tiItem As TLI.TypeInfo
Dim l_varVariable As Variant
    l_strVariable = ""
    l_strLibrary = ""
    For Each l_tiItem In Classes
        If l_strLibrary <> l_tiItem.Parent.Name Then
            l_strLibrary = l_tiItem.Parent.Name
            l_strVariable = l_strVariable & "!" & l_strLibrary & vbCrLf
        End If
        l_strName = Replace(l_tiItem.VTableInterface.Name, "_", "")
        l_strVariable = l_strVariable & "        * Documentation." & l_strName & ".[" & l_strName & "]" & vbCrLf
    Next l_tiItem
    SetVariable "ClassLinks", l_strVariable
    Log "* $ClassLinks$"
    l_strVariable = ""
    For Each l_tiItem In Enums
        l_strName = Replace(l_tiItem.Name, "_", "")
        l_strVariable = l_strVariable & "        * Documentation." & l_strName & ".[" & l_strName & "]" & vbCrLf
    Next l_tiItem
    l_varVariable = Split(l_strVariable, vbCrLf)
    ArraySort l_varVariable
    l_strVariable = Join(l_varVariable, vbCrLf)
    SetVariable "EnumLinks", l_strVariable
    Log "* $EnumLinks$"
    l_strVariable = ""
    For Each l_tiItem In Enums
        l_strName = Replace(l_tiItem.Name, "_", "")
        l_strVariable = l_strVariable & "  <Namespace Root="".\Documentation\" & l_strName & """ Secure=""false"" Namespace=""Documentation." & l_strName & """ />" & vbCrLf
    Next l_tiItem
    For Each l_tiItem In Classes
        l_strName = Replace(l_tiItem.VTableInterface.Name, "_", "")
        l_strVariable = l_strVariable & "  <Namespace Root="".\Documentation\" & l_strName & """ Secure=""false"" Namespace=""Documentation." & l_strName & """ />" & vbCrLf
    Next l_tiItem
    SetVariable "Namespaces", l_strVariable
    Log "* $Namespaces$"
    SetVariable "Date", CStr(Now)
    SetVariable "Version", Engine.Fury2Version
End Sub

Sub Log(ByRef Text)
On Error Resume Next
    txtOutput.SelText = CStr(Text) & vbCrLf
    txtOutput.SelStart = Len(txtOutput.Text)
End Sub

Sub EnumLibrary(ByRef FileName As String)
On Error Resume Next
Dim l_libLibrary As TLI.TypeLibInfo
Dim l_srResults As SearchResults
Dim l_siItem As SearchItem
Dim l_tiItem As TypeInfo
    Set l_libLibrary = TypeLibInfoFromFile(FileName)
    Log "Scanning " & l_libLibrary.Name
    Set l_srResults = l_libLibrary.GetTypes(, tliStAppObject Or tliStClasses Or tliStConstants, True)
    For Each l_siItem In l_srResults
        Set l_tiItem = l_libLibrary.TypeInfos(l_siItem.TypeInfoNumber)
        If l_tiItem.VTableInterface Is Nothing Then
            Enums.Add l_tiItem
        Else
            Classes.Add l_tiItem
        End If
        Log "+ " & l_siItem.Name
    Next l_siItem
End Sub

Sub WriteLists()
On Error Resume Next
    Templates("NamespaceMap.xml").WriteOutput "Output\", Variables
    Templates("ClassReference.wiki").WriteOutput "Output\Documentation\", Variables
    DoEvents
End Sub

Sub WriteEnums()
On Error Resume Next
Dim l_tiItem As TypeInfo
Dim l_temTemplate As Template
Dim l_memValue As MemberInfo
Dim l_strValues As String
Dim l_varValues As Variant
    For Each l_tiItem In Enums
        With l_tiItem
            SetVariable "EnumName", .Name
            SetVariable "Summary", l_tiItem.HelpString
            l_strValues = ""
            For Each l_memValue In l_tiItem.Members
                l_strValues = l_strValues & "        * Documentation." & .Name & ".[" & l_memValue.Name & "]" & vbCrLf
            Next l_memValue
            l_varValues = Split(l_strValues, vbCrLf)
            ArraySort l_varValues
            l_strValues = Join(l_varValues, vbCrLf)
            SetVariable "Values", l_strValues
        End With
        For Each l_temTemplate In Templates
            MkDir "Input\" & l_tiItem.Name
            MkDir "Output\Documentation\" & l_tiItem.Name
            If InStr(l_temTemplate.FileName, "$EnumName$") Then
                If InStr(l_temTemplate.FileName, "$ValueName$") Then
                    For Each l_memValue In l_tiItem.Members
                        SetVariable "Value", CStr(l_memValue.Value)
                        SetVariable "ValueName", l_memValue.Name
                        SetVariable "Summary", l_memValue.HelpString
                        l_temTemplate.ReadInput "Input\", Variables
                        l_temTemplate.WriteOutput "Output\Documentation\", Variables
                    Next l_memValue
                Else
                    l_temTemplate.ReadInput "Input\", Variables
                    l_temTemplate.WriteOutput "Output\Documentation\", Variables
                End If
            End If
        Next l_temTemplate
        DoEvents
    Next l_tiItem
End Sub

Function TableParameterList(Member As MemberInfo) As String
On Error Resume Next
Dim l_parParameter As ParameterInfo
    TableParameterList = "||'''Name'''||'''Type'''||'''Default Value'''||" & vbCrLf
    For Each l_parParameter In Member.Parameters
        With l_parParameter
            If .Default Then
                TableParameterList = TableParameterList & "||" & .Name & "||" & VarTypeSignature(.VarTypeInfo) & "||" & TLIToString(.VarTypeInfo, .DefaultValue) & "||" & vbCrLf
            Else
                TableParameterList = TableParameterList & "||" & .Name & "||" & VarTypeSignature(.VarTypeInfo) & "||None||" & vbCrLf
            End If
        End With
    Next l_parParameter
End Function

Sub WriteClasses()
On Error Resume Next
Dim l_tiItem As TypeInfo
Dim l_temTemplate As Template
Dim l_memMember As MemberInfo
Dim l_intInterface As InterfaceInfo
Dim l_strName As String
Dim l_strProperties As String
Dim l_strMethods As String
Dim l_strInterfaces As String
Dim l_varMethods As Variant
Dim l_varProperties As Variant
    For Each l_tiItem In Classes
        With l_tiItem
            l_strName = Replace(.VTableInterface.Name, "_", "")
            SetVariable "ClassName", l_strName
            SetVariable "Summary", l_tiItem.HelpString
            l_strMethods = ""
            l_strProperties = ""
            l_strInterfaces = ""
            For Each l_memMember In l_tiItem.Members
                If l_memMember.AttributeMask <> 1 Then
                    If l_memMember.InvokeKind = INVOKE_FUNC Then
                        l_strMethods = l_strMethods & "        * Documentation." & l_strName & ".[" & l_memMember.Name & "]" & vbCrLf
                    ElseIf l_memMember.InvokeKind = INVOKE_PROPERTYGET Then
                        l_strProperties = l_strProperties & "        * Documentation." & l_strName & ".[" & l_memMember.Name & "]" & vbCrLf
                    End If
                End If
            Next l_memMember
            l_varProperties = Split(l_strProperties, vbCrLf)
            ArraySort l_varProperties
            l_strProperties = Join(l_varProperties, vbCrLf)
            l_varMethods = Split(l_strMethods, vbCrLf)
            ArraySort l_varMethods
            l_strMethods = Join(l_varMethods, vbCrLf)
            SetVariable "Interfaces", l_strInterfaces
            SetVariable "Methods", l_strMethods
            SetVariable "Properties", l_strProperties
        End With
        For Each l_temTemplate In Templates
            MkDir "Input\" & l_strName
            MkDir "Output\Documentation\" & l_strName
            If InStr(l_temTemplate.FileName, "$ClassName$") Then
                If InStr(l_temTemplate.FileName, "$MethodName$") Then
                    For Each l_memMember In l_tiItem.Members
                        If l_memMember.AttributeMask <> 1 Then
                            If l_memMember.InvokeKind = INVOKE_FUNC Then
                                SetVariable "MethodName", l_memMember.Name
                                SetVariable "Summary", l_memMember.HelpString
                                SetVariable "ReturnType", VarTypeSignature(l_memMember.ReturnType)
                                SetVariable "Parameters", TableParameterList(l_memMember)
                                If l_memMember.ReturnType <> VT_VOID Then
                                    SetVariable "Example", l_memMember.Name & ParameterList(l_memMember, False, True)
                                Else
                                    SetVariable "Example", l_memMember.Name & " " & ParameterList(l_memMember, False, False)
                                End If
                                l_temTemplate.ReadInput "Input\", Variables
                                l_temTemplate.WriteOutput "Output\Documentation\", Variables
                            End If
                        End If
                    Next l_memMember
                ElseIf InStr(l_temTemplate.FileName, "$PropertyName$") Then
                    For Each l_memMember In l_tiItem.Members
                        If l_memMember.InvokeKind = INVOKE_PROPERTYGET Then
                            SetVariable "PropertyName", l_memMember.Name
                            SetVariable "Summary", l_memMember.HelpString
                            SetVariable "Type", VarTypeSignature(l_memMember.ReturnType)
                            SetVariable "Example", l_memMember.Name & ParameterList(l_memMember, False, True)
                            SetVariable "Parameters", TableParameterList(l_memMember)
                            l_temTemplate.ReadInput "Input\", Variables
                            l_temTemplate.WriteOutput "Output\Documentation\", Variables
                        End If
                    Next l_memMember
                Else
                    l_temTemplate.ReadInput "Input\", Variables
                    l_temTemplate.WriteOutput "Output\Documentation\", Variables
                End If
            End If
        Next l_temTemplate
        DoEvents
    Next l_tiItem
End Sub

Sub Generate()
On Error Resume Next
    ChDrive Left(App.Path, 2)
    ChDir App.Path
    Log "Generating"
    Set Classes = New Collection
    Set Enums = New Collection
    EnumLibrary "J:\development\binary\sys\engine.dll"
    EnumLibrary "J:\development\binary\sys\graphics.dll"
    EnumLibrary "J:\development\binary\sys\filesystem.dll"
    EnumLibrary "J:\development\binary\sys\sound.dll"
    EnumLibrary "J:\development\binary\sys\script2.dll"
    EnumLibrary "J:\development\binary\sys\uikit.dll"
    Log "Building List Variables"
    BuildVariables
    Log "Writing List Output"
    WriteLists
    Log "Writing Enum Output"
    WriteEnums
    Log "Writing Class Output"
    WriteClasses
    Log "Done"
End Sub

Private Sub cmdRun_Click()
On Error Resume Next
    Generate
End Sub

Private Sub Form_Load()
On Error Resume Next
    Log "Loading"
    Init
    Log "Done"
End Sub

Private Sub tmrStartup_Timer()
On Error Resume Next
    tmrStartup.Enabled = False
    If Trim(LCase(Command$)) = "/run" Then
        Generate
        Unload Me
        End
    End If
End Sub
