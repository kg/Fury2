Attribute VB_Name = "mdlRegistry"
Option Explicit

Public Function ReadRegSetting(SettingName As String, Optional ByRef DefaultValue As Variant = Empty) As Variant
On Error Resume Next
Dim l_regRegistry As New cRegistry
    With l_regRegistry
        .ClassKey = HKEY_LOCAL_MACHINE
        If InStr(SettingName, "\") Then
            .SectionKey = "Software\Squared Interactive\ngIDE\" & left(SettingName, InStrRev(SettingName, "\") - 1) & "\"
            .ValueKey = Mid(SettingName, InStrRev(SettingName, "\") + 1)
        Else
            .SectionKey = "Software\Squared Interactive\ngIDE\"
            .ValueKey = SettingName
        End If
        .ValueType = REG_SZ
        If (VarType(.Value) <> vbEmpty) Then
            ReadRegSetting = CStr(.Value)
        Else
            .ValueType = REG_DWORD
            If (VarType(.Value) <> vbEmpty) Then
                ReadRegSetting = CLng(.Value)
            Else
                ReadRegSetting = DefaultValue
            End If
        End If
    End With
End Function

Public Sub WriteRegSetting(SettingName As String, Value As Variant)
On Error Resume Next
Dim l_regRegistry As New cRegistry
    With l_regRegistry
        .ClassKey = HKEY_LOCAL_MACHINE
        If InStr(SettingName, "\") Then
            .SectionKey = "Software\Squared Interactive\ngIDE\" & left(SettingName, InStrRev(SettingName, "\") - 1)
            .ValueKey = Mid(SettingName, InStrRev(SettingName, "\") + 1)
        Else
            .SectionKey = "Software\Squared Interactive\ngIDE"
            .ValueKey = SettingName
        End If
        If VarType(Value) = vbString Then
            .ValueType = REG_SZ
            .Value = CStr(Value)
        Else
            .ValueType = REG_DWORD
            .Value = CLng(Value)
        End If
    End With
End Sub

Public Function ReadRegData(SettingName As String) As Byte()
On Error Resume Next
Dim l_regRegistry As New cRegistry
Dim l_bytData() As Byte
    With l_regRegistry
        .ClassKey = HKEY_LOCAL_MACHINE
        If InStr(SettingName, "\") Then
            .SectionKey = "Software\Squared Interactive\ngIDE\" & left(SettingName, InStrRev(SettingName, "\") - 1) & "\"
            .ValueKey = Mid(SettingName, InStrRev(SettingName, "\") + 1)
        Else
            .SectionKey = "Software\Squared Interactive\ngIDE\"
            .ValueKey = SettingName
        End If
        .ValueType = REG_BINARY
        ReadRegData = l_bytData
        ReadRegData = .Value
    End With
End Function

Public Sub WriteRegData(SettingName As String, Data() As Byte)
On Error Resume Next
Dim l_regRegistry As New cRegistry
    With l_regRegistry
        .ClassKey = HKEY_LOCAL_MACHINE
        If InStr(SettingName, "\") Then
            .SectionKey = "Software\Squared Interactive\ngIDE\" & left(SettingName, InStrRev(SettingName, "\") - 1)
            .ValueKey = Mid(SettingName, InStrRev(SettingName, "\") + 1)
        Else
            .SectionKey = "Software\Squared Interactive\ngIDE"
            .ValueKey = SettingName
        End If
        .ValueType = REG_BINARY
        .Value = Data
    End With
End Sub

Public Sub SaveFormPosition(Form As Object)
On Error Resume Next
Dim l_strKey As String
    If ReadRegSetting("Options\Remember Window Positions", 0) = 0 Then Exit Sub
    If Form Is Nothing Then Exit Sub
    If TypeOf Form Is MDIForm Then
    Else
        If Form.MDIChild Then Exit Sub
    End If
    If Form.WindowState = 1 Then Form.WindowState = 0
    If TypeOf Form Is iDocument Then Exit Sub
    With Form
        l_strKey = "Window Positions\" & .Name & ":" & .Caption & "\"
        WriteRegSetting l_strKey & "Saved", CLng(1)
        WriteRegSetting l_strKey & "Maximized", Abs(CLng(.WindowState = 2))
        If CLng(.WindowState = 2) Then
            .WindowState = 0
        End If
        WriteRegSetting l_strKey & "Left", CLng(.left)
        WriteRegSetting l_strKey & "Top", CLng(.tOp)
        WriteRegSetting l_strKey & "Width", CLng(.Width)
        WriteRegSetting l_strKey & "Height", CLng(.Height)
    End With
End Sub

Public Sub LoadFormPosition(Form As Object)
On Error Resume Next
Dim l_strKey As String
    If ReadRegSetting("Options\Remember Window Positions", 0) = 0 Then Exit Sub
    If Form Is Nothing Then Exit Sub
    If TypeOf Form Is MDIForm Then
    Else
        If Form.MDIChild Then Exit Sub
    End If
    If TypeOf Form Is iDocument Then Exit Sub
    With Form
        l_strKey = "Window Positions\" & .Name & ":" & .Caption & "\"
        If ReadRegSetting(l_strKey & "Saved") <> Empty Then
            .Move ReadRegSetting(l_strKey & "Left", 0), _
                ReadRegSetting(l_strKey & "Top", 0), _
                ReadRegSetting(l_strKey & "Width", 0), _
                ReadRegSetting(l_strKey & "Height", 0)
            If ReadRegSetting(l_strKey & "Maximized", 0) Then
                .WindowState = 2
            End If
        End If
    End With
End Sub
