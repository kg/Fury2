VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Template"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Public Text As String
Public FileName As String

Public Sub Load(InputPath As String)
On Error Resume Next
    l_lngFileHandle = FreeFile()
    Open "Templates\" & InputPath For Binary Access Read As #l_lngFileHandle
        Text = Space(LOF(l_lngFileHandle))
        Get #l_lngFileHandle, 1, Text
    Close #l_lngFileHandle
    FileName = InputPath
End Sub

Public Sub ReadInput(InputPath As String, Variables As Collection)
On Error Resume Next
Dim l_strText As String
Dim l_strFilename As String
Dim l_varVariable As Variable
Dim l_lngFileHandle As Long
Dim l_bytText() As Byte
Dim l_lngLength As Long
    l_strFilename = FileName
    For Each l_varVariable In Variables
        With l_varVariable
            l_strFilename = Replace(l_strFilename, "$" & .Name & "$", .Value, , , vbBinaryCompare)
        End With
    Next l_varVariable
    For Each l_varVariable In Variables
        With l_varVariable
            l_lngFileHandle = FreeFile()
            Err.Clear
            l_lngLength = -1
            l_lngLength = FileLen(InputPath & l_strFilename & "." & l_varVariable.Name)
            If l_lngLength > 0 Then
                Err.Clear
                Open InputPath & l_strFilename & "." & l_varVariable.Name For Binary Access Read As #l_lngFileHandle
                    ReDim l_bytText(0 To LOF(l_lngFileHandle) - 1)
                    Get #l_lngFileHandle, 1, l_bytText
                    l_varVariable.Value = StrConv(l_bytText, vbUnicode)
                Close #l_lngFileHandle
            End If
        End With
    Next l_varVariable
End Sub

Public Sub WriteOutput(OutputPath As String, Variables As Collection)
On Error Resume Next
Dim l_strText As String
Dim l_strFilename As String
Dim l_varVariable As Variable
Dim l_lngFileHandle As Long
    l_strText = Text
    l_strFilename = FileName
    For Each l_varVariable In Variables
        With l_varVariable
            l_strText = Replace(l_strText, "$" & .Name & "$", .Value, , , vbBinaryCompare)
            l_strFilename = Replace(l_strFilename, "$" & .Name & "$", .Value, , , vbBinaryCompare)
        End With
    Next l_varVariable
    l_lngFileHandle = FreeFile()
    Kill OutputPath & l_strFilename
    Open OutputPath & l_strFilename For Binary Access Write As #l_lngFileHandle
        Put #l_lngFileHandle, 1, l_strText
    Close #l_lngFileHandle
    frmDocGen.Log "-> " & l_strFilename
End Sub

