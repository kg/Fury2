VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ShowPictureDesign"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Implements IVirtualFileSaveable
Public Pictures As New Fury2Collection

Public Property Get Rectangle() As Fury2Rect
On Error Resume Next
Dim l_lngX1 As Long, l_lngY1 As Long, l_lngX2 As Long, l_lngY2 As Long
Dim l_picPicture As Picture
    l_lngX1 = 999999
    l_lngY1 = 999999
    l_lngX2 = -999999
    l_lngY2 = -999999
    For Each l_picPicture In Pictures
        With l_picPicture.Rectangle
            If .Left < l_lngX1 Then l_lngX1 = .Left
            If .Top < l_lngY1 Then l_lngY1 = .Top
            If .Right > l_lngX2 Then l_lngX2 = .Right
            If .Bottom > l_lngY2 Then l_lngY2 = .Bottom
        End With
    Next l_picPicture
    Set Rectangle = F2Rect(l_lngX1, l_lngY1, l_lngX2, l_lngY2, True)
End Property

Public Function Export() As String
On Error Resume Next
Dim l_lngIndex As Long
Dim l_strScript As String
Dim l_picPicture As Picture
Dim se As IScriptLanguage
    Set se = DefaultEngine.ScriptEngine.Language
    l_strScript = se.GenerateAssignment("Slot", "0", False) & se.LineTerminator
    For Each l_picPicture In Pictures
        With l_picPicture
            l_strScript = l_strScript & se.GenerateSubCall("ShowPicture", _
                Array("Slot + " & l_lngIndex, _
                se.GenerateFunctionCall("LoadImage", Array("""" & se.GenerateStringLiteral(.Filename) & """")), _
                .Rectangle.Left, Rectangle.Top, .Rectangle.Width, .Rectangle.Height, _
                .SourceRectangle.Left, .SourceRectangle.Top, _
                .BlitMode, .Opacity, _
                se.GenerateFunctionCall("F2RGB", Array(GetRed(.Color), GetGreen(.Color), GetBlue(.Color), GetAlpha(.Color))))) & _
                se.LineTerminator
        End With
        l_lngIndex = l_lngIndex + 1
    Next l_picPicture
    Export = l_strScript
End Function

Public Sub Render(Surface As Fury2Image)
On Error Resume Next
Dim l_picPicture As Picture
    For Each l_picPicture In Pictures
        l_picPicture.Draw Surface
    Next l_picPicture
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_lngCount As Long
Dim l_lngItems As Long
Dim l_picNew As Picture
    With File
        Set Pictures = New Fury2Collection
        .Load l_lngCount
        If l_lngCount > 0 Then
            For l_lngItems = 1 To l_lngCount
                Set l_picNew = New Picture
                .Load l_picNew
                Pictures.Add l_picNew
            Next l_lngItems
        End If
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
Dim l_picPicture As Picture
    With File
        .Save Pictures.Count
        For Each l_picPicture In Pictures
            .Save l_picPicture
        Next l_picPicture
    End With
End Sub
