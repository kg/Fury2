Attribute VB_Name = "mdlProfiler"
Option Explicit
Public ProfileTextCurrentHeight

Sub ProfileClear()
On Error Resume Next
    ProfileTextCurrentHeight = 0
    If frmProfile.Visible Then
    Else
        frmProfile.Show
    End If
    frmProfile.Cls
    frmProfile.CurrentY = frmProfile.CurrentY - frmProfile.vsScroll.Value
End Sub

Sub ProfilePrint(ByVal Text As Long)
On Error Resume Next
Dim l_bytText() As Byte
Dim l_lngOldY As Long
    If Text = 0 Then Exit Sub
    ReDim l_bytText(0 To GetStringLength(Text))
    StringCopy VarPtr(l_bytText(0)), Text
    l_lngOldY = frmProfile.CurrentY
    frmProfile.Print StrConv(l_bytText, vbUnicode);
    ProfileTextCurrentHeight = ProfileTextCurrentHeight + frmProfile.CurrentY - l_lngOldY
End Sub

Sub ProfileQuery(ByRef Value As Double)
On Error Resume Next
Dim PCounter As Currency, PFrequency As Currency
    Call QueryPerformanceCounter(PCounter)
    Call QueryPerformanceFrequency(PFrequency)
    Value = PCounter / PFrequency
End Sub
