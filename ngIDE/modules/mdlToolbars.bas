Attribute VB_Name = "mdlToolbars"
Option Explicit

Public Function ButtonString(ByRef Name As String, Optional ByRef Description As String = "", Optional ByRef key As String = "", Optional ByVal Icon = "", Optional ByVal Enabled As Boolean = True, Optional ByVal Style As ECTBToolButtonSyle = CTBNormal) As String
On Error Resume Next
    ButtonString = Name & "·" & Description & "·" & CStr(Icon) & "·" & Style & "·" & Enabled & "·" & key
End Function

Public Function ButtonString2(ByRef Name As String, Optional ByRef Description As String = "", Optional ByRef key As String = "", Optional ByVal Icon = "", Optional ByVal Enabled As Boolean = True, Optional ByVal Style As ngToolButtonStyles = bsyNormal) As String
On Error Resume Next
    ButtonString2 = Name & "·" & Description & "·" & CStr(Icon) & "·" & Style & "·" & Enabled & "·" & key
End Function

Public Function Buttons(ParamArray Values() As Variant) As Variant
On Error Resume Next
Dim l_lngItems As Long
Dim l_varValue As Variant
    ReDim l_varValue(LBound(Values) To UBound(Values))
    For l_lngItems = LBound(Values) To UBound(Values)
        l_varValue(l_lngItems) = Values(l_lngItems)
    Next l_lngItems
    Buttons = l_varValue
End Function

Public Function DefineToolbar(Toolbar As cToolbar, ImageList As vbalImageList, Items As Variant)
On Error Resume Next
    Toolbar.ImageSource = CTBExternalImageList
    Toolbar.SetImageList ImageList
    ParseToolbar Toolbar, ImageList, Items
End Function

Public Function DefineToolbar2(Toolbar As ngToolbar, ImageList As vbalImageList, Items As Variant)
On Error Resume Next
    ParseToolbar2 Toolbar, ImageList, Items
End Function

Public Sub ParseToolbar(Toolbar As cToolbar, ImageList As vbalImageList, Items As Variant)
On Error Resume Next
Dim l_lngItems As Long, l_varItem As Variant, l_varParameters As Variant
    With Toolbar
        For l_lngItems = LBound(Items) To UBound(Items)
            l_varItem = Items(l_lngItems)
            Select Case VarType(l_varItem)
            Case vbString
                If InStr(l_varItem, "·") Then
                    l_varParameters = Split(l_varItem, "·")
                    If VarType(l_varParameters(2)) = vbString Then
                        Err.Clear
                        l_varParameters(2) = CLng(ImageList.ItemIndex(CStr(l_varParameters(2)))) - 1
                        If Err <> 0 Then
                            l_varParameters(2) = -1
                        End If
                    End If
                    .AddButton l_varParameters(0), l_varParameters(2), , , , CLng(l_varParameters(3)), l_varParameters(5)
                    If Not CBool(l_varParameters(4)) Then
                        .ButtonEnabled(.ButtonCount - 1) = False
                    End If
                Else
                    If l_varItem = "-" Then
                        .AddButton , , , , , CTBSeparator
                    End If
                End If
            Case vbVariant Or vbArray
                ParseToolbar Toolbar, ImageList, l_varItem
            Case Else
            End Select
        Next l_lngItems
    End With
End Sub

Public Sub ParseToolbar2(Toolbar As ngToolbar, ImageList As vbalImageList, Items As Variant)
On Error Resume Next
Dim l_lngItems As Long, l_varItem As Variant, l_varParameters As Variant
Dim l_imgImage As Fury2Image, l_picImage As StdPicture
    With Toolbar
        For l_lngItems = LBound(Items) To UBound(Items)
            l_varItem = Items(l_lngItems)
            Select Case VarType(l_varItem)
            Case vbString
                Set l_imgImage = Nothing
                If InStr(l_varItem, "·") Then
                    l_varParameters = Split(l_varItem, "·")
                    If VarType(l_varParameters(2)) = vbString Then
                        Err.Clear
                        l_varParameters(2) = CLng(ImageList.ItemIndex(CStr(l_varParameters(2)))) - 1
                        If Err <> 0 Then
                            l_varParameters(2) = ""
                        End If
                    End If
                    Set l_picImage = Nothing
                    Set l_picImage = ImageList.ItemPicture(CLng(l_varParameters(2)) + 1)
                    If (l_picImage Is Nothing) Then
                    Else
                        Set l_imgImage = F2ImageFromPicture(l_picImage)
                    End If
                    .Buttons.AddNew , CStr(l_varParameters(5)), l_imgImage, CStr(l_varParameters(0)), CLng(l_varParameters(3)), , CBool(l_varParameters(4))
                Else
                    If l_varItem = "-" Then
                        .Buttons.AddNew "-"
                    End If
                End If
            Case vbVariant Or vbArray
                ParseToolbar2 Toolbar, ImageList, l_varItem
            Case Else
            End Select
        Next l_lngItems
    End With
End Sub

' Provided by vbAccelerator:
Public Function vbal_getVerticalHeight(tbrThis As cToolbar) As Long
Dim l As Long
Dim lHeight As Long
Dim lMaxWidth As Long
Dim lRowHeight As Long
Dim lRowWidth As Long

   lMaxWidth = vbal_getVerticalWidth(tbrThis)

   For l = 0 To tbrThis.ButtonCount - 1
      If tbrThis.ButtonVisible(l) Then
         If tbrThis.ButtonControl(l) = 0 Then

            If tbrThis.ButtonStyle(l) = CTBSeparator Then
               ' we'll start a new row for the next one
               lHeight = lHeight + lRowHeight + IIf(l < tbrThis.ButtonCount - 1, 8, 0)
               lRowHeight = 0
               lRowWidth = 0
            Else
               If (lRowWidth + tbrThis.ButtonWidth(l) > lMaxWidth) Then
                  ' This button needs to go on a new row:
                  lHeight = lHeight + lRowHeight
                  lRowHeight = 0
                  lRowWidth = lRowWidth + tbrThis.ButtonWidth(l)
                  If (tbrThis.ButtonHeight(l) > lRowHeight) Then
                     lRowHeight = tbrThis.ButtonHeight(l)
                  End If
               Else
                  ' This button goes on this row:
                  If (tbrThis.ButtonHeight(l) > lRowHeight) Then
                     lRowHeight = tbrThis.ButtonHeight(l)
                  End If
                  lRowWidth = lRowWidth + tbrThis.ButtonWidth(l)
               End If
            End If
         End If
      End If
   Next l
   lHeight = lHeight + lRowHeight
   vbal_getVerticalHeight = lHeight
End Function

' Provided by vbAccelerator:
Public Function vbal_getVerticalWidth(tbrThis As cToolbar) As Long
Dim l As Long
Dim lMaxWidth As Long
   For l = 0 To tbrThis.ButtonCount - 1
      If tbrThis.ButtonVisible(l) Then
         If tbrThis.ButtonControl(l) = 0 Then
            If (tbrThis.ButtonWidth(l) > lMaxWidth) Then
               lMaxWidth = tbrThis.ButtonWidth(l)
            End If
         End If
      End If
   Next l
   vbal_getVerticalWidth = lMaxWidth

End Function


