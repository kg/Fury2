VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ngToolButtons"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Description = "Fury² Generic Collection Object"
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Obj"
Attribute VB_Ext_KEY = "Member0" ,"Obj"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Implements IVirtualFileSaveable

Private m_tbrParent As ngToolbar
Private m_colCollection As Collection

Friend Sub Free()
On Error Resume Next
Dim l_btnButton As ngToolButton
    Set m_tbrParent = Nothing
    For Each l_btnButton In m_colCollection
        l_btnButton.Free
    Next l_btnButton
End Sub

Friend Sub SetParent(Parent As ngToolbar)
On Error Resume Next
    Set m_tbrParent = Parent
End Sub

Public Function AddNew(Optional ByRef Text As String = "", Optional ByRef Key As String = "", Optional ByRef Image As Fury2Image = Nothing, Optional ByRef Tooltip As String = "", Optional ByVal Style As ngToolButtonStyles = bsyNormal, Optional ByVal TextAlignment As ngToolButtonTextAlignment = btaRight, Optional ByVal Enabled As Boolean = True, Optional ByVal Checked As Boolean = False, Optional ByVal AtIndex As Long = -1) As ngToolButton
On Error Resume Next
Dim l_btnNew As ngToolButton
    Set l_btnNew = New ngToolButton
    With l_btnNew
        .Key = Key
        If (Text = "-") Then
            .Style = bsySeparator
        Else
            .Text = Text
            .Style = Style
        End If
        .Tooltip = Tooltip
        Set .Image = Image
        .TextAlignment = TextAlignment
        .Enabled = Enabled
        .Checked = Checked
    End With
    Add l_btnNew
    Set AddNew = l_btnNew
End Function

Public Sub Add(ByVal Obj As ngToolButton, Optional ByVal AtIndex As Long = -1)
On Error Resume Next
Dim l_strKey As String
    l_strKey = Obj.Key
    If Len(l_strKey) = 0 Then
        If AtIndex = -1 Then
            m_colCollection.Add Obj
        Else
            If m_colCollection.Count = 0 Then
                m_colCollection.Add Obj
            ElseIf AtIndex <= 1 Then
                m_colCollection.Add Obj, , 1
            ElseIf m_colCollection.Count > AtIndex Then
                m_colCollection.Add Obj, , AtIndex
            Else
                m_colCollection.Add Obj, , , AtIndex - 1
            End If
        End If
    Else
        If AtIndex = -1 Then
            m_colCollection.Add Obj, l_strKey
        Else
            If m_colCollection.Count = 0 Then
                m_colCollection.Add Obj, l_strKey
            ElseIf AtIndex <= 1 Then
                m_colCollection.Add Obj, l_strKey, 1
            ElseIf m_colCollection.Count > AtIndex Then
                m_colCollection.Add Obj, l_strKey, AtIndex
            Else
                m_colCollection.Add Obj, l_strKey, , AtIndex - 1
            End If
        End If
    End If
    Obj.SetParent m_tbrParent
    m_tbrParent.Reflow
    Err.Clear
End Sub

Public Property Get Item(ByVal IndexOrKey) As ngToolButton
Attribute Item.VB_UserMemId = 0
On Error Resume Next
    If VarType(IndexOrKey) = vbString Then
        IndexOrKey = CStr(IndexOrKey)
    End If
    Err.Clear
    Set Item = m_colCollection(IndexOrKey)
    Err.Clear
End Property

Public Function Find(ByVal Obj As ngToolButton) As Long
On Error Resume Next
Dim l_lngItems As Long, l_btnItem As ngToolButton
    If m_colCollection.Count <= 0 Then Exit Function
    l_lngItems = 1
    For Each l_btnItem In m_colCollection
        If l_btnItem Is Obj Then
            Find = l_lngItems
            Exit Function
        End If
        l_lngItems = l_lngItems + 1
    Next l_btnItem
End Function

Public Property Get Count() As Long
On Error Resume Next
    Count = m_colCollection.Count
End Property

Public Sub Remove(ByVal IndexOrKey)
On Error Resume Next
    If VarType(IndexOrKey) = vbString Then
        IndexOrKey = CStr(IndexOrKey)
    ElseIf VarType(IndexOrKey) = vbObject Then
        IndexOrKey = Find(IndexOrKey)
    Else
        IndexOrKey = CLng(IndexOrKey)
    End If
    m_colCollection.Remove IndexOrKey
    m_tbrParent.Reflow
End Sub

Public Sub Move(ByVal FromIndex As Long, ByVal ToIndex As Long)
On Error Resume Next
Dim l_btnButton As Object
    If FromIndex < 1 Or FromIndex > m_colCollection.Count Then Exit Sub
    If ToIndex < 1 Or ToIndex > (m_colCollection.Count + 1) Then Exit Sub
    If FromIndex = ToIndex Then Exit Sub
    Set l_btnButton = m_colCollection.Item(FromIndex)
    Remove FromIndex
    If (FromIndex < ToIndex) Then
'        Trust me, this works - even though it doesn't make sense unless you think about it
'        ToIndex = ToIndex - 1
    End If
    Add l_btnButton, ToIndex
End Sub

Public Sub Clear()
On Error Resume Next
    Set m_colCollection = New Collection
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
On Error Resume Next
    Set NewEnum = m_colCollection.[_NewEnum]
End Property

Private Sub Class_Initialize()
On Error Resume Next
    Set m_colCollection = New Collection
End Sub

Private Sub Class_Terminate()
On Error Resume Next
    Set m_colCollection = Nothing
End Sub

Private Sub IVirtualFileSaveable_Deserialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .ReadSegment_Begin
        .ReadSegment_End
    End With
End Sub

Private Sub IVirtualFileSaveable_Serialize(File As libGraphics.VirtualFile)
On Error Resume Next
    With File
        .WriteSegment_Begin
        .WriteSegment_End
    End With
End Sub

